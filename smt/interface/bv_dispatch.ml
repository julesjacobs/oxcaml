open Oxsmt_core
module Bv_solve = Oxsmt_bitblast.Bv_solve
module Bv_adapter = Oxsmt_bitblast.Bv_adapter

let is_bv (s : Sort.t) =
  match Bv.width_of_sort s with
  | Some _ -> true
  | None -> false
;;

(* A term is a Bool-or-BV connective/leaf the blaster can encode. Conservative: any
   construct outside the QF_BV fragment (an uninterpreted function application, an
   arithmetic atom, a non-Bool/non-BV sort) makes the whole set NOT pure, so it stays on
   the combinator's fail-closed degrade path (never a wrong verdict via a half-understood
   route). [any_bv] records whether at least one bit-vector term is present, so a pure
   propositional formula is left to the normal SAT path rather than hijacked here. *)
let is_pure_bv (asserted : Term.t list) =
  let seen : bool Term.Table.t = Term.Table.create 256 in
  let any_bv = ref false in
  let bool_or_bv (s : Sort.t) = Sort.equal s Sort.bool || is_bv s in
  let rec ok (t : Term.t) =
    match Term.Table.find_opt seen t with
    | Some r -> r
    | None ->
      if is_bv t.sort then any_bv := true;
      let r =
        match t.node with
        | Bool_const _ -> true
        | Not a -> ok a
        | And args | Or args -> List.for_all ok (Iarr.to_list args)
        | Ite (c, a, b) -> bool_or_bv t.sort && ok c && ok a && ok b
        | Eq (a, b) -> bool_or_bv a.sort && ok a && ok b
        | App (_sym, args) ->
          (match Bv.view t with
           | Some (Bv.Const _) -> true
           | Some (Bv.Op { op = _; args = op_args; result_width = _ }) ->
             List.for_all ok op_args
           | None ->
             (* not a bit-vector operator/literal: admissible only as a nullary Bool or BV
                variable; an applied uninterpreted symbol is out of pure QF_BV *)
             Iarr.length args = 0 && bool_or_bv t.sort)
        | Le _ | Arith _ | Int_const _ -> false
      in
      Term.Table.replace seen t r;
      r
  in
  List.for_all ok asserted && !any_bv
;;

let name_of_var (t : Term.t) =
  match t.node with
  | App (sym, args) when Iarr.length args = 0 -> Some (Symbol.name sym)
  | _ -> None
;;

(* Free USER variables of [terms]: nullary applications that are NOT bit-vector
   operator/literal symbols (those are internal), of Bool or BitVec sort. Returns the
   bit-vector vars as [(term, width)] and the Bool vars as [term], each deduplicated,
   walking the shared DAG once. Used to COMPLETE a model: a sound word-level rewrite
   ({!Bv_simplify}) can eliminate a variable's only occurrence (e.g.
   [(= (extract 7 4 (concat x y)) x)] reduces to [x = x]), so the blaster never sees it
   and returns no binding — but the ORIGINAL query names it, so the surfaced model must
   still bind it (to any value; it is unconstrained, since the rewrite that dropped it is
   equivalence- preserving per the exhaustive oracle). Without this, such a [sat] loses
   its model and the CLI degrades it to [unknown] (a spurious non-solve). *)
let free_user_vars (terms : Term.t list) =
  let seen : unit Term.Table.t = Term.Table.create 256 in
  let bv = ref [] in
  let bool = ref [] in
  let rec walk (t : Term.t) =
    match Term.Table.find_opt seen t with
    | Some () -> ()
    | None ->
      Term.Table.replace seen t ();
      (match t.node with
       | App (sym, args) when Iarr.length args = 0 ->
         if not (Bv.is_bv_sym sym)
         then (
           match Bv.width_of_sort t.sort with
           | Some w -> bv := (t, w) :: !bv
           | None -> if Sort.equal t.sort Sort.bool then bool := t :: !bool)
       | Not a | Le a -> walk a
       | And xs | Or xs -> List.iter walk (Iarr.to_list xs)
       | Ite (c, a, b) ->
         walk c;
         walk a;
         walk b
       | Eq (a, b) ->
         walk a;
         walk b
       | App (_, args) -> List.iter walk (Iarr.to_list args)
       | Bool_const _ | Int_const _ | Arith _ -> ())
  in
  List.iter walk terms;
  List.rev !bv, List.rev !bool
;;

type result =
  | Unsat
  | Unknown
  | Sat of
      { bv_vars : (string * Bigint.t * int) list
      ; bool_vars : (string * bool) list
      }

module Bv_simplify = Oxsmt_bitblast.Bv_simplify

(* Solve a pure-QF_BV assertion set by eager bit-blasting. A word-level pre-blast pass
   ({!Bv_simplify}) first normalizes the assertions to shrink the SAT instance; it never
   renames free variables, so the model read back below is still keyed by the user's
   names. [Bv_solve] re-checks every sat model with the independent evaluator before
   returning [Sat], so a [Sat] here is already self-certified — the session surfaces its
   bindings without re-running the (BV-unaware) R1 combinator checker. *)
let solve ctx mint (asserted : Term.t list) : result =
  let simplified = Bv_simplify.simplify ctx mint asserted in
  match Bv_solve.solve Bv_adapter.defs simplified with
  | Bv_solve.Unsat -> Unsat
  | Bv_solve.Unknown _ -> Unknown
  | Bv_solve.Sat (model, bool_model) ->
    (* Model COMPLETION for rewrite-eliminated variables: the extract/concat/bitwise/shift
       families (task #36) can eliminate a variable's only occurrence, so the blaster
       never binds it and a [sat] would lose its model (CLI degrades to [unknown]). Any
       user var named in the ORIGINAL query but absent from the blaster's model was
       dropped by an (equivalence-preserving, oracle-certified) rewrite, so it is
       unconstrained — bind it to 0 / false. Guarded on the rewrite gate so the OFF path
       is byte-identical to before this task (the additive-only normalizer's pre-existing
       cancellation behaviour is left exactly as it was). *)
    let extra_bv, extra_bool =
      if not (Bv_simplify.rewrite2_enabled ())
      then [], []
      else (
        let present : unit Term.Table.t = Term.Table.create 256 in
        List.iter (fun (t, _) -> Term.Table.replace present t ()) model;
        List.iter (fun (t, _) -> Term.Table.replace present t ()) bool_model;
        let orig_bv, orig_bool = free_user_vars asserted in
        ( List.filter_map
            (fun (t, w) ->
               if Term.Table.mem present t then None else Some (t, (Bigint.zero, w)))
            orig_bv
        , List.filter_map
            (fun t -> if Term.Table.mem present t then None else Some (t, false))
            orig_bool ))
    in
    let named f xs =
      List.filter_map
        (fun (t, r) ->
           match name_of_var t with
           | Some n -> Some (f n r)
           | None -> None)
        xs
    in
    Sat
      { bv_vars = named (fun n (v, w) -> n, v, w) (model @ extra_bv)
      ; bool_vars = named (fun n b -> n, b) (bool_model @ extra_bool)
      }
;;
