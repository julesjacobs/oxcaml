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
  let asserted = Bv_simplify.simplify ctx mint asserted in
  match Bv_solve.solve Bv_adapter.defs asserted with
  | Bv_solve.Unsat -> Unsat
  | Bv_solve.Unknown _ -> Unknown
  | Bv_solve.Sat (model, bool_model) ->
    let named f xs =
      List.filter_map
        (fun (t, r) ->
           match name_of_var t with
           | Some n -> Some (f n r)
           | None -> None)
        xs
    in
    Sat
      { bv_vars = named (fun n (v, w) -> n, v, w) model
      ; bool_vars = named (fun n b -> n, b) bool_model
      }
;;
