(* The obligatory solver-side, in-process model self-check for the DATATYPES theory (GOALS
   Datatypes: "every sat answer includes a model with actual constructor trees that the
   evaluator checks"). The datatypes analogue of {!Model_check} (which speaks the UF
   {!Cdclt} vocabulary and cannot interpret constructor/selector/tester applications).

   Given a candidate constructor-tree model (a [Term.t -> Dt.ctor_tree] assignment,
   extracted at the accepting Final->Sat by {!Oxsmt_dt.Dt.check_model}) and the ORIGINAL
   asserted formula, evaluate EVERY assertion under the model with faithful datatype
   semantics; {!Session} promotes the DT [Sat] only if all hold, else fails closed to
   [unknown].

   {b Why this is the soundness authority, not the theory.} A candidate model [M] is a
   genuine witness iff every original assertion evaluates to [true] under one consistent
   interpretation [M]. This evaluator re-derives that truth INDEPENDENTLY of the DT
   solving engine (it consults only the extracted model + the datatype {e shape} registry,
   never the [Euf] engine): constructors build trees, testers check the head, selectors
   project a matching field, equality is structural tree equality, the boolean skeleton
   folds as usual. If all assertions evaluate true, the formula is satisfiable BY
   DEFINITION — irrespective of what the theory intended. Fail-closed: any term it cannot
   evaluate, any missing model value, or any type confusion raises {!Bad} => [false] =>
   [unknown], never a wrong verdict. An explicit raise/branch, never an [assert] (release
   is -noassert). *)

open Oxsmt_core
module Defs = Datatype_defs
module Dt = Oxsmt_dt.Dt

(* Fail-closed sentinel: the model cannot self-certify this assertion. *)
exception Bad

(* The value domain IS the theory's constructor tree: [Ctor (name, fields)] for a datatype
   value, [Leaf v] for a scalar (Bool / Int / uninterpreted witness, {!Model.value}). *)
type v = Dt.ctor_tree =
  | Ctor of string * v list
  | Leaf of Model.value

(* SORT-INHABITANCE validation (codex B1/B2): a value in the model must INHABIT its
   position's declared sort, or the model is ill-formed and cannot certify anything =>
   reject. This is the soundness half of the fix for the Boolean-datatype-field wrong-Sat:
   a 2-element sort ([Bool]) must never be admitted through the unbounded [Uninterp]
   bucket. Concretely a [Bool] position must hold [Model.Bool] (so N mutually-distinct
   Bool leaves can never all be pairwise-unequal beyond the sort's cardinality — three
   distinct boxes of a [Bool] field then collapse and the [distinct] fails), an [Int]
   position [Model.Int] (a >int63 field degrades to [Uninterp] and is thereby rejected ->
   unknown, the documented bignum boundary), an uninterpreted-sort position an [Uninterp]
   element, and a datatype position a [Ctor] naming a LEGAL constructor of that datatype
   with the right arity and recursively-inhabiting fields (also closing B2: a fabricated
   [Ctor "bogus"] or an out-of-sort tree is rejected structurally rather than trusted).
   Converts the whole class from "the extractor must be right" to "the checker catches
   it". *)
let rec inhabits reg (sort : Sort.t) (tree : v) : bool =
  match sort, tree with
  | Sort.Bool, Leaf (Model.Bool _) -> true
  | Sort.Int _, Leaf (Model.Int _) -> true
  | Sort.Uninterpreted _, Leaf (Model.Uninterp _) -> true
  | Sort.Datatype s, Ctor (name, fields) ->
    (match Defs.datatype_of_sort reg s with
     | None -> false
     | Some dt ->
       (match
          List.find_opt
            (fun (c : Defs.constructor) -> String.equal (Symbol.name c.Defs.sym) name)
            dt.Defs.constructors
        with
        | None -> false (* not a legal constructor of this datatype *)
        | Some c ->
          List.length c.Defs.selectors = List.length fields
          && List.for_all2
               (fun (sel : Defs.selector) f -> inhabits reg sel.Defs.field_sort f)
               c.Defs.selectors
               fields))
  (* Array/BitVec positions are out of the pure-DT fragment (a DT+arrays/BV mix degrades
     to unknown at solve time); a DT model value never legitimately inhabits one, so
     reject. *)
  | ( ( Sort.Bool
      | Sort.Int _
      | Sort.Uninterpreted _
      | Sort.Datatype _
      | Sort.Array _
      | Sort.BitVec _ )
    , _ ) -> false
;;

let model_value_eq (a : Model.value) (b : Model.value) =
  match a, b with
  | Model.Bool x, Model.Bool y -> Bool.equal x y
  | Model.Int x, Model.Int y -> Int.equal x y
  | Model.Uninterp x, Model.Uninterp y -> Int.equal x y
  | _ -> raise Bad
;;

(* Structural value equality. Well-sorted [Eq] never mixes a tree with a leaf (nor two
   leaves of different kinds); a mismatch is a fault => fail closed. *)
let rec v_eq (a : v) (b : v) =
  match a, b with
  | Leaf x, Leaf y -> model_value_eq x y
  | Ctor (n1, k1), Ctor (n2, k2) ->
    String.equal n1 n2 && List.length k1 = List.length k2 && List.for_all2 v_eq k1 k2
  | _ -> raise Bad
;;

(* review F3: a bare unconstrained propositional variable degrades SOUNDLY here. An
   asserted Bool var is merged with true/false at assert time so it evaluates fine; a Bool
   var that appears only as an unconstrained subterm gets a non-Bool leaf (or none), and
   [as_bool] raises [Bad] => the check fails closed to [unknown], never a guessed Bool
   value. *)
let as_bool = function
  | Leaf (Model.Bool b) -> b
  | _ -> raise Bad
;;

(* Evaluate [term] under the model table [env] (leaf variables + underspecified selector
   terms) and the datatype shape [reg]; raises {!Bad} on any fault. Compound terms are
   computed structurally — only leaves and genuinely-underspecified selector applications
   are read from [env]. *)
let ev_with (reg : Defs.t) (env : v Term.Table.t) =
  let rec ev (t : Term.t) : v =
    match t.Term.node with
    | Term.Bool_const b -> Leaf (Model.Bool b)
    | Term.Int_const n ->
      (match Bigint.to_int_opt n with
       | Some i -> Leaf (Model.Int i)
       | None ->
         raise Bad (* a >63-bit integer literal has no native scalar; fail closed *))
    | Term.Eq (a, b) -> Leaf (Model.Bool (v_eq (ev a) (ev b)))
    | Term.Not a -> Leaf (Model.Bool (not (as_bool (ev a))))
    | Term.And xs ->
      Leaf (Model.Bool (Iarr.fold (fun acc x -> acc && as_bool (ev x)) true xs))
    | Term.Or xs ->
      Leaf (Model.Bool (Iarr.fold (fun acc x -> acc || as_bool (ev x)) false xs))
    | Term.Ite (c, a, b) -> if as_bool (ev c) then ev a else ev b
    | Term.Le _ | Term.Arith _ ->
      (* arithmetic is not in the pure-DT fragment (a datatype+LIA problem degrades to
         [unknown] at solve time before this runs); no faithful value here => fail closed. *)
      raise Bad
    | Term.App (sym, args) ->
      let args = Array.of_list (Iarr.to_list args) in
      (match Defs.constructor_of_sym reg sym with
       | Some (_dt, c) ->
         (* a constructor application builds a tree structurally from its evaluated fields *)
         Ctor (Symbol.name c.Defs.sym, Array.to_list (Array.map ev args))
       | None ->
         (match Defs.tester_of_sym reg sym with
          | Some (_dt, c) when Array.length args = 1 ->
            (match ev args.(0) with
             | Ctor (name, _) ->
               Leaf (Model.Bool (String.equal name (Symbol.name c.Defs.sym)))
             | Leaf _ -> raise Bad)
          | Some _ -> raise Bad
          | None ->
            (match Defs.selector_of_sym reg sym with
             | Some (_dt, c, sel) when Array.length args = 1 ->
               (match ev args.(0) with
                | Ctor (name, fields) when String.equal name (Symbol.name c.Defs.sym) ->
                  (match List.nth_opt fields sel.Defs.index with
                   | Some v -> v
                   | None -> raise Bad)
                | Ctor _ ->
                  (* selector applied to a value of a DIFFERENT constructor: SMT-LIB
                     leaves this underspecified. Use the model's own (consistent) value
                     for this selector term, extracted at Final; absent => fail closed. *)
                  (match Term.Table.find_opt env t with
                   | Some v -> v
                   | None -> raise Bad)
                | Leaf _ -> raise Bad)
             | Some _ -> raise Bad
             | None ->
               (* not a constructor/selector/tester. A leaf (nullary) variable/constant is
                  read from the model; an applied uninterpreted function (only in QF_UFDT,
                  out of the v1 fragment) has no value => fail closed. *)
               if Array.length args = 0
               then (
                 match Term.Table.find_opt env t with
                 | Some v -> v
                 | None -> raise Bad)
               else raise Bad)))
  in
  ev
;;

let build_env (model : (Term.t * Dt.ctor_tree) list) : v Term.Table.t =
  let env = Term.Table.create 128 in
  List.iter (fun (t, tree) -> Term.Table.replace env t tree) model;
  env
;;

(* [check reg model assertions] is [true] iff (1) every model value INHABITS its term's
   declared sort (so a finite sort — [Bool] — can never be admitted as unbounded, the B1
   fix) AND (2) every assertion evaluates to [Bool true] under [model] with faithful
   datatype semantics. Fail-closed: [false] on any inhabitance violation or evaluation
   fault. *)
let check reg model assertions =
  if not (List.for_all (fun (t, tree) -> inhabits reg t.Term.sort tree) model)
  then false
  else (
    let env = build_env model in
    let ev = ev_with reg env in
    try List.for_all (fun a -> as_bool (ev a)) assertions with
    | Bad -> false)
;;
