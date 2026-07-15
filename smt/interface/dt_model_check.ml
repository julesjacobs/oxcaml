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

(* DAG-AWARE re-derivation (codex/fable dt-spine finding 2). The model BUILDER memoizes
   [base_tree] per sort ({!Oxsmt_dt.Dt}), so a candidate model value is a shared DAG: one
   physical [Ctor] node reached by exponentially many paths (e.g. [Si = si(Si+1, Si+1)]
   embeds the SAME physical [base_tree Si+1] object in both fields, so a depth-[N] value
   has [N] distinct nodes but [2^N] paths). The checker re-derives INDEPENDENTLY, so a
   naive structural walk of [inhabits]/[v_eq]/[ev] would re-visit each shared node once
   per path — [~2^N] — and a trivially-satisfiable input could TIME OUT at the sat
   authority. Keying each recursive re-derivation on the node's PHYSICAL identity
   collapses the walk back to the DAG's node count.

   {b Soundness.} Every memoized function ([inhabits]/[v_eq]/[ev]) is a pure function of
   its (physical) arguments within one [check] call — [reg] and the model [env] are fixed
   for the call and nothing mutates a tree — so a memo hit returns EXACTLY what
   recomputation would (a hit is the SAME physical object, hence structurally identical).
   Memoization can therefore only make the checker faster; it can never make it accept a
   model it would otherwise reject, so the "no wrong verdict" firewall is preserved.
   Tables are created per [check] call (see {!check}/{!ev_with}), never shared across
   calls (per-context caches).

   The key uses physical [==] for the tree component; [hash] is the stdlib bounded
   structural hash ([hash_param] examines a bounded prefix, so it terminates on an
   arbitrarily large or shared DAG). Physically-equal trees are structurally identical, so
   they hash equal and a genuine hit lands. Two distinct objects that hash-collide are
   separated by [==] and treated as distinct keys — a missed hit (recompute), never a
   wrong merge. *)
let phys_hash (t : v) = Hashtbl.hash t

module Inhabits_key = struct
  type nonrec t = Sort.t * v

  let equal (s1, t1) (s2, t2) = Sort.equal s1 s2 && t1 == t2
  let hash (s, t) = Hashtbl.hash (Sort.hash s, phys_hash t)
end

module Inhabits_tbl = Hashtbl.Make (Inhabits_key)

module Pair_key = struct
  type nonrec t = v * v

  let equal (a1, b1) (a2, b2) = a1 == a2 && b1 == b2
  let hash (a, b) = Hashtbl.hash (phys_hash a, phys_hash b)
end

module Pair_tbl = Hashtbl.Make (Pair_key)

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
let make_inhabits reg : Sort.t -> v -> bool =
  let memo = Inhabits_tbl.create 256 in
  let rec inhabits (sort : Sort.t) (tree : v) : bool =
    match sort, tree with
    | Sort.Bool, Leaf (Model.Bool _) -> true
    | Sort.Int _, Leaf (Model.Int _) -> true
    | Sort.Uninterpreted _, Leaf (Model.Uninterp _) -> true
    (* Only the datatype arm RECURSES (and only there can the value be a shared DAG), so
       this is the only arm that consults the per-call memo. Confining the memo here keeps
       the key computation ([Inhabits_key.hash]/[equal], which call
       [Sort.hash]/[Sort.equal]) off every non-datatype sort: a [Datatype] sort
       hashes/compares in O(1) (identity is its symbol), whereas an [Array] sort's
       hash/equal RECURSE over the (untrusted) sort tree. A deeply- nested [Array]-sorted
       position therefore returns [false] in O(1) via the catch-all below WITHOUT ever
       entering [Sort.hash] (codex HIGH: eager [Sort.hash] Stack_overflow). *)
    | Sort.Datatype s, Ctor (name, fields) ->
      (match Inhabits_tbl.find_opt memo (sort, tree) with
       | Some b -> b
       | None ->
         let b =
           match Defs.datatype_of_sort reg s with
           | None -> false
           | Some dt ->
             (match
                List.find_opt
                  (fun (c : Defs.constructor) ->
                     String.equal (Symbol.name c.Defs.sym) name)
                  dt.Defs.constructors
              with
              | None -> false (* not a legal constructor of this datatype *)
              | Some c ->
                List.length c.Defs.selectors = List.length fields
                && List.for_all2
                     (fun (sel : Defs.selector) f -> inhabits sel.Defs.field_sort f)
                     c.Defs.selectors
                     fields)
         in
         Inhabits_tbl.replace memo (sort, tree) b;
         b)
    (* Array/BitVec positions are out of the pure-DT fragment (a DT+arrays/BV mix degrades
       to unknown at solve time); a DT model value never legitimately inhabits one, so
       reject. A sort/tree kind mismatch (e.g. a [Ctor] in a [Bool] position) rejects here
       too. *)
    | ( ( Sort.Bool
        | Sort.Int _
        | Sort.Uninterpreted _
        | Sort.Datatype _
        | Sort.Array _
        | Sort.BitVec _ )
      , _ ) -> false
  in
  inhabits
;;

let model_value_eq (a : Model.value) (b : Model.value) =
  match a, b with
  | Model.Bool x, Model.Bool y -> Bool.equal x y
  | Model.Int x, Model.Int y -> Bigint.equal x y
  | Model.Uninterp x, Model.Uninterp y -> Int.equal x y
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
  (* Structural value equality, DAG-aware (see the physical-identity note above).
     Well-sorted [Eq] never mixes a tree with a leaf (nor two leaves of different kinds);
     a mismatch is a fault => fail closed. Memoized on the physical pair so equating two
     shared DAGs does not re-descend a node once per path; only successful (non-raising)
     results are cached, so a [Bad]-raising pair is simply recomputed (and raises again). *)
  let v_eq_memo = Pair_tbl.create 256 in
  let rec v_eq (a : v) (b : v) : bool =
    match Pair_tbl.find_opt v_eq_memo (a, b) with
    | Some r -> r
    | None ->
      let r =
        match a, b with
        | Leaf x, Leaf y -> model_value_eq x y
        | Ctor (n1, k1), Ctor (n2, k2) ->
          String.equal n1 n2
          && List.length k1 = List.length k2
          && List.for_all2 v_eq k1 k2
        | _ -> raise Bad
      in
      Pair_tbl.replace v_eq_memo (a, b) r;
      r
  in
  (* [ev] is memoized on term identity: it is a pure function of the term ([env]/[reg] are
     fixed for this call), so a shared subterm is evaluated once. Only successful results
     are cached; a [Bad]-raising term is recomputed. Short-circuiting ([And]/[Or]/[Ite])
     is unaffected — a branch that was never evaluated is never cached. *)
  let ev_memo = Term.Table.create 256 in
  let rec ev (t : Term.t) : v =
    match Term.Table.find_opt ev_memo t with
    | Some v -> v
    | None ->
      let v = ev_node t in
      Term.Table.replace ev_memo t v;
      v
  and ev_node (t : Term.t) : v =
    match t.Term.node with
    | Term.Bool_const b -> Leaf (Model.Bool b)
    | Term.Int_const n -> Leaf (Model.Int n)
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
  let inhabits = make_inhabits reg in
  if not (List.for_all (fun (t, tree) -> inhabits t.Term.sort tree) model)
  then false
  else (
    let env = build_env model in
    let ev = ev_with reg env in
    try List.for_all (fun a -> as_bool (ev a)) assertions with
    | Bad -> false)
;;
