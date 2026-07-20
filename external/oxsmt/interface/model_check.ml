(* R1 (ADR-UF-models §3, normative): the OBLIGATORY solver-side, in-process model
   self-check. Given a reconstructed candidate model ({!Cdclt} sort cardinalities +
   bindings) and the ORIGINAL asserted formula, evaluate EVERY assertion under the model;
   {!Session} promotes [Sat] only if all hold, else fails closed to [unknown].

   {b N-version firewall intact.} This is [oxsmt_core] + the {!Cdclt} value/binding
   vocabulary only; it does NOT import [tests/eval] — the external N-version validator
   stays an independent lineage. It is a fail-closed WITNESS / self-cert guard, not the
   verdict authority: soundness rests on the combination (a theory-certified [sat] is
   genuine by N-O soundness), and this catches a wrong/incomplete extracted table by
   degrading to [unknown], never a wrong verdict (ADR-UF-models §6). Any missing binding,
   type error, or arithmetic overflow ⇒ the assertion is not satisfied ⇒ [false] ⇒
   [unknown]. *)

open Oxsmt_core
module Rational = Oxsmt_lia.Rational

(* Fail-closed sentinel: the model cannot self-certify this assertion. *)
exception Bad

let value_eq (a : Cdclt.value) (b : Cdclt.value) =
  match a, b with
  | VBool x, VBool y -> Bool.equal x y
  | VInt x, VInt y -> Bigint.equal x y
  | VReal x, VReal y -> Rational.equal x y
  | VUninterp x, VUninterp y -> x = y
  | _ -> raise Bad
;;

let as_bool = function
  | Cdclt.VBool b -> b
  | _ -> raise Bad
;;

let as_int = function
  | Cdclt.VInt n -> n
  | _ -> raise Bad
;;

let as_real = function
  | Cdclt.VReal q -> q
  | _ -> raise Bad
;;

let real_of_term_rational (q : Term.rational) = Rational.of_big_frac ~num:q.num ~den:q.den

(* Evaluate [t] under the model tables ([consts]/[funs]); raises {!Bad} on any fault
   (missing binding, type error, arithmetic overflow). Shared by {!check} (over the
   original assertions) and {!eval_value} (W1b eliminated-variable re-derivation) so both
   use the identical fail-closed / overflow-guarded semantics. *)
let ev_with
  (consts : (string, Cdclt.value) Hashtbl.t)
  (funs : (string, Cdclt.fun_table) Hashtbl.t)
  =
  (* Memoize by hash-cons tag: the model tables are FIXED for this [ev_with] call, so a
     term's value is a pure function of the term. Without this a shared DAG (AND/OR/ITE
     diamonds) is evaluated as a tree — exponential in depth. Same fix/precedent as the DT
     checker's DAG memo (trunk a0a1f011e4). Sound: the memo is per-[ev_with]-call, and no
     caller holds the returned closure across a table mutation ([eval_in]/[eval_value]
     rebuild a fresh [ev_with] per call; [check] never mutates its tables), so a cached
     value is never stale. Short-circuit (And/Or/Ite) is preserved — the memo only records
     evaluations that actually happen. *)
  let memo : Cdclt.value Term.Table.t = Term.Table.create 256 in
  let rec ev (t : Term.t) : Cdclt.value =
    match Term.Table.find_opt memo t with
    | Some v -> v
    | None ->
      let v = ev_node t in
      Term.Table.replace memo t v;
      v
  and ev_node (t : Term.t) : Cdclt.value =
    match t.Term.node with
    | Term.Bool_const b -> VBool b
    | Term.Int_const n -> VInt n
    | Term.Real_const q -> VReal (real_of_term_rational q)
    | Term.App (sym, args) ->
      let name = Symbol.name sym in
      if Nia_config.is_mul_name name && Iarr.length args = 2
      then (
        (* Nonlinear-integer product marker (dark OXSMT_NIA): evaluate as ACTUAL integer
           multiplication of the argument values, NOT via the reconstructed uninterpreted
           table. This is the SAT-soundness gate — a candidate model in which the product
           constraint is violated makes the enclosing assertion evaluate false and fails
           closed to [unknown]. Fail-closed on any non-Int argument (rank disagreement). *)
        match Iarr.to_list args with
        | [ x; y ] -> VInt (Bigint.mul (as_int (ev x)) (as_int (ev y)))
        | _ -> raise Bad)
      else if Iarr.length args = 0
      then (
        match Hashtbl.find_opt consts name with
        | Some v -> v
        | None -> raise Bad)
      else (
        match Hashtbl.find_opt funs name with
        | None -> raise Bad
        | Some tbl ->
          let argv = List.map ev (Iarr.to_list args) in
          let n = List.length argv in
          let matches (case_args, _) =
            List.length case_args = n && List.for_all2 value_eq case_args argv
          in
          (match List.find_opt matches tbl.Cdclt.cases with
           | Some (_, r) -> r
           | None -> tbl.Cdclt.default))
    | Term.Arith lin ->
      (* Exact arbitrary-precision fold (core-bignum W2): a big coefficient or constant is
         evaluated precisely, never truncated. *)
      let s =
        Iarr.fold
          (fun acc (c, coeff) -> Bigint.add acc (Bigint.mul coeff (as_int (ev c))))
          lin.Term.const
          lin.Term.coeffs
      in
      VInt s
    | Term.Real_arith lin ->
      let s =
        Iarr.fold
          (fun acc (c, coeff) ->
            Rational.add acc (Rational.mul (real_of_term_rational coeff) (as_real (ev c))))
          (real_of_term_rational lin.Term.const)
          lin.Term.coeffs
      in
      VReal s
    | Term.Le a ->
      (match a.sort with
       | Sort.Int _ -> VBool (Bigint.compare (as_int (ev a)) Bigint.zero <= 0)
       | Sort.Real -> VBool (Rational.compare (as_real (ev a)) Rational.zero <= 0)
       | _ -> raise Bad)
    | Term.Eq (a, b) -> VBool (value_eq (ev a) (ev b))
    | Term.Not a -> VBool (not (as_bool (ev a)))
    | Term.And xs -> VBool (Iarr.fold (fun acc x -> acc && as_bool (ev x)) true xs)
    | Term.Or xs -> VBool (Iarr.fold (fun acc x -> acc || as_bool (ev x)) false xs)
    | Term.Ite (c, a, b) -> if as_bool (ev c) then ev a else ev b
  in
  ev
;;

(* The evaluator's lookup tables: nullary consts and function/predicate tables, both keyed
   by symbol name. Building them from a binding list is O(n); once built they are reused
   across every evaluation, so a caller that evaluates many terms against a growing model
   (the W1b eliminated-variable re-derivation) can keep ONE set of tables and mutate it in
   place ([add_const]) rather than rebuilding per evaluation (which was O(defs x bindings)
   overall — the SMPT quadratic). *)
type tables =
  { consts : (string, Cdclt.value) Hashtbl.t
  ; funs : (string, Cdclt.fun_table) Hashtbl.t
  }

let tables_of_bindings (bindings : Cdclt.binding list) =
  let consts : (string, Cdclt.value) Hashtbl.t = Hashtbl.create 64 in
  let funs : (string, Cdclt.fun_table) Hashtbl.t = Hashtbl.create 64 in
  List.iter
    (function
      | Cdclt.Const (n, v) -> Hashtbl.replace consts n v
      | Cdclt.Fun (n, tbl) -> Hashtbl.replace funs n tbl)
    bindings;
  { consts; funs }
;;

let add_const tbls name v = Hashtbl.replace tbls.consts name v

(* [eval_in tbls t] is [Some v] when [t] evaluates to [v] under the tables, else [None]
   (any missing binding / type error / overflow). Same fail-closed / overflow-guarded
   [ev_with] as {!check}. *)
let eval_in tbls t =
  match ev_with tbls.consts tbls.funs t with
  | v -> Some v
  | exception Bad -> None
;;

(* [check (sorts, bindings) assertions] is [true] iff every assertion evaluates to
   [VBool true] under the candidate model. Fail-closed: [false] on any evaluation fault. *)
let check ((_sorts : Cdclt.sort_card list), (bindings : Cdclt.binding list)) assertions =
  let tbls = tables_of_bindings bindings in
  let ev = ev_with tbls.consts tbls.funs in
  try List.for_all (fun a -> as_bool (ev a)) assertions with
  | Bad -> false
;;

(* [eval_value model t] is [Some v] when [t] evaluates to [v] under [model], else [None]
   (any missing binding / type error / overflow). Same fail-closed / overflow-guarded
   evaluator as {!check}. Builds the tables fresh; callers that evaluate many terms
   against an evolving model should instead hold a {!tables} and use {!eval_in} /
   {!add_const}. *)
let eval_value ((_sorts : Cdclt.sort_card list), (bindings : Cdclt.binding list)) t =
  eval_in (tables_of_bindings bindings) t
;;
