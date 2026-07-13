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

(* Fail-closed sentinel: the model cannot self-certify this assertion. *)
exception Bad

let value_eq (a : Cdclt.value) (b : Cdclt.value) =
  match a, b with
  | VBool x, VBool y -> Bool.equal x y
  | VInt x, VInt y -> Bigint.equal x y
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

(* Evaluate [t] under the model tables ([consts]/[funs]); raises {!Bad} on any fault
   (missing binding, type error, arithmetic overflow). Shared by {!check} (over the
   original assertions) and {!eval_value} (W1b eliminated-variable re-derivation) so both
   use the identical fail-closed / overflow-guarded semantics. *)
let ev_with
      (consts : (string, Cdclt.value) Hashtbl.t)
      (funs : (string, Cdclt.fun_table) Hashtbl.t)
  =
  let rec ev (t : Term.t) : Cdclt.value =
    match t.Term.node with
    | Term.Bool_const b -> VBool b
    | Term.Int_const n -> VInt n
    | Term.App (sym, args) ->
      let name = Symbol.name sym in
      if Iarr.length args = 0
      then (
        match Hashtbl.find_opt consts name with
        | Some v -> v
        | None -> raise Bad)
      else (
        match Hashtbl.find_opt funs name with
        | None -> raise Bad
        | Some tbl ->
          let argv = List.map ev (Iarr.to_list args) in
          let matches (case_args, _) =
            List.length case_args = List.length argv
            && List.for_all2 value_eq case_args argv
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
    | Term.Le a -> VBool (Bigint.compare (as_int (ev a)) Bigint.zero <= 0)
    | Term.Eq (a, b) -> VBool (value_eq (ev a) (ev b))
    | Term.Not a -> VBool (not (as_bool (ev a)))
    | Term.And xs -> VBool (Iarr.fold (fun acc x -> acc && as_bool (ev x)) true xs)
    | Term.Or xs -> VBool (Iarr.fold (fun acc x -> acc || as_bool (ev x)) false xs)
    | Term.Ite (c, a, b) -> if as_bool (ev c) then ev a else ev b
  in
  ev
;;

let build_tables (bindings : Cdclt.binding list) =
  let consts : (string, Cdclt.value) Hashtbl.t = Hashtbl.create 64 in
  let funs : (string, Cdclt.fun_table) Hashtbl.t = Hashtbl.create 64 in
  List.iter
    (function
      | Cdclt.Const (n, v) -> Hashtbl.replace consts n v
      | Cdclt.Fun (n, tbl) -> Hashtbl.replace funs n tbl)
    bindings;
  consts, funs
;;

(* [check (sorts, bindings) assertions] is [true] iff every assertion evaluates to
   [VBool true] under the candidate model. Fail-closed: [false] on any evaluation fault. *)
let check ((_sorts : Cdclt.sort_card list), (bindings : Cdclt.binding list)) assertions =
  let consts, funs = build_tables bindings in
  let ev = ev_with consts funs in
  try List.for_all (fun a -> as_bool (ev a)) assertions with
  | Bad -> false
;;

(* [eval_value model t] is [Some v] when [t] evaluates to [v] under [model], else [None]
   (any missing binding / type error / overflow). Same fail-closed / overflow-guarded
   evaluator as {!check}, exposed for the W1b presolve's eliminated-variable re-derivation
   (session.ml): a value it cannot compute leaves the variable unbound, so R1 then rejects
   the model — never a wrong value. *)
let eval_value ((_sorts : Cdclt.sort_card list), (bindings : Cdclt.binding list)) t =
  let consts, funs = build_tables bindings in
  match ev_with consts funs t with
  | v -> Some v
  | exception Bad -> None
;;
