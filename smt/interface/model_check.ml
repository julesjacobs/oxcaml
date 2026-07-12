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

let add_ovf a b =
  let r = a + b in
  if Bool.equal (a >= 0) (b >= 0) && not (Bool.equal (r >= 0) (a >= 0))
  then raise Bad
  else r
;;

let mul_ovf a b =
  if a = 0 || b = 0
  then 0
  else (
    let r = a * b in
    if r / a <> b then raise Bad else r)
;;

let value_eq (a : Cdclt.value) (b : Cdclt.value) =
  match a, b with
  | VBool x, VBool y -> Bool.equal x y
  | VInt x, VInt y -> x = y
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

(* [check (sorts, bindings) assertions] is [true] iff every assertion evaluates to
   [VBool true] under the candidate model. Fail-closed: [false] on any evaluation fault. *)
let check ((_sorts : Cdclt.sort_card list), (bindings : Cdclt.binding list)) assertions =
  let consts : (string, Cdclt.value) Hashtbl.t = Hashtbl.create 64 in
  let funs : (string, Cdclt.fun_table) Hashtbl.t = Hashtbl.create 64 in
  List.iter
    (function
      | Cdclt.Const (n, v) -> Hashtbl.replace consts n v
      | Cdclt.Fun (n, tbl) -> Hashtbl.replace funs n tbl)
    bindings;
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
      let s =
        Iarr.fold
          (fun acc (c, coeff) -> add_ovf acc (mul_ovf coeff (as_int (ev c))))
          lin.Term.const
          lin.Term.coeffs
      in
      VInt s
    | Term.Le a -> VBool (as_int (ev a) <= 0)
    | Term.Eq (a, b) -> VBool (value_eq (ev a) (ev b))
    | Term.Not a -> VBool (not (as_bool (ev a)))
    | Term.And xs -> VBool (Iarr.fold (fun acc x -> acc && as_bool (ev x)) true xs)
    | Term.Or xs -> VBool (Iarr.fold (fun acc x -> acc || as_bool (ev x)) false xs)
    | Term.Ite (c, a, b) -> if as_bool (ev c) then ev a else ev b
  in
  try List.for_all (fun a -> as_bool (ev a)) assertions with
  | Bad -> false
;;
