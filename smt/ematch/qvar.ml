(* Placeholder qvars (ADR-0012 §1.1). A qvar is a fresh nullary constant in the reserved
   [.oxsmt.qvar.<lemma-id>.<index>] namespace, disjoint from preprocessing's
   [.oxsmt.<kind>.<n>] fresh symbols (distinct [qvar] segment, ADR-0012 §1.1 Rider). *)

open Oxsmt_core

type t = Term.t

let prefix = ".oxsmt.qvar."

let is_qvar_name name =
  let p = prefix in
  String.length name >= String.length p && String.sub name 0 (String.length p) = p
;;

let to_term q = q

let mint cap env ctx ~lemma_id ~index sort =
  let name = Printf.sprintf "%s%d.%d" prefix lemma_id index in
  (* cap-gated mint (R1): the public [Env.declare_fun]/[declare_sort] reject [.oxsmt.*]. *)
  let sym = Env.declare_reserved cap env name (Rank.create [] sort) in
  Context.const ctx sym
;;

let rec term_contains_qvar (t : Term.t) =
  match t.node with
  | App (sym, args) ->
    is_qvar_name (Symbol.name sym) || Iarr.exists term_contains_qvar args
  | Arith l -> Iarr.exists (fun (tm, _c) -> term_contains_qvar tm) l.coeffs
  | Le a -> term_contains_qvar a
  | Eq (a, b) -> term_contains_qvar a || term_contains_qvar b
  | Not a -> term_contains_qvar a
  | And xs | Or xs -> Iarr.exists term_contains_qvar xs
  | Ite (c, a, b) -> term_contains_qvar c || term_contains_qvar a || term_contains_qvar b
  | Bool_const _ | Int_const _ -> false
;;
