(* Ground instances (ADR-0012 §1.1). Substitution-as-rebuild: a bottom-up walk through the
   session [Context]'s smart constructors, replacing each placeholder [App] with its
   ground image. Hash-consing makes the result canonical and O(1)-comparable. *)

open Oxsmt_core

type t = Term.t

let to_term i = i

let qvar_symbol (q : Qvar.t) =
  match (Qvar.to_term q).node with
  | App (sym, args) when Iarr.length args = 0 -> sym
  | _ ->
    (* A [Qvar.t] is always a nullary [App] by construction (Qvar.mint). *)
    failwith "Instance.of_subst: malformed qvar (not a nullary constant)"
;;

(* Rebuild [body] replacing any nullary [App] whose head is one of the qvar symbols with
   its ground image. [lookup sym] returns [Some ground] for a bound qvar, [None]
   otherwise. This mirrors the preprocessing DAG rewrite (preprocess.ml) node-for-node. *)
let subst ctx lookup body =
  let rec go (term : Term.t) =
    match term.node with
    | App (sym, args) when Iarr.length args = 0 ->
      (match lookup sym with
       | Some ground -> ground
       | None -> term)
    | App (sym, args) -> Context.app ctx sym (List.map go (Iarr.to_list args))
    | Arith l ->
      Context.linear_combination_big
        ctx
        (List.map (fun (tm, c) -> c, go tm) (Iarr.to_list l.coeffs))
        l.const
    | Le a -> Context.le ctx (go a) (Context.int_const ctx 0)
    | Eq (a, b) -> Context.eq ctx (go a) (go b)
    | Not a -> Context.not_ ctx (go a)
    | And xs -> Context.and_ ctx (List.map go (Iarr.to_list xs))
    | Or xs -> Context.or_ ctx (List.map go (Iarr.to_list xs))
    | Ite (c, a, b) -> Context.ite ctx (go c) (go a) (go b)
    | Bool_const _ | Int_const _ -> term
  in
  go body
;;

let of_subst ctx ~qvars ~body sigma =
  if Array.length qvars <> Array.length sigma
  then invalid_arg "Instance.of_subst: substitution arity mismatch";
  (* Build the placeholder-symbol -> ground lookup. Qvar count is tiny; a linear scan by
     symbol identity is deterministic and adequate. *)
  let bindings = Array.map2 (fun q g -> qvar_symbol q, g) qvars sigma in
  let lookup sym =
    let rec find i =
      if i >= Array.length bindings
      then None
      else (
        let s, g = bindings.(i) in
        if Symbol.equal s sym then Some g else find (i + 1))
    in
    find 0
  in
  let result = subst ctx lookup body in
  (* Internal bug-catch (ADR-0012 §1.1): the manager only feeds ground sigma, so a
     residual placeholder here is an invariant violation, not a user error. Loud
     [Failure]. *)
  if Qvar.term_contains_qvar result
  then failwith "Instance.of_subst: residual placeholder after substitution (bug)";
  result
;;
