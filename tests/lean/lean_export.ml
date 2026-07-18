(* Certificate/model -> Lean 4 TRANSLATION (lean-proofs lane).

   This is NOT the tests/gate encoder: that one re-solves the query in Lean via
   grind/decide-on-open-goal. This module NEVER asks Lean to solve. It takes oxsmt's OWN
   output — for [Sat], the reconstructed model; for [Unsat] (later rungs), the recorded
   certificate — and emits a self-contained core-Lean-4 source file (no mathlib, no lake)
   whose kernel check succeeds iff oxsmt's verdict is corroborated:

   - SAT (rung 1): the model is rendered as concrete witnesses (every uninterpreted-sort
     element is its integer index; equality of elements is integer equality — faithful for
     the equality/disequality-only operations of the quantifier-free fragment). Each
     original assertion is lowered to a CLOSED GROUND [Prop], and their conjunction is
     proved by [decide] — pure kernel EVALUATION of a ground formula under the given
     model, not proof search. A companion "refutation control" claims the NEGATION of that
     same conjunction and MUST be rejected by the kernel: that is what shows the check can
     fail (a wrong model is rejected, not silently passed).

   Anything the model layer does not carry self-checkably (function tables, arrays,
   datatypes, Reals — no rational in core Lean) is a LOUD gap ([Unsupported]); it degrades
   to a documented non-result, never a fake proof. *)

module Term = Oxsmt_core.Term
module Sort = Oxsmt_core.Sort
module Symbol = Oxsmt_core.Symbol
module Iarr = Oxsmt_core.Iarr
module Bigint = Oxsmt_core.Bigint
module Session = Oxsmt_interface.Session

(* Raised while lowering when we hit a construct this rung cannot faithfully translate.
   Caught at the top and turned into [Unsupported]. *)
exception Gap of string

let gapf fmt = Printf.ksprintf (fun s -> raise (Gap s)) fmt

(* A model as a name -> value lookup for the nullary bindings (rung 1: no function tables
   — [Session.get_model] returns [None] when a table would be needed). *)
type model_lookup = (string, Session.model_value) Hashtbl.t

let model_lookup_of_bindings (bindings : Session.model_binding list) : model_lookup =
  let tbl = Hashtbl.create 32 in
  List.iter
    (fun (b : Session.model_binding) ->
      match b with
      | Session.Const (name, v) -> Hashtbl.replace tbl name v
      | Session.Fun (name, _) ->
        (* A self-checkable [get_model] never carries a table; if one appears, refuse. *)
        gapf "function table in model for %s (not self-checkable)" name)
    bindings;
  tbl
;;

(* Render a Bigint as a Lean [Int] literal, always parenthesised (negatives). *)
let lean_int_of_bigint b = Printf.sprintf "(%s : Int)" (Bigint.to_string b)

(* Look up a nullary user symbol's model value; a symbol with no binding is a gap (the
   assertion mentions a symbol the model does not constrain — do not invent a value). *)
let model_value tbl (sym : Symbol.t) : Session.model_value =
  let name = Symbol.name sym in
  match Hashtbl.find_opt tbl name with
  | Some v -> v
  | None -> gapf "model omits a value for %s" name
;;

(* Lower an Int- or uninterpreted-sorted term to a ground Lean [Int] expression. An
   uninterpreted-sort element becomes its 0-based universe index (VUninterp i -> i). *)
let rec int_of_term tbl (t : Term.t) : string =
  match t.node with
  | Term.Int_const n -> lean_int_of_bigint n
  | Term.App (sym, args) when Iarr.length args = 0 ->
    (match model_value tbl sym with
     | Session.VInt n -> lean_int_of_bigint n
     | Session.VUninterp i -> Printf.sprintf "(%d : Int)" i
     | Session.VBool _ -> gapf "Bool value for Int/element symbol %s" (Symbol.name sym)
     | Session.VReal _ -> gapf "Real value (no core-Lean rational)")
  | Term.App (sym, _) -> gapf "applied uninterpreted symbol %s" (Symbol.name sym)
  | Term.Arith { coeffs; const } ->
    let terms =
      List.map
        (fun (child, coeff) ->
          Printf.sprintf "(%s * %s)" (lean_int_of_bigint coeff) (int_of_term tbl child))
        (Iarr.to_list coeffs)
    in
    let all = lean_int_of_bigint const :: terms in
    "(" ^ String.concat " + " all ^ ")"
  | Term.Ite (c, a, b) ->
    Printf.sprintf
      "(if %s then %s else %s)"
      (prop_of_term tbl c)
      (int_of_term tbl a)
      (int_of_term tbl b)
  | Term.Real_const _ | Term.Real_arith _ ->
    gapf "Real arithmetic (no core-Lean rational)"
  | Term.Bool_const _ | Term.Le _ | Term.Eq _ | Term.Not _ | Term.And _ | Term.Or _ ->
    gapf "Bool-sorted node in Int position (ill-sorted?)"

(* Lower a Bool-sorted term to a ground Lean [Prop]. *)
and prop_of_term tbl (t : Term.t) : string =
  match t.node with
  | Term.Bool_const true -> "True"
  | Term.Bool_const false -> "False"
  | Term.App (sym, args) when Iarr.length args = 0 ->
    (match model_value tbl sym with
     | Session.VBool true -> "True"
     | Session.VBool false -> "False"
     | Session.VInt _ | Session.VUninterp _ ->
       gapf "non-Bool value for Bool symbol %s" (Symbol.name sym)
     | Session.VReal _ -> gapf "Real value for Bool symbol %s" (Symbol.name sym))
  | Term.App (sym, _) -> gapf "applied uninterpreted predicate %s" (Symbol.name sym)
  | Term.Le arg ->
    (match arg.sort with
     | Sort.Int _ -> Printf.sprintf "(%s <= (0 : Int))" (int_of_term tbl arg)
     | Sort.Real -> gapf "Real inequality (no core-Lean rational)"
     | _ -> gapf "Le over non-arith sort")
  | Term.Eq (a, b) ->
    (match a.sort with
     | Sort.Bool -> Printf.sprintf "(%s <-> %s)" (prop_of_term tbl a) (prop_of_term tbl b)
     | Sort.Int _ | Sort.Uninterpreted _ ->
       Printf.sprintf "(%s = %s)" (int_of_term tbl a) (int_of_term tbl b)
     | Sort.Real -> gapf "Real equality (no core-Lean rational)"
     | _ -> gapf "Eq over unsupported sort")
  | Term.Not a -> Printf.sprintf "(Not %s)" (prop_of_term tbl a)
  | Term.And xs ->
    "(" ^ String.concat " /\\ " (List.map (prop_of_term tbl) (Iarr.to_list xs)) ^ ")"
  | Term.Or xs ->
    "(" ^ String.concat " \\/ " (List.map (prop_of_term tbl) (Iarr.to_list xs)) ^ ")"
  | Term.Ite (c, a, b) ->
    Printf.sprintf
      "(if %s then %s else %s)"
      (prop_of_term tbl c)
      (prop_of_term tbl a)
      (prop_of_term tbl b)
  | Term.Int_const _ | Term.Arith _ | Term.Real_const _ | Term.Real_arith _ ->
    gapf "arith-sorted node in Bool position (ill-sorted?)"
;;

type sat_source =
  { positive : string (* proves the assertions hold under the model *)
  ; refutation_control : string (* claims the NEGATION; MUST be rejected by the kernel *)
  }

(* Build the two Lean sources for a SAT model + assertion batch. Raises {!Gap} (caught by
   caller) on any unsupported construct. An empty assertion batch is a degenerate [True]. *)
let emit_sat ~(model : Session.model) ~(assertions : Term.t list) : sat_source =
  let _sort_cards, bindings = model in
  let tbl = model_lookup_of_bindings bindings in
  let conj =
    match assertions with
    | [] -> "True"
    | _ -> "(" ^ String.concat " /\\ " (List.map (prop_of_term tbl) assertions) ^ ")"
  in
  let header =
    "-- oxsmt SAT model, kernel-EVALUATED (lean-proofs lane). Core Lean 4, no mathlib.\n\
     -- `decide` here is ground evaluation under a fixed model, not proof search.\n"
  in
  { positive = Printf.sprintf "%sexample : %s := by decide\n" header conj
  ; refutation_control =
      Printf.sprintf
        "%s-- NEGATIVE CONTROL: the model satisfies the assertions, so their negation is\n\
         -- false and this MUST be rejected by the kernel.\n\
         example : Not %s := by decide\n"
        header
        conj
  }
;;
