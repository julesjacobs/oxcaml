(* Query AST for the QF_UFLIA subset the gate understands.

   Deliberately independent of [smt/core] (DESIGN.md §10, N-version isolation). Terms are
   sort-checked by [Reader]; any [term] the rest of the gate sees has passed that check.
   Sorts are recomputed on demand by [Reader.sort_of] rather than cached in the node —
   queries are tiny, clarity beats sharing here. *)

type sort =
  | Bool
  | Int
  | Usort of string (* uninterpreted sort, declared arity 0 *)

type verdict =
  | Sat
  | Unsat
  | Unknown

(* Numerals are kept as their nonnegative decimal string (arbitrary precision); negation
   is an explicit [Neg]. This avoids native-int overflow in the gate and matches Lean's
   [Int] literals. *)
type term =
  | True
  | False
  | Int_lit of string
  | Const of string (* reference to a declared 0-ary symbol *)
  | App of string * term list (* uninterpreted function application *)
  | Not of term
  | And of term list
  | Or of term list
  | Implies of term * term
  | Ite of term * term * term
  | Eq of term * term (* over non-Bool sorts; Bool-sorted [=] becomes [Iff] *)
  | Iff of term * term (* Bool-sorted [=] (SMT [=] over Bool is pairwise iff) *)
  | Distinct of term list
  | Le of term * term
  | Lt of term * term
  | Ge of term * term
  | Gt of term * term
  | Add of term list
  | Sub of term list (* left-associative subtraction, >= 1 args *)
  | Neg of term
  | Mul of term list
  | Div of term * term
    (* euclidean integer division (SMT-LIB [div]); divisor must be a nonzero integer
       literal — see [Elim] *)
  | Mod of term * term (* euclidean remainder (SMT-LIB [mod]); same divisor restriction *)

type query =
  { logic : string option
  ; sort_decls : string list (* uninterpreted sort names *)
  ; fun_decls :
      (string * sort list * sort) list (* name, arg sorts, result; [] args = const *)
  ; asserts : term list
  ; status : verdict option
  }

let verdict_to_string = function
  | Sat -> "sat"
  | Unsat -> "unsat"
  | Unknown -> "unknown"
;;

let sort_to_string = function
  | Bool -> "Bool"
  | Int -> "Int"
  | Usort s -> s
;;
