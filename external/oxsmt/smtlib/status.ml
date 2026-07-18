(* The [(set-info :status ...)] verdict label, shared by the printer and the test-only
   parser. Kept tiny and dependency-free so both sides agree on one representation. *)

type t =
  | Sat
  | Unsat
  | Unknown

let to_string = function
  | Sat -> "sat"
  | Unsat -> "unsat"
  | Unknown -> "unknown"
;;

let of_string = function
  | "sat" -> Some Sat
  | "unsat" -> Some Unsat
  | "unknown" -> Some Unknown
  | _ -> None
;;

let equal (a : t) (b : t) = a = b
