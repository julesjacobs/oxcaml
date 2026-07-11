(* Canonical form of a query, for content-addressed cache keying (DESIGN.md §8).

   The canonical string is invariant under semantics-preserving surface changes:
   - assertion order (conjunction is commutative),
   - operand order of commutative operators (and or + * = distinct),
   - declaration order,
   - whitespace / comments (already gone after parsing).

   It is NOT invariant under renaming of uninterpreted symbols: v1 deliberately does not
   rename (NOTES.md "Open questions"). Renaming would be verdict-preserving and raise the
   hit rate, but is a collision-bug risk in the trust-critical path, so it is deferred.
   Sorting cannot merge two semantically-distinct queries, so it is obviously sound.

   Two queries with the same canonical string are logically equivalent, hence share a
   verdict — safe to share a cache entry. *)

open Ast

let rec cterm (t : term) : string =
  match t with
  | True -> "true"
  | False -> "false"
  | Int_lit n -> "#" ^ n
  | Const s -> "$" ^ s
  | App (f, args) ->
      Printf.sprintf "(app %s %s)" f (String.concat " " (List.map cterm args))
  | Not a -> "(not " ^ cterm a ^ ")"
  | And xs -> "(and " ^ commutative xs ^ ")"
  | Or xs -> "(or " ^ commutative xs ^ ")"
  | Implies (a, b) -> Printf.sprintf "(=> %s %s)" (cterm a) (cterm b)
  | Ite (c, th, el) ->
      Printf.sprintf "(ite %s %s %s)" (cterm c) (cterm th) (cterm el)
  | Eq (a, b) -> "(= " ^ commutative [ a; b ] ^ ")"
  | Distinct xs -> "(distinct " ^ commutative xs ^ ")"
  | Le (a, b) -> Printf.sprintf "(<= %s %s)" (cterm a) (cterm b)
  | Lt (a, b) -> Printf.sprintf "(< %s %s)" (cterm a) (cterm b)
  | Ge (a, b) -> Printf.sprintf "(>= %s %s)" (cterm a) (cterm b)
  | Gt (a, b) -> Printf.sprintf "(> %s %s)" (cterm a) (cterm b)
  | Add xs -> "(+ " ^ commutative xs ^ ")"
  | Mul xs -> "(* " ^ commutative xs ^ ")"
  | Neg a -> "(neg " ^ cterm a ^ ")"
  | Sub xs -> "(- " ^ String.concat " " (List.map cterm xs) ^ ")"

and commutative xs =
  String.concat " " (List.sort String.compare (List.map cterm xs))

let csort = function Bool -> "Bool" | Int -> "Int" | Usort s -> s

let cdecl (name, params, ret) =
  Printf.sprintf "%s:(%s)->%s" name
    (String.concat "," (List.map csort params))
    (csort ret)

let canonical_query (q : query) : string =
  let sorts = String.concat " " (List.sort String.compare q.sort_decls) in
  let funs =
    String.concat " " (List.sort String.compare (List.map cdecl q.fun_decls))
  in
  let asserts =
    String.concat "\n" (List.sort String.compare (List.map cterm q.asserts))
  in
  Printf.sprintf "sorts: %s\nfuns: %s\nasserts:\n%s\n" sorts funs asserts
