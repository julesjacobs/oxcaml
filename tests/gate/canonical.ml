(* Canonical form of a query, for content-addressed cache keying (DESIGN.md §8).

   The canonical string is invariant under semantics-preserving surface changes:
   - assertion order (conjunction is commutative),
   - operand order of commutative operators (and or + * = distinct),
   - declaration order,
   - whitespace / comments (already gone after parsing).

   It is NOT invariant under renaming of uninterpreted symbols: v1 deliberately does not
   rename (NOTES.md "Open questions"). Sorting/reordering cannot merge two
   semantically-distinct queries, so it is sound.

   INJECTIVITY (the property the cache's soundness rests on — a review found the previous
   space/paren-concatenated form was NOT injective: a |quoted symbol| may contain any byte
   except '|', including the separators, so two different queries could forge identical
   canonical bytes and share a cache entry). The fix: [ser] is a self-delimiting netstring
   encoding. Each node emits a 1-byte type tag ('A' atom, 'L' list), a decimal count, ':',
   then exactly that many payload bytes — for an atom the raw string read by length, for a
   list that many sub-nodes read recursively. A reader can therefore parse [ser x] back to
   [x] deterministically, so [ser x = ser y] implies [x = y]; because atom payloads are
   length-counted, arbitrary bytes in a symbol name (spaces, newlines, parens, ':',
   digits, 'A'/'L') are consumed as data and cannot forge a boundary. [ctree_of_query]
   places every symbol / sort / numeral in a tagged positional slot, so distinct queries
   (modulo the sound sorting of commutative operands and assertions) map to distinct
   trees, hence distinct strings. *)

open Ast

(* Canonical tree: atoms carry arbitrary bytes, lists carry ordered children. *)
type ctree = A of string | L of ctree list

let rec ser = function
  | A s -> Printf.sprintf "A%d:%s" (String.length s) s
  | L xs ->
      Printf.sprintf "L%d:%s" (List.length xs)
        (String.concat "" (List.map ser xs))

(* Sort children of a commutative operator by their (injective) serialization, so operand
   order does not affect the canonical form. *)
let sort_children (xs : ctree list) : ctree list =
  List.sort (fun a b -> String.compare (ser a) (ser b)) xs

let csort = function
  | Bool -> A "Bool"
  | Int -> A "Int"
  (* Tagged so a user sort literally named "Int"/"Bool" cannot collide with the builtin. *)
  | Usort s -> L [ A "usort"; A s ]

let rec cterm (t : term) : ctree =
  match t with
  | True -> L [ A "true" ]
  | False -> L [ A "false" ]
  | Int_lit n -> L [ A "num"; A n ]
  | Const s -> L [ A "const"; A s ]
  | App (f, args) -> L (A "app" :: A f :: List.map cterm args)
  | Not a -> L [ A "not"; cterm a ]
  | And xs -> L (A "and" :: sort_children (List.map cterm xs))
  | Or xs -> L (A "or" :: sort_children (List.map cterm xs))
  | Implies (a, b) -> L [ A "=>"; cterm a; cterm b ]
  | Ite (c, th, el) -> L [ A "ite"; cterm c; cterm th; cterm el ]
  | Eq (a, b) -> L (A "=" :: sort_children [ cterm a; cterm b ])
  | Distinct xs -> L (A "distinct" :: sort_children (List.map cterm xs))
  | Le (a, b) -> L [ A "<="; cterm a; cterm b ]
  | Lt (a, b) -> L [ A "<"; cterm a; cterm b ]
  | Ge (a, b) -> L [ A ">="; cterm a; cterm b ]
  | Gt (a, b) -> L [ A ">"; cterm a; cterm b ]
  | Add xs -> L (A "+" :: sort_children (List.map cterm xs))
  | Mul xs -> L (A "*" :: sort_children (List.map cterm xs))
  | Neg a -> L [ A "neg"; cterm a ]
  | Sub xs -> L (A "-" :: List.map cterm xs)

(* non-commutative: order preserved *)

let cdecl (name, params, ret) =
  L [ A "decl"; A name; L (List.map csort params); csort ret ]

let ctree_of_query (q : query) : ctree =
  (* Sort names have no payload beyond the name; sort them as atoms. Fun-decls and
     assertions are sorted by their serialization. *)
  let sorts =
    L
      (A "sorts"
      :: List.map (fun s -> A s) (List.sort String.compare q.sort_decls))
  in
  let funs = L (A "funs" :: sort_children (List.map cdecl q.fun_decls)) in
  let asserts = L (A "asserts" :: sort_children (List.map cterm q.asserts)) in
  L [ A "query"; sorts; funs; asserts ]

let canonical_query (q : query) : string = ser (ctree_of_query q)
