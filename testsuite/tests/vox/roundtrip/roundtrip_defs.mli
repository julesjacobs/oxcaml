(* Refined declarations that cross a .cmi boundary: written here, read back
   and printed by the test. *)

type nat = int{ _ >= 0 }

type dep = x:int{ x > 0 } -> int{ _ >= x }

val total_length : string -> int @@ total

val sub : s:string -> int{ _ < total_length s } -> char

val labelled : ~x:int{ x > 0 } -> unit

type wf = { size : int{ _ >= 0 } }

type pos = Pos of int{ _ > 0 }

(* A predicate referencing a value of the same signature: import must
   rewrite the path. *)
val positive : int -> bool @@ total

type p = int{ positive _ }

(* Typed-mirror identities across the .cmi: two records sharing a label
   name and two variants sharing a constructor name.  The predicates'
   identities are disambiguated by the payload type on the producer side,
   and the stored (parent path, name) / constructor path keys must survive
   import. *)
type fr1 = { sel : int }
type fr2 = { sel : bool }
type selected = fr1{ _.sel > 0 }
type fv1 = C of int
type fv2 = C of bool
type chosen = fv1{ let _v = if true then _ else C 1 in true }
