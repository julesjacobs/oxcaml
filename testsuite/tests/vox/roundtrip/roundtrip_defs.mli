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

(* Every round-4 persistent mirror form is declared in an interface so the
   roundtrip test reads it from a CMI and prints only its source shape. *)
val mirror_labelled :
  x:int @ total -> (y:bool @ total -> bool @ total) @ total @@ total
val mirror_optional :
  ?o:int @ total -> (unit @ total -> bool @ total) @ total @@ total
val mirror_optional_labelled :
  ?o:int @ total -> (y:bool @ total -> bool @ total) @ total @@ total
val mirror_positional :
  p:[%call_pos] @ total ->
  (y:bool @ total ->
   (unit @ total -> bool @ total) @ total) @ total @@ total
(* [%apply]/[%revapply] preserve the operand mode relationally. *)
val mirror_id : bool @ total -> bool @ total @@ total
val mirror_accepts_format :
  (int -> string, unit, string) format @ total -> bool @ total @@ total
val mirror_accepts_unlabelled :
  (unit @ total -> bool @ total) @ total -> bool @ total @@ total

type completion_source =
  bool{ mirror_labelled ~y:true ~x:0 }

type completion_wrapper =
  bool{ mirror_optional ~o:0 () }

type completion_default =
  bool{ mirror_optional () }

type completion_eta_default =
  bool{ mirror_accepts_unlabelled mirror_optional }

type completion_omitted_optional =
  bool{ let _f = mirror_optional_labelled ~y:true in true }

type completion_call_pos =
  bool{ mirror_positional ~y:true () }

type completion_omitted_position =
  bool{ let _f = mirror_positional ~y:true in true }

type completion_omitted_required =
  bool{ let _f = mirror_labelled ~y:true in true }

type primitive_apply = bool{ mirror_id @@ true }
type primitive_revapply = bool{ true |> mirror_id }

type format_mirror = bool{ mirror_accepts_format "%d" }

type _ mirror_gadt = Mirror_int : int mirror_gadt
type gadt_mirror =
  int{ match (Mirror_int : int mirror_gadt) with Mirror_int -> _ > 0 }

type mirror_exists = Mirror_pack : 'a -> mirror_exists
type existential_mirror =
  bool{
    match Mirror_pack 0 with
    | Mirror_pack _ -> true
  }

type mirror_exists_pair = Mirror_pair : 'a * int -> mirror_exists_pair
type existential_bound_mirror =
  bool{
    match Mirror_pair ((), 0) with
    | Mirror_pair (_, n) -> n = n
  }
