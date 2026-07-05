(* TEST
 expect;
*)

(* A ghost sort's Lean name is rendered VERBATIM, so it may not sit in
   the solver emitter's reserved namespaces: [Vox_] (datatypes, tuples,
   opaques) and [v_] (reflected values).  Such a name would silently
   ALIAS an emitted datatype/value name -- e.g. [lean "Vox_foo"] would
   be captured by the datatype [foo]'s emitted [Vox_foo].  Rejected
   eagerly at the declaration, fail-closed. *)

type t [@@vox.sort lean "Vox_foo"]
[%%expect{|
Line 1, characters 7-34:
1 | type t [@@vox.sort lean "Vox_foo"]
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: "Vox_foo" may not name a ghost sort -- the Vox_ prefix is reserved        for the solver's emitted datatype names (it would collide)
|}]

type u [@@vox.sort lean "v_bar"]
[%%expect{|
Line 1, characters 7-32:
1 | type u [@@vox.sort lean "v_bar"]
           ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: "v_bar" may not name a ghost sort -- the v_ prefix is reserved        for the solver's emitted value names (it would collide)
|}]

(* A name outside the reserved namespaces is fine. *)
type ok [@@vox.sort lean "MySet"]
[%%expect{|
type ok
|}]
