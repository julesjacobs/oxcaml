(* TEST
 modules = "vox_spec.ml";
 setup-ocamlc.byte-build-env;
 module = "vox_spec.ml";
 ocamlc.byte;
 module = "";
 flags += " -I ${test_build_directory}/ocamlc.byte";
 expect;
*)

(* The length of a list, stated against a measure.  The first half of this
   file is the route that does not work and why; the second half is a route
   that does.

   What blocks the first is not that a recursive measure cannot be total.  It
   can, and could before termination measures existed: a list recursion
   written out is structural, and the mode checker establishes that with no
   help at all.  What blocks it is that [Vox_spec.list_length] wraps
   [List.length], whose interface declares no mode, and an interface fixes
   the mode of what it describes whatever the implementation would have
   inferred.  Measured rather than assumed: the identical recursion compiled
   behind an interface reading [@@ total] is admitted, and behind one that
   says nothing is not.  So what this needs is an annotation on the standard
   library's interface, or a measure defined where no interface intervenes,
   which is what follows. *)

#load "vox_spec.cmo";;

(* @ex id=list_length_measure final=ACCEPT today=REJECT stable=no unlocks=stdlib-interface-modes+verification *)
let rec length (values : int list)
    : int{ _ = Vox_spec.list_length values }
  =
  match values with
  | [] -> 0
  | _head :: tail -> 1 + length tail

[%%expect {|
Line 2, characters 15-35:
2 |     : int{ _ = Vox_spec.list_length values }
                   ^^^^^^^^^^^^^^^^^^^^
Error: The value "Vox_spec.list_length" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 2, characters 6-44).
|}]

(* The measure defined here instead of imported.  It is structural on its
   argument, so it is total with nothing said, and [@vox.def] additionally
   publishes its defining equation as a trusted lemma.  Nothing unfolds that
   equation on the solver's behalf: each branch instantiates it at the value
   that branch is looking at, which is how every quantified fact reaches an
   obligation here. *)
let[@vox.def] rec spec_length (values : int list) : int =
  match values with
  | [] -> 0
  | _head :: tail -> 1 + spec_length tail

[%%expect {|
val spec_length : int list -> int = <fun>
val spec_length_def :
  (values : int list) ->
  unit{
   spec_length values = (match values with | [] -> 0 | :: _head tail -> 1 + spec_length tail)
   } =
  <fun>
|}]

let rec length (values : int list) : int{ _ = spec_length values } =
  match values with
  | [] ->
    let () = spec_length_def [] in
    0
  | head :: tail ->
    let () = spec_length_def (head :: tail) in
    1 + length tail

[%%expect {|
val length : (values : int list) -> int{ _ = spec_length values } = <fun>
|}]

(* Worth being explicit about what this claims, because a length is exactly
   where a machine integer stops being a number.  Both sides of
   [_ = spec_length values] add with the same wrapping arithmetic, so they
   agree at every input including one where both have wrapped: the claim is
   that two functions compute the same thing, and a relative claim like that
   is indifferent to overflow.  An absolute claim about the same value --
   that a length is non-negative, say -- is not, and is false past the
   maximum.  A measure that terminates and a measure whose value is
   representable are different properties; only the first is what carries the
   recursion above. *)
