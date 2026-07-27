(* TEST
 expect;
*)

(* Where [assume] is refused, and where it is not.

   The rule is not that the constraint must be written at the call -- a law
   stated as a result annotation is admitted without being restated, which
   is the whole point of the feature -- but that a site imposing no
   refinement has no obligation to admit.  So every use that reaches the
   identifier itself is refused: passed, aliased, piped, partially applied,
   over-applied, or reached through an expected type that carries no
   refinement.

   The refusal is raised before the identifier is otherwise typed, so the
   reason given is the reason.  A law lives inside a total function, and
   [assume] is a partial one; being told that instead would send a reader
   somewhere useless. *)

let bare = assume
[%%expect {|
Line 1, characters 11-17:
1 | let bare = assume
               ^^^^^^
Error: [assume] admits the obligation of the refinement imposed on it,
       and there is none here
|}]

let qualified = Stdlib.assume
[%%expect {|
Line 1, characters 16-29:
1 | let qualified = Stdlib.assume
                    ^^^^^^^^^^^^^
Error: [assume] admits the obligation of the refinement imposed on it,
       and there is none here
|}]

let unimposed () = assume ()
[%%expect {|
Line 1, characters 19-25:
1 | let unimposed () = assume ()
                       ^^^^^^
Error: [assume] admits the obligation of the refinement imposed on it,
       and there is none here
|}]

let unrefined () = (assume () : unit)
[%%expect {|
Line 1, characters 20-26:
1 | let unrefined () = (assume () : unit)
                        ^^^^^^
Error: [assume] admits the obligation of the refinement imposed on it,
       and there is none here
|}]

let piped (y : int) = y |> assume
[%%expect {|
Line 1, characters 27-33:
1 | let piped (y : int) = y |> assume
                               ^^^^^^
Error: [assume] admits the obligation of the refinement imposed on it,
       and there is none here
|}]

let first_class : int -> int = assume
[%%expect {|
Line 1, characters 31-37:
1 | let first_class : int -> int = assume
                                   ^^^^^^
Error: [assume] admits the obligation of the refinement imposed on it,
       and there is none here
|}]

let over_applied = assume 1 2
[%%expect {|
Line 1, characters 19-25:
1 | let over_applied = assume 1 2
                       ^^^^^^
Error: [assume] admits the obligation of the refinement imposed on it,
       and there is none here
|}]

(* Inside a function that must be total, which is where a law is written.
   [assume] is a partial function, and the refusal is what is reported. *)
type nonneg = unit{ 1 = 1 }
[%%expect {|
type nonneg = unit{ 1 = 1 }
|}]

let (law @ total) () : nonneg = assume ()
[%%expect {|
Line 1, characters 32-38:
1 | let (law @ total) () : nonneg = assume ()
                                    ^^^^^^
Error: [assume] admits the obligation of the refinement imposed on it,
       and there is none here
|}]

(* A named alias of a refinement is not an imposition channel for anything.
   An ordinary annotation through one is a type error, not an obligation, so
   there is nothing here for [assume] to admit either. *)
type positive = int{ _ > 0 }
[%%expect {|
type positive = int{ _ > 0 }
|}]

let ordinary_through_alias (y : int) = (y : positive)
[%%expect {|
Line 1, characters 40-41:
1 | let ordinary_through_alias (y : int) = (y : positive)
                                            ^
Error: The value "y" has type "int" but an expression was expected of type
         "positive" = "int{ _ > 0 }"
|}]

let admitted_through_alias (y : int) = (assume y : positive)
[%%expect {|
Line 1, characters 40-46:
1 | let admitted_through_alias (y : int) = (assume y : positive)
                                            ^^^^^^
Error: [assume] admits the obligation of the refinement imposed on it,
       and there is none here
|}]

(* The argument position.  An argument is typed at its carrier and its
   refinement is checked afterwards by the verifier, so there is no refined
   expected type at the argument for an imposition to find, and neither
   spelling reaches one.  Writing the callee's precondition out again does
   work, and is exactly the restatement this feature exists to avoid, so
   this is a real gap rather than a rule. *)
let (needs_positive @ total) (n : int{ _ > 0 }) = n
[%%expect {|
val needs_positive : int{ _ > 0 } -> int = <fun>
|}]

let bare_argument (y : int) = needs_positive (assume y)
[%%expect {|
Line 1, characters 46-52:
1 | let bare_argument (y : int) = needs_positive (assume y)
                                                  ^^^^^^
Error: [assume] admits the obligation of the refinement imposed on it,
       and there is none here
|}]

let wrapped_annotation (y : int) = needs_positive (assume (y : int{ _ > 0 }))
[%%expect {|
Line 1, characters 51-57:
1 | let wrapped_annotation (y : int) = needs_positive (assume (y : int{ _ > 0 }))
                                                       ^^^^^^
Error: [assume] admits the obligation of the refinement imposed on it,
       and there is none here
|}]

(* A refinement IS imposed here and the admission is still refused, because
   a proposition has no code in it for an admission to be about.  The body of
   a definition reflected by [@vox.def] becomes a proposition too, and lands
   here for the same reason, which is why the wording does not say
   "predicate".  The staged case is in assume_refused_quotation.ml, which
   needs an extension flag of its own. *)
type in_a_predicate = int{ (assume _ : int{ _ > 0 }) = _ }
[%%expect {|
Line 1, characters 27-52:
1 | type in_a_predicate = int{ (assume _ : int{ _ > 0 }) = _ }
                               ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This obligation cannot be admitted: this is inside a proposition, and
       a proposition has no code in it for an admission to be about
|}]

(* Not a refusal of [assume] at all, and here because of what it rules out.
   A predicate cannot pass a logical value where a physical one is wanted, so
   the shape whose check would have read something with no run-time existence
   never reaches the tier: it is gone before there is an admission to make. *)
type vec
[%%expect {|
type vec
|}]

external size : vec -> int @@ total = "vec_size"
[%%expect {|
external size : vec -> int @@ total = "vec_size"
|}]

let (physical_of_logical @ total) (v : vec @ logical) : unit{ size v >= 0 } =
  assume ()
[%%expect {|
Line 1, characters 67-68:
1 | let (physical_of_logical @ total) (v : vec @ logical) : unit{ size v >= 0 } =
                                                                       ^
Error: This value is logical but is expected to be physical.
|}]

(* The name is not what is recognised.  A user's own [assume] is an ordinary
   function and keeps its ordinary meaning. *)
let shadowed =
  let assume x = x + 1 in
  assume 1
[%%expect {|
val shadowed : int = 2
|}]
