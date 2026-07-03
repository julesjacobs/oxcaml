(* TEST
 flags = "-vox-dry-run";
 expect;
*)

(* vox: static (typechecker) errors.  All cases run under -vox-dry-run:
   these are rejected before any solver would run. *)

(* assume_ needs a refined expected type from context (refine_ without
   one synthesizes the exact refinement instead; see exact.ml). *)
let x = assume_ 3
[%%expect{|
Line 1, characters 8-17:
1 | let x = assume_ 3
            ^^^^^^^^^
Error: vox: assume_ needs a refined expected type; add a type annotation
|}]

(* Refinements are allowed at every skeleton type; non-int/bool
   skeletons get equality-only reasoning at an uninterpreted solver
   sort (see compact.ml). *)
let s : {v:string | v = v} = refine_ "hi"
[%%expect{|
val s : string{ _ = _ } = "hi"
|}]

(* Unpacking requires a refined scrutinee. *)
let f (x : int) = let refine_ w = x in w
[%%expect{|
Line 1, characters 22-31:
1 | let f (x : int) = let refine_ w = x in w
                          ^^^^^^^^^
Error: vox: a refine_ pattern requires the scrutinee to have a refined type
|}]

(* Rigid refined types: predicates compare structurally, so v > 0 and
   0 < v are DIFFERENT types (a documented sharp edge). *)
let pos : {v:int | v > 0} = refine_ 3
[%%expect{|
val pos : int{ _ > 0 } = 3
|}]

let y : {v:int | 0 < v} = pos
[%%expect{|
Line 1, characters 26-29:
1 | let y : {v:int | 0 < v} = pos
                              ^^^
Error: The value "pos" has type "int{ _ > 0 }"
       but an expression was expected of type "int{ 0 < _ }"
|}]

(* A refined and an unrefined branch do not unify: refinements never
   flow through unification (soundness: which branch's refinement
   "won" would be an implementation accident).  DESIGN.md's required
   counterexample. *)
let join (b : {v:int | v > 0}) (c : bool) = if c then 0 else b
[%%expect{|
Line 1, characters 61-62:
1 | let join (b : {v:int | v > 0}) (c : bool) = if c then 0 else b
                                                                 ^
Error: The value "b" has type "int{ _ > 0 }"
       but an expression was expected of type "int"
|}]

(* The argument for a dependent parameter must be a variable. *)
let lt : (x : int) -> (y : int) -> {z:bool | z = (x < y)} =
  fun x y -> assume_ (x < y)
[%%expect{|
val lt : (x : int) -> (y : int) -> bool{ _ = (x < y) } = <fun>
|}]

(* (Variables and literals have stable logical names; a compound
   expression does not.) *)
let bad (x : int) = lt (x * 2) x
[%%expect{|
Line 1, characters 23-30:
1 | let bad (x : int) = lt (x * 2) x
                           ^^^^^^^
Error: vox: the argument for a dependent parameter must be a variable (let-bind it first)
|}]

(* Escaped refinements are errors (DESIGN: "escape is an error").  An
   inferred function type whose refinement mentions the function's own
   parameter must be rejected: under recursion the same stamp names a
   different value per activation, so its facts would be unsound. *)
let dep : (x:int) -> {v:int | v = x} = fun x -> assume_ x
[%%expect{|
val dep : (x : int) -> int{ _ = x } = <fun>
|}]

let rec self (b : bool) (y : int) =
  if b then dep y
  else begin
    let refine_ r = self true 42 in
    let _w : {v:int | v = r} = refine_ y in
    dep y
  end
[%%expect{|
Line 1, characters 8-12:
1 | let rec self (b : bool) (y : int) =
            ^^^^
Error: vox: the type of self carries a refinement mentioning y, which may not appear in a module-level type; annotate with a dependent arrow ((y : ...) -> ...) or a self-contained refinement
|}]

(* Module-level types must have self-contained refinements: a stamp
   does not survive its compilation unit, so .cmi predicates may not
   mention program variables at all. *)
let forty_two = 42
[%%expect{|
val forty_two : int = 42
|}]

let get : {v:int | v = forty_two} = assume_ forty_two
[%%expect{|
Line 1, characters 4-7:
1 | let get : {v:int | v = forty_two} = assume_ forty_two
        ^^^
Error: vox: the type of get carries a refinement mentioning forty_two, which may not appear in a module-level type; annotate with a dependent arrow ((forty_two : ...) -> ...) or a self-contained refinement
|}]

(* Dependent application is keyed to the arrow, so labels, commuting
   and partial application substitute the right argument; what remains
   an error here is [use]'s own inferred type mentioning its parameter
   (annotate with a dependent arrow to export the dependency). *)
let g : l:int -> (x:int) -> int -> {v:int | v = x} =
  fun ~l:_ x _y -> assume_ x
[%%expect{|
val g : l:int -> (x : int) -> int -> int{ _ = x } = <fun>
|}]

let use (a : int) (b : int) = g a b
[%%expect{|
Line 1, characters 4-7:
1 | let use (a : int) (b : int) = g a b
        ^^^
Error: vox: the type of use carries a refinement mentioning a, which may not appear in a module-level type; annotate with a dependent arrow ((a : ...) -> ...) or a self-contained refinement
|}]

(* A dependent parameter cannot be commuted past: it would be omitted,
   and an omitted argument has no name to substitute. *)
let h : (x:int) -> l:int -> {v:int | v = x} = fun x ~l:_ -> assume_ x
[%%expect{|
val h : (x : int) -> l:int -> int{ _ = x } = <fun>
|}]

let part (n : int) = h ~l:n
[%%expect{|
Line 1, characters 21-22:
1 | let part (n : int) = h ~l:n
                         ^
Error: vox: a dependent parameter cannot be omitted
|}]

(* Cross-activation closures are rejected by instantiation: the
   parameter type is opened at the ARGUMENT of each call, so a closure
   refined at the current activation's variable does not match the
   type instantiated at the next activation's argument. *)
let rec unsound : (x:int) -> (unit -> {v:int | v = x}) option -> int =
  fun x prev ->
    ignore prev;
    if x = 0 then 0
    else
      let x2 = x - 1 in
      let f : unit -> {v:int | v = x} = fun () -> assume_ x in
      unsound x2 (Some f)
[%%expect{|
Line 8, characters 23-24:
8 |       unsound x2 (Some f)
                           ^
Error: The value "f" has type "unit -> int{ _ = x }"
       but an expression was expected of type "unit -> int{ _ = x2 }"
       Type "int{ _ = x }" is not compatible with type "int{ _ = x2 }"
|}]

(* Structure-level unpack is not supported (the scrutinee's type is
   not known when the pattern is typed); the error says what to do. *)
let refine_ w = pos
[%%expect{|
Line 1, characters 4-13:
1 | let refine_ w = pos
        ^^^^^^^^^
Error: vox: cannot determine the scrutinee's refined type here; unpack inside an expression ([let refine_ x = e in ...]) or bind with the refined type annotated instead
|}]

(* Escape enforcement is not limited to directly-walked binders: a
   binder inside a module structure would otherwise smuggle a stored
   closure between activations of a recursive function. *)
module M = struct let hidden_cell = ref None end
[%%expect{|
module M : sig val hidden_cell : '_weak1 option ref end
|}]

let rec cell_loop (x : int) (n : int) : int =
  (match (!M.hidden_cell : (unit -> {v:int | v = x}) option) with
   | Some g ->
     let refine_ r = g () in
     let _w : {v:int | v = x} = refine_ r in
     ()
   | None -> ());
  M.hidden_cell := Some (fun () -> (assume_ x : {v:int | v = x}));
  if n = 0 then 0 else cell_loop (x + 1) (n - 1)
[%%expect{|
Line 1, characters 22-33:
1 | module M = struct let hidden_cell = ref None end
                          ^^^^^^^^^^^
Error: vox: the type of hidden_cell carries a refinement mentioning x, which may not appear in a module-level type; annotate with a dependent arrow ((x : ...) -> ...) or a self-contained refinement
|}]

(* .cmi self-containment applies to every exported item, not just
   values: type manifests, record fields, and exception payloads may
   not mention program variables either. *)
type leak_alias = {v:int | v = forty_two}
[%%expect{|
Line 1, characters 0-41:
1 | type leak_alias = {v:int | v = forty_two}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: the type of type leak_alias carries a refinement mentioning forty_two, which may not appear in a module-level type; annotate with a dependent arrow ((forty_two : ...) -> ...) or a self-contained refinement
|}]

type leak_record = { fld : {v:int | v = forty_two} }
[%%expect{|
Line 1, characters 21-50:
1 | type leak_record = { fld : {v:int | v = forty_two} }
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: the type of type leak_record carries a refinement mentioning forty_two, which may not appear in a module-level type; annotate with a dependent arrow ((forty_two : ...) -> ...) or a self-contained refinement
|}]

exception Leak of {v:int | v = forty_two}
[%%expect{|
Line 1, characters 18-41:
1 | exception Leak of {v:int | v = forty_two}
                      ^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: the type of Leak carries a refinement mentioning forty_two, which may not appear in a module-level type; annotate with a dependent arrow ((forty_two : ...) -> ...) or a self-contained refinement
|}]

(* Embedded prelude blocks are validated at typing time: the payload
   must be a single string literal... *)
[%%vox.solver 42]
[%%expect{|
Line 1, characters 3-13:
1 | [%%vox.solver 42]
       ^^^^^^^^^^
Error: vox: unknown block extension "vox.solver" (expected "vox.lean")
|}]

(* ...and the backend suffix must name a known solver. *)
[%%vox.coq "Definition x := 3."]
[%%expect{|
Line 1, characters 3-10:
1 | [%%vox.coq "Definition x := 3."]
       ^^^^^^^
Error: vox: unknown block extension "vox.coq" (expected "vox.lean")
|}]

(* A well-formed block is accepted (and inert under -vox-dry-run). *)
[%%vox.lean {lean|@[grind] def spec_id (n : Int) : Int := n|lean}]
[%%expect{|
|}]

(* A dependent parameter under a polymorphic variant (or object /
   first-class module) cannot be substituted at applications; such
   types are rejected when the binder is opened. *)
let vf : (x:int) -> [ `A of {v:int | v = x} ] = fun x -> `A (assume_ x)
[%%expect{|
Line 1, characters 52-53:
1 | let vf : (x:int) -> [ `A of {v:int | v = x} ] = fun x -> `A (assume_ x)
                                                        ^
Error: vox: this dependent parameter occurs under a type the substitution cannot reach (an object, polymorphic variant, or first-class module); this is not supported
|}]

(* assume_ compiles a runtime check, which is only faithful to the
   logic for int- and bool-sorted operands: physical equality on other
   sorts is stricter than logical equality, so a coherent assumption
   could fail at run time.  Rejected; assume_unchecked_ is the escape
   hatch. *)
let strings () =
  let s = String.make 1 'a' in
  let t : string{ _ = s } = assume_ (String.make 1 'a') in
  (t :> string)
[%%expect{|
Line 3, characters 36-55:
3 |   let t : string{ _ = s } = assume_ (String.make 1 'a') in
                                        ^^^^^^^^^^^^^^^^^^^
Error: vox: assume_ compiles a runtime check of this refinement, but s is not an int or bool, so the runtime comparison would not agree with the logic's equality; use assume_unchecked_
|}]

let strings_unchecked () =
  let s = String.make 1 'a' in
  let t : string{ _ = s } = assume_unchecked_ (String.make 1 'a') in
  (t :> string) ^ s
[%%expect{|
val strings_unchecked : unit -> string = <fun>
|}]

(* Intro forms may not be stacked directly on one another (the markers
   would collapse onto a single node, hiding the inner obligation from
   the VC pass and the runtime check): let-bind the inner form. *)
let nested : {v: int{ _ > 0 } | v > 1} = refine_ (assume_ 2)
[%%expect{|
Line 1, characters 41-60:
1 | let nested : {v: int{ _ > 0 } | v > 1} = refine_ (assume_ 2)
                                             ^^^^^^^^^^^^^^^^^^^
Error: vox: refine_ applied directly to another refine_/assume_/assume_unchecked_ expression is not supported; let-bind the inner expression first
|}]

(* A dependent binder is not supported on an optional or position
   parameter: the caller may pass the option itself, so the binder
   would not name the value the definition sees. *)
let opt_dep : ?l:((x : string)) -> int -> int = fun ?l:_ n -> n
[%%expect{|
Line 1, characters 23-29:
1 | let opt_dep : ?l:((x : string)) -> int -> int = fun ?l:_ n -> n
                           ^^^^^^
Error: vox: a dependent binder is not supported on an optional or position parameter
|}]

(* A binder's name denotes the refined value only in the refinement at
   the top of its own annotation; in a nested refinement it is an
   ordinary (here unbound) variable. *)
let apply_nested (g : int -> int{ g > 0 }) = g
[%%expect{|
Line 1, characters 4-16:
1 | let apply_nested (g : int -> int{ g > 0 }) = g
        ^^^^^^^^^^^^
Error: vox: the type of apply_nested carries a refinement mentioning g, which may not appear in a module-level type; annotate with a dependent arrow ((g : ...) -> ...) or a self-contained refinement
|}]

(* Mutable variables may not appear in refinements: a mutable variable
   has no stable logical value, so facts recorded about it at different
   times would contradict (e.g. [a = m] before [m <- 1] and [b = m]
   after would prove [a = b]). *)
let mut_pred () =
  let mutable m = 0 in
  let a : {v:int | v = m} = assume_unchecked_ m in
  m <- 1;
  (a :> int)
[%%expect{|
Line 3, characters 23-24:
3 |   let a : {v:int | v = m} = assume_unchecked_ m in
                           ^
Error: vox: mutable variables may not appear in refinements
|}]

(* The same rejection at dependent applications: substituting a mutable
   variable's stamp would smuggle it into refinements. *)
let dep_imm : (x:int) -> {v:int | v = x} = fun x -> refine_ x
[%%expect{|
val dep_imm : (x : int) -> int{ _ = x } = <fun>
|}]

let mut_dep () =
  let mutable m = 0 in
  let refine_ a = dep_imm m in
  m <- 1;
  a
[%%expect{|
Line 3, characters 26-27:
3 |   let refine_ a = dep_imm m in
                              ^
Error: vox: the argument for a dependent parameter must be an immutable variable (let-bind it first)
|}]
