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

(* Unpacking an UNREFINED scrutinee is dead code: under binders as
   facts every binder already binds at the skeleton, so there is
   nothing to unpack -- a plain [let] carries the self fact. *)
let f (x : int) = let refine_ w = x in w
[%%expect{|
Line 1, characters 22-31:
1 | let f (x : int) = let refine_ w = x in w
                          ^^^^^^^^^
Error: vox: a refine_ pattern requires the scrutinee to have a refined type (a plain let binds at the skeleton and carries the fact already)
|}]

(* Rigid refined types: predicates compare structurally, so v > 0 and
   0 < v are DIFFERENT types.  At a check position a VARIABLE is
   implicitly re-refined -- a proof obligation rather than a type
   error (see infer.ml). *)
let pos : {v:int | v > 0} = refine_ 3
[%%expect{|
val pos : int{ _ > 0 } = 3
|}]

let y : {v:int | 0 < v} = pos
[%%expect{|
val y : int{ 0 < _ } = 3
|}]

(* Under a type constructor there is no implicit step: refinements
   unify only with structurally equal predicates, so v > 0 and 0 < v
   remain distinct (the documented sharp edge). *)
let xs : {v:int | v > 0} list = [pos]
[%%expect{|
val xs : int{ _ > 0 } list = [3]
|}]

let ys : {v:int | 0 < v} list = xs
[%%expect{|
Line 1, characters 32-34:
1 | let ys : {v:int | 0 < v} list = xs
                                    ^^
Error: The value "xs" has type "int{ _ > 0 } list"
       but an expression was expected of type "int{ 0 < _ } list"
       Type "int{ _ > 0 }" is not compatible with type "int{ 0 < _ }"
|}]

(* Refinements never flow through unification -- and under binders as
   facts they no longer try: the parameter [b] binds at the skeleton
   [int] (its refinement is a fact at its stamp), so the join is
   plainly [int] in either branch order.  DESIGN.md's counterexample,
   now simply well-typed. *)
let join (b : {v:int | v > 0}) (c : bool) = if c then 0 else b
[%%expect{|
val join : int{ _ > 0 } -> bool -> int = <fun>
|}]

(* The argument for a dependent parameter must have a stable logical
   name: a variable, a literal, or a pure expression over the
   reflected operations (recognized by what they resolve to). *)
let lt : (x : int) -> (y : int) -> {z:bool | z = (x < y)} =
  fun x y -> assume_ (x < y)
[%%expect{|
val lt : (x : int) -> (y : int) -> bool{ _ = (x < y) } = <fun>
|}]

(* A translatable compound argument names itself. *)
let ok (x : int) =
  let refine_ b = lt (x * 2) x in
  b
[%%expect{|
val ok : int -> bool = <fun>
|}]

(* An expression the logic cannot name is still an error. *)
let bad (x : int) = lt (abs x) x
[%%expect{|
Line 1, characters 23-30:
1 | let bad (x : int) = lt (abs x) x
                           ^^^^^^^
Error: vox: this argument for a dependent parameter cannot be named in the logic: it is neither a reflectable expression (a variable, literal, arithmetic, constructor, field read, or reflected call) nor a call with an exact result contract; bind it with a let first
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

(* A dependent parameter commuted past is DEFERRED, not omitted: the
   binder stays unopened and the partial application's type rebinds it
   (see commute.ml for the full walkthrough). *)
let h : (x:int) -> l:int -> {v:int | v = x} = fun x ~l:_ -> assume_ x
[%%expect{|
val h : (x : int) -> l:int -> int{ _ = x } = <fun>
|}]

let part (n : int) = h ~l:n
[%%expect{|
val part : int -> (x : int) -> int{ _ = x } = <fun>
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
Error: vox: assume_ compiles a runtime check of this refinement, but the checked value has a sort the check cannot evaluate faithfully (only ints, bools, and datatypes built from them can be checked); use assume_unchecked_
|}]

let strings_unchecked () =
  let s = String.make 1 'a' in
  let t : string{ _ = s } = assume_unchecked_ (String.make 1 'a') in
  (t :> string) ^ s
[%%expect{|
val strings_unchecked : unit -> string = <fun>
|}]

(* A literal nested refinement FLATTENS (the layers conjoin on the
   underlying skeleton, mechanics/flatten.ml), so the TYPE is fine;
   the inner intro then sits at the bare skeleton and errors. *)
let nested : {v: int{ _ > 0 } | v > 1} = refine_ (assume_ 2)
[%%expect{|
Line 1, characters 49-60:
1 | let nested : {v: int{ _ > 0 } | v > 1} = refine_ (assume_ 2)
                                                     ^^^^^^^^^^^
Error: vox: assume_ used where the expected type is not refined
|}]

(* Stacking intro forms is rejected -- here the annotated inner intro
   meets the outer's bare skeleton and rigid unification refuses;
   let-bind the inner form. *)
let stacked : {v:int | v > 1} = refine_ ((assume_ 2 : int{ _ > 0 }))
[%%expect{|
Line 1, characters 40-68:
1 | let stacked : {v:int | v > 1} = refine_ ((assume_ 2 : int{ _ > 0 }))
                                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "int{ _ > 0 }"
       but an expression was expected of type "int"
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

(* A reflected [match] on bool is rejected up front (bool passes the
   simple-variant test but is a proposition on the solver side); the
   fix -- [if] -- is spelled out. *)
let rec total_ bmatch (b : bool) =
  match b with
  | true -> 1
  | false -> 0
[%%expect{|
Line 2, characters 8-9:
2 |   match b with
            ^
Error: vox: a reflected function cannot [match] on bool (the solver models bool as a proposition); use [if] instead
|}]

(* R1 (kinds study): order / arithmetic / tuple-projection applied to a
   value at an uninterpreted solver sort (fixed-width int64#, float#, or an
   unboxed product) is rejected HERE, naming the operand and its sort --
   rather than reaching Lean and failing instance synthesis, surfaced as a
   misleading "NOT PROVED (may still hold)".  Equality at such a sort stays
   ACCEPTED (the (b) cells below): uninterpreted equality is sound. *)
let ord_i64 : (x : int64#) -> int64#{ _ > x } = fun x -> x
[%%expect{|
Line 1, characters 57-58:
1 | let ord_i64 : (x : int64#) -> int64#{ _ > x } = fun x -> x
                                                             ^
Error: vox: the operator (>) needs Int operands, but "x" is modeled at an opaque sort; only equality (= and <>) is available for this kind (fixed-width and unboxed types are left uninterpreted)
|}]

let arith_fu : (x : float#) -> int{ _ = x + 1 } = fun x -> 0
[%%expect{|
Line 1, characters 59-60:
1 | let arith_fu : (x : float#) -> int{ _ = x + 1 } = fun x -> 0
                                                               ^
Error: vox: the operator (+) needs Int operands, but "x" is modeled at an opaque sort; only equality (= and <>) is available for this kind (fixed-width and unboxed types are left uninterpreted)
|}]

let proj_uprod : (x : #(int * int)) -> int{ _ = fst x } =
  fun x -> let #(a, _) = x in a
[%%expect{|
Line 2, characters 30-31:
2 |   fun x -> let #(a, _) = x in a
                                  ^
Error: vox: fst/snd project a tuple, but "x" is modeled at an opaque sort (unboxed products #( ) are not modeled; only boxed tuples project)
|}]

(* Preserved (b) cells: equality/disequality at an uninterpreted sort must
   NOT be over-rejected. *)
let eq_fu : (x : float#) -> float#{ _ = x } = fun x -> x
[%%expect{|
val eq_fu : (x : float#) -> float#{ _ = x } = <fun>
|}]

let eq_uprod : (x : #(int * int)) -> #(int * int){ _ = x } = fun x -> x
[%%expect{|
val eq_uprod : (x : #(int * int)) -> #(int * int){ _ = x } = <fun>
|}]
