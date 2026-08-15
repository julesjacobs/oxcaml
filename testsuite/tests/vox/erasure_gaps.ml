(* TEST
 expect;
*)

(* Known conformance gaps on the erasure axis, found by review.

   Each block records CURRENT behaviour. Blocks still marked GAP are wrong.
   Fixing a gap means re-promoting this file, so the expectation diff shows
   exactly what changed. Do not "fix" a block by editing the expectation.

   Gaps 1a/1b/1d are FIXED: the laundering mechanism was the arrow-mode
   loosening in [Typecore.type_argument] ([loosen_arrow_modes]), which grants
   arrow arguments contravariant/covariant mode subsumption; the erasure
   component of the argument mode is now equated instead of loosened, so all
   three now agree with the explicit-coercion case. Gap 3 is FIXED by giving
   non-defaulted erased optionals the retained convention on the callee side
   too (matching the defaulted spelling and the caller), rather than the
   rejection anticipated below. Gap 2 (call-site inference) remains open.

   Whether an argument is passed is ABI, so the design requires erasure to be
   invariant in argument position: an arrow whose parameter is erased must not
   flow into an arrow whose parameter is retained, in either direction. An
   erased parameter really is zero-width — [f] below translates with its first
   parameter at layout [#()] and its second at [value<int>], so the emitted
   function takes one argument, not two — and a caller compiled against the
   laundered type passes two.

   The invariance rule is enforced, but only by the explicit [:>] subtype
   check. An ordinary type annotation goes through unification instead, where
   erasure still follows the usual contravariant comonadic submoding, and the
   erased parameter is silently forgotten. The piece's own invariance tests all
   use [:>], which is why this path was not covered. *)

let f : int @ erased -> int -> int = fun _ y -> y

[%%expect {|
val f : int @ erased -> int -> int = <fun>
|}]

(* ------------------------------------------------------------------ *)
(* FIXED (was GAP 1a): annotating a let-bound value used to drop the
   erasure. *)

let a : int -> int -> int = f

[%%expect {|
Line 1, characters 28-29:
1 | let a : int -> int -> int = f
                                ^
Error: The value "f" has type "int @ erased -> int -> int"
       but an expression was expected of type "int -> int -> int"
|}]

(* FIXED (was GAP 1b): the same laundering with no instantiation involved,
   which is what showed the mechanism was the [type_argument] loosening and
   not [instance]. *)

let b (p : int @ erased -> int -> int) : int -> int -> int = p

[%%expect {|
Line 1, characters 61-62:
1 | let b (p : int @ erased -> int -> int) : int -> int -> int = p
                                                                 ^
Error: The value "p" has type "int @ erased -> int -> int"
       but an expression was expected of type "int -> int -> int"
|}]

(* The comparison case, and the reason the gap survived review: spelled as an
   explicit coercion, the identical conversion is correctly rejected. Any fix
   should make 1a and 1b agree with this. *)

let c (p : int @ erased -> int -> int) = (p :> int -> int -> int)

[%%expect {|
Line 1, characters 41-65:
1 | let c (p : int @ erased -> int -> int) = (p :> int -> int -> int)
                                             ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "int @ erased -> int -> int" is not a subtype of "int -> int -> int"
|}]

(* FIXED (was GAP 1d): the same laundering through an ordinary polymorphic
   higher-order function. An erased-parameter function can no longer be
   passed where a retained-parameter arrow is expected; there is no single
   ABI that serves both, and OCaml does not monomorphize. *)

module App : sig
  val app : ('a -> int) -> 'a -> int
end = struct
  let app g x = g x
end

let d (x : int @ erased) = 42
let e = App.app d

[%%expect {|
module App : sig val app : ('a -> int) -> 'a -> int end
val d : int @ erased -> int = <fun>
Line 8, characters 16-17:
8 | let e = App.app d
                    ^
Error: The value "d" has type "int @ erased -> int"
       but an expression was expected of type "'a -> int"
|}]

(* ------------------------------------------------------------------ *)
(* GAP 2: a call site infers an unannotated parameter's erasure.

   The design says erasure is never inferred: a parameter is erased because it
   was annotated, so that the calling convention is visible in the source.
   Here one erased application is enough to change the parameter's mode, and
   with it the function's ABI.

   The module wrapper is load-bearing. At the end of a structure item a
   binding's mode variables are zapped to their legacy default, so at toplevel
   [h] is already pinned retained before the application is typed and the
   application is correctly rejected. Inside one structure the use is typed
   first, which is also the situation in an ordinary source file. *)

module M = struct
  let h x = 42
  let () = ignore (h (erased_ 5))
end

[%%expect {|
module M : sig val h : 'a @ erased -> int end
|}]

(* The same definition without the erased application stays retained, which is
   what shows the mode came from the call site and not the definition. *)

module N = struct
  let h x = 42
end

[%%expect {|
module N : sig val h : 'a -> int end
|}]

(* ------------------------------------------------------------------ *)
(* GAP 3: an erased optional parameter without a default gets the void
   representation on the callee side only.

   The design records the opposite as a settled decision: erased optional
   parameters keep the retained convention on both sides of a call, so caller
   and callee agree. The caller side does that — [Typeopt.function_arg_erasures]
   answers [false] for [Optional _]. The callee side never looks at the label:
   [Translcore] marks any [Tparam_pat] whose mode is erased. A *defaulted*
   optional is [Tparam_optional_default] and so escapes the marking, but an
   optional without a default is [Tparam_pat] and is given the void layout,
   against a caller that passes a word.

   So this declaration is accepted, and the program it produces aborts:
   compiled natively it dies with a flambda2
   [Direct_application_parameter_kind_mismatch (params_arity void x int)
   against (args_arity V x int)], SIGABRT, exit 134. The defaulted spelling
   runs correctly, which is what isolates the label as the cause.

   FIXED, by the other route than anticipated here: instead of rejecting the
   declaration, the callee-side marking now skips Optional-labelled
   parameters (like the caller side already did), so both spellings keep the
   retained convention and agree. Rejection remains an option if accepting
   modally-erased-but-physically-passed optionals proves confusing. *)

let opt ?a:(a : int option @ erased) () = 1

[%%expect {|
Line 1, characters 8-43:
1 | let opt ?a:(a : int option @ erased) () = 1
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Optional parameters cannot be erased: there is no erased calling
       convention for an optional argument, which is physically passed
       as an option.
|}]

(* The defaulted spelling, which takes the other translation path and agrees
   with its caller. *)

let opt_defaulted ?(a : int @ erased = 0) () = 1

[%%expect {|
Line 1, characters 18-48:
1 | let opt_defaulted ?(a : int @ erased = 0) () = 1
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Optional parameters cannot be erased: there is no erased calling
       convention for an optional argument, which is physically passed
       as an option.
|}]

(* ------------------------------------------------------------------ *)
(* Round-2 soundness review: positions that used to launder or observe an
   erased value at run time, now closed. Each was a VERIFIED repro (segfault,
   silent wrong value, or a caller/callee ABI mismatch) before the fix. *)

(* An [(e :> ty)] coercion whose target is not closed went through
   [build_subtype], a fourth arrow-mode loosening path that erasure invariance
   had not reached. It laundered an erased parameter into a retained arrow;
   calling the result read a real word as the erased (absent) parameter. *)
let f (s : string @ erased) (y : int) = y + 1
let g = (f :> string -> int -> _)
[%%expect{|
val f : string @ erased -> int -> int = <fun>
Line 2, characters 9-10:
2 | let g = (f :> string -> int -> _)
             ^
Error: This expression cannot be coerced to type ""string -> int -> 'a"";
       it has type "string @ erased -> int -> int" but is here used with type
         "string -> int -> 'a"
|}]

let launder (f : 'a @ erased -> int) = (f :> 'a -> int)
[%%expect{|
Line 1, characters 40-41:
1 | let launder (f : 'a @ erased -> int) = (f :> 'a -> int)
                                            ^
Error: This expression cannot be coerced to type ""'a0 -> int""; it has type
         "'a @ erased -> int"
       but is here used with type "'a -> int"
|}]

(* Object coercion is the idiomatic trigger of the same path (open row ->
   not closed). *)
let f (o : < m : string @ erased -> int; .. >) = (o :> < m : string -> int >)
[%%expect{|
Line 1, characters 50-51:
1 | let f (o : < m : string @ erased -> int; .. >) = (o :> < m : string -> int >)
                                                      ^
Error: This expression cannot be coerced to type ""< m : string -> int >"";
       it has type "< m : string @ erased -> int; .. >"
       but is here used with type "< m : string -> int; .. >"
       The method "m" has type "string @ erased -> int",
       but the expected method type was "string -> int"
|}]

(* Optional parameters cannot be erased: an optional is physically passed as
   an option, so there is no erased calling convention for it. Both the
   definition site and a written signature are rejected. *)
let opt ?(a : int @ erased = 0) () = 1
[%%expect{|
Line 1, characters 8-38:
1 | let opt ?(a : int @ erased = 0) () = 1
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Optional parameters cannot be erased: there is no erased calling
       convention for an optional argument, which is physically passed
       as an option.
|}]

let opt2 ?a:(_ : int option @ erased) () = 1
[%%expect{|
Line 1, characters 9-44:
1 | let opt2 ?a:(_ : int option @ erased) () = 1
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Optional parameters cannot be erased: there is no erased calling
       convention for an optional argument, which is physically passed
       as an option.
|}]

(* An external cannot have an erased parameter anywhere in its type, including
   in a result arrow (a stub that returns a closure). *)
external ext : unit -> (int @ erased -> int) = "caml_id"
[%%expect{|
Line 1, characters 15-44:
1 | external ext : unit -> (int @ erased -> int) = "caml_id"
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: External declarations cannot have erased parameters:
       nothing would be passed for them across the FFI.
|}]

(* A module is a runtime block, so an erased value cannot be stored in one,
   including a local module reached through [let open struct ... end]. *)
let f () = let open struct let x = erased_ 5 let y = 42 end in ignore x; y
[%%expect{|
Line 1, characters 31-32:
1 | let f () = let open struct let x = erased_ 5 let y = 42 end in ignore x; y
                                   ^
Error: The expression is "erased"
       but is expected to be "retained"
         because it is the value "x" in the structure at line 1, characters 27-55
         which is expected to be "retained"
         because modules always need to be allocated on the heap.
|}]
