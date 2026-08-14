(* TEST
 expect;
*)

(* Known conformance gaps on the erasure axis, found by review.

   Each block records CURRENT behaviour, which is wrong. Fixing a gap means
   re-promoting this file, so the expectation diff shows exactly what changed.
   Do not "fix" a block by editing the expectation.

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
(* GAP 1a: annotating a let-bound value drops the erasure. Referencing [f]
   instantiates it, so this is also the arrow-mode instantiation route: the
   generic arrow's modes are copied into fresh independent variables, which is
   sound for locality but not for erasure. *)

let a : int -> int -> int = f

[%%expect {|
val a : int -> int -> int = <fun>
|}]

(* GAP 1b: the same laundering with no instantiation involved, which is what
   shows the gap is not specific to [instance]. [p] is lambda-bound, its type
   is fixed by the annotation, and returning it at a retained-parameter arrow
   is still accepted. *)

let b (p : int @ erased -> int -> int) : int -> int -> int = p

[%%expect {|
val b : (int @ erased -> int -> int) -> int -> int -> int = <fun>
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

(* GAP 1d: the same laundering through ordinary sealing of an ordinary
   polymorphic higher-order function, with no mode annotation anywhere in the
   generic code. This is the form that shows the footprint is not exotic: any
   [('a -> int) -> 'a -> int] in a signature will do it. *)

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
val e : int -> int = <fun>
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

   Recorded here as the accepted declaration, because the fix is expected to
   reject it the way externals with erased parameters are already rejected:
   the erased optional convention does not exist. When that lands this block
   becomes an error block, which is the diff a reviewer should see. *)

let opt ?a:(a : int option @ erased) () = 1

[%%expect {|
val opt : ?a:int @ erased -> unit -> int = <fun>
|}]

(* The defaulted spelling, which takes the other translation path and agrees
   with its caller. *)

let opt_defaulted ?(a : int @ erased = 0) () = 1

[%%expect {|
val opt_defaulted : ?a:int @ erased -> unit -> int = <fun>
|}]
