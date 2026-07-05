(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* 0-ARY SPEC CONSTANTS in refinements (solverless).  A bare lowercase
   identifier that is neither the bound value / [_], nor an enclosing
   binder, nor any value in scope is read as a 0-ary spec constant --
   the nullary case of the spec-function namespace an APPLIED lowercase
   head already enters ([mem x s] is [Pfun ("mem", ...)]; [emp] is
   [Pfun ("emp", [])]).  Like every spec name, block constants are
   opaque to the compiler (they live only in the shipped block text and
   imported VoxSig), so the name is validated by the solver at VC time,
   not here.  See typing/typetexp.ml (the [exception _] arm of
   refinement-predicate name resolution).  The end-to-end proof that
   such a constant discharges is lib/xset.mli's [val empty : t{ _ = emp }]
   (proved in lean_xset_seal.ml).  Any name that IS a value in scope --
   a local, a module value, or a Stdlib value like [succ] -- resolves to
   that value first, so the constant fallback is a genuine last resort
   (this is also why the reserved builtin words never reach it). *)

type iset [@@vox.sort lean "ISet"]

[%%vox.lean {lean|
def ISet := Int -> Prop
@[grind] def mem (x : Int) (s : ISet) : Prop := s x
@[grind] def emp : ISet := fun _ => False
|lean}]
[%%expect{|
type iset
|}]

(* ELABORATION: a bare 0-ary constant [emp] resolves (it used to raise
   "unbound variable in refinement predicate").  The goal carries [emp]
   as a logical term. *)
let uses_emp : (s : iset) -> unit{ s = emp } =
  fun s -> ()
[%%expect{|
Line 2, characters 11-13: vox VC:
  goal: s = emp
  hypotheses: <none>
val uses_emp : (s : iset) -> unit{ s = emp } = <fun>
|}]

(* SHADOWING (dependent binder).  A binder named [emp] resolves to the
   BINDER, not the constant: name resolution checks enclosing binders
   before the constant fallback, so the goal is discharged from the
   binder's own fact. *)
let shadow_binder : (emp : int) -> (n : int{ _ = emp }) -> unit{ n = emp } =
  fun emp n -> ()
[%%expect{|
Line 2, characters 15-17: vox VC:
  goal: n = emp
  hypotheses:
  n = emp
val shadow_binder : (emp : int) -> (n : int{ _ = emp }) -> unit{ n = emp } =
  <fun>
|}]

(* SHADOWING (let-local).  A let-bound [emp] resolves to the local (a
   program value in scope wins over the constant fallback). *)
let shadow_local : int -> int =
  fun z ->
    let emp = z in
    let _ : unit{ emp = z } = () in
    emp
[%%expect{|
Line 4, characters 30-32: vox VC:
  goal: emp = z
  hypotheses:
  emp = z
val shadow_local : int -> int = <fun>
|}]

(* UNKNOWN bare name -> a 0-ary spec constant, NOT an elaboration error.
   [totallyunknownthing] elaborates as a constant; an unknown (or
   arity>0-used-bare) name is a solver error at VC time, exactly as for
   an applied spec function. *)
let unknown_is_constant : (s : iset) -> unit{ s = totallyunknownthing } =
  fun s -> ()
[%%expect{|
Line 2, characters 11-13: vox VC:
  goal: s = totallyunknownthing
  hypotheses: <none>
val unknown_is_constant : (s : iset) -> unit{ s = totallyunknownthing } =
  <fun>
|}]

(* GATED: in INVARIANT mode a bare unbound name is NOT a constant.  A
   loop invariant is a formula over program state, where a bare
   out-of-scope name is a scoping error (cf. mechanics/mutable.ml), so
   the existing "unbound variable" error is retained. *)
let invariant_gate : unit -> unit =
  fun () -> (while false do () done) [@vox.invariant nonexistent >= 0]
[%%expect{|
Line 2, characters 53-64:
2 |   fun () -> (while false do () done) [@vox.invariant nonexistent >= 0]
                                                         ^^^^^^^^^^^
Error: vox: unbound variable in refinement predicate
|}]
