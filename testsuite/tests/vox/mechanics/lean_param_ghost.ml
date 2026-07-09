(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* PARAMETERIZED ghost sorts (STAGE 4): [type 'a iset [@@vox.sort lean
   "ISet"]] names a parameterized Lean type, so [int iset] models at
   [(ISet Int)] and the nested [int iset iset] at [(ISet (ISet Int))].
   Argument sorts instantiate positionally (like [Vs_data]) and render
   parenthesized; an opaque argument instantiates at [VoxU].  A model
   that CONSTRAINS its parameter (here [DecidableEq a] on a decidable
   membership) fails CLOSED at the solver for an instantiation lacking
   the instance -- never a silent pass.  See DESIGN.md. *)

type 'a iset [@@vox.sort lean "ISet"]

[%%vox.lean {lean|
inductive ISet (a : Type) where
  | nil : ISet a
  | cons : a -> ISet a -> ISet a

@[grind] def mem {a : Type} (x : a) : ISet a -> Prop
  | .nil => False
  | .cons y s => x = y ∨ mem x s

@[grind] def ins {a : Type} (x : a) (s : ISet a) : ISet a := ISet.cons x s

@[grind] theorem mem_ins {a : Type} (x : a) (s : ISet a) : mem x (ins x s) := by
  grind
grind_pattern mem_ins => mem x (ins x s)

@[grind] def tmem {a : Type} [DecidableEq a] (x : a) : ISet a -> Bool
  | .nil => false
  | .cons y s => if x = y then true else tmem x s
|lean}]

(* An [int iset] binder models at [(ISet Int)]: [mem x (ins x s)] holds
   by the exported lemma, so the argument sort [Int] is really in play. *)
let at_int : (x : int) -> (s : int iset) -> unit{ mem x (ins x s) } =
  fun x s -> ignore (x, s); ()
[%%expect{|
type 'a iset
val at_int : (x : int) -> (s : int iset) -> unit{ mem x (ins x s) } = <fun>
|}]

(* NESTED argument sort: [int iset iset] is [(ISet (ISet Int))] -- a set
   whose elements are int-sets.  The membership at the OUTER sort takes
   an [int iset] element, so both instantiations are exercised at once. *)
let nested : (s : int iset) -> (ss : int iset iset) -> unit{ mem s (ins s ss) } =
  fun s ss -> ignore (s, ss); ()
[%%expect{|
val nested :
  (s : int iset) -> (ss : int iset iset) -> unit{ mem s (ins s ss) } = <fun>
|}]

(* OVERCLAIM at [int]: [mem x s] follows from no hypothesis. *)
let overclaim : (x : int) -> (s : int iset) -> unit{ mem x s } =
  fun x s -> ignore (x, s); ()
[%%expect{|
Line 2, characters 28-30:
2 |   fun x s -> ignore (x, s); ()
                                ^^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: mem x s
Hypotheses: <none>
(lean: error: `grind` failed)
|}]

(* TYPECLASS fail-closed: [tmem] needs [DecidableEq a].  At an opaque
   element type ([opaque_elt iset] is [(ISet VoxU)]) no such instance
   exists, so the solver rejects at the DecidableEq layer.  VoxU IS
   emitted (the argument sort renders), so the failure is typeclass
   synthesis, not an unknown identifier -- fail closed at the right
   layer. *)
type opaque_elt

let fail_closed
  : (x : opaque_elt) -> (s : opaque_elt iset{ tmem x _ = true })
    -> unit{ tmem x s = true } =
  fun x s -> ignore (x, s); ()
[%%expect{|
type opaque_elt
Line 6, characters 28-30:
6 |   fun x s -> ignore (x, s); ()
                                ^^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: tmem x s = true
Hypotheses:
  tmem x s = true
(lean: error(lean.synthInstanceFailed): failed to synthesize instance of type class)
|}]
