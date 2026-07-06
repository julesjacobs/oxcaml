(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* The EXTENSIONAL set model, transparent (the sealed version is
   lib/xset.mli + lean_xset_seal.ml).  [ISet := Int -> Prop] is Lean's
   actual set type; [mem]/[ins]/[uni]/[emp] are its algebra, and
   [iset_ext] gives EXTENSIONALITY -- two sets with the same members are
   equal ([ins_idem]).  This is the counterpoint to lean_via.ml's
   inductive-list model: that model has a decidable [card] observable
   (so overclaims print concrete counterexamples) but is NOT extensional;
   this one is extensional but has no [card], so overclaims fail
   WITHOUT a witness (see [overclaim] below).  Two ends of the via
   modeling spectrum. *)

type tree = Leaf | Node of tree * int * tree
type iset [@@vox.sort lean "ISet"]

[%%vox.lean {lean|
@[expose] def ISet := Int -> Prop

@[grind] def mem (x : Int) (s : ISet) : Prop := s x
@[grind] def emp : ISet := fun _ => False
@[grind] def ins (x : Int) (s : ISet) : ISet := fun y => y = x ∨ s y
@[grind] def uni (a b : ISet) : ISet := fun y => a y ∨ b y

@[grind] theorem mem_emp (x : Int) : ¬ mem x emp := by grind
@[grind] theorem mem_ins_iff (x y : Int) (s : ISet) :
    mem x (ins y s) ↔ (x = y ∨ mem x s) := by grind
@[grind] theorem mem_uni (x : Int) (a b : ISet) :
    mem x (uni a b) ↔ (mem x a ∨ mem x b) := by grind
@[grind] theorem mem_ins (x : Int) (s : ISet) : mem x (ins x s) := by grind

theorem iset_ext (a b : ISet) (h : ∀ x, mem x a ↔ mem x b) : a = b := by
  funext x
  have hx : a x ↔ b x := h x
  exact propext hx
@[grind] theorem ins_idem (x : Int) (s : ISet) : ins x (ins x s) = ins x s :=
  iset_ext _ _ (by grind)
@[grind] theorem uni_emp (s : ISet) : uni s emp = s := iset_ext _ _ (by grind)

@[grind] def bst : Vox_tree -> Prop
  | .Leaf => True
  | .Node l _ _ => bst l
@[grind] def elems : Vox_tree -> ISet
  | .Leaf => emp
  | .Node l v r => ins v (uni (elems l) (elems r))
@[grind] def tmem (x : Int) : Vox_tree -> Bool
  | .Leaf => false
  | .Node l v r => if x = v then true else (tmem x l || tmem x r)
@[grind] theorem tmem_elems (x : Int) (u : Vox_tree) :
    (tmem x u = true) = mem x (elems u) := by
  induction u <;> grind
grind_pattern tmem_elems => mem x (elems u)
|lean}]
type t = tree{ bst _ } [@vox.via (elems : iset)]
[%%expect{|
type tree = Leaf | Node of tree * int * tree
type iset
type t = tree{ bst _ via (elems : iset) }
|}]

(* [add] proves honestly: rebuild [Node (t0, x, Leaf)] and discharge
   [elems (Node ..) = ins x s] through the unpack link and [uni_emp]. *)
let add : (x : int) -> (s : t) -> t{ _ = ins x s } =
  fun x s ->
    let refine_ t0 = s in
    (Node (t0, x, Leaf) : t{ _ = ins x s })
[%%expect{|
val add :
  (x : int) ->
  (s : t) -> tree{ bst _ && elems _ = ins x s via (elems : iset) } = <fun>
|}]

(* [member]: each recursive result is let-bound so its spec fact is
   named (a refined bool as a bare [if] condition does not thread its
   fact). *)
let member : (x : int) -> (s : t) -> bool{ _ = mem x s } =
  fun x s ->
    let refine_ t0 = s in
    let rec go : (u : tree) -> bool{ _ = mem x (elems u) } =
      fun u ->
        match u with
        | Leaf -> false
        | Node (l, v, r) ->
          if x = v then true
          else
            let bl = go l in
            if bl then true else go r
    in
    go t0
[%%expect{|
val member : (x : int) -> (s : t) -> bool{ _ = mem x s } = <fun>
|}]

(* The solver runs on the Prop model and rejects a false claim
   fail-closed: [member x s] is [mem x s], and [_ = mem (x + 1) s] is
   refuted -- here WITH an integer witness [x = 0], because the claim
   turns on an [Int].  The [Int -> Prop] trade-off (no [card]) bites
   only for a false SET-LEVEL equality with no integer to pin, which
   fails WITHOUT a witness -- see [overclaim_setlevel] at the end. *)
let overclaim : (x : int) -> (s : t) -> bool{ _ = mem (x + 1) s } =
  fun x s -> member x s
[%%expect{|
Line 2, characters 13-23:
2 |   fun x s -> member x s
                 ^^^^^^^^^^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: *unknown6* = mem (x + 1) s
Hypotheses:
  *unknown6* = mem x s
(lean: error: `grind` failed)
|}]

(* WITNESS-FREE FAILURE (contrast with [overclaim] above).  A false
   SET-LEVEL claim -- an arbitrary [s] equals the empty set -- turns on
   no [Int], so the [Int -> Prop] model has nothing to enumerate: the
   solver rejects it fail-closed with NO "Possible counterexample" line,
   where lean_via.ml's [card] model would print one.  This is the
   witness loss the earlier int-turned overclaim could not express; it
   is expressible now that a bare 0-ary constant [emp] resolves in a
   refinement. *)
let overclaim_setlevel : (s : iset) -> unit{ s = emp } =
  fun s -> ()
[%%expect{|
Line 2, characters 11-13:
2 |   fun s -> ()
               ^^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: s = emp
Hypotheses: <none>
(lean: error: `grind` failed)
|}]
