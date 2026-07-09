(* Vpmap_ord: the PARAMETER-STYLE polymorphic-KEY map -- the poly fallback
   for [Vmap_make.Make(ORD)] when the key is a GENUINELY ABSTRACT type ['a]
   (no [@@vox.sort int], so no DecidableEq at its Lean sort).  Instead of a
   functor argument, the comparator is threaded as a CALL-SITE VALUE
   (WP-2's eq-param route, per Vpset): each query takes a total key relation
   [e] (the reflected [%[@vox.total]%] equality) and a runtime comparator
   [cmp] whose zero-case tracks [e].  Values are [int]; the key is the
   abstract parameter ['a].  ZERO trust.

   THE BOUNDARY vs the functor (why this is the FALLBACK, not the headline):
   - [mem] SHIPS: key-presence is PROPOSITIONAL ([mp_haskeyr], a relational
     OR up to [e]), and the threaded [cmp] decides it at runtime -- exactly
     Vpset.mem.  This is the one poly query that works at an abstract key.
   - [find] is ABSENT.  find must return a FUNCTIONAL VALUE (the binding for
     the matching key); at an abstract key the MODEL cannot choose that
     value without [DecidableEq 'a], which does not exist at the element
     sort (the poly study F-X1 wall, same one that blocks Vpset's bool
     [mem]-as-run and [remove]-elaboration).  A relational find-spec +
     inductive discharge could express it (heavier; not shipped).  A client
     that needs VALUES BY KEY uses the concrete-ordered route
     ([Vmap_make.Make] / [Vpmap]), where the key models at [Int] and the
     lookup is a real function.  THIS is the crisp functor-vs-param delta.
   - No ordered iteration / no [cardinal] / no canonical form: the repr is a
     prepend assoc-list (first-binding-wins), scanned linearly; the
     comparator's FULL ordered contract (the [<] part) is unused here -- only
     its [= 0 <-> key-equality] case is needed, since an unordered assoc map
     needs key EQUALITY, not order.  Ordering buys the functor its BST
     efficiency + ordered fold; the param-style fallback forgoes both.

   Ships (all equality/order-free in the REPR, Vpset shape): [empty]
   (unspecced -- F-B2 nullary via producer), [singleton], [add] (prepend,
   structural [_ = mp_cons k v m]), and [mem] (eq/ord-param query).  The one
   law [mp_haskeyr_cons] connects presence-after-add to the threaded [e];
   proven load-bearing by deletion (clients/smoke_vpmap_ord.ml). *)

open Vhof

type 'a mmap [@@vox.sort lean "MMapP"]
type mopt = MNone | MSome of int
type 'a t : value refines ('a mmap)

[%%vox.lean {lean|
public inductive MMapP (a : Type) where
  | MNil : MMapP a
  | MCons : a -> Int -> MMapP a -> MMapP a

-- key PRESENCE up to a client key-relation [e] (the Vpset ps_memr shape: a
-- relational OR, never an [if] -- no DecidableEq needed).  Recursive over
-- the abstract MMapP, so [expose] leaves it load-bearing.
@[grind, expose] public def mp_haskeyr {a : Type} (e : a -> a -> Prop) (k : a) :
    MMapP a -> Prop
  | .MNil => False
  | .MCons k' _ t => eqHolds e k k' ∨ mp_haskeyr e k t

-- add / empty as OPAQUE producers (obligation pattern): [mp_cons] is a
-- non-recursive prepend, so an exposed def would let grind discharge the
-- presence-after-add law by unfolding -- the shipped law would be dead.
public axiom mp_nil {a : Type} : MMapP a
public axiom mp_cons {a : Type} : a -> Int -> MMapP a -> MMapP a

-- THE law (LIVE under the opaque producers): presence after a prepend is
-- "key e-matches the new key, or was present before".
public axiom mp_haskeyr_cons {a : Type} (e : a -> a -> Prop) (k k' : a)
    (v : Int) (m : MMapP a) :
    mp_haskeyr e k (mp_cons k' v m) = (eqHolds e k k' ∨ mp_haskeyr e k m)
grind_pattern mp_haskeyr_cons => mp_haskeyr e k (mp_cons k' v m)
public axiom mp_haskeyr_nil {a : Type} (e : a -> a -> Prop) (k : a) :
    mp_haskeyr e k (mp_nil : MMapP a) = False
grind_pattern mp_haskeyr_nil => mp_haskeyr e k (mp_nil : MMapP a)
|lean}]

val empty : (u : unit) -> 'a t
val singleton : (k : 'a) -> (v : int) -> 'a t{ _ = mp_cons k v mp_nil }
val add : (k : 'a) -> (v : int) -> (m : 'a t) -> 'a t{ _ = mp_cons k v m }

(* [mem e cmp k s] decides whether some binding's key e-matches [k], using
   the threaded comparator [cmp] (its [= 0] case tracks [e]).  [e] is the
   reflected total key relation; [cmp] is the runtime witness -- the SAME
   comparator a client would hand [Vmap_make.Make], reused as a value. *)
val mem :
  (e : (('a -> 'a -> bool) [@vox.total])) ->
  (cmp : ((x : 'a) -> (y : 'a) -> int{ (_ = 0) = eqHolds e x y })) ->
  (k : 'a) -> (s : 'a t) -> bool{ _ = mp_haskeyr e k s }
