(* Pays Vpmap_ord.mli over a concrete prepend assoc-list of (key, int-value)
   entries, key abstract ['a] (the Vpset mechanism for a map).  [mp_repr]
   maps the repr to the model [MMapP a]; the opaque producers [mp_nil]/
   [mp_cons] and the presence law become obligations the seal demands.
   [mem] scans with the threaded comparator: its [= 0] case yields the key
   relation [e] (from cmp's contract), so the relational [mp_haskeyr] closes
   in each branch -- exactly Vpset.mem with a comparator in place of the
   bool decider.  ZERO trust; zero [assume_unchecked_]. *)

open Vhof

type 'a mmap [@@vox.sort lean "MMapP"]
type mopt = MNone | MSome of int
type 'a alist = ANil | ACons of 'a * int * 'a alist
type 'a t = 'a alist{ 0 = 0 } [@vox.via (mp_repr : 'a mmap)]

[%%vox.lean {lean|
inductive MMapP (a : Type) where
  | MNil : MMapP a
  | MCons : a -> Int -> MMapP a -> MMapP a

@[grind, expose] def mp_haskeyr {a : Type} (e : a -> a -> Prop) (k : a) :
    MMapP a -> Prop
  | .MNil => False
  | .MCons k' _ t => eqHolds e k k' ∨ mp_haskeyr e k t

@[grind] def mp_nil {a : Type} : MMapP a := .MNil
@[grind] def mp_cons {a : Type} (k : a) (v : Int) (m : MMapP a) : MMapP a :=
  .MCons k v m

@[grind] def mp_repr {a : Type} : Vox_Vpmap_ord_alist a -> MMapP a
  | .ANil => .MNil
  | .ACons k v t => .MCons k v (mp_repr t)

-- the presence laws, discharged over the concrete producers
theorem mp_haskeyr_cons {a : Type} (e : a -> a -> Prop) (k k' : a)
    (v : Int) (m : MMapP a) :
    mp_haskeyr e k (mp_cons k' v m) = (eqHolds e k k' ∨ mp_haskeyr e k m) := by
  simp only [mp_cons, mp_haskeyr]
grind_pattern mp_haskeyr_cons => mp_haskeyr e k (mp_cons k' v m)
theorem mp_haskeyr_nil {a : Type} (e : a -> a -> Prop) (k : a) :
    mp_haskeyr e k (mp_nil : MMapP a) = False := by
  simp only [mp_nil, mp_haskeyr]
grind_pattern mp_haskeyr_nil => mp_haskeyr e k (mp_nil : MMapP a)
|lean}]

let empty : (u : unit) -> 'a t =
  fun _ -> (ANil : 'a t)

let singleton : (k : 'a) -> (v : int) -> 'a t{ _ = mp_cons k v mp_nil } =
  fun k v ->
    (ACons (k, v, ANil) : 'a t{ _ = mp_cons k v mp_nil })

let add : (k : 'a) -> (v : int) -> (m : 'a t) -> 'a t{ _ = mp_cons k v m } =
  fun k v m ->
    let refine_ t0 = m in
    (ACons (k, v, t0) : 'a t{ _ = mp_cons k v m })

let mem :
    (e : (('a -> 'a -> bool) [@vox.total])) ->
    (cmp : ((x : 'a) -> (y : 'a) -> int{ (_ = 0) = eqHolds e x y })) ->
    (k : 'a) -> (s : 'a t) -> bool{ _ = mp_haskeyr e k s } =
  fun e cmp k s ->
    let refine_ t0 = s in
    let rec go : (u : 'a alist) -> bool{ _ = mp_haskeyr e k (mp_repr u) } =
      fun u ->
        match u with
        | ANil -> false
        | ACons (k', _, r) ->
            let c = cmp k k' in
            if c = 0 then true else go r
    in
    (go t0 : bool{ _ = mp_haskeyr e k s })
