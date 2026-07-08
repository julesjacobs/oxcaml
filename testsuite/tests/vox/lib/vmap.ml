(* Pays vmap.mli's obligations over the CONCRETE cons assoc-list.  The
   representation [alist] is a genuine prepend list of (key, value)
   entries; [m_repr] maps it structurally to the model [MList].  The
   interface's opaque model ops ([m_find]/[m_add]/[m_isempty]/[m_empty])
   are given real defs here, and the four laws become theorems the seal
   demands.  [add] is a single prepend (like Vlist.cons) -- it does NOT
   thread a recursive via result, so there is NO #31 site here; [find] is
   a one-path recursion branching on a primitive [k = k'] test (like
   Vlist.mem), so there is NO #32 site either.  See notes/vmap.md. *)

open Vlist

type mlist [@@vox.sort lean "MList"]
type mopt = MMiss | MFound of int
type alist = ANil | ACons of int * int * alist
type t = alist{ 0 = 0 } [@vox.via (m_repr : mlist)]

[%%vox.lean {lean|
inductive MList where
  | MNil : MList
  | MCons : Int -> Int -> MList -> MList

@[grind] def m_empty : MList := .MNil

@[grind] def m_isempty : MList -> Prop
  | .MNil => True
  | .MCons _ _ _ => False

@[grind] def m_find (k : Int) : MList -> Vox_Vmap_mopt
  | .MNil => .MMiss
  | .MCons k' v t => if k = k' then .MFound v else m_find k t

@[grind] def m_add (k v : Int) (m : MList) : MList := .MCons k v m

@[grind] def m_repr : Vox_Vmap_alist -> MList
  | .ANil => .MNil
  | .ACons k v t => .MCons k v (m_repr t)

theorem m_isempty_empty : m_isempty m_empty := by grind
grind_pattern m_isempty_empty => m_isempty m_empty

theorem m_find_empty (k : Int) : m_find k m_empty = .MMiss := by grind
grind_pattern m_find_empty => m_find k m_empty

theorem m_find_add_eq (k v : Int) (m : MList) :
    m_find k (m_add k v m) = .MFound v := by grind
grind_pattern m_find_add_eq => m_find k (m_add k v m)

theorem m_find_add_ne (k k' v : Int) (m : MList) (h : k ≠ k') :
    m_find k (m_add k' v m) = m_find k m := by grind
grind_pattern m_find_add_ne => m_find k (m_add k' v m)

-- remove drops EVERY binding for k (shadowed ones too), so find k misses.
@[grind] def m_remove (k : Int) : MList -> MList
  | .MNil => .MNil
  | .MCons k' v t => if k = k' then m_remove k t else .MCons k' v (m_remove k t)

-- The find-characterization of remove, by induction: this is the genuine
-- proof (grind cannot instantiate the ∀ postcondition at goal indices without
-- it -- the loop-invariant-as-step-lemma discipline).
theorem m_remove_find (k k' : Int) (m : MList) :
    m_find k' (m_remove k m) = (if k' = k then .MMiss else m_find k' m) := by
  induction m <;> grind
grind_pattern m_remove_find => m_find k' (m_remove k m)

@[grind, expose] def m_remove_spec (r : MList) (k : Int) (m : MList) : Prop :=
  ∀ k', m_find k' r = (if k' = k then .MMiss else m_find k' m)

@[grind, expose] def m_agree (a : MList) (b : MList) : Prop :=
  ∀ k, m_find k a = m_find k b

@[grind, expose] def m_haskey (k : Int) (m : MList) : Prop :=
  m_find k m ≠ .MMiss
@[grind, expose] def m_keys_spec (l : LList) (m : MList) : Prop :=
  ∀ k, ll_mem k l = m_haskey k m
|lean}]

let empty : (u : unit) -> t{ _ = m_empty } =
  fun _ -> (ANil : t{ _ = m_empty })

let is_empty : (m : t) -> bool{ _ = m_isempty m } =
  fun m ->
    let refine_ t0 = m in
    let go : (u : alist) -> bool{ _ = m_isempty (m_repr u) } =
      fun u ->
        match u with
        | ANil -> true
        | ACons (_, _, _) -> false
    in
    go t0

let find : (k : int) -> (m : t) -> mopt{ _ = m_find k m } =
  fun k m ->
    let refine_ t0 = m in
    let rec go : (u : alist) -> mopt{ _ = m_find k (m_repr u) } =
      fun u ->
        match u with
        | ANil -> MMiss
        | ACons (k', v, r) -> if k = k' then MFound v else go r
    in
    go t0

let add : (k : int) -> (v : int) -> (m : t) -> t{ _ = m_add k v m } =
  fun k v m ->
    let refine_ t0 = m in
    (ACons (k, v, t0) : t{ _ = m_add k v m })

(* remove is a recursive via-returning op (like Vlist.append), so it threads
   a refined SKELETON (m_repr _ = m_remove k (m_repr u)) through the recursion
   and injects into t once (the #31 workaround); the branch is on the primitive
   k = k' (no #32).  m_remove_spec is discharged from m_remove_find. *)
let remove : (k : int) -> (m : t) -> t{ m_remove_spec _ k m } =
  fun k m ->
    let refine_ t0 = m in
    let rec go : (u : alist) -> alist{ m_repr _ = m_remove k (m_repr u) } =
      fun u ->
        match u with
        | ANil -> (ANil : alist{ m_repr _ = m_remove k (m_repr u) })
        | ACons (k', v, r) ->
            let rest = go r in
            if k = k'
            then (rest : alist{ m_repr _ = m_remove k (m_repr u) })
            else (ACons (k', v, rest) : alist{ m_repr _ = m_remove k (m_repr u) })
    in
    let res = go t0 in
    (res : t{ m_remove_spec _ k m })

(* keys eliminates the map into a Vlist of its keys (Mech A).  Building an
   EXTERNAL module's via type (Vlist) does NOT hit #31: the recursive Vlist.t
   result keeps its refinement across the let, so the membership spec threads
   directly (no skeleton needed, and none available -- Vlist's repr is hidden).
   Base closes by Vlist.ll_nil_not_mem; cons step by Vlist.ll_mem_cons.  The
   recursive [go r] flows straight into Vlist.cons's dependent list parameter
   (nested refined expressions -- no let-bind). *)
let keys : (m : t) -> Vlist.t{ m_keys_spec _ m } =
  fun m ->
    let refine_ t0 = m in
    let rec go : (u : alist) -> Vlist.t{ m_keys_spec _ (m_repr u) } =
      fun u ->
        match u with
        | ANil -> (Vlist.empty () : Vlist.t{ m_keys_spec _ (m_repr u) })
        | ACons (k, _, r) ->
            (Vlist.cons k (go r) : Vlist.t{ m_keys_spec _ (m_repr u) })
    in
    let res = go t0 in
    (res : Vlist.t{ m_keys_spec _ m })
