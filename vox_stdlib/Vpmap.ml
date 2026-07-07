(* Pays Vpmap.mli's obligations over the CONCRETE cons assoc-list,
   PARAMETERIZED on the value ['v] (the Pset mechanism applied to Vmap).
   The representation ['v alist] is a genuine prepend list of (key, value)
   entries; [m_repr] maps it structurally to the model [MList v].  The
   interface's opaque model ops are given real defs here, and the four
   laws + remove's spec become theorems the seal demands.  As in Vmap:
   [add] is a single prepend (NO #31), [find] branches on a primitive
   [k = k'] (NO #32); only [remove] is a recursive via-returning op and
   hits the #31 skeleton-thread.  See notes/vpmap.md. *)

type 'v mlist [@@vox.sort lean "MList"]
type 'v mopt = MMiss | MFound of 'v
type 'v alist = ANil | ACons of int * 'v * 'v alist
type 'v t = 'v alist{ 0 = 0 } [@vox.via (m_repr : 'v mlist)]

[%%vox.lean {lean|
inductive MList (v : Type) where
  | MNil : MList v
  | MCons : Int -> v -> MList v -> MList v

@[grind] def m_empty {v : Type} : MList v := .MNil

@[grind] def m_isempty {v : Type} : MList v -> Prop
  | .MNil => True
  | .MCons _ _ _ => False

@[grind] def m_find {v : Type} (k : Int) : MList v -> Vox_Vpmap_mopt v
  | .MNil => .MMiss
  | .MCons k' w t => if k = k' then .MFound w else m_find k t

@[grind] def m_add {v : Type} (k : Int) (w : v) (m : MList v) : MList v :=
  .MCons k w m

@[grind] def m_repr {v : Type} : Vox_Vpmap_alist v -> MList v
  | .ANil => .MNil
  | .ACons k w t => .MCons k w (m_repr t)

theorem m_isempty_empty {v : Type} : m_isempty (m_empty : MList v) := by grind
grind_pattern m_isempty_empty => m_isempty (m_empty : MList v)

theorem m_find_empty {v : Type} (k : Int) :
    m_find k (m_empty : MList v) = .MMiss := by grind
grind_pattern m_find_empty => m_find k (m_empty : MList v)

theorem m_find_add_eq {v : Type} (k : Int) (w : v) (m : MList v) :
    m_find k (m_add k w m) = .MFound w := by grind
grind_pattern m_find_add_eq => m_find k (m_add k w m)

theorem m_find_add_ne {v : Type} (k k' : Int) (w : v) (m : MList v)
    (h : k ≠ k') :
    m_find k (m_add k' w m) = m_find k m := by grind
grind_pattern m_find_add_ne => m_find k (m_add k' w m)

-- remove drops EVERY binding for k (shadowed ones too), so find k misses.
@[grind] def m_remove {v : Type} (k : Int) : MList v -> MList v
  | .MNil => .MNil
  | .MCons k' w t => if k = k' then m_remove k t else .MCons k' w (m_remove k t)

-- The find-characterization of remove, by induction: grind cannot
-- instantiate the forall postcondition at goal indices without it.
theorem m_remove_find {v : Type} (k k' : Int) (m : MList v) :
    m_find k' (m_remove k m) = (if k' = k then .MMiss else m_find k' m) := by
  induction m <;> grind
grind_pattern m_remove_find => m_find k' (m_remove k m)

@[grind, expose] def m_remove_spec {v : Type}
    (r : MList v) (k : Int) (m : MList v) : Prop :=
  ∀ k', m_find k' r = (if k' = k then .MMiss else m_find k' m)
|lean}]

let empty : (u : unit) -> 'v t =
  fun _ -> (ANil : 'v t)

let is_empty : (m : 'v t) -> bool{ _ = m_isempty m } =
  fun m ->
    let refine_ t0 = m in
    let go : (u : 'v alist) -> bool{ _ = m_isempty (m_repr u) } =
      fun u ->
        match u with
        | ANil -> true
        | ACons (_, _, _) -> false
    in
    go t0

let find : (k : int) -> (m : 'v t) -> 'v mopt{ _ = m_find k m } =
  fun k m ->
    let refine_ t0 = m in
    let rec go : (u : 'v alist) -> 'v mopt{ _ = m_find k (m_repr u) } =
      fun u ->
        match u with
        | ANil -> MMiss
        | ACons (k', w, r) -> if k = k' then MFound w else go r
    in
    go t0

let add : (k : int) -> (w : 'v) -> (m : 'v t) -> 'v t{ _ = m_add k w m } =
  fun k w m ->
    let refine_ t0 = m in
    (ACons (k, w, t0) : 'v t{ _ = m_add k w m })

(* remove is a recursive via-returning op (like Vmap.remove / Vlist.append),
   so it threads a refined SKELETON (m_repr _ = m_remove k (m_repr u))
   through the recursion and injects into ['v t] once (the #31 workaround);
   the branch is on the primitive k = k' (no #32).  m_remove_spec is
   discharged from m_remove_find. *)
let remove : (k : int) -> (m : 'v t) -> 'v t{ m_remove_spec _ k m } =
  fun k m ->
    let refine_ t0 = m in
    let rec go : (u : 'v alist) -> 'v alist{ m_repr _ = m_remove k (m_repr u) } =
      fun u ->
        match u with
        | ANil -> (ANil : 'v alist{ m_repr _ = m_remove k (m_repr u) })
        | ACons (k', w, r) ->
            let rest = go r in
            if k = k'
            then (rest : 'v alist{ m_repr _ = m_remove k (m_repr u) })
            else (ACons (k', w, rest) : 'v alist{ m_repr _ = m_remove k (m_repr u) })
    in
    let res = go t0 in
    (res : 'v t{ m_remove_spec _ k m })
