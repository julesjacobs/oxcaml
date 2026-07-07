(* Pays Vplist.mli's obligations over a concrete cons-list repr, at the
   ABSTRACT element sort (so ONE proof serves every instantiation -- pset's
   image-binder discipline generalized from set to list).  The model defs are
   RESTATED here without [public] (the model-duplication tax), the via
   abstraction [pl_repr] maps the concrete ['a tree] onto [PList a], and each
   .mli axiom is discharged by a same-named, same-typed theorem.  [append]
   uses the gap #31 skeleton-threading pattern (as Vlist): its recursive [go]
   returns a refined SKELETON at the ['a tree] level and the via injection
   happens once, through a variable. *)
type 'a plist [@@vox.sort lean "PList"]
type 'a tree = Nil | Cons of 'a * 'a tree
type 'a t = 'a tree{ 0 = 0 } [@vox.via (pl_repr : 'a plist)]

[%%vox.lean {lean|
inductive PList (a : Type) where
  | PNil : PList a
  | PCons : a -> PList a -> PList a

@[grind] def pl_cons {a : Type} (x : a) (l : PList a) : PList a := .PCons x l

@[grind] def pl_isnil {a : Type} : PList a -> Prop
  | .PNil => True
  | .PCons _ _ => False

@[grind] def pl_nil {a : Type} : PList a := .PNil

@[grind, expose] def pl_len {a : Type} : PList a -> Int
  | .PNil => 0
  | .PCons _ t => 1 + pl_len t

@[grind, expose] def pl_mem {a : Type} (x : a) : PList a -> Prop
  | .PNil => False
  | .PCons y t => x = y ∨ pl_mem x t

@[grind, expose] def pl_app {a : Type} : PList a -> PList a -> PList a
  | .PNil, m => m
  | .PCons x t, m => .PCons x (pl_app t m)

@[grind] def pl_repr {a : Type} : Vox_Vplist_tree a -> PList a
  | .Nil => .PNil
  | .Cons x t => .PCons x (pl_repr t)

theorem pl_isnil_nil {a : Type} : pl_isnil (@pl_nil a) := by grind
grind_pattern pl_isnil_nil => pl_isnil (@pl_nil a)

@[grind] theorem pl_not_isnil_cons {a : Type} (x : a) (l : PList a) :
    ¬ pl_isnil (pl_cons x l) := by grind

theorem pl_len_nonneg {a : Type} (l : PList a) : pl_len l >= 0 := by
  induction l <;> grind
grind_pattern pl_len_nonneg => pl_len l

theorem pl_len_cons {a : Type} (x : a) (l : PList a) :
    pl_len (pl_cons x l) = 1 + pl_len l := by grind
grind_pattern pl_len_cons => pl_len (pl_cons x l)

theorem pl_len_app {a : Type} (p q : PList a) :
    pl_len (pl_app p q) = pl_len p + pl_len q := by
  induction p <;> grind
grind_pattern pl_len_app => pl_len (pl_app p q)

theorem pl_mem_cons {a : Type} (x y : a) (l : PList a) :
    pl_mem x (pl_cons y l) = (x = y ∨ pl_mem x l) := by grind
grind_pattern pl_mem_cons => pl_mem x (pl_cons y l)

theorem pl_mem_app {a : Type} (x : a) (p q : PList a) :
    pl_mem x (pl_app p q) = (pl_mem x p ∨ pl_mem x q) := by
  induction p <;> grind
grind_pattern pl_mem_app => pl_mem x (pl_app p q)
|lean}]

let empty : (u : unit) -> 'a t =
  fun _ -> (Nil : 'a t)

let cons : (x : 'a) -> (l : 'a t) -> 'a t{ _ = pl_cons x l } =
  fun x l ->
    let refine_ t0 = l in
    (Cons (x, t0) : 'a t{ _ = pl_cons x l })

let is_empty : (l : 'a t) -> bool{ _ = pl_isnil l } =
  fun l ->
    let refine_ t0 = l in
    (match t0 with
     | Nil -> (true : bool{ _ = pl_isnil l })
     | Cons (_, _) -> (false : bool{ _ = pl_isnil l }))

let length : (l : 'a t) -> int{ _ = pl_len l } =
  fun l ->
    let refine_ t0 = l in
    let rec go : (u : 'a tree) -> int{ _ = pl_len (pl_repr u) } =
      fun u ->
        match u with
        | Nil -> 0
        | Cons (_, r) -> let n = go r in 1 + n
    in
    go t0

let append : (p : 'a t) -> (q : 'a t) -> 'a t{ _ = pl_app p q } =
  fun p q ->
    let refine_ tp = p in
    let refine_ tq = q in
    let rec go : (u : 'a tree) -> 'a tree{ pl_repr _ = pl_app (pl_repr u) (pl_repr tq) } =
      fun u ->
        match u with
        | Nil -> (tq : 'a tree{ pl_repr _ = pl_app (pl_repr u) (pl_repr tq) })
        | Cons (x, r) ->
            let rest = go r in
            (Cons (x, rest) : 'a tree{ pl_repr _ = pl_app (pl_repr u) (pl_repr tq) })
    in
    let res = go tp in
    (res : 'a t{ _ = pl_app p q })
