(* Pays Vlist.mli's obligations over the concrete cons-list repr.  The
   model defs are RESTATED here without [public] (the model-duplication
   tax -- see notes/vlist.md), the via abstraction [ll_repr] maps the
   concrete [tree] onto [LList], and each .mli axiom is discharged by a
   same-named, same-typed theorem with identical grind_pattern.  [append]
   uses the gap #31 skeleton-threading workaround: its recursive [go]
   returns a refined SKELETON and the via injection happens once, through
   a variable (see notes/vlist.md). *)
type llist [@@vox.sort lean "LList"]
type tree = Nil | Cons of int * tree
type t = tree{ 0 = 0 } [@vox.via (ll_repr : llist)]

[%%vox.lean {lean|
inductive LList where
  | LNil : LList
  | LCons : Int -> LList -> LList

@[grind] def ll_cons (x : Int) (l : LList) : LList := .LCons x l

@[grind] def ll_isnil : LList -> Prop
  | .LNil => True
  | .LCons _ _ => False

@[grind] def ll_nil : LList := .LNil

@[grind] def ll_len : LList -> Int
  | .LNil => 0
  | .LCons _ t => 1 + ll_len t

@[grind] def ll_head : LList -> Int
  | .LNil => 0
  | .LCons h _ => h

@[grind] def ll_tail : LList -> LList
  | .LNil => .LNil
  | .LCons _ t => t

@[grind] def ll_mem (x : Int) : LList -> Prop
  | .LNil => False
  | .LCons y t => x = y ∨ ll_mem x t

@[grind] def ll_app : LList -> LList -> LList
  | .LNil, m => m
  | .LCons x t, m => .LCons x (ll_app t m)

@[grind] def ll_repr : Vox_Vlist_tree -> LList
  | .Nil => .LNil
  | .Cons x t => .LCons x (ll_repr t)

theorem ll_len_nonneg (l : LList) : ll_len l >= 0 := by
  induction l <;> grind
grind_pattern ll_len_nonneg => ll_len l

theorem ll_len_cons (x : Int) (l : LList) :
    ll_len (ll_cons x l) = 1 + ll_len l := by grind
grind_pattern ll_len_cons => ll_len (ll_cons x l)

theorem ll_len_app (a b : LList) :
    ll_len (ll_app a b) = ll_len a + ll_len b := by
  induction a <;> grind
grind_pattern ll_len_app => ll_len (ll_app a b)

theorem ll_mem_cons (x y : Int) (l : LList) :
    ll_mem x (ll_cons y l) = (x = y ∨ ll_mem x l) := by grind
grind_pattern ll_mem_cons => ll_mem x (ll_cons y l)

theorem ll_mem_app (x : Int) (a b : LList) :
    ll_mem x (ll_app a b) = (ll_mem x a ∨ ll_mem x b) := by
  induction a <;> grind
grind_pattern ll_mem_app => ll_mem x (ll_app a b)

theorem ll_nil_not_mem (x : Int) : ¬ ll_mem x ll_nil := by grind
grind_pattern ll_nil_not_mem => ll_mem x ll_nil

theorem ll_isnil_nil : ll_isnil ll_nil := by grind
grind_pattern ll_isnil_nil => ll_isnil ll_nil

@[grind] theorem ll_not_isnil_cons (x : Int) (l : LList) :
    ¬ ll_isnil (ll_cons x l) := by grind

theorem ll_head_cons (x : Int) (l : LList) : ll_head (ll_cons x l) = x := by grind
grind_pattern ll_head_cons => ll_head (ll_cons x l)

theorem ll_tail_cons (x : Int) (l : LList) : ll_tail (ll_cons x l) = l := by grind
grind_pattern ll_tail_cons => ll_tail (ll_cons x l)

theorem ll_cons_head_tail (l : LList) (h : ¬ ll_isnil l) :
    ll_cons (ll_head l) (ll_tail l) = l := by cases l <;> grind
grind_pattern ll_cons_head_tail => ll_cons (ll_head l) (ll_tail l)
|lean}]

let empty : (u : unit) -> t{ _ = ll_nil } =
  fun _ -> (Nil : t{ _ = ll_nil })

let cons : (x : int) -> (l : t) -> t{ _ = ll_cons x l } =
  fun x l ->
    let refine_ t0 = l in
    (Cons (x, t0) : t{ _ = ll_cons x l })

let is_empty : (l : t) -> bool{ _ = ll_isnil l } =
  fun l ->
    let refine_ t0 = l in
    (match t0 with
     | Nil -> (true : bool{ _ = ll_isnil l })
     | Cons (_, _) -> (false : bool{ _ = ll_isnil l }))

let length : (l : t) -> int{ _ = ll_len l } =
  fun l ->
    let refine_ t0 = l in
    let rec go : (u : tree) -> int{ _ = ll_len (ll_repr u) } =
      fun u ->
        match u with
        | Nil -> 0
        | Cons (_, r) -> let n = go r in 1 + n
    in
    go t0

let mem : (x : int) -> (l : t) -> bool{ _ = ll_mem x l } =
  fun x l ->
    let refine_ t0 = l in
    let rec go : (u : tree) -> bool{ _ = ll_mem x (ll_repr u) } =
      fun u ->
        match u with
        | Nil -> false
        | Cons (y, r) -> if x = y then true else go r
    in
    go t0

let append : (a : t) -> (b : t) -> t{ _ = ll_app a b } =
  fun a b ->
    let refine_ ta = a in
    let refine_ tb = b in
    let rec go : (u : tree) -> tree{ ll_repr _ = ll_app (ll_repr u) (ll_repr tb) } =
      fun u ->
        match u with
        | Nil -> (tb : tree{ ll_repr _ = ll_app (ll_repr u) (ll_repr tb) })
        | Cons (x, r) ->
            let rest = go r in
            (Cons (x, rest) : tree{ ll_repr _ = ll_app (ll_repr u) (ll_repr tb) })
    in
    let res = go ta in
    (res : t{ _ = ll_app a b })

(* head/tail: TOTAL (ll_head/ll_tail are total on the model -- .LNil -> 0/.LNil),
   so no precondition is needed; the empty case returns the model's default.
   De-contorted on the origin/vox compiler: the old refined-via arg
   (t{ not (ll_isnil _) }) + alias-then-refine_ workaround is gone -- an
   unrefined arg + the proven inner-`go`-over-tree pattern (as in length/mem)
   verifies directly. See notes/vlist.md (head/tail-total de-contortion). *)
let head : (l : t) -> int{ _ = ll_head l } =
  fun l ->
    let refine_ t0 = l in
    let go : (u : tree) -> int{ _ = ll_head (ll_repr u) } =
      fun u -> match u with Nil -> 0 | Cons (y, _) -> y
    in go t0

let tail : (l : t) -> t{ _ = ll_tail l } =
  fun l ->
    let refine_ t0 = l in
    let rec go : (u : tree) -> tree{ ll_repr _ = ll_tail (ll_repr u) } =
      fun u -> match u with
        | Nil -> (Nil : tree{ ll_repr _ = ll_tail (ll_repr u) })
        | Cons (_, r) -> (r : tree{ ll_repr _ = ll_tail (ll_repr u) })
    in let res = go t0 in (res : t{ _ = ll_tail l })
