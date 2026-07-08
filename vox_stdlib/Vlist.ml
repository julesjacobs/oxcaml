(* Pays Vlist.mli's obligations over the concrete cons-list repr.  The
   model defs are RESTATED here without [public] (the model-duplication
   tax -- see notes/vlist.md), the via abstraction [ll_repr] maps the
   concrete [tree] onto [LList], and each .mli axiom is discharged by a
   same-named, same-typed theorem with identical grind_pattern.  [append]
   uses the gap #31 skeleton-threading workaround: its recursive [go]
   returns a refined SKELETON and the via injection happens once, through
   a variable (see notes/vlist.md). *)
open Vhof
open Voption
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


-- ===== HOF KIT: per-container relational lifts over LList =====
-- ll_listRel: b is pointwise r-related to a (same length) -- map's spec.
@[grind, expose] def ll_listRel (r : IntRel) : LList -> LList -> Prop
  | .LNil, .LNil => True
  | .LCons a s, .LCons b t => r a b /\ ll_listRel r s t
  | _, _ => False
-- ll_allP / ll_exP: every / some element satisfies p -- filter/for_all/exists.
@[grind, expose] def ll_allP (p : IntPred) : LList -> Prop
  | .LNil => True
  | .LCons x t => pHolds p x /\ ll_allP p t
@[grind, expose] def ll_exP (p : IntPred) : LList -> Prop
  | .LNil => False
  | .LCons x t => pHolds p x \/ ll_exP p t
-- ll_relFold: fold_left with a TERNARY element-aware step (acc, elem, acc').
@[grind, expose] def ll_relFold (r : IntRel3) : LList -> Int -> Int -> Prop
  | .LNil, init, final => init = final
  | .LCons x t, init, final => exists acc, r init x acc /\ ll_relFold r t acc final
-- ll_sum: list sum accessor for fold's exact sum-law.
@[grind, expose] def ll_sum : LList -> Int
  | .LNil => 0
  | .LCons x t => x + ll_sum t
-- ll_rev / ll_nth: first-order surface ops.
@[grind, expose] def ll_rev : LList -> LList
  | .LNil => .LNil
  | .LCons x t => ll_app (ll_rev t) (.LCons x .LNil)
@[grind, expose] def ll_nth : Int -> LList -> Int
  | _, .LNil => 0
  | i, .LCons x t => if i <= 0 then x else ll_nth (i-1) t
-- ll_nosat: no element satisfies p -- find_opt's None-case spec.
@[grind, expose] def ll_nosat (p : IntPred) : LList -> Prop
  | .LNil => True
  | .LCons x t => (¬ pHolds p x) /\ ll_nosat p t
-- ll_find_result: find_opt's spec (references imported Voption model). Some ->
-- the found value satisfies p AND is a member; None -> no element satisfies p.
@[grind, expose] def ll_find_result (p : IntPred) (l : LList) (o : Vox_Voption_t) : Prop :=
  (vo_is_some o -> (pHolds p (vo_get o) /\ ll_mem (vo_get o) l)) /\
  ((¬ vo_is_some o) -> ll_nosat p l)

-- ===== HOF laws (discharge the .mli obligations) =====
theorem ll_listRel_len (r : IntRel) (a b : LList) :
    ll_listRel r a b -> ll_len a = ll_len b := by
  induction a generalizing b <;> cases b <;> grind
grind_pattern ll_listRel_len => ll_listRel r a b
theorem ll_len_rev (l : LList) : ll_len (ll_rev l) = ll_len l := by
  induction l <;> grind
grind_pattern ll_len_rev => ll_len (ll_rev l)
theorem ll_mem_rev (x : Int) (l : LList) : ll_mem x (ll_rev l) = ll_mem x l := by
  induction l <;> grind
grind_pattern ll_mem_rev => ll_mem x (ll_rev l)
theorem ll_nth_cons (i x : Int) (l : LList) :
    ll_nth i (ll_cons x l) = (if i <= 0 then x else ll_nth (i-1) l) := by grind
grind_pattern ll_nth_cons => ll_nth i (ll_cons x l)
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

(* append_tree: concrete-list append (no via) -- rev's building block. Its
   image law ll_repr (append_tree a b) = ll_app (ll_repr a) (ll_repr b) is what
   lets rev's skeleton spec close; proven inline via the go-over-tree pattern. *)
let append_tree : (a : tree) -> (b : tree) -> tree{ ll_repr _ = ll_app (ll_repr a) (ll_repr b) } =
  fun a b ->
    let rec go : (u : tree) -> tree{ ll_repr _ = ll_app (ll_repr u) (ll_repr b) } =
      fun u -> match u with
        | Nil -> (b : tree{ ll_repr _ = ll_app (ll_repr u) (ll_repr b) })
        | Cons (x, r) ->
            let rest = go r in
            (Cons (x, rest) : tree{ ll_repr _ = ll_app (ll_repr u) (ll_repr b) })
    in
    go a

(* ===== HOF surface (WP-0) ===== *)
let rev : (l : t) -> t{ _ = ll_rev l } =
  fun l ->
    let refine_ t0 = l in
    let rec go : (u : tree) -> tree{ ll_repr _ = ll_rev (ll_repr u) } =
      fun u -> match u with
        | Nil -> (Nil : tree{ ll_repr _ = ll_rev (ll_repr u) })
        | Cons (x, r) ->
            let rr = go r in
            let one = Cons (x, Nil) in
            let res = append_tree rr one in
            (res : tree{ ll_repr _ = ll_rev (ll_repr u) })
    in
    let res = go t0 in
    (res : t{ _ = ll_rev l })

let nth : (i : int) -> (l : t) -> int{ _ = ll_nth i l } =
  fun i l ->
    let refine_ t0 = l in
    let rec go : (j : int) -> (u : tree) -> int{ _ = ll_nth j (ll_repr u) } =
      fun j u -> match u with
        | Nil -> 0
        | Cons (x, r) -> if j <= 0 then x else go (j - 1) r
    in
    go i t0

let map :
      (r : ((int -> int -> bool) [@vox.total])) ->
      (f : ((x : int) -> int{ rHolds r x _ })) ->
      (l : t) -> t{ ll_listRel r l _ } =
  fun r f l ->
    ignore r;
    let refine_ t0 = l in
    let rec go : (u : tree) -> tree{ ll_listRel r (ll_repr u) (ll_repr _) } =
      fun u -> match u with
        | Nil -> (Nil : tree{ ll_listRel r (ll_repr u) (ll_repr _) })
        | Cons (x, rest) ->
            let y = f x in
            let ys = go rest in
            (Cons (y, ys) : tree{ ll_listRel r (ll_repr u) (ll_repr _) })
    in
    let res = go t0 in
    (res : t{ ll_listRel r l _ })

let filter :
      (p : ((int -> bool) [@vox.total])) ->
      (test : ((x : int) -> bool{ _ = pHolds p x })) ->
      (l : t) -> t{ ll_allP p _ } =
  fun p test l ->
    ignore p;
    let refine_ t0 = l in
    let rec go : (u : tree) -> tree{ ll_allP p (ll_repr _) } =
      fun u -> match u with
        | Nil -> (Nil : tree{ ll_allP p (ll_repr _) })
        | Cons (x, rest) ->
            let ys = go rest in
            if test x then (Cons (x, ys) : tree{ ll_allP p (ll_repr _) })
            else (ys : tree{ ll_allP p (ll_repr _) })
    in
    let res = go t0 in
    (res : t{ ll_allP p _ })

let fold_left :
      (r : ((int -> int -> int -> bool) [@vox.total])) ->
      (f : ((acc : int) -> (x : int) -> int{ r3Holds r acc x _ })) ->
      (init : int) -> (l : t) -> int{ ll_relFold r l init _ } =
  fun r f init l ->
    ignore r;
    let refine_ t0 = l in
    let rec go : (a : int) -> (u : tree) -> int{ ll_relFold r (ll_repr u) a _ } =
      fun a u -> match u with
        | Nil -> (a : int{ ll_relFold r (ll_repr u) a _ })
        | Cons (x, rest) ->
            let a' = f a x in
            let res = go a' rest in
            (res : int{ ll_relFold r (ll_repr u) a _ })
    in
    go init t0

let for_all :
      (p : ((int -> bool) [@vox.total])) ->
      (test : ((x : int) -> bool{ _ = pHolds p x })) ->
      (l : t) -> bool{ _ = ll_allP p l } =
  fun p test l ->
    ignore p;
    let refine_ t0 = l in
    let rec go : (u : tree) -> bool{ _ = ll_allP p (ll_repr u) } =
      fun u -> match u with
        | Nil -> true
        | Cons (x, rest) -> if test x then go rest else false
    in
    go t0

let exists :
      (p : ((int -> bool) [@vox.total])) ->
      (test : ((x : int) -> bool{ _ = pHolds p x })) ->
      (l : t) -> bool{ _ = ll_exP p l } =
  fun p test l ->
    ignore p;
    let refine_ t0 = l in
    let rec go : (u : tree) -> bool{ _ = ll_exP p (ll_repr u) } =
      fun u -> match u with
        | Nil -> false
        | Cons (x, rest) -> if test x then true else go rest
    in
    go t0

let find_opt :
      (p : ((int -> bool) [@vox.total])) ->
      (test : ((x : int) -> bool{ _ = pHolds p x })) ->
      (l : t) -> Voption.t{ ll_find_result p l _ } =
  fun p test l ->
    ignore p;
    let refine_ t0 = l in
    let rec go : (u : tree) -> Voption.t{ ll_find_result p (ll_repr u) _ } =
      fun u -> match u with
        | Nil -> (Vnone : Voption.t{ ll_find_result p (ll_repr u) _ })
        | Cons (x, rest) ->
            if test x then (Vsome x : Voption.t{ ll_find_result p (ll_repr u) _ })
            else let o = go rest in (o : Voption.t{ ll_find_result p (ll_repr u) _ })
    in
    go t0
