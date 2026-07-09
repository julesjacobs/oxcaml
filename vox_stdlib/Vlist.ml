(* Pays Vlist.mli's obligations over the EXPOSED cons-list repr.  Because
   [t] is a native ADT (not a via type), its Lean correspondent is the
   auto-derived inductive `Vox_Vlist_t` and the ops are written DIRECTLY
   over it -- native `match`, native `Nil`/`Cons`, top-level `let rec` (the
   Vset_bst backend style).  The old via machinery (ll_repr abstraction fn,
   refine_ peels, skeleton-threading for #31) is GONE: there is no
   abstraction to thread.  The model defs are RESTATED here without [public]
   (the model-duplication tax -- see notes/vlist.md); each .mli axiom is
   discharged by a same-named, same-typed theorem with identical
   grind_pattern. *)
open Vhof
open Voption
type t = Nil | Cons of int * t

[%%vox.lean {lean|
@[grind] def ll_cons (x : Int) (l : Vox_Vlist_t) : Vox_Vlist_t := .Cons x l

@[grind] def ll_isnil : Vox_Vlist_t -> Prop
  | .Nil => True
  | .Cons _ _ => False

@[grind] def ll_nil : Vox_Vlist_t := .Nil

@[grind] def ll_len : Vox_Vlist_t -> Int
  | .Nil => 0
  | .Cons _ t => 1 + ll_len t

@[grind] def ll_head : Vox_Vlist_t -> Int
  | .Nil => 0
  | .Cons h _ => h

@[grind] def ll_tail : Vox_Vlist_t -> Vox_Vlist_t
  | .Nil => .Nil
  | .Cons _ t => t

@[grind] def ll_mem (x : Int) : Vox_Vlist_t -> Prop
  | .Nil => False
  | .Cons y t => x = y ∨ ll_mem x t

@[grind] def ll_app : Vox_Vlist_t -> Vox_Vlist_t -> Vox_Vlist_t
  | .Nil, m => m
  | .Cons x t, m => .Cons x (ll_app t m)

theorem ll_len_nonneg (l : Vox_Vlist_t) : ll_len l >= 0 := by
  induction l <;> grind
grind_pattern ll_len_nonneg => ll_len l

theorem ll_len_cons (x : Int) (l : Vox_Vlist_t) :
    ll_len (ll_cons x l) = 1 + ll_len l := by grind
grind_pattern ll_len_cons => ll_len (ll_cons x l)

theorem ll_len_app (a b : Vox_Vlist_t) :
    ll_len (ll_app a b) = ll_len a + ll_len b := by
  induction a <;> grind
grind_pattern ll_len_app => ll_len (ll_app a b)

theorem ll_mem_cons (x y : Int) (l : Vox_Vlist_t) :
    ll_mem x (ll_cons y l) = (x = y ∨ ll_mem x l) := by grind
grind_pattern ll_mem_cons => ll_mem x (ll_cons y l)

theorem ll_mem_app (x : Int) (a b : Vox_Vlist_t) :
    ll_mem x (ll_app a b) = (ll_mem x a ∨ ll_mem x b) := by
  induction a <;> grind
grind_pattern ll_mem_app => ll_mem x (ll_app a b)

theorem ll_nil_not_mem (x : Int) : ¬ ll_mem x ll_nil := by grind
grind_pattern ll_nil_not_mem => ll_mem x ll_nil

theorem ll_isnil_nil : ll_isnil ll_nil := by grind
grind_pattern ll_isnil_nil => ll_isnil ll_nil

@[grind] theorem ll_not_isnil_cons (x : Int) (l : Vox_Vlist_t) :
    ¬ ll_isnil (ll_cons x l) := by grind

theorem ll_head_cons (x : Int) (l : Vox_Vlist_t) : ll_head (ll_cons x l) = x := by grind
grind_pattern ll_head_cons => ll_head (ll_cons x l)

theorem ll_tail_cons (x : Int) (l : Vox_Vlist_t) : ll_tail (ll_cons x l) = l := by grind
grind_pattern ll_tail_cons => ll_tail (ll_cons x l)

theorem ll_cons_head_tail (l : Vox_Vlist_t) (h : ¬ ll_isnil l) :
    ll_cons (ll_head l) (ll_tail l) = l := by cases l <;> grind
grind_pattern ll_cons_head_tail => ll_cons (ll_head l) (ll_tail l)


-- ===== HOF KIT: per-container relational lifts over Vox_Vlist_t =====
@[grind, expose] def ll_listRel (r : IntRel) : Vox_Vlist_t -> Vox_Vlist_t -> Prop
  | .Nil, .Nil => True
  | .Cons a s, .Cons b t => r a b /\ ll_listRel r s t
  | _, _ => False
@[grind, expose] def ll_allP (p : IntPred) : Vox_Vlist_t -> Prop
  | .Nil => True
  | .Cons x t => pHolds p x /\ ll_allP p t
@[grind, expose] def ll_exP (p : IntPred) : Vox_Vlist_t -> Prop
  | .Nil => False
  | .Cons x t => pHolds p x \/ ll_exP p t
@[grind, expose] def ll_relFold (r : IntRel3) : Vox_Vlist_t -> Int -> Int -> Prop
  | .Nil, init, final => init = final
  | .Cons x t, init, final => exists acc, r init x acc /\ ll_relFold r t acc final
@[grind, expose] def ll_sum : Vox_Vlist_t -> Int
  | .Nil => 0
  | .Cons x t => x + ll_sum t
@[grind, expose] def ll_rev : Vox_Vlist_t -> Vox_Vlist_t
  | .Nil => .Nil
  | .Cons x t => ll_app (ll_rev t) (.Cons x .Nil)
@[grind, expose] def ll_nth : Int -> Vox_Vlist_t -> Int
  | _, .Nil => 0
  | i, .Cons x t => if i <= 0 then x else ll_nth (i-1) t
@[grind, expose] def ll_nosat (p : IntPred) : Vox_Vlist_t -> Prop
  | .Nil => True
  | .Cons x t => (¬ pHolds p x) /\ ll_nosat p t
@[grind, expose] def ll_find_result (p : IntPred) (l : Vox_Vlist_t) (o : Vox_Voption_t) : Prop :=
  (vo_is_some o -> (pHolds p (vo_get o) /\ ll_mem (vo_get o) l)) /\
  ((¬ vo_is_some o) -> ll_nosat p l)

-- ===== HOF laws (discharge the .mli obligations) =====
theorem ll_listRel_len (r : IntRel) (a b : Vox_Vlist_t) :
    ll_listRel r a b -> ll_len a = ll_len b := by
  induction a generalizing b <;> cases b <;> grind
grind_pattern ll_listRel_len => ll_listRel r a b
theorem ll_len_rev (l : Vox_Vlist_t) : ll_len (ll_rev l) = ll_len l := by
  induction l <;> grind
grind_pattern ll_len_rev => ll_len (ll_rev l)
theorem ll_mem_rev (x : Int) (l : Vox_Vlist_t) : ll_mem x (ll_rev l) = ll_mem x l := by
  induction l <;> grind
grind_pattern ll_mem_rev => ll_mem x (ll_rev l)
theorem ll_nth_cons (i x : Int) (l : Vox_Vlist_t) :
    ll_nth i (ll_cons x l) = (if i <= 0 then x else ll_nth (i-1) l) := by grind
grind_pattern ll_nth_cons => ll_nth i (ll_cons x l)

-- ll_sum cons/append laws (F-B2).
theorem ll_sum_cons (x : Int) (l : Vox_Vlist_t) :
    ll_sum (ll_cons x l) = x + ll_sum l := by grind
grind_pattern ll_sum_cons => ll_sum (ll_cons x l)
theorem ll_sum_app (a b : Vox_Vlist_t) :
    ll_sum (ll_app a b) = ll_sum a + ll_sum b := by
  induction a <;> grind
grind_pattern ll_sum_app => ll_sum (ll_app a b)
|lean}]

let empty : (u : unit) -> t{ _ = ll_nil } =
  fun _ -> (Nil : t{ _ = ll_nil })

let cons : (x : int) -> (l : t) -> t{ _ = ll_cons x l } =
  fun x l -> (Cons (x, l) : t{ _ = ll_cons x l })

let is_empty : (l : t) -> bool{ _ = ll_isnil l } =
  fun l -> match l with Nil -> true | Cons (_, _) -> false

let rec length : (l : t) -> int{ _ = ll_len l } =
  fun l ->
    match l with
    | Nil -> 0
    | Cons (_, r) -> let n = length r in 1 + n

let rec mem : (x : int) -> (l : t) -> bool{ _ = ll_mem x l } =
  fun x l ->
    match l with
    | Nil -> false
    | Cons (y, r) -> if x = y then true else mem x r

let rec append : (a : t) -> (b : t) -> t{ _ = ll_app a b } =
  fun a b ->
    match a with
    | Nil -> b
    | Cons (x, r) -> let rest = append r b in Cons (x, rest)

let head : (l : t) -> int{ _ = ll_head l } =
  fun l -> match l with Nil -> 0 | Cons (y, _) -> y

let tail : (l : t) -> t{ _ = ll_tail l } =
  fun l -> match l with Nil -> Nil | Cons (_, r) -> r

(* ===== HOF surface (WP-0) ===== *)
let rec rev : (l : t) -> t{ _ = ll_rev l } =
  fun l ->
    match l with
    | Nil -> Nil
    | Cons (x, r) ->
        let rr = rev r in
        let one = Cons (x, Nil) in
        let res = append rr one in
        res

let rec nth : (i : int) -> (l : t) -> int{ _ = ll_nth i l } =
  fun i l ->
    match l with
    | Nil -> 0
    | Cons (x, r) -> if i <= 0 then x else nth (i - 1) r

let map :
      (r : ((int -> int -> bool) [@vox.total])) ->
      (f : ((x : int) -> int{ rHolds r x _ })) ->
      (l : t) -> t{ ll_listRel r l _ } =
  fun r f l ->
    ignore r;
    let rec go : (u : t) -> t{ ll_listRel r u _ } =
      fun u -> match u with
        | Nil -> (Nil : t{ ll_listRel r u _ })
        | Cons (x, rest) ->
            let y = f x in
            let ys = go rest in
            (Cons (y, ys) : t{ ll_listRel r u _ })
    in
    go l

let filter :
      (p : ((int -> bool) [@vox.total])) ->
      (test : ((x : int) -> bool{ _ = pHolds p x })) ->
      (l : t) -> t{ ll_allP p _ } =
  fun p test l ->
    ignore p;
    let rec go : (u : t) -> t{ ll_allP p _ } =
      fun u -> match u with
        | Nil -> (Nil : t{ ll_allP p _ })
        | Cons (x, rest) ->
            let ys = go rest in
            if test x then (Cons (x, ys) : t{ ll_allP p _ })
            else (ys : t{ ll_allP p _ })
    in
    go l

let fold_left :
      (r : ((int -> int -> int -> bool) [@vox.total])) ->
      (f : ((acc : int) -> (x : int) -> int{ r3Holds r acc x _ })) ->
      (init : int) -> (l : t) -> int{ ll_relFold r l init _ } =
  fun r f init l ->
    ignore r;
    let rec go : (a : int) -> (u : t) -> int{ ll_relFold r u a _ } =
      fun a u -> match u with
        | Nil -> (a : int{ ll_relFold r u a _ })
        | Cons (x, rest) ->
            let a' = f a x in
            let res = go a' rest in
            (res : int{ ll_relFold r u a _ })
    in
    go init l

let for_all :
      (p : ((int -> bool) [@vox.total])) ->
      (test : ((x : int) -> bool{ _ = pHolds p x })) ->
      (l : t) -> bool{ _ = ll_allP p l } =
  fun p test l ->
    ignore p;
    let rec go : (u : t) -> bool{ _ = ll_allP p u } =
      fun u -> match u with
        | Nil -> true
        | Cons (x, rest) -> if test x then go rest else false
    in
    go l

let exists :
      (p : ((int -> bool) [@vox.total])) ->
      (test : ((x : int) -> bool{ _ = pHolds p x })) ->
      (l : t) -> bool{ _ = ll_exP p l } =
  fun p test l ->
    ignore p;
    let rec go : (u : t) -> bool{ _ = ll_exP p u } =
      fun u -> match u with
        | Nil -> false
        | Cons (x, rest) -> if test x then true else go rest
    in
    go l

let find_opt :
      (p : ((int -> bool) [@vox.total])) ->
      (test : ((x : int) -> bool{ _ = pHolds p x })) ->
      (l : t) -> Voption.t{ ll_find_result p l _ } =
  fun p test l ->
    ignore p;
    let rec go : (u : t) -> Voption.t{ ll_find_result p u _ } =
      fun u -> match u with
        | Nil -> (Vnone : Voption.t{ ll_find_result p u _ })
        | Cons (x, rest) ->
            if test x then (Vsome x : Voption.t{ ll_find_result p u _ })
            else let o = go rest in (o : Voption.t{ ll_find_result p u _ })
    in
    go l
