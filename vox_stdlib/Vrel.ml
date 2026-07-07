(* Pays Vrel.mli's obligation (listRel_len) and proves each combinator's
   contract.  The substrate defs are RESTATED here without [public] (the
   model-duplication tax -- see notes/vrel.md); three INTERNAL helper
   theorems (the toNat bridges + il_len_nonneg) are what let iter's and
   fold's symbolic-fuel recursions close without a hand loop lemma -- the
   forward recursion matches relIterN's PREPEND form, so one definitional
   unfold plus the toNat arithmetic discharges each step. *)

type ilist = Inil | Icons of int * ilist

[%%vox.lean {lean|
abbrev IntRel := Int -> Int -> Prop
abbrev IntPred := Int -> Prop

@[grind, expose] def rHolds (r : IntRel) (a b : Int) : Prop := r a b
@[grind, expose] def pHolds (p : IntPred) (x : Int) : Prop := p x

@[grind, expose] def rcomp (r s : IntRel) : IntRel :=
  fun a c => exists b, r a b /\ s b c
@[grind, expose] def rand (r s : IntRel) : IntRel := fun a b => r a b /\ s a b
@[grind, expose] def ror  (r s : IntRel) : IntRel := fun a b => r a b \/ s a b
@[grind, expose] def rconverse (r : IntRel) : IntRel := fun a b => r b a

@[grind, expose] def relIterN (r : IntRel) : Nat -> Int -> Int -> Prop
  | 0,     x, y => x = y
  | (k+1), x, y => exists z, r x z /\ relIterN r k z y
@[grind, expose] def relIter (r : IntRel) (n : Int) (x y : Int) : Prop :=
  relIterN r n.toNat x y

-- toNat bridges: reduce the fuel at the two shapes the recursions hit.
@[grind] theorem toNat_nonpos (m : Int) (h : m <= 0) : m.toNat = 0 := by omega
@[grind] theorem toNat_succ (m : Int) (h : 1 <= m) :
    m.toNat = (m - 1).toNat + 1 := by omega

@[grind, expose] def listRel (r : IntRel) :
    Vox_Vrel_ilist -> Vox_Vrel_ilist -> Prop
  | .Inil, .Inil => True
  | .Icons a s, .Icons b t => r a b /\ listRel r s t
  | _, _ => False

@[grind, expose] def il_len : Vox_Vrel_ilist -> Int
  | .Inil => 0
  | .Icons _ t => 1 + il_len t
@[grind] theorem il_len_nonneg (l : Vox_Vrel_ilist) : il_len l >= 0 := by
  induction l <;> grind
grind_pattern il_len_nonneg => il_len l

@[grind, expose] def allP (p : IntPred) : Vox_Vrel_ilist -> Prop
  | .Inil => True
  | .Icons x t => pHolds p x /\ allP p t

theorem listRel_len (r : IntRel) (a b : Vox_Vrel_ilist) :
    listRel r a b -> il_len a = il_len b := by
  induction a generalizing b <;> cases b <;> grind
grind_pattern listRel_len => listRel r a b
|lean}]

let iter :
      (r : (int -> int -> bool)) ->
      (f : ((x : int) -> int{ rHolds r x _ })) ->
      (x0 : int) -> (n : int) -> int{ relIter r n x0 _ } =
  fun r f x0 n ->
    ignore r;
    let rec go : (a : int) -> (m : int) -> int{ relIter r m a _ } =
      fun a m ->
        if m <= 0 then (a : int{ relIter r m a _ })
        else
          let b = f a in
          let res = go b (m - 1) in
          (res : int{ relIter r m a _ })
    in
    go x0 n

let map :
      (r : (int -> int -> bool)) ->
      (f : ((x : int) -> int{ rHolds r x _ })) ->
      (xs : ilist) -> ilist{ listRel r xs _ } =
  fun r f xs ->
    ignore r;
    let rec go : (u : ilist) -> ilist{ listRel r u _ } =
      fun u ->
        match u with
        | Inil -> (Inil : ilist{ listRel r u _ })
        | Icons (x, rest) ->
            let y = f x in
            let ys = go rest in
            (Icons (y, ys) : ilist{ listRel r u _ })
    in
    go xs

let length : (l : ilist) -> int{ _ = il_len l } =
  fun l ->
    let rec go : (u : ilist) -> int{ _ = il_len u } =
      fun u -> match u with Inil -> 0 | Icons (_, t) -> let n = go t in 1 + n
    in
    go l

let fold :
      (r : (int -> int -> bool)) ->
      (f : ((acc : int) -> (x : int) -> int{ rHolds r acc _ })) ->
      (init : int) -> (xs : ilist) -> int{ relIter r (il_len xs) init _ } =
  fun r f init xs ->
    ignore r;
    let rec go : (a : int) -> (u : ilist) -> int{ relIter r (il_len u) a _ } =
      fun a u ->
        match u with
        | Inil -> (a : int{ relIter r (il_len u) a _ })
        | Icons (x, rest) ->
            let a' = f a x in
            let res = go a' rest in
            (res : int{ relIter r (il_len u) a _ })
    in
    go init xs

let filter :
      (p : (int -> bool)) ->
      (test : ((x : int) -> bool{ _ = pHolds p x })) ->
      (xs : ilist) -> ilist{ allP p _ } =
  fun p test xs ->
    ignore p;
    let rec go : (u : ilist) -> ilist{ allP p _ } =
      fun u ->
        match u with
        | Inil -> (Inil : ilist{ allP p _ })
        | Icons (x, rest) ->
            let ys = go rest in
            if test x then (Icons (x, ys) : ilist{ allP p _ })
            else (ys : ilist{ allP p _ })
    in
    go xs

let compose2 :
      (r : (int -> int -> bool)) ->
      (s : (int -> int -> bool)) ->
      (f : ((x : int) -> int{ rHolds r x _ })) ->
      (g : ((y : int) -> int{ rHolds s y _ })) ->
      (x : int) -> int{ rHolds (rcomp r s) x _ } =
  fun r s f g x -> ignore (r, s); let z = f x in g z
