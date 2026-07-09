(* In-place array reverse over McCARTHY ARRAYS -- the array
   methodology: aset's ghost result is the STORE (_ = upd a j w);
   aget/alen return the SAME atom (_ = a); no quantified frame
   conditions exist anywhere in the API.  The three upd axioms carry
   structured triggers grind finds itself; the loop invariant is a
   prelude Prop whose step lemma has all variables bound by its
   conclusion, so the automatic pattern applies and discharge is
   ground congruence.  (Quantified per-call frames do NOT scale:
   grind cannot instantiate forall-facts at goal indices --
   lean_borrow_elem's style works for single borrows but composed
   mutation wants stores.) *)

[%%vox.lean {lean|
opaque len : VoxU -> Int
opaque elem : VoxU -> Int -> Int
axiom upd : VoxU -> Int -> Int -> VoxU

-- the store axioms (the trusted array theory, McCarthy-style)
@[grind] axiom len_upd : forall (a : VoxU) (j w : Int),
  len (upd a j w) = len a
@[grind] axiom elem_upd_eq : forall (a : VoxU) (j w : Int),
  elem (upd a j w) j = w
@[grind] axiom elem_upd_ne : forall (a : VoxU) (j w k : Int),
  ¬ (k = j) -> elem (upd a j w) k = elem a k

-- the loop invariant: c agrees with b outside the middle window
-- [i, len b - i) and is reversed inside it
def revinv (b c : VoxU) (i : Int) : Prop :=
  len c = len b
  /\ (forall k, (0 <= k /\ k < i) -> elem c k = elem b k)
  /\ (forall k, (len b - i <= k /\ k < len b) -> elem c k = elem b k)
  /\ (forall k, (i <= k /\ k < len b - i) ->
        elem c k = elem b (len b - 1 - k))

-- the recursive step: all variables bound by the conclusion, so the
-- automatic pattern works; the swapped array arrives as a ground
-- upd-chain that congruence closure identifies
@[grind] theorem revinv_step (b b5 : VoxU) (i : Int)
    (hi : 0 <= i) (hlt : 2 * i + 1 < len b)
    (hIH : revinv
      (upd (upd b i (elem b (len b - 1 - i))) (len b - 1 - i) (elem b i))
      b5 (i + 1)) :
    revinv b b5 i := by
  obtain ⟨l5, kl, kr, mid⟩ := hIH
  have lb4 :
      len (upd (upd b i (elem b (len b - 1 - i))) (len b - 1 - i)
             (elem b i))
      = len b := by
    rw [len_upd, len_upd]
  have e4 : forall k, ¬ (k = i) -> ¬ (k = len b - 1 - i) ->
      elem (upd (upd b i (elem b (len b - 1 - i))) (len b - 1 - i)
              (elem b i)) k
      = elem b k := by
    intro k hki hkj
    rw [elem_upd_ne _ _ _ _ hkj, elem_upd_ne _ _ _ _ hki]
  refine ⟨by omega, ?_, ?_, ?_⟩
  · intro k hk
    rw [kl k (by omega), e4 k (by omega) (by omega)]
  · intro k hk
    rw [kr k (by omega), e4 k (by omega) (by omega)]
  · intro k hk
    by_cases hki : k = i
    · rw [hki, kl i (by omega), elem_upd_ne _ _ _ _ (by omega),
          elem_upd_eq]
    · by_cases hkj : k = len b - 1 - i
      · have harith : len b - 1 - (len b - 1 - i) = i := by omega
        rw [hkj, kr (len b - 1 - i) (by omega), elem_upd_eq, harith]
      · rw [mid k (by omega), lb4,
            e4 (len b - 1 - k) (by omega) (by omega)]

-- middle of width <= 1 is already reversed
@[grind] theorem revinv_base (b : VoxU) (i : Int)
    (hi : 0 <= i) (hle : 2 * i <= len b) (hge : ¬ (2 * i + 1 < len b)) :
    revinv b b i := by
  refine ⟨rfl, fun k hk => rfl, fun k hk => rfl, ?_⟩
  intro k hk
  have hkk : k = len b - 1 - k := by omega
  rw [<- hkk]

-- unpack the invariant at i = 0 into the user-facing spec
@[grind] theorem revinv_zero (b c : VoxU) (h : revinv b c 0) :
    len c = len b
    /\ forall k, (0 <= k /\ k < len b) ->
         elem c k = elem b (len b - 1 - k) := by
  obtain ⟨l, _, _, mid⟩ := h
  exact ⟨l, fun k hk => mid k (by omega)⟩
|lean}]

module A : sig
  type varr

  val anew :
    (n : int) -> (v : int) ->
    varr{ len _ = n && (forall_ i. 0 <= i && i < n -> elem _ i = v) }
      @ unique

  val alen :
    (a : varr) @ unique ->
    (int{ _ = len a && 0 <= _ } * varr{ _ = a }) @ unique

  val aget :
    (a : varr) @ unique -> (j : int{ 0 <= _ && _ < len a }) ->
    (int{ _ = elem a j } * varr{ _ = a }) @ unique

  val apeek :
    (a : varr) -> (j : int{ 0 <= _ && _ < len a }) -> int{ _ = elem a j }

  val aset :
    (a : varr) @ unique -> (j : int{ 0 <= _ && _ < len a }) -> (w : int) ->
    varr{ _ = upd a j w } @ unique
end = struct
  type varr = { mutable arr : int array }

  let anew :
    (n : int) -> (v : int) ->
    varr{ len _ = n && (forall_ i. 0 <= i && i < n -> elem _ i = v) }
      @ unique =
    fun n v -> assume_unchecked_ (Obj.magic_unique { arr = Array.make n v })

  let alen :
    (a : varr) @ unique ->
    (int{ _ = len a && 0 <= _ } * varr{ _ = a }) @ unique =
    fun a ->
      let n = Array.length a.arr in
      Obj.magic_unique
        ((assume_unchecked_ n : int{ _ = len a && 0 <= _ }),
         (assume_unchecked_ (Obj.magic a) : varr{ _ = a }))

  let aget :
    (a : varr) @ unique -> (j : int{ 0 <= _ && _ < len a }) ->
    (int{ _ = elem a j } * varr{ _ = a }) @ unique =
    fun a j ->
      let v = a.arr.(j) in
      Obj.magic_unique
        ((assume_unchecked_ v : int{ _ = elem a j }),
         (assume_unchecked_ (Obj.magic a) : varr{ _ = a }))

  let apeek :
    (a : varr) -> (j : int{ 0 <= _ && _ < len a }) -> int{ _ = elem a j } =
    fun a j -> assume_unchecked_ a.arr.(j)

  let aset :
    (a : varr) @ unique -> (j : int{ 0 <= _ && _ < len a }) -> (w : int) ->
    varr{ _ = upd a j w } @ unique =
    fun a j w ->
      a.arr.(j) <- w;
      assume_unchecked_ (Obj.magic_unique (Obj.magic a))
end

open A

(* Reverse the middle [i, len b - i) of b: the loop invariant as a
   recursive contract over the store chain. *)
let rec go :
  (b : varr) @ unique -> (n : int{ _ = len b }) ->
  (i : int{ 0 <= _ && 2 * _ <= len b }) ->
  varr{ revinv b _ i } @ unique =
  fun b n i ->
  if 2 * i + 1 < n
  then (
    let j = n - 1 - i in
    let (vi, b1) = aget b i in
    let (vj, b2) = aget b1 j in
    let b3 = aset b2 i vj in
    let b4 = aset b3 j vi in
    let b5 = go b4 n (i + 1) in
    b5)
  else b

let reverse :
  (a : varr) @ unique ->
  varr{ len _ = len a
     && (forall_ k. 0 <= k && k < len a ->
           elem _ k = elem a (len a - 1 - k)) } @ unique =
  fun a ->
  let (n, a1) = alen a in
  let r = go a1 n 0 in
  r
