(* Mutable INT-ARRAY SLICES as RUSTHORN-STYLE BORROWS: borrow_lib's
   prophecy discipline, extended from one cell to array segments,
   with no dedicated checker support.

   A [varr] is an owned linear array whose ghost [cts] is its
   contents as a Lean list.  [borrow] opens a CPS bracket handing the
   continuation the root LOAN over the whole array: a [slice] is a
   live mutable borrow of a segment, logically the RustHorn pair
   (now m, fin m) = (current contents, prophesied final contents),
   both sequences.  A prophecy [p] denotes the sequence [pv p] it
   will resolve to; it is opaque, unforgeable, and consumed
   [@ unique], so one prophecy serves exactly one loan.  Loans are
   [@ local unique]: they cannot escape their bracket, so when the
   bracket returns, the residual [varr{ cts _ = pv p }] is honest.

   [split] is a REBORROW: it consumes a loan and hands ITS bracket
   two sub-loans over the halves, each with a fresh prophecy;
   adjacency needs no side condition -- the sub-loans exist only
   inside the bracket, and the parent comes back already advanced to
   the prophesied recombination [now _ = app (pv pl) (pv pr)] with
   its own [fin] untouched.  Disjointness of the halves is the mode
   checker's: the parent is consumed, each half is its own unique
   loan, and two tasks of a fork-join can each take one.

   [sdrop] RESOLVES a loan's prophecy: its refined unit carries
   [fin m = now m], which chains through the loan's facts to reveal
   the prophecy's value ([sdropa] is the aliased variant for
   terminal read phases).  Dropping is optional: an unresolved
   prophecy is a sound leak -- its [pv] simply stays opaque.

   Every mutation consumes the loan and re-issues it with contents a
   LIST EXPRESSION over the old ([upd], [take]/[drop], [app]), so
   the facts stay quantifier-free; the quantified reasoning (frames,
   permutations, sortedness) lives in the embedded prelude's lemmas,
   where [grind_pattern]s control instantiation and the bespoke
   swap/glue lemmas are keyed on exactly the shapes a
   partition-based sort produces.

   TRUSTED: [varr]/[proph]/[slice] abstract and boxed; the
   implementation asserts with assume_unchecked_ that the ghosts
   track the real array segments; every fact a client sees is proved
   from the signatures below.  (Spec names [now]/[fin] coincide with
   borrow_lib's on purpose -- same concept, sequence-sorted; a unit
   importing both specs would collide and must rename, which fails
   closed.) *)

type varr
type proph
type slice

[%%vox.lean {lean|

-- ghosts: an owned array denotes its contents [cts]; a live loan
-- denotes current and prophesied-final contents [now]/[fin]; a
-- prophecy denotes the sequence it will resolve to [pv]
opaque cts : VoxU -> List Int
opaque now : VoxU -> List Int
opaque fin : VoxU -> List Int
opaque pv : VoxU -> List Int

-- Int-indexed list operations, defined by structural recursion on
-- the list so every lemma below is a uniform induction (no
-- dependence on core Nat lemmas).
@[grind] def len : List Int -> Int
  | [] => 0
  | _ :: t => 1 + len t

@[grind] def elem : List Int -> Int -> Int
  | [], _ => 0
  | x :: t, i => if i = 0 then x else elem t (i - 1)

@[grind] def upd : List Int -> Int -> Int -> List Int
  | [], _, _ => []
  | x :: t, i, v => if i = 0 then v :: t else x :: upd t (i - 1) v

@[grind] def take : Int -> List Int -> List Int
  | _, [] => []
  | i, x :: t => if i <= 0 then [] else x :: take (i - 1) t

@[grind] def drop : Int -> List Int -> List Int
  | _, [] => []
  | i, x :: t => if i <= 0 then x :: t else drop (i - 1) t

@[grind] def app : List Int -> List Int -> List Int
  | [], r => r
  | x :: t, r => x :: app t r

@[grind] def sngl (x : Int) : List Int := [x]

@[grind] def seg (i j : Int) (l : List Int) : List Int :=
  take (j - i) (drop i l)

@[grind] def all_le : List Int -> Int -> Prop
  | [], _ => True
  | x :: t, p => x <= p ∧ all_le t p

@[grind] def all_ge : List Int -> Int -> Prop
  | [], _ => True
  | x :: t, p => x >= p ∧ all_ge t p

@[grind] def sorted : List Int -> Prop
  | [] => True
  | [_] => True
  | x :: y :: t => x <= y ∧ sorted (y :: t)

@[grind] def cnt : List Int -> Int -> Int
  | [], _ => 0
  | y :: t, x => (if y = x then 1 else 0) + cnt t x

-- permutation, kept folded (no @[grind]): all reasoning goes
-- through the theorem interface below
def perm (l r : List Int) : Prop := ∀ x : Int, cnt l x = cnt r x

-- ----- length -----
theorem len_nonneg (l : List Int) : 0 <= len l := by
  induction l <;> grind

theorem len_take (i : Int) (l : List Int)
    (h1 : 0 <= i) (h2 : i <= len l) : len (take i l) = i := by
  induction l generalizing i <;> grind [len_nonneg]

theorem len_drop (i : Int) (l : List Int)
    (h1 : 0 <= i) (h2 : i <= len l) : len (drop i l) = len l - i := by
  induction l generalizing i <;> grind

theorem len_app (a b : List Int) : len (app a b) = len a + len b := by
  induction a <;> grind

theorem len_upd (l : List Int) (i v : Int) : len (upd l i v) = len l := by
  induction l generalizing i <;> grind

-- ----- elem / upd -----
theorem elem_upd (l : List Int) (i j v : Int)
    (h1 : 0 <= i) (h2 : i < len l) :
    elem (upd l i v) j = if j = i then v else elem l j := by
  induction l generalizing i j <;> grind

theorem elem_drop (i k : Int) (l : List Int)
    (h1 : 0 <= i) (h2 : 0 <= k) :
    elem (drop i l) k = elem l (i + k) := by
  induction l generalizing i with
  | nil => grind
  | cons x t ih => have := ih (i - 1); grind

-- ----- take / drop -----
theorem take_nonpos (i : Int) (l : List Int) (h : i <= 0) : take i l = [] := by
  induction l <;> grind

theorem drop_nonpos (i : Int) (l : List Int) (h : i <= 0) : drop i l = l := by
  induction l <;> grind

theorem drop_all (i : Int) (l : List Int) (h : len l <= i) : drop i l = [] := by
  induction l generalizing i with
  | nil => grind
  | cons x t ih => have := ih (i - 1); have := len_nonneg t; grind

theorem drop_drop (i j : Int) (l : List Int)
    (h1 : 0 <= i) (h2 : 0 <= j) :
    drop i (drop j l) = drop (i + j) l := by
  induction l generalizing j with
  | nil => grind
  | cons x t ih => have := ih (j - 1); grind

theorem take_upd_ge (n i v : Int) (l : List Int)
    (h : n <= i) : take n (upd l i v) = take n l := by
  induction l generalizing n i with
  | nil => grind
  | cons x t ih => have := ih (n - 1) (i - 1); grind

theorem drop_upd_lt (n i v : Int) (l : List Int)
    (h1 : 0 <= i) (h2 : i < n) : drop n (upd l i v) = drop n l := by
  induction l generalizing n i with
  | nil => grind
  | cons x t ih => have := ih (n - 1) (i - 1); grind

theorem drop_upd_ge (n i v : Int) (l : List Int)
    (h1 : 0 <= n) (h2 : n <= i) :
    drop n (upd l i v) = upd (drop n l) (i - n) v := by
  induction l generalizing n i with
  | nil => grind
  | cons x t ih => have := ih (n - 1) (i - 1); grind

theorem upd_out (l : List Int) (i v : Int)
    (h : i < 0 ∨ len l <= i) : upd l i v = l := by
  induction l generalizing i with
  | nil => grind
  | cons x t ih => have := ih (i - 1); have := len_nonneg t; grind

theorem take_drop_app (i : Int) (l : List Int) :
    app (take i l) (drop i l) = l := by
  induction l generalizing i with
  | nil => grind
  | cons x t ih => have := ih (i - 1); grind

theorem take_snoc (i : Int) (l : List Int)
    (h1 : 0 <= i) (h2 : i < len l) :
    take (i + 1) l = app (take i l) (sngl (elem l i)) := by
  induction l generalizing i with
  | nil => grind
  | cons x t ih => have := ih (i - 1); grind [take_nonpos]

theorem drop_cons (i : Int) (l : List Int)
    (h1 : 0 <= i) (h2 : i < len l) :
    drop i l = app (sngl (elem l i)) (drop (i + 1) l) := by
  induction l generalizing i with
  | nil => grind
  | cons x t ih => have := ih (i - 1); grind [drop_nonpos]

theorem take1_drop (k : Int) (l : List Int)
    (h1 : 0 <= k) (h2 : k < len l) :
    take 1 (drop k l) = sngl (elem l k) := by
  induction l generalizing k with
  | nil => grind
  | cons x t ih => have := ih (k - 1); grind [take_nonpos]

-- ----- seg -----
theorem seg_upd_out (i j k v : Int) (l : List Int)
    (h0 : 0 <= i) (h : k < i ∨ j <= k) :
    seg i j (upd l k v) = seg i j l := by
  cases h with
  | inl hk =>
    by_cases h0k : 0 <= k
    · grind [drop_upd_lt]
    · grind [upd_out]
  | inr hk =>
    by_cases hij : i <= j
    · by_cases hkl : k < len l
      · grind [drop_upd_ge, take_upd_ge]
      · have := len_nonneg l; grind [upd_out]
    · grind [take_nonpos]

theorem seg_snoc (i j : Int) (l : List Int)
    (h1 : 0 <= i) (h2 : i <= j) (h3 : j < len l) :
    seg i (j + 1) l = app (seg i j l) (sngl (elem l j)) := by
  have hd := elem_drop i (j - i) l h1 (by grind)
  have hl := len_drop i l h1 (by grind [len_nonneg])
  grind [take_snoc]

theorem seg_cons (i j : Int) (l : List Int)
    (h1 : 0 <= i) (h2 : i < j) (h3 : j <= len l) :
    seg i j l = app (sngl (elem l i)) (seg (i + 1) j l) := by
  have hd := drop_cons i l h1 (by grind)
  have hl := len_drop i l h1 (by grind)
  grind

theorem drop_split (i j : Int) (l : List Int)
    (h1 : 0 <= i) (h2 : i <= j) (_h3 : j <= len l) :
    drop i l = app (seg i j l) (drop j l) := by
  have := take_drop_app (j - i) (drop i l)
  grind [drop_drop]

-- ----- all_le / all_ge -----
theorem all_le_app (a b : List Int) (p : Int) :
    all_le (app a b) p ↔ (all_le a p ∧ all_le b p) := by
  induction a <;> grind

theorem all_ge_app (a b : List Int) (p : Int) :
    all_ge (app a b) p ↔ (all_ge a p ∧ all_ge b p) := by
  induction a <;> grind

-- ----- sorted -----
theorem sorted_short (l : List Int) (h : len l <= 1) : sorted l := by
  cases l with
  | nil => grind
  | cons x t =>
    cases t with
    | nil => grind
    | cons y t2 => have := len_nonneg t2; grind

theorem sorted_cons_ge (b : List Int) (p : Int)
    (hge : all_ge b p) (hb : sorted b) : sorted (p :: b) := by
  cases b <;> grind

theorem sorted_cons_app (b : List Int) (p : Int)
    (hge : all_ge b p) (hb : sorted b) :
    sorted (app (sngl p) b) := by
  grind [sorted_cons_ge]

theorem sorted_app_bound (a b : List Int) (p : Int)
    (ha : sorted a) (hb : sorted b)
    (hle : all_le a p) (hge : all_ge b p) :
    sorted (app a b) := by
  induction a with
  | nil => grind
  | cons x t ih =>
    cases t with
    | nil => cases b <;> grind
    | cons y t2 => grind

theorem sorted_elem (l : List Int) (i j : Int)
    (hs : sorted l) (h1 : 0 <= i) (h2 : i <= j) (h3 : j < len l) :
    elem l i <= elem l j := by
  induction l generalizing i j with
  | nil => grind
  | cons x t ih =>
    have ha := ih 0 (j - 1)
    have hb := ih (i - 1) (j - 1)
    cases t <;> grind [len_nonneg]

-- ----- perm -----
theorem cnt_nonneg (l : List Int) (x : Int) : 0 <= cnt l x := by
  induction l <;> grind

theorem cnt_app (a b : List Int) (x : Int) :
    cnt (app a b) x = cnt a x + cnt b x := by
  induction a <;> grind

theorem cnt_upd (l : List Int) (i v x : Int)
    (h1 : 0 <= i) (h2 : i < len l) :
    cnt (upd l i v) x
      = cnt l x - (if elem l i = x then 1 else 0)
        + (if v = x then 1 else 0) := by
  induction l generalizing i <;> grind

@[grind] def erase : List Int -> Int -> List Int
  | [], _ => []
  | y :: t, x => if y = x then t else y :: erase t x

theorem cnt_erase (l : List Int) (a x : Int) (h : 0 < cnt l a) :
    cnt (erase l a) x = cnt l x - (if a = x then 1 else 0) := by
  induction l <;> grind [cnt_nonneg]

theorem len_erase (l : List Int) (a : Int) (h : 0 < cnt l a) :
    len (erase l a) = len l - 1 := by
  induction l <;> grind [cnt_nonneg]

theorem cnt_zero_nil (l : List Int) (h : ∀ x : Int, cnt l x = 0) : l = [] := by
  cases l with
  | nil => rfl
  | cons y t => have := h y; have := cnt_nonneg t y; grind

theorem perm_len (a b : List Int) (h : perm a b) : len a = len b := by
  induction a generalizing b with
  | nil =>
    have : b = [] := cnt_zero_nil b (fun x => (h x).symm)
    grind
  | cons y t ih =>
    have hy : 0 < cnt b y := by have := h y; have := cnt_nonneg t y; grind
    have hperm : perm t (erase b y) := by
      intro x
      have := h x
      have := cnt_erase b y x hy
      grind
    have := ih (erase b y) hperm
    have := len_erase b y hy
    grind

theorem perm_refl (l : List Int) : perm l l := by
  intro x; rfl

theorem perm_trans (a b c : List Int)
    (h1 : perm a b) (h2 : perm b c) : perm a c := by
  intro x; exact (h1 x).trans (h2 x)

theorem perm_swap (l : List Int) (i j : Int)
    (h1 : 0 <= i) (h2 : i < len l) (h3 : 0 <= j) (h4 : j < len l) :
    perm l (upd (upd l i (elem l j)) j (elem l i)) := by
  intro x
  have hu := len_upd l i (elem l j)
  have e1 := cnt_upd l i (elem l j) x h1 h2
  have e2 := cnt_upd (upd l i (elem l j)) j (elem l i) x h3 (by grind)
  have e3 := elem_upd l i j (elem l j) h1 h2
  grind

-- the exact composition quicksort's reborrow produces, keyed on the
-- goal atom so instantiation is non-generative
theorem perm_glue2 (l0 a a' b b' : List Int)
    (h0 : perm l0 (app a b))
    (h1 : perm a a') (h2 : perm b b') :
    perm l0 (app a' b') := by
  intro x
  have e0 := h0 x
  have e1 := cnt_app a b x
  have e2 := cnt_app a' b' x
  grind [perm]

theorem perm_glue_right (l0 a b b' : List Int)
    (h0 : perm l0 (app a b))
    (h2 : perm b b') :
    perm l0 (app a b') := by
  intro x
  have e0 := h0 x
  have e1 := cnt_app a b x
  have e2 := cnt_app a b' x
  grind [perm]

theorem cnt_pos_le (l : List Int) (p x : Int)
    (ha : all_le l p) (hx : 0 < cnt l x) : x <= p := by
  induction l <;> grind [cnt_nonneg]

theorem cnt_pos_ge (l : List Int) (p x : Int)
    (ha : all_ge l p) (hx : 0 < cnt l x) : x >= p := by
  induction l <;> grind [cnt_nonneg]

theorem all_le_of_cnt (l : List Int) (p : Int)
    (h : ∀ x : Int, 0 < cnt l x → x <= p) : all_le l p := by
  induction l with
  | nil => grind
  | cons y t ih =>
    have hy := h y
    have hn := cnt_nonneg t y
    have ht : all_le t p := by
      apply ih; intro x hx
      have := cnt_nonneg t x
      exact h x (by grind)
    grind

theorem all_ge_of_cnt (l : List Int) (p : Int)
    (h : ∀ x : Int, 0 < cnt l x → x >= p) : all_ge l p := by
  induction l with
  | nil => grind
  | cons y t ih =>
    have hy := h y
    have hn := cnt_nonneg t y
    have ht : all_ge t p := by
      apply ih; intro x hx
      have := cnt_nonneg t x
      exact h x (by grind)
    grind

theorem all_le_perm (a b : List Int) (p : Int)
    (h : perm a b) (ha : all_le a p) : all_le b p := by
  apply all_le_of_cnt
  intro x hx
  exact cnt_pos_le a p x ha (by rw [h x]; exact hx)

theorem all_ge_perm (a b : List Int) (p : Int)
    (h : perm a b) (ha : all_ge a p) : all_ge b p := by
  apply all_ge_of_cnt
  intro x hx
  exact cnt_pos_ge a p x ha (by rw [h x]; exact hx)

-- ----- bespoke partition-maintenance lemmas -----
-- After swap i j (i <= j < len l), the <=-prefix grows by the
-- swapped-in elem l j and the middle window slides to (i+1, j+1).
theorem swap_le (l : List Int) (p i j : Int)
    (h1 : 0 <= i) (h2 : i <= j) (h3 : j < len l)
    (hle : all_le (take i l) p) (hj : elem l j <= p) :
    all_le (take (i + 1) (upd (upd l i (elem l j)) j (elem l i))) p := by
  have hlu : len (upd (upd l i (elem l j)) j (elem l i)) = len l := by
    grind [len_upd]
  have ht : take i (upd (upd l i (elem l j)) j (elem l i)) = take i l := by
    grind [take_upd_ge]
  have hs := take_snoc i (upd (upd l i (elem l j)) j (elem l i))
    h1 (by grind)
  have he := elem_upd (upd l i (elem l j)) j i (elem l i)
    (by grind) (by grind [len_upd])
  have he2 := elem_upd l i i (elem l j) h1 (by grind)
  grind [all_le_app]

theorem swap_mid (l : List Int) (p i j : Int)
    (h1 : 0 <= i) (h2 : i <= j) (h3 : j < len l)
    (hge : all_ge (seg i j l) p) :
    all_ge (seg (i + 1) (j + 1) (upd (upd l i (elem l j)) j (elem l i))) p := by
  by_cases hij : i = j
  · subst hij
    have : ((i : Int) + 1) - (i + 1) = 0 := by grind
    grind [take_nonpos]
  · -- i < j: window (i+1, j+1) of the swapped list is the old
    -- (i+1, j) window plus old elem l i at position j
    have hlu : len (upd (upd l i (elem l j)) j (elem l i)) = len l := by
      grind [len_upd]
    have hsn := seg_snoc (i + 1) j (upd (upd l i (elem l j)) j (elem l i))
      (by grind) (by grind) (by grind)
    have hout1 := seg_upd_out (i + 1) j j (elem l i) (upd l i (elem l j))
      (by grind) (by grind)
    have hout2 := seg_upd_out (i + 1) j i (elem l j) l
      (by grind) (by grind)
    have hcons := seg_cons i j l h1 (by grind) (by grind)
    have he := elem_upd (upd l i (elem l j)) j j (elem l i)
      (by grind) (by grind [len_upd])
    grind [all_ge_app]

-- Final phase (the pivot sits at j = len l - 1): after swap i j the
-- suffix from i is all >= p, with p itself now at position i.
theorem swap_final_ge (l : List Int) (p i j : Int)
    (h1 : 0 <= i) (h2 : i <= j) (hj : j = len l - 1)
    (hpiv : elem l j = p)
    (hge : all_ge (seg i j l) p) :
    all_ge (drop i (upd (upd l i (elem l j)) j (elem l i))) p := by
  have hlu : len (upd (upd l i (elem l j)) j (elem l i)) = len l := by
    grind [len_upd]
  have hdc := drop_cons i (upd (upd l i (elem l j)) j (elem l i))
    h1 (by grind)
  have hei : elem (upd (upd l i (elem l j)) j (elem l i)) i = p := by
    have := elem_upd (upd l i (elem l j)) j i (elem l i) (by grind)
      (by grind [len_upd])
    have := elem_upd l i i (elem l j) h1 (by grind)
    grind
  by_cases hij : i = j
  · -- singleton suffix: drop (i+1) is empty
    have := drop_all (i + 1) (upd (upd l i (elem l j)) j (elem l i))
      (by grind)
    grind [all_ge_app]
  · -- i < j: suffix = p :: old-window(i+1..j) ++ [old elem l i]
    have hds := drop_split (i + 1) j (upd (upd l i (elem l j)) j (elem l i))
      (by grind) (by grind) (by grind)
    have hout1 := seg_upd_out (i + 1) j j (elem l i) (upd l i (elem l j))
      (by grind) (by grind)
    have hout2 := seg_upd_out (i + 1) j i (elem l j) l
      (by grind) (by grind)
    have hcons := seg_cons i j l h1 (by grind) (by grind)
    have hdcj := drop_cons j (upd (upd l i (elem l j)) j (elem l i))
      (by grind) (by grind)
    have hej := elem_upd (upd l i (elem l j)) j j (elem l i)
      (by grind) (by grind [len_upd])
    have := drop_all (j + 1) (upd (upd l i (elem l j)) j (elem l i))
      (by grind)
    grind [all_ge_app]

-- ----- split3 glue -----
theorem seg_take1 (i : Int) (l : List Int)
    (h1 : 0 <= i) (h2 : i < len l) :
    seg i (i + 1) l = sngl (elem l i) := by
  have := take1_drop i l h1 h2
  grind

theorem app3_decomp (i j : Int) (l : List Int)
    (h1 : 0 <= i) (h2 : i <= j) (h3 : j <= len l) :
    app (take i l) (app (seg i j l) (drop j l)) = l := by
  have := take_drop_app i l
  have := drop_split i j l h1 h2 h3
  grind

theorem sorted_glue (a b : List Int) (p : Int)
    (ha : sorted a) (hb : sorted b)
    (hle : all_le a p) (hge : all_ge b p) :
    sorted (app a (app (sngl p) b)) := by
  have h1 := sorted_cons_app b p hge hb
  have h2 : all_ge (app (sngl p) b) p := by grind [all_ge_app]
  exact sorted_app_bound a (app (sngl p) b) p ha h1 hle h2

theorem perm_glue3_mid (l0 a a' b c c' : List Int)
    (h0 : perm l0 (app a (app b c)))
    (h1 : perm a a') (h2 : perm c c') :
    perm l0 (app a' (app b c')) := by
  intro x
  have e0 := h0 x
  have e1 := cnt_app a (app b c) x
  have e2 := cnt_app b c x
  have e3 := cnt_app a' (app b c') x
  have e4 := cnt_app b c' x
  grind [perm]

theorem all_ge_suffix (l : List Int) (p n : Int)
    (h : all_ge l p) : all_ge (drop n l) p := by
  induction l generalizing n <;> grind

theorem swap_final_ge1 (l : List Int) (p i j : Int)
    (h1 : 0 <= i) (h2 : i <= j) (hj : j = len l - 1)
    (hpiv : elem l j = p)
    (hge : all_ge (seg i j l) p) :
    all_ge (drop (i + 1) (upd (upd l i (elem l j)) j (elem l i))) p := by
  have hall := swap_final_ge l p i j h1 h2 hj hpiv hge
  have hdd := drop_drop 1 i (upd (upd l i (elem l j)) j (elem l i))
    (by grind) h1
  have := all_ge_suffix (drop i (upd (upd l i (elem l j)) j (elem l i))) p 1 hall
  grind

-- ----- E-matching interface for the VCs -----
grind_pattern len_nonneg => len l
grind_pattern len_take => take i l
grind_pattern len_drop => drop i l
grind_pattern len_app => app a b
grind_pattern len_upd => upd l i v
grind_pattern elem_upd => elem (upd l i v) j
grind_pattern take_nonpos => take i l
grind_pattern take_upd_ge => take n (upd l i v)
grind_pattern take_drop_app => take i l, drop i l
grind_pattern take1_drop => take 1 (drop k l)
grind_pattern seg_snoc => seg i (j + 1) l
grind_pattern all_le_app => all_le (app a b) p
grind_pattern all_ge_app => all_ge (app a b) p
grind_pattern sorted_short => sorted l
grind_pattern sorted_cons_app => app (sngl p) b
grind_pattern sorted_app_bound => app a b, all_le a p
grind_pattern sorted_elem => sorted l, elem l i, elem l j
grind_pattern perm_len => perm a b
grind_pattern perm_refl => perm l l
grind_pattern perm_trans => perm a b, perm b c
grind_pattern perm_swap => upd (upd l i (elem l j)) j (elem l i)
grind_pattern perm_glue2 => perm l0 (app a' b'), perm a a', perm b b'
grind_pattern perm_glue_right => perm l0 (app a b'), perm b b'
grind_pattern swap_le => all_le (take (i + 1) (upd (upd l i (elem l j)) j (elem l i))) p
grind_pattern swap_mid => all_ge (seg (i + 1) (j + 1) (upd (upd l i (elem l j)) j (elem l i))) p
grind_pattern swap_final_ge => all_ge (drop i (upd (upd l i (elem l j)) j (elem l i))) p
grind_pattern swap_final_ge1 => all_ge (drop (i + 1) (upd (upd l i (elem l j)) j (elem l i))) p
grind_pattern seg_take1 => seg i (i + 1) l
grind_pattern app3_decomp => take i l, seg i j l, drop j l
grind_pattern sorted_glue => app a (app (sngl p) b)
grind_pattern perm_glue3_mid => perm l0 (app a' (app b c')), perm a a', perm c c'
grind_pattern all_le_perm => perm a b, all_le a p
grind_pattern all_ge_perm => perm a b, all_ge a p
|lean}]

val anew : (n : int{ 0 <= _ }) -> (v : int) -> varr{ len (cts _) = n } @ unique

val alen :
  (x : varr) @ unique ->
  (int{ _ = len (cts x) } * varr{ cts _ = cts x }) @ unique

val aget :
  (x : varr) @ unique -> (i : int{ 0 <= _ && _ < len (cts x) }) ->
  (int{ _ = elem (cts x) i } * varr{ cts _ = cts x }) @ unique

val new_proph : unit -> proph @ unique

val borrow :
  (p : proph) @ unique -> (x : varr) @ unique ->
  ((m : slice{ now _ = cts x && fin _ = pv p }) @ local unique -> 'b @ unique)
    @ once local ->
  (varr{ cts _ = pv p } * 'b) @ unique

val slen :
  (m : slice) @ local unique ->
  (int{ _ = len (now m) } * slice{ now _ = now m && fin _ = fin m })
    @ local unique

val sget :
  (m : slice) @ local unique -> (i : int{ 0 <= _ && _ < len (now m) }) ->
  (int{ _ = elem (now m) i } * slice{ now _ = now m && fin _ = fin m })
    @ local unique

val sset :
  (m : slice) @ local unique -> (i : int{ 0 <= _ && _ < len (now m) }) ->
  (v : int) ->
  slice{ now _ = upd (now m) i v && fin _ = fin m } @ local unique

val split :
  (pl : proph) @ unique ->
  (pr : proph) @ unique ->
  (m : slice) @ local unique ->
  (i : int{ 0 <= _ && _ <= len (now m) }) ->
  ((left : slice{ now _ = take i (now m) && fin _ = pv pl }) @ local unique ->
   (right : slice{ now _ = drop i (now m) && fin _ = pv pr }) @ local unique ->
   'a @ unique)
    @ once local ->
  (slice{ now _ = app (pv pl) (pv pr) && fin _ = fin m } * 'a) @ local unique


val split3 :
  (p1 : proph) @ unique ->
  (p2 : proph) @ unique ->
  (p3 : proph) @ unique ->
  (m : slice) @ local unique ->
  (i : int{ 0 <= _ }) ->
  (j : int{ i <= _ && _ <= len (now m) }) ->
  ((a : slice{ now _ = take i (now m) && fin _ = pv p1 }) @ local unique ->
   (b : slice{ now _ = seg i j (now m) && fin _ = pv p2 }) @ local unique ->
   (c : slice{ now _ = drop j (now m) && fin _ = pv p3 }) @ local unique ->
   'a @ unique)
    @ once local ->
  (slice{ now _ = app (pv p1) (app (pv p2) (pv p3)) && fin _ = fin m } * 'a)
    @ local unique

val sdrop : (m : slice) @ local unique -> unit{ fin m = now m }
val sdropa : (m : slice) @ local -> unit{ fin m = now m }
