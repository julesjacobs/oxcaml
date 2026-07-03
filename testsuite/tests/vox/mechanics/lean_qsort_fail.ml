(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* Slice-borrow soundness probes: the attacks that must NOT verify,
   each pinned to its rejection layer.  The API is ascribed inline
   (same signatures as demo/slice_lib, which the positive test
   demo/lean_qsort.ml exercises).  Lean rejects out-of-bounds access,
   a "sort" that does not sort, and claims about UNRESOLVED
   prophecies (an undropped loan is a sound leak: its prophecy stays
   opaque); the mode checkers reject strong-update reuse, prophecy
   reuse, loan escape from a bracket, and -- the fork-join
   guarantee -- sending the SAME loan to both tasks. *)

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
grind_pattern all_le_perm => perm a b, all_le a p
grind_pattern all_ge_perm => perm a b, all_ge a p
|lean}]
[%%expect{|
|}]

module S : sig
  type varr
  type proph
  type slice

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
  
  val sdrop : (m : slice) @ local unique -> unit{ fin m = now m }
  val sdropa : (m : slice) @ local -> unit{ fin m = now m }

  val fork_join2 :
    (unit -> 'a @ unique) @ once local ->
    (unit -> 'b @ unique) @ once local ->
    ('a * 'b) @ unique
end = struct
  type varr = A of { base : int array }
  type proph = P of { u : unit }
  type slice = L of { base : int array; off_ : int; len_ : int }
  
  let anew : (n : int{ 0 <= _ }) -> (v : int) -> varr{ len (cts _) = n } @ unique =
    fun n v -> assume_unchecked_ (Obj.magic_unique (A { base = Array.make n v }))
  
  let alen :
    (x : varr) @ unique ->
    (int{ _ = len (cts x) } * varr{ cts _ = cts x }) @ unique =
    fun x ->
      let (A { base }) = x in
      let n = Array.length base in
      Obj.magic_unique
        ( (assume_unchecked_ n : int{ _ = len (cts x) }),
          (assume_unchecked_ (A { base }) : varr{ cts _ = cts x }) )
  
  let aget :
    (x : varr) @ unique -> (i : int{ 0 <= _ && _ < len (cts x) }) ->
    (int{ _ = elem (cts x) i } * varr{ cts _ = cts x }) @ unique =
    fun x i ->
      let (A { base }) = x in
      let v = base.(i) in
      Obj.magic_unique
        ( (assume_unchecked_ v : int{ _ = elem (cts x) i }),
          (assume_unchecked_ (A { base }) : varr{ cts _ = cts x }) )
  
  let new_proph : unit -> proph @ unique =
    fun () -> Obj.magic_unique (P { u = () })
  
  let borrow :
    (p : proph) @ unique -> (x : varr) @ unique ->
    ((m : slice{ now _ = cts x && fin _ = pv p }) @ local unique -> 'b @ unique)
      @ once local ->
    (varr{ cts _ = pv p } * 'b) @ unique =
    fun p x k ->
      let (P _) = p in
      let (A { base }) = x in
      let m0 =
        (assume_unchecked_
           (Obj.magic_unique (L { base; off_ = 0; len_ = Array.length base }))
          : slice{ now _ = cts x && fin _ = pv p })
      in
      let b = k m0 in
      Obj.magic_unique
        ((assume_unchecked_ (A { base }) : varr{ cts _ = pv p }), b)
  
  let slen :
    (m : slice) @ local unique ->
    (int{ _ = len (now m) } * slice{ now _ = now m && fin _ = fin m })
      @ local unique =
    fun m ->
      let (L { base; off_; len_ }) = m in
      exclave_
        (Obj.magic_unique
           ( (assume_unchecked_ len_ : int{ _ = len (now m) }),
             (assume_unchecked_ (L { base; off_; len_ })
               : slice{ now _ = now m && fin _ = fin m }) ))
  
  let sget :
    (m : slice) @ local unique -> (i : int{ 0 <= _ && _ < len (now m) }) ->
    (int{ _ = elem (now m) i } * slice{ now _ = now m && fin _ = fin m })
      @ local unique =
    fun m i ->
      let (L { base; off_; len_ }) = m in
      let v = base.(off_ + i) in
      exclave_
        (Obj.magic_unique
           ( (assume_unchecked_ v : int{ _ = elem (now m) i }),
             (assume_unchecked_ (L { base; off_; len_ })
               : slice{ now _ = now m && fin _ = fin m }) ))
  
  let sset :
    (m : slice) @ local unique -> (i : int{ 0 <= _ && _ < len (now m) }) ->
    (v : int) ->
    slice{ now _ = upd (now m) i v && fin _ = fin m } @ local unique =
    fun m i v ->
      let (L { base; off_; len_ }) = m in
      base.(off_ + i) <- v;
      exclave_
        (Obj.magic_unique
           (assume_unchecked_ (L { base; off_; len_ })
             : slice{ now _ = upd (now m) i v && fin _ = fin m }))
  
  let split :
    (pl : proph) @ unique ->
    (pr : proph) @ unique ->
    (m : slice) @ local unique ->
    (i : int{ 0 <= _ && _ <= len (now m) }) ->
    ((left : slice{ now _ = take i (now m) && fin _ = pv pl }) @ local unique ->
     (right : slice{ now _ = drop i (now m) && fin _ = pv pr }) @ local unique ->
     'a @ unique)
      @ once local ->
    (slice{ now _ = app (pv pl) (pv pr) && fin _ = fin m } * 'a) @ local unique =
    fun pl pr m i k ->
      let (P _) = pl in
      let (P _) = pr in
      let (L { base; off_; len_ }) = m in
      let left =
        (assume_unchecked_ (Obj.magic_unique (L { base; off_; len_ = i }))
          : slice{ now _ = take i (now m) && fin _ = pv pl })
      in
      let right =
        (assume_unchecked_
           (Obj.magic_unique (L { base; off_ = off_ + i; len_ = len_ - i }))
          : slice{ now _ = drop i (now m) && fin _ = pv pr })
      in
      let a = k left right in
      exclave_
        (Obj.magic_unique
           ( (assume_unchecked_ (L { base; off_; len_ })
               : slice{ now _ = app (pv pl) (pv pr) && fin _ = fin m }),
             a ))
  
  let sdrop : (m : slice) @ local unique -> unit{ fin m = now m } =
    fun m ->
      let (L _) = m in
      assume_unchecked_ ()
  
  let sdropa : (m : slice) @ local -> unit{ fin m = now m } =
    fun m ->
      let (L _) = m in
      assume_unchecked_ ()

  external unsafe_globalize_task :
  (unit -> 'a @ unique) @ once local -> (unit -> 'a) = "%identity"

  let fork_join2 :
    (unit -> 'a @ unique) @ once local ->
    (unit -> 'b @ unique) @ once local ->
    ('a * 'b) @ unique =
    fun f g ->
      let f = unsafe_globalize_task f in
      let g = unsafe_globalize_task g in
      let a = f () in
      let b = g () in
      Obj.magic_unique (a, b)
end
[%%expect{|
module S :
  sig
    type varr
    type proph
    type slice
    val anew :
      (n : int{ 0 <= _ }) -> int -> varr{ (len (cts _)) = n } @ unique
    val alen :
      (x : varr) @ unique ->
      int{ _ = (len (cts x)) } * varr{ (cts _) = (cts x) } @ unique
    val aget :
      (x : varr) @ unique ->
      (i : int{ (0 <= _) && (_ < (len (cts x))) }) ->
      int{ _ = (elem (cts x) i) } * varr{ (cts _) = (cts x) } @ unique
    val new_proph : unit -> proph @ unique
    val borrow :
      (p : proph) @ unique ->
      (x : varr) @ unique ->
      (slice{ ((now _) = (cts x)) && ((fin _) = (pv p)) } @ local unique ->
       'b @ unique) @ local
      once -> varr{ (cts _) = (pv p) } * 'b @ unique
    val slen :
      (m : slice) @ local unique ->
      int{ _ = (len (now m)) } *
      slice{ ((now _) = (now m)) && ((fin _) = (fin m)) } @ local unique
    val sget :
      (m : slice) @ local unique ->
      (i : int{ (0 <= _) && (_ < (len (now m))) }) ->
      int{ _ = (elem (now m) i) } *
      slice{ ((now _) = (now m)) && ((fin _) = (fin m)) } @ local unique
    val sset :
      (m : slice) @ local unique ->
      (i : int{ (0 <= _) && (_ < (len (now m))) }) ->
      (v : int) ->
      slice{ ((now _) = (upd (now m) i v)) && ((fin _) = (fin m)) } @ local
      unique
    val split :
      (pl : proph) @ unique ->
      (pr : proph) @ unique ->
      (m : slice) @ local unique ->
      (i : int{ (0 <= _) && (_ <= (len (now m))) }) ->
      (slice{ ((now _) = (take i (now m))) && ((fin _) = (pv pl)) } @ local
       unique ->
       slice{ ((now _) = (drop i (now m))) && ((fin _) = (pv pr)) } @ local
       unique -> 'a @ unique) @ local
      once ->
      slice{ ((now _) = (app (pv pl) (pv pr))) && ((fin _) = (fin m)) } * 'a @ local
      unique
    val sdrop : (m : slice) @ local unique -> unit{ (fin m) = (now m) }
    val sdropa : (m : slice) @ local -> unit{ (fin m) = (now m) }
    val fork_join2 :
      (unit -> 'a @ unique) @ local once ->
      (unit -> 'b @ unique) @ local once -> 'a * 'b @ unique
  end
|}]

open S

(* LEAN LAYER.  Out of bounds: reading at [len] (one past the end)
   leaves the bounds contract unprovable. *)
let oob : (x : varr) @ unique -> int =
  fun x ->
    let p = new_proph () in
    let (x', v) =
      borrow p x (fun m ->
        let (n, m0) = slen m in
        let (v, m1) = sget m0 n in
        let _u = sdrop m1 in
        (v : int))
    in
    ignore x';
    v
[%%expect{|
Line 11, characters 30-31:
11 |         let (v, m1) = sget m0 n in
                                   ^
Error: vox: verification failed (lean).
       Goal: (0 <= n) && (n < (len (now m0)))
Hypotheses:
  n = (len (now m))
  ((now m0) = (now m)) && ((fin m0) = (fin m))
  ((now m) = (cts x)) && ((fin m) = (pv p))
Possible counterexample:
  n = 0
  len (now m0) = 0
  len (now m) = 0
(lean: error: `grind` failed)
|}]

(* A "sort" that returns its loan untouched cannot claim [sorted]:
   the counterexample is an unsorted two-element segment. *)
let fake_sort :
  (m : slice) @ local unique ->
  slice{ perm (now m) (now _) && sorted (now _) && fin _ = fin m }
    @ local unique =
  fun m -> m
[%%expect{|
Line 5, characters 11-12:
5 |   fun m -> m
               ^
Error: vox: verification failed (lean).
       Goal: ((perm (now m) (now m)) && (sorted (now m))) && ((fin m) = (fin m))
Hypotheses: <none>
Possible counterexample:
  len (now m) = 2
(lean: error: `grind` failed)
|}]

(* An unresolved prophecy is a SOUND LEAK: the loan dies at the
   bracket without [sdrop], and nothing ties [pv p] to anything --
   even its length is unprovable. *)
let leak : (x : varr) @ unique -> unit =
  fun x ->
    let p = new_proph () in
    let (x', u) =
      borrow p x (fun m ->
        let (_n, m0) = slen m in
        let _dead = m0 in
        (() : unit{ len (pv p) = len (cts x) }))
    in
    ignore x'; ignore u
[%%expect{|
Line 8, characters 9-11:
8 |         (() : unit{ len (pv p) = len (cts x) }))
             ^^
Error: vox: verification failed (lean).
       Goal: (len (pv p)) = (len (cts x))
Hypotheses:
  _dead = m0
  _n = (len (now m))
  ((now m0) = (now m)) && ((fin m0) = (fin m))
  ((now m) = (cts x)) && ((fin m) = (pv p))
Possible counterexample:
  _n = 1
  len (cts x) = 1
  len (now m) = 1
  len (pv p) = 0
(lean: error: `grind` failed)
|}]

(* MODE LAYER.  Strong update consumes: writing twice through the
   same loan name is a stale view. *)
let reuse : (m : slice{ 2 <= len (now _) }) @ local unique -> unit =
  fun m ->
    let m1 = sset m 0 1 in
    let m2 = sset m 1 2 in
    let _u1 = sdrop m1 in
    let _u2 = sdrop m2 in
    ()
[%%expect{|
Line 4, characters 18-19:
4 |     let m2 = sset m 1 2 in
                      ^
Error: This value is used here, but it has already been used as unique at:
Line 3, characters 18-19:
3 |     let m1 = sset m 0 1 in
                      ^

|}]

(* One prophecy, one loan: reusing it for both halves of a split
   would let two resolutions prove False, so the second use is
   rejected. *)
let preuse : (m : slice) @ local unique -> unit =
  fun m ->
    let pl = new_proph () in
    let (mres, u) =
      split pl pl m 0 (fun left right ->
        let _u1 = sdrop left in
        let _u2 = sdrop right in
        ())
    in
    let _u3 = sdrop mres in
    ignore u
[%%expect{|
Line 5, characters 15-17:
5 |       split pl pl m 0 (fun left right ->
                   ^^
Error: This value is used here, but it is also being used as unique at:
Line 5, characters 12-14:
5 |       split pl pl m 0 (fun left right ->
                ^^

|}]

(* A sub-loan cannot escape its bracket: the continuation's result
   must be global, and the loan is local. *)
let escape : (m : slice) @ local unique -> unit =
  fun m ->
    let pl = new_proph () in
    let pr = new_proph () in
    let (mres, l) =
      split pl pr m 0 (fun left right ->
        let _u = sdrop right in
        left)
    in
    let _u2 = sdrop mres in
    let _u3 = sdrop l in
    ()
[%%expect{|
Line 8, characters 8-12:
8 |         left)
            ^^^^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

(* THE FORK-JOIN GUARANTEE: both tasks want the same loan -- the
   second once-closure captures a loan the first already consumed.
   Disjointness is not a convention; it is the mode checker. *)
let race : (m : slice{ 2 <= len (now _) }) @ local unique -> unit =
  fun m ->
    let (a, b) =
      fork_join2
        (fun () ->
          let m1 = sset m 0 1 in
          let _u = sdrop m1 in
          ())
        (fun () ->
          let m2 = sset m 1 2 in
          let _u = sdrop m2 in
          ())
    in
    ignore a; ignore b
[%%expect{|
Line 10, characters 24-25:
10 |           let m2 = sset m 1 2 in
                             ^
Error: This value is used here, but it is also being used as unique at:
Line 6, characters 24-25:
6 |           let m1 = sset m 0 1 in
                            ^

|}]
