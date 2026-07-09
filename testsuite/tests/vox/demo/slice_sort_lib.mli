(* The MERGE of merge sort, fully verified over slice_lib's loans --
   nothing here is trusted.  [merged] is the functional spec (stable,
   left-biased on ties); the theorems prove it a sorted permutation
   of the concatenation; [fill_inv] is the loop invariant of the
   implementation's fill phase, with one hand-proved step lemma per
   branch, keyed so the VCs discharge by e-matching.

   [merge_sorted_halves] is PROOF-STYLE: it consumes the loan,
   merges in place, and RESOLVES the prophecy -- the refined unit
   reports the result through [fin m], the same way [sdrop] does.
   It does not require the halves to be sorted: its contract is the
   exact [merged] equation, and sortedness of the result is a
   theorem ([sorted_merged]) the caller gets when the halves were. *)

open Slice_lib

[%%vox.lean {lean|
@[expose] public def merged : List Int -> List Int -> List Int
  | [], right => right
  | left, [] => left
  | x :: xs, y :: ys =>
    if x <= y
    then x :: merged xs (y :: ys)
    else y :: merged (x :: xs) ys
termination_by left right => left.length + right.length
decreasing_by all_goals simp_wf

public theorem cnt_merged (left right : List Int) (value : Int) :
    cnt (merged left right) value = cnt left value + cnt right value := by
  fun_induction merged left right with
  | case1 => grind
  | case2 => grind
  | case3 x xs y ys hxy ih =>
    have := ih
    grind
  | case4 x xs y ys hxy ih =>
    have := ih
    grind

public theorem perm_merged (left right : List Int) :
    perm (merged left right) (app left right) := by
  intro value
  rw [cnt_merged, cnt_app]

public theorem perm_symm (left right : List Int) (h : perm left right) :
    perm right left := by
  intro value
  exact (h value).symm

public theorem perm_app_merged (left right : List Int) :
    perm (app left right) (merged left right) :=
  perm_symm _ _ (perm_merged left right)

public theorem perm_split_results (original left right : List Int) (k : Int)
    (hl : perm (take k original) left)
    (hr : perm (drop k original) right) :
    perm original (app left right) := by
  have h0 : perm original (app (take k original) (drop k original)) := by
    rw [take_drop_app]
    exact perm_refl original
  exact perm_glue2 original (take k original) left
    (drop k original) right h0 hl hr

public theorem merged_all_ge (left right : List Int) (bound : Int)
    (hl : all_ge left bound) (hr : all_ge right bound) :
    all_ge (merged left right) bound := by
  fun_induction merged left right <;> grind

public theorem all_ge_weaken (values : List Int) (high low : Int)
    (hge : all_ge values high) (hle : low <= high) :
    all_ge values low := by
  induction values <;> grind

public theorem sorted_tail_ge (head : Int) (tail : List Int)
    (hs : sorted (head :: tail)) : all_ge tail head := by
  induction tail generalizing head with
  | nil => grind
  | cons next rest ih =>
    have hrest := ih next (by grind)
    have hweak := all_ge_weaken rest next head hrest (by grind)
    grind

public theorem sorted_tail (head : Int) (tail : List Int)
    (hs : sorted (head :: tail)) : sorted tail := by
  cases tail <;> grind

public theorem sorted_merged (left right : List Int)
    (hl : sorted left) (hr : sorted right) :
    sorted (merged left right) := by
  fun_induction merged left right with
  | case1 => grind
  | case2 => grind
  | case3 x xs y ys hxy ih =>
    have hsx := sorted_tail x xs hl
    have hsy := sorted_tail y ys hr
    have hxs := sorted_tail_ge x xs hl
    have hys := sorted_tail_ge y ys hr
    have hysx := all_ge_weaken ys y x hys hxy
    have hright : all_ge (y :: ys) x := by grind
    have htail := merged_all_ge xs (y :: ys) x hxs hright
    have hs := ih hsx hr
    exact sorted_cons_ge _ _ htail hs
  | case4 x xs y ys hxy ih =>
    have hsx := sorted_tail x xs hl
    have hsy := sorted_tail y ys hr
    have hxs := sorted_tail_ge x xs hl
    have hleft : all_ge (x :: xs) y := by
      have hweaken := all_ge_weaken xs x y hxs (by grind)
      grind
    have hys := sorted_tail_ge y ys hr
    have htail := merged_all_ge (x :: xs) ys y hleft hys
    have hs := ih hl hsy
    exact sorted_cons_ge _ _ htail hs

-- ----- list plumbing the loop proofs need -----
public theorem merged_nil (l : List Int) : merged l [] = l := by
  cases l <;> grind [merged]

public theorem len_merged (a b : List Int) : len (merged a b) = len a + len b := by
  fun_induction merged a b <;> grind

public theorem app_nil (l : List Int) : app l [] = l := by
  induction l <;> grind

public theorem app_assoc (a b c : List Int) :
    app (app a b) c = app a (app b c) := by
  induction a <;> grind

public theorem take_all (i : Int) (l : List Int) (h : len l <= i) :
    take i l = l := by
  induction l generalizing i with
  | nil => grind
  | cons x t ih => have := ih (i - 1); have := len_nonneg t; grind

public theorem take_app_exact (i : Int) (a b : List Int) (h : i = len a) :
    take i (app a b) = a := by
  induction a generalizing i with
  | nil => grind [take_nonpos]
  | cons x t ih => have := ih (i - 1); have := len_nonneg t; grind

public theorem drop_app_exact (i : Int) (a b : List Int) (h : i = len a) :
    drop i (app a b) = b := by
  induction a generalizing i with
  | nil => grind [drop_nonpos]
  | cons x t ih => have := ih (i - 1); have := len_nonneg t; grind

-- one-step unfoldings of [merged], phrased on the head element so
-- the step lemmas below can case on which side the loop consumed
public theorem merged_step_left (x : Int) (xs ys : List Int)
    (h : ys = [] ∨ x <= elem ys 0) :
    merged (x :: xs) ys = x :: merged xs ys := by
  cases ys with
  | nil => grind [merged_nil, merged]
  | cons y t =>
    cases h with
    | inl h => grind
    | inr h => grind [merged]

public theorem merged_step_right (y : Int) (xs ys : List Int)
    (h : xs = [] ∨ y < elem xs 0) :
    merged xs (y :: ys) = y :: merged xs ys := by
  cases xs with
  | nil => grind [merged]
  | cons x t =>
    cases h with
    | inl h => grind
    | inr h => grind [merged]

-- ----- the fill-phase invariant -----
-- After consuming [i] elements of the left run [take k src] and
-- [j - k] of the right run [drop k src], the output prefix written
-- so far, continued by the merge of what remains, is the full merge.
@[expose] public def fill_inv (src tmp : List Int) (k i j : Int) : Prop :=
  len tmp = len src
  /\ app (take (i + j - k) tmp)
       (merged (seg i k src) (seg j (len src) src))
     = merged (take k src) (drop k src)

public theorem fill_inv_len (src tmp : List Int) (k i j : Int)
    (h : fill_inv src tmp k i j) : len tmp = len src := h.1

public theorem fill_inv_init (src tmp : List Int) (k : Int)
    (hlen : len tmp = len src) (h0 : 0 <= k) (hk : k <= len src) :
    fill_inv src tmp k 0 k := by
  have h1 := take_nonpos (0 + k - k) tmp (by omega)
  have h2 := drop_nonpos 0 src (by omega)
  have h3 := len_drop k src h0 hk
  have h4 := take_all (len src - k) (drop k src) (by omega)
  unfold fill_inv
  grind

public theorem fill_inv_left (src tmp : List Int) (k i j : Int)
    (h0 : 0 <= i) (hik : i < k) (hkj : k <= j) (hjn : j <= len src)
    (hpick : j = len src ∨ elem src i <= elem src j)
    (hinv : fill_inv src tmp k i j) :
    fill_inv src (upd tmp (i + j - k) (elem src i)) k (i + 1) j := by
  obtain ⟨hlen, heq⟩ := hinv
  have hlu := len_upd tmp (i + j - k) (elem src i)
  have htu := take_upd_ge (i + j - k) (i + j - k) (elem src i) tmp
    (by omega)
  have hsnoc := take_snoc (i + j - k) (upd tmp (i + j - k) (elem src i))
    (by omega) (by omega)
  have helem := elem_upd tmp (i + j - k) (i + j - k) (elem src i)
    (by omega) (by omega)
  have hconsl := seg_cons i k src h0 hik (by omega)
  have hstep : merged (seg i k src) (seg j (len src) src)
      = elem src i :: merged (seg (i + 1) k src) (seg j (len src) src) := by
    by_cases hj : j = len src
    · have hnil : seg j (len src) src = [] := by
        have := take_nonpos (len src - j) (drop j src) (by omega)
        grind
      have := merged_step_left (elem src i) (seg (i + 1) k src)
        (seg j (len src) src) (Or.inl hnil)
      grind
    · have hle : elem src i <= elem src j := by grind
      have hconsr := seg_cons j (len src) src (by omega) (by omega)
        (by omega)
      have := merged_step_left (elem src i) (seg (i + 1) k src)
        (seg j (len src) src) (by grind)
      grind
  have hassoc := app_assoc (take (i + j - k) tmp) (sngl (elem src i))
    (merged (seg (i + 1) k src) (seg j (len src) src))
  unfold fill_inv
  constructor
  · grind
  · have harith : i + 1 + j - k = (i + j - k) + 1 := by omega
    grind

public theorem fill_inv_right (src tmp : List Int) (k i j : Int)
    (h0 : 0 <= i) (hik : i <= k) (hkj : k <= j) (hjn : j < len src)
    (hpick : i = k ∨ elem src j < elem src i)
    (hinv : fill_inv src tmp k i j) :
    fill_inv src (upd tmp (i + j - k) (elem src j)) k i (j + 1) := by
  obtain ⟨hlen, heq⟩ := hinv
  have hlu := len_upd tmp (i + j - k) (elem src j)
  have htu := take_upd_ge (i + j - k) (i + j - k) (elem src j) tmp
    (by omega)
  have hsnoc := take_snoc (i + j - k) (upd tmp (i + j - k) (elem src j))
    (by omega) (by omega)
  have helem := elem_upd tmp (i + j - k) (i + j - k) (elem src j)
    (by omega) (by omega)
  have hconsr := seg_cons j (len src) src (by omega) (by omega) (by omega)
  have hstep : merged (seg i k src) (seg j (len src) src)
      = elem src j :: merged (seg i k src) (seg (j + 1) (len src) src) := by
    by_cases hi : i = k
    · have hnil : seg i k src = [] := by
        have := take_nonpos (k - i) (drop i src) (by omega)
        grind
      have := merged_step_right (elem src j) (seg i k src)
        (seg (j + 1) (len src) src) (Or.inl hnil)
      grind
    · have hgt : elem src j < elem src i := by grind
      have hconsl := seg_cons i k src h0 (by omega) (by omega)
      have := merged_step_right (elem src j) (seg i k src)
        (seg (j + 1) (len src) src) (by grind)
      grind
  have hassoc := app_assoc (take (i + j - k) tmp) (sngl (elem src j))
    (merged (seg i k src) (seg (j + 1) (len src) src))
  unfold fill_inv
  constructor
  · grind
  · have harith : i + (j + 1) - k = (i + j - k) + 1 := by omega
    grind

public theorem fill_inv_done (src tmp : List Int) (k : Int)
    (h0 : 0 <= k) (hk : k <= len src)
    (hinv : fill_inv src tmp k k (len src)) :
    tmp = merged (take k src) (drop k src) := by
  obtain ⟨hlen, heq⟩ := hinv
  have h1 : seg k k src = [] := by
    have := take_nonpos (k - k) (drop k src) (by omega)
    grind
  have h2 : seg (len src) (len src) src = [] := by
    have := take_nonpos (len src - len src) (drop (len src) src) (by omega)
    grind
  have h3 := take_all (k + len src - k) tmp (by omega)
  have h4 := app_nil (take (k + len src - k) tmp)
  grind [merged]

-- ----- the copy-back step -----
public theorem copy_step (dst src : List Int) (idx : Int)
    (h0 : 0 <= idx) (hlt : idx < len src) (hlen : len dst = len src)
    (hpre : take idx dst = take idx src) :
    take (idx + 1) (upd dst idx (elem src idx)) = take (idx + 1) src := by
  have h1 := take_snoc idx (upd dst idx (elem src idx)) h0
    (by have := len_upd dst idx (elem src idx); omega)
  have h2 := take_upd_ge idx idx (elem src idx) dst (by omega)
  have h3 := elem_upd dst idx idx (elem src idx) h0 (by omega)
  have h4 := take_snoc idx src h0 hlt
  grind

grind_pattern perm_merged => perm (merged left right) (app left right)
grind_pattern perm_app_merged => perm (app left right) (merged left right)
grind_pattern perm_split_results =>
  perm original (app left right), take k original, drop k original
grind_pattern sorted_merged => sorted (merged left right)
grind_pattern len_merged => merged a b
grind_pattern take_all => take i l
grind_pattern take_app_exact => take i (app a b)
grind_pattern drop_app_exact => drop i (app a b)
grind_pattern fill_inv_len => fill_inv src tmp k i j
grind_pattern fill_inv_init => fill_inv src tmp k 0 k
grind_pattern fill_inv_left => fill_inv src tmp k i j, elem src i
grind_pattern fill_inv_right => fill_inv src tmp k i j, elem src j
grind_pattern fill_inv_done => fill_inv src tmp k k (len src)
grind_pattern copy_step => take (idx + 1) (upd dst idx (elem src idx))
|lean}]

val merge_sorted_halves :
  (k : int{ 0 <= _ }) ->
  (m : slice{ k <= len (now _) }) @ local unique ->
  unit{ fin m = merged (take k (now m)) (drop k (now m)) }
