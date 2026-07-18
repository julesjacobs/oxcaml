/- Reflective Farkas prelude (core Lean 4, no mathlib). Proved ONCE; the emitter then
   only supplies ground data + `by decide` on the coefficient cancellation. -/
namespace OxsmtFarkas

abbrev Assign := Nat → Int

-- a linear expression: list of (coeff, var) terms + a constant
abbrev Terms := List (Int × Nat)
abbrev LinExpr := Terms × Int

def evalT : Terms → Assign → Int
  | [], _ => 0
  | (c, v) :: t, ρ => c * ρ v + evalT t ρ

def eval (e : LinExpr) (ρ : Assign) : Int := evalT e.1 ρ + e.2

def scaleT (m : Int) : Terms → Terms
  | [] => []
  | (c, v) :: t => (m * c, v) :: scaleT m t

def scale (m : Int) (e : LinExpr) : LinExpr := (scaleT m e.1, m * e.2)

def addL (a b : LinExpr) : LinExpr := (a.1 ++ b.1, a.2 + b.2)

-- add coefficient c for variable v into an accumulator, merging same var
def addCoeff (v : Nat) (c : Int) : Terms → Terms
  | [] => [(c, v)]
  | (c', v') :: t => if v = v' then (c + c', v') :: t else (c', v') :: addCoeff v c t

def mergeT : Terms → Terms
  | [] => []
  | (c, v) :: t => addCoeff v c (mergeT t)

def dropZeros : Terms → Terms
  | [] => []
  | (c, v) :: t => if c = 0 then dropZeros t else (c, v) :: dropZeros t

def normalizeT (t : Terms) : Terms := dropZeros (mergeT t)

-- ρ-homomorphism lemmas
theorem evalT_append (a b : Terms) (ρ : Assign) :
    evalT (a ++ b) ρ = evalT a ρ + evalT b ρ := by
  induction a with
  | nil => simp [evalT]
  | cons hd t ih => cases hd with | mk c v => simp [evalT, ih, Int.add_assoc]

theorem evalT_scale (m : Int) (a : Terms) (ρ : Assign) :
    evalT (scaleT m a) ρ = m * evalT a ρ := by
  induction a with
  | nil => simp [evalT, scaleT]
  | cons hd t ih =>
    cases hd with
    | mk c v => simp [evalT, scaleT, ih, Int.mul_add, Int.mul_assoc]

theorem eval_scale (m : Int) (e : LinExpr) (ρ : Assign) :
    eval (scale m e) ρ = m * eval e ρ := by
  cases e with
  | mk t k => simp [eval, scale, evalT_scale, Int.mul_add]

theorem eval_add (a b : LinExpr) (ρ : Assign) :
    eval (addL a b) ρ = eval a ρ + eval b ρ := by
  cases a with
  | mk ta ka => cases b with
    | mk tb kb =>
      simp only [eval, addL, evalT_append]
      -- (evalT ta + evalT tb) + (ka + kb) = (evalT ta + ka) + (evalT tb + kb)
      ac_rfl

theorem evalT_addCoeff (v : Nat) (c : Int) (t : Terms) (ρ : Assign) :
    evalT (addCoeff v c t) ρ = c * ρ v + evalT t ρ := by
  induction t with
  | nil => simp [addCoeff, evalT]
  | cons hd tl ih =>
    cases hd with
    | mk c' v' =>
      simp only [addCoeff]
      by_cases h : v = v'
      · subst h; simp only [if_true, evalT, Int.add_mul]; ac_rfl
      · rw [if_neg h]; simp only [evalT, ih]; ac_rfl

theorem evalT_merge (t : Terms) (ρ : Assign) :
    evalT (mergeT t) ρ = evalT t ρ := by
  induction t with
  | nil => simp [mergeT, evalT]
  | cons hd tl ih =>
    cases hd with
    | mk c v => simp [mergeT, evalT_addCoeff, evalT, ih]

theorem evalT_dropZeros (t : Terms) (ρ : Assign) :
    evalT (dropZeros t) ρ = evalT t ρ := by
  induction t with
  | nil => simp [dropZeros, evalT]
  | cons hd tl ih =>
    cases hd with
    | mk c v =>
      by_cases h : c = 0
      · subst h; simp only [dropZeros, if_true, evalT, ih]; simp
      · rw [dropZeros]; rw [if_neg h]; simp only [evalT, ih]

theorem evalT_normalize (t : Terms) (ρ : Assign) :
    evalT (normalizeT t) ρ = evalT t ρ := by
  simp [normalizeT, evalT_dropZeros, evalT_merge]

-- The weighted combination of premises: ∑ mᵢ · eᵢ, built as a LinExpr.
def combine : List (Int × LinExpr) → LinExpr
  | [] => ([], 0)
  | (m, e) :: rest => addL (scale m e) (combine rest)

-- m ≥ 0, x ≤ 0  ⇒  m * x ≤ 0
theorem mul_nonpos {m x : Int} (hm : 0 ≤ m) (hx : x ≤ 0) : m * x ≤ 0 := by
  have h := Int.mul_le_mul_of_nonneg_left hx hm
  simpa using h

-- If every premise has a nonnegative multiplier and evaluates ≤ 0, the combination ≤ 0.
theorem combine_nonpos (l : List (Int × LinExpr)) (ρ : Assign)
    (H : ∀ me ∈ l, 0 ≤ me.1 ∧ eval me.2 ρ ≤ 0) :
    eval (combine l) ρ ≤ 0 := by
  induction l with
  | nil => simp [combine, eval, evalT]
  | cons hd tl ih =>
    cases hd with
    | mk m e =>
      have hhead := H (m, e) (by simp)
      have htl : ∀ me ∈ tl, 0 ≤ me.1 ∧ eval me.2 ρ ≤ 0 := by
        intro me hme; exact H me (by simp [hme])
      have hstep : eval (combine ((m, e) :: tl)) ρ = m * eval e ρ + eval (combine tl) ρ := by
        simp [combine, eval_add, eval_scale]
      rw [hstep]
      exact Int.add_nonpos (mul_nonpos hhead.1 hhead.2) (ih htl)

/- The Farkas refutation. Every premise is a true half-plane [eval eᵢ ρ ≤ 0] with a
   nonnegative multiplier; the combination's variable part normalizes to EMPTY (the ground
   [decide] the emitter discharges) and its constant is strictly positive. Contradiction. -/
theorem farkas_false (l : List (Int × LinExpr)) (ρ : Assign)
    (H : ∀ me ∈ l, 0 ≤ me.1 ∧ eval me.2 ρ ≤ 0)
    (hcancel : normalizeT (combine l).1 = [])
    (hpos : 0 < (combine l).2) :
    False := by
  have hle : eval (combine l) ρ ≤ 0 := combine_nonpos l ρ H
  have hz : evalT (combine l).1 ρ = 0 := by
    have := evalT_normalize (combine l).1 ρ
    rw [hcancel] at this; simpa [evalT] using this.symm
  have hval : eval (combine l) ρ = (combine l).2 := by
    simp [eval, hz]
  rw [hval] at hle
  exact Int.lt_irrefl 0 (Int.lt_of_lt_of_le hpos hle)

end OxsmtFarkas

-- Self-test: a concrete infeasible pair (x ≤ 0 ∧ -x + 1 ≤ 0) refuted through farkas_false
-- with multipliers (1, 1). Guards that the exported lemma is actually usable as the emitter
-- drives it. Kept in the prelude so `lean farkas_prelude.lean` exercises the full path.
section SelfTest
open OxsmtFarkas
example (x : Int) (h1 : x ≤ 0) (h2 : -x + 1 ≤ 0) : False := by
  refine farkas_false [(1, ([(1, 0)], 0)), (1, ([((-1 : Int), 0)], 1))]
    (fun n => if n = 0 then x else 0) ?_ (by decide) (by decide)
  intro me hme
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hme
  rcases hme with rfl | rfl
  · exact ⟨by decide, by simp [eval, evalT]; exact h1⟩
  · exact ⟨by decide, by simp [eval, evalT]; exact h2⟩
end SelfTest
