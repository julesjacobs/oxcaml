/- Reflective propositional resolution prelude (core Lean 4, no mathlib). The Rung-3 core:
   clauses over Boolean vars, resolution steps as lemma applications, empty clause = ⊥. -/
namespace OxsmtRes

abbrev Assign := Nat → Bool
abbrev Lit := Bool × Nat        -- (polarity, var):  (true,v)=v   (false,v)=¬v
abbrev Clause := List Lit

def satLit (ρ : Assign) (l : Lit) : Bool := ρ l.2 == l.1
def satClause (ρ : Assign) (c : Clause) : Bool := c.any (satLit ρ)

-- append of clauses: satisfied iff either side is
theorem satClause_append (ρ : Assign) (a b : Clause) :
    satClause ρ (a ++ b) = (satClause ρ a || satClause ρ b) := by
  simp [satClause, List.any_append]

-- the empty clause is never satisfied
theorem satClause_nil (ρ : Assign) : satClause ρ [] = false := by
  simp [satClause]

-- removing a FALSE literal preserves satisfaction (the core of a resolution step)
theorem sat_erase_false (ρ : Assign) (c : Clause) (l : Lit)
    (hl : satLit ρ l = false) (hc : satClause ρ c = true) :
    satClause ρ (c.erase l) = true := by
  induction c with
  | nil => simp [satClause] at hc
  | cons hd tl ih =>
    by_cases h : hd = l
    · subst h
      -- erase drops the head; its sat is false so the rest must carry the truth
      simp only [List.erase_cons_head]
      have : satClause ρ (hd :: tl) = (satLit ρ hd || satClause ρ tl) := by
        simp [satClause]
      rw [this, hl] at hc
      simpa using hc
    · rw [List.erase_cons_tail (by simp [h])]
      have hsplit : satClause ρ (hd :: tl) = (satLit ρ hd || satClause ρ tl) := by
        simp [satClause]
      have hsplit2 : satClause ρ (hd :: tl.erase l) = (satLit ρ hd || satClause ρ (tl.erase l)) := by
        simp [satClause]
      rw [hsplit] at hc
      rw [hsplit2]
      by_cases hh : satLit ρ hd = true
      · simp [hh]
      · simp only [Bool.or_eq_true] at hc ⊢
        rcases hc with hc | hc
        · exact absurd hc (by simp [hh])
        · exact Or.inr (ih hc)

-- RESOLUTION step: the resolvent of two satisfied clauses on pivot p is satisfied.
-- (Holds unconditionally: if a pivot literal is absent, its erase is a no-op.)
theorem resolve (ρ : Assign) (p : Nat) (c1 c2 : Clause)
    (h1 : satClause ρ c1 = true) (h2 : satClause ρ c2 = true) :
    satClause ρ (c1.erase (true, p) ++ c2.erase (false, p)) = true := by
  rw [satClause_append]
  by_cases hp : ρ p = true
  · have hlf : satLit ρ (false, p) = false := by simp [satLit, hp]
    have := sat_erase_false ρ c2 (false, p) hlf h2
    simp [this]
  · have hp' : ρ p = false := by simpa using hp
    have hlf : satLit ρ (true, p) = false := by simp [satLit, hp']
    have := sat_erase_false ρ c1 (true, p) hlf h1
    simp [this]

-- A refutation ending in the empty clause is a contradiction.
theorem empty_absurd (ρ : Assign) (h : satClause ρ [] = true) : False := by
  rw [satClause_nil] at h; exact Bool.noConfusion h

-- Self-test: the unit clauses p and ¬p resolve to the empty clause → False.
example (ρ : Assign) (h1 : satClause ρ [(true, 0)] = true) (h2 : satClause ρ [(false, 0)] = true) :
    False := by
  have hr := resolve ρ 0 [(true, 0)] [(false, 0)] h1 h2
  simp only [List.erase_cons_head, List.append_nil] at hr
  exact empty_absurd ρ hr

-- Monotonicity: a clause entailed by a satisfied SUBSET is satisfied. Lets the emitter
-- reconcile a computed resolvent with the certificate's learned clause (same literals up to
-- order/duplication): the subset check is ground `by decide` at each use site.
theorem sat_mono (ρ : Assign) (a b : Clause)
    (hsub : ∀ l ∈ a, l ∈ b) (h : satClause ρ a = true) : satClause ρ b = true := by
  simp only [satClause, List.any_eq_true] at h ⊢
  rcases h with ⟨l, hla, hl⟩
  exact ⟨l, hsub l hla, hl⟩

end OxsmtRes

