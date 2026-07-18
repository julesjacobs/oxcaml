/- Bridge prelude (core Lean 4, no mathlib). Connects the Boolean resolution domain
   (OxsmtRes, res_prelude.lean) to the reflective Farkas domain (OxsmtFarkas,
   farkas_prelude.lean); BOTH must be prepended. Proved ONCE (this is trusted checker
   substrate; per team-lead's 2026-07-17 ruling `omega` is permitted HERE — cert-independent
   closed lemmas, kernel still checks the term, axioms stay ⊆ {propext, Quot.sound} — but
   NEVER in an emitted per-certificate file, which the gate enforces mechanically). Emitted
   per-file proofs only APPLY these lemmas as terms + ground `by decide`. -/
namespace OxsmtBridge

open OxsmtRes
open OxsmtFarkas

/- Row transforms that build a Farkas ≤0 half-plane from an atom's reflective row.
   [negbump r] is the integer-strengthened negation (-r + 1 ≤ 0 ⟺ ¬(r ≤ 0) over ℤ), the
   cut a negated [Le] premise contributes. [subL a b] is a - b, the (=0) row an equality
   premise contributes. -/
def negbump (r : LinExpr) : LinExpr := (scaleT (-1) r.1, -r.2 + 1)
def subL (a b : LinExpr) : LinExpr := addL a (scale (-1) b)

theorem eval_negbump (r : LinExpr) (ρ : OxsmtFarkas.Assign) : eval (negbump r) ρ = -(eval r ρ) + 1 := by
  simp only [negbump, eval, evalT_scale]
  omega

theorem eval_subL (a b : LinExpr) (ρ : OxsmtFarkas.Assign) : eval (subL a b) ρ = eval a ρ - eval b ρ := by
  simp only [subL, eval_add, eval_scale]
  omega

theorem strengthen_neg (x : Int) : decide (x ≤ 0) = !decide (-x + 1 ≤ 0) := by
  by_cases h : x ≤ 0 <;> simp [h] <;> omega

/- Per-premise implication helpers: "leaf literal unsatisfied ⟹ its Farkas half-plane
   holds" — the one direction [farkas_false] needs. Each takes the emitter's definitional
   [ρb v = decide (atom)] and the [satLit … = false] fact; the emitter applies the matching
   one as a TERM. -/

theorem prem_pos (r : LinExpr) (ρf : OxsmtFarkas.Assign) (ρb : OxsmtRes.Assign) (v : Nat)
    (hv : ρb v = decide (eval r ρf ≤ 0))
    (h : OxsmtRes.satLit ρb (false, v) = false) : eval r ρf ≤ 0 := by
  have hbt : ρb v = true := by
    cases hh : ρb v with
    | true => rfl
    | false => simp [OxsmtRes.satLit, hh] at h
  rw [hv] at hbt
  exact of_decide_eq_true hbt

theorem prem_neg (r : LinExpr) (ρf : OxsmtFarkas.Assign) (ρb : OxsmtRes.Assign) (v : Nat)
    (hv : ρb v = decide (eval r ρf ≤ 0))
    (h : OxsmtRes.satLit ρb (true, v) = false) : eval (negbump r) ρf ≤ 0 := by
  have hbf : ρb v = false := by
    cases hh : ρb v with
    | true => simp [OxsmtRes.satLit, hh] at h
    | false => rfl
  rw [hv] at hbf
  have hnp : ¬eval r ρf ≤ 0 := of_decide_eq_false hbf
  rw [eval_negbump]
  omega

theorem prem_eq (a b : LinExpr) (ρf : OxsmtFarkas.Assign) (ρb : OxsmtRes.Assign) (v : Nat)
    (hv : ρb v = decide (eval a ρf = eval b ρf))
    (h : OxsmtRes.satLit ρb (false, v) = false) : eval (subL a b) ρf ≤ 0 := by
  have hbt : ρb v = true := by
    cases hh : ρb v with
    | true => rfl
    | false => simp [OxsmtRes.satLit, hh] at h
  rw [hv] at hbt
  have heq : eval a ρf = eval b ρf := of_decide_eq_true hbt
  rw [eval_subL]
  omega

/- The theory-leaf discharge. Each premise carries its leaf literal [ℓ], nonnegative
   multiplier, and Farkas ≤0 row; [hlink] says each unsatisfied leaf literal forces its row
   ≤ 0. Since Farkas makes the weighted rows sum to a positive constant, they cannot all be
   ≤ 0, so some leaf literal is satisfied — the leaf clause holds. The Farkas conditions
   (nonneg multipliers, cancellation, positive constant) are the ground data the emitter
   discharges by [decide]. -/
theorem leaf_sat
    (prems : List (OxsmtRes.Lit × (Int × LinExpr)))
    (ρb : OxsmtRes.Assign)
    (ρf : OxsmtFarkas.Assign)
    (hlink : ∀ p ∈ prems, OxsmtRes.satLit ρb p.1 = false → OxsmtFarkas.eval p.2.2 ρf ≤ 0)
    (hnn : ∀ p ∈ prems, 0 ≤ p.2.1)
    (hcancel : OxsmtFarkas.normalizeT (OxsmtFarkas.combine (prems.map (fun p => p.2))).1 = [])
    (hpos : 0 < (OxsmtFarkas.combine (prems.map (fun p => p.2))).2) :
    OxsmtRes.satClause ρb (prems.map (fun p => p.1)) = true := by
  cases hc : OxsmtRes.satClause ρb (prems.map (fun p => p.1)) with
  | true => rfl
  | false =>
    exfalso
    rw [OxsmtRes.satClause, List.any_eq_false] at hc
    apply OxsmtFarkas.farkas_false (prems.map (fun p => p.2)) ρf _ hcancel hpos
    intro me hme
    rw [List.mem_map] at hme
    obtain ⟨p, hp, rfl⟩ := hme
    refine ⟨hnn p hp, ?_⟩
    have hlf : ¬OxsmtRes.satLit ρb p.1 = true := hc _ (List.mem_map_of_mem hp)
    have hfalse : OxsmtRes.satLit ρb p.1 = false := by
      cases hh : OxsmtRes.satLit ρb p.1 with
      | true => exact absurd hh hlf
      | false => rfl
    exact hlink p hp hfalse

end OxsmtBridge
