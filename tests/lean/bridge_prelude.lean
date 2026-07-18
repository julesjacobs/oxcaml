/- Bridge prelude (core Lean 4, no mathlib). Connects the Boolean resolution domain
   (OxsmtRes, res_prelude.lean) to the reflective Farkas domain (OxsmtFarkas,
   farkas_prelude.lean); BOTH must be prepended. Proved ONCE; emitted per-file proofs only
   APPLY `leaf_sat` and discharge the Farkas side conditions by ground `by decide` — the
   same discipline as the Rung-2 leaf emitter. -/
namespace OxsmtBridge

open OxsmtRes
open OxsmtFarkas

/- A LIA theory-leaf clause is the negation of Farkas-infeasible premises. If the Boolean
   assignment [ρb] sets each premise's SAT var to [decide (eval row ρf ≤ 0)] (the premise
   atom's arithmetic truth under the integer interpretation [ρf]), then the leaf clause (all
   negative literals over those vars) is satisfied by [ρb]: Farkas makes the premises jointly
   false, so some premise is false, so its negated literal is true.

   [prems] entries are [(satvar, (multiplier, reflective-row))]. The last three hypotheses
   are EXACTLY the ground Farkas data the Rung-2 leaf checker discharges by [decide]. *) -/
theorem leaf_sat
    (prems : List (Nat × (Int × LinExpr)))
    (ρb : OxsmtRes.Assign)
    (ρf : OxsmtFarkas.Assign)
    (hlink : ∀ p ∈ prems, ρb p.1 = decide (OxsmtFarkas.eval p.2.2 ρf ≤ 0))
    (hnn : ∀ p ∈ prems, 0 ≤ p.2.1)
    (hcancel : OxsmtFarkas.normalizeT (OxsmtFarkas.combine (prems.map (fun p => p.2))).1 = [])
    (hpos : 0 < (OxsmtFarkas.combine (prems.map (fun p => p.2))).2) :
    OxsmtRes.satClause ρb (prems.map (fun p => ((false, p.1) : OxsmtRes.Lit))) = true := by
  cases hc :
    OxsmtRes.satClause ρb (prems.map (fun p => ((false, p.1) : OxsmtRes.Lit))) with
  | true => rfl
  | false =>
    exfalso
    rw [OxsmtRes.satClause, List.any_eq_false] at hc
    -- hc : ∀ x ∈ prems.map (fun p => (false, p.1)), satLit ρb x ≠ true
    apply OxsmtFarkas.farkas_false (prems.map (fun p => p.2)) ρf _ hcancel hpos
    intro me hme
    rw [List.mem_map] at hme
    obtain ⟨p, hp, rfl⟩ := hme
    refine ⟨hnn p hp, ?_⟩
    have hlf : ¬OxsmtRes.satLit ρb (false, p.1) = true :=
      hc _ (List.mem_map_of_mem hp)
    have hb : ρb p.1 = true := by
      cases h : ρb p.1 with
      | true => rfl
      | false => exact absurd (by simp [OxsmtRes.satLit, h]) hlf
    rw [hlink p hp] at hb
    exact of_decide_eq_true hb

end OxsmtBridge
