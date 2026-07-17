/- PIFO trees: stateless flush-equivalence implies interleaved-equivalence. Authored by Codex and Claude. -/
import PifoStatement


namespace PifoGeneral
open PifoStatement

variable {α γ : Type}




def Agree (ρ1 ρ2 : α → Nat) (x y : α) : Prop :=
  (ρ1 x < ρ1 y ↔ ρ2 x < ρ2 y) ∧ (ρ1 x = ρ1 y ↔ ρ2 x = ρ2 y)

theorem Agree.symm {ρ1 ρ2 : α → Nat} {x y : α}
    (h : Agree ρ1 ρ2 x y) : Agree ρ1 ρ2 y x := by
  obtain ⟨h1, h2⟩ := h
  constructor <;> omega


inductive Linked (D : α → α → Prop) : α → α → Prop
  | base {x y} : D x y → Linked D x y
  | refl (x) : Linked D x x
  | symm {x y} : Linked D x y → Linked D y x
  | trans {x y z} : Linked D x y → Linked D y z → Linked D x z


def Dis (ρ1 ρ2 : α → Nat) (x y : α) : Prop := ¬ Agree ρ1 ρ2 x y

theorem not_linked_agree {ρ1 ρ2 : α → Nat} {x y : α}
    (h : ¬ Linked (Dis ρ1 ρ2) x y) : Agree ρ1 ρ2 x y :=
  Classical.byContradiction fun hn => h (Linked.base hn)


theorem linked_colEq {ρ1 ρ2 : α → Nat} {col : α → γ}
    (H : ∀ x y, col x ≠ col y → Agree ρ1 ρ2 x y) :
    ∀ {x y : α}, Linked (Dis ρ1 ρ2) x y → col x = col y := by
  intro x y h
  induction h with
  | base hd => exact Classical.byContradiction fun hc => hd (H _ _ hc)
  | refl => rfl
  | symm _ ih => exact ih.symm
  | trans _ _ ih1 ih2 => exact ih1.trans ih2


theorem module_step {a1 b1 c1 a2 b2 c2 : Nat}
    (hD : ¬ (((a1 < b1) ↔ (a2 < b2)) ∧ ((a1 = b1) ↔ (a2 = b2))))
    (hzx : ((c1 < a1) ↔ (c2 < a2)) ∧ ((c1 = a1) ↔ (c2 = a2)))
    (hzy : ((c1 < b1) ↔ (c2 < b2)) ∧ ((c1 = b1) ↔ (c2 = b2))) :
    (((c1 < a1) ↔ (c1 < b1)) ∧ ((c1 = a1) ↔ (c1 = b1))) ∧
    (((c2 < a2) ↔ (c2 < b2)) ∧ ((c2 = a2) ↔ (c2 = b2))) ∧
    c1 ≠ a1 ∧ c2 ≠ a2 := by
  constructor
  · constructor <;> omega
  constructor
  · constructor <;> omega
  constructor <;> omega


def ModEq (ρ1 ρ2 : α → Nat) (z x y : α) : Prop :=
  ((ρ1 z < ρ1 x ↔ ρ1 z < ρ1 y) ∧ (ρ1 z = ρ1 x ↔ ρ1 z = ρ1 y)) ∧
  ((ρ2 z < ρ2 x ↔ ρ2 z < ρ2 y) ∧ (ρ2 z = ρ2 x ↔ ρ2 z = ρ2 y))

theorem ModEq.refl (ρ1 ρ2 : α → Nat) (z x : α) : ModEq ρ1 ρ2 z x x :=
  ⟨⟨Iff.rfl, Iff.rfl⟩, ⟨Iff.rfl, Iff.rfl⟩⟩

theorem ModEq.symm' {ρ1 ρ2 : α → Nat} {z x y : α}
    (h : ModEq ρ1 ρ2 z x y) : ModEq ρ1 ρ2 z y x :=
  ⟨⟨h.1.1.symm, h.1.2.symm⟩, ⟨h.2.1.symm, h.2.2.symm⟩⟩

theorem ModEq.trans' {ρ1 ρ2 : α → Nat} {z x y w : α}
    (h1 : ModEq ρ1 ρ2 z x y) (h2 : ModEq ρ1 ρ2 z y w) : ModEq ρ1 ρ2 z x w :=
  ⟨⟨h1.1.1.trans h2.1.1, h1.1.2.trans h2.1.2⟩,
   ⟨h1.2.1.trans h2.2.1, h1.2.2.trans h2.2.2⟩⟩


theorem linked_module {ρ1 ρ2 : α → Nat} {x y z : α}
    (hxy : Linked (Dis ρ1 ρ2) x y) :
    ¬ Linked (Dis ρ1 ρ2) z x → ModEq ρ1 ρ2 z x y := by
  induction hxy with
  | @base u v hd =>
    intro hz
    have hzu : Agree ρ1 ρ2 z u := not_linked_agree hz
    have hzv : Agree ρ1 ρ2 z v := not_linked_agree (fun h =>
      hz (h.trans (Linked.symm (Linked.base hd))))
    have := module_step (a1 := ρ1 u) (b1 := ρ1 v) (c1 := ρ1 z)
      (a2 := ρ2 u) (b2 := ρ2 v) (c2 := ρ2 z) hd ⟨hzu.1, hzu.2⟩ ⟨hzv.1, hzv.2⟩
    exact ⟨this.1, this.2.1⟩
  | refl u => intro _; exact ModEq.refl ρ1 ρ2 z u
  | @symm u v h ih =>
    intro hzv
    have hzu : ¬ Linked (Dis ρ1 ρ2) z u := fun hc => hzv (hc.trans h)
    exact (ih hzu).symm'
  | @trans u v w h1 h2 ih1 ih2 =>
    intro hzu
    have hzv : ¬ Linked (Dis ρ1 ρ2) z v := fun hc => hzu (hc.trans (Linked.symm h1))
    exact (ih1 hzu).trans' (ih2 hzv)


theorem linked_edge {D : α → α → Prop} :
    ∀ {x y : α}, Linked D x y → x = y ∨ ∃ u v, D u v ∧ Linked D u x := by
  intro x y h
  induction h with
  | @base u v hd => exact Or.inr ⟨u, v, hd, Linked.refl u⟩
  | refl u => exact Or.inl rfl
  | @symm u v h ih =>
    rcases ih with rfl | ⟨p, q, hd, hl⟩
    · exact Or.inl rfl
    · exact Or.inr ⟨p, q, hd, hl.trans h⟩
  | @trans u v w h1 h2 ih1 ih2 =>
    rcases ih1 with rfl | ⟨p, q, hd, hl⟩
    · rcases ih2 with rfl | ⟨p, q, hd, hl⟩
      · exact Or.inl rfl
      · exact Or.inr ⟨p, q, hd, hl⟩
    · exact Or.inr ⟨p, q, hd, hl⟩


theorem linked_strict {ρ1 ρ2 : α → Nat} {x x' z : α}
    (hxx : Linked (Dis ρ1 ρ2) x x') (hne : x ≠ x')
    (hz : ¬ Linked (Dis ρ1 ρ2) z x) :
    ρ1 z ≠ ρ1 x ∧ ρ2 z ≠ ρ2 x := by
  rcases linked_edge hxx with rfl | ⟨u, v, hd, hux⟩
  · exact absurd rfl hne
  · 
    by_cases h1 : ρ1 z = ρ1 x
    · exfalso
      have hzx : Agree ρ1 ρ2 z x := not_linked_agree hz
      have h2 : ρ2 z = ρ2 x := hzx.2.mp h1
      have hmu : ModEq ρ1 ρ2 z x u := linked_module (Linked.symm hux) hz
      have hvx : Linked (Dis ρ1 ρ2) v x := (Linked.symm (Linked.base hd)).trans hux
      have hmv : ModEq ρ1 ρ2 z x v := linked_module (Linked.symm hvx) hz
      have hu1 : ρ1 z = ρ1 u := hmu.1.2.mp h1
      have hu2 : ρ2 z = ρ2 u := hmu.2.2.mp h2
      have hv1 : ρ1 z = ρ1 v := hmv.1.2.mp h1
      have hv2 : ρ2 z = ρ2 v := hmv.2.2.mp h2
      exact hd ⟨by omega, by omega⟩
    · refine ⟨h1, fun h2 => h1 ?_⟩
      exact (not_linked_agree hz).2.mpr h2




def embedP (ρ : α → Nat) (p : α × Nat) : Entry α := ⟨p.1, ρ p.1, p.2⟩


def qstate (ρ : α → Nat) (xs : List (α × Nat)) : Queue α := xs.map (embedP ρ)


def DistinctArr (xs : List (α × Nat)) : Prop :=
  List.Pairwise (fun p q => p.2 ≠ q.2) xs


def removeArr (xs : List (α × Nat)) (a : Nat) : List (α × Nat) :=
  xs.filter (fun p => p.2 != a)

theorem better_iff (a b : Entry α) :
    better a b = true ↔ (a.rank < b.rank ∨ (a.rank = b.rank ∧ a.arr < b.arr)) := by
  simp [better]

theorem better_trans {a b c : Entry α}
    (h1 : better a b = true) (h2 : better b c = true) : better a c = true := by
  rw [better_iff] at *
  omega

theorem better_total {a b : Entry α} (hne : a.arr ≠ b.arr)
    (h : ¬ better a b = true) : better b a = true := by
  rw [better_iff] at *
  omega

theorem removeArr_of_not_mem {xs : List (α × Nat)} {a : Nat}
    (h : ∀ p ∈ xs, p.2 ≠ a) : removeArr xs a = xs := by
  induction xs with
  | nil => rfl
  | cons p ps ih =>
    show List.filter _ (p :: ps) = p :: ps
    rw [List.filter_cons]
    have hp : (p.2 != a) = true := by
      simpa using h p (List.mem_cons_self ..)
    rw [if_pos hp]
    exact congrArg (p :: ·) (ih fun q hq => h q (List.mem_cons_of_mem p hq))

theorem qpop_cons_some (e best : Entry α) (es rest : Queue α)
    (h : qpop es = some (best, rest)) :
    qpop (e :: es)
      = if better e best then some (e, es) else some (best, e :: rest) := by
  simp [qpop, h]


theorem qpop_state (ρ : α → Nat) :
    ∀ (p : α × Nat) (xs : List (α × Nat)), DistinctArr (p :: xs) →
      ∃ q, q ∈ p :: xs ∧
        qpop (qstate ρ (p :: xs))
          = some (embedP ρ q, qstate ρ (removeArr (p :: xs) q.2)) ∧
        ∀ r ∈ p :: xs, r ≠ q → better (embedP ρ q) (embedP ρ r) = true := by
  intro p xs
  induction xs generalizing p with
  | nil =>
    intro _
    refine ⟨p, List.mem_cons_self .., ?_, ?_⟩
    · show qpop [embedP ρ p] = _
      have : removeArr [p] p.2 = [] := by
        show List.filter _ [p] = []
        rw [List.filter_cons]
        simp
      rw [this]
      rfl
    · intro r hr hne
      cases hr with
      | head => exact absurd rfl hne
      | tail _ h => cases h
  | cons x xs ih =>
    intro hd
    have hd' : DistinctArr (x :: xs) := hd.sublist (List.sublist_cons_self ..)
    obtain ⟨q, hqmem, hqpop, hqmin⟩ := ih x hd'
    have hpq : p.2 ≠ q.2 := (List.pairwise_cons.mp hd).1 q hqmem
    have hcons : qstate ρ (p :: x :: xs) = embedP ρ p :: qstate ρ (x :: xs) := rfl
    rw [hcons, qpop_cons_some _ _ _ _ hqpop]
    by_cases hb : better (embedP ρ p) (embedP ρ q) = true
    · refine ⟨p, List.mem_cons_self .., ?_, ?_⟩
      · rw [if_pos hb]
        have : removeArr (p :: x :: xs) p.2 = x :: xs := by
          show List.filter _ (p :: x :: xs) = x :: xs
          rw [List.filter_cons]
          simp only [bne_self_eq_false, Bool.false_eq_true]
          exact removeArr_of_not_mem fun r hr =>
            fun h => (List.pairwise_cons.mp hd).1 r hr h.symm
        rw [this]
      · intro r hr hne
        cases hr with
        | head => exact absurd rfl hne
        | tail _ h =>
          by_cases hrq : r = q
          · subst hrq; exact hb
          · exact better_trans hb (hqmin r h hrq)
    · refine ⟨q, List.mem_cons_of_mem p hqmem, ?_, ?_⟩
      · rw [if_neg hb]
        have : removeArr (p :: x :: xs) q.2 = p :: removeArr (x :: xs) q.2 := by
          show List.filter _ (p :: x :: xs) = _
          rw [List.filter_cons]
          have : (p.2 != q.2) = true := by simpa using hpq
          rw [if_pos this]
          rfl
        rw [this]
        rfl
      · intro r hr hne
        cases hr with
        | head =>
          exact better_total hpq hb
        | tail _ h => exact hqmin r h hne



open Classical


noncomputable def countIf {β : Type} (P : β → Prop) : List β → Nat
  | []      => 0
  | p :: ps => (if P p then 1 else 0) + countIf P ps

theorem countIf_congr {β : Type} {P Q : β → Prop} :
    ∀ {xs : List β}, (∀ p ∈ xs, (P p ↔ Q p)) →
      countIf P xs = countIf Q xs := by
  intro xs
  induction xs with
  | nil => intro _; rfl
  | cons p ps ih =>
    intro h
    show (if P p then 1 else 0) + countIf P ps
       = (if Q p then 1 else 0) + countIf Q ps
    rw [ih fun q hq => h q (List.mem_cons_of_mem p hq)]
    by_cases hp : P p
    · rw [if_pos hp, if_pos ((h p (List.mem_cons_self ..)).mp hp)]
    · rw [if_neg hp, if_neg (fun hq => hp ((h p (List.mem_cons_self ..)).mpr hq))]

theorem countIf_append {β : Type} (P : β → Prop) (xs : List β) (p : β) :
    countIf P (xs ++ [p]) = countIf P xs + (if P p then 1 else 0) := by
  induction xs with
  | nil => show (if P p then 1 else 0) + 0 = 0 + (if P p then 1 else 0); omega
  | cons q qs ih =>
    show (if P q then 1 else 0) + countIf P (qs ++ [p])
       = (if P q then 1 else 0) + countIf P qs + (if P p then 1 else 0)
    rw [ih]
    omega

theorem countIf_pos {β : Type} {P : β → Prop} :
    ∀ {xs : List β}, 0 < countIf P xs → ∃ p ∈ xs, P p := by
  intro xs
  induction xs with
  | nil => intro h; exact absurd h (by simp [countIf])
  | cons p ps ih =>
    intro h
    by_cases hp : P p
    · exact ⟨p, List.mem_cons_self .., hp⟩
    · have : 0 < countIf P ps := by
        have : countIf P (p :: ps) = 0 + countIf P ps := by
          show (if P p then 1 else 0) + countIf P ps = _
          rw [if_neg hp]
        omega
      obtain ⟨q, hq, hPq⟩ := ih this
      exact ⟨q, List.mem_cons_of_mem p hq, hPq⟩

theorem countIf_mem_pos {β : Type} {P : β → Prop} {xs : List β} {p : β}
    (hmem : p ∈ xs) (hP : P p) : 0 < countIf P xs := by
  induction xs with
  | nil => cases hmem
  | cons q qs ih =>
    show 0 < (if P q then 1 else 0) + countIf P qs
    cases hmem with
    | head => rw [if_pos hP]; omega
    | tail _ h => have := ih h; omega


theorem countIf_removeArr {P : α × Nat → Prop} :
    ∀ {xs : List (α × Nat)} {x : α} {a : Nat}, DistinctArr xs → (x, a) ∈ xs →
      countIf P xs = countIf P (removeArr xs a) + (if P (x, a) then 1 else 0) := by
  intro xs
  induction xs with
  | nil => intro x a _ h; cases h
  | cons p ps ih =>
    intro x a hd hmem
    have hdp := List.pairwise_cons.mp hd
    cases hmem with
    | head =>
      have : removeArr ((x, a) :: ps) a = ps := by
        show List.filter _ _ = _
        rw [List.filter_cons]
        simp only [bne_self_eq_false, Bool.false_eq_true]
        exact removeArr_of_not_mem fun r hr h => hdp.1 r hr h.symm
      rw [this]
      show (if P (x, a) then 1 else 0) + countIf P ps
         = countIf P ps + (if P (x, a) then 1 else 0)
      omega
    | tail _ h =>
      have hpa : p.2 ≠ a := fun hpa => hdp.1 (x, a) h hpa
      have : removeArr (p :: ps) a = p :: removeArr ps a := by
        show List.filter _ _ = _
        rw [List.filter_cons, if_pos (by simpa using hpa)]; rfl
      rw [this]
      show (if P p then 1 else 0) + countIf P ps
         = (if P p then 1 else 0) + countIf P (removeArr ps a) + (if P (x, a) then 1 else 0)
      rw [ih hdp.2 h]
      omega



variable [DecidableEq α]


def valFilter (xs : List (α × Nat)) (z : α) : List (α × Nat) :=
  xs.filter (fun p => decide (p.1 = z))

theorem valFilter_append (xs : List (α × Nat)) (p : α × Nat) (z : α) :
    valFilter (xs ++ [p]) z
      = valFilter xs z ++ (if p.1 = z then [p] else []) := by
  show List.filter _ _ = _
  rw [List.filter_append]
  by_cases h : p.1 = z
  · rw [if_pos h]
    show valFilter xs z ++ List.filter _ [p] = _
    rw [List.filter_cons, if_pos (by simpa using h)]
    rfl
  · rw [if_neg h]
    show valFilter xs z ++ List.filter _ [p] = _
    rw [List.filter_cons, if_neg (by simpa using h)]
    rfl

theorem mem_valFilter {xs : List (α × Nat)} {p : α × Nat} {z : α} :
    p ∈ valFilter xs z ↔ p ∈ xs ∧ p.1 = z := by
  show p ∈ List.filter _ _ ↔ _
  rw [List.mem_filter]
  simp


theorem valFilter_removeArr_other {xs : List (α × Nat)} {y : α} {a : Nat} {z : α}
    (hd : DistinctArr xs) (hmem : (y, a) ∈ xs) (hne : y ≠ z) :
    valFilter (removeArr xs a) z = valFilter xs z := by
  induction xs with
  | nil => cases hmem
  | cons p ps ih =>
    have hdp := List.pairwise_cons.mp hd
    cases hmem with
    | head =>
      have h1 : removeArr ((y, a) :: ps) a = ps := by
        show List.filter _ _ = _
        rw [List.filter_cons]
        simp only [bne_self_eq_false, Bool.false_eq_true]
        exact removeArr_of_not_mem fun r hr h => hdp.1 r hr h.symm
      rw [h1]
      show valFilter ps z = List.filter _ ((y, a) :: ps)
      rw [List.filter_cons, if_neg (by simpa using hne)]
      rfl
    | tail _ h =>
      have hpa : p.2 ≠ a := fun hpa => hdp.1 (y, a) h hpa
      have h1 : removeArr (p :: ps) a = p :: removeArr ps a := by
        show List.filter _ _ = _
        rw [List.filter_cons, if_pos (by simpa using hpa)]; rfl
      rw [h1]
      show List.filter _ (p :: removeArr ps a) = List.filter _ (p :: ps)
      rw [List.filter_cons, List.filter_cons]
      by_cases hpz : p.1 = z
      · rw [if_pos (by simpa using hpz), if_pos (by simpa using hpz)]
        exact congrArg (p :: ·) (ih hdp.2 h)
      · rw [if_neg (by simpa using hpz), if_neg (by simpa using hpz)]
        exact ih hdp.2 h


theorem valFilter_removeArr_self {xs : List (α × Nat)} {a : Nat} {z : α} :
    valFilter (removeArr xs a) z = removeArr (valFilter xs z) a := by
  induction xs with
  | nil => rfl
  | cons p ps ih =>
    by_cases hpa : p.2 = a
    · have h1 : removeArr (p :: ps) a = removeArr ps a := by
        show List.filter _ _ = _
        rw [List.filter_cons, if_neg (by simpa using hpa)]; rfl
      rw [h1]
      by_cases hpz : p.1 = z
      · have h2 : valFilter (p :: ps) z = p :: valFilter ps z := by
          show List.filter _ _ = _
          rw [List.filter_cons, if_pos (by simpa using hpz)]; rfl
        rw [h2]
        have h3 : removeArr (p :: valFilter ps z) a = removeArr (valFilter ps z) a := by
          show List.filter _ _ = _
          rw [List.filter_cons, if_neg (by simpa using hpa)]; rfl
        rw [h3]
        exact ih
      · have h2 : valFilter (p :: ps) z = valFilter ps z := by
          show List.filter _ _ = _
          rw [List.filter_cons, if_neg (by simpa using hpz)]; rfl
        rw [h2]
        exact ih
    · have h1 : removeArr (p :: ps) a = p :: removeArr ps a := by
        show List.filter _ _ = _
        rw [List.filter_cons, if_pos (by simpa using hpa)]; rfl
      rw [h1]
      by_cases hpz : p.1 = z
      · have h2 : valFilter (p :: removeArr ps a) z = p :: valFilter (removeArr ps a) z := by
          show List.filter _ _ = _
          rw [List.filter_cons, if_pos (by simpa using hpz)]; rfl
        have h3 : valFilter (p :: ps) z = p :: valFilter ps z := by
          show List.filter _ _ = _
          rw [List.filter_cons, if_pos (by simpa using hpz)]; rfl
        rw [h2, h3]
        have h4 : removeArr (p :: valFilter ps z) a = p :: removeArr (valFilter ps z) a := by
          show List.filter _ _ = _
          rw [List.filter_cons, if_pos (by simpa using hpa)]; rfl
        rw [h4]
        exact congrArg (p :: ·) ih
      · have h2 : valFilter (p :: removeArr ps a) z = valFilter (removeArr ps a) z := by
          show List.filter _ _ = _
          rw [List.filter_cons, if_neg (by simpa using hpz)]; rfl
        have h3 : valFilter (p :: ps) z = valFilter ps z := by
          show List.filter _ _ = _
          rw [List.filter_cons, if_neg (by simpa using hpz)]; rfl
        rw [h2, h3]
        exact ih

omit [DecidableEq α] in

theorem removeArr_length {xs : List (α × Nat)} {x : α} {a : Nat}
    (hd : DistinctArr xs) (hmem : (x, a) ∈ xs) :
    (removeArr xs a).length + 1 = xs.length := by
  induction xs with
  | nil => cases hmem
  | cons p ps ih =>
    have hdp := List.pairwise_cons.mp hd
    cases hmem with
    | head =>
      have h1 : removeArr ((x, a) :: ps) a = ps := by
        show List.filter _ _ = _
        rw [List.filter_cons]
        simp only [bne_self_eq_false, Bool.false_eq_true]
        exact removeArr_of_not_mem fun r hr h => hdp.1 r hr h.symm
      rw [h1]
      rfl
    | tail _ h =>
      have hpa : p.2 ≠ a := fun hpa => hdp.1 (x, a) h hpa
      have h1 : removeArr (p :: ps) a = p :: removeArr ps a := by
        show List.filter _ _ = _
        rw [List.filter_cons, if_pos (by simpa using hpa)]; rfl
      rw [h1]
      show (removeArr ps a).length + 1 + 1 = ps.length + 1
      rw [ih hdp.2 h]

omit [DecidableEq α] in
theorem distinctArr_removeArr {xs : List (α × Nat)} {a : Nat}
    (hd : DistinctArr xs) : DistinctArr (removeArr xs a) :=
  hd.sublist List.filter_sublist

omit [DecidableEq α] in
theorem mem_removeArr {xs : List (α × Nat)} {p : α × Nat} {a : Nat}
    (h : p ∈ removeArr xs a) : p ∈ xs :=
  (List.mem_filter.mp h).1




inductive QOp (α : Type) where
  | push (x : α) (arr : Nat)
  | pop


def qrun (ρ : α → Nat) : Queue α → List (QOp α) → List (Option α)
  | _, []                => []
  | q, .push x a :: ops  => qrun ρ (q ++ [⟨x, ρ x, a⟩]) ops
  | q, .pop      :: ops  =>
    match qpop q with
    | none         => none       :: qrun ρ q  ops
    | some (e, q') => some e.val :: qrun ρ q' ops


def OkOps : Nat → List (QOp α) → Prop
  | _, []                => True
  | n, .push _ a :: ops  => n < a ∧ OkOps a ops
  | n, .pop      :: ops  => OkOps n ops


def AllBelow (xs : List (α × Nat)) (n : Nat) : Prop := ∀ p ∈ xs, p.2 ≤ n

omit [DecidableEq α] in
theorem allBelow_push {xs : List (α × Nat)} {n : Nat} (h : AllBelow xs n)
    {x : α} {a : Nat} (hna : n < a) : AllBelow (xs ++ [(x, a)]) a := by
  intro p hp
  rcases List.mem_append.mp hp with hp | hp
  · exact Nat.le_of_lt (Nat.lt_of_le_of_lt (h p hp) hna)
  · cases hp with
    | head => exact Nat.le_refl a
    | tail _ h' => cases h'

omit [DecidableEq α] in
theorem allBelow_removeArr {xs : List (α × Nat)} {n a : Nat}
    (h : AllBelow xs n) : AllBelow (removeArr xs a) n :=
  fun p hp => h p (mem_removeArr hp)

omit [DecidableEq α] in

theorem better_embedP (ρ : α → Nat) (p q : α × Nat) :
    better (embedP ρ p) (embedP ρ q) = true
      ↔ (ρ p.1 < ρ q.1 ∨ (ρ p.1 = ρ q.1 ∧ p.2 < q.2)) :=
  better_iff _ _

omit [DecidableEq α] in
@[simp] theorem embedP_val (ρ : α → Nat) (p : α × Nat) :
    (embedP ρ p).val = p.1 := rfl


structure Coupled (ρ1 ρ2 : α → Nat) (xs1 xs2 : List (α × Nat)) : Prop where
  len : xs1.length = xs2.length
  d1 : DistinctArr xs1
  d2 : DistinctArr xs2
  blocks : ∀ z, countIf (fun p => Linked (Dis ρ1 ρ2) p.1 z) xs1
              = countIf (fun p => Linked (Dis ρ1 ρ2) p.1 z) xs2
  singl : ∀ z, (∀ y, Linked (Dis ρ1 ρ2) y z → y = z) →
      valFilter xs1 z = valFilter xs2 z

theorem coupled_nil (ρ1 ρ2 : α → Nat) : Coupled ρ1 ρ2 [] [] where
  len := rfl
  d1 := List.Pairwise.nil
  d2 := List.Pairwise.nil
  blocks _ := rfl
  singl _ _ := rfl

theorem coupled_push {ρ1 ρ2 : α → Nat} {xs1 xs2 : List (α × Nat)}
    (C : Coupled ρ1 ρ2 xs1 xs2) {n : Nat}
    (b1 : AllBelow xs1 n) (b2 : AllBelow xs2 n) {x : α} {a : Nat} (hna : n < a) :
    Coupled ρ1 ρ2 (xs1 ++ [(x, a)]) (xs2 ++ [(x, a)]) where
  len := by rw [List.length_append, List.length_append, C.len]
  d1 := by
    refine List.pairwise_append.mpr ⟨C.d1, ?_, ?_⟩
    · exact List.Pairwise.cons (fun b hb => nomatch hb) List.Pairwise.nil
    · intro p hp q hq
      cases hq with
      | head => exact fun h => absurd (h ▸ b1 p hp) (by omega)
      | tail _ h' => cases h'
  d2 := by
    refine List.pairwise_append.mpr ⟨C.d2, ?_, ?_⟩
    · exact List.Pairwise.cons (fun b hb => nomatch hb) List.Pairwise.nil
    · intro p hp q hq
      cases hq with
      | head => exact fun h => absurd (h ▸ b2 p hp) (by omega)
      | tail _ h' => cases h'
  blocks z := by rw [countIf_append, countIf_append, C.blocks z]
  singl z hz := by rw [valFilter_append, valFilter_append, C.singl z hz]


theorem min_block {ρ1 ρ2 : α → Nat} {xs1 xs2 : List (α × Nat)}
    (C : Coupled ρ1 ρ2 xs1 xs2) {x1 x2 : α} {a1 a2 : Nat}
    (m1 : (x1, a1) ∈ xs1) (m2 : (x2, a2) ∈ xs2)
    (min1 : ∀ r ∈ xs1, r ≠ (x1, a1) → better (embedP ρ1 (x1, a1)) (embedP ρ1 r) = true)
    (min2 : ∀ r ∈ xs2, r ≠ (x2, a2) → better (embedP ρ2 (x2, a2)) (embedP ρ2 r) = true) :
    Linked (Dis ρ1 ρ2) x1 x2 := by
  refine Classical.byContradiction fun hL => ?_
  have hAg : Agree ρ1 ρ2 x1 x2 := not_linked_agree hL
  have hzx : ¬ Linked (Dis ρ1 ρ2) x2 x1 := fun hc => hL (Linked.symm hc)
  rcases Nat.lt_trichotomy (ρ1 x1) (ρ1 x2) with hlt | heq | hgt
  · 
    
    have h2lt : ρ2 x1 < ρ2 x2 := hAg.1.mp hlt
    have hpos : 0 < countIf (fun p => Linked (Dis ρ1 ρ2) p.1 x1) xs2 := by
      rw [← C.blocks x1]
      exact countIf_mem_pos m1 (Linked.refl x1)
    obtain ⟨⟨u, b⟩, hub, hLu⟩ := countIf_pos hpos
    have hne : (u, b) ≠ (x2, a2) := fun h => by
      rw [Prod.mk.injEq] at h
      rw [h.1] at hLu
      exact hL (Linked.symm hLu)
    have hm : ModEq ρ1 ρ2 x2 x1 u := linked_module (Linked.symm hLu) hzx
    have hu2 : ρ2 u < ρ2 x2 := by
      have hm1 := hm.2.1
      have hm2 := hm.2.2
      omega
    have := min2 (u, b) hub hne
    simp only [better_embedP] at this
    omega
  · 
    
    have h2eq : ρ2 x1 = ρ2 x2 := hAg.2.mp heq
    have htriv1 : ∀ y, Linked (Dis ρ1 ρ2) y x1 → y = x1 := fun y hy =>
      Classical.byContradiction fun hne =>
        (linked_strict (Linked.symm hy) (fun h => hne h.symm) hzx).1 heq.symm
    have htriv2 : ∀ y, Linked (Dis ρ1 ρ2) y x2 → y = x2 := fun y hy =>
      Classical.byContradiction fun hne =>
        (linked_strict (Linked.symm hy) (fun h => hne h.symm) hL).1 heq
    have hf1 := C.singl x1 htriv1
    have hf2 := C.singl x2 htriv2
    have hmem12 : (x2, a2) ∈ xs1 := by
      have h := mem_valFilter.mpr ⟨m2, rfl⟩
      rw [← hf2] at h
      exact (mem_valFilter.mp h).1
    have hmem21 : (x1, a1) ∈ xs2 := by
      have h := mem_valFilter.mpr ⟨m1, rfl⟩
      rw [hf1] at h
      exact (mem_valFilter.mp h).1
    by_cases hpair : (x2, a2) = (x1, a1)
    · refine hL ?_
      rw [Prod.mk.injEq] at hpair
      rw [hpair.1]
      exact Linked.refl x1
    · have hb1 := min1 (x2, a2) hmem12 hpair
      have hb2 := min2 (x1, a1) hmem21 (fun h => hpair h.symm)
      simp only [better_embedP] at hb1 hb2
      omega
  · 
    have h2gt : ρ2 x2 < ρ2 x1 := by
      have h1 := hAg.1
      have h2 := hAg.2
      omega
    have hpos : 0 < countIf (fun p => Linked (Dis ρ1 ρ2) p.1 x2) xs1 := by
      rw [C.blocks x2]
      exact countIf_mem_pos m2 (Linked.refl x2)
    obtain ⟨⟨u, b⟩, hub, hLu⟩ := countIf_pos hpos
    have hne : (u, b) ≠ (x1, a1) := fun h => by
      rw [Prod.mk.injEq] at h
      rw [h.1] at hLu
      exact hL hLu
    have hm : ModEq ρ1 ρ2 x1 x2 u := linked_module (Linked.symm hLu) hL
    have hu1 : ρ1 u < ρ1 x1 := by
      have hm1 := hm.1.1
      have hm2 := hm.1.2
      omega
    have := min1 (u, b) hub hne
    simp only [better_embedP] at this
    omega


theorem coupled_pop {ρ1 ρ2 : α → Nat} {xs1 xs2 : List (α × Nat)}
    (C : Coupled ρ1 ρ2 xs1 xs2) {x1 x2 : α} {a1 a2 : Nat}
    (m1 : (x1, a1) ∈ xs1) (m2 : (x2, a2) ∈ xs2)
    (min1 : ∀ r ∈ xs1, r ≠ (x1, a1) → better (embedP ρ1 (x1, a1)) (embedP ρ1 r) = true)
    (min2 : ∀ r ∈ xs2, r ≠ (x2, a2) → better (embedP ρ2 (x2, a2)) (embedP ρ2 r) = true)
    (hLx : Linked (Dis ρ1 ρ2) x1 x2) :
    Coupled ρ1 ρ2 (removeArr xs1 a1) (removeArr xs2 a2) where
  len := by
    have h1 := removeArr_length C.d1 m1
    have h2 := removeArr_length C.d2 m2
    have h := C.len
    omega
  d1 := distinctArr_removeArr C.d1
  d2 := distinctArr_removeArr C.d2
  blocks z := by
    have h1 := countIf_removeArr (P := fun p => Linked (Dis ρ1 ρ2) p.1 z) C.d1 m1
    have h2 := countIf_removeArr (P := fun p => Linked (Dis ρ1 ρ2) p.1 z) C.d2 m2
    have hiff : Linked (Dis ρ1 ρ2) x1 z ↔ Linked (Dis ρ1 ρ2) x2 z :=
      ⟨fun h => (Linked.symm hLx).trans h, fun h => hLx.trans h⟩
    have hb := C.blocks z
    by_cases hz : Linked (Dis ρ1 ρ2) x1 z
    · rw [if_pos hz] at h1
      rw [if_pos (hiff.mp hz)] at h2
      omega
    · rw [if_neg hz] at h1
      rw [if_neg (fun h => hz (hiff.mpr h))] at h2
      omega
  singl z hz := by
    by_cases hx1z : Linked (Dis ρ1 ρ2) x1 z
    · 
      
      have hx1 : x1 = z := hz x1 hx1z
      have hx2 : x2 = z := hz x2 ((Linked.symm hLx).trans hx1z)
      have hf := C.singl z hz
      have m12 : (x1, a1) ∈ xs2 := by
        have h := mem_valFilter.mpr ⟨m1, hx1⟩
        rw [hf] at h
        exact (mem_valFilter.mp h).1
      have m21 : (x2, a2) ∈ xs1 := by
        have h := mem_valFilter.mpr ⟨m2, hx2⟩
        rw [← hf] at h
        exact (mem_valFilter.mp h).1
      have ha : a1 = a2 := by
        by_cases h : a1 = a2
        · exact h
        · have hb1 := min1 (x2, a2) m21 (fun hc => h (congrArg Prod.snd hc).symm)
          have hb2 := min2 (x1, a1) m12 (fun hc => h (congrArg Prod.snd hc))
          simp only [better_embedP] at hb1 hb2
          rw [hx1, hx2] at hb1 hb2
          omega
      rw [valFilter_removeArr_self, valFilter_removeArr_self, hf, ha]
    · 
      have hx2z : ¬ Linked (Dis ρ1 ρ2) x2 z := fun h => hx1z (hLx.trans h)
      have hne1 : x1 ≠ z := fun h => hx1z (h ▸ Linked.refl x1)
      have hne2 : x2 ≠ z := fun h => hx2z (h ▸ Linked.refl x2)

      rw [valFilter_removeArr_other C.d1 m1 hne1,
          valFilter_removeArr_other C.d2 m2 hne2]
      exact C.singl z hz

omit [DecidableEq α] in
theorem qstate_push (ρ : α → Nat) (xs : List (α × Nat)) (x : α) (a : Nat) :
    qstate ρ xs ++ [(⟨x, ρ x, a⟩ : Entry α)] = qstate ρ (xs ++ [(x, a)]) := by
  show _ = List.map _ _
  rw [List.map_append]
  rfl


theorem colored_congruence {ρ1 ρ2 : α → Nat} {col : α → γ}
    (H : ∀ x y, col x ≠ col y → Agree ρ1 ρ2 x y) :
    ∀ (ops : List (QOp α)) (n : Nat) (xs1 xs2 : List (α × Nat)),
      Coupled ρ1 ρ2 xs1 xs2 → AllBelow xs1 n → AllBelow xs2 n → OkOps n ops →
      (qrun ρ1 (qstate ρ1 xs1) ops).map (Option.map col)
        = (qrun ρ2 (qstate ρ2 xs2) ops).map (Option.map col) := by
  intro ops
  induction ops with
  | nil => intros; rfl
  | cons op rest ih =>
    intro n xs1 xs2 C b1 b2 hok
    cases op with
    | push x a =>
      obtain ⟨hna, hok'⟩ := hok
      show (qrun ρ1 (qstate ρ1 xs1 ++ [⟨x, ρ1 x, a⟩]) rest).map (Option.map col)
         = (qrun ρ2 (qstate ρ2 xs2 ++ [⟨x, ρ2 x, a⟩]) rest).map (Option.map col)
      rw [qstate_push, qstate_push]
      exact ih a (xs1 ++ [(x, a)]) (xs2 ++ [(x, a)])
        (coupled_push C b1 b2 hna) (allBelow_push b1 hna) (allBelow_push b2 hna) hok'
    | pop =>
      cases xs1 with
      | nil =>
        cases xs2 with
        | cons q qs =>
          have h := C.len
          simp at h
        | nil =>
          show (none :: qrun ρ1 (qstate ρ1 []) rest).map (Option.map col)
             = (none :: qrun ρ2 (qstate ρ2 []) rest).map (Option.map col)
          rw [List.map_cons, List.map_cons, ih n [] [] C b1 b2 hok]
      | cons p ps =>
        cases xs2 with
        | nil =>
          have h := C.len
          simp at h
        | cons q qs =>
          obtain ⟨e1, he1mem, he1pop, he1min⟩ := qpop_state ρ1 p ps C.d1
          obtain ⟨e2, he2mem, he2pop, he2min⟩ := qpop_state ρ2 q qs C.d2
          obtain ⟨y1, c1⟩ := e1
          obtain ⟨y2, c2⟩ := e2
          have hLx : Linked (Dis ρ1 ρ2) y1 y2 :=
            min_block C he1mem he2mem he1min he2min
          simp only [qrun, he1pop, he2pop, List.map_cons, Option.map_some, embedP_val]
          rw [linked_colEq H hLx,
              ih n (removeArr (p :: ps) c1) (removeArr (q :: qs) c2)
                (coupled_pop C he1mem he2mem he1min he2min hLx)
                (allBelow_removeArr b1) (allBelow_removeArr b2) hok]

end PifoGeneral

namespace PifoFull

open PifoStatement





theorem qpop_eq_none_iff {α : Type} (q : Queue α) :
    qpop q = none ↔ q = [] := by
  induction q with
  | nil => simp [qpop]
  | cons e es ih =>
    cases ht : qpop es with
    | none => simp [qpop, ht]
    | some p =>
      obtain ⟨best, rest⟩ := p
      by_cases hb : better e best = true <;> simp [qpop, ht, hb]

theorem qpop_nonempty {α : Type} (e : Entry α) (es : Queue α) :
    ∃ best rest, qpop (e :: es) = some (best, rest) := by
  cases h : qpop (e :: es) with
  | none =>
      have := (qpop_eq_none_iff (e :: es)).mp h
      simp at this
  | some p =>
      obtain ⟨best, rest⟩ := p
      exact ⟨best, rest, rfl⟩

def valCount {α : Type} [DecidableEq α] (x : α) : Queue α → Nat
  | [] => 0
  | e :: es => (if e.val = x then 1 else 0) + valCount x es

theorem qpop_valCount {α : Type} [DecidableEq α]
    {q : Queue α} {best : Entry α} {rest : Queue α}
    (h : qpop q = some (best, rest)) (x : α) :
    valCount x q = valCount x rest + (if best.val = x then 1 else 0) := by
  induction q generalizing best rest with
  | nil => simp [qpop] at h
  | cons e es ih =>
    cases ht : qpop es with
    | none =>
      have hes : es = [] := (qpop_eq_none_iff es).mp ht
      subst es
      simp [qpop] at h
      obtain ⟨rfl, rfl⟩ := h
      simp [valCount]
    | some p =>
      obtain ⟨tailBest, tailRest⟩ := p
      by_cases hb : better e tailBest = true
      · simp only [qpop, ht, if_pos hb, Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        simp only [valCount]
        omega
      · simp only [qpop, ht, if_neg hb, Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        have hit := ih ht
        simp only [valCount]
        omega

theorem qpop_length {α : Type} {q : Queue α} {best : Entry α} {rest : Queue α}
    (h : qpop q = some (best, rest)) : rest.length + 1 = q.length := by
  induction q generalizing best rest with
  | nil => simp [qpop] at h
  | cons e es ih =>
    cases ht : qpop es with
    | none =>
      have hes : es = [] := (qpop_eq_none_iff es).mp ht
      subst es
      simp only [qpop, Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      simp
    | some p =>
      obtain ⟨tailBest, tailRest⟩ := p
      by_cases hb : better e tailBest = true
      · simp only [qpop, ht, if_pos hb, Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        simp
      · simp only [qpop, ht, if_neg hb, Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        have hit := ih ht
        simp only [List.length_cons]
        omega



theorem treePop_emptyTree {α : Type} (topo : Topology) :
    treePop (emptyTree (α := α) topo) = none := by
  cases topo <;> rfl

theorem interEquiv_fin_zero (S₁ S₂ : Scheduler 0) : interEquiv S₁ S₂ := by
  intro ops
  unfold run
  change runFrom S₁ 0 (emptyTree S₁.topo) ops =
    runFrom S₂ 0 (emptyTree S₂.topo) ops
  induction ops with
  | nil => rfl
  | cons op ops ih =>
    cases op with
    | push i => exact Fin.elim0 i
    | pop =>
      simp only [runFrom, treePop_emptyTree]
      congr



mutual
  def packetCount {α : Type} : Tree α → Nat
    | .leaf q => q.length
    | .node _ children => forestCount children

  def forestCount {α : Type} : List (Tree α) → Nat
    | [] => 0
    | t :: ts => packetCount t + forestCount ts

  def packetCountAt {α : Type} : List (Tree α) → Nat → Nat
    | [], _ => 0
    | t :: _, 0 => packetCount t
    | _ :: ts, i + 1 => packetCountAt ts i
end

mutual
  def packetValCount {α : Type} [DecidableEq α] (x : α) : Tree α → Nat
    | .leaf q => valCount x q
    | .node _ children => forestValCount x children

  def forestValCount {α : Type} [DecidableEq α] (x : α) : List (Tree α) → Nat
    | [] => 0
    | tree :: trees => packetValCount x tree + forestValCount x trees
end

mutual
  theorem packetValCount_emptyTree {α : Type} [DecidableEq α]
      (x : α) (topology : Topology) :
      packetValCount x (emptyTree (α := α) topology) = 0 := by
    cases topology with
    | leaf => rfl
    | node topologies => exact forestValCount_emptyForest x topologies

  theorem forestValCount_emptyForest {α : Type} [DecidableEq α]
      (x : α) (topologies : List Topology) :
      forestValCount x (emptyForest (α := α) topologies) = 0 := by
    cases topologies with
    | nil => rfl
    | cons topology topologies =>
      simp only [emptyForest, forestValCount, packetValCount_emptyTree,
        forestValCount_emptyForest, Nat.zero_add]
end

theorem queue_eq_nil_of_valCount_eq_zero (q : Queue Nat)
    (h : ∀ i, valCount i q = 0) : q = [] := by
  cases q with
  | nil => rfl
  | cons e es =>
    have hz := h e.val
    simp [valCount] at hz

theorem packetCountAt_le_forestCount {α : Type} (trees : List (Tree α)) (i : Nat) :
    packetCountAt trees i ≤ forestCount trees := by
  induction trees generalizing i with
  | nil => simp [packetCountAt, forestCount]
  | cons tree trees ih =>
    cases i with
    | zero => simp [packetCountAt, forestCount]
    | succ i =>
      simp only [packetCountAt, forestCount]
      have := ih i
      omega

theorem forestCount_pos_has_index {α : Type} (trees : List (Tree α))
    (h : 0 < forestCount trees) : ∃ i, 0 < packetCountAt trees i := by
  induction trees with
  | nil => simp [forestCount] at h
  | cons tree trees ih =>
    by_cases ht : 0 < packetCount tree
    · exact ⟨0, ht⟩
    · have hf : 0 < forestCount trees := by
        simp only [forestCount] at h
        omega
      obtain ⟨i, hi⟩ := ih hf
      exact ⟨i + 1, hi⟩

theorem qpop_exists_of_forall_count {q : Queue Nat} {counts : Nat → Nat}
    (hcount : ∀ i, valCount i q = counts i) (hpos : ∃ i, 0 < counts i) :
    ∃ best rest, qpop q = some (best, rest) := by
  have hq : q ≠ [] := by
    intro he
    subst q
    obtain ⟨i, hi⟩ := hpos
    have := hcount i
    simp [valCount] at this
    omega
  cases q with
  | nil => contradiction
  | cons e es => exact qpop_nonempty e es

mutual
  def Good {α : Type} : Topology → Tree α → Prop
    | .leaf, .leaf _ => True
    | .node topologies, .node q children =>
      GoodForest topologies children ∧
        ∀ i, valCount i q = packetCountAt children i
    | _, _ => False

  def GoodForest {α : Type} : List Topology → List (Tree α) → Prop
    | [], [] => True
    | topology :: topologies, tree :: trees =>
      Good topology tree ∧ GoodForest topologies trees
    | _, _ => False
end

theorem valCount_append {α : Type} [DecidableEq α]
    (x : α) (q₁ q₂ : Queue α) :
    valCount x (q₁ ++ q₂) = valCount x q₁ + valCount x q₂ := by
  induction q₁ with
  | nil => simp [valCount]
  | cons e es ih =>
    simp only [List.cons_append, valCount, ih]
    omega

theorem valCount_singleton {α : Type} [DecidableEq α]
    (x : α) (e : Entry α) :
    valCount x [e] = if e.val = x then 1 else 0 := by
  simp [valCount]

mutual
  theorem packetCount_emptyTree {α : Type} (topology : Topology) :
      packetCount (emptyTree (α := α) topology) = 0 := by
    cases topology with
    | leaf => rfl
    | node topologies => exact forestCount_emptyForest topologies

  theorem forestCount_emptyForest {α : Type} (topologies : List Topology) :
      forestCount (emptyForest (α := α) topologies) = 0 := by
    cases topologies with
    | nil => rfl
    | cons topology topologies =>
      simp only [emptyForest, forestCount, packetCount_emptyTree topology,
        forestCount_emptyForest topologies, Nat.zero_add]
end

theorem packetCountAt_emptyForest {α : Type} (topologies : List Topology) (i : Nat) :
    packetCountAt (emptyForest (α := α) topologies) i = 0 := by
  induction topologies generalizing i with
  | nil => rfl
  | cons topology topologies ih =>
    cases i with
    | zero => exact packetCount_emptyTree topology
    | succ i => exact ih i

mutual
  theorem emptyTree_good {α : Type} (topology : Topology) :
      Good topology (emptyTree (α := α) topology) := by
    cases topology with
    | leaf => trivial
    | node topologies =>
      exact ⟨emptyForest_good topologies, fun i => by
        simp only [valCount]
        exact (packetCountAt_emptyForest topologies i).symm⟩

  theorem emptyForest_good {α : Type} (topologies : List Topology) :
      GoodForest topologies (emptyForest (α := α) topologies) := by
    cases topologies with
    | nil => trivial
    | cons topology topologies =>
      exact ⟨emptyTree_good topology, emptyForest_good topologies⟩
end

mutual
  theorem treePush_good_count {α : Type} (pkt : α) (arr : Nat)
      (topology : Topology) (tree : Tree α) (path : Path)
      (hg : Good topology tree) (hp : pathOk topology path = true) :
      Good topology (treePush pkt arr tree path) ∧
        packetCount (treePush pkt arr tree path) = packetCount tree + 1 := by
    cases topology with
    | leaf =>
      cases tree with
      | node q children => simp [Good] at hg
      | leaf q =>
        cases path with
        | node c r rest => simp [pathOk] at hp
        | leaf r =>
          constructor
          · trivial
          · simp [treePush, packetCount]
    | node topologies =>
      cases tree with
      | leaf q => simp [Good] at hg
      | node q children =>
        cases path with
        | leaf r => simp [pathOk] at hp
        | node child rank rest =>
          obtain ⟨hgForest, hgCount⟩ := hg
          obtain ⟨hgForest', hForestCount, hAt⟩ :=
            treePushAt_good_count pkt arr topologies children child rest hgForest hp
          constructor
          · constructor
            · exact hgForest'
            · intro i
              rw [valCount_append, valCount_singleton, hgCount, hAt]
          · exact hForestCount

  theorem treePushAt_good_count {α : Type} (pkt : α) (arr : Nat)
      (topologies : List Topology) (trees : List (Tree α)) (child : Nat) (path : Path)
      (hg : GoodForest topologies trees) (hp : pathOkAt topologies child path = true) :
      GoodForest topologies (treePushAt pkt arr trees child path) ∧
        forestCount (treePushAt pkt arr trees child path) = forestCount trees + 1 ∧
        ∀ i, packetCountAt (treePushAt pkt arr trees child path) i =
          packetCountAt trees i + (if child = i then 1 else 0) := by
    cases topologies with
    | nil => simp [pathOkAt] at hp
    | cons topology topologies =>
      cases trees with
      | nil => simp [GoodForest] at hg
      | cons tree trees =>
        obtain ⟨hgTree, hgTrees⟩ := hg
        cases child with
        | zero =>
          have pushed := treePush_good_count pkt arr topology tree path hgTree hp
          constructor
          · exact ⟨pushed.1, hgTrees⟩
          · constructor
            · simp only [treePushAt, forestCount]
              omega
            · intro i
              cases i with
              | zero =>
                simpa [treePushAt, packetCountAt] using pushed.2
              | succ i =>
                simp [treePushAt, packetCountAt]
        | succ child =>
          obtain ⟨hgTrees', hForestCount, hAt⟩ :=
            treePushAt_good_count pkt arr topologies trees child path hgTrees hp
          constructor
          · exact ⟨hgTree, hgTrees'⟩
          · constructor
            · simp only [treePushAt, forestCount]
              omega
            · intro i
              cases i with
              | zero => simp [treePushAt, packetCountAt]
              | succ i =>
                simp only [treePushAt, packetCountAt, Nat.add_left_inj]
                exact hAt i
end

mutual
  theorem treePush_packetValCount {α : Type} [DecidableEq α]
      (pkt x : α) (arr : Nat) (topology : Topology) (tree : Tree α) (path : Path)
      (hg : Good topology tree) (hp : pathOk topology path = true) :
      packetValCount x (treePush pkt arr tree path) =
        packetValCount x tree + (if pkt = x then 1 else 0) := by
    cases topology with
    | leaf =>
      cases tree with
      | node q children => simp [Good] at hg
      | leaf q =>
        cases path with
        | node child rank rest => simp [pathOk] at hp
        | leaf rank =>
          simp [treePush, packetValCount, valCount_append, valCount_singleton]
    | node topologies =>
      cases tree with
      | leaf q => simp [Good] at hg
      | node q children =>
        cases path with
        | leaf rank => simp [pathOk] at hp
        | node child rank rest =>
          exact treePushAt_packetValCount pkt x arr topologies children child rest
            hg.1 hp

  theorem treePushAt_packetValCount {α : Type} [DecidableEq α]
      (pkt x : α) (arr : Nat) (topologies : List Topology)
      (trees : List (Tree α)) (child : Nat) (path : Path)
      (hg : GoodForest topologies trees)
      (hp : pathOkAt topologies child path = true) :
      forestValCount x (treePushAt pkt arr trees child path) =
        forestValCount x trees + (if pkt = x then 1 else 0) := by
    cases topologies with
    | nil => simp [pathOkAt] at hp
    | cons topology topologies =>
      cases trees with
      | nil => simp [GoodForest] at hg
      | cons tree trees =>
        obtain ⟨hgTree, hgTrees⟩ := hg
        cases child with
        | zero =>
          have hc := treePush_packetValCount pkt x arr topology tree path hgTree hp
          simp only [treePushAt, forestValCount]
          omega
        | succ child =>
          have hc := treePushAt_packetValCount pkt x arr topologies trees child path
            hgTrees hp
          simp only [treePushAt, forestValCount]
          omega
end

mutual
  theorem treePop_good_pos {α : Type} (topology : Topology) (tree : Tree α)
      (hg : Good topology tree) (hpos : 0 < packetCount tree) :
      ∃ pkt tree', treePop tree = some (pkt, tree') ∧ Good topology tree' ∧
        packetCount tree = packetCount tree' + 1 := by
    cases topology with
    | leaf =>
      cases tree with
      | node q children => simp [Good] at hg
      | leaf q =>
        have hq : q ≠ [] := by
          intro he
          subst q
          simp [packetCount] at hpos
        cases q with
        | nil => contradiction
        | cons e es =>
          obtain ⟨best, rest, hp⟩ := qpop_nonempty e es
          exact ⟨best.val, .leaf rest, by simp [treePop, hp], trivial, by
            simp only [packetCount]
            exact (qpop_length hp).symm⟩
    | node topologies =>
      cases tree with
      | leaf q => simp [Good] at hg
      | node q children =>
        obtain ⟨hgForest, hgCount⟩ := hg
        have hasIndex := forestCount_pos_has_index children hpos
        obtain ⟨best, rest, hq⟩ :=
          qpop_exists_of_forall_count hgCount hasIndex
        have hchild : 0 < packetCountAt children best.val := by
          have hc := qpop_valCount hq best.val
          rw [hgCount] at hc
          split at hc <;> omega
        obtain ⟨pkt, children', hp, hgForest', hForestCount, hAt⟩ :=
          treePopAt_good_pos topologies children best.val hgForest hchild
        refine ⟨pkt, .node rest children', ?_, ?_, ?_⟩
        · simp [treePop, hq, hp]
        · constructor
          · exact hgForest'
          · intro i
            have hc := qpop_valCount hq i
            rw [hgCount, hAt i] at hc
            omega
        · exact hForestCount

  theorem treePopAt_good_pos {α : Type} (topologies : List Topology)
      (trees : List (Tree α)) (child : Nat)
      (hg : GoodForest topologies trees) (hpos : 0 < packetCountAt trees child) :
      ∃ pkt trees', treePopAt trees child = some (pkt, trees') ∧
        GoodForest topologies trees' ∧
        forestCount trees = forestCount trees' + 1 ∧
        ∀ i, packetCountAt trees i = packetCountAt trees' i +
          (if child = i then 1 else 0) := by
    cases topologies with
    | nil =>
      cases trees <;> simp [GoodForest, packetCountAt] at hg hpos
    | cons topology topologies =>
      cases trees with
      | nil => simp [GoodForest] at hg
      | cons tree trees =>
        obtain ⟨hgTree, hgTrees⟩ := hg
        cases child with
        | zero =>
          obtain ⟨pkt, tree', hp, hgTree', hTreeCount⟩ :=
            treePop_good_pos topology tree hgTree hpos
          refine ⟨pkt, tree' :: trees, ?_, ⟨hgTree', hgTrees⟩, ?_, ?_⟩
          · simp [treePopAt, hp]
          · simp only [forestCount]
            omega
          · intro i
            cases i with
            | zero => simpa [packetCountAt] using hTreeCount
            | succ i => simp [packetCountAt]
        | succ child =>
          obtain ⟨pkt, trees', hp, hgTrees', hForestCount, hAt⟩ :=
            treePopAt_good_pos topologies trees child hgTrees hpos
          refine ⟨pkt, tree :: trees', ?_, ⟨hgTree, hgTrees'⟩, ?_, ?_⟩
          · simp [treePopAt, hp]
          · simp only [forestCount]
            omega
          · intro i
            cases i with
            | zero => simp [packetCountAt]
            | succ i => simpa [packetCountAt] using hAt i
end

theorem treePop_good_zero {α : Type} (topology : Topology) (tree : Tree α)
    (hg : Good topology tree) (hz : packetCount tree = 0) : treePop tree = none := by
  cases topology with
  | leaf =>
    cases tree with
    | node q children => simp [Good] at hg
    | leaf q =>
      have hlen : q.length = 0 := hz
      have hq : q = [] := List.length_eq_zero_iff.mp hlen
      subst q
      rfl
  | node topologies =>
    cases tree with
    | leaf q => simp [Good] at hg
    | node q children =>
      obtain ⟨hgForest, hgCount⟩ := hg
      have hq : q = [] := queue_eq_nil_of_valCount_eq_zero q (by
        intro i
        rw [hgCount]
        have hle := packetCountAt_le_forestCount children i
        simp only [packetCount] at hz
        omega)
      subst q
      rfl

mutual
  theorem treePop_packetValCount {α : Type} [DecidableEq α]
      {tree : Tree α} {pkt : α} {tree' : Tree α}
      (hpop : treePop tree = some (pkt, tree')) (x : α) :
      packetValCount x tree = packetValCount x tree' + (if pkt = x then 1 else 0) := by
    cases tree with
    | leaf q =>
      cases hq : qpop q with
      | none => simp [treePop, hq] at hpop
      | some result =>
        obtain ⟨e, rest⟩ := result
        simp only [treePop, hq, Option.some.injEq, Prod.mk.injEq] at hpop
        obtain ⟨rfl, rfl⟩ := hpop
        exact qpop_valCount hq x
    | node q children =>
      cases hq : qpop q with
      | none => simp [treePop, hq] at hpop
      | some result =>
        obtain ⟨e, rest⟩ := result
        cases hc : treePopAt children e.val with
        | none => simp [treePop, hq, hc] at hpop
        | some result =>
          obtain ⟨childPkt, children'⟩ := result
          simp only [treePop, hq, hc, Option.some.injEq, Prod.mk.injEq] at hpop
          obtain ⟨rfl, rfl⟩ := hpop
          exact treePopAt_packetValCount hc x

  theorem treePopAt_packetValCount {α : Type} [DecidableEq α]
      {trees : List (Tree α)} {child : Nat} {pkt : α} {trees' : List (Tree α)}
      (hpop : treePopAt trees child = some (pkt, trees')) (x : α) :
      forestValCount x trees =
        forestValCount x trees' + (if pkt = x then 1 else 0) := by
    cases trees with
    | nil => simp [treePopAt] at hpop
    | cons tree trees =>
      cases child with
      | zero =>
        cases ht : treePop tree with
        | none => simp [treePopAt, ht] at hpop
        | some result =>
          obtain ⟨treePkt, tree'⟩ := result
          simp only [treePopAt, ht, Option.some.injEq, Prod.mk.injEq] at hpop
          obtain ⟨rfl, rfl⟩ := hpop
          have hc := treePop_packetValCount ht x
          simp only [forestValCount]
          omega
      | succ child =>
        cases ht : treePopAt trees child with
        | none => simp [treePopAt, ht] at hpop
        | some result =>
          obtain ⟨treePkt, trees'⟩ := result
          simp only [treePopAt, ht, Option.some.injEq, Prod.mk.injEq] at hpop
          obtain ⟨rfl, rfl⟩ := hpop
          have hc := treePopAt_packetValCount ht x
          simp only [forestValCount]
          omega
end



theorem runFrom_fin_one_congr (S₁ S₂ : Scheduler 1)
    (hv₁ : S₁.Valid) (hv₂ : S₂.Valid) :
    ∀ (ops : List (Op 1)) (cnt : Nat) (tree₁ tree₂ : Tree (Fin 1)),
      Good S₁.topo tree₁ → Good S₂.topo tree₂ →
      packetCount tree₁ = packetCount tree₂ →
      runFrom S₁ cnt tree₁ ops = runFrom S₂ cnt tree₂ ops := by
  intro ops
  induction ops with
  | nil => intros; rfl
  | cons op ops ih =>
    intro cnt tree₁ tree₂ hg₁ hg₂ hcount
    cases op with
    | push i =>
      simp only [runFrom]
      have pushed₁ := treePush_good_count i (cnt + 1) S₁.topo tree₁ (S₁.assign i)
        hg₁ (hv₁ i)
      have pushed₂ := treePush_good_count i (cnt + 1) S₂.topo tree₂ (S₂.assign i)
        hg₂ (hv₂ i)
      apply ih (cnt + 1) _ _ pushed₁.1 pushed₂.1
      omega
    | pop =>
      by_cases hz : packetCount tree₁ = 0
      · have hz₂ : packetCount tree₂ = 0 := by omega
        have hp₁ := treePop_good_zero S₁.topo tree₁ hg₁ hz
        have hp₂ := treePop_good_zero S₂.topo tree₂ hg₂ hz₂
        simp only [runFrom, hp₁, hp₂, List.cons.injEq, true_and]
        exact ih cnt tree₁ tree₂ hg₁ hg₂ hcount
      · have hpos₁ : 0 < packetCount tree₁ := by omega
        have hpos₂ : 0 < packetCount tree₂ := by omega
        obtain ⟨pkt₁, tree₁', hp₁, hg₁', hc₁⟩ :=
          treePop_good_pos S₁.topo tree₁ hg₁ hpos₁
        obtain ⟨pkt₂, tree₂', hp₂, hg₂', hc₂⟩ :=
          treePop_good_pos S₂.topo tree₂ hg₂ hpos₂
        have hpkt : pkt₁ = pkt₂ := Subsingleton.elim _ _
        simp only [runFrom, hp₁, hp₂, List.cons.injEq, Option.some.injEq]
        exact ⟨hpkt, ih cnt tree₁' tree₂' hg₁' hg₂' (by omega)⟩

theorem interEquiv_fin_one (S₁ S₂ : Scheduler 1)
    (hv₁ : S₁.Valid) (hv₂ : S₂.Valid) : interEquiv S₁ S₂ := by
  intro ops
  unfold run
  apply runFrom_fin_one_congr S₁ S₂ hv₁ hv₂ ops 0
  · exact emptyTree_good S₁.topo
  · exact emptyTree_good S₂.topo
  · rw [packetCount_emptyTree, packetCount_emptyTree]



def reArr {α : Type} (f : Nat → Nat) (e : Entry α) : Entry α :=
  ⟨e.val, e.rank, f e.arr⟩

theorem better_reArr {α : Type} (f : Nat → Nat)
    (hf : ∀ a b, (a < b) ↔ (f a < f b)) (x y : Entry α) :
    better (reArr f x) (reArr f y) = better x y := by
  cases x with
  | mk xv xr xa =>
    cases y with
    | mk yv yr ya =>
      have harr : decide (f xa < f ya) = decide (xa < ya) :=
        decide_eq_decide.mpr (hf xa ya).symm
      change (decide (xr < yr) || (xr == yr && decide (f xa < f ya))) =
        (decide (xr < yr) || (xr == yr && decide (xa < ya)))
      exact congrArg (fun b => decide (xr < yr) || (xr == yr && b)) harr

theorem qpop_cons_none_generic {α : Type} (e : Entry α) (es : Queue α)
    (hn : qpop es = none) : qpop (e :: es) = some (e, []) := by
  simp [qpop, hn]

theorem qpop_cons_some_generic {α : Type} (e best : Entry α) (es rest : Queue α)
    (hp : qpop es = some (best, rest)) :
    qpop (e :: es) = if better e best then some (e, es) else some (best, e :: rest) := by
  simp [qpop, hp]

theorem qpop_reArr {α : Type} (f : Nat → Nat)
    (hf : ∀ a b, (a < b) ↔ (f a < f b)) (q : Queue α) :
    (qpop q = none ∧ qpop (q.map (reArr f)) = none) ∨
      ∃ e rest, qpop q = some (e, rest) ∧
        qpop (q.map (reArr f)) = some (reArr f e, rest.map (reArr f)) := by
  induction q with
  | nil => left; exact ⟨rfl, rfl⟩
  | cons e es ih =>
    rcases ih with ⟨hn, hn'⟩ | ⟨best, rest, hp, hp'⟩
    · right
      refine ⟨e, [], ?_, ?_⟩
      · exact qpop_cons_none_generic e es hn
      · simpa using qpop_cons_none_generic (reArr f e) (es.map (reArr f)) hn'
    · have hb := better_reArr f hf e best
      by_cases hbetter : better e best = true
      · right
        refine ⟨e, es, ?_, ?_⟩
        · rw [qpop_cons_some_generic e best es rest hp, if_pos hbetter]
        · simp only [List.map_cons]
          rw [qpop_cons_some_generic (reArr f e) (reArr f best) (es.map (reArr f))
              (rest.map (reArr f)) hp',
            if_pos (by rw [hb]; exact hbetter)]
      · right
        refine ⟨best, e :: rest, ?_, ?_⟩
        · rw [qpop_cons_some_generic e best es rest hp, if_neg hbetter]
        · simp only [List.map_cons]
          rw [qpop_cons_some_generic (reArr f e) (reArr f best) (es.map (reArr f))
              (rest.map (reArr f)) hp',
            if_neg (by rw [hb]; exact hbetter)]



mutual
  def reArrTree {α : Type} (f : Nat → Nat) : Tree α → Tree α
    | .leaf q => .leaf (q.map (reArr f))
    | .node q children => .node (q.map (reArr f)) (reArrForest f children)

  def reArrForest {α : Type} (f : Nat → Nat) : List (Tree α) → List (Tree α)
    | [] => []
    | tree :: trees => reArrTree f tree :: reArrForest f trees
end

mutual
  theorem reArrTree_treePush {α : Type} (f : Nat → Nat) (pkt : α) (arr : Nat)
      (tree : Tree α) (path : Path) :
      reArrTree f (treePush pkt arr tree path) =
        treePush pkt (f arr) (reArrTree f tree) path := by
    cases tree with
    | leaf q =>
      cases path <;> simp [treePush, reArrTree, reArr, List.map_append]
    | node q children =>
      cases path with
      | leaf rank => rfl
      | node child rank rest =>
        simp only [treePush, reArrTree, List.map_append, List.map_singleton]
        rw [reArrForest_treePushAt]
        rfl

  theorem reArrForest_treePushAt {α : Type} (f : Nat → Nat) (pkt : α)
      (arr : Nat) (trees : List (Tree α)) (child : Nat) (path : Path) :
      reArrForest f (treePushAt pkt arr trees child path) =
        treePushAt pkt (f arr) (reArrForest f trees) child path := by
    cases trees with
    | nil => rfl
    | cons tree trees =>
      cases child with
      | zero => simp [treePushAt, reArrForest, reArrTree_treePush]
      | succ child => simp [treePushAt, reArrForest, reArrForest_treePushAt]
end

mutual
  theorem treePop_reArr {α : Type} (f : Nat → Nat)
      (hf : ∀ a b, (a < b) ↔ (f a < f b)) (tree : Tree α) :
      (treePop tree = none ∧ treePop (reArrTree f tree) = none) ∨
        ∃ pkt tree', treePop tree = some (pkt, tree') ∧
          treePop (reArrTree f tree) = some (pkt, reArrTree f tree') := by
    cases tree with
    | leaf q =>
      rcases qpop_reArr f hf q with ⟨hn, hn'⟩ | ⟨e, rest, hp, hp'⟩
      · left
        simp [treePop, reArrTree, hn, hn']
      · right
        exact ⟨e.val, .leaf rest, by simp [treePop, reArrTree, reArr, hp, hp']⟩
    | node q children =>
      rcases qpop_reArr f hf q with ⟨hn, hn'⟩ | ⟨e, rest, hp, hp'⟩
      · left
        simp [treePop, reArrTree, hn, hn']
      · rcases treePopAt_reArr f hf children e.val with
          ⟨hc, hc'⟩ | ⟨pkt, children', hc, hc'⟩
        · left
          simp [treePop, reArrTree, hp, hp', hc, hc', reArr]
        · right
          exact ⟨pkt, .node rest children', by
            simp [treePop, reArrTree, hp, hp', hc, hc', reArr]⟩

  theorem treePopAt_reArr {α : Type} (f : Nat → Nat)
      (hf : ∀ a b, (a < b) ↔ (f a < f b)) (trees : List (Tree α)) (child : Nat) :
      (treePopAt trees child = none ∧
          treePopAt (reArrForest f trees) child = none) ∨
        ∃ pkt trees', treePopAt trees child = some (pkt, trees') ∧
          treePopAt (reArrForest f trees) child =
            some (pkt, reArrForest f trees') := by
    cases trees with
    | nil => left; exact ⟨rfl, rfl⟩
    | cons tree trees =>
      cases child with
      | zero =>
        rcases treePop_reArr f hf tree with
          ⟨ht, ht'⟩ | ⟨pkt, tree', ht, ht'⟩
        · left
          simp [treePopAt, reArrForest, ht, ht']
        · right
          exact ⟨pkt, tree' :: trees, by
            simp [treePopAt, reArrForest, ht, ht']⟩
      | succ child =>
        rcases treePopAt_reArr f hf trees child with
          ⟨ht, ht'⟩ | ⟨pkt, trees', ht, ht'⟩
        · left
          simp [treePopAt, reArrForest, ht, ht']
        · right
          exact ⟨pkt, tree :: trees', by
            simp [treePopAt, reArrForest, ht, ht']⟩
end

inductive TimedOp (α : Type) where
  | push (pkt : α) (arr : Nat)
  | pop

def mapTimedArr {α : Type} (f : Nat → Nat) : List (TimedOp α) → List (TimedOp α)
  | [] => []
  | .push pkt arr :: ops => .push pkt (f arr) :: mapTimedArr f ops
  | .pop :: ops => .pop :: mapTimedArr f ops

def runTimedFrom {α : Type} (assign : α → Path) :
    Tree α → List (TimedOp α) → List (Option α)
  | _, [] => []
  | tree, .push pkt arr :: ops =>
      runTimedFrom assign (treePush pkt arr tree (assign pkt)) ops
  | tree, .pop :: ops =>
      match treePop tree with
      | none => none :: runTimedFrom assign tree ops
      | some (pkt, tree') => some pkt :: runTimedFrom assign tree' ops

def timedOpsFrom {k : Nat} : Nat → List (Op k) → List (TimedOp (Fin k))
  | _, [] => []
  | cnt, .push pkt :: ops =>
      .push pkt (cnt + 1) :: timedOpsFrom (cnt + 1) ops
  | cnt, .pop :: ops => .pop :: timedOpsFrom cnt ops

theorem runFrom_eq_runTimedFrom {k : Nat} (S : Scheduler k) :
    ∀ (ops : List (Op k)) (cnt : Nat) (tree : Tree (Fin k)),
      runFrom S cnt tree ops = runTimedFrom S.assign tree (timedOpsFrom cnt ops) := by
  intro ops
  induction ops with
  | nil => intros; rfl
  | cons op ops ih =>
    intro cnt tree
    cases op with
    | push pkt =>
      simp only [runFrom, timedOpsFrom, runTimedFrom]
      exact ih (cnt + 1) (treePush pkt (cnt + 1) tree (S.assign pkt))
    | pop =>
      simp only [runFrom, timedOpsFrom, runTimedFrom]
      cases hp : treePop tree with
      | none =>
        simp only
        exact congrArg (fun xs => none :: xs) (ih cnt tree)
      | some result =>
        obtain ⟨pkt, tree'⟩ := result
        simp only
        exact congrArg (fun xs => some pkt :: xs) (ih cnt tree')

theorem runTimedFrom_reArr {α : Type} (assign : α → Path) (f : Nat → Nat)
    (hf : ∀ a b, (a < b) ↔ (f a < f b)) :
    ∀ (ops : List (TimedOp α)) (tree : Tree α),
      runTimedFrom assign (reArrTree f tree) (mapTimedArr f ops) =
        runTimedFrom assign tree ops := by
  intro ops
  induction ops with
  | nil => intro tree; rfl
  | cons op ops ih =>
    intro tree
    cases op with
    | push pkt arr =>
      simp only [mapTimedArr, runTimedFrom]
      rw [← reArrTree_treePush]
      exact ih (treePush pkt arr tree (assign pkt))
    | pop =>
      simp only [mapTimedArr, runTimedFrom]
      rcases treePop_reArr f hf tree with
        ⟨hp, hp'⟩ | ⟨pkt, tree', hp, hp'⟩
      · simp only [hp, hp', ih]
      · simp only [hp, hp', ih]

def TimedOpsOn {α : Type} (P : α → Prop) : List (TimedOp α) → Prop
  | [] => True
  | .push pkt _ :: ops => P pkt ∧ TimedOpsOn P ops
  | .pop :: ops => TimedOpsOn P ops

theorem timedOpsFrom_on_true {k : Nat} (cnt : Nat) (ops : List (Op k)) :
    TimedOpsOn (fun _ : Fin k => True) (timedOpsFrom cnt ops) := by
  induction ops generalizing cnt with
  | nil => trivial
  | cons op ops ih =>
    cases op with
    | push pkt => exact ⟨trivial, ih (cnt + 1)⟩
    | pop => exact ih cnt

def TimedEquivOn {α : Type} (P : α → Prop) (assign₁ : α → Path)
    (tree₁ : Tree α) (assign₂ : α → Path) (tree₂ : Tree α) : Prop :=
  ∀ ops, TimedOpsOn P ops →
    runTimedFrom assign₁ tree₁ ops = runTimedFrom assign₂ tree₂ ops

theorem interEquiv_of_timedEquiv_empty {k : Nat} (S₁ S₂ : Scheduler k)
    (h : TimedEquivOn (fun _ : Fin k => True) S₁.assign (emptyTree S₁.topo)
      S₂.assign (emptyTree S₂.topo)) : interEquiv S₁ S₂ := by
  intro ops
  unfold run
  rw [runFrom_eq_runTimedFrom, runFrom_eq_runTimedFrom]
  exact h (timedOpsFrom 0 ops) (timedOpsFrom_on_true 0 ops)

def mapValEntry {α β : Type} (f : α → β) (e : Entry α) : Entry β :=
  ⟨f e.val, e.rank, e.arr⟩

theorem better_mapValEntry {α β : Type} (f : α → β) (x y : Entry α) :
    better (mapValEntry f x) (mapValEntry f y) = better x y := by
  cases x
  cases y
  rfl

theorem qpop_mapVal {α β : Type} (f : α → β) (q : Queue α) :
    (qpop q = none ∧ qpop (q.map (mapValEntry f)) = none) ∨
      ∃ e rest, qpop q = some (e, rest) ∧
        qpop (q.map (mapValEntry f)) =
          some (mapValEntry f e, rest.map (mapValEntry f)) := by
  induction q with
  | nil => left; exact ⟨rfl, rfl⟩
  | cons e es ih =>
    rcases ih with ⟨hn, hn'⟩ | ⟨best, rest, hp, hp'⟩
    · right
      exact ⟨e, [], qpop_cons_none_generic e es hn, by
        simpa using qpop_cons_none_generic (mapValEntry f e)
          (es.map (mapValEntry f)) hn'⟩
    · have hb := better_mapValEntry f e best
      by_cases hbetter : better e best = true
      · right
        refine ⟨e, es, ?_, ?_⟩
        · rw [qpop_cons_some_generic e best es rest hp, if_pos hbetter]
        · simp only [List.map_cons]
          rw [qpop_cons_some_generic (mapValEntry f e) (mapValEntry f best)
              (es.map (mapValEntry f)) (rest.map (mapValEntry f)) hp',
            if_pos (by rw [hb]; exact hbetter)]
      · right
        refine ⟨best, e :: rest, ?_, ?_⟩
        · rw [qpop_cons_some_generic e best es rest hp, if_neg hbetter]
        · simp only [List.map_cons]
          rw [qpop_cons_some_generic (mapValEntry f e) (mapValEntry f best)
              (es.map (mapValEntry f)) (rest.map (mapValEntry f)) hp',
            if_neg (by rw [hb]; exact hbetter)]

mutual
  def mapValTree {α β : Type} (f : α → β) : Tree α → Tree β
    | .leaf q => .leaf (q.map (mapValEntry f))
    | .node q children => .node q (mapValForest f children)

  def mapValForest {α β : Type} (f : α → β) : List (Tree α) → List (Tree β)
    | [] => []
    | tree :: trees => mapValTree f tree :: mapValForest f trees
end

mutual
  theorem mapValTree_treePush {α β : Type} (f : α → β) (pkt : α) (arr : Nat)
      (tree : Tree α) (path : Path) :
      mapValTree f (treePush pkt arr tree path) =
        treePush (f pkt) arr (mapValTree f tree) path := by
    cases tree with
    | leaf q =>
      cases path <;> simp [treePush, mapValTree, mapValEntry, List.map_append]
    | node q children =>
      cases path with
      | leaf rank => rfl
      | node child rank rest =>
        simp only [treePush, mapValTree]
        rw [mapValForest_treePushAt]

  theorem mapValForest_treePushAt {α β : Type} (f : α → β) (pkt : α)
      (arr : Nat) (trees : List (Tree α)) (child : Nat) (path : Path) :
      mapValForest f (treePushAt pkt arr trees child path) =
        treePushAt (f pkt) arr (mapValForest f trees) child path := by
    cases trees with
    | nil => rfl
    | cons tree trees =>
      cases child with
      | zero => simp [treePushAt, mapValForest, mapValTree_treePush]
      | succ child => simp [treePushAt, mapValForest, mapValForest_treePushAt]
end

mutual
  theorem treePop_mapVal {α β : Type} (f : α → β) (tree : Tree α) :
      (treePop tree = none ∧ treePop (mapValTree f tree) = none) ∨
        ∃ pkt tree', treePop tree = some (pkt, tree') ∧
          treePop (mapValTree f tree) = some (f pkt, mapValTree f tree') := by
    cases tree with
    | leaf q =>
      rcases qpop_mapVal f q with ⟨hn, hn'⟩ | ⟨e, rest, hp, hp'⟩
      · left
        simp [treePop, mapValTree, hn, hn']
      · right
        exact ⟨e.val, .leaf rest, by
          simp [treePop, mapValTree, mapValEntry, hp, hp']⟩
    | node q children =>
      cases hq : qpop q with
      | none =>
        left
        simp [treePop, mapValTree, hq]
      | some result =>
        obtain ⟨e, rest⟩ := result
        rcases treePopAt_mapVal f children e.val with
          ⟨hc, hc'⟩ | ⟨pkt, children', hc, hc'⟩
        · left
          simp [treePop, mapValTree, hq, hc, hc']
        · right
          exact ⟨pkt, .node rest children', by
            simp [treePop, mapValTree, hq, hc, hc']⟩

  theorem treePopAt_mapVal {α β : Type} (f : α → β)
      (trees : List (Tree α)) (child : Nat) :
      (treePopAt trees child = none ∧
          treePopAt (mapValForest f trees) child = none) ∨
        ∃ pkt trees', treePopAt trees child = some (pkt, trees') ∧
          treePopAt (mapValForest f trees) child =
            some (f pkt, mapValForest f trees') := by
    cases trees with
    | nil => left; exact ⟨rfl, rfl⟩
    | cons tree trees =>
      cases child with
      | zero =>
        rcases treePop_mapVal f tree with
          ⟨ht, ht'⟩ | ⟨pkt, tree', ht, ht'⟩
        · left
          simp [treePopAt, mapValForest, ht, ht']
        · right
          exact ⟨pkt, tree' :: trees, by
            simp [treePopAt, mapValForest, ht, ht']⟩
      | succ child =>
        rcases treePopAt_mapVal f trees child with
          ⟨ht, ht'⟩ | ⟨pkt, trees', ht, ht'⟩
        · left
          simp [treePopAt, mapValForest, ht, ht']
        · right
          exact ⟨pkt, tree :: trees', by
            simp [treePopAt, mapValForest, ht, ht']⟩
end

def mapTimedVal {α β : Type} (f : α → β) : List (TimedOp α) → List (TimedOp β)
  | [] => []
  | .push pkt arr :: ops => .push (f pkt) arr :: mapTimedVal f ops
  | .pop :: ops => .pop :: mapTimedVal f ops

theorem runTimedFrom_mapVal {α β : Type} (f : α → β)
    (assign₁ : α → Path) (assign₂ : β → Path)
    (hassign : ∀ pkt, assign₂ (f pkt) = assign₁ pkt) :
    ∀ (ops : List (TimedOp α)) (tree : Tree α),
      runTimedFrom assign₂ (mapValTree f tree) (mapTimedVal f ops) =
        (runTimedFrom assign₁ tree ops).map (Option.map f) := by
  intro ops
  induction ops with
  | nil => intro tree; rfl
  | cons op ops ih =>
    intro tree
    cases op with
    | push pkt arr =>
      simp only [mapTimedVal, runTimedFrom, hassign pkt]
      rw [← mapValTree_treePush]
      exact ih (treePush pkt arr tree (assign₁ pkt))
    | pop =>
      simp only [mapTimedVal, runTimedFrom]
      rcases treePop_mapVal f tree with
        ⟨hp, hp'⟩ | ⟨pkt, tree', hp, hp'⟩
      · simp only [hp, hp', ih, List.map_cons, Option.map_none]
      · simp only [hp, hp', ih, List.map_cons, Option.map_some]

mutual
  theorem mapValTree_emptyTree {α β : Type} (f : α → β) (topology : Topology) :
      mapValTree f (emptyTree (α := α) topology) = emptyTree (α := β) topology := by
    cases topology with
    | leaf => rfl
    | node topologies =>
      simp only [emptyTree, mapValTree]
      rw [mapValForest_emptyForest]

  theorem mapValForest_emptyForest {α β : Type} (f : α → β)
      (topologies : List Topology) :
      mapValForest f (emptyForest (α := α) topologies) =
        emptyForest (α := β) topologies := by
    cases topologies with
    | nil => rfl
    | cons topology topologies =>
      simp only [emptyForest, mapValForest, List.cons.injEq]
      exact ⟨mapValTree_emptyTree f topology, mapValForest_emptyForest f topologies⟩
end

def schedulerComap {m k : Nat} (S : Scheduler k) (embedding : Fin m → Fin k) :
    Scheduler m :=
  ⟨S.topo, fun pkt => S.assign (embedding pkt)⟩

theorem schedulerComap_valid {m k : Nat} (S : Scheduler k) (embedding : Fin m → Fin k)
    (hvalid : S.Valid) : (schedulerComap S embedding).Valid := by
  intro pkt
  exact hvalid (embedding pkt)

def mapOpsVal {m k : Nat} (embedding : Fin m → Fin k) :
    List (Op m) → List (Op k)
  | [] => []
  | .push pkt :: ops => .push (embedding pkt) :: mapOpsVal embedding ops
  | .pop :: ops => .pop :: mapOpsVal embedding ops

theorem timedOpsFrom_mapOpsVal {m k : Nat} (embedding : Fin m → Fin k) :
    ∀ (cnt : Nat) (ops : List (Op m)),
      timedOpsFrom cnt (mapOpsVal embedding ops) =
        mapTimedVal embedding (timedOpsFrom cnt ops) := by
  intro cnt ops
  induction ops generalizing cnt with
  | nil => rfl
  | cons op ops ih =>
    cases op with
    | push pkt =>
      simp only [mapOpsVal, timedOpsFrom, mapTimedVal, List.cons.injEq, true_and]
      exact ih (cnt + 1)
    | pop =>
      simp only [mapOpsVal, timedOpsFrom, mapTimedVal, List.cons.injEq, true_and]
      exact ih cnt

theorem run_schedulerComap {m k : Nat} (S : Scheduler k) (embedding : Fin m → Fin k)
    (ops : List (Op m)) :
    run S (mapOpsVal embedding ops) =
      (run (schedulerComap S embedding) ops).map (Option.map embedding) := by
  unfold run
  rw [runFrom_eq_runTimedFrom, runFrom_eq_runTimedFrom,
    timedOpsFrom_mapOpsVal, ← mapValTree_emptyTree embedding S.topo]
  exact runTimedFrom_mapVal embedding (schedulerComap S embedding).assign S.assign
    (fun _ => rfl) (timedOpsFrom 0 ops) (emptyTree S.topo)

theorem mapOpsVal_append {m k : Nat} (embedding : Fin m → Fin k)
    (ops₁ ops₂ : List (Op m)) :
    mapOpsVal embedding (ops₁ ++ ops₂) =
      mapOpsVal embedding ops₁ ++ mapOpsVal embedding ops₂ := by
  induction ops₁ with
  | nil => rfl
  | cons op ops₁ ih =>
    cases op <;> simp [mapOpsVal, ih]

theorem mapOpsVal_pushes {m k : Nat} (embedding : Fin m → Fin k)
    (word : List (Fin m)) :
    mapOpsVal embedding (word.map Op.push) = (word.map embedding).map Op.push := by
  induction word with
  | nil => rfl
  | cons pkt word ih => simp [mapOpsVal, ih]

theorem mapOpsVal_pops {m k : Nat} (embedding : Fin m → Fin k) (n : Nat) :
    mapOpsVal embedding (List.replicate n (Op.pop : Op m)) =
      List.replicate n (Op.pop : Op k) := by
  induction n with
  | zero => rfl
  | succ n ih =>
    change mapOpsVal embedding (List.replicate (n + 1) (Op.pop : Op m)) =
      List.replicate (n + 1) (Op.pop : Op k)
    rw [List.replicate_succ, List.replicate_succ]
    exact congrArg (fun ops => Op.pop :: ops) ih

theorem mapOpsVal_flushOps {m k : Nat} (embedding : Fin m → Fin k)
    (word : List (Fin m)) :
    mapOpsVal embedding (flushOps word) = flushOps (word.map embedding) := by
  simp [flushOps, mapOpsVal_append, mapOpsVal_pushes, mapOpsVal_pops]

theorem listMap_injective {α β : Type} (f : α → β) (hf : Function.Injective f) :
    Function.Injective (List.map f) := by
  intro xs
  induction xs with
  | nil =>
    intro ys h
    cases ys with
    | nil => rfl
    | cons y ys => simp at h
  | cons x xs ih =>
    intro ys h
    cases ys with
    | nil => simp at h
    | cons y ys =>
      simp only [List.map_cons, List.cons.injEq] at h
      obtain ⟨hxy, hrest⟩ := h
      rw [hf hxy, ih hrest]

theorem flushEquiv_schedulerComap {m k : Nat} (S₁ S₂ : Scheduler k)
    (embedding : Fin m → Fin k) (hinj : Function.Injective embedding)
    (hflush : flushEquiv S₁ S₂) :
    flushEquiv (schedulerComap S₁ embedding) (schedulerComap S₂ embedding) := by
  intro word
  have h := hflush (word.map embedding)
  rw [← mapOpsVal_flushOps embedding word, run_schedulerComap,
    run_schedulerComap] at h
  exact listMap_injective (Option.map embedding) (Option.map_injective hinj) h

inductive ListAt {α : Type} (value : α) : List α → Nat → Prop where
  | zero (tail : List α) : ListAt value (value :: tail) 0
  | succ (head : α) {tail : List α} {index : Nat} :
      ListAt value tail index → ListAt value (head :: tail) (index + 1)

theorem ListAt.treePushAt {α : Type} (pkt : α) (arr : Nat) (path : Path)
    {tree : Tree α} {trees : List (Tree α)} {child : Nat}
    (h : ListAt tree trees child) :
    ListAt (treePush pkt arr tree path) (treePushAt pkt arr trees child path) child := by
  induction h with
  | zero tail => exact .zero tail
  | succ head h ih => exact .succ head ih

theorem ListAt.treePopAt_some {α : Type} {tree tree' : Tree α}
    {trees : List (Tree α)} {child : Nat} {pkt : α}
    (h : ListAt tree trees child) (hp : treePop tree = some (pkt, tree')) :
    ∃ trees', treePopAt trees child = some (pkt, trees') ∧
      ListAt tree' trees' child := by
  induction h with
  | zero tail =>
    exact ⟨tree' :: tail, by simp [treePopAt, hp], .zero tail⟩
  | succ head h ih =>
    obtain ⟨trees', hpop, hat⟩ := ih
    exact ⟨head :: trees', by simp [treePopAt, hpop], .succ head hat⟩

theorem ListAt.treePushAt_other {α : Type} (pkt : α) (arr : Nat)
    (path : Path) {tree : Tree α} {trees : List (Tree α)} {target child : Nat}
    (h : ListAt tree trees target) (hne : child ≠ target) :
    ListAt tree (PifoStatement.treePushAt pkt arr trees child path) target := by
  induction h generalizing child with
  | zero tail =>
    cases child with
    | zero => exact False.elim (hne rfl)
    | succ child => exact .zero _
  | @succ head tail target h ih =>
    cases child with
    | zero => exact .succ (treePush pkt arr head path) h
    | succ child =>
      exact ListAt.succ head (ih (fun heq =>
        hne (congrArg Nat.succ heq)))

theorem ListAt.of_treePopAt_some {α : Type} {tree : Tree α}
    {trees trees' : List (Tree α)} {child : Nat} {pkt : α}
    (h : ListAt tree trees child)
    (hpop : treePopAt trees child = some (pkt, trees')) :
    ∃ tree', treePop tree = some (pkt, tree') ∧ ListAt tree' trees' child := by
  induction h generalizing trees' with
  | zero tail =>
    simp only [treePopAt] at hpop
    cases hp : treePop tree with
    | none => simp [hp] at hpop
    | some result =>
      obtain ⟨value, tree'⟩ := result
      simp only [hp, Option.some.injEq, Prod.mk.injEq] at hpop
      obtain ⟨rfl, rfl⟩ := hpop
      exact ⟨tree', rfl, .zero tail⟩
  | @succ head tail child h ih =>
    simp only [treePopAt] at hpop
    cases hp : treePopAt tail child with
    | none => simp [hp] at hpop
    | some result =>
      obtain ⟨value, tail'⟩ := result
      simp only [hp, Option.some.injEq, Prod.mk.injEq] at hpop
      obtain ⟨rfl, rfl⟩ := hpop
      obtain ⟨tree', htree, hat⟩ := ih hp
      exact ⟨tree', htree, .succ head hat⟩

theorem ListAt.treePopAt_other {α : Type} {tree : Tree α}
    {trees trees' : List (Tree α)} {target child : Nat} {pkt : α}
    (h : ListAt tree trees target) (hne : child ≠ target)
    (hpop : treePopAt trees child = some (pkt, trees')) :
    ListAt tree trees' target := by
  induction h generalizing child trees' with
  | zero tail =>
    cases child with
    | zero => exact False.elim (hne rfl)
    | succ child =>
      simp only [treePopAt] at hpop
      cases hp : treePopAt tail child with
      | none => simp [hp] at hpop
      | some result =>
        obtain ⟨value, tail'⟩ := result
        simp only [hp, Option.some.injEq, Prod.mk.injEq] at hpop
        obtain ⟨rfl, rfl⟩ := hpop
        exact .zero tail'
  | @succ head tail target h ih =>
    cases child with
    | zero =>
      simp only [treePopAt] at hpop
      cases hp : treePop head with
      | none => simp [hp] at hpop
      | some result =>
        obtain ⟨value, head'⟩ := result
        simp only [hp, Option.some.injEq, Prod.mk.injEq] at hpop
        obtain ⟨rfl, rfl⟩ := hpop
        exact .succ head' h
    | succ child =>
      simp only [treePopAt] at hpop
      cases hp : treePopAt tail child with
      | none => simp [hp] at hpop
      | some result =>
        obtain ⟨value, tail'⟩ := result
        simp only [hp, Option.some.injEq, Prod.mk.injEq] at hpop
        obtain ⟨rfl, rfl⟩ := hpop
        exact ListAt.succ head (ih (fun heq =>
          hne (congrArg Nat.succ heq)) hp)

theorem ListAt.emptyForest {α : Type} {topology : Topology}
    {topologies : List Topology} {child : Nat}
    (h : ListAt topology topologies child) :
    ListAt (emptyTree (α := α) topology) (emptyForest topologies) child := by
  induction h with
  | zero tail => exact .zero (PifoStatement.emptyForest tail)
  | succ head h ih => exact .succ (emptyTree head) ih

theorem ListAt.packetCountAt {α : Type} {tree : Tree α}
    {trees : List (Tree α)} {child : Nat} (h : ListAt tree trees child) :
    packetCountAt trees child = packetCount tree := by
  induction h with
  | zero tail => rfl
  | succ head h ih => exact ih

def QueueOnly (child : Nat) (q : Queue Nat) : Prop :=
  ∀ other, other ≠ child → valCount other q = 0

theorem queueOnly_nil (child : Nat) : QueueOnly child [] := by
  intro other hne
  rfl

theorem queueOnly_append (child rank arr : Nat) (q : Queue Nat)
    (honly : QueueOnly child q) :
    QueueOnly child (q ++ [⟨child, rank, arr⟩]) := by
  intro other hne
  rw [valCount_append, honly other hne, valCount_singleton]
  have hrev : child ≠ other := fun h => hne h.symm
  simp [hrev]

theorem queueOnly_qpop {child : Nat} {q : Queue Nat} {best : Entry Nat}
    {rest : Queue Nat} (honly : QueueOnly child q)
    (hpop : qpop q = some (best, rest)) :
    best.val = child ∧ QueueOnly child rest := by
  have hbest : best.val = child := Classical.byContradiction (fun hne => by
    have hc := qpop_valCount hpop best.val
    rw [honly best.val hne] at hc
    simp at hc)
  refine ⟨hbest, ?_⟩
  intro other hne
  have hc := qpop_valCount hpop other
  rw [honly other hne, hbest] at hc
  have hrev : child ≠ other := fun h => hne h.symm
  simp [hrev] at hc
  exact hc.symm

theorem runTimedFrom_unary_congr {α : Type} {P : α → Prop}
    (assign tail : α → Path) (rank : α → Nat) (child : Nat)
    (topology : Topology)
    (hassign : ∀ pkt, P pkt → assign pkt = .node child (rank pkt) (tail pkt))
    (hvalidTail : ∀ pkt, P pkt → pathOk topology (tail pkt) = true) :
    ∀ (ops : List (TimedOp α)) (q : Queue Nat) (trees : List (Tree α))
      (tree : Tree α),
      ListAt tree trees child → Good topology tree →
      q.length = packetCount tree → QueueOnly child q →
      TimedOpsOn P ops →
      runTimedFrom assign (.node q trees) ops = runTimedFrom tail tree ops := by
  intro ops
  induction ops with
  | nil => intros; rfl
  | cons op ops ih =>
    intro q trees tree hat hgood hcount honly hops
    cases op with
    | push pkt arr =>
      obtain ⟨hp, hops⟩ := hops
      simp only [runTimedFrom]
      rw [hassign pkt hp]
      simp only [treePush]
      apply ih (q ++ [({ val := child, rank := rank pkt, arr := arr } : Entry Nat)])
        (treePushAt pkt arr trees child (tail pkt))
        (treePush pkt arr tree (tail pkt))
      · exact hat.treePushAt pkt arr (tail pkt)
      · exact (treePush_good_count pkt arr topology tree (tail pkt) hgood
          (hvalidTail pkt hp)).1
      · have hc := (treePush_good_count pkt arr topology tree (tail pkt) hgood
          (hvalidTail pkt hp)).2
        simp only [List.length_append, List.length_singleton]
        omega
      · exact queueOnly_append child (rank pkt) arr q honly
      · exact hops
    | pop =>
      simp only [TimedOpsOn] at hops
      by_cases hz : packetCount tree = 0
      · have hqnil : q = [] := by
          apply List.length_eq_zero_iff.mp
          omega
        have hp := treePop_good_zero topology tree hgood hz
        subst q
        simp only [runTimedFrom, treePop, qpop, hp]
        exact congrArg (fun xs => none :: xs)
          (ih [] trees tree hat hgood (by simp [hz]) (queueOnly_nil child) hops)
      · have hpos : 0 < packetCount tree := by omega
        have hqne : q ≠ [] := by
          intro hq
          subst q
          simp at hcount
          omega
        cases q with
        | nil => contradiction
        | cons e es =>
          obtain ⟨best, rest, hqpop⟩ := qpop_nonempty e es
          obtain ⟨hbest, honlyRest⟩ := queueOnly_qpop honly hqpop
          obtain ⟨pkt, tree', htreePop, hgood', htreeCount⟩ :=
            treePop_good_pos topology tree hgood hpos
          obtain ⟨trees', htreesPop, hat'⟩ := hat.treePopAt_some htreePop
          have hnodePop : treePop (.node (e :: es) trees) =
              some (pkt, .node rest trees') := by
            simp [treePop, hqpop, hbest, htreesPop]
          simp only [runTimedFrom, hnodePop, htreePop, List.cons.injEq, true_and]
          apply ih rest trees' tree' hat' hgood'
          · have hqLength : rest.length + 1 = (e :: es).length := qpop_length hqpop
            have heq : rest.length + 1 = packetCount tree := hqLength.trans hcount
            have heq' : rest.length + 1 = packetCount tree' + 1 :=
              heq.trans htreeCount
            exact Nat.add_right_cancel heq'
          · exact honlyRest
          · exact hops

theorem unaryNodeTimedEquiv_empty {α : Type} {P : α → Prop}
    (assign tail : α → Path) (rank : α → Nat) (child : Nat)
    (topology : Topology) (topologies : List Topology)
    (hat : ListAt topology topologies child)
    (hassign : ∀ pkt, P pkt → assign pkt = .node child (rank pkt) (tail pkt))
    (hvalidTail : ∀ pkt, P pkt → pathOk topology (tail pkt) = true) :
    TimedEquivOn P assign (emptyTree (.node topologies))
      tail (emptyTree topology) := by
  intro ops hops
  apply runTimedFrom_unary_congr assign tail rank child topology hassign hvalidTail
    ops [] (emptyForest topologies) (emptyTree topology)
  · exact hat.emptyForest
  · exact emptyTree_good topology
  · simp [packetCount_emptyTree]
  · exact queueOnly_nil child
  · exact hops

def packetValCountAt {α : Type} [DecidableEq α] (x : α) :
    List (Tree α) → Nat → Nat
  | [], _ => 0
  | tree :: _, 0 => packetValCount x tree
  | _ :: trees, index + 1 => packetValCountAt x trees index

theorem packetValCountAt_emptyForest {α : Type} [DecidableEq α] (x : α)
    (topologies : List Topology) (index : Nat) :
    packetValCountAt x (emptyForest topologies) index = 0 := by
  induction topologies generalizing index with
  | nil => rfl
  | cons topology topologies ih =>
    cases index with
    | zero => exact packetValCount_emptyTree x topology
    | succ index => exact ih index

theorem treePushAt_packetValCountAt {α : Type} [DecidableEq α]
    (pkt x : α) (arr : Nat) (topologies : List Topology)
    (trees : List (Tree α)) (child index : Nat) (path : Path)
    (hgood : GoodForest topologies trees)
    (hvalid : pathOkAt topologies child path = true) :
    packetValCountAt x (treePushAt pkt arr trees child path) index =
      packetValCountAt x trees index +
        (if child = index ∧ pkt = x then 1 else 0) := by
  cases topologies with
  | nil => simp [pathOkAt] at hvalid
  | cons topology topologies =>
    cases trees with
    | nil => simp [GoodForest] at hgood
    | cons tree trees =>
      obtain ⟨hgoodTree, hgoodTrees⟩ := hgood
      cases child with
      | zero =>
        have hc := treePush_packetValCount pkt x arr topology tree path
          hgoodTree hvalid
        cases index with
        | zero => simpa [treePushAt, packetValCountAt] using hc
        | succ index => simp [treePushAt, packetValCountAt]
      | succ child =>
        cases index with
        | zero => simp [treePushAt, packetValCountAt]
        | succ index =>
          have hc := treePushAt_packetValCountAt pkt x arr topologies trees child
            index path hgoodTrees hvalid
          simpa [treePushAt, packetValCountAt] using hc

theorem treePopAt_packetValCountAt {α : Type} [DecidableEq α]
    {trees : List (Tree α)} {child : Nat} {pkt : α} {trees' : List (Tree α)}
    (hpop : treePopAt trees child = some (pkt, trees')) (x : α) (index : Nat) :
    packetValCountAt x trees index = packetValCountAt x trees' index +
      (if child = index ∧ pkt = x then 1 else 0) := by
  cases trees with
  | nil => simp [treePopAt] at hpop
  | cons tree trees =>
    cases child with
    | zero =>
      cases ht : treePop tree with
      | none => simp [treePopAt, ht] at hpop
      | some result =>
        obtain ⟨treePkt, tree'⟩ := result
        simp only [treePopAt, ht, Option.some.injEq, Prod.mk.injEq] at hpop
        obtain ⟨rfl, rfl⟩ := hpop
        cases index with
        | zero => simpa [packetValCountAt] using treePop_packetValCount ht x
        | succ index => simp [packetValCountAt]
    | succ child =>
      cases ht : treePopAt trees child with
      | none => simp [treePopAt, ht] at hpop
      | some result =>
        obtain ⟨treePkt, trees'⟩ := result
        simp only [treePopAt, ht, Option.some.injEq, Prod.mk.injEq] at hpop
        obtain ⟨rfl, rfl⟩ := hpop
        cases index with
        | zero => simp [packetValCountAt]
        | succ index =>
          simpa [packetValCountAt] using treePopAt_packetValCountAt ht x index

def IndexedOnly {k : Nat} (trees : List (Tree (Fin k))) : Prop :=
  ∀ (index pkt : Fin k), pkt ≠ index →
    packetValCountAt pkt trees index.val = 0

theorem indexedOnly_emptyForest {k : Nat} (topologies : List Topology) :
    IndexedOnly (emptyForest (α := Fin k) topologies) := by
  intro index pkt hne
  exact packetValCountAt_emptyForest pkt topologies index.val

theorem indexedOnly_treePushAt {k : Nat} (topologies : List Topology)
    (trees : List (Tree (Fin k))) (pkt : Fin k) (arr : Nat) (path : Path)
    (hgood : GoodForest topologies trees)
    (hvalid : pathOkAt topologies pkt.val path = true)
    (honly : IndexedOnly trees) :
    IndexedOnly (treePushAt pkt arr trees pkt.val path) := by
  intro index other hne
  rw [treePushAt_packetValCountAt pkt other arr topologies trees pkt.val
    index.val path hgood hvalid, honly index other hne]
  by_cases hindex : pkt.val = index.val
  · have hpkt : pkt = index := Fin.ext hindex
    have hpother : pkt ≠ other := fun h => hne (h.symm.trans hpkt)
    simp [hindex, hpother]
  · simp [hindex]

theorem indexedOnly_treePopAt {k : Nat} {trees : List (Tree (Fin k))}
    {index : Fin k} {pkt : Fin k} {trees' : List (Tree (Fin k))}
    (honly : IndexedOnly trees)
    (hpop : treePopAt trees index.val = some (pkt, trees')) :
    pkt = index ∧ IndexedOnly trees' := by
  have hpkt : pkt = index := Classical.byContradiction (fun hne => by
    have hc := treePopAt_packetValCountAt hpop pkt index.val
    rw [honly index pkt hne] at hc
    simp at hc)
  refine ⟨hpkt, ?_⟩
  intro other value hne
  have hc := treePopAt_packetValCountAt hpop value other.val
  rw [honly other value hne, hpkt] at hc
  by_cases hindex : index.val = other.val
  · have hi : index = other := Fin.ext hindex
    subst other
    have hrev : index ≠ value := fun h => hne h.symm
    simp [hrev] at hc
    exact hc.symm
  · simp [hindex] at hc
    exact hc.symm

theorem ListAt.pathOkAt {topology : Topology} {topologies : List Topology}
    {child : Nat} (h : ListAt topology topologies child) (path : Path) :
    pathOkAt topologies child path = pathOk topology path := by
  induction h with
  | zero tail => rfl
  | succ head h ih => exact ih

theorem listAt_replicate_fin {α : Type} (value : α) {k : Nat} (index : Fin k) :
    ListAt value (List.replicate k value) index.val := by
  cases k with
  | zero => exact Fin.elim0 index
  | succ k =>
    change ListAt value (List.replicate (k + 1) value) index.val
    rw [List.replicate_succ]
    exact Fin.cases (.zero _) (fun i => .succ value (listAt_replicate_fin value i)) index

def explodedTopology (k : Nat) : Topology :=
  .node (List.replicate k .leaf)

def leafAssign {k : Nat} (rank : Fin k → Nat) (pkt : Fin k) : Path :=
  .leaf (rank pkt)

def explodedAssign {k : Nat} (rank : Fin k → Nat) (pkt : Fin k) : Path :=
  .node pkt.val (rank pkt) (.leaf 0)

theorem explodedAssign_valid {k : Nat} (rank : Fin k → Nat) (pkt : Fin k) :
    pathOk (explodedTopology k) (explodedAssign rank pkt) = true := by
  change pathOkAt (List.replicate k .leaf) pkt.val (.leaf 0) = true
  rw [(listAt_replicate_fin Topology.leaf pkt).pathOkAt]
  rfl

theorem valCount_mapValEntry_fin {k : Nat} (q : Queue (Fin k)) (pkt : Fin k) :
    valCount pkt.val (q.map (mapValEntry Fin.val)) = valCount pkt q := by
  induction q with
  | nil => rfl
  | cons e es ih =>
    simp only [List.map_cons, valCount, ih, mapValEntry]
    by_cases h : e.val = pkt
    · subst pkt; simp
    · have hv : e.val.val ≠ pkt.val := fun heq => h (Fin.ext heq)
      simp [h, hv]

theorem runTimedFrom_leaf_exploded {k : Nat} (rank : Fin k → Nat) :
    ∀ (ops : List (TimedOp (Fin k))) (q : Queue (Fin k))
      (children : List (Tree (Fin k))),
      Good (explodedTopology k)
        (.node (q.map (mapValEntry Fin.val)) children) →
      IndexedOnly children →
      runTimedFrom (leafAssign rank) (.leaf q) ops =
        runTimedFrom (explodedAssign rank)
          (.node (q.map (mapValEntry Fin.val)) children) ops := by
  intro ops
  induction ops with
  | nil => intros; rfl
  | cons op ops ih =>
    intro q children hgood honly
    cases op with
    | push pkt arr =>
      simp only [runTimedFrom, leafAssign, explodedAssign, treePush]
      have hvalid := explodedAssign_valid rank pkt
      have pushed := treePush_good_count pkt arr (explodedTopology k)
        (.node (q.map (mapValEntry Fin.val)) children) (explodedAssign rank pkt)
        hgood hvalid
      have hqueue :
          q.map (mapValEntry Fin.val) ++
              [({ val := pkt.val, rank := rank pkt, arr := arr } : Entry Nat)] =
            (q ++ [({ val := pkt, rank := rank pkt, arr := arr } : Entry (Fin k))]).map
              (mapValEntry Fin.val) := by
        simp [List.map_append, mapValEntry]
      rw [hqueue]
      apply ih (q ++ [({ val := pkt, rank := rank pkt, arr := arr } : Entry (Fin k))])
        (treePushAt pkt arr children pkt.val (.leaf 0))
      · simpa [explodedTopology, explodedAssign, treePush, List.map_append,
          mapValEntry] using pushed.1
      · apply indexedOnly_treePushAt (List.replicate k .leaf) children pkt arr
          (.leaf 0) hgood.1
        · exact hvalid
        · exact honly
    | pop =>
      cases hp : qpop q with
      | none =>
        have hq : q = [] := (qpop_eq_none_iff q).mp hp
        subst q
        simp only [runTimedFrom, treePop, qpop, List.map]
        exact congrArg (fun xs => none :: xs) (ih [] children hgood honly)
      | some result =>
        obtain ⟨e, rest⟩ := result
        have hpMap : qpop (q.map (mapValEntry Fin.val)) =
            some (mapValEntry Fin.val e, rest.map (mapValEntry Fin.val)) := by
          rcases qpop_mapVal Fin.val q with
            ⟨hn, hn'⟩ | ⟨e', rest', hp', hpMap'⟩
          · rw [hp] at hn
            contradiction
          · rw [hp] at hp'
            obtain ⟨rfl, rfl⟩ := Option.some.inj hp'
            exact hpMap'
        have hchild : 0 < packetCountAt children e.val.val := by
          have hc := qpop_valCount hp e.val
          have hcpos : 0 < valCount e.val q := by
            simp at hc
            omega
          rw [← hgood.2 e.val.val, valCount_mapValEntry_fin q e.val]
          exact hcpos
        obtain ⟨pkt, children', hchildrenPop, hgoodForest', hforestCount, hAt⟩ :=
          treePopAt_good_pos (List.replicate k .leaf) children e.val.val
            hgood.1 hchild
        obtain ⟨hpkt, honly'⟩ := indexedOnly_treePopAt honly hchildrenPop
        subst pkt
        have hgood' : Good (explodedTopology k)
            (.node (rest.map (mapValEntry Fin.val)) children') := by
          constructor
          · exact hgoodForest'
          · intro index
            have hc := qpop_valCount hpMap index
            rw [hgood.2 index, hAt index] at hc
            simp only [mapValEntry] at hc
            exact (Nat.add_right_cancel hc).symm
        simp only [runTimedFrom, treePop, hp, hpMap, hchildrenPop, mapValEntry,
          List.cons.injEq, true_and]
        exact ih rest children' hgood' honly'

theorem leafExplodedTimedEquiv {k : Nat} (rank : Fin k → Nat) :
    TimedEquivOn (fun _ : Fin k => True) (leafAssign rank) (emptyTree .leaf)
      (explodedAssign rank) (emptyTree (explodedTopology k)) := by
  intro ops hops
  apply runTimedFrom_leaf_exploded rank ops []
    (emptyForest (List.replicate k .leaf))
  · exact emptyTree_good (explodedTopology k)
  · exact indexedOnly_emptyForest (List.replicate k .leaf)

def pathHeadRank : Path → Nat
  | .leaf rank => rank
  | .node _ rank _ => rank

theorem path_eq_leaf_of_ok (path : Path) (h : pathOk .leaf path = true) :
    path = .leaf (pathHeadRank path) := by
  cases path with
  | leaf rank => rfl
  | node child rank tail => simp [pathOk] at h

def RoutedOnly {α : Type} [DecidableEq α] (color : α → Nat)
    (trees : List (Tree α)) : Prop :=
  ∀ (pkt : α) (index : Nat), color pkt ≠ index →
    packetValCountAt pkt trees index = 0

theorem routedOnly_emptyForest {α : Type} [DecidableEq α]
    (color : α → Nat) (topologies : List Topology) :
    RoutedOnly color (emptyForest (α := α) topologies) := by
  intro pkt index hne
  exact packetValCountAt_emptyForest pkt topologies index

theorem routedOnly_treePushAt {α : Type} [DecidableEq α]
    (color : α → Nat) (topologies : List Topology) (trees : List (Tree α))
    (pkt : α) (arr : Nat) (path : Path)
    (hgood : GoodForest topologies trees)
    (hvalid : pathOkAt topologies (color pkt) path = true)
    (honly : RoutedOnly color trees) :
    RoutedOnly color (treePushAt pkt arr trees (color pkt) path) := by
  intro value index hne
  rw [treePushAt_packetValCountAt pkt value arr topologies trees (color pkt)
    index path hgood hvalid, honly value index hne]
  have hfalse : ¬(color pkt = index ∧ pkt = value) := by
    rintro ⟨hcolor, rfl⟩
    exact hne hcolor
  simp [hfalse]

theorem routedOnly_treePopAt {α : Type} [DecidableEq α]
    (color : α → Nat) (hinj : Function.Injective color)
    {trees : List (Tree α)} {selected pkt : α} {trees' : List (Tree α)}
    (honly : RoutedOnly color trees)
    (hpop : treePopAt trees (color selected) = some (pkt, trees')) :
    pkt = selected ∧ RoutedOnly color trees' := by
  have hpkt : pkt = selected := Classical.byContradiction (fun hne => by
    have hcolor : color pkt ≠ color selected := fun heq => hne (hinj heq)
    have hc := treePopAt_packetValCountAt hpop pkt (color selected)
    rw [honly pkt (color selected) hcolor] at hc
    simp at hc)
  refine ⟨hpkt, ?_⟩
  intro value index hne
  have hc := treePopAt_packetValCountAt hpop value index
  rw [honly value index hne, hpkt] at hc
  have hfalse : ¬(color selected = index ∧ selected = value) := by
    rintro ⟨hcolor, rfl⟩
    exact hne hcolor
  simp [hfalse] at hc
  exact hc.symm

theorem routedOnly_treePopAt_color {α : Type} [DecidableEq α]
    (color : α → Nat) {trees : List (Tree α)} {selected pkt : α}
    {trees' : List (Tree α)} (honly : RoutedOnly color trees)
    (hpop : treePopAt trees (color selected) = some (pkt, trees')) :
    color pkt = color selected ∧ RoutedOnly color trees' := by
  have hcolor : color pkt = color selected := Classical.byContradiction (fun hne => by
    have hc := treePopAt_packetValCountAt hpop pkt (color selected)
    rw [honly pkt (color selected) hne] at hc
    simp at hc)
  refine ⟨hcolor, ?_⟩
  intro value index hne
  have hc := treePopAt_packetValCountAt hpop value index
  rw [honly value index hne] at hc
  have hfalse : ¬(color selected = index ∧ pkt = value) := by
    rintro ⟨hselected, rfl⟩
    exact hne (hcolor.trans hselected)
  simp [hfalse] at hc
  exact hc.symm

theorem routedOnly_treePopAt_index {α : Type} [DecidableEq α]
    (color : α → Nat) {trees : List (Tree α)} {index : Nat} {pkt : α}
    {trees' : List (Tree α)} (honly : RoutedOnly color trees)
    (hpop : treePopAt trees index = some (pkt, trees')) :
    color pkt = index ∧ RoutedOnly color trees' := by
  have hcolor : color pkt = index := Classical.byContradiction (fun hne => by
    have hc := treePopAt_packetValCountAt hpop pkt index
    rw [honly pkt index hne] at hc
    simp at hc)
  refine ⟨hcolor, ?_⟩
  intro value other hne
  have hc := treePopAt_packetValCountAt hpop value other
  rw [honly value other hne] at hc
  have hfalse : ¬(index = other ∧ pkt = value) := by
    rintro ⟨hindex, rfl⟩
    exact hne (hcolor.trans hindex)
  simp [hfalse] at hc
  exact hc.symm

theorem valCount_mapValEntry_injective {α β : Type} [DecidableEq α]
    [DecidableEq β] (f : α → β) (hinj : Function.Injective f)
    (q : Queue α) (pkt : α) :
    valCount (f pkt) (q.map (mapValEntry f)) = valCount pkt q := by
  induction q with
  | nil => rfl
  | cons e es ih =>
    simp only [List.map_cons, valCount, ih, mapValEntry]
    by_cases h : e.val = pkt
    · subst pkt; simp
    · have hv : f e.val ≠ f pkt := fun heq => h (hinj heq)
      simp [h, hv]

theorem runTimedFrom_leaf_routed {α : Type} [DecidableEq α]
    (rank color : α → Nat) (hinj : Function.Injective color)
    (assign tail : α → Path) (topologies : List Topology)
    (hassign : ∀ pkt, assign pkt = .node (color pkt) (rank pkt) (tail pkt))
    (hvalidTail : ∀ pkt, pathOkAt topologies (color pkt) (tail pkt) = true) :
    ∀ (ops : List (TimedOp α)) (q : Queue α) (children : List (Tree α)),
      Good (.node topologies) (.node (q.map (mapValEntry color)) children) →
      RoutedOnly color children →
      runTimedFrom (fun pkt => .leaf (rank pkt)) (.leaf q) ops =
        runTimedFrom assign (.node (q.map (mapValEntry color)) children) ops := by
  intro ops
  induction ops with
  | nil => intros; rfl
  | cons op ops ih =>
    intro q children hgood honly
    cases op with
    | push pkt arr =>
      simp only [runTimedFrom]
      rw [hassign pkt]
      simp only [treePush]
      have pushed := treePush_good_count pkt arr (.node topologies)
        (.node (q.map (mapValEntry color)) children)
        (.node (color pkt) (rank pkt) (tail pkt)) hgood (hvalidTail pkt)
      have hqueue :
          q.map (mapValEntry color) ++
              [({ val := color pkt, rank := rank pkt, arr := arr } : Entry Nat)] =
            (q ++ [({ val := pkt, rank := rank pkt, arr := arr } : Entry α)]).map
              (mapValEntry color) := by
        simp [List.map_append, mapValEntry]
      rw [hqueue]
      apply ih (q ++ [({ val := pkt, rank := rank pkt, arr := arr } : Entry α)])
        (treePushAt pkt arr children (color pkt) (tail pkt))
      · simpa [treePush, List.map_append, mapValEntry] using pushed.1
      · exact routedOnly_treePushAt color topologies children pkt arr (tail pkt)
          hgood.1 (hvalidTail pkt) honly
    | pop =>
      cases hp : qpop q with
      | none =>
        have hq : q = [] := (qpop_eq_none_iff q).mp hp
        subst q
        simp only [runTimedFrom, treePop, qpop, List.map]
        exact congrArg (fun xs => none :: xs) (ih [] children hgood honly)
      | some result =>
        obtain ⟨e, rest⟩ := result
        have hpMap : qpop (q.map (mapValEntry color)) =
            some (mapValEntry color e, rest.map (mapValEntry color)) := by
          rcases qpop_mapVal color q with
            ⟨hn, hn'⟩ | ⟨e', rest', hp', hpMap'⟩
          · rw [hp] at hn
            contradiction
          · rw [hp] at hp'
            obtain ⟨rfl, rfl⟩ := Option.some.inj hp'
            exact hpMap'
        have hchild : 0 < packetCountAt children (color e.val) := by
          have hc := qpop_valCount hp e.val
          have hcpos : 0 < valCount e.val q := by
            simp at hc
            omega
          rw [← hgood.2 (color e.val),
            valCount_mapValEntry_injective color hinj q e.val]
          exact hcpos
        obtain ⟨pkt, children', hchildrenPop, hgoodForest', hforestCount, hAt⟩ :=
          treePopAt_good_pos topologies children (color e.val) hgood.1 hchild
        obtain ⟨hpkt, honly'⟩ :=
          routedOnly_treePopAt color hinj honly hchildrenPop
        subst pkt
        have hgood' : Good (.node topologies)
            (.node (rest.map (mapValEntry color)) children') := by
          constructor
          · exact hgoodForest'
          · intro index
            have hc := qpop_valCount hpMap index
            rw [hgood.2 index, hAt index] at hc
            simp only [mapValEntry] at hc
            exact (Nat.add_right_cancel hc).symm
        simp only [runTimedFrom, treePop, hp, hpMap, hchildrenPop, mapValEntry,
          List.cons.injEq, true_and]
        exact ih rest children' hgood' honly'

theorem valCount_le_mapValEntry_color {α : Type} [DecidableEq α]
    (color : α → Nat) (q : Queue α) (pkt : α) :
    valCount pkt q ≤ valCount (color pkt) (q.map (mapValEntry color)) := by
  induction q with
  | nil => simp [valCount]
  | cons entry q ih =>
    simp only [List.map_cons, valCount, mapValEntry]
    by_cases he : entry.val = pkt
    · subst pkt
      simp
      omega
    · by_cases hc : color entry.val = color pkt
      · simp [he, hc]
        omega
      · simp [he, hc]
        omega

theorem runTimedFrom_rootColor {α : Type} [DecidableEq α]
    (rank color : α → Nat) (assign tail : α → Path) (topologies : List Topology)
    (hassign : ∀ pkt, assign pkt = .node (color pkt) (rank pkt) (tail pkt))
    (hvalidTail : ∀ pkt, pathOkAt topologies (color pkt) (tail pkt) = true) :
    ∀ (ops : List (TimedOp α)) (q : Queue α) (children : List (Tree α)),
      Good (.node topologies) (.node (q.map (mapValEntry color)) children) →
      RoutedOnly color children →
      (runTimedFrom (fun pkt => .leaf (rank pkt)) (.leaf q) ops).map
          (Option.map color) =
        (runTimedFrom assign (.node (q.map (mapValEntry color)) children) ops).map
          (Option.map color) := by
  intro ops
  induction ops with
  | nil => intros; rfl
  | cons op ops ih =>
    intro q children hgood honly
    cases op with
    | push pkt arr =>
      simp only [runTimedFrom]
      rw [hassign pkt]
      simp only [treePush]
      have pushed := treePush_good_count pkt arr (.node topologies)
        (.node (q.map (mapValEntry color)) children)
        (.node (color pkt) (rank pkt) (tail pkt)) hgood (hvalidTail pkt)
      have hqueue :
          q.map (mapValEntry color) ++
              [({ val := color pkt, rank := rank pkt, arr := arr } : Entry Nat)] =
            (q ++ [({ val := pkt, rank := rank pkt, arr := arr } : Entry α)]).map
              (mapValEntry color) := by
        simp [List.map_append, mapValEntry]
      rw [hqueue]
      apply ih (q ++ [({ val := pkt, rank := rank pkt, arr := arr } : Entry α)])
        (treePushAt pkt arr children (color pkt) (tail pkt))
      · simpa [treePush, List.map_append, mapValEntry] using pushed.1
      · exact routedOnly_treePushAt color topologies children pkt arr (tail pkt)
          hgood.1 (hvalidTail pkt) honly
    | pop =>
      cases hp : qpop q with
      | none =>
        have hq : q = [] := (qpop_eq_none_iff q).mp hp
        subst q
        simp only [runTimedFrom, treePop, qpop, List.map, Option.map_none,
          List.cons.injEq, true_and]
        exact ih [] children hgood honly
      | some result =>
        obtain ⟨e, rest⟩ := result
        have hpMap : qpop (q.map (mapValEntry color)) =
            some (mapValEntry color e, rest.map (mapValEntry color)) := by
          rcases qpop_mapVal color q with
            ⟨hn, hn'⟩ | ⟨e', rest', hp', hpMap'⟩
          · rw [hp] at hn
            contradiction
          · rw [hp] at hp'
            obtain ⟨rfl, rfl⟩ := Option.some.inj hp'
            exact hpMap'
        have hchild : 0 < packetCountAt children (color e.val) := by
          have hc := qpop_valCount hp e.val
          have hcpos : 0 < valCount e.val q := by
            simp at hc
            omega
          rw [← hgood.2 (color e.val)]
          have hle := valCount_le_mapValEntry_color color q e.val
          omega
        obtain ⟨pkt, children', hchildrenPop, hgoodForest', hforestCount, hAt⟩ :=
          treePopAt_good_pos topologies children (color e.val) hgood.1 hchild
        obtain ⟨hcolor, honly'⟩ :=
          routedOnly_treePopAt_color color honly hchildrenPop
        have hgood' : Good (.node topologies)
            (.node (rest.map (mapValEntry color)) children') := by
          constructor
          · exact hgoodForest'
          · intro index
            have hc := qpop_valCount hpMap index
            rw [hgood.2 index, hAt index] at hc
            simp only [mapValEntry] at hc
            exact (Nat.add_right_cancel hc).symm
        simp only [runTimedFrom, treePop, hp, hpMap, hchildrenPop, mapValEntry,
          List.map_cons, Option.map_some, List.cons.injEq]
        exact ⟨congrArg some hcolor.symm, ih rest children' hgood' honly'⟩

theorem injectiveRootTimedEquivLeaf {α : Type} [DecidableEq α]
    (rank color : α → Nat) (hinj : Function.Injective color)
    (assign tail : α → Path) (topologies : List Topology)
    (hassign : ∀ pkt, assign pkt = .node (color pkt) (rank pkt) (tail pkt))
    (hvalidTail : ∀ pkt, pathOkAt topologies (color pkt) (tail pkt) = true) :
    TimedEquivOn (fun _ : α => True) (fun pkt => .leaf (rank pkt))
      (emptyTree .leaf) assign (emptyTree (.node topologies)) := by
  intro ops hops
  apply runTimedFrom_leaf_routed rank color hinj assign tail topologies hassign
    hvalidTail ops [] (emptyForest topologies)
  · exact emptyTree_good (.node topologies)
  · exact routedOnly_emptyForest color topologies

theorem timedEquivOn_symm {α : Type} {P : α → Prop}
    {assign₁ assign₂ : α → Path} {tree₁ tree₂ : Tree α}
    (h : TimedEquivOn P assign₁ tree₁ assign₂ tree₂) :
    TimedEquivOn P assign₂ tree₂ assign₁ tree₁ := by
  intro ops hops
  exact (h ops hops).symm

theorem timedEquivOn_trans {α : Type} {P : α → Prop}
    {assign₁ assign₂ assign₃ : α → Path} {tree₁ tree₂ tree₃ : Tree α}
    (h₁₂ : TimedEquivOn P assign₁ tree₁ assign₂ tree₂)
    (h₂₃ : TimedEquivOn P assign₂ tree₂ assign₃ tree₃) :
    TimedEquivOn P assign₁ tree₁ assign₃ tree₃ := by
  intro ops hops
  exact (h₁₂ ops hops).trans (h₂₃ ops hops)

def pathChild : Path → Nat
  | .leaf _ => 0
  | .node child _ _ => child

def pathTail : Path → Path
  | .leaf _ => .leaf 0
  | .node _ _ tail => tail

theorem path_eq_node_of_ok (topologies : List Topology) (path : Path)
    (h : pathOk (.node topologies) path = true) :
    path = .node (pathChild path) (pathHeadRank path) (pathTail path) := by
  cases path with
  | leaf rank => simp [pathOk] at h
  | node child rank tail => rfl

theorem pathOkAt_exists (topologies : List Topology) (child : Nat) (path : Path)
    (h : pathOkAt topologies child path = true) :
    ∃ topology, ListAt topology topologies child ∧ pathOk topology path = true := by
  cases topologies with
  | nil => simp [pathOkAt] at h
  | cons topology topologies =>
    cases child with
    | zero => exact ⟨topology, .zero topologies, h⟩
    | succ child =>
      obtain ⟨target, hat, hvalid⟩ := pathOkAt_exists topologies child path h
      exact ⟨target, .succ topology hat, hvalid⟩

theorem fin_two_eq_zero_or_one (pkt : Fin 2) : pkt = 0 ∨ pkt = 1 := by
  refine Fin.cases (Or.inl rfl) (fun pkt₁ => ?_) pkt
  have hpkt₁ : pkt₁ = 0 := Subsingleton.elim _ _
  subst pkt₁
  exact Or.inr rfl

theorem fin_two_injective_of_ne (color : Fin 2 → Nat)
    (hne : color 0 ≠ color 1) : Function.Injective color := by
  intro x y hxy
  rcases fin_two_eq_zero_or_one x with rfl | rfl <;>
    rcases fin_two_eq_zero_or_one y with rfl | rfl
  · rfl
  · exact False.elim (hne hxy)
  · exact False.elim (hne hxy.symm)
  · rfl

mutual
  def topologySize : Topology → Nat
    | .leaf => 1
    | .node topologies => topologyListSize topologies + 1

  def topologyListSize : List Topology → Nat
    | [] => 0
    | topology :: topologies =>
        topologySize topology + topologyListSize topologies + 1
end

theorem ListAt.topologySize_lt {topology : Topology}
    {topologies : List Topology} {index : Nat}
    (h : ListAt topology topologies index) :
    topologySize topology < topologyListSize topologies := by
  induction h with
  | zero tail =>
    simp only [topologyListSize]
    omega
  | succ head h ih =>
    simp only [topologyListSize]
    omega

theorem normalizeFinTwoTimed (topology : Topology) (assign : Fin 2 → Path)
      (hvalid : ∀ pkt, pathOk topology (assign pkt) = true) :
      ∃ rank : Fin 2 → Nat,
        TimedEquivOn (fun _ : Fin 2 => True) assign (emptyTree topology)
          (leafAssign rank) (emptyTree .leaf) := by
    cases htopology : topology with
    | leaf =>
      let rank : Fin 2 → Nat := fun pkt => pathHeadRank (assign pkt)
      have hassign : assign = leafAssign rank := by
        funext pkt
        have hp := hvalid pkt
        rw [htopology] at hp
        exact path_eq_leaf_of_ok (assign pkt) hp
      refine ⟨rank, ?_⟩
      intro ops hops
      rw [hassign]
    | node topologies =>
      let color : Fin 2 → Nat := fun pkt => pathChild (assign pkt)
      let rank : Fin 2 → Nat := fun pkt => pathHeadRank (assign pkt)
      let tail : Fin 2 → Path := fun pkt => pathTail (assign pkt)
      have hassignNode : ∀ pkt,
          assign pkt = .node (color pkt) (rank pkt) (tail pkt) := by
        intro pkt
        have hp := hvalid pkt
        rw [htopology] at hp
        exact path_eq_node_of_ok topologies (assign pkt) hp
      have hvalidTail : ∀ pkt,
          pathOkAt topologies (color pkt) (tail pkt) = true := by
        intro pkt
        have hp := hvalid pkt
        rw [htopology, hassignNode pkt] at hp
        exact hp
      by_cases heq : color 0 = color 1
      · obtain ⟨childTopology, hat, hvalidZero⟩ :=
          pathOkAt_exists topologies (color 0) (tail 0) (hvalidTail 0)
        have hcolor : ∀ pkt, color pkt = color 0 := by
          intro pkt
          rcases fin_two_eq_zero_or_one pkt with rfl | rfl
          · rfl
          · exact heq.symm
        have hvalidChild : ∀ pkt, pathOk childTopology (tail pkt) = true := by
          intro pkt
          rw [← hat.pathOkAt]
          have hp := hvalidTail pkt
          rw [hcolor pkt] at hp
          exact hp
        have hdec : topologySize childTopology < topologySize topology := by
          rw [htopology]
          simp only [topologySize]
          exact Nat.lt_trans hat.topologySize_lt (Nat.lt_succ_self _)
        obtain ⟨childRank, hchild⟩ :=
          normalizeFinTwoTimed childTopology tail hvalidChild
        have hunary : TimedEquivOn (fun _ : Fin 2 => True) assign
            (emptyTree (.node topologies)) tail (emptyTree childTopology) :=
          unaryNodeTimedEquiv_empty assign tail rank (color 0) childTopology
            topologies hat (by
              intro pkt hp
              rw [hassignNode pkt, hcolor pkt]) (by
              intro pkt hp
              exact hvalidChild pkt)
        exact ⟨childRank, timedEquivOn_trans hunary hchild⟩
      · have hinj : Function.Injective color := fin_two_injective_of_ne color heq
        have hleaf := injectiveRootTimedEquivLeaf rank color hinj assign tail
          topologies hassignNode hvalidTail
        exact ⟨rank, timedEquivOn_symm hleaf⟩
termination_by topologySize topology
decreasing_by exact hdec



def timedToQOp {α : Type} : TimedOp α → PifoGeneral.QOp α
  | .push pkt arr => .push pkt arr
  | .pop => .pop

def timedToQOps {α : Type} (ops : List (TimedOp α)) : List (PifoGeneral.QOp α) :=
  ops.map timedToQOp

def TimedOpsAbove {α : Type} : Nat → List (TimedOp α) → Prop
  | _, [] => True
  | bound, .push _ arr :: ops => bound < arr ∧ TimedOpsAbove arr ops
  | bound, .pop :: ops => TimedOpsAbove bound ops

theorem timedToQOps_ok {α : Type} {bound : Nat} {ops : List (TimedOp α)}
    (hops : TimedOpsAbove bound ops) :
    PifoGeneral.OkOps bound (timedToQOps ops) := by
  induction ops generalizing bound with
  | nil => trivial
  | cons op ops ih =>
    cases op with
    | push pkt arr =>
      exact ⟨hops.1, ih hops.2⟩
    | pop =>
      exact ih hops

theorem timedOpsFrom_above {k : Nat} (bound : Nat) (ops : List (Op k)) :
    TimedOpsAbove bound (timedOpsFrom bound ops) := by
  induction ops generalizing bound with
  | nil => trivial
  | cons op ops ih =>
    cases op with
    | push pkt => exact ⟨by omega, ih (bound + 1)⟩
    | pop => exact ih bound

theorem rankAgree_of_le_iff {α : Type} (rank₁ rank₂ : α → Nat) (x y : α)
    (hxy : rank₁ x ≤ rank₁ y ↔ rank₂ x ≤ rank₂ y)
    (hyx : rank₁ y ≤ rank₁ x ↔ rank₂ y ≤ rank₂ x) :
    PifoGeneral.Agree rank₁ rank₂ x y := by
  constructor <;> omega

theorem qrun_timedToQOps {α : Type} (rank : α → Nat) :
    ∀ (ops : List (TimedOp α)) (q : Queue α),
      PifoGeneral.qrun rank q (timedToQOps ops) =
        runTimedFrom (fun pkt => .leaf (rank pkt)) (.leaf q) ops := by
  intro ops
  induction ops with
  | nil => intro q; rfl
  | cons op ops ih =>
    intro q
    cases op with
    | push pkt arr =>
      simp only [timedToQOps, List.map_cons, timedToQOp, PifoGeneral.qrun,
        runTimedFrom, treePush]
      exact ih (q ++ [⟨pkt, rank pkt, arr⟩])
    | pop =>
      simp only [timedToQOps, List.map_cons, timedToQOp, PifoGeneral.qrun,
        runTimedFrom, treePop]
      cases hpop : qpop q with
      | none =>
        exact congrArg (fun output => none :: output) (ih q)
      | some result =>
        obtain ⟨entry, rest⟩ := result
        exact congrArg (fun output => some entry.val :: output) (ih rest)

theorem coloredLeafTimedCongruence {α γ : Type} [DecidableEq α]
    (rank₁ rank₂ : α → Nat) (color : α → γ)
    (hagrees : ∀ x y, color x ≠ color y →
      PifoGeneral.Agree rank₁ rank₂ x y)
    (ops : List (TimedOp α)) (bound : Nat) (hops : TimedOpsAbove bound ops) :
    (runTimedFrom (fun pkt => .leaf (rank₁ pkt)) (emptyTree .leaf) ops).map
        (Option.map color) =
      (runTimedFrom (fun pkt => .leaf (rank₂ pkt)) (emptyTree .leaf) ops).map
        (Option.map color) := by
  have hcolored := PifoGeneral.colored_congruence hagrees
    (timedToQOps ops) bound [] []
    (PifoGeneral.coupled_nil rank₁ rank₂)
    (by intro p hp; simp at hp)
    (by intro p hp; simp at hp)
    (timedToQOps_ok hops)
  change
    (runTimedFrom (fun pkt => .leaf (rank₁ pkt)) (.leaf []) ops).map
        (Option.map color) =
      (runTimedFrom (fun pkt => .leaf (rank₂ pkt)) (.leaf []) ops).map
        (Option.map color)
  simpa [qrun_timedToQOps, PifoGeneral.qstate] using hcolored

def rankLeafScheduler {k : Nat} (rank : Fin k → Nat) : Scheduler k :=
  ⟨.leaf, leafAssign rank⟩

theorem rankLeaf_flush_pair {k : Nat} (rank : Fin k → Nat) (x y : Fin k) :
    run (rankLeafScheduler rank) (flushOps [x, y]) =
      if rank x ≤ rank y then [some x, some y] else [some y, some x] := by
  let ex : Entry (Fin k) := ⟨x, rank x, 1⟩
  let ey : Entry (Fin k) := ⟨y, rank y, 2⟩
  by_cases h : rank x ≤ rank y
  · have hb : better ex ey = true := by simp [better, ex, ey]; omega
    have hq₁ : qpop [ex, ey] = some (ex, [ey]) := by simp [qpop, hb]
    have hq₂ : qpop [ey] = some (ey, []) := by simp [qpop]
    simp [run, flushOps, rankLeafScheduler, leafAssign, emptyTree, runFrom,
      treePush, treePop, ex, ey, hq₁, hq₂, h]
  · have hb : better ex ey = false := by simp [better, ex, ey]; omega
    have hq₁ : qpop [ex, ey] = some (ey, [ex]) := by simp [qpop, hb]
    have hq₂ : qpop [ex] = some (ex, []) := by simp [qpop]
    simp [run, flushOps, rankLeafScheduler, leafAssign, emptyTree, runFrom,
      treePush, treePop, ex, ey, hq₁, hq₂, h]

theorem rankLeaf_interEquiv_of_flush {k : Nat} (rank₁ rank₂ : Fin k → Nat)
    (hflush : flushEquiv (rankLeafScheduler rank₁) (rankLeafScheduler rank₂)) :
    interEquiv (rankLeafScheduler rank₁) (rankLeafScheduler rank₂) := by
  have hle : ∀ x y, x ≠ y →
      (rank₁ x ≤ rank₁ y ↔ rank₂ x ≤ rank₂ y) := by
    intro x y hxy
    have hrun := hflush [x, y]
    rw [rankLeaf_flush_pair, rankLeaf_flush_pair] at hrun
    by_cases h₁ : rank₁ x ≤ rank₁ y <;>
      by_cases h₂ : rank₂ x ≤ rank₂ y
    · exact ⟨fun _ => h₂, fun _ => h₁⟩
    · simp [h₁, h₂] at hrun
      exact False.elim (hxy hrun.1)
    · simp [h₁, h₂] at hrun
      exact False.elim (hxy hrun.1.symm)
    · exact ⟨fun h => False.elim (h₁ h), fun h => False.elim (h₂ h)⟩
  have hagrees : ∀ x y : Fin k, x ≠ y →
      PifoGeneral.Agree rank₁ rank₂ x y := by
    intro x y hxy
    exact rankAgree_of_le_iff rank₁ rank₂ x y (hle x y hxy)
      (hle y x hxy.symm)
  intro ops
  unfold run
  rw [runFrom_eq_runTimedFrom, runFrom_eq_runTimedFrom]
  change runTimedFrom (fun pkt => .leaf (rank₁ pkt)) (emptyTree .leaf)
      (timedOpsFrom 0 ops) =
    runTimedFrom (fun pkt => .leaf (rank₂ pkt)) (emptyTree .leaf)
      (timedOpsFrom 0 ops)
  have hrun := coloredLeafTimedCongruence rank₁ rank₂ (fun pkt => pkt)
    hagrees (timedOpsFrom 0 ops) 0 (timedOpsFrom_above 0 ops)
  simpa using hrun

theorem interEquiv_symm {k : Nat} {S₁ S₂ : Scheduler k}
    (h : interEquiv S₁ S₂) : interEquiv S₂ S₁ := by
  intro ops
  exact (h ops).symm

theorem interEquiv_trans {k : Nat} {S₁ S₂ S₃ : Scheduler k}
    (h₁₂ : interEquiv S₁ S₂) (h₂₃ : interEquiv S₂ S₃) : interEquiv S₁ S₃ := by
  intro ops
  exact (h₁₂ ops).trans (h₂₃ ops)

theorem normalizeSchedulerFinTwo (S : Scheduler 2) (hvalid : S.Valid) :
    ∃ rank : Fin 2 → Nat, interEquiv S (rankLeafScheduler rank) := by
  obtain ⟨rank, htimed⟩ := normalizeFinTwoTimed S.topo S.assign hvalid
  refine ⟨rank, ?_⟩
  apply interEquiv_of_timedEquiv_empty
  simpa [rankLeafScheduler] using htimed

theorem interEquiv_fin_two (S₁ S₂ : Scheduler 2)
    (hvalid₁ : S₁.Valid) (hvalid₂ : S₂.Valid)
    (hflush : flushEquiv S₁ S₂) : interEquiv S₁ S₂ := by
  obtain ⟨rank₁, hnorm₁⟩ := normalizeSchedulerFinTwo S₁ hvalid₁
  obtain ⟨rank₂, hnorm₂⟩ := normalizeSchedulerFinTwo S₂ hvalid₂
  have hflushLeaf : flushEquiv (rankLeafScheduler rank₁)
      (rankLeafScheduler rank₂) := by
    intro word
    exact (hnorm₁ (flushOps word)).symm.trans
      ((hflush word).trans (hnorm₂ (flushOps word)))
  have hleaf : interEquiv (rankLeafScheduler rank₁) (rankLeafScheduler rank₂) :=
    rankLeaf_interEquiv_of_flush rank₁ rank₂ hflushLeaf
  exact interEquiv_trans hnorm₁ (interEquiv_trans hleaf (interEquiv_symm hnorm₂))

def selectorQueue {α : Type} (rank color : α → Nat)
    (state : List (α × Nat)) : Queue Nat :=
  (PifoGeneral.qstate rank state).map (mapValEntry color)

theorem selectorQueue_push {α : Type} (rank color : α → Nat)
    (state : List (α × Nat)) (pkt : α) (arr : Nat) :
    selectorQueue rank color state ++ [⟨color pkt, rank pkt, arr⟩] =
      selectorQueue rank color (state ++ [(pkt, arr)]) := by
  simp [selectorQueue, PifoGeneral.qstate, List.map_append, mapValEntry,
    PifoGeneral.embedP]

theorem qpop_selectorQueue {α : Type} (rank color : α → Nat)
    {state : List (α × Nat)} {picked : α × Nat}
    {rest : List (α × Nat)}
    (hpop : PifoStatement.qpop (PifoGeneral.qstate rank state) =
      some (PifoGeneral.embedP rank picked, PifoGeneral.qstate rank rest)) :
    PifoStatement.qpop (selectorQueue rank color state) =
      some (⟨color picked.1, rank picked.1, picked.2⟩,
        selectorQueue rank color rest) := by
  rcases qpop_mapVal color (PifoGeneral.qstate rank state) with
    ⟨hn, hn'⟩ | ⟨entry, queue, hp, hpmap⟩
  · rw [hpop] at hn
    contradiction
  · rw [hpop] at hp
    obtain ⟨rfl, rfl⟩ := Option.some.inj hp
    simpa [selectorQueue, mapValEntry, PifoGeneral.embedP] using hpmap

theorem coloredNodeTimedCongruence {α γ : Type} [DecidableEq α]
    (rank₁ rank₂ color : α → Nat) (outputColor : α → γ)
    (tail : α → Path)
    (hagrees : ∀ x y, color x ≠ color y →
      PifoGeneral.Agree rank₁ rank₂ x y) :
    ∀ (ops : List (TimedOp α)) (bound : Nat)
      (state₁ state₂ : List (α × Nat)) (children : List (Tree α)),
      PifoGeneral.Coupled rank₁ rank₂ state₁ state₂ →
      PifoGeneral.AllBelow state₁ bound →
      PifoGeneral.AllBelow state₂ bound →
      TimedOpsAbove bound ops →
      (runTimedFrom
          (fun pkt => .node (color pkt) (rank₁ pkt) (tail pkt))
          (.node (selectorQueue rank₁ color state₁) children) ops).map
          (Option.map outputColor) =
        (runTimedFrom
          (fun pkt => .node (color pkt) (rank₂ pkt) (tail pkt))
          (.node (selectorQueue rank₂ color state₂) children) ops).map
          (Option.map outputColor) := by
  intro ops
  induction ops with
  | nil => intros; rfl
  | cons op ops ih =>
    intro bound state₁ state₂ children hcoupled hbelow₁ hbelow₂ hops
    cases op with
    | push pkt arr =>
      obtain ⟨harr, hops⟩ := hops
      simp only [runTimedFrom, treePush]
      rw [selectorQueue_push, selectorQueue_push]
      exact ih arr (state₁ ++ [(pkt, arr)]) (state₂ ++ [(pkt, arr)])
        (treePushAt pkt arr children (color pkt) (tail pkt))
        (PifoGeneral.coupled_push hcoupled hbelow₁ hbelow₂ harr)
        (PifoGeneral.allBelow_push hbelow₁ harr)
        (PifoGeneral.allBelow_push hbelow₂ harr) hops
    | pop =>
      simp only [runTimedFrom]
      cases state₁ with
      | nil =>
        cases state₂ with
        | nil =>
          simp only [selectorQueue, PifoGeneral.qstate, List.map, treePop, qpop,
            List.map_cons, Option.map_none, List.cons.injEq, true_and]
          exact ih bound [] [] children hcoupled hbelow₁ hbelow₂ hops
        | cons picked state₂ =>
          have hlen := hcoupled.len
          simp at hlen
      | cons picked₁ rest₁ =>
        cases state₂ with
        | nil =>
          have hlen := hcoupled.len
          simp at hlen
        | cons picked₂ rest₂ =>
          obtain ⟨minimum₁, hmem₁, hpop₁, hmin₁⟩ :=
            PifoGeneral.qpop_state rank₁ picked₁ rest₁ hcoupled.d1
          obtain ⟨minimum₂, hmem₂, hpop₂, hmin₂⟩ :=
            PifoGeneral.qpop_state rank₂ picked₂ rest₂ hcoupled.d2
          have hlinked : PifoGeneral.Linked (PifoGeneral.Dis rank₁ rank₂)
              minimum₁.1 minimum₂.1 :=
            PifoGeneral.min_block hcoupled hmem₁ hmem₂ hmin₁ hmin₂
          have hcolor : color minimum₁.1 = color minimum₂.1 :=
            PifoGeneral.linked_colEq hagrees hlinked
          have hroot₁ := qpop_selectorQueue rank₁ color hpop₁
          have hroot₂ := qpop_selectorQueue rank₂ color hpop₂
          rw [treePop, hroot₁, treePop, hroot₂, hcolor]
          cases hchild : treePopAt children (color minimum₂.1) with
          | none =>
            simp only [hchild, List.map_cons, Option.map_none, List.cons.injEq,
              true_and]
            exact ih bound (picked₁ :: rest₁) (picked₂ :: rest₂)
              children hcoupled hbelow₁ hbelow₂ hops
          | some result =>
            obtain ⟨pkt, children'⟩ := result
            simp only [hchild, List.map_cons, Option.map_some, List.cons.injEq,
              true_and]
            exact ih bound
              (PifoGeneral.removeArr (picked₁ :: rest₁) minimum₁.2)
              (PifoGeneral.removeArr (picked₂ :: rest₂) minimum₂.2)
              children'
              (PifoGeneral.coupled_pop hcoupled hmem₁ hmem₂ hmin₁ hmin₂
                hlinked)
              (PifoGeneral.allBelow_removeArr hbelow₁)
              (PifoGeneral.allBelow_removeArr hbelow₂) hops

theorem coloredNodeInterEquiv {k : Nat}
    (rank₁ rank₂ color : Fin k → Nat) (tail : Fin k → Path)
    (topologies : List Topology)
    (hagrees : ∀ x y, color x ≠ color y →
      PifoGeneral.Agree rank₁ rank₂ x y) :
    interEquiv
      ⟨.node topologies, fun pkt => .node (color pkt) (rank₁ pkt) (tail pkt)⟩
      ⟨.node topologies, fun pkt => .node (color pkt) (rank₂ pkt) (tail pkt)⟩ := by
  intro ops
  unfold run
  rw [runFrom_eq_runTimedFrom, runFrom_eq_runTimedFrom]
  change runTimedFrom
      (fun pkt => .node (color pkt) (rank₁ pkt) (tail pkt))
      (.node [] (emptyForest topologies)) (timedOpsFrom 0 ops) =
    runTimedFrom
      (fun pkt => .node (color pkt) (rank₂ pkt) (tail pkt))
      (.node [] (emptyForest topologies)) (timedOpsFrom 0 ops)
  have hrun := coloredNodeTimedCongruence rank₁ rank₂ color
    (fun pkt : Fin k => pkt) tail hagrees (timedOpsFrom 0 ops) 0 [] []
    (emptyForest topologies) (PifoGeneral.coupled_nil rank₁ rank₂)
    (by intro p hp; simp at hp) (by intro p hp; simp at hp)
    (timedOpsFrom_above 0 ops)
  simpa [selectorQueue, PifoGeneral.qstate] using hrun



structure NormalRoot (k : Nat) where
  topologies : List Topology
  color : Fin k → Nat
  rank : Fin k → Nat
  tail : Fin k → Path
  hvalidTail : ∀ pkt, pathOkAt topologies (color pkt) (tail pkt) = true
  nonconstant : ∃ x y, color x ≠ color y

def NormalRoot.assign {k : Nat} (root : NormalRoot k) (pkt : Fin k) : Path :=
  .node (root.color pkt) (root.rank pkt) (root.tail pkt)

def NormalRoot.scheduler {k : Nat} (root : NormalRoot k) : Scheduler k :=
  ⟨.node root.topologies, root.assign⟩

theorem NormalRoot.valid {k : Nat} (root : NormalRoot k) : root.scheduler.Valid := by
  intro pkt
  exact root.hvalidTail pkt

theorem normalizeRootTimed (k : Nat) (hk : 2 ≤ k) (topology : Topology)
    (assign : Fin k → Path) (hvalid : ∀ pkt, pathOk topology (assign pkt) = true) :
    ∃ root : NormalRoot k,
      TimedEquivOn (fun _ : Fin k => True) assign (emptyTree topology)
        root.assign (emptyTree (.node root.topologies)) := by
  cases htopology : topology with
  | leaf =>
    let rank : Fin k → Nat := fun pkt => pathHeadRank (assign pkt)
    have hassign : assign = leafAssign rank := by
      funext pkt
      have hp := hvalid pkt
      rw [htopology] at hp
      exact path_eq_leaf_of_ok (assign pkt) hp
    let root : NormalRoot k :=
      { topologies := List.replicate k .leaf
        color := Fin.val
        rank := rank
        tail := fun _ => .leaf 0
        hvalidTail := fun pkt => by
          rw [(listAt_replicate_fin Topology.leaf pkt).pathOkAt]
          rfl
        nonconstant := by
          let zero : Fin k := ⟨0, by omega⟩
          let one : Fin k := ⟨1, by omega⟩
          exact ⟨zero, one, by simp [zero, one]⟩ }
    refine ⟨root, ?_⟩
    have hrootAssign : root.assign = explodedAssign rank := by
      funext pkt
      rfl
    rw [hassign, hrootAssign]
    exact leafExplodedTimedEquiv rank
  | node topologies =>
    let color : Fin k → Nat := fun pkt => pathChild (assign pkt)
    let rank : Fin k → Nat := fun pkt => pathHeadRank (assign pkt)
    let tail : Fin k → Path := fun pkt => pathTail (assign pkt)
    have hassignNode : ∀ pkt,
        assign pkt = .node (color pkt) (rank pkt) (tail pkt) := by
      intro pkt
      have hp := hvalid pkt
      rw [htopology] at hp
      exact path_eq_node_of_ok topologies (assign pkt) hp
    have hvalidTail : ∀ pkt,
        pathOkAt topologies (color pkt) (tail pkt) = true := by
      intro pkt
      have hp := hvalid pkt
      rw [htopology, hassignNode pkt] at hp
      exact hp
    by_cases hnonconstant : ∃ x y, color x ≠ color y
    · let root : NormalRoot k :=
        { topologies := topologies
          color := color
          rank := rank
          tail := tail
          hvalidTail := hvalidTail
          nonconstant := hnonconstant }
      refine ⟨root, ?_⟩
      have hassignEq : assign = root.assign := by
        funext pkt
        exact hassignNode pkt
      rw [hassignEq]
      intro ops hops
      rfl
    · have hcolor : ∀ pkt, color pkt = color ⟨0, by omega⟩ := by
        intro pkt
        apply Classical.byContradiction
        intro hne
        exact hnonconstant ⟨pkt, ⟨0, by omega⟩, hne⟩
      let first : Fin k := ⟨0, by omega⟩
      obtain ⟨childTopology, hat, hvalidFirst⟩ :=
        pathOkAt_exists topologies (color first) (tail first) (hvalidTail first)
      have hvalidChild : ∀ pkt, pathOk childTopology (tail pkt) = true := by
        intro pkt
        rw [← hat.pathOkAt]
        have hp := hvalidTail pkt
        rw [hcolor pkt] at hp
        exact hp
      have hdec : topologySize childTopology < topologySize topology := by
        rw [htopology]
        simp only [topologySize]
        exact Nat.lt_trans hat.topologySize_lt (Nat.lt_succ_self _)
      obtain ⟨root, hroot⟩ :=
        normalizeRootTimed k hk childTopology tail hvalidChild
      have hunary : TimedEquivOn (fun _ : Fin k => True) assign
          (emptyTree (.node topologies)) tail (emptyTree childTopology) :=
        unaryNodeTimedEquiv_empty assign tail rank (color first) childTopology
          topologies hat (by
            intro pkt hp
            rw [hassignNode pkt, hcolor pkt]) (by
            intro pkt hp
            exact hvalidChild pkt)
      exact ⟨root, timedEquivOn_trans hunary hroot⟩
termination_by topologySize topology
decreasing_by exact hdec

theorem normalizeRootScheduler {k : Nat} (hk : 2 ≤ k) (S : Scheduler k)
    (hvalid : S.Valid) :
    ∃ root : NormalRoot k, interEquiv S root.scheduler := by
  obtain ⟨root, htimed⟩ := normalizeRootTimed k hk S.topo S.assign hvalid
  exact ⟨root, interEquiv_of_timedEquiv_empty S root.scheduler htimed⟩

theorem finRange_nodup (k : Nat) : (List.finRange k).Nodup := by
  rw [List.nodup_iff_pairwise_ne, List.pairwise_iff_getElem]
  intro i j hi hj hij heq
  have hval := congrArg Fin.val heq
  simp [List.finRange] at hval
  omega

theorem List.Nodup.filterBool {α : Type} (p : α → Bool) {values : List α}
    (h : values.Nodup) : (values.filter p).Nodup := by
  induction values with
  | nil => trivial
  | cons value values ih =>
    rw [List.nodup_cons] at h
    cases hp : p value with
    | false =>
      simp only [List.filter, hp]
      exact ih h.2
    | true =>
      simp only [List.filter, hp, List.nodup_cons]
      exact ⟨fun hmem => h.1 (List.mem_filter.mp hmem).1, ih h.2⟩

theorem List.Nodup.get_injective' {α : Type} {values : List α}
    (h : values.Nodup) : Function.Injective values.get := by
  intro i j heq
  have hpair := List.nodup_iff_pairwise_ne.mp h
  rw [List.pairwise_iff_getElem] at hpair
  by_cases hij : i.val < j.val
  · exact False.elim ((hpair i.val j.val i.isLt j.isLt hij) (by
      simpa [List.get_eq_getElem] using heq))
  · by_cases hji : j.val < i.val
    · exact False.elim ((hpair j.val i.val j.isLt i.isLt hji) (by
        simpa [List.get_eq_getElem] using heq.symm))
    · apply Fin.ext
      omega

def fiberList {k : Nat} (color : Fin k → Nat) (index : Nat) : List (Fin k) :=
  (List.finRange k).filter (fun pkt => color pkt == index)

def fiberEmbedding {k : Nat} (color : Fin k → Nat) (index : Nat) :
    Fin (fiberList color index).length → Fin k :=
  (fiberList color index).get

theorem fiberList_nodup {k : Nat} (color : Fin k → Nat) (index : Nat) :
    (fiberList color index).Nodup := by
  exact List.Nodup.filterBool _ (finRange_nodup k)

theorem fiberEmbedding_injective {k : Nat} (color : Fin k → Nat) (index : Nat) :
    Function.Injective (fiberEmbedding color index) := by
  exact List.Nodup.get_injective' (fiberList_nodup color index)

theorem fiberEmbedding_color {k : Nat} (color : Fin k → Nat) (index : Nat)
    (pkt : Fin (fiberList color index).length) :
    color (fiberEmbedding color index pkt) = index := by
  have hmem := List.get_mem (fiberList color index) pkt
  simp only [fiberList, List.mem_filter] at hmem
  exact beq_iff_eq.mp hmem.2

theorem mem_fiberList {k : Nat} (color : Fin k → Nat) (index : Nat) (pkt : Fin k) :
    pkt ∈ fiberList color index ↔ color pkt = index := by
  rw [fiberList, List.mem_filter]
  constructor
  · intro h
    exact beq_iff_eq.mp h.2
  · intro h
    exact ⟨List.mem_finRange pkt, beq_iff_eq.mpr h⟩

theorem filter_length_lt_of_mem_false {α : Type} (p : α → Bool)
    (values : List α) (value : α) (hmem : value ∈ values) (hp : p value = false) :
    (values.filter p).length < values.length := by
  induction values generalizing value with
  | nil => simp at hmem
  | cons head values ih =>
    simp only [List.mem_cons] at hmem
    cases hhead : p head with
    | true =>
      simp only [List.filter, hhead, List.length_cons]
      rcases hmem with rfl | hmem
      · rw [hhead] at hp
        contradiction
      · have hlt := ih value hmem hp
        omega
    | false =>
      simp only [List.filter, hhead, List.length_cons]
      rcases hmem with rfl | hmem
      · have hle := List.length_filter_le p values
        omega
      · have hlt := ih value hmem hp
        omega

theorem fiberList_length_lt {k : Nat} (color : Fin k → Nat)
    (hnonconstant : ∃ x y, color x ≠ color y) (member : Fin k) :
    (fiberList color (color member)).length < k := by
  have houtside : ∃ outside, color outside ≠ color member := by
    obtain ⟨x, y, hxy⟩ := hnonconstant
    by_cases hx : color x = color member
    · exact ⟨y, fun hy => hxy (hx.trans hy.symm)⟩
    · exact ⟨x, hx⟩
  obtain ⟨outside, houtside⟩ := houtside
  have hfalse : (color outside == color member) = false := by
    exact beq_eq_false_iff_ne.mpr houtside
  have hlt := filter_length_lt_of_mem_false
    (fun pkt : Fin k => color pkt == color member) (List.finRange k) outside
    (List.mem_finRange outside) hfalse
  simpa [fiberList] using hlt

noncomputable def NormalRoot.childTopology {k : Nat} (root : NormalRoot k)
    (member : Fin k) : Topology :=
  Classical.choose (pathOkAt_exists root.topologies (root.color member)
    (root.tail member) (root.hvalidTail member))

theorem NormalRoot.childTopology_at {k : Nat} (root : NormalRoot k)
    (member : Fin k) :
    ListAt (root.childTopology member) root.topologies (root.color member) :=
  (Classical.choose_spec (pathOkAt_exists root.topologies (root.color member)
    (root.tail member) (root.hvalidTail member))).1

noncomputable def NormalRoot.childScheduler {k : Nat} (root : NormalRoot k)
    (member : Fin k) :
    Scheduler (fiberList root.color (root.color member)).length :=
  ⟨root.childTopology member,
    fun pkt => root.tail (fiberEmbedding root.color (root.color member) pkt)⟩

theorem NormalRoot.childScheduler_valid {k : Nat} (root : NormalRoot k)
    (member : Fin k) : (root.childScheduler member).Valid := by
  intro pkt
  change pathOk (root.childTopology member)
    (root.tail (fiberEmbedding root.color (root.color member) pkt)) = true
  rw [← (root.childTopology_at member).pathOkAt]
  have hp := root.hvalidTail (fiberEmbedding root.color (root.color member) pkt)
  rw [fiberEmbedding_color root.color (root.color member) pkt] at hp
  exact hp

theorem NormalRoot.childAlphabet_lt {k : Nat} (root : NormalRoot k)
    (member : Fin k) :
    (fiberList root.color (root.color member)).length < k :=
  fiberList_length_lt root.color root.nonconstant member

noncomputable def NormalRoot.fullChildScheduler {k : Nat} (root : NormalRoot k)
    (member : Fin k) : Scheduler k :=
  ⟨root.childTopology member, root.tail⟩

theorem TimedOpsOn.mapTimedVal {α β : Type} {P : β → Prop}
    (embedding : α → β) (himage : ∀ pkt, P (embedding pkt)) :
    ∀ ops : List (TimedOp α), TimedOpsOn P (mapTimedVal embedding ops) := by
  intro ops
  induction ops with
  | nil => trivial
  | cons op ops ih =>
    cases op with
    | push pkt arr => exact ⟨himage pkt, ih⟩
    | pop => exact ih

theorem NormalRoot.blockRun {k : Nat} (root : NormalRoot k) (member : Fin k)
    (ops : List (Op (fiberList root.color (root.color member)).length)) :
    run root.scheduler
        (mapOpsVal (fiberEmbedding root.color (root.color member)) ops) =
      (run (root.childScheduler member) ops).map
        (Option.map (fiberEmbedding root.color (root.color member))) := by
  let embedding := fiberEmbedding root.color (root.color member)
  let child := root.fullChildScheduler member
  have hunary : TimedEquivOn
      (fun pkt : Fin k => root.color pkt = root.color member)
      root.assign (emptyTree (.node root.topologies)) root.tail
        (emptyTree (root.childTopology member)) :=
    unaryNodeTimedEquiv_empty root.assign root.tail root.rank (root.color member)
      (root.childTopology member) root.topologies (root.childTopology_at member) (by
        intro pkt hp
        simp only [NormalRoot.assign]
        rw [hp]) (by
        intro pkt hp
        rw [← (root.childTopology_at member).pathOkAt]
        have hv := root.hvalidTail pkt
        rw [hp] at hv
        exact hv)
  have hopsOn : TimedOpsOn (fun pkt : Fin k => root.color pkt = root.color member)
      (mapTimedVal embedding (timedOpsFrom 0 ops)) := by
    apply TimedOpsOn.mapTimedVal embedding
    intro pkt
    exact fiberEmbedding_color root.color (root.color member) pkt
  have htimed := hunary (mapTimedVal embedding (timedOpsFrom 0 ops)) hopsOn
  have hroot : run root.scheduler (mapOpsVal embedding ops) =
      run child (mapOpsVal embedding ops) := by
    unfold run
    rw [runFrom_eq_runTimedFrom, runFrom_eq_runTimedFrom,
      timedOpsFrom_mapOpsVal]
    exact htimed
  rw [hroot, run_schedulerComap]
  rfl

theorem NormalRoot.colorRun {k : Nat} (root : NormalRoot k) (ops : List (Op k)) :
    (run (rankLeafScheduler root.rank) ops).map (Option.map root.color) =
      (run root.scheduler ops).map (Option.map root.color) := by
  unfold run
  rw [runFrom_eq_runTimedFrom, runFrom_eq_runTimedFrom]
  apply runTimedFrom_rootColor root.rank root.color root.assign root.tail
    root.topologies
  · intro pkt
    rfl
  · exact root.hvalidTail
  · exact emptyTree_good (.node root.topologies)
  · exact routedOnly_emptyForest root.color root.topologies

def pairEmbedding {k : Nat} (x y : Fin k) : Fin 2 → Fin k :=
  fun index => Fin.cases x (fun _ => y) index

theorem pairEmbedding_zero {k : Nat} (x y : Fin k) : pairEmbedding x y 0 = x := rfl

theorem pairEmbedding_one {k : Nat} (x y : Fin k) : pairEmbedding x y 1 = y := rfl

theorem NormalRoot.pairRestrictionInterLeaf {k : Nat} (root : NormalRoot k)
    (x y : Fin k) (hcolor : root.color x ≠ root.color y) :
    interEquiv (schedulerComap root.scheduler (pairEmbedding x y))
      (rankLeafScheduler (fun i => root.rank (pairEmbedding x y i))) := by
  let color : Fin 2 → Nat := fun i => root.color (pairEmbedding x y i)
  let rank : Fin 2 → Nat := fun i => root.rank (pairEmbedding x y i)
  let tail : Fin 2 → Path := fun i => root.tail (pairEmbedding x y i)
  have hinj : Function.Injective color := fin_two_injective_of_ne color (by
    exact hcolor)
  have hleaf := injectiveRootTimedEquivLeaf rank color hinj
    (schedulerComap root.scheduler (pairEmbedding x y)).assign tail root.topologies
    (by intro i; rfl) (by
      intro i
      exact root.hvalidTail (pairEmbedding x y i))
  apply interEquiv_of_timedEquiv_empty
  exact timedEquivOn_symm hleaf

theorem NormalRoot.flushPair_cross {k : Nat} (root : NormalRoot k)
    (x y : Fin k) (hcolor : root.color x ≠ root.color y) :
    run root.scheduler (flushOps [x, y]) =
      if root.rank x ≤ root.rank y then [some x, some y] else [some y, some x] := by
  have hxy : x ≠ y := fun h => hcolor (congrArg root.color h)
  let embedding := pairEmbedding x y
  let restricted := schedulerComap root.scheduler embedding
  let leaf := rankLeafScheduler (fun i => root.rank (embedding i))
  have hinter : interEquiv restricted leaf :=
    root.pairRestrictionInterLeaf x y hcolor
  have hmap := run_schedulerComap root.scheduler embedding (flushOps [0, 1])
  rw [mapOpsVal_flushOps] at hmap
  have hleaf := rankLeaf_flush_pair (fun i => root.rank (embedding i)) 0 1
  have hrestricted := hinter (flushOps [0, 1])
  rw [hleaf] at hrestricted
  rw [hrestricted] at hmap
  by_cases hrank : root.rank x ≤ root.rank y
  · simpa only [embedding, leaf, pairEmbedding_zero, pairEmbedding_one,
      hrank, if_pos, List.map_cons, List.map_nil, Option.map_some] using hmap
  · simpa [embedding, leaf, pairEmbedding_zero, pairEmbedding_one, hrank]
      using hmap

theorem crossRootRank_le_iff {k : Nat} (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler) (x y : Fin k)
    (hcross₁ : root₁.color x ≠ root₁.color y)
    (hcross₂ : root₂.color x ≠ root₂.color y) :
    root₁.rank x ≤ root₁.rank y ↔ root₂.rank x ≤ root₂.rank y := by
  have hrun := hflush [x, y]
  rw [root₁.flushPair_cross x y hcross₁,
    root₂.flushPair_cross x y hcross₂] at hrun
  have hxy : x ≠ y := fun h => hcross₁ (congrArg root₁.color h)
  by_cases h₁ : root₁.rank x ≤ root₁.rank y <;>
    by_cases h₂ : root₂.rank x ≤ root₂.rank y
  · exact ⟨fun _ => h₂, fun _ => h₁⟩
  · simp [h₁, h₂] at hrun
    exact False.elim (hxy hrun.1)
  · simp [h₁, h₂] at hrun
    exact False.elim (hxy hrun.1.symm)
  · exact ⟨fun h => False.elim (h₁ h), fun h => False.elim (h₂ h)⟩

theorem rankLeaf_flush_three_middle_first {k : Nat} (rank : Fin k → Nat)
    (x y z : Fin k) (hyz : rank y ≤ rank z) (hzx : rank z < rank x) :
    run (rankLeafScheduler rank) (flushOps [x, y, z]) =
      [some y, some z, some x] := by
  have hyx : rank y < rank x := by omega
  let S := rankLeafScheduler rank
  let ex : Entry (Fin k) := ⟨x, rank x, 1⟩
  let ey : Entry (Fin k) := ⟨y, rank y, 2⟩
  let ez : Entry (Fin k) := ⟨z, rank z, 3⟩
  have hyzBetter : better ey ez = true := by
    simp [better, ey, ez]
    omega
  have hxyBetter : better ex ey = false := by
    simp [better, ex, ey]
    omega
  have hxzBetter : better ex ez = false := by
    simp [better, ex, ez]
    omega
  have hq₁ : qpop [ex, ey, ez] = some (ey, [ex, ez]) := by
    simp [qpop, hyzBetter, hxyBetter]
  have hq₂ : qpop [ex, ez] = some (ez, [ex]) := by
    simp [qpop, hxzBetter]
  have hq₃ : qpop [ex] = some (ex, []) := by
    simp [qpop]
  change runFrom (rankLeafScheduler rank) 0 (.leaf [])
      [.push x, .push y, .push z, .pop, .pop, .pop] =
    [some y, some z, some x]
  simp only [runFrom, rankLeafScheduler, leafAssign, treePush]
  change runFrom (rankLeafScheduler rank) 3 (.leaf [ex, ey, ez])
    [.pop, .pop, .pop] = _
  simp only [runFrom, treePop, hq₁, hq₂, hq₃, ex, ey, ez]

theorem rankLeaf_flush_three_chain {k : Nat} (rank : Fin k → Nat)
    (x y z : Fin k) (hxy : rank x ≤ rank y) (hyz : rank y ≤ rank z) :
    run (rankLeafScheduler rank) (flushOps [x, y, z]) =
      [some x, some y, some z] := by
  let ex : Entry (Fin k) := ⟨x, rank x, 1⟩
  let ey : Entry (Fin k) := ⟨y, rank y, 2⟩
  let ez : Entry (Fin k) := ⟨z, rank z, 3⟩
  have hyzBetter : better ey ez = true := by
    simp [better, ey, ez]
    omega
  have hxyBetter : better ex ey = true := by
    simp [better, ex, ey]
    omega
  have hq₁ : qpop [ex, ey, ez] = some (ex, [ey, ez]) := by
    simp [qpop, hyzBetter, hxyBetter]
  have hq₂ : qpop [ey, ez] = some (ey, [ez]) := by
    simp [qpop, hyzBetter]
  have hq₃ : qpop [ez] = some (ez, []) := by
    simp [qpop]
  change runFrom (rankLeafScheduler rank) 0 (.leaf [])
      [.push x, .push y, .push z, .pop, .pop, .pop] =
    [some x, some y, some z]
  simp only [runFrom, rankLeafScheduler, leafAssign, treePush]
  change runFrom (rankLeafScheduler rank) 3 (.leaf [ex, ey, ez])
    [.pop, .pop, .pop] = _
  simp only [runFrom, treePop, hq₁, hq₂, hq₃, ex, ey, ez]

theorem rankLeaf_flush_three_last_first {k : Nat} (rank : Fin k → Nat)
    (x y z : Fin k) (hzx : rank z < rank x) (hxy : rank x ≤ rank y) :
    run (rankLeafScheduler rank) (flushOps [x, y, z]) =
      [some z, some x, some y] := by
  let ex : Entry (Fin k) := ⟨x, rank x, 1⟩
  let ey : Entry (Fin k) := ⟨y, rank y, 2⟩
  let ez : Entry (Fin k) := ⟨z, rank z, 3⟩
  have hzyBetter : better ey ez = false := by
    simp [better, ey, ez]
    omega
  have hzxBetter : better ex ez = false := by
    simp [better, ex, ez]
    omega
  have hxyBetter : better ex ey = true := by
    simp [better, ex, ey]
    omega
  have hq₁ : qpop [ex, ey, ez] = some (ez, [ex, ey]) := by
    simp [qpop, hzyBetter, hzxBetter]
  have hq₂ : qpop [ex, ey] = some (ex, [ey]) := by
    simp [qpop, hxyBetter]
  have hq₃ : qpop [ey] = some (ey, []) := by
    simp [qpop]
  change runFrom (rankLeafScheduler rank) 0 (.leaf [])
      [.push x, .push y, .push z, .pop, .pop, .pop] =
    [some z, some x, some y]
  simp only [runFrom, rankLeafScheduler, leafAssign, treePush]
  change runFrom (rankLeafScheduler rank) 3 (.leaf [ex, ey, ez])
    [.pop, .pop, .pop] = _
  simp only [runFrom, treePop, hq₁, hq₂, hq₃, ex, ey, ez]

def OnlyValues {α : Type} [DecidableEq α] (values : List α) (tree : Tree α) : Prop :=
  ∀ value, value ∉ values → packetValCount value tree = 0

theorem onlyValues_emptyTree {α : Type} [DecidableEq α] (values : List α)
    (topology : Topology) : OnlyValues values (emptyTree topology) := by
  intro value hmem
  exact packetValCount_emptyTree value topology

theorem onlyValues_treePush {α : Type} [DecidableEq α] (values : List α)
    (pkt : α) (arr : Nat) (topology : Topology) (tree : Tree α) (path : Path)
    (hgood : Good topology tree) (hvalid : pathOk topology path = true)
    (honly : OnlyValues values tree) :
    OnlyValues (pkt :: values) (treePush pkt arr tree path) := by
  intro value hmem
  simp only [List.mem_cons, not_or] at hmem
  rw [treePush_packetValCount pkt value arr topology tree path hgood hvalid,
    honly value hmem.2]
  have hne : pkt ≠ value := fun h => hmem.1 h.symm
  simp [hne]

theorem onlyValues_treePop {α : Type} [DecidableEq α] (values : List α)
    {tree : Tree α} {pkt : α} {tree' : Tree α} (honly : OnlyValues values tree)
    (hpop : treePop tree = some (pkt, tree')) :
    pkt ∈ values ∧ OnlyValues values tree' := by
  have hpkt : pkt ∈ values := Classical.byContradiction (fun hmem => by
    have hc := treePop_packetValCount hpop pkt
    rw [honly pkt hmem] at hc
    simp at hc)
  refine ⟨hpkt, ?_⟩
  intro value hmem
  have hc := treePop_packetValCount hpop value
  rw [honly value hmem] at hc
  have hne : pkt ≠ value := fun h => hmem (h ▸ hpkt)
  simp [hne] at hc
  exact hc.symm

theorem flush_three_outputs {k : Nat} (S : Scheduler k) (hvalid : S.Valid)
    (x y z : Fin k) :
    ∃ a b c, a ∈ [x, y, z] ∧ b ∈ [x, y, z] ∧ c ∈ [x, y, z] ∧
      run S (flushOps [x, y, z]) = [some a, some b, some c] := by
  let tree₀ : Tree (Fin k) := emptyTree S.topo
  let tree₁ := treePush x 1 tree₀ (S.assign x)
  let tree₂ := treePush y 2 tree₁ (S.assign y)
  let tree₃ := treePush z 3 tree₂ (S.assign z)
  have hgood₀ : Good S.topo tree₀ := emptyTree_good S.topo
  have hx := treePush_good_count x 1 S.topo tree₀ (S.assign x) hgood₀ (hvalid x)
  have hy := treePush_good_count y 2 S.topo tree₁ (S.assign y) hx.1 (hvalid y)
  have hz := treePush_good_count z 3 S.topo tree₂ (S.assign z) hy.1 (hvalid z)
  have hcount₃ : packetCount tree₃ = 3 := by
    have hcount₀ : packetCount tree₀ = 0 := by
      exact packetCount_emptyTree S.topo
    have hcount₁ : packetCount tree₁ = packetCount tree₀ + 1 := hx.2
    have hcount₂ : packetCount tree₂ = packetCount tree₁ + 1 := hy.2
    have hcount₃' : packetCount tree₃ = packetCount tree₂ + 1 := hz.2
    omega
  have honly₀ : OnlyValues [] tree₀ := onlyValues_emptyTree [] S.topo
  have honly₁ : OnlyValues [x] tree₁ :=
    onlyValues_treePush [] x 1 S.topo tree₀ (S.assign x) hgood₀ (hvalid x) honly₀
  have honly₂ : OnlyValues [y, x] tree₂ :=
    onlyValues_treePush [x] y 2 S.topo tree₁ (S.assign y) hx.1 (hvalid y) honly₁
  have honly₃' : OnlyValues [z, y, x] tree₃ :=
    onlyValues_treePush [y, x] z 3 S.topo tree₂ (S.assign z) hy.1 (hvalid z) honly₂
  have honly₃ : OnlyValues [x, y, z] tree₃ := by
    intro value hmem
    apply honly₃' value
    simp only [List.mem_cons, List.not_mem_nil, or_false, not_or] at hmem ⊢
    exact ⟨hmem.2.2, hmem.2.1, hmem.1⟩
  obtain ⟨a, tree₄, hpop₁, hgood₄, hcount₄⟩ :=
    treePop_good_pos S.topo tree₃ hz.1 (by omega)
  obtain ⟨ha, honly₄⟩ := onlyValues_treePop [x, y, z] honly₃ hpop₁
  obtain ⟨b, tree₅, hpop₂, hgood₅, hcount₅⟩ :=
    treePop_good_pos S.topo tree₄ hgood₄ (by omega)
  obtain ⟨hb, honly₅⟩ := onlyValues_treePop [x, y, z] honly₄ hpop₂
  obtain ⟨c, tree₆, hpop₃, hgood₆, hcount₆⟩ :=
    treePop_good_pos S.topo tree₅ hgood₅ (by omega)
  obtain ⟨hc, honly₆⟩ := onlyValues_treePop [x, y, z] honly₅ hpop₃
  refine ⟨a, b, c, ha, hb, hc, ?_⟩
  change runFrom S 0 tree₀ [.push x, .push y, .push z, .pop, .pop, .pop] = _
  simp only [runFrom]
  change runFrom S 3 tree₃ [.pop, .pop, .pop] = _
  simp [runFrom, hpop₁, hpop₂, hpop₃]

theorem lDiamond_no_cycle {k : Nat} (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler) (x y z : Fin k)
    (hPxy : root₁.color x = root₁.color y)
    (hPxz : root₁.color x ≠ root₁.color z)
    (hQxz : root₂.color x = root₂.color z)
    (hQxy : root₂.color x ≠ root₂.color y)
    (hSxy : root₂.rank x ≤ root₂.rank y)
    (hRyz : root₁.rank y ≤ root₁.rank z)
    (hRxz : ¬root₁.rank x ≤ root₁.rank z) : False := by
  have hPyz : root₁.color y ≠ root₁.color z := by
    rw [← hPxy]
    exact hPxz
  have hQyz : root₂.color y ≠ root₂.color z := by
    intro h
    apply hQxy
    exact hQxz.trans h.symm
  have hSyz : root₂.rank y ≤ root₂.rank z :=
    (crossRootRank_le_iff root₁ root₂ hflush y z hPyz hQyz).mp hRyz
  have hRzx : root₁.rank z < root₁.rank x := by omega
  have hleaf₁ := rankLeaf_flush_three_middle_first root₁.rank x y z hRyz hRzx
  have hleaf₂ := rankLeaf_flush_three_chain root₂.rank x y z hSxy hSyz
  have hpattern₁ := root₁.colorRun (flushOps [x, y, z])
  have hpattern₂ := root₂.colorRun (flushOps [x, y, z])
  rw [hleaf₁] at hpattern₁
  rw [hleaf₂] at hpattern₂
  have hruns := hflush [x, y, z]
  rw [← hruns] at hpattern₂
  obtain ⟨a, b, c, ha, hb, hc, hout⟩ :=
    flush_three_outputs root₁.scheduler root₁.valid x y z
  rw [hout] at hpattern₁ hpattern₂
  simp only [List.map_cons, List.map_nil, Option.map_some, List.cons.injEq,
    Option.some.injEq] at hpattern₁ hpattern₂
  have hPb : root₁.color z = root₁.color b := hpattern₁.2.1
  have hQb : root₂.color y = root₂.color b := hpattern₂.2.1
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hb
  rcases hb with rfl | rfl | rfl
  · exact hPxz hPb.symm
  · exact hPxz (hPxy.trans hPb.symm)
  · exact hQxy (hQxz.trans hQb.symm)

theorem lDiamond_no_corner_cycle {k : Nat} (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler) (x y z : Fin k)
    (hPxy : root₁.color x = root₁.color y)
    (hPxz : root₁.color x ≠ root₁.color z)
    (hQyz : root₂.color y = root₂.color z)
    (hQxy : root₂.color x ≠ root₂.color y)
    (hSxy : root₂.rank x ≤ root₂.rank y)
    (hRyz : root₁.rank y ≤ root₁.rank z)
    (hRxz : ¬root₁.rank x ≤ root₁.rank z) : False := by
  have hPyz : root₁.color y ≠ root₁.color z := by
    rw [← hPxy]
    exact hPxz
  have hQxz : root₂.color x ≠ root₂.color z := by
    rw [← hQyz]
    exact hQxy
  have hRzx : root₁.rank z < root₁.rank x := by omega
  have hSzx : root₂.rank z < root₂.rank x := by
    have hs : root₂.rank z ≤ root₂.rank x :=
      (crossRootRank_le_iff root₁ root₂ hflush z x hPxz.symm hQxz.symm).mp
        (by omega)
    have hnot : ¬root₂.rank x ≤ root₂.rank z := by
      intro hback
      have hrback :=
        (crossRootRank_le_iff root₁ root₂ hflush x z hPxz hQxz).mpr hback
      omega
    omega
  have hleaf₁ := rankLeaf_flush_three_middle_first root₁.rank x y z hRyz hRzx
  have hleaf₂ := rankLeaf_flush_three_last_first root₂.rank x y z hSzx hSxy
  have hpattern₁ := root₁.colorRun (flushOps [x, y, z])
  have hpattern₂ := root₂.colorRun (flushOps [x, y, z])
  rw [hleaf₁] at hpattern₁
  rw [hleaf₂] at hpattern₂
  have hruns := hflush [x, y, z]
  rw [← hruns] at hpattern₂
  obtain ⟨a, b, c, ha, hb, hc, hout⟩ :=
    flush_three_outputs root₁.scheduler root₁.valid x y z
  rw [hout] at hpattern₁ hpattern₂
  simp only [List.map_cons, List.map_nil, Option.map_some, List.cons.injEq,
    Option.some.injEq] at hpattern₁ hpattern₂
  have hPb : root₁.color z = root₁.color b := hpattern₁.2.1
  have hQb : root₂.color x = root₂.color b := hpattern₂.2.1
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hb
  rcases hb with rfl | rfl | rfl
  · exact hPxz hPb.symm
  · exact hPxz (hPxy.trans hPb.symm)
  · exact hQxz hQb

theorem lDiamond_no_other_corner_cycle {k : Nat} (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler) (x y z : Fin k)
    (hPyz : root₁.color y = root₁.color z)
    (hPxy : root₁.color x ≠ root₁.color y)
    (hQxz : root₂.color x = root₂.color z)
    (hQxy : root₂.color x ≠ root₂.color y)
    (hRxy : root₁.rank x ≤ root₁.rank y)
    (hSyz : root₂.rank y ≤ root₂.rank z)
    (hRxz : ¬root₁.rank x ≤ root₁.rank z) : False := by
  have hPxz : root₁.color x ≠ root₁.color z := by
    rw [← hPyz]
    exact hPxy
  have hQyz : root₂.color y ≠ root₂.color z := by
    rw [← hQxz]
    exact hQxy.symm
  have hRzx : root₁.rank z < root₁.rank x := by omega
  have hSxy : root₂.rank x ≤ root₂.rank y :=
    (crossRootRank_le_iff root₁ root₂ hflush x y hPxy hQxy).mp hRxy
  have hleaf₁ := rankLeaf_flush_three_last_first root₁.rank x y z hRzx hRxy
  have hleaf₂ := rankLeaf_flush_three_chain root₂.rank x y z hSxy hSyz
  have hpattern₁ := root₁.colorRun (flushOps [x, y, z])
  have hpattern₂ := root₂.colorRun (flushOps [x, y, z])
  rw [hleaf₁] at hpattern₁
  rw [hleaf₂] at hpattern₂
  have hruns := hflush [x, y, z]
  rw [← hruns] at hpattern₂
  obtain ⟨a, b, c, ha, hb, hc, hout⟩ :=
    flush_three_outputs root₁.scheduler root₁.valid x y z
  rw [hout] at hpattern₁ hpattern₂
  simp only [List.map_cons, List.map_nil, Option.map_some, List.cons.injEq,
    Option.some.injEq] at hpattern₁ hpattern₂
  have hPb : root₁.color x = root₁.color b := hpattern₁.2.1
  have hQb : root₂.color y = root₂.color b := hpattern₂.2.1
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hb
  rcases hb with rfl | rfl | rfl
  · exact hQxy hQb.symm
  · exact hPxy hPb
  · exact hPxy (hPb.trans hPyz.symm)

theorem lDiamond_no_left_endpoint_cycle {k : Nat} (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler) (x y z : Fin k)
    (hPxz : root₁.color x = root₁.color z)
    (hPxy : root₁.color x ≠ root₁.color y)
    (hQxy : root₂.color x = root₂.color y)
    (hQxz : root₂.color x ≠ root₂.color z)
    (hRxy : root₁.rank x ≤ root₁.rank y)
    (hRyz : root₁.rank y ≤ root₁.rank z)
    (hSxz : ¬root₂.rank x ≤ root₂.rank z) : False := by
  have hPyz : root₁.color y ≠ root₁.color z := by
    rw [← hPxz]
    exact hPxy.symm
  have hQyz : root₂.color y ≠ root₂.color z := by
    rw [← hQxy]
    exact hQxz
  have hSyz : root₂.rank y ≤ root₂.rank z :=
    (crossRootRank_le_iff root₁ root₂ hflush y z hPyz hQyz).mp hRyz
  have hSzx : root₂.rank z < root₂.rank x := by omega
  have hleaf₁ := rankLeaf_flush_three_chain root₁.rank x y z hRxy hRyz
  have hleaf₂ := rankLeaf_flush_three_middle_first root₂.rank x y z hSyz hSzx
  have hpattern₁ := root₁.colorRun (flushOps [x, y, z])
  have hpattern₂ := root₂.colorRun (flushOps [x, y, z])
  rw [hleaf₁] at hpattern₁
  rw [hleaf₂] at hpattern₂
  have hruns := hflush [x, y, z]
  rw [← hruns] at hpattern₂
  obtain ⟨a, b, c, ha, hb, hc, hout⟩ :=
    flush_three_outputs root₁.scheduler root₁.valid x y z
  rw [hout] at hpattern₁ hpattern₂
  simp only [List.map_cons, List.map_nil, Option.map_some, List.cons.injEq,
    Option.some.injEq] at hpattern₁ hpattern₂
  have hPb : root₁.color y = root₁.color b := hpattern₁.2.1
  have hQb : root₂.color z = root₂.color b := hpattern₂.2.1
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hb
  rcases hb with rfl | rfl | rfl
  · exact hPxy hPb.symm
  · exact hQyz hQb.symm
  · exact hPxy (hPxz.trans hPb.symm)

theorem lDiamond_no_right_endpoint_cycle {k : Nat} (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler) (x y z : Fin k)
    (hPxz : root₁.color x = root₁.color z)
    (hPxy : root₁.color x ≠ root₁.color y)
    (hQyz : root₂.color y = root₂.color z)
    (hQxz : root₂.color x ≠ root₂.color z)
    (hRxy : root₁.rank x ≤ root₁.rank y)
    (hRyz : root₁.rank y ≤ root₁.rank z)
    (hSxz : ¬root₂.rank x ≤ root₂.rank z) : False := by
  have hPyz : root₁.color y ≠ root₁.color z := by
    rw [← hPxz]
    exact hPxy.symm
  have hQxy : root₂.color x ≠ root₂.color y := by
    rw [hQyz]
    exact hQxz
  have hSxy : root₂.rank x ≤ root₂.rank y :=
    (crossRootRank_le_iff root₁ root₂ hflush x y hPxy hQxy).mp hRxy
  have hSzx : root₂.rank z < root₂.rank x := by omega
  have hleaf₁ := rankLeaf_flush_three_chain root₁.rank x y z hRxy hRyz
  have hleaf₂ := rankLeaf_flush_three_last_first root₂.rank x y z hSzx hSxy
  have hpattern₁ := root₁.colorRun (flushOps [x, y, z])
  have hpattern₂ := root₂.colorRun (flushOps [x, y, z])
  rw [hleaf₁] at hpattern₁
  rw [hleaf₂] at hpattern₂
  have hruns := hflush [x, y, z]
  rw [← hruns] at hpattern₂
  obtain ⟨a, b, c, ha, hb, hc, hout⟩ :=
    flush_three_outputs root₁.scheduler root₁.valid x y z
  rw [hout] at hpattern₁ hpattern₂
  simp only [List.map_cons, List.map_nil, Option.map_some, List.cons.injEq,
    Option.some.injEq] at hpattern₁ hpattern₂
  have hPb : root₁.color y = root₁.color b := hpattern₁.2.1
  have hQb : root₂.color x = root₂.color b := hpattern₂.2.1
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hb
  rcases hb with rfl | rfl | rfl
  · exact hPxy hPb.symm
  · exact hQxy hQb
  · exact hQxz hQb



def CommonCell {k : Nat} (root₁ root₂ : NormalRoot k) (x y : Fin k) : Prop :=
  root₁.color x = root₁.color y ∧ root₂.color x = root₂.color y

def commonLe {k : Nat} (root₁ root₂ : NormalRoot k) (x y : Fin k) : Prop :=
  if root₁.color x = root₁.color y
  then root₂.rank x ≤ root₂.rank y
  else root₁.rank x ≤ root₁.rank y

theorem commonLe_total_of_not_cell {k : Nat} (root₁ root₂ : NormalRoot k)
    (x y : Fin k) (_hcell : ¬CommonCell root₁ root₂ x y) :
    commonLe root₁ root₂ x y ∨ commonLe root₁ root₂ y x := by
  by_cases hp : root₁.color x = root₁.color y
  · simp only [commonLe, if_pos hp, if_pos hp.symm]
    omega
  · have hp' : root₁.color y ≠ root₁.color x := Ne.symm hp
    simp only [commonLe, if_neg hp, if_neg hp']
    omega

theorem commonLe_trans_of_distinct_cells {k : Nat}
    (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler)
    (x y z : Fin k)
    (hcellxy : ¬CommonCell root₁ root₂ x y)
    (hcellyz : ¬CommonCell root₁ root₂ y z)
    (hcellxz : ¬CommonCell root₁ root₂ x z)
    (hxy : commonLe root₁ root₂ x y)
    (hyz : commonLe root₁ root₂ y z) :
    commonLe root₁ root₂ x z := by
  by_cases hPxy : root₁.color x = root₁.color y
  · have hQxy : root₂.color x ≠ root₂.color y := fun h =>
      hcellxy ⟨hPxy, h⟩
    have hSxy : root₂.rank x ≤ root₂.rank y := by
      simpa only [commonLe, if_pos hPxy] using hxy
    by_cases hPyz : root₁.color y = root₁.color z
    · have hPxz : root₁.color x = root₁.color z := hPxy.trans hPyz
      have hSyz : root₂.rank y ≤ root₂.rank z := by
        simpa only [commonLe, if_pos hPyz] using hyz
      simpa only [commonLe, if_pos hPxz] using Nat.le_trans hSxy hSyz
    · have hPxz : root₁.color x ≠ root₁.color z := by
        intro h
        exact hPyz (hPxy.symm.trans h)
      have hRyz : root₁.rank y ≤ root₁.rank z := by
        simpa only [commonLe, if_neg hPyz] using hyz
      simp only [commonLe, if_neg hPxz]
      apply Classical.byContradiction
      intro hRxz
      by_cases hQxz : root₂.color x = root₂.color z
      · exact lDiamond_no_cycle root₁ root₂ hflush x y z hPxy hPxz hQxz hQxy
          hSxy hRyz hRxz
      · by_cases hQyz : root₂.color y = root₂.color z
        · exact lDiamond_no_corner_cycle root₁ root₂ hflush x y z hPxy hPxz
            hQyz hQxy hSxy hRyz hRxz
        · have hSyz : root₂.rank y ≤ root₂.rank z :=
            (crossRootRank_le_iff root₁ root₂ hflush y z hPyz hQyz).mp hRyz
          have hSxz : root₂.rank x ≤ root₂.rank z := Nat.le_trans hSxy hSyz
          exact hRxz
            ((crossRootRank_le_iff root₁ root₂ hflush x z hPxz hQxz).mpr hSxz)
  · have hRxy : root₁.rank x ≤ root₁.rank y := by
      simpa only [commonLe, if_neg hPxy] using hxy
    by_cases hPyz : root₁.color y = root₁.color z
    · have hPxz : root₁.color x ≠ root₁.color z := by
        intro h
        exact hPxy (h.trans hPyz.symm)
      have hQyz : root₂.color y ≠ root₂.color z := fun h =>
        hcellyz ⟨hPyz, h⟩
      have hSyz : root₂.rank y ≤ root₂.rank z := by
        simpa only [commonLe, if_pos hPyz] using hyz
      simp only [commonLe, if_neg hPxz]
      apply Classical.byContradiction
      intro hRxz
      by_cases hQxy : root₂.color x = root₂.color y
      · have hQxz : root₂.color x ≠ root₂.color z := by
          rw [hQxy]
          exact hQyz
        have hSxzNot : ¬root₂.rank x ≤ root₂.rank z := by
          intro hs
          exact hRxz
            ((crossRootRank_le_iff root₁ root₂ hflush x z hPxz hQxz).mpr hs)
        exact lDiamond_no_corner_cycle root₂ root₁ (fun w => (hflush w).symm)
          x y z hQxy hQxz hPyz hPxy hRxy hSyz hSxzNot
      · by_cases hQxz : root₂.color x = root₂.color z
        · exact lDiamond_no_other_corner_cycle root₁ root₂ hflush x y z
            hPyz hPxy hQxz hQxy hRxy hSyz hRxz
        · have hSxy : root₂.rank x ≤ root₂.rank y :=
            (crossRootRank_le_iff root₁ root₂ hflush x y hPxy hQxy).mp hRxy
          have hSxz : root₂.rank x ≤ root₂.rank z := Nat.le_trans hSxy hSyz
          exact hRxz
            ((crossRootRank_le_iff root₁ root₂ hflush x z hPxz hQxz).mpr hSxz)
    · have hRyz : root₁.rank y ≤ root₁.rank z := by
        simpa only [commonLe, if_neg hPyz] using hyz
      by_cases hPxz : root₁.color x = root₁.color z
      · have hQxz : root₂.color x ≠ root₂.color z := fun h =>
          hcellxz ⟨hPxz, h⟩
        simp only [commonLe, if_pos hPxz]
        apply Classical.byContradiction
        intro hSxz
        by_cases hQxy : root₂.color x = root₂.color y
        · exact lDiamond_no_left_endpoint_cycle root₁ root₂ hflush x y z
            hPxz hPxy hQxy hQxz hRxy hRyz hSxz
        · by_cases hQyz : root₂.color y = root₂.color z
          · exact lDiamond_no_right_endpoint_cycle root₁ root₂ hflush x y z
              hPxz hPxy hQyz hQxz hRxy hRyz hSxz
          · have hSxy : root₂.rank x ≤ root₂.rank y :=
              (crossRootRank_le_iff root₁ root₂ hflush x y hPxy hQxy).mp hRxy
            have hSyz : root₂.rank y ≤ root₂.rank z :=
              (crossRootRank_le_iff root₁ root₂ hflush y z hPyz hQyz).mp hRyz
            exact hSxz (Nat.le_trans hSxy hSyz)
      · have hRxz : root₁.rank x ≤ root₁.rank z := Nat.le_trans hRxy hRyz
        simpa only [commonLe, if_neg hPxz] using hRxz



def CrossStep {α : Type} (sameCell relation : α → α → Prop)
    (x y : α) : Prop := ¬sameCell x y ∧ relation x y

def CrossReach {α : Type} (sameCell relation : α → α → Prop)
    (x y : α) : Prop :=
  x = y ∨ Relation.TransGen (CrossStep sameCell relation) x y

def StrictCrossReach {α : Type} (sameCell relation : α → α → Prop)
    (x y : α) : Prop :=
  CrossReach sameCell relation x y ∧ ¬CrossReach sameCell relation y x

def NoStrictBackPath {α : Type} (sameCell relation : α → α → Prop) : Prop :=
  ∀ x y, CrossStep sameCell relation x y → ¬relation y x →
    ¬CrossReach sameCell relation y x

theorem crossReach_refl {α : Type} (sameCell relation : α → α → Prop)
    (x : α) : CrossReach sameCell relation x x := Or.inl rfl

theorem crossReach_step {α : Type} {sameCell relation : α → α → Prop}
    {x y : α} (h : CrossStep sameCell relation x y) :
    CrossReach sameCell relation x y :=
  Or.inr (Relation.TransGen.single h)

theorem crossReach_trans {α : Type} {sameCell relation : α → α → Prop}
    {x y z : α} (hxy : CrossReach sameCell relation x y)
    (hyz : CrossReach sameCell relation y z) :
    CrossReach sameCell relation x z := by
  rcases hxy with rfl | hxy
  · exact hyz
  rcases hyz with rfl | hyz
  · exact Or.inr hxy
  · exact Or.inr (hxy.trans hyz)

theorem strictCrossReach_irrefl {α : Type} (sameCell relation : α → α → Prop)
    (x : α) : ¬StrictCrossReach sameCell relation x x := by
  intro h
  exact h.2 (crossReach_refl sameCell relation x)

theorem strictCrossReach_mono_right {α : Type}
    {sameCell relation : α → α → Prop} {x y z : α}
    (hxy : CrossReach sameCell relation x y)
    (hzx : StrictCrossReach sameCell relation z x) :
    StrictCrossReach sameCell relation z y := by
  refine ⟨crossReach_trans hzx.1 hxy, ?_⟩
  intro hyz
  exact hzx.2 (crossReach_trans hxy hyz)

theorem strictCrossReach_congr_right {α : Type}
    {sameCell relation : α → α → Prop} {x y z : α}
    (hxy : CrossReach sameCell relation x y)
    (hyx : CrossReach sameCell relation y x) :
    StrictCrossReach sameCell relation z x ↔
      StrictCrossReach sameCell relation z y := by
  exact ⟨strictCrossReach_mono_right hxy,
    strictCrossReach_mono_right hyx⟩

theorem filter_length_le_of_imp {α : Type} (small large : α → Bool)
    (values : List α)
    (himp : ∀ value, value ∈ values → small value = true →
      large value = true) :
    (values.filter small).length ≤ (values.filter large).length := by
  induction values with
  | nil => simp
  | cons value values ih =>
    cases hs : small value <;> cases hl : large value
    · simpa [hs, hl] using ih (fun x hx =>
        himp x (List.mem_cons_of_mem value hx))
    · simp only [List.filter, hs, hl, List.length_cons]
      have hle := ih (fun x hx => himp x (List.mem_cons_of_mem value hx))
      omega
    · have := himp value (List.mem_cons_self ..) hs
      rw [hl] at this
      contradiction
    · simp only [List.filter, hs, hl, List.length_cons]
      have hle := ih (fun x hx => himp x (List.mem_cons_of_mem value hx))
      omega

theorem filter_length_lt_of_imp_of_witness {α : Type}
    (small large : α → Bool) (values : List α) (witness : α)
    (himp : ∀ value, value ∈ values → small value = true →
      large value = true)
    (hmem : witness ∈ values) (hsmall : small witness = false)
    (hlarge : large witness = true) :
    (values.filter small).length < (values.filter large).length := by
  induction values generalizing witness with
  | nil => simp at hmem
  | cons value values ih =>
    simp only [List.mem_cons] at hmem
    cases hs : small value <;> cases hl : large value
    · simp only [List.filter, hs, hl]
      rcases hmem with rfl | hmem
      · rw [hl] at hlarge
        contradiction
      · exact ih witness (fun x hx => himp x (by simp [hx])) hmem
          hsmall hlarge
    · simp only [List.filter, hs, hl, List.length_cons]
      rcases hmem with rfl | hmem
      · exact Nat.lt_succ_of_le (filter_length_le_of_imp small large values
          (fun x hx => himp x (by simp [hx])))
      · have hlt := ih witness (fun x hx => himp x (by simp [hx])) hmem
            hsmall hlarge
        omega
    · have := himp value (List.mem_cons_self ..) hs
      rw [hl] at this
      contradiction
    · simp only [List.filter, hs, hl, List.length_cons]
      rcases hmem with rfl | hmem
      · rw [hs] at hsmall
        contradiction
      · have hlt := ih witness (fun x hx => himp x (by simp [hx])) hmem
            hsmall hlarge
        omega

noncomputable def strictPredecessorRank {k : Nat}
    (sameCell relation : Fin k → Fin k → Prop) (x : Fin k) : Nat := by
  classical
  exact ((List.finRange k).filter
    (fun z => decide (StrictCrossReach sameCell relation z x))).length

theorem strictPredecessorRank_eq_of_mutual_reach {k : Nat}
    (sameCell relation : Fin k → Fin k → Prop) {x y : Fin k}
    (hxy : CrossReach sameCell relation x y)
    (hyx : CrossReach sameCell relation y x) :
    strictPredecessorRank sameCell relation x =
      strictPredecessorRank sameCell relation y := by
  classical
  unfold strictPredecessorRank
  congr 1
  apply List.filter_congr
  intro z hz
  have hiff := strictCrossReach_congr_right (z := z) hxy hyx
  by_cases hx : StrictCrossReach sameCell relation z x
  · simp [hx, hiff.mp hx]
  · have hy : ¬StrictCrossReach sameCell relation z y := fun hy =>
      hx (hiff.mpr hy)
    simp [hx, hy]

theorem strictPredecessorRank_lt_of_strict_reach {k : Nat}
    (sameCell relation : Fin k → Fin k → Prop) {x y : Fin k}
    (hxy : StrictCrossReach sameCell relation x y) :
    strictPredecessorRank sameCell relation x <
      strictPredecessorRank sameCell relation y := by
  classical
  unfold strictPredecessorRank
  apply filter_length_lt_of_imp_of_witness
    (fun z => decide (StrictCrossReach sameCell relation z x))
    (fun z => decide (StrictCrossReach sameCell relation z y))
    (List.finRange k) x
  · intro z hz hzx
    exact decide_eq_true (strictCrossReach_mono_right hxy.1
      (of_decide_eq_true hzx))
  · exact List.mem_finRange x
  · exact decide_eq_false (strictCrossReach_irrefl sameCell relation x)
  · exact decide_eq_true hxy

theorem finite_cross_relation_rank {k : Nat}
    (sameCell relation : Fin k → Fin k → Prop)
    (hsymm : ∀ x y, sameCell x y → sameCell y x)
    (htotal : ∀ x y, ¬sameCell x y → relation x y ∨ relation y x)
    (hacyclic : NoStrictBackPath sameCell relation) :
    ∀ x y, ¬sameCell x y →
      (strictPredecessorRank sameCell relation x ≤
          strictPredecessorRank sameCell relation y ↔ relation x y) := by
  intro x y hcell
  have hstep_iff : CrossStep sameCell relation x y ↔ relation x y := by
    simp [CrossStep, hcell]
  constructor
  · intro hrank
    apply Classical.byContradiction
    intro hrelation
    have hyx : relation y x := (htotal x y hcell).resolve_left hrelation
    have hcell' : ¬sameCell y x := by
      intro h
      exact hcell (hsymm y x h)
    have hstep : CrossStep sameCell relation y x := ⟨hcell', hyx⟩
    have hnreach : ¬CrossReach sameCell relation x y :=
      hacyclic y x hstep hrelation
    have hstrict : StrictCrossReach sameCell relation y x :=
      ⟨crossReach_step hstep, hnreach⟩
    have hlt := strictPredecessorRank_lt_of_strict_reach sameCell relation hstrict
    omega
  · intro hrelation
    have hreach : CrossReach sameCell relation x y :=
      crossReach_step (hstep_iff.mpr hrelation)
    by_cases hyx : relation y x
    · have hcell' : ¬sameCell y x := by
        intro h
        exact hcell (hsymm y x h)
      have hreach' : CrossReach sameCell relation y x :=
        crossReach_step ⟨hcell', hyx⟩
      exact Nat.le_of_eq
        (strictPredecessorRank_eq_of_mutual_reach sameCell relation hreach hreach')
    · have hnreach : ¬CrossReach sameCell relation y x :=
        hacyclic x y (hstep_iff.mpr hrelation) hyx
      exact Nat.le_of_lt (strictPredecessorRank_lt_of_strict_reach sameCell relation
        ⟨hreach, hnreach⟩)

noncomputable def ExtendedCrossLe {α : Type}
    (sameCell relation : α → α → Prop) (x y : α) : Prop := by
  classical
  exact if sameCell x y then
      ∀ z, ¬sameCell x z →
        (relation z x → relation z y) ∧ (relation y z → relation x z)
    else relation x y

def AlternatingTrans {α : Type} (sameCell relation : α → α → Prop) : Prop :=
  ∀ a b c d, sameCell a c → sameCell b d → ¬sameCell a b →
    relation a b → relation b c → relation c d → relation a d

theorem extendedCrossLe_refl {α : Type} (sameCell relation : α → α → Prop)
    (hrefl : ∀ x, sameCell x x) (x : α) :
    ExtendedCrossLe sameCell relation x x := by
  classical
  simp only [ExtendedCrossLe, if_pos (hrefl x)]
  intro z hz
  exact ⟨fun h => h, fun h => h⟩

theorem extendedCrossLe_step {α : Type} {sameCell relation : α → α → Prop}
    {x y : α} (h : CrossStep sameCell relation x y) :
    ExtendedCrossLe sameCell relation x y := by
  classical
  simp [ExtendedCrossLe, h.1, h.2]

theorem extendedCrossLe_trans {α : Type}
    (sameCell relation : α → α → Prop)
    (hsymm : ∀ x y, sameCell x y → sameCell y x)
    (htrans : ∀ x y z, sameCell x y → sameCell y z → sameCell x z)
    (hlocal : ∀ x y z, ¬sameCell x y → ¬sameCell y z →
      ¬sameCell x z → relation x y → relation y z → relation x z)
    (halt : AlternatingTrans sameCell relation)
    {x y z : α}
    (hxy : ExtendedCrossLe sameCell relation x y)
    (hyz : ExtendedCrossLe sameCell relation y z) :
    ExtendedCrossLe sameCell relation x z := by
  classical
  by_cases cxy : sameCell x y
  · by_cases cyz : sameCell y z
    · have cxz : sameCell x z := htrans x y z cxy cyz
      simp only [ExtendedCrossLe, if_pos cxy] at hxy
      simp only [ExtendedCrossLe, if_pos cyz] at hyz
      simp only [ExtendedCrossLe, if_pos cxz]
      intro w hxw
      have hyw : ¬sameCell y w := fun hyw =>
        hxw (htrans x y w cxy hyw)
      exact ⟨fun hwx => (hyz w hyw).1 ((hxy w hxw).1 hwx),
        fun hzw => (hxy w hxw).2 ((hyz w hyw).2 hzw)⟩
    · have cxz : ¬sameCell x z := fun hxz =>
        cyz (htrans y x z (hsymm x y cxy) hxz)
      simp only [ExtendedCrossLe, if_pos cxy] at hxy
      simp only [ExtendedCrossLe, if_neg cyz] at hyz
      simp only [ExtendedCrossLe, if_neg cxz]
      exact (hxy z cxz).2 hyz
  · by_cases cyz : sameCell y z
    · have cxz : ¬sameCell x z := fun hxz =>
        cxy (htrans x z y hxz (hsymm y z cyz))
      simp only [ExtendedCrossLe, if_neg cxy] at hxy
      simp only [ExtendedCrossLe, if_pos cyz] at hyz
      simp only [ExtendedCrossLe, if_neg cxz]
      have hzx : ¬sameCell z x := fun h => cxz (hsymm z x h)
      exact (hyz x (fun h => cxy (hsymm y x h))).1 hxy
    · by_cases cxz : sameCell x z
      · simp only [ExtendedCrossLe, if_neg cxy] at hxy
        simp only [ExtendedCrossLe, if_neg cyz] at hyz
        simp only [ExtendedCrossLe, if_pos cxz]
        intro w hxw
        have hzw : ¬sameCell z w := fun hzw =>
          hxw (htrans x z w cxz hzw)
        by_cases cyw : sameCell y w
        · have hwy : sameCell w y := hsymm y w cyw
          have hwx : relation w x → relation w z := by
            intro hwx
            exact halt w x y z hwy cxz (fun h => hxw (hsymm w x h))
              hwx hxy hyz
          have hzw' : relation z w → relation x w := by
            intro hzwrel
            exact halt x y z w cxz cyw cxy hxy hyz hzwrel
          exact ⟨hwx, hzw'⟩
        · have hwy : ¬sameCell w y := fun h => cyw (hsymm w y h)
          have hwxcell : ¬sameCell w x := fun h => hxw (hsymm w x h)
          have hwz : ¬sameCell w z := fun h => hzw (hsymm w z h)
          exact ⟨fun hwx =>
              hlocal w y z hwy cyz hwz
                (hlocal w x y hwxcell cxy hwy hwx hxy) hyz,
            fun hzwrel => hlocal x y w cxy cyw hxw hxy
              (hlocal y z w cyz hzw cyw hyz hzwrel)⟩
      · simp only [ExtendedCrossLe, if_neg cxy] at hxy
        simp only [ExtendedCrossLe, if_neg cyz] at hyz
        simp only [ExtendedCrossLe, if_neg cxz]
        exact hlocal x y z cxy cyz cxz hxy hyz

theorem crossReach_extendedCrossLe {α : Type}
    (sameCell relation : α → α → Prop)
    (hrefl : ∀ x, sameCell x x)
    (hsymm : ∀ x y, sameCell x y → sameCell y x)
    (htrans : ∀ x y z, sameCell x y → sameCell y z → sameCell x z)
    (hlocal : ∀ x y z, ¬sameCell x y → ¬sameCell y z →
      ¬sameCell x z → relation x y → relation y z → relation x z)
    (halt : AlternatingTrans sameCell relation)
    {x y : α} (hreach : CrossReach sameCell relation x y) :
    ExtendedCrossLe sameCell relation x y := by
  classical
  rcases hreach with rfl | hreach
  · exact extendedCrossLe_refl sameCell relation hrefl x
  induction hreach with
  | single h => exact extendedCrossLe_step h
  | tail hxy hyz ih =>
    exact extendedCrossLe_trans sameCell relation hsymm htrans hlocal halt ih
      (extendedCrossLe_step hyz)

theorem noStrictBackPath_of_local_and_alternating {α : Type}
    (sameCell relation : α → α → Prop)
    (hrefl : ∀ x, sameCell x x)
    (hsymm : ∀ x y, sameCell x y → sameCell y x)
    (htrans : ∀ x y z, sameCell x y → sameCell y z → sameCell x z)
    (hlocal : ∀ x y z, ¬sameCell x y → ¬sameCell y z →
      ¬sameCell x z → relation x y → relation y z → relation x z)
    (halt : AlternatingTrans sameCell relation) :
    NoStrictBackPath sameCell relation := by
  classical
  intro x y hstep hstrict hback
  have hext := crossReach_extendedCrossLe sameCell relation hrefl hsymm htrans
    hlocal halt hback
  have hcell : ¬sameCell y x := fun h => hstep.1 (hsymm y x h)
  have hrelation : relation y x := by
    simpa [ExtendedCrossLe, hcell] using hext
  exact hstrict hrelation

theorem CommonCell.symm {k : Nat} {root₁ root₂ : NormalRoot k}
    {x y : Fin k} (h : CommonCell root₁ root₂ x y) :
    CommonCell root₁ root₂ y x :=
  ⟨h.1.symm, h.2.symm⟩

theorem CommonCell.refl {k : Nat} (root₁ root₂ : NormalRoot k)
    (x : Fin k) : CommonCell root₁ root₂ x x :=
  ⟨rfl, rfl⟩

theorem CommonCell.trans {k : Nat} {root₁ root₂ : NormalRoot k}
    {x y z : Fin k} (hxy : CommonCell root₁ root₂ x y)
    (hyz : CommonCell root₁ root₂ y z) :
    CommonCell root₁ root₂ x z :=
  ⟨hxy.1.trans hyz.1, hxy.2.trans hyz.2⟩

theorem commonLe_alternating {k : Nat} (root₁ root₂ : NormalRoot k) :
    AlternatingTrans (CommonCell root₁ root₂) (commonLe root₁ root₂) := by
  intro a b c d hac hbd hab habLe hbcLe hcdLe
  by_cases hp : root₁.color a = root₁.color b
  · have hpbc : root₁.color b = root₁.color c := hp.symm.trans hac.1
    have hpcd : root₁.color c = root₁.color d :=
      hac.1.symm.trans (hp.trans hbd.1)
    have hpad : root₁.color a = root₁.color d := hp.trans hbd.1
    simp only [commonLe, if_pos hp, if_pos hpbc, if_pos hpcd, if_pos hpad]
      at habLe hbcLe hcdLe ⊢
    omega
  · have hpbc : root₁.color b ≠ root₁.color c := fun h =>
      hp (hac.1.trans h.symm)
    have hpcd : root₁.color c ≠ root₁.color d := fun h =>
      hp (hac.1.trans (h.trans hbd.1.symm))
    have hpad : root₁.color a ≠ root₁.color d := fun h =>
      hp (h.trans hbd.1.symm)
    simp only [commonLe, if_neg hp, if_neg hpbc, if_neg hpcd, if_neg hpad]
      at habLe hbcLe hcdLe ⊢
    omega

theorem commonLe_noStrictBackPath {k : Nat} (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler) :
    NoStrictBackPath (CommonCell root₁ root₂)
      (commonLe root₁ root₂) := by
  apply noStrictBackPath_of_local_and_alternating
    (CommonCell root₁ root₂) (commonLe root₁ root₂)
    (CommonCell.refl root₁ root₂)
    (fun _ _ h => h.symm)
    (fun _ _ _ hxy hyz => hxy.trans hyz)
  · intro x y z hxy hyz hxz
    exact commonLe_trans_of_distinct_cells root₁ root₂ hflush x y z
      hxy hyz hxz
  · exact commonLe_alternating root₁ root₂

noncomputable def commonRank {k : Nat} (root₁ root₂ : NormalRoot k) :
    Fin k → Nat :=
  strictPredecessorRank (CommonCell root₁ root₂) (commonLe root₁ root₂)

theorem commonRank_le_iff {k : Nat} (root₁ root₂ : NormalRoot k)
    (hacyclic : NoStrictBackPath (CommonCell root₁ root₂)
      (commonLe root₁ root₂))
    (x y : Fin k) (hcell : ¬CommonCell root₁ root₂ x y) :
    commonRank root₁ root₂ x ≤ commonRank root₁ root₂ y ↔
      commonLe root₁ root₂ x y := by
  exact finite_cross_relation_rank
    (CommonCell root₁ root₂) (commonLe root₁ root₂)
    (fun _ _ h => h.symm)
    (commonLe_total_of_not_cell root₁ root₂) hacyclic x y hcell

theorem rootOne_commonRank_agree {k : Nat} (root₁ root₂ : NormalRoot k)
    (hacyclic : NoStrictBackPath (CommonCell root₁ root₂)
      (commonLe root₁ root₂))
    (x y : Fin k) (hcolor : root₁.color x ≠ root₁.color y) :
    PifoGeneral.Agree root₁.rank (commonRank root₁ root₂) x y := by
  apply rankAgree_of_le_iff
  · rw [commonRank_le_iff root₁ root₂ hacyclic x y (fun h => hcolor h.1)]
    simp [commonLe, hcolor]
  · rw [commonRank_le_iff root₁ root₂ hacyclic y x
      (fun h => hcolor h.1.symm)]
    have hp : root₁.color y ≠ root₁.color x := Ne.symm hcolor
    simp [commonLe, hp]

theorem rootTwo_commonRank_agree {k : Nat} (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler)
    (hacyclic : NoStrictBackPath (CommonCell root₁ root₂)
      (commonLe root₁ root₂))
    (x y : Fin k) (hcolor : root₂.color x ≠ root₂.color y) :
    PifoGeneral.Agree root₂.rank (commonRank root₁ root₂) x y := by
  apply rankAgree_of_le_iff
  · rw [commonRank_le_iff root₁ root₂ hacyclic x y (fun h => hcolor h.2)]
    by_cases hp : root₁.color x = root₁.color y
    · simp [commonLe, hp]
    · simp only [commonLe, if_neg hp]
      exact (crossRootRank_le_iff root₁ root₂ hflush x y hp hcolor).symm
  · rw [commonRank_le_iff root₁ root₂ hacyclic y x
      (fun h => hcolor h.2.symm)]
    by_cases hp : root₁.color y = root₁.color x
    · simp [commonLe, hp]
    · simp only [commonLe, if_neg hp]
      exact (crossRootRank_le_iff root₁ root₂ hflush y x hp hcolor.symm).symm

def NormalRoot.rankReplacement {k : Nat} (root : NormalRoot k)
    (replacement : Fin k → Nat) : Scheduler k :=
  ⟨.node root.topologies,
    fun pkt => .node (root.color pkt) (replacement pkt) (root.tail pkt)⟩

theorem rootOne_commonRank_interEquiv {k : Nat} (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler) :
    interEquiv root₁.scheduler
      (root₁.rankReplacement (commonRank root₁ root₂)) := by
  exact coloredNodeInterEquiv root₁.rank (commonRank root₁ root₂)
    root₁.color root₁.tail root₁.topologies
    (rootOne_commonRank_agree root₁ root₂
      (commonLe_noStrictBackPath root₁ root₂ hflush))

theorem rootTwo_commonRank_interEquiv {k : Nat} (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler) :
    interEquiv root₂.scheduler
      (root₂.rankReplacement (commonRank root₁ root₂)) := by
  exact coloredNodeInterEquiv root₂.rank (commonRank root₁ root₂)
    root₂.color root₂.tail root₂.topologies
    (rootTwo_commonRank_agree root₁ root₂ hflush
      (commonLe_noStrictBackPath root₁ root₂ hflush))



theorem ListAt.length_lt {α : Type} {value : α} {values : List α}
    {index : Nat} (h : ListAt value values index) : index < values.length := by
  induction h with
  | zero tail => simp
  | succ head h ih => simp; omega

theorem ListAt.append_left {α : Type} {value : α} {values : List α}
    {index : Nat} (h : ListAt value values index) (suffix : List α) :
    ListAt value (values ++ suffix) index := by
  induction h with
  | zero tail => exact .zero (tail ++ suffix)
  | succ head h ih => exact .succ head ih

theorem ListAt.append_right {α : Type} (initial : List α) {value : α}
    {values : List α} {index : Nat} (h : ListAt value values index) :
    ListAt value (initial ++ values) (initial.length + index) := by
  induction initial with
  | nil => simpa
  | cons head initial ih =>
      change ListAt value (head :: (initial ++ values))
        ((initial.length + 1) + index)
      have heq : (initial.length + index) + 1 =
          (initial.length + 1) + index := by omega
      rw [← heq]
      exact ListAt.succ head ih

theorem listAt_flatMap_replicate {α : Type} {value : α}
    {values : List α} {index width : Nat}
    (h : ListAt value values index) (offset : Fin width) :
    ListAt value
      (values.flatMap (fun entry => List.replicate width entry))
      (index * width + offset.val) := by
  induction h with
  | zero tail =>
      simp only [List.flatMap_cons, Nat.zero_mul, Nat.zero_add]
      exact (listAt_replicate_fin value offset).append_left _
  | @succ head tail index h ih =>
      simp only [List.flatMap_cons]
      have hright := ListAt.append_right (List.replicate width head) ih
      simpa [List.length_replicate, Nat.add_assoc, Nat.add_comm,
        Nat.add_left_comm, Nat.succ_mul] using hright

theorem NormalRoot.color_lt {k : Nat} (root : NormalRoot k) (pkt : Fin k) :
    root.color pkt < root.topologies.length := by
  obtain ⟨topology, hat, _⟩ := pathOkAt_exists root.topologies
    (root.color pkt) (root.tail pkt) (root.hvalidTail pkt)
  exact hat.length_lt

def commonColor {k : Nat} (root₁ root₂ : NormalRoot k) (pkt : Fin k) : Nat :=
  root₁.color pkt * root₂.topologies.length + root₂.color pkt

def commonTopologies {k : Nat} (root₁ root₂ : NormalRoot k) :
    List Topology :=
  root₁.topologies.flatMap
    (fun topology => List.replicate root₂.topologies.length topology)

theorem commonColor_eq_iff {k : Nat} (root₁ root₂ : NormalRoot k)
    (x y : Fin k) :
    commonColor root₁ root₂ x = commonColor root₁ root₂ y ↔
      CommonCell root₁ root₂ x y := by
  let width := root₂.topologies.length
  have hwidth : 0 < width := by
    have := root₂.color_lt x
    omega
  have hx : root₂.color x / width = 0 :=
    Nat.div_eq_of_lt (root₂.color_lt x)
  have hy : root₂.color y / width = 0 :=
    Nat.div_eq_of_lt (root₂.color_lt y)
  constructor
  · intro hcode
    constructor
    · have hdiv := congrArg (fun value => value / width) hcode
      simp only [commonColor] at hdiv
      simpa [width, Nat.mul_comm, Nat.mul_add_div hwidth,
        hx, hy] using hdiv
    · have hmod := congrArg (fun value => value % width) hcode
      simp only [commonColor] at hmod
      simpa [width, Nat.mul_comm, Nat.add_mod,
        Nat.mod_eq_of_lt (root₂.color_lt x),
        Nat.mod_eq_of_lt (root₂.color_lt y)] using hmod
  · rintro ⟨h₁, h₂⟩
    simp [commonColor, h₁, h₂]

theorem commonColor_listAt {k : Nat} (root₁ root₂ : NormalRoot k)
    (pkt : Fin k) :
    ∃ topology,
      ListAt topology (commonTopologies root₁ root₂)
        (commonColor root₁ root₂ pkt) ∧
      pathOk topology (root₁.tail pkt) = true := by
  obtain ⟨topology, hat, hvalid⟩ := pathOkAt_exists root₁.topologies
    (root₁.color pkt) (root₁.tail pkt) (root₁.hvalidTail pkt)
  let offset : Fin root₂.topologies.length :=
    ⟨root₂.color pkt, root₂.color_lt pkt⟩
  refine ⟨topology, ?_, hvalid⟩
  exact listAt_flatMap_replicate hat offset

theorem commonColor_validTail {k : Nat} (root₁ root₂ : NormalRoot k)
    (pkt : Fin k) :
    pathOkAt (commonTopologies root₁ root₂)
      (commonColor root₁ root₂ pkt) (root₁.tail pkt) = true := by
  obtain ⟨topology, hat, hvalid⟩ := commonColor_listAt root₁ root₂ pkt
  rw [hat.pathOkAt]
  exact hvalid

noncomputable def commonRootOne {k : Nat} (root₁ root₂ : NormalRoot k) :
    NormalRoot k :=
  { topologies := commonTopologies root₁ root₂
    color := commonColor root₁ root₂
    rank := commonRank root₁ root₂
    tail := root₁.tail
    hvalidTail := commonColor_validTail root₁ root₂
    nonconstant := by
      obtain ⟨x, y, hxy⟩ := root₁.nonconstant
      exact ⟨x, y, fun hcode =>
        hxy ((commonColor_eq_iff root₁ root₂ x y).mp hcode).1⟩ }

theorem listAt_flatten_replicate {α : Type} {value : α}
    {values : List α} {index count : Nat} (h : ListAt value values index)
    (outer : Fin count) :
    ListAt value (List.replicate count values).flatten
      (outer.val * values.length + index) := by
  cases count with
  | zero => exact Fin.elim0 outer
  | succ count =>
      change ListAt value (List.replicate (count + 1) values).flatten
        (outer.val * values.length + index)
      rw [List.replicate_succ, List.flatten_cons]
      refine Fin.cases ?_ (fun next => ?_) outer
      · simpa using h.append_left (List.replicate count values).flatten
      · have htail := listAt_flatten_replicate h next
        have hright := ListAt.append_right values htail
        simpa [Nat.succ_mul, Nat.add_assoc, Nat.add_comm,
          Nat.add_left_comm] using hright

def commonTopologiesTwo {k : Nat} (root₁ root₂ : NormalRoot k) :
    List Topology :=
  (List.replicate root₁.topologies.length root₂.topologies).flatten

theorem commonColorTwo_listAt {k : Nat} (root₁ root₂ : NormalRoot k)
    (pkt : Fin k) :
    ∃ topology,
      ListAt topology (commonTopologiesTwo root₁ root₂)
        (commonColor root₁ root₂ pkt) ∧
      pathOk topology (root₂.tail pkt) = true := by
  obtain ⟨topology, hat, hvalid⟩ := pathOkAt_exists root₂.topologies
    (root₂.color pkt) (root₂.tail pkt) (root₂.hvalidTail pkt)
  let outer : Fin root₁.topologies.length :=
    ⟨root₁.color pkt, root₁.color_lt pkt⟩
  refine ⟨topology, ?_, hvalid⟩
  exact listAt_flatten_replicate hat outer

theorem commonColorTwo_validTail {k : Nat} (root₁ root₂ : NormalRoot k)
    (pkt : Fin k) :
    pathOkAt (commonTopologiesTwo root₁ root₂)
      (commonColor root₁ root₂ pkt) (root₂.tail pkt) = true := by
  obtain ⟨topology, hat, hvalid⟩ := commonColorTwo_listAt root₁ root₂ pkt
  rw [hat.pathOkAt]
  exact hvalid

noncomputable def commonRootTwo {k : Nat} (root₁ root₂ : NormalRoot k) :
    NormalRoot k :=
  { topologies := commonTopologiesTwo root₁ root₂
    color := commonColor root₁ root₂
    rank := commonRank root₁ root₂
    tail := root₂.tail
    hvalidTail := commonColorTwo_validTail root₁ root₂
    nonconstant := by
      obtain ⟨x, y, hxy⟩ := root₂.nonconstant
      exact ⟨x, y, fun hcode =>
        hxy ((commonColor_eq_iff root₁ root₂ x y).mp hcode).2⟩ }

def OpsOn {k : Nat} (P : Fin k → Prop) : List (Op k) → Prop
  | [] => True
  | .push pkt :: ops => P pkt ∧ OpsOn P ops
  | .pop :: ops => OpsOn P ops

theorem OpsOn.append {k : Nat} {P : Fin k → Prop} :
    ∀ {left right : List (Op k)}, OpsOn P left → OpsOn P right →
      OpsOn P (left ++ right) := by
  intro left right hleft hright
  induction left with
  | nil => exact hright
  | cons op left ih =>
      cases op with
      | push pkt => exact ⟨hleft.1, ih hleft.2⟩
      | pop => exact ih hleft

theorem opsOn_pushes {k : Nat} (P : Fin k → Prop) (word : List (Fin k))
    (hword : ∀ pkt, pkt ∈ word → P pkt) : OpsOn P (word.map Op.push) := by
  induction word with
  | nil => trivial
  | cons pkt word ih =>
      exact ⟨hword pkt (by simp), ih (fun value hmem => hword value (by simp [hmem]))⟩

theorem opsOn_pops {k : Nat} (P : Fin k → Prop) (count : Nat) :
    OpsOn P (List.replicate count (Op.pop : Op k)) := by
  induction count with
  | zero => trivial
  | succ count ih =>
      rw [List.replicate_succ]
      exact ih

theorem opsOn_flushOps {k : Nat} (P : Fin k → Prop) (word : List (Fin k))
    (hword : ∀ pkt, pkt ∈ word → P pkt) : OpsOn P (flushOps word) := by
  exact OpsOn.append (opsOn_pushes P word hword) (opsOn_pops P word.length)

theorem timedOpsFrom_on {k : Nat} (P : Fin k → Prop) :
    ∀ (ops : List (Op k)) (cnt : Nat), OpsOn P ops →
      TimedOpsOn P (timedOpsFrom cnt ops) := by
  intro ops
  induction ops with
  | nil => intros; trivial
  | cons op ops ih =>
      intro cnt hops
      cases op with
      | push pkt => exact ⟨hops.1, ih (cnt + 1) hops.2⟩
      | pop => exact ih cnt hops

theorem NormalRoot.runBlockFull {k : Nat} (root : NormalRoot k)
    (member : Fin k) (ops : List (Op k))
    (hops : OpsOn (fun pkt => root.color pkt = root.color member) ops) :
    run root.scheduler ops = run (root.fullChildScheduler member) ops := by
  have hunary : TimedEquivOn
      (fun pkt : Fin k => root.color pkt = root.color member)
      root.assign (emptyTree (.node root.topologies)) root.tail
        (emptyTree (root.childTopology member)) :=
    unaryNodeTimedEquiv_empty root.assign root.tail root.rank (root.color member)
      (root.childTopology member) root.topologies (root.childTopology_at member) (by
        intro pkt hp
        simp only [NormalRoot.assign]
        rw [hp]) (by
        intro pkt hp
        rw [← (root.childTopology_at member).pathOkAt]
        have hv := root.hvalidTail pkt
        rw [hp] at hv
        exact hv)
  unfold run
  rw [runFrom_eq_runTimedFrom, runFrom_eq_runTimedFrom]
  exact hunary (timedOpsFrom 0 ops) (timedOpsFrom_on _ ops 0 hops)

theorem NormalRoot.flushBlockFull {k : Nat} (root : NormalRoot k)
    (member : Fin k) (word : List (Fin k))
    (hword : ∀ pkt, pkt ∈ word → root.color pkt = root.color member) :
    run root.scheduler (flushOps word) =
      run (root.fullChildScheduler member) (flushOps word) := by
  exact root.runBlockFull member (flushOps word)
    (opsOn_flushOps _ word hword)

def FlushEquivOn {k : Nat} (P : Fin k → Prop)
    (S₁ S₂ : Scheduler k) : Prop :=
  ∀ word, (∀ pkt, pkt ∈ word → P pkt) →
    run S₁ (flushOps word) = run S₂ (flushOps word)

theorem commonCell_fullChildren_flush {k : Nat}
    (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler)
    (member : Fin k) :
    FlushEquivOn (fun pkt => CommonCell root₁ root₂ pkt member)
      (root₁.fullChildScheduler member) (root₂.fullChildScheduler member) := by
  intro word hword
  have hcolor₁ : ∀ pkt, pkt ∈ word →
      root₁.color pkt = root₁.color member := by
    intro pkt hmem
    exact (hword pkt hmem).1
  have hcolor₂ : ∀ pkt, pkt ∈ word →
      root₂.color pkt = root₂.color member := by
    intro pkt hmem
    exact (hword pkt hmem).2
  exact (root₁.flushBlockFull member word hcolor₁).symm.trans
    ((hflush word).trans (root₂.flushBlockFull member word hcolor₂))

theorem ListAt.unique {α : Type} {left right : α} {values : List α}
    {index : Nat} (hleft : ListAt left values index)
    (hright : ListAt right values index) : left = right := by
  induction hleft generalizing right with
  | zero tail =>
      cases hright
      rfl
  | succ head hleft ih =>
      cases hright with
      | succ _ hright => exact ih hright

theorem commonRootOne_childTopology_eq {k : Nat}
    (root₁ root₂ : NormalRoot k) (member : Fin k) :
    (commonRootOne root₁ root₂).childTopology member =
      root₁.childTopology member := by
  let offset : Fin root₂.topologies.length :=
    ⟨root₂.color member, root₂.color_lt member⟩
  have hold : ListAt (root₁.childTopology member)
      (commonTopologies root₁ root₂) (commonColor root₁ root₂ member) :=
    listAt_flatMap_replicate (root₁.childTopology_at member) offset
  exact ((commonRootOne root₁ root₂).childTopology_at member).unique hold

theorem commonRootTwo_childTopology_eq {k : Nat}
    (root₁ root₂ : NormalRoot k) (member : Fin k) :
    (commonRootTwo root₁ root₂).childTopology member =
      root₂.childTopology member := by
  let outer : Fin root₁.topologies.length :=
    ⟨root₁.color member, root₁.color_lt member⟩
  have hold : ListAt (root₂.childTopology member)
      (commonTopologiesTwo root₁ root₂) (commonColor root₁ root₂ member) :=
    listAt_flatten_replicate (root₂.childTopology_at member) outer
  exact ((commonRootTwo root₁ root₂).childTopology_at member).unique hold

theorem commonRootOne_childScheduler_eq_comap {k : Nat}
    (root₁ root₂ : NormalRoot k) (member : Fin k) :
    (commonRootOne root₁ root₂).childScheduler member =
      schedulerComap (root₁.fullChildScheduler member)
        (fiberEmbedding (commonColor root₁ root₂)
          (commonColor root₁ root₂ member)) := by
  change (⟨(commonRootOne root₁ root₂).childTopology member,
      fun pkt => root₁.tail (fiberEmbedding (commonColor root₁ root₂)
        (commonColor root₁ root₂ member) pkt)⟩ : Scheduler _) =
    ⟨root₁.childTopology member,
      fun pkt => root₁.tail (fiberEmbedding (commonColor root₁ root₂)
        (commonColor root₁ root₂ member) pkt)⟩
  rw [Scheduler.mk.injEq]
  constructor
  · exact commonRootOne_childTopology_eq root₁ root₂ member
  · rfl

theorem commonRootTwo_childScheduler_eq_comap {k : Nat}
    (root₁ root₂ : NormalRoot k) (member : Fin k) :
    (commonRootTwo root₁ root₂).childScheduler member =
      schedulerComap (root₂.fullChildScheduler member)
        (fiberEmbedding (commonColor root₁ root₂)
          (commonColor root₁ root₂ member)) := by
  change (⟨(commonRootTwo root₁ root₂).childTopology member,
      fun pkt => root₂.tail (fiberEmbedding (commonColor root₁ root₂)
        (commonColor root₁ root₂ member) pkt)⟩ : Scheduler _) =
    ⟨root₂.childTopology member,
      fun pkt => root₂.tail (fiberEmbedding (commonColor root₁ root₂)
        (commonColor root₁ root₂ member) pkt)⟩
  rw [Scheduler.mk.injEq]
  constructor
  · exact commonRootTwo_childTopology_eq root₁ root₂ member
  · rfl

theorem commonChildren_flushEquiv {k : Nat}
    (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler)
    (member : Fin k) :
    flushEquiv ((commonRootOne root₁ root₂).childScheduler member)
      ((commonRootTwo root₁ root₂).childScheduler member) := by
  let color := commonColor root₁ root₂
  let embedding := fiberEmbedding color (color member)
  have himage : ∀ pkt, CommonCell root₁ root₂ (embedding pkt) member := by
    intro pkt
    apply (commonColor_eq_iff root₁ root₂ (embedding pkt) member).mp
    exact fiberEmbedding_color color (color member) pkt
  have hlocal := commonCell_fullChildren_flush root₁ root₂ hflush member
  have hcomap : flushEquiv
      (schedulerComap (root₁.fullChildScheduler member) embedding)
      (schedulerComap (root₂.fullChildScheduler member) embedding) := by
    intro word
    have hrun := hlocal (word.map embedding) (by
      intro pkt hmem
      obtain ⟨source, _, rfl⟩ := List.mem_map.mp hmem
      exact himage source)
    rw [← mapOpsVal_flushOps embedding word, run_schedulerComap,
      run_schedulerComap] at hrun
    exact listMap_injective (Option.map embedding)
      (Option.map_injective (fiberEmbedding_injective color (color member))) hrun
  rw [commonRootOne_childScheduler_eq_comap,
    commonRootTwo_childScheduler_eq_comap]
  exact hcomap

def filterBy {α β : Type} [BEq β] (key : α → β) (wanted : β)
    (values : List α) : List α :=
  values.filter (fun value => key value == wanted)

def drainTree {α : Type} : Nat → Tree α → List (Option α)
  | 0, _ => []
  | count + 1, tree =>
    match treePop tree with
    | none => none :: drainTree count tree
    | some (pkt, tree') => some pkt :: drainTree count tree'

theorem drainTree_node_filter_aux {α : Type} [DecidableEq α]
    (color : α → Nat) :
    ∀ (fuel : Nat) (q : Queue Nat) (trees : List (Tree α))
      (target : Nat) (targetTree : Tree α),
      Good (.node topologies) (.node q trees) →
      RoutedOnly color trees →
      ListAt targetTree trees target →
      forestCount trees = fuel →
      filterBy (Option.map color) (some target)
          (drainTree fuel (.node q trees)) =
        drainTree (packetCount targetTree) targetTree := by
  intro fuel
  induction fuel with
  | zero =>
    intro q trees target targetTree _ _ hat hcount
    have htarget : packetCount targetTree = 0 := by
      have hle := packetCountAt_le_forestCount trees target
      rw [hat.packetCountAt, hcount] at hle
      omega
    simp [drainTree, htarget, filterBy]
  | succ fuel ih =>
    intro q trees target targetTree hgood honly hat hcount
    have hforestPos : 0 < forestCount trees := by omega
    obtain ⟨selected, rest, hqpop⟩ :=
      qpop_exists_of_forall_count hgood.2
        (forestCount_pos_has_index trees hforestPos)
    have hchild : 0 < packetCountAt trees selected.val := by
      have hc := qpop_valCount hqpop selected.val
      rw [hgood.2] at hc
      split at hc <;> omega
    obtain ⟨pkt, trees', hpop, hgoodForest', hforestCount, hAt⟩ :=
      treePopAt_good_pos topologies trees selected.val hgood.1 hchild
    obtain ⟨hcolor, honly'⟩ :=
      routedOnly_treePopAt_index color honly hpop
    have hgood' : Good (.node topologies) (.node rest trees') := by
      constructor
      · exact hgoodForest'
      · intro index
        have hc := qpop_valCount hqpop index
        rw [hgood.2 index, hAt index] at hc
        exact (Nat.add_right_cancel hc).symm
    have hremaining : forestCount trees' = fuel := by omega
    have hrootPop : treePop (.node q trees) = some (pkt, .node rest trees') := by
      simp [treePop, hqpop, hpop]
    by_cases hselected : selected.val = target
    · subst target
      obtain ⟨targetTree', htargetPop, hat'⟩ := hat.of_treePopAt_some hpop
      have htargetCount : packetCount targetTree =
          packetCount targetTree' + 1 := by
        rw [← hat.packetCountAt, ← hat'.packetCountAt]
        simpa using hAt selected.val
      have hrec := ih rest trees' selected.val targetTree' hgood' honly' hat'
        hremaining
      have hkeep : (some (color pkt) == some selected.val) = true :=
        beq_iff_eq.mpr (congrArg some hcolor)
      simp only [drainTree, hrootPop, htargetPop, filterBy, List.filter,
        Option.map_some, hkeep, htargetCount]
      exact congrArg (fun tail => some pkt :: tail) hrec
    · have hat' := hat.treePopAt_other hselected hpop
      have htargetCount : packetCount targetTree =
          packetCountAt trees' target := by
        rw [← hat.packetCountAt, hAt target]
        simp [hselected]
      have htargetCount' : packetCountAt trees' target =
          packetCount targetTree := hat'.packetCountAt
      have hrec := ih rest trees' target targetTree hgood' honly' hat'
        hremaining
      have hcolorNe : color pkt ≠ target := by
        intro hp
        exact hselected (hcolor.symm.trans hp)
      have hkeep : (some (color pkt) == some target) = false :=
        beq_eq_false_iff_ne.mpr (fun h => hcolorNe (Option.some.inj h))
      simp only [drainTree, hrootPop, filterBy, List.filter, Option.map_some,
        hkeep]
      exact hrec

theorem drainTree_node_filter {α : Type} [DecidableEq α]
    (color : α → Nat) (q : Queue Nat) (trees : List (Tree α))
    (target : Nat) (targetTree : Tree α)
    (hgood : Good (.node topologies) (.node q trees))
    (honly : RoutedOnly color trees)
    (hat : ListAt targetTree trees target) :
    filterBy (Option.map color) (some target)
        (drainTree (forestCount trees) (.node q trees)) =
      drainTree (packetCount targetTree) targetTree := by
  exact drainTree_node_filter_aux color (forestCount trees) q trees target
    targetTree hgood honly hat rfl



def filterEmbedding {α : Type} (keep : α → Bool) : List α → Nat → Nat
  | [], timestamp => timestamp
  | value :: values, timestamp =>
    if keep value then
      match timestamp with
      | 0 => 0
      | next + 1 => filterEmbedding keep values next + 1
    else
      match timestamp with
      | 0 => 0
      | next + 1 => filterEmbedding keep values (next + 1) + 1

theorem filterEmbedding_zero {α : Type} (keep : α → Bool) :
    ∀ values : List α, filterEmbedding keep values 0 = 0 := by
  intro values
  cases values <;> simp [filterEmbedding]

theorem filterEmbedding_lt_iff {α : Type} (keep : α → Bool) :
    ∀ (values : List α) (left right : Nat),
      left < right ↔
        filterEmbedding keep values left < filterEmbedding keep values right := by
  intro values
  induction values with
  | nil => simp [filterEmbedding]
  | cons value values ih =>
    intro left right
    cases hkeep : keep value with
    | false =>
      cases left with
      | zero =>
        cases right with
        | zero => simp [filterEmbedding, hkeep]
        | succ right => simp [filterEmbedding, hkeep]
      | succ left =>
        cases right with
        | zero => simp [filterEmbedding, hkeep]
        | succ right =>
          simp only [filterEmbedding, hkeep, Bool.false_eq_true, ↓reduceIte]
          rw [Nat.add_lt_add_iff_right, Nat.succ_lt_succ_iff]
          simpa only [Nat.add_lt_add_iff_right] using
            (ih (left + 1) (right + 1))
    | true =>
      cases left with
      | zero =>
        cases right with
        | zero => simp [filterEmbedding, hkeep]
        | succ right => simp [filterEmbedding, hkeep]
      | succ left =>
        cases right with
        | zero => simp [filterEmbedding, hkeep]
        | succ right =>
          simp only [filterEmbedding, hkeep, ↓reduceIte]
          rw [Nat.add_lt_add_iff_right, Nat.succ_lt_succ_iff]
          exact ih left right

def timedPushes {α : Type} : List α → List (TimedOp α)
  | [] => []
  | value :: values =>
    .push value 1 :: mapTimedArr Nat.succ (timedPushes values)

def filteredTimedPushes {α : Type} (keep : α → Bool) :
    List α → List (TimedOp α)
  | [] => []
  | value :: values =>
    if keep value then
      .push value 1 :: mapTimedArr Nat.succ (filteredTimedPushes keep values)
    else
      mapTimedArr Nat.succ (filteredTimedPushes keep values)

theorem mapTimedArr_comp {α : Type} (outer inner : Nat → Nat) :
    ∀ ops : List (TimedOp α),
      mapTimedArr outer (mapTimedArr inner ops) =
        mapTimedArr (fun timestamp => outer (inner timestamp)) ops := by
  intro ops
  induction ops with
  | nil => rfl
  | cons op ops ih => cases op <;> simp [mapTimedArr, ih]

theorem mapTimedArr_append {α : Type} (rearrange : Nat → Nat) :
    ∀ left right : List (TimedOp α),
      mapTimedArr rearrange (left ++ right) =
        mapTimedArr rearrange left ++ mapTimedArr rearrange right := by
  intro left right
  induction left with
  | nil => rfl
  | cons op left ih => cases op <;> simp [mapTimedArr, ih]

def TimedOpsPositive {α : Type} : List (TimedOp α) → Prop
  | [] => True
  | .push _ timestamp :: ops => 0 < timestamp ∧ TimedOpsPositive ops
  | .pop :: ops => TimedOpsPositive ops

theorem timedOpsPositive_map_succ {α : Type} :
    ∀ ops : List (TimedOp α), TimedOpsPositive ops →
      TimedOpsPositive (mapTimedArr Nat.succ ops) := by
  intro ops
  induction ops with
  | nil => intros; trivial
  | cons op ops ih =>
    intro hpositive
    cases op with
    | push pkt timestamp =>
      simp only [TimedOpsPositive] at hpositive ⊢
      exact ⟨by omega, ih hpositive.2⟩
    | pop => exact ih hpositive

theorem timedPushes_positive {α : Type} :
    ∀ values : List α, TimedOpsPositive (timedPushes values) := by
  intro values
  induction values with
  | nil => trivial
  | cons value values ih =>
    constructor
    · omega
    · exact timedOpsPositive_map_succ _ ih

theorem mapTimedArr_congr_of_positive {α : Type} {left right : Nat → Nat}
    (h : ∀ timestamp, 0 < timestamp → left timestamp = right timestamp) :
    ∀ ops : List (TimedOp α), TimedOpsPositive ops →
      mapTimedArr left ops = mapTimedArr right ops := by
  intro ops
  induction ops with
  | nil => intros; rfl
  | cons op ops ih =>
    intro hpositive
    cases op with
    | push pkt timestamp =>
      simp only [TimedOpsPositive] at hpositive
      simp [mapTimedArr, h timestamp hpositive.1, ih hpositive.2]
    | pop => simp [mapTimedArr, ih hpositive]

theorem mapTimedArr_timedPushes_filter {α : Type} (keep : α → Bool) :
    ∀ values : List α,
      mapTimedArr (filterEmbedding keep values)
          (timedPushes (values.filter keep)) =
        filteredTimedPushes keep values := by
  intro values
  induction values with
  | nil => rfl
  | cons value values ih =>
    cases hkeep : keep value with
    | false =>
      simp only [List.filter, hkeep, filteredTimedPushes,
        filterEmbedding, Bool.false_eq_true, ↓reduceIte]
      rw [← ih, mapTimedArr_comp]
      apply mapTimedArr_congr_of_positive
      · intro timestamp htimestamp
        cases timestamp with
        | zero => omega
        | succ timestamp => rfl
      · exact timedPushes_positive _
    | true =>
      simp only [List.filter, hkeep, timedPushes, filteredTimedPushes,
        filterEmbedding, ↓reduceIte, mapTimedArr, List.cons.injEq]
      constructor
      · rw [filterEmbedding_zero]
      · rw [mapTimedArr_comp, ← ih, mapTimedArr_comp]

def pushWordFrom {α : Type} (assign : α → Path) :
    Nat → Tree α → List α → Tree α
  | _, tree, [] => tree
  | count, tree, value :: values =>
    pushWordFrom assign (count + 1)
      (treePush value (count + 1) tree (assign value)) values

def pushFilteredWordFrom {α : Type} (keep : α → Bool)
    (assign : α → Path) : Nat → Tree α → List α → Tree α
  | _, tree, [] => tree
  | count, tree, value :: values =>
    pushFilteredWordFrom keep assign (count + 1)
      (if keep value then treePush value (count + 1) tree (assign value)
       else tree) values

def pushForestWordFrom {α : Type} (color : α → Nat) (tail : α → Path) :
    Nat → List (Tree α) → List α → List (Tree α)
  | _, trees, [] => trees
  | count, trees, value :: values =>
    pushForestWordFrom color tail (count + 1)
      (treePushAt value (count + 1) trees (color value) (tail value)) values

theorem pushWordFrom_node {α : Type} (color rank : α → Nat)
    (tail : α → Path) :
    ∀ (values : List α) (count : Nat) (q : Queue Nat)
      (trees : List (Tree α)),
      ∃ q', pushWordFrom (fun pkt => .node (color pkt) (rank pkt) (tail pkt))
          count (.node q trees) values =
        .node q' (pushForestWordFrom color tail count trees values) := by
  intro values
  induction values with
  | nil =>
    intro count q trees
    exact ⟨q, rfl⟩
  | cons value values ih =>
    intro count q trees
    simp only [pushWordFrom, treePush, pushForestWordFrom]
    exact ih (count + 1)
      (q ++ [⟨color value, rank value, count + 1⟩])
      (treePushAt value (count + 1) trees (color value) (tail value))

theorem ListAt.pushForestWordFrom {α : Type} (color : α → Nat)
    (tail : α → Path) (target : Nat) :
    ∀ (values : List α) (count : Nat) {tree : Tree α}
      {trees : List (Tree α)},
      ListAt tree trees target →
      ListAt
        (pushFilteredWordFrom (fun pkt => color pkt == target) tail
          count tree values)
        (pushForestWordFrom color tail count trees values) target := by
  intro values
  induction values with
  | nil =>
    intro count tree trees hat
    exact hat
  | cons value values ih =>
    intro count tree trees hat
    by_cases hcolor : color value = target
    · have hkeep : (color value == target) = true := beq_iff_eq.mpr hcolor
      simp only [pushFilteredWordFrom, hkeep, ↓reduceIte]
      apply ih
      rw [hcolor]
      exact hat.treePushAt value (count + 1) (tail value)
    · have hkeep : (color value == target) = false :=
        beq_eq_false_iff_ne.mpr hcolor
      simp only [pushFilteredWordFrom, hkeep, Bool.false_eq_true, ↓reduceIte]
      apply ih
      exact hat.treePushAt_other value (count + 1) (tail value) hcolor

theorem pushForestWordFrom_good_routed {α : Type} [DecidableEq α]
    (color : α → Nat) (tail : α → Path) (topologies : List Topology)
    (hvalid : ∀ pkt, pathOkAt topologies (color pkt) (tail pkt) = true) :
    ∀ (values : List α) (count : Nat) (trees : List (Tree α)),
      GoodForest topologies trees → RoutedOnly color trees →
      GoodForest topologies (pushForestWordFrom color tail count trees values) ∧
      RoutedOnly color (pushForestWordFrom color tail count trees values) ∧
      forestCount (pushForestWordFrom color tail count trees values) =
        forestCount trees + values.length := by
  intro values
  induction values with
  | nil =>
    intro count trees hgood honly
    exact ⟨hgood, honly, by simp [pushForestWordFrom]⟩
  | cons value values ih =>
    intro count trees hgood honly
    let trees' := treePushAt value (count + 1) trees (color value) (tail value)
    have hpushed := treePushAt_good_count value (count + 1) topologies trees
      (color value) (tail value) hgood (hvalid value)
    have honly' := routedOnly_treePushAt color topologies trees value
      (count + 1) (tail value) hgood (hvalid value) honly
    have htail := ih (count + 1) trees' hpushed.1 honly'
    have htrees' : forestCount trees' = forestCount trees + 1 :=
      hpushed.2.1
    change GoodForest topologies
        (pushForestWordFrom color tail (count + 1) trees' values) ∧
      RoutedOnly color
        (pushForestWordFrom color tail (count + 1) trees' values) ∧
      forestCount (pushForestWordFrom color tail (count + 1) trees' values) =
        forestCount trees + (values.length + 1)
    refine ⟨htail.1, htail.2.1, ?_⟩
    omega

theorem pushFilteredWordFrom_good_count {α : Type} (keep : α → Bool)
    (assign : α → Path) (topology : Topology)
    (hvalid : ∀ pkt, keep pkt = true → pathOk topology (assign pkt) = true) :
    ∀ (values : List α) (count : Nat) (tree : Tree α),
      Good topology tree →
      Good topology (pushFilteredWordFrom keep assign count tree values) ∧
      packetCount (pushFilteredWordFrom keep assign count tree values) =
        packetCount tree + (values.filter keep).length := by
  intro values
  induction values with
  | nil =>
    intro count tree hgood
    exact ⟨hgood, by simp [pushFilteredWordFrom]⟩
  | cons value values ih =>
    intro count tree hgood
    cases hkeep : keep value with
    | false =>
      simp only [pushFilteredWordFrom, hkeep, Bool.false_eq_true, ↓reduceIte,
        List.filter]
      exact ih (count + 1) tree hgood
    | true =>
      have hpushed := treePush_good_count value (count + 1) topology tree
        (assign value) hgood (hvalid value hkeep)
      have htail := ih (count + 1)
        (treePush value (count + 1) tree (assign value)) hpushed.1
      simp only [pushFilteredWordFrom, hkeep, ↓reduceIte, List.filter,
        List.length_cons]
      refine ⟨htail.1, ?_⟩
      omega

theorem pushWordFrom_good_count {α : Type} (assign : α → Path)
    (topology : Topology) (hvalid : ∀ pkt, pathOk topology (assign pkt) = true) :
    ∀ (values : List α) (count : Nat) (tree : Tree α),
      Good topology tree →
      Good topology (pushWordFrom assign count tree values) ∧
      packetCount (pushWordFrom assign count tree values) =
        packetCount tree + values.length := by
  intro values
  induction values with
  | nil =>
    intro count tree hgood
    exact ⟨hgood, by simp [pushWordFrom]⟩
  | cons value values ih =>
    intro count tree hgood
    have hpushed := treePush_good_count value (count + 1) topology tree
      (assign value) hgood (hvalid value)
    have htail := ih (count + 1)
      (treePush value (count + 1) tree (assign value)) hpushed.1
    simp only [pushWordFrom, List.length_cons]
    refine ⟨htail.1, ?_⟩
    omega

theorem runTimedFrom_timedPushes_append {α : Type} (assign : α → Path) :
    ∀ (values : List α) (count : Nat) (tree : Tree α)
      (rest : List (TimedOp α)),
      runTimedFrom assign tree
          (mapTimedArr (fun timestamp => count + timestamp)
            (timedPushes values) ++ rest) =
        runTimedFrom assign (pushWordFrom assign count tree values) rest := by
  intro values
  induction values with
  | nil => intros; rfl
  | cons value values ih =>
    intro count tree rest
    simp only [timedPushes, mapTimedArr, List.cons_append, runTimedFrom,
      pushWordFrom]
    rw [mapTimedArr_comp]
    have hfunctions :
        (fun timestamp => count + Nat.succ timestamp) =
          (fun timestamp => count + 1 + timestamp) := by
      funext timestamp
      omega
    rw [hfunctions]
    exact ih (count + 1)
      (treePush value (count + 1) tree (assign value)) rest

theorem runTimedFrom_timedPushes_append_zero {α : Type}
    (assign : α → Path) (values : List α) (tree : Tree α)
    (rest : List (TimedOp α)) :
    runTimedFrom assign tree (timedPushes values ++ rest) =
      runTimedFrom assign (pushWordFrom assign 0 tree values) rest := by
  have hid : mapTimedArr (fun timestamp : Nat => timestamp)
      (timedPushes values) = timedPushes values := by
    induction (timedPushes values) with
    | nil => rfl
    | cons op ops ih => cases op <;> simp [mapTimedArr, ih]
  rw [← hid]
  simpa only [Nat.zero_add] using
    runTimedFrom_timedPushes_append assign values 0 tree rest

theorem runTimedFrom_filteredTimedPushes_append {α : Type}
    (keep : α → Bool) (assign : α → Path) :
    ∀ (values : List α) (count : Nat) (tree : Tree α)
      (rest : List (TimedOp α)),
      runTimedFrom assign tree
          (mapTimedArr (fun timestamp => count + timestamp)
            (filteredTimedPushes keep values) ++ rest) =
        runTimedFrom assign
          (pushFilteredWordFrom keep assign count tree values) rest := by
  intro values
  induction values with
  | nil => intros; rfl
  | cons value values ih =>
    intro count tree rest
    cases hkeep : keep value with
    | false =>
      simp only [filteredTimedPushes, hkeep, Bool.false_eq_true, ↓reduceIte,
        pushFilteredWordFrom]
      rw [mapTimedArr_comp]
      have hfunctions :
          (fun timestamp => count + Nat.succ timestamp) =
            (fun timestamp => count + 1 + timestamp) := by
        funext timestamp
        omega
      rw [hfunctions]
      exact ih (count + 1) tree rest
    | true =>
      simp only [filteredTimedPushes, hkeep, ↓reduceIte, mapTimedArr,
        List.cons_append, runTimedFrom, pushFilteredWordFrom]
      rw [mapTimedArr_comp]
      have hfunctions :
          (fun timestamp => count + Nat.succ timestamp) =
            (fun timestamp => count + 1 + timestamp) := by
        funext timestamp
        omega
      rw [hfunctions]
      exact ih (count + 1)
        (treePush value (count + 1) tree (assign value)) rest

theorem runTimedFrom_filteredTimedPushes_append_zero {α : Type}
    (keep : α → Bool) (assign : α → Path) (values : List α)
    (tree : Tree α) (rest : List (TimedOp α)) :
    runTimedFrom assign tree (filteredTimedPushes keep values ++ rest) =
      runTimedFrom assign
        (pushFilteredWordFrom keep assign 0 tree values) rest := by
  have hid : mapTimedArr (fun timestamp : Nat => timestamp)
      (filteredTimedPushes keep values) = filteredTimedPushes keep values := by
    induction (filteredTimedPushes keep values) with
    | nil => rfl
    | cons op ops ih => cases op <;> simp [mapTimedArr, ih]
  rw [← hid]
  simpa only [Nat.zero_add] using
    runTimedFrom_filteredTimedPushes_append keep assign values 0 tree rest

theorem runTimedFrom_pops {α : Type} (assign : α → Path) :
    ∀ (count : Nat) (tree : Tree α),
      runTimedFrom assign tree (List.replicate count (.pop : TimedOp α)) =
        drainTree count tree := by
  intro count
  induction count with
  | zero => intros; rfl
  | succ count ih =>
    intro tree
    rw [List.replicate_succ]
    simp only [runTimedFrom, drainTree]
    cases treePop tree <;> simp [ih]

theorem timedOpsFrom_pushes_append {k : Nat} :
    ∀ (word : List (Fin k)) (rest : List (Op k)) (count : Nat),
      timedOpsFrom count (word.map Op.push ++ rest) =
        mapTimedArr (fun timestamp => count + timestamp) (timedPushes word) ++
          timedOpsFrom (count + word.length) rest := by
  intro word
  induction word with
  | nil => intros; rfl
  | cons pkt word ih =>
    intro rest count
    simp only [List.map_cons, List.cons_append, timedOpsFrom, timedPushes,
      mapTimedArr, List.cons.injEq, true_and]
    rw [mapTimedArr_comp]
    have hfunctions :
        (fun timestamp => count + Nat.succ timestamp) =
          (fun timestamp => count + 1 + timestamp) := by
      funext timestamp
      omega
    rw [hfunctions, ih]
    congr 2
    simp only [List.length_cons]
    omega

theorem timedOpsFrom_pops {k : Nat} :
    ∀ (count pushes : Nat),
      timedOpsFrom count (List.replicate pushes (.pop : Op k)) =
        List.replicate pushes (.pop : TimedOp (Fin k)) := by
  intro count pushes
  induction pushes with
  | zero => rfl
  | succ pushes ih =>
    change timedOpsFrom count
        ((.pop : Op k) :: List.replicate pushes (.pop : Op k)) =
      (.pop : TimedOp (Fin k)) ::
        List.replicate pushes (.pop : TimedOp (Fin k))
    simp [timedOpsFrom, ih]

theorem timedOpsFrom_flushOps {k : Nat} (word : List (Fin k)) :
    timedOpsFrom 0 (flushOps word) =
      timedPushes word ++
        List.replicate word.length (.pop : TimedOp (Fin k)) := by
  rw [flushOps, timedOpsFrom_pushes_append]
  simp only [Nat.zero_add]
  rw [timedOpsFrom_pops]
  have hid : (fun timestamp : Nat => timestamp) = id := rfl
  rw [hid]
  induction (timedPushes word) with
  | nil => rfl
  | cons op ops ih => cases op <;> simp [mapTimedArr, ih]

mutual
  theorem reArrTree_emptyTree {α : Type} (rearrange : Nat → Nat)
      (topology : Topology) :
      reArrTree rearrange (emptyTree (α := α) topology) = emptyTree topology := by
    cases topology with
    | leaf => rfl
    | node topologies =>
      simp only [emptyTree, reArrTree, List.map_nil]
      rw [reArrForest_emptyForest]

  theorem reArrForest_emptyForest {α : Type} (rearrange : Nat → Nat)
      (topologies : List Topology) :
      reArrForest rearrange (emptyForest (α := α) topologies) =
        emptyForest topologies := by
    cases topologies with
    | nil => rfl
    | cons topology topologies =>
      simp only [emptyForest, reArrForest, List.cons.injEq]
      exact ⟨reArrTree_emptyTree rearrange topology,
        reArrForest_emptyForest rearrange topologies⟩
end

theorem mapTimedArr_replicate_pop {α : Type} (rearrange : Nat → Nat)
    (count : Nat) :
    mapTimedArr rearrange (List.replicate count (.pop : TimedOp α)) =
      List.replicate count .pop := by
  induction count with
  | zero => rfl
  | succ count ih =>
    change mapTimedArr rearrange
        ((.pop : TimedOp α) :: List.replicate count (.pop : TimedOp α)) =
      (.pop : TimedOp α) :: List.replicate count (.pop : TimedOp α)
    simp [mapTimedArr, ih]

theorem filteredBatch_compress {α : Type} (assign : α → Path)
    (topology : Topology) (keep : α → Bool) (word : List α) :
    runTimedFrom assign (emptyTree topology)
        (filteredTimedPushes keep word ++
          List.replicate (word.filter keep).length (.pop : TimedOp α)) =
      runTimedFrom assign (emptyTree topology)
        (timedPushes (word.filter keep) ++
          List.replicate (word.filter keep).length (.pop : TimedOp α)) := by
  let rearrange := filterEmbedding keep word
  let compressed := timedPushes (word.filter keep) ++
    List.replicate (word.filter keep).length (.pop : TimedOp α)
  have hmap : mapTimedArr rearrange compressed =
      filteredTimedPushes keep word ++
        List.replicate (word.filter keep).length (.pop : TimedOp α) := by
    rw [mapTimedArr_append, mapTimedArr_timedPushes_filter,
      mapTimedArr_replicate_pop]
  have hrun := runTimedFrom_reArr assign rearrange
    (filterEmbedding_lt_iff keep word) compressed (emptyTree topology)
  rw [reArrTree_emptyTree, hmap] at hrun
  exact hrun

theorem NormalRoot.flush_filterBy_color {k : Nat} (root : NormalRoot k)
    (member : Fin k) (word : List (Fin k)) :
    filterBy (Option.map root.color) (some (root.color member))
        (run root.scheduler (flushOps word)) =
      run root.scheduler
        (flushOps (filterBy root.color (root.color member) word)) := by
  let target := root.color member
  let keep : Fin k → Bool := fun pkt => root.color pkt == target
  let childTopology := root.childTopology member
  let initialForest := emptyForest (α := Fin k) root.topologies
  let finalForest := pushForestWordFrom root.color root.tail 0 initialForest word
  let initialChild := emptyTree (α := Fin k) childTopology
  let finalChild := pushFilteredWordFrom keep root.tail 0 initialChild word
  let pops := List.replicate word.length (.pop : TimedOp (Fin k))
  let childPops := List.replicate (word.filter keep).length
    (.pop : TimedOp (Fin k))
  have hatInitial : ListAt initialChild initialForest target := by
    exact (root.childTopology_at member).emptyForest
  have hatFinal : ListAt finalChild finalForest target := by
    exact hatInitial.pushForestWordFrom root.color root.tail target word 0
  have hforest := pushForestWordFrom_good_routed root.color root.tail
    root.topologies root.hvalidTail word 0 initialForest
    (emptyTree_good (.node root.topologies)).1
    (routedOnly_emptyForest root.color root.topologies)
  have hforestCount : forestCount finalForest = word.length := by
    simpa [finalForest, initialForest, forestCount_emptyForest] using hforest.2.2
  obtain ⟨finalQueue, hfinalState⟩ :=
    pushWordFrom_node root.color root.rank root.tail word 0 [] initialForest
  have hfinalState' :
      pushWordFrom root.assign 0 (emptyTree (.node root.topologies)) word =
        .node finalQueue finalForest := by
    change pushWordFrom
      (fun pkt => .node (root.color pkt) (root.rank pkt) (root.tail pkt))
      0 (.node [] initialForest) word = .node finalQueue finalForest
    simpa [finalForest] using hfinalState
  have hrootGood : Good (.node root.topologies)
      (.node finalQueue finalForest) := by
    have hgood := (pushWordFrom_good_count root.assign
      (.node root.topologies) root.valid word 0
      (emptyTree (.node root.topologies)) (emptyTree_good _)).1
    rw [hfinalState'] at hgood
    exact hgood
  have hchildValid : ∀ pkt, keep pkt = true →
      pathOk childTopology (root.tail pkt) = true := by
    intro pkt hkeep
    have hcolor : root.color pkt = target := beq_iff_eq.mp hkeep
    rw [← (root.childTopology_at member).pathOkAt]
    have hp := root.hvalidTail pkt
    simpa [target, hcolor] using hp
  have hchild := pushFilteredWordFrom_good_count keep root.tail childTopology
    hchildValid word 0 initialChild (emptyTree_good childTopology)
  have hchildCount : packetCount finalChild = (word.filter keep).length := by
    simpa [finalChild, initialChild, packetCount_emptyTree] using hchild.2
  have hfullRun :
      runTimedFrom root.assign (emptyTree (.node root.topologies))
          (timedPushes word ++ pops) =
        drainTree word.length (.node finalQueue finalForest) := by
    rw [runTimedFrom_timedPushes_append_zero, hfinalState',
      runTimedFrom_pops]
  have hchildRun :
      runTimedFrom root.tail initialChild
          (filteredTimedPushes keep word ++ childPops) =
        drainTree (packetCount finalChild) finalChild := by
    rw [runTimedFrom_filteredTimedPushes_append_zero,
      runTimedFrom_pops, hchildCount]
  have hprojectionTimed :
      filterBy (Option.map root.color) (some target)
          (runTimedFrom root.assign (emptyTree (.node root.topologies))
            (timedPushes word ++ pops)) =
        runTimedFrom root.tail initialChild
          (filteredTimedPushes keep word ++ childPops) := by
    rw [hfullRun, hchildRun]
    rw [← hforestCount]
    exact drainTree_node_filter root.color finalQueue finalForest target
      finalChild hrootGood hforest.2.1 hatFinal
  have hcompressed := filteredBatch_compress root.tail childTopology keep word
  have hprojectionCompressed :
      filterBy (Option.map root.color) (some target)
          (runTimedFrom root.assign (emptyTree (.node root.topologies))
            (timedPushes word ++ pops)) =
        runTimedFrom root.tail initialChild
          (timedPushes (word.filter keep) ++ childPops) :=
    hprojectionTimed.trans hcompressed
  have hrootRun : run root.scheduler (flushOps word) =
      runTimedFrom root.assign (emptyTree (.node root.topologies))
        (timedPushes word ++ pops) := by
    unfold run
    rw [runFrom_eq_runTimedFrom, timedOpsFrom_flushOps]
    rfl
  have hchildSchedulerRun :
      run (root.fullChildScheduler member) (flushOps (word.filter keep)) =
        runTimedFrom root.tail initialChild
          (timedPushes (word.filter keep) ++ childPops) := by
    unfold run
    rw [runFrom_eq_runTimedFrom, timedOpsFrom_flushOps]
    rfl
  have hword : ∀ pkt, pkt ∈ word.filter keep →
      root.color pkt = root.color member := by
    intro pkt hmem
    have hp := (List.mem_filter.mp hmem).2
    exact beq_iff_eq.mp hp
  have hblock := root.flushBlockFull member (word.filter keep) hword
  change filterBy (Option.map root.color) (some target)
      (run root.scheduler (flushOps word)) =
    run root.scheduler (flushOps (word.filter keep))
  rw [hrootRun, hblock, hchildSchedulerRun]
  exact hprojectionCompressed

theorem drainTree_good_some {α : Type} (topology : Topology) :
    ∀ (count : Nat) (tree : Tree α), Good topology tree →
      packetCount tree = count →
      ∃ output : List α, output.length = count ∧
        drainTree count tree = output.map some := by
  intro count
  induction count with
  | zero =>
    intro tree hgood hcount
    exact ⟨[], rfl, rfl⟩
  | succ count ih =>
    intro tree hgood hcount
    have hpos : 0 < packetCount tree := by omega
    obtain ⟨pkt, tree', hpop, hgood', hcount'⟩ :=
      treePop_good_pos topology tree hgood hpos
    have hremaining : packetCount tree' = count := by omega
    obtain ⟨output, hlength, hdrain⟩ := ih tree' hgood' hremaining
    refine ⟨pkt :: output, by simp [hlength], ?_⟩
    simp [drainTree, hpop, hdrain]

theorem valid_flush_some {k : Nat} (S : Scheduler k) (hvalid : S.Valid)
    (word : List (Fin k)) :
    ∃ output : List (Fin k), output.length = word.length ∧
      run S (flushOps word) = output.map some := by
  let finalTree := pushWordFrom S.assign 0 (emptyTree S.topo) word
  have hfinal := pushWordFrom_good_count S.assign S.topo hvalid word 0
    (emptyTree S.topo) (emptyTree_good S.topo)
  have hcount : packetCount finalTree = word.length := by
    simpa [finalTree, packetCount_emptyTree] using hfinal.2
  obtain ⟨output, hlength, hdrain⟩ := drainTree_good_some S.topo
    word.length finalTree hfinal.1 hcount
  refine ⟨output, hlength, ?_⟩
  unfold run
  rw [runFrom_eq_runTimedFrom, timedOpsFrom_flushOps,
    runTimedFrom_timedPushes_append_zero, runTimedFrom_pops]
  exact hdrain

theorem filterBy_none_valid_flush {k : Nat} (S : Scheduler k)
    (hvalid : S.Valid) (color : Fin k → Nat) (word : List (Fin k)) :
    filterBy (Option.map color) none
      (run S (flushOps word)) = [] := by
  obtain ⟨output, _, hrun⟩ := valid_flush_some S hvalid word
  rw [hrun]
  simp [filterBy]

theorem filterBy_some_valid_flush_of_not_exists {k : Nat} (S : Scheduler k)
    (hvalid : S.Valid) (color : Fin k → Nat) (word : List (Fin k))
    (target : Nat) (habsent : ¬∃ pkt, color pkt = target) :
    filterBy (Option.map color) (some target) (run S (flushOps word)) = [] := by
  obtain ⟨output, hlength, hrun⟩ := valid_flush_some S hvalid word
  rw [hrun]
  clear hlength hrun
  induction output with
  | nil => rfl
  | cons pkt output ih =>
    have hne : color pkt ≠ target := fun h => habsent ⟨pkt, h⟩
    have hkeep : (some (color pkt) == some target) = false :=
      beq_eq_false_iff_ne.mpr (fun h => hne (Option.some.inj h))
    change List.filter (fun value => Option.map color value == some target)
      (some pkt :: output.map some) = []
    change List.filter (fun value => Option.map color value == some target)
      (output.map some) = [] at ih
    simp only [List.filter, Option.map_some, hkeep]
    exact ih

def stateFilter {α : Type} (keep : α → Bool) (state : List (α × Nat)) :
    List (α × Nat) := state.filter (fun entry => keep entry.1)

theorem stateFilter_mem {α : Type} (keep : α → Bool)
    {entry : α × Nat} {state : List (α × Nat)} :
    entry ∈ stateFilter keep state ↔ entry ∈ state ∧ keep entry.1 = true := by
  simp [stateFilter]

theorem stateFilter_distinctArr {α : Type} (keep : α → Bool)
    {state : List (α × Nat)} (hd : PifoGeneral.DistinctArr state) :
    PifoGeneral.DistinctArr (stateFilter keep state) :=
  hd.sublist List.filter_sublist

theorem stateFilter_removeArr {α : Type} (keep : α → Bool) :
    ∀ (state : List (α × Nat)) (arrival : Nat),
      stateFilter keep (PifoGeneral.removeArr state arrival) =
        PifoGeneral.removeArr (stateFilter keep state) arrival := by
  intro state arrival
  simp only [stateFilter, PifoGeneral.removeArr, List.filter_filter]
  apply List.filter_congr
  intro entry hmem
  exact Bool.and_comm _ _

theorem distinctArr_eq_of_arr_eq {α : Type} {state : List (α × Nat)}
    (hd : PifoGeneral.DistinctArr state) {left right : α × Nat}
    (hleft : left ∈ state) (hright : right ∈ state)
    (harr : left.2 = right.2) : left = right := by
  induction state with
  | nil => cases hleft
  | cons entry state ih =>
    have hpair := List.pairwise_cons.mp hd
    simp only [List.mem_cons] at hleft hright
    rcases hleft with rfl | hleft <;> rcases hright with rfl | hright
    · rfl
    · exact False.elim ((hpair.1 right hright) harr)
    · exact False.elim ((hpair.1 left hleft) harr.symm)
    · exact ih hpair.2 hleft hright

theorem stateFilter_removeArr_other {α : Type} (keep : α → Bool)
    {state : List (α × Nat)} {entry : α × Nat}
    (hd : PifoGeneral.DistinctArr state) (hmem : entry ∈ state)
    (hkeep : keep entry.1 = false) :
    stateFilter keep (PifoGeneral.removeArr state entry.2) =
      stateFilter keep state := by
  rw [stateFilter_removeArr]
  apply PifoGeneral.removeArr_of_not_mem
  intro other hother harr
  have hotherState := (stateFilter_mem keep).mp hother
  have heq : other = entry :=
    distinctArr_eq_of_arr_eq hd hotherState.1 hmem harr
  subst other
  rw [hkeep] at hotherState
  exact Bool.noConfusion hotherState.2

theorem drainLeaf_filter_state_aux {α : Type} [DecidableEq α]
    (rank : α → Nat) (color : α → Nat) (target : Nat) :
    ∀ (fuel : Nat) (state : List (α × Nat)),
      PifoGeneral.DistinctArr state → state.length = fuel →
      filterBy (Option.map color) (some target)
          (drainTree fuel (.leaf (PifoGeneral.qstate rank state))) =
        drainTree (stateFilter (fun pkt => color pkt == target) state).length
          (.leaf (PifoGeneral.qstate rank
            (stateFilter (fun pkt => color pkt == target) state))) := by
  intro fuel
  induction fuel with
  | zero =>
    intro state hd hlength
    cases state with
    | nil => rfl
    | cons entry state => simp at hlength
  | succ fuel ih =>
    intro state hd hlength
    cases state with
    | nil => simp at hlength
    | cons entry state =>
      obtain ⟨picked, hpicked, hpop, hminimum⟩ :=
        PifoGeneral.qpop_state rank entry state hd
      have hremoveLength := PifoGeneral.removeArr_length hd hpicked
      have hremaining :
          (PifoGeneral.removeArr (entry :: state) picked.2).length = fuel := by
        apply Nat.add_right_cancel (m := 1)
        exact hremoveLength.trans hlength
      have hrec := ih (PifoGeneral.removeArr (entry :: state) picked.2)
        (PifoGeneral.distinctArr_removeArr hd) hremaining
      let keep : α → Bool := fun pkt => color pkt == target
      cases hkeep : keep picked.1 with
      | false =>
        have hfilter := stateFilter_removeArr_other keep hd hpicked hkeep
        have hcolorNe : color picked.1 ≠ target := by
          exact beq_eq_false_iff_ne.mp hkeep
        have houtKeep :
            (some (color picked.1) == some target) = false :=
          beq_eq_false_iff_ne.mpr (fun h => hcolorNe (Option.some.inj h))
        simp only [drainTree, treePop, hpop, PifoGeneral.embedP,
          filterBy, List.filter, Option.map_some, houtKeep]
        rw [hfilter] at hrec
        exact hrec
      | true =>
        have hpickedFiltered : picked ∈ stateFilter keep (entry :: state) :=
          (stateFilter_mem keep).mpr ⟨hpicked, hkeep⟩
        have hdFiltered := stateFilter_distinctArr keep hd
        cases hfiltered : stateFilter keep (entry :: state) with
        | nil => rw [hfiltered] at hpickedFiltered; contradiction
        | cons filteredHead filteredTail =>
          obtain ⟨filteredPicked, hfilteredPicked, hfilteredPop,
              hfilteredMinimum⟩ :=
            PifoGeneral.qpop_state rank filteredHead filteredTail (by
              simpa [hfiltered] using hdFiltered)
          have hpickedEq : filteredPicked = picked := by
            apply Classical.byContradiction
            intro hne
            have hfilteredOriginal : filteredPicked ∈ entry :: state :=
              ((stateFilter_mem keep).mp (by
                simpa [hfiltered] using hfilteredPicked)).1
            have hforward := hminimum filteredPicked hfilteredOriginal hne
            have hbackward := hfilteredMinimum picked (by
              simpa [hfiltered] using hpickedFiltered) (Ne.symm hne)
            rw [PifoGeneral.better_iff] at hforward hbackward
            omega
          subst filteredPicked
          have hcommute := stateFilter_removeArr keep (entry :: state) picked.2
          have houtKeep :
              (some (color picked.1) == some target) = true := by
            exact beq_iff_eq.mpr (congrArg some (beq_iff_eq.mp hkeep))
          simp only [drainTree, treePop, hpop, PifoGeneral.embedP,
            filterBy, List.filter, Option.map_some, houtKeep]
          change some picked.1 :: _ =
            drainTree (filteredTail.length + 1)
              (.leaf (PifoGeneral.qstate rank (filteredHead :: filteredTail)))
          simp only [drainTree, treePop, hfilteredPop, PifoGeneral.embedP]
          rw [hfiltered] at hcommute
          have hfilteredLength :=
            PifoGeneral.removeArr_length hdFiltered hpickedFiltered
          rw [hfiltered] at hfilteredLength
          have hremainingFiltered :
              (stateFilter keep
                (PifoGeneral.removeArr (entry :: state) picked.2)).length =
                filteredTail.length := by
            rw [hcommute]
            apply Nat.add_right_cancel (m := 1)
            simpa only [List.length_cons] using hfilteredLength
          rw [← hcommute]
          rw [← hremainingFiltered]
          change some picked.1 :: filterBy (Option.map color) (some target)
              (drainTree fuel
                (.leaf (PifoGeneral.qstate rank
                  (PifoGeneral.removeArr (entry :: state) picked.2)))) = _
          exact congrArg (fun tail => some picked.1 :: tail) hrec

theorem drainLeaf_filter_state {α : Type} [DecidableEq α]
    (rank : α → Nat) (color : α → Nat) (target : Nat)
    (state : List (α × Nat)) (hd : PifoGeneral.DistinctArr state) :
    filterBy (Option.map color) (some target)
        (drainTree state.length (.leaf (PifoGeneral.qstate rank state))) =
      drainTree (stateFilter (fun pkt => color pkt == target) state).length
        (.leaf (PifoGeneral.qstate rank
          (stateFilter (fun pkt => color pkt == target) state))) :=
  drainLeaf_filter_state_aux rank color target state.length state hd rfl

def stampWordFrom {α : Type} : Nat → List α → List (α × Nat)
  | _, [] => []
  | count, value :: values =>
    (value, count + 1) :: stampWordFrom (count + 1) values

theorem stampWordFrom_length {α : Type} :
    ∀ (values : List α) (count : Nat),
      (stampWordFrom count values).length = values.length := by
  intro values
  induction values with
  | nil => intros; rfl
  | cons value values ih =>
    intro count
    simp [stampWordFrom, ih]

theorem stampWordFrom_allAbove {α : Type} :
    ∀ (values : List α) (count : Nat) (entry : α × Nat),
      entry ∈ stampWordFrom count values → count < entry.2 := by
  intro values
  induction values with
  | nil => intros; contradiction
  | cons value values ih =>
    intro count entry hmem
    simp only [stampWordFrom, List.mem_cons] at hmem
    rcases hmem with rfl | hmem
    · omega
    · have := ih (count + 1) entry hmem
      omega

theorem stampWordFrom_distinctArr {α : Type} :
    ∀ (values : List α) (count : Nat),
      PifoGeneral.DistinctArr (stampWordFrom count values) := by
  intro values
  induction values with
  | nil => intros; exact List.Pairwise.nil
  | cons value values ih =>
    intro count
    rw [stampWordFrom]
    apply List.Pairwise.cons
    · intro entry hmem heq
      have habove := stampWordFrom_allAbove values (count + 1) entry hmem
      omega
    · exact ih (count + 1)

theorem pushWordFrom_rankLeaf_queue {α : Type} (rank : α → Nat) :
    ∀ (values : List α) (count : Nat) (q : Queue α),
      pushWordFrom (fun pkt => .leaf (rank pkt)) count (.leaf q) values =
        .leaf (q ++ PifoGeneral.qstate rank (stampWordFrom count values)) := by
  intro values
  induction values with
  | nil => intros; simp [pushWordFrom, stampWordFrom, PifoGeneral.qstate]
  | cons value values ih =>
    intro count q
    simp only [pushWordFrom, treePush, stampWordFrom, PifoGeneral.qstate,
      List.map_cons]
    rw [ih]
    simp [PifoGeneral.embedP, List.append_assoc]
    rfl

theorem pushWordFrom_rankLeaf {α : Type} (rank : α → Nat)
    (values : List α) (count : Nat) :
    pushWordFrom (fun pkt => .leaf (rank pkt)) count (.leaf []) values =
      .leaf (PifoGeneral.qstate rank (stampWordFrom count values)) := by
  simpa using pushWordFrom_rankLeaf_queue rank values count []

theorem pushFilteredWordFrom_rankLeaf_queue {α : Type} (rank : α → Nat)
    (keep : α → Bool) :
    ∀ (values : List α) (count : Nat) (q : Queue α),
      pushFilteredWordFrom keep (fun pkt => .leaf (rank pkt)) count
          (.leaf q) values =
        .leaf (q ++ PifoGeneral.qstate rank
          (stateFilter keep (stampWordFrom count values))) := by
  intro values
  induction values with
  | nil => intros; simp [pushFilteredWordFrom, stampWordFrom, stateFilter,
      PifoGeneral.qstate]
  | cons value values ih =>
    intro count q
    cases hkeep : keep value with
    | false =>
      simp only [pushFilteredWordFrom, hkeep, Bool.false_eq_true, ↓reduceIte,
        stampWordFrom, stateFilter, List.filter, PifoGeneral.qstate]
      exact ih (count + 1) q
    | true =>
      simp only [pushFilteredWordFrom, hkeep, ↓reduceIte, treePush,
        stampWordFrom, stateFilter, List.filter, PifoGeneral.qstate,
        List.map_cons]
      rw [ih]
      simp [PifoGeneral.embedP, List.append_assoc]
      rfl

theorem pushFilteredWordFrom_rankLeaf {α : Type} (rank : α → Nat)
    (keep : α → Bool) (values : List α) (count : Nat) :
    pushFilteredWordFrom keep (fun pkt => .leaf (rank pkt)) count
        (.leaf []) values =
      .leaf (PifoGeneral.qstate rank
        (stateFilter keep (stampWordFrom count values))) := by
  simpa using pushFilteredWordFrom_rankLeaf_queue rank keep values count []

theorem stateFilter_stampWordFrom_length {α : Type} (keep : α → Bool) :
    ∀ (values : List α) (count : Nat),
      (stateFilter keep (stampWordFrom count values)).length =
        (values.filter keep).length := by
  intro values
  induction values with
  | nil => intros; rfl
  | cons value values ih =>
    intro count
    cases hkeep : keep value <;>
      simp only [stateFilter, stampWordFrom, hkeep, List.filter,
        List.length_cons] <;>
      simpa [stateFilter] using ih (count + 1)

theorem rankLeaf_flush_filterBy_color {k : Nat} (rank : Fin k → Nat)
    (color : Fin k → Nat) (target : Nat) (word : List (Fin k)) :
    filterBy (Option.map color) (some target)
        (run (rankLeafScheduler rank) (flushOps word)) =
      run (rankLeafScheduler rank)
        (flushOps (filterBy color target word)) := by
  let keep : Fin k → Bool := fun pkt => color pkt == target
  let state := stampWordFrom 0 word
  let filteredState := stateFilter keep state
  let pops := List.replicate word.length (.pop : TimedOp (Fin k))
  let filteredPops := List.replicate (word.filter keep).length
    (.pop : TimedOp (Fin k))
  have hstateLength : state.length = word.length := by
    exact stampWordFrom_length word 0
  have hfilteredLength : filteredState.length = (word.filter keep).length := by
    exact stateFilter_stampWordFrom_length keep word 0
  have hfull : run (rankLeafScheduler rank) (flushOps word) =
      drainTree state.length (.leaf (PifoGeneral.qstate rank state)) := by
    unfold run
    rw [runFrom_eq_runTimedFrom, timedOpsFrom_flushOps,
      runTimedFrom_timedPushes_append_zero]
    change runTimedFrom (fun pkt => .leaf (rank pkt))
      (pushWordFrom (fun pkt => .leaf (rank pkt)) 0 (.leaf []) word) pops = _
    rw [pushWordFrom_rankLeaf, runTimedFrom_pops]
    rw [hstateLength]
  have horiginalFiltered :
      runTimedFrom (fun pkt => .leaf (rank pkt)) (emptyTree .leaf)
          (filteredTimedPushes keep word ++ filteredPops) =
        drainTree filteredState.length
          (.leaf (PifoGeneral.qstate rank filteredState)) := by
    rw [runTimedFrom_filteredTimedPushes_append_zero,
      ]
    change runTimedFrom (fun pkt => .leaf (rank pkt))
      (pushFilteredWordFrom keep (fun pkt => .leaf (rank pkt)) 0
        (.leaf []) word) filteredPops = _
    rw [pushFilteredWordFrom_rankLeaf, runTimedFrom_pops]
    exact congrArg (fun count =>
      drainTree count (.leaf (PifoGeneral.qstate rank filteredState)))
      hfilteredLength.symm
  have hprojection := drainLeaf_filter_state rank color target state
    (stampWordFrom_distinctArr word 0)
  have hcompressed := filteredBatch_compress
    (fun pkt : Fin k => .leaf (rank pkt)) .leaf keep word
  have hfilteredRun : run (rankLeafScheduler rank)
      (flushOps (word.filter keep)) =
      runTimedFrom (fun pkt => .leaf (rank pkt)) (emptyTree .leaf)
        (timedPushes (word.filter keep) ++ filteredPops) := by
    unfold run
    rw [runFrom_eq_runTimedFrom, timedOpsFrom_flushOps]
    rfl
  change filterBy (Option.map color) (some target)
      (run (rankLeafScheduler rank) (flushOps word)) =
    run (rankLeafScheduler rank) (flushOps (word.filter keep))
  rw [hfull, hfilteredRun]
  rw [horiginalFiltered] at hcompressed
  exact hprojection.trans hcompressed

theorem coloredLeafColorRun {k : Nat} (rank₁ rank₂ : Fin k → Nat)
    (color : Fin k → Nat)
    (hagrees : ∀ x y, color x ≠ color y →
      PifoGeneral.Agree rank₁ rank₂ x y) (ops : List (Op k)) :
    (run (rankLeafScheduler rank₁) ops).map (Option.map color) =
      (run (rankLeafScheduler rank₂) ops).map (Option.map color) := by
  unfold run
  rw [runFrom_eq_runTimedFrom, runFrom_eq_runTimedFrom]
  exact coloredLeafTimedCongruence rank₁ rank₂ color hagrees
    (timedOpsFrom 0 ops) 0 (timedOpsFrom_above 0 ops)

theorem NormalRoot.controlRun {k : Nat} (root : NormalRoot k)
    {γ : Type} (outputColor : Fin k → γ) (decode : Nat → γ)
    (hdecode : ∀ pkt, decode (root.color pkt) = outputColor pkt)
    (ops : List (Op k)) :
    (run (rankLeafScheduler root.rank) ops).map (Option.map outputColor) =
      (run root.scheduler ops).map (Option.map outputColor) := by
  have hrun := root.colorRun ops
  have hmapped := congrArg (List.map (Option.map decode)) hrun
  have hfun : Option.map decode ∘ Option.map root.color =
      Option.map outputColor := by
    funext value
    cases value with
    | none => rfl
    | some pkt => simp [hdecode]
  simpa only [List.map_map, hfun] using hmapped

theorem commonColor_div {k : Nat} (root₁ root₂ : NormalRoot k)
    (pkt : Fin k) :
    commonColor root₁ root₂ pkt / root₂.topologies.length = root₁.color pkt := by
  have hwidth : 0 < root₂.topologies.length := by
    have := root₂.color_lt pkt
    omega
  have hsmall := Nat.div_eq_of_lt (root₂.color_lt pkt)
  simp [commonColor, Nat.mul_comm, Nat.mul_add_div hwidth, hsmall]

theorem commonColor_mod {k : Nat} (root₁ root₂ : NormalRoot k)
    (pkt : Fin k) :
    commonColor root₁ root₂ pkt % root₂.topologies.length = root₂.color pkt := by
  simp [commonColor, Nat.add_mod,
    Nat.mod_eq_of_lt (root₂.color_lt pkt)]

def pairFilterSecond {β γ : Type} [BEq β] (wanted : β) :
    List (Option β × Option γ) → List (Option γ)
  | [] => []
  | pair :: pairs =>
    if pair.1 == some wanted then pair.2 :: pairFilterSecond wanted pairs
    else pairFilterSecond wanted pairs

theorem pairFilterSecond_map {α β γ : Type} [BEq β]
    (first : α → β) (second : α → γ) (wanted : β) :
    ∀ values : List (Option α),
      pairFilterSecond wanted
          (values.map (fun value => (value.map first, value.map second))) =
        (filterBy (Option.map first) (some wanted) values).map
          (Option.map second) := by
  intro values
  induction values with
  | nil => rfl
  | cons value values ih =>
    cases hkeep : (value.map first == some wanted) <;>
      simp [pairFilterSecond, filterBy, hkeep, ih]

def splitOption {β γ : Type} : Option (β × γ) → Option β × Option γ
  | none => (none, none)
  | some pair => (some pair.1, some pair.2)

theorem splitOption_map_pair {α β γ : Type} (first : α → β)
    (second : α → γ) (value : Option α) :
    splitOption (value.map (fun pkt => (first pkt, second pkt))) =
      (value.map first, value.map second) := by
  cases value <;> rfl

theorem commonRootOne_pairControl {k : Nat} (root₁ root₂ : NormalRoot k)
    (word : List (Fin k)) :
    (run (rankLeafScheduler (commonRank root₁ root₂)) (flushOps word)).map
        (fun value => (value.map root₁.color, value.map root₂.color)) =
      (run (commonRootOne root₁ root₂).scheduler (flushOps word)).map
        (fun value => (value.map root₁.color, value.map root₂.color)) := by
  let common := commonRootOne root₁ root₂
  let decode : Nat → Nat × Nat := fun code =>
    (code / root₂.topologies.length, code % root₂.topologies.length)
  have hcontrol := common.controlRun
    (fun pkt => (root₁.color pkt, root₂.color pkt)) decode (by
      intro pkt
      exact Prod.ext (commonColor_div root₁ root₂ pkt)
        (commonColor_mod root₁ root₂ pkt)) (flushOps word)
  have hsplit := congrArg (List.map splitOption) hcontrol
  have hfun : splitOption ∘ Option.map
      (fun pkt => (root₁.color pkt, root₂.color pkt)) =
      (fun value => (value.map root₁.color, value.map root₂.color)) := by
    funext value
    exact splitOption_map_pair root₁.color root₂.color value
  rw [List.map_map, hfun, List.map_map, hfun] at hsplit
  change (run (rankLeafScheduler (commonRank root₁ root₂))
      (flushOps word)).map
        (fun value => (value.map root₁.color, value.map root₂.color)) = _
    at hsplit
  exact hsplit

theorem commonRootTwo_pairControl {k : Nat} (root₁ root₂ : NormalRoot k)
    (word : List (Fin k)) :
    (run (rankLeafScheduler (commonRank root₁ root₂)) (flushOps word)).map
        (fun value => (value.map root₁.color, value.map root₂.color)) =
      (run (commonRootTwo root₁ root₂).scheduler (flushOps word)).map
        (fun value => (value.map root₁.color, value.map root₂.color)) := by
  let common := commonRootTwo root₁ root₂
  let decode : Nat → Nat × Nat := fun code =>
    (code / root₂.topologies.length, code % root₂.topologies.length)
  have hcontrol := common.controlRun
    (fun pkt => (root₁.color pkt, root₂.color pkt)) decode (by
      intro pkt
      exact Prod.ext (commonColor_div root₁ root₂ pkt)
        (commonColor_mod root₁ root₂ pkt)) (flushOps word)
  have hsplit := congrArg (List.map splitOption) hcontrol
  have hfun : splitOption ∘ Option.map
      (fun pkt => (root₁.color pkt, root₂.color pkt)) =
      (fun value => (value.map root₁.color, value.map root₂.color)) := by
    funext value
    exact splitOption_map_pair root₁.color root₂.color value
  rw [List.map_map, hfun, List.map_map, hfun] at hsplit
  change (run (rankLeafScheduler (commonRank root₁ root₂))
      (flushOps word)).map
        (fun value => (value.map root₁.color, value.map root₂.color)) = _
    at hsplit
  exact hsplit

theorem map_pair_eq_of_map_eq_of_filterBy {α β γ : Type}
    [BEq β] [LawfulBEq β] (first : α → β) (second : α → γ) :
    ∀ (xs ys : List α),
      xs.map first = ys.map first →
      (∀ value, (filterBy first value xs).map second =
        (filterBy first value ys).map second) →
      xs.map (fun x => (first x, second x)) =
        ys.map (fun y => (first y, second y)) := by
  intro xs
  induction xs with
  | nil =>
    intro ys hfirst hfilters
    cases ys with
    | nil => rfl
    | cons y ys => simp at hfirst
  | cons x xs ih =>
    intro ys hfirst hfilters
    cases ys with
    | nil => simp at hfirst
    | cons y ys =>
      simp only [List.map_cons, List.cons.injEq] at hfirst
      have hxy : first x = first y := hfirst.1
      have hsecond := hfilters (first x)
      have hx : (first x == first x) = true := beq_iff_eq.mpr rfl
      have hy : (first y == first x) = true := beq_iff_eq.mpr hxy.symm
      simp only [filterBy, List.filter, hx, hy, List.map_cons,
        List.cons.injEq] at hsecond
      have htail := ih ys hfirst.2 (by
        intro value
        have hfilter := hfilters value
        by_cases hv : first x = value
        · have hxv : (first x == value) = true := beq_iff_eq.mpr hv
          have hyv : (first y == value) = true := beq_iff_eq.mpr (hxy.symm.trans hv)
          simp only [filterBy, List.filter, hxv, hyv, List.map_cons,
            List.cons.injEq] at hfilter
          exact hfilter.2
        · have hxv : (first x == value) = false := beq_eq_false_iff_ne.mpr hv
          have hyv : (first y == value) = false := beq_eq_false_iff_ne.mpr
            (fun h => hv (hxy.trans h))
          simpa only [filterBy, List.filter, hxv, hyv] using hfilter)
      simp only [List.map_cons, hxy, hsecond.1]
      exact congrArg (fun tail => (first y, second y) :: tail) htail

theorem rootOne_commonRootOne_control {k : Nat}
    (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler)
    (word : List (Fin k)) :
    (run root₁.scheduler (flushOps word)).map
        (fun value => (value.map root₁.color, value.map root₂.color)) =
      (run (commonRootOne root₁ root₂).scheduler (flushOps word)).map
        (fun value => (value.map root₁.color, value.map root₂.color)) := by
  let common := commonRootOne root₁ root₂
  let rank := commonRank root₁ root₂
  let leaf := rankLeafScheduler rank
  have hcommon := commonRootOne_pairControl root₁ root₂ word
  have hcommonFirst :
      (run leaf (flushOps word)).map (Option.map root₁.color) =
        (run common.scheduler (flushOps word)).map (Option.map root₁.color) := by
    have hmapped := congrArg (List.map Prod.fst) hcommon
    have hfun : Prod.fst ∘
        (fun value : Option (Fin k) =>
          (value.map root₁.color, value.map root₂.color)) =
        Option.map root₁.color := by
      rfl
    simpa [List.map_map, hfun, leaf, rank, common] using hmapped
  have hleafFirst := coloredLeafColorRun root₁.rank rank root₁.color
    (rootOne_commonRank_agree root₁ root₂
      (commonLe_noStrictBackPath root₁ root₂ hflush)) (flushOps word)
  have hfirst :
      (run root₁.scheduler (flushOps word)).map (Option.map root₁.color) =
        (run common.scheduler (flushOps word)).map (Option.map root₁.color) :=
    (root₁.colorRun (flushOps word)).symm.trans
      (hleafFirst.trans hcommonFirst)
  apply map_pair_eq_of_map_eq_of_filterBy
    (Option.map root₁.color) (Option.map root₂.color)
  · exact hfirst
  · intro value
    cases value with
    | none =>
      rw [filterBy_none_valid_flush root₁.scheduler root₁.valid root₁.color,
        filterBy_none_valid_flush common.scheduler common.valid root₁.color]
    | some block =>
      by_cases hmember : ∃ member, root₁.color member = block
      · obtain ⟨member, hmember⟩ := hmember
        let blockWord := filterBy root₁.color block word
        have hproject₁ :
            filterBy (Option.map root₁.color) (some block)
                (run root₁.scheduler (flushOps word)) =
              run root₁.scheduler (flushOps blockWord) := by
          simpa [blockWord, hmember] using
            root₁.flush_filterBy_color member word
        have hprojectLeaf := rankLeaf_flush_filterBy_color rank root₁.color
          block word
        have hcommonBlock := congrArg (pairFilterSecond block) hcommon
        have hcommonBlock' :
            (filterBy (Option.map root₁.color) (some block)
              (run leaf (flushOps word))).map (Option.map root₂.color) =
            (filterBy (Option.map root₁.color) (some block)
              (run common.scheduler (flushOps word))).map
                (Option.map root₂.color) := by
          simpa [pairFilterSecond_map, leaf, common] using hcommonBlock
        have hleft :
            (filterBy (Option.map root₁.color) (some block)
              (run root₁.scheduler (flushOps word))).map
                (Option.map root₂.color) =
              (run leaf (flushOps blockWord)).map
                (Option.map root₂.color) := by
          have hproject₁' := congrArg (List.map (Option.map root₂.color)) hproject₁
          have hflushBlock := congrArg (List.map (Option.map root₂.color))
            (hflush blockWord)
          have hroot₂Color := root₂.colorRun (flushOps blockWord)
          have hleafColor := coloredLeafColorRun root₂.rank rank root₂.color
            (rootTwo_commonRank_agree root₁ root₂ hflush
              (commonLe_noStrictBackPath root₁ root₂ hflush))
            (flushOps blockWord)
          simpa using
            hproject₁'.trans (hflushBlock.trans
              (hroot₂Color.symm.trans hleafColor))
        have hright :
            (filterBy (Option.map root₁.color) (some block)
              (run common.scheduler (flushOps word))).map
                (Option.map root₂.color) =
              (run leaf (flushOps blockWord)).map
                (Option.map root₂.color) := by
          have hprojectLeaf' := congrArg (List.map (Option.map root₂.color))
            hprojectLeaf
          exact hcommonBlock'.symm.trans (by
            simpa [blockWord, filterBy] using hprojectLeaf')
        simpa [common] using hleft.trans hright.symm
      · rw [filterBy_some_valid_flush_of_not_exists root₁.scheduler root₁.valid
          root₁.color word block hmember,
        filterBy_some_valid_flush_of_not_exists common.scheduler common.valid
          root₁.color word block hmember]

def swapPair {α β : Type} (pair : α × β) : β × α := (pair.2, pair.1)

theorem rootTwo_commonRootTwo_control {k : Nat}
    (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler)
    (word : List (Fin k)) :
    (run root₂.scheduler (flushOps word)).map
        (fun value => (value.map root₁.color, value.map root₂.color)) =
      (run (commonRootTwo root₁ root₂).scheduler (flushOps word)).map
        (fun value => (value.map root₁.color, value.map root₂.color)) := by
  let common := commonRootTwo root₁ root₂
  let rank := commonRank root₁ root₂
  let leaf := rankLeafScheduler rank
  have hcommon := commonRootTwo_pairControl root₁ root₂ word
  have hcommonSwap :
      (run leaf (flushOps word)).map
          (fun value => (value.map root₂.color, value.map root₁.color)) =
        (run common.scheduler (flushOps word)).map
          (fun value => (value.map root₂.color, value.map root₁.color)) := by
    have hmapped := congrArg (List.map swapPair) hcommon
    have hfun : swapPair ∘
        (fun value : Option (Fin k) =>
          (value.map root₁.color, value.map root₂.color)) =
        (fun value => (value.map root₂.color, value.map root₁.color)) := by rfl
    simpa [List.map_map, hfun, leaf, rank, common] using hmapped
  have hcommonFirst :
      (run leaf (flushOps word)).map (Option.map root₂.color) =
        (run common.scheduler (flushOps word)).map (Option.map root₂.color) := by
    have hmapped := congrArg (List.map Prod.fst) hcommonSwap
    have hfun : Prod.fst ∘
        (fun value : Option (Fin k) =>
          (value.map root₂.color, value.map root₁.color)) =
        Option.map root₂.color := by rfl
    simpa [List.map_map, hfun] using hmapped
  have hleafFirst := coloredLeafColorRun root₂.rank rank root₂.color
    (rootTwo_commonRank_agree root₁ root₂ hflush
      (commonLe_noStrictBackPath root₁ root₂ hflush)) (flushOps word)
  have hfirst :
      (run root₂.scheduler (flushOps word)).map (Option.map root₂.color) =
        (run common.scheduler (flushOps word)).map (Option.map root₂.color) :=
    (root₂.colorRun (flushOps word)).symm.trans
      (hleafFirst.trans hcommonFirst)
  have hreversed :
      (run root₂.scheduler (flushOps word)).map
          (fun value => (value.map root₂.color, value.map root₁.color)) =
        (run common.scheduler (flushOps word)).map
          (fun value => (value.map root₂.color, value.map root₁.color)) := by
    apply map_pair_eq_of_map_eq_of_filterBy
      (Option.map root₂.color) (Option.map root₁.color)
    · exact hfirst
    · intro value
      cases value with
      | none =>
        rw [filterBy_none_valid_flush root₂.scheduler root₂.valid root₂.color,
          filterBy_none_valid_flush common.scheduler common.valid root₂.color]
      | some block =>
        by_cases hmember : ∃ member, root₂.color member = block
        · obtain ⟨member, hmember⟩ := hmember
          let blockWord := filterBy root₂.color block word
          have hproject₂ :
              filterBy (Option.map root₂.color) (some block)
                  (run root₂.scheduler (flushOps word)) =
                run root₂.scheduler (flushOps blockWord) := by
            simpa [blockWord, hmember] using
              root₂.flush_filterBy_color member word
          have hprojectLeaf := rankLeaf_flush_filterBy_color rank root₂.color
            block word
          have hcommonBlock := congrArg (pairFilterSecond block) hcommonSwap
          have hcommonBlock' :
              (filterBy (Option.map root₂.color) (some block)
                (run leaf (flushOps word))).map (Option.map root₁.color) =
              (filterBy (Option.map root₂.color) (some block)
                (run common.scheduler (flushOps word))).map
                  (Option.map root₁.color) := by
            simpa [pairFilterSecond_map, leaf, common] using hcommonBlock
          have hleft :
              (filterBy (Option.map root₂.color) (some block)
                (run root₂.scheduler (flushOps word))).map
                  (Option.map root₁.color) =
                (run leaf (flushOps blockWord)).map
                  (Option.map root₁.color) := by
            have hproject₂' := congrArg (List.map (Option.map root₁.color))
              hproject₂
            have hflushBlock := congrArg (List.map (Option.map root₁.color))
              (hflush blockWord).symm
            have hroot₁Color := root₁.colorRun (flushOps blockWord)
            have hleafColor := coloredLeafColorRun root₁.rank rank root₁.color
              (rootOne_commonRank_agree root₁ root₂
                (commonLe_noStrictBackPath root₁ root₂ hflush))
              (flushOps blockWord)
            simpa using hproject₂'.trans (hflushBlock.trans
              (hroot₁Color.symm.trans hleafColor))
          have hright :
              (filterBy (Option.map root₂.color) (some block)
                (run common.scheduler (flushOps word))).map
                  (Option.map root₁.color) =
                (run leaf (flushOps blockWord)).map
                  (Option.map root₁.color) := by
            have hprojectLeaf' := congrArg (List.map (Option.map root₁.color))
              hprojectLeaf
            exact hcommonBlock'.symm.trans (by
              simpa [blockWord, filterBy] using hprojectLeaf')
          simpa [common] using hleft.trans hright.symm
        · rw [filterBy_some_valid_flush_of_not_exists root₂.scheduler root₂.valid
            root₂.color word block hmember,
          filterBy_some_valid_flush_of_not_exists common.scheduler common.valid
            root₂.color word block hmember]
  have hmapped := congrArg (List.map swapPair) hreversed
  have hfun : swapPair ∘
      (fun value : Option (Fin k) =>
        (value.map root₂.color, value.map root₁.color)) =
      (fun value => (value.map root₁.color, value.map root₂.color)) := by rfl
  simpa [List.map_map, hfun, common] using hmapped

def combineOptionPair (width : Nat) : Option Nat × Option Nat → Option Nat
  | (some first, some second) => some (first * width + second)
  | _ => none

theorem combineOptionPair_commonColor {k : Nat}
    (root₁ root₂ : NormalRoot k) (value : Option (Fin k)) :
    combineOptionPair root₂.topologies.length
        (value.map root₁.color, value.map root₂.color) =
      value.map (commonColor root₁ root₂) := by
  cases value <;> rfl

theorem filterBy_commonColor_nested {k : Nat}
    (root₁ root₂ : NormalRoot k) (member : Fin k) :
    ∀ values : List (Fin k),
      filterBy (commonColor root₁ root₂) (commonColor root₁ root₂ member) values =
        filterBy root₂.color (root₂.color member)
          (filterBy root₁.color (root₁.color member) values) := by
  intro values
  unfold filterBy
  rw [List.filter_filter]
  apply List.filter_congr
  intro pkt hmem
  by_cases h₁ : root₁.color pkt = root₁.color member <;>
    by_cases h₂ : root₂.color pkt = root₂.color member
  · have hc := (commonColor_eq_iff root₁ root₂ pkt member).mpr ⟨h₁, h₂⟩
    rw [beq_iff_eq.mpr hc, beq_iff_eq.mpr h₂, beq_iff_eq.mpr h₁]
    rfl
  · have hc : commonColor root₁ root₂ pkt ≠ commonColor root₁ root₂ member :=
      fun h => h₂ ((commonColor_eq_iff root₁ root₂ pkt member).mp h).2
    rw [beq_eq_false_iff_ne.mpr hc, beq_eq_false_iff_ne.mpr h₂,
      beq_iff_eq.mpr h₁]
    rfl
  · have hc : commonColor root₁ root₂ pkt ≠ commonColor root₁ root₂ member :=
      fun h => h₁ ((commonColor_eq_iff root₁ root₂ pkt member).mp h).1
    rw [beq_eq_false_iff_ne.mpr hc, beq_iff_eq.mpr h₂,
      beq_eq_false_iff_ne.mpr h₁]
    rfl
  · have hc : commonColor root₁ root₂ pkt ≠ commonColor root₁ root₂ member :=
      fun h => h₁ ((commonColor_eq_iff root₁ root₂ pkt member).mp h).1
    rw [beq_eq_false_iff_ne.mpr hc, beq_eq_false_iff_ne.mpr h₂,
      beq_eq_false_iff_ne.mpr h₁]
    rfl

theorem filterBy_option_commonColor_nested {k : Nat}
    (root₁ root₂ : NormalRoot k) (member : Fin k) :
    ∀ values : List (Option (Fin k)),
      filterBy (Option.map (commonColor root₁ root₂))
          (some (commonColor root₁ root₂ member)) values =
        filterBy (Option.map root₂.color) (some (root₂.color member))
          (filterBy (Option.map root₁.color) (some (root₁.color member)) values) := by
  intro values
  unfold filterBy
  rw [List.filter_filter]
  apply List.filter_congr
  intro value hmem
  cases value with
  | none => rfl
  | some pkt =>
    simp only [Option.map_some]
    by_cases h₁ : root₁.color pkt = root₁.color member <;>
      by_cases h₂ : root₂.color pkt = root₂.color member
    · have hc := (commonColor_eq_iff root₁ root₂ pkt member).mpr ⟨h₁, h₂⟩
      rw [beq_iff_eq.mpr (congrArg some hc),
        beq_iff_eq.mpr (congrArg some h₂),
        beq_iff_eq.mpr (congrArg some h₁)]
      rfl
    · have hc : commonColor root₁ root₂ pkt ≠ commonColor root₁ root₂ member :=
        fun h => h₂ ((commonColor_eq_iff root₁ root₂ pkt member).mp h).2
      rw [beq_eq_false_iff_ne.mpr (fun h => hc (Option.some.inj h)),
        beq_eq_false_iff_ne.mpr (fun h => h₂ (Option.some.inj h)),
        beq_iff_eq.mpr (congrArg some h₁)]
      rfl
    · have hc : commonColor root₁ root₂ pkt ≠ commonColor root₁ root₂ member :=
        fun h => h₁ ((commonColor_eq_iff root₁ root₂ pkt member).mp h).1
      rw [beq_eq_false_iff_ne.mpr (fun h => hc (Option.some.inj h)),
        beq_iff_eq.mpr (congrArg some h₂),
        beq_eq_false_iff_ne.mpr (fun h => h₁ (Option.some.inj h))]
      rfl
    · have hc : commonColor root₁ root₂ pkt ≠ commonColor root₁ root₂ member :=
        fun h => h₁ ((commonColor_eq_iff root₁ root₂ pkt member).mp h).1
      rw [beq_eq_false_iff_ne.mpr (fun h => hc (Option.some.inj h)),
        beq_eq_false_iff_ne.mpr (fun h => h₂ (Option.some.inj h)),
        beq_eq_false_iff_ne.mpr (fun h => h₁ (Option.some.inj h))]
      rfl

theorem filterBy_commonColor_nested_rev {k : Nat}
    (root₁ root₂ : NormalRoot k) (member : Fin k) (values : List (Fin k)) :
    filterBy (commonColor root₁ root₂) (commonColor root₁ root₂ member) values =
      filterBy root₁.color (root₁.color member)
        (filterBy root₂.color (root₂.color member) values) := by
  rw [filterBy_commonColor_nested root₁ root₂ member]
  unfold filterBy
  rw [List.filter_filter, List.filter_filter]
  apply List.filter_congr
  intro pkt hmem
  exact Bool.and_comm _ _

theorem filterBy_option_commonColor_nested_rev {k : Nat}
    (root₁ root₂ : NormalRoot k) (member : Fin k)
    (values : List (Option (Fin k))) :
    filterBy (Option.map (commonColor root₁ root₂))
        (some (commonColor root₁ root₂ member)) values =
      filterBy (Option.map root₁.color) (some (root₁.color member))
        (filterBy (Option.map root₂.color) (some (root₂.color member)) values) := by
  rw [filterBy_option_commonColor_nested root₁ root₂ member]
  unfold filterBy
  rw [List.filter_filter, List.filter_filter]
  apply List.filter_congr
  intro pkt hmem
  exact Bool.and_comm _ _

theorem commonRootOne_fullChildScheduler_eq {k : Nat}
    (root₁ root₂ : NormalRoot k) (member : Fin k) :
    (commonRootOne root₁ root₂).fullChildScheduler member =
      root₁.fullChildScheduler member := by
  rw [Scheduler.mk.injEq]
  constructor
  · exact commonRootOne_childTopology_eq root₁ root₂ member
  · rfl

theorem commonRootTwo_fullChildScheduler_eq {k : Nat}
    (root₁ root₂ : NormalRoot k) (member : Fin k) :
    (commonRootTwo root₁ root₂).fullChildScheduler member =
      root₂.fullChildScheduler member := by
  rw [Scheduler.mk.injEq]
  constructor
  · exact commonRootTwo_childTopology_eq root₁ root₂ member
  · rfl

theorem rootOne_commonRootOne_flushEquiv {k : Nat}
    (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler) :
    flushEquiv root₁.scheduler (commonRootOne root₁ root₂).scheduler := by
  intro word
  let common := commonRootOne root₁ root₂
  let color := commonColor root₁ root₂
  have hcontrol := rootOne_commonRootOne_control root₁ root₂ hflush word
  have hcolor :
      (run root₁.scheduler (flushOps word)).map (Option.map color) =
        (run common.scheduler (flushOps word)).map (Option.map color) := by
    have hmapped := congrArg
      (List.map (combineOptionPair root₂.topologies.length)) hcontrol
    have hfun : combineOptionPair root₂.topologies.length ∘
        (fun value : Option (Fin k) =>
          (value.map root₁.color, value.map root₂.color)) =
        Option.map color := by
      funext value
      exact combineOptionPair_commonColor root₁ root₂ value
    simpa [List.map_map, hfun, color, common] using hmapped
  have hpair := map_pair_eq_of_map_eq_of_filterBy
    (Option.map color) (fun value : Option (Fin k) => value)
    (run root₁.scheduler (flushOps word))
    (run common.scheduler (flushOps word)) hcolor (by
      intro value
      cases value with
      | none =>
        rw [filterBy_none_valid_flush root₁.scheduler root₁.valid color,
          filterBy_none_valid_flush common.scheduler common.valid color]
      | some cell =>
        by_cases hmember : ∃ member, color member = cell
        · obtain ⟨member, hmember⟩ := hmember
          let blockWord := filterBy root₁.color (root₁.color member) word
          let cellWord := filterBy color cell word
          have hproject₁ := root₁.flush_filterBy_color member word
          have hproject₂ := root₂.flush_filterBy_color member blockWord
          have hrootCell :
              filterBy (Option.map color) (some cell)
                  (run root₁.scheduler (flushOps word)) =
                run root₁.scheduler (flushOps cellWord) := by
            calc
              _ = filterBy (Option.map root₂.color) (some (root₂.color member))
                    (filterBy (Option.map root₁.color)
                      (some (root₁.color member))
                      (run root₁.scheduler (flushOps word))) := by
                    simpa [color, hmember] using
                      filterBy_option_commonColor_nested root₁ root₂ member
                        (run root₁.scheduler (flushOps word))
              _ = filterBy (Option.map root₂.color) (some (root₂.color member))
                    (run root₁.scheduler (flushOps blockWord)) := by
                    rw [hproject₁]
              _ = filterBy (Option.map root₂.color) (some (root₂.color member))
                    (run root₂.scheduler (flushOps blockWord)) := by
                    rw [hflush blockWord]
              _ = run root₂.scheduler (flushOps cellWord) := by
                    rw [hproject₂]
                    congr 2
                    simpa [blockWord, cellWord, color, hmember] using
                      (filterBy_commonColor_nested root₁ root₂ member word).symm
              _ = run root₁.scheduler (flushOps cellWord) :=
                    (hflush cellWord).symm
          have hcellWord : ∀ pkt, pkt ∈ cellWord →
              color pkt = color member := by
            intro pkt hmem
            have hp := (List.mem_filter.mp hmem).2
            exact (beq_iff_eq.mp hp).trans hmember.symm
          have hrootWord : ∀ pkt, pkt ∈ cellWord →
              root₁.color pkt = root₁.color member := by
            intro pkt hmem
            exact ((commonColor_eq_iff root₁ root₂ pkt member).mp
              (hcellWord pkt hmem)).1
          have hcommonCell := common.flushBlockFull member cellWord hcellWord
          have hrootBlock := root₁.flushBlockFull member cellWord hrootWord
          have hcommonProject :
              filterBy (Option.map color) (some cell)
                  (run common.scheduler (flushOps word)) =
                run common.scheduler (flushOps cellWord) := by
            have hp := common.flush_filterBy_color member word
            have hcommonColor : common.color = color := rfl
            rw [hcommonColor] at hp
            simpa [cellWord, hmember] using hp
          have hcellRuns : run root₁.scheduler (flushOps cellWord) =
              run common.scheduler (flushOps cellWord) := by
            rw [hrootBlock, hcommonCell,
              commonRootOne_fullChildScheduler_eq root₁ root₂ member]
          have hcellEq := hrootCell.trans (hcellRuns.trans
            hcommonProject.symm)
          simpa using congrArg (List.map id) hcellEq
        · rw [filterBy_some_valid_flush_of_not_exists root₁.scheduler root₁.valid
            color word cell hmember,
          filterBy_some_valid_flush_of_not_exists common.scheduler common.valid
            color word cell hmember])
  have hmapped := congrArg (List.map Prod.snd) hpair
  have hfun : Prod.snd ∘
      (fun value : Option (Fin k) => (value.map color, value)) = id := by rfl
  simpa [List.map_map, hfun, common] using hmapped

theorem rootTwo_commonRootTwo_flushEquiv {k : Nat}
    (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler) :
    flushEquiv root₂.scheduler (commonRootTwo root₁ root₂).scheduler := by
  intro word
  let common := commonRootTwo root₁ root₂
  let color := commonColor root₁ root₂
  have hcontrol := rootTwo_commonRootTwo_control root₁ root₂ hflush word
  have hcolor :
      (run root₂.scheduler (flushOps word)).map (Option.map color) =
        (run common.scheduler (flushOps word)).map (Option.map color) := by
    have hmapped := congrArg
      (List.map (combineOptionPair root₂.topologies.length)) hcontrol
    have hfun : combineOptionPair root₂.topologies.length ∘
        (fun value : Option (Fin k) =>
          (value.map root₁.color, value.map root₂.color)) =
        Option.map color := by
      funext value
      exact combineOptionPair_commonColor root₁ root₂ value
    simpa [List.map_map, hfun, color, common] using hmapped
  have hpair := map_pair_eq_of_map_eq_of_filterBy
    (Option.map color) (fun value : Option (Fin k) => value)
    (run root₂.scheduler (flushOps word))
    (run common.scheduler (flushOps word)) hcolor (by
      intro value
      cases value with
      | none =>
        rw [filterBy_none_valid_flush root₂.scheduler root₂.valid color,
          filterBy_none_valid_flush common.scheduler common.valid color]
      | some cell =>
        by_cases hmember : ∃ member, color member = cell
        · obtain ⟨member, hmember⟩ := hmember
          let blockWord := filterBy root₂.color (root₂.color member) word
          let cellWord := filterBy color cell word
          have hproject₂ := root₂.flush_filterBy_color member word
          have hproject₁ := root₁.flush_filterBy_color member blockWord
          have hrootCell :
              filterBy (Option.map color) (some cell)
                  (run root₂.scheduler (flushOps word)) =
                run root₂.scheduler (flushOps cellWord) := by
            calc
              _ = filterBy (Option.map root₁.color) (some (root₁.color member))
                    (filterBy (Option.map root₂.color)
                      (some (root₂.color member))
                      (run root₂.scheduler (flushOps word))) := by
                    simpa [color, hmember] using
                      filterBy_option_commonColor_nested_rev root₁ root₂ member
                        (run root₂.scheduler (flushOps word))
              _ = filterBy (Option.map root₁.color) (some (root₁.color member))
                    (run root₂.scheduler (flushOps blockWord)) := by
                    rw [hproject₂]
              _ = filterBy (Option.map root₁.color) (some (root₁.color member))
                    (run root₁.scheduler (flushOps blockWord)) := by
                    rw [(hflush blockWord).symm]
              _ = run root₁.scheduler (flushOps cellWord) := by
                    rw [hproject₁]
                    congr 2
                    simpa [blockWord, cellWord, color, hmember] using
                      (filterBy_commonColor_nested_rev root₁ root₂ member word).symm
              _ = run root₂.scheduler (flushOps cellWord) := hflush cellWord
          have hcellWord : ∀ pkt, pkt ∈ cellWord →
              color pkt = color member := by
            intro pkt hmem
            have hp := (List.mem_filter.mp hmem).2
            exact (beq_iff_eq.mp hp).trans hmember.symm
          have hrootWord : ∀ pkt, pkt ∈ cellWord →
              root₂.color pkt = root₂.color member := by
            intro pkt hmem
            exact ((commonColor_eq_iff root₁ root₂ pkt member).mp
              (hcellWord pkt hmem)).2
          have hcommonCell := common.flushBlockFull member cellWord hcellWord
          have hrootBlock := root₂.flushBlockFull member cellWord hrootWord
          have hcommonProject :
              filterBy (Option.map color) (some cell)
                  (run common.scheduler (flushOps word)) =
                run common.scheduler (flushOps cellWord) := by
            have hp := common.flush_filterBy_color member word
            have hcommonColor : common.color = color := rfl
            rw [hcommonColor] at hp
            simpa [cellWord, hmember] using hp
          have hcellRuns : run root₂.scheduler (flushOps cellWord) =
              run common.scheduler (flushOps cellWord) := by
            rw [hrootBlock, hcommonCell,
              commonRootTwo_fullChildScheduler_eq root₁ root₂ member]
          have hcellEq := hrootCell.trans (hcellRuns.trans
            hcommonProject.symm)
          simpa using congrArg (List.map id) hcellEq
        · rw [filterBy_some_valid_flush_of_not_exists root₂.scheduler root₂.valid
            color word cell hmember,
          filterBy_some_valid_flush_of_not_exists common.scheduler common.valid
            color word cell hmember])
  have hmapped := congrArg (List.map Prod.snd) hpair
  have hfun : Prod.snd ∘
      (fun value : Option (Fin k) => (value.map color, value)) = id := by rfl
  simpa [List.map_map, hfun, common] using hmapped

def eraseTimedOps {k : Nat} : List (TimedOp (Fin k)) → List (Op k)
  | [] => []
  | .push pkt _ :: ops => .push pkt :: eraseTimedOps ops
  | .pop :: ops => .pop :: eraseTimedOps ops

def compressTimedFrom {α : Type} : Nat → List (TimedOp α) → List (TimedOp α)
  | _, [] => []
  | count, .push pkt _ :: ops =>
    .push pkt (count + 1) :: compressTimedFrom (count + 1) ops
  | count, .pop :: ops => .pop :: compressTimedFrom count ops

def TimedBy {α : Type} (rearrange : Nat → Nat) :
    Nat → List (TimedOp α) → Prop
  | _, [] => True
  | count, .push _ arrival :: ops =>
    arrival = rearrange (count + 1) ∧ TimedBy rearrange (count + 1) ops
  | count, .pop :: ops => TimedBy rearrange count ops

theorem mapTimedArr_compressTimedFrom {α : Type} (rearrange : Nat → Nat) :
    ∀ (ops : List (TimedOp α)) (count : Nat), TimedBy rearrange count ops →
      mapTimedArr rearrange (compressTimedFrom count ops) = ops := by
  intro ops
  induction ops with
  | nil => intros; rfl
  | cons op ops ih =>
    intro count htimed
    cases op with
    | push pkt arrival =>
      simp only [TimedBy] at htimed
      simp [compressTimedFrom, mapTimedArr, htimed.1, ih (count + 1) htimed.2]
    | pop => simp [compressTimedFrom, mapTimedArr, ih count htimed]

theorem timedOpsFrom_eraseTimedOps {k : Nat} :
    ∀ (ops : List (TimedOp (Fin k))) (count : Nat),
      timedOpsFrom count (eraseTimedOps ops) = compressTimedFrom count ops := by
  intro ops
  induction ops with
  | nil => intros; rfl
  | cons op ops ih =>
    intro count
    cases op with
    | push pkt arrival =>
      simp [eraseTimedOps, timedOpsFrom, compressTimedFrom, ih]
    | pop => simp [eraseTimedOps, timedOpsFrom, compressTimedFrom, ih]

theorem interEquiv_runTimedFrom {k : Nat} {S₁ S₂ : Scheduler k}
    (hinter : interEquiv S₁ S₂) (rearrange : Nat → Nat)
    (hrearrange : ∀ left right,
      left < right ↔ rearrange left < rearrange right)
    (ops : List (TimedOp (Fin k))) (htimed : TimedBy rearrange 0 ops) :
    runTimedFrom S₁.assign (emptyTree S₁.topo) ops =
      runTimedFrom S₂.assign (emptyTree S₂.topo) ops := by
  have hcompressed := hinter (eraseTimedOps ops)
  unfold run at hcompressed
  rw [runFrom_eq_runTimedFrom, runFrom_eq_runTimedFrom,
    timedOpsFrom_eraseTimedOps] at hcompressed
  have hmap := mapTimedArr_compressTimedFrom rearrange ops 0 htimed
  have hrun₁ := runTimedFrom_reArr S₁.assign rearrange hrearrange
    (compressTimedFrom 0 ops) (emptyTree S₁.topo)
  have hrun₂ := runTimedFrom_reArr S₂.assign rearrange hrearrange
    (compressTimedFrom 0 ops) (emptyTree S₂.topo)
  rw [reArrTree_emptyTree, hmap] at hrun₁ hrun₂
  exact hrun₁.trans (hcompressed.trans hrun₂.symm)



def TimedAfter {α : Type} : Nat → List (TimedOp α) → Prop
  | _, [] => True
  | bound, .push _ arrival :: ops =>
      bound < arrival ∧ TimedAfter arrival ops
  | bound, .pop :: ops => TimedAfter bound ops

def timedArrivals {α : Type} : List (TimedOp α) → List Nat
  | [] => []
  | .push _ arrival :: ops => arrival :: timedArrivals ops
  | .pop :: ops => timedArrivals ops

def ArrivalsAfter : Nat → List Nat → Prop
  | _, [] => True
  | bound, arrival :: arrivals =>
      bound < arrival ∧ ArrivalsAfter arrival arrivals

def arrivalEmbeddingFrom : Nat → List Nat → Nat → Nat
  | bound, [], timestamp => bound + timestamp
  | bound, _ :: _, 0 => bound
  | _, arrival :: arrivals, timestamp + 1 =>
      arrivalEmbeddingFrom arrival arrivals timestamp

theorem arrivalEmbeddingFrom_zero (bound : Nat) (arrivals : List Nat) :
    arrivalEmbeddingFrom bound arrivals 0 = bound := by
  cases arrivals <;> rfl

theorem timedAfter_arrivalsAfter {α : Type} :
    ∀ (ops : List (TimedOp α)) (bound : Nat), TimedAfter bound ops →
      ArrivalsAfter bound (timedArrivals ops) := by
  intro ops
  induction ops with
  | nil => intros; trivial
  | cons op ops ih =>
      intro bound htimed
      cases op with
      | push pkt arrival =>
        exact ⟨htimed.1, ih arrival htimed.2⟩
      | pop => exact ih bound htimed

theorem arrivalEmbeddingFrom_succ_lt (bound : Nat) :
    ∀ (arrivals : List Nat), ArrivalsAfter bound arrivals →
      ∀ timestamp,
        arrivalEmbeddingFrom bound arrivals timestamp <
          arrivalEmbeddingFrom bound arrivals (timestamp + 1) := by
  intro arrivals
  induction arrivals generalizing bound with
  | nil =>
      intro h timestamp
      simp [arrivalEmbeddingFrom]
  | cons arrival arrivals ih =>
      intro h timestamp
      cases timestamp with
      | zero =>
        calc
          arrivalEmbeddingFrom bound (arrival :: arrivals) 0 = bound :=
            arrivalEmbeddingFrom_zero bound _
          _ < arrival := h.1
          _ = arrivalEmbeddingFrom bound (arrival :: arrivals) 1 := by
            simp only [arrivalEmbeddingFrom]
            exact (arrivalEmbeddingFrom_zero arrival arrivals).symm
      | succ timestamp =>
        simpa [arrivalEmbeddingFrom] using ih arrival h.2 timestamp

theorem lt_of_succ_lt_values (f : Nat → Nat)
    (hsucc : ∀ n, f n < f (n + 1)) :
    ∀ left right, left < right → f left < f right := by
  intro left right hlt
  induction right with
  | zero => omega
  | succ right ih =>
      by_cases heq : left = right
      · subst left
        simpa only [Nat.succ_eq_add_one] using hsucc right
      · exact Nat.lt_trans (ih (by omega)) (by
          simpa only [Nat.succ_eq_add_one] using hsucc right)

theorem arrivalEmbeddingFrom_lt_iff (bound : Nat) (arrivals : List Nat)
    (h : ArrivalsAfter bound arrivals) (left right : Nat) :
    left < right ↔
      arrivalEmbeddingFrom bound arrivals left <
        arrivalEmbeddingFrom bound arrivals right := by
  let f := arrivalEmbeddingFrom bound arrivals
  have hmono : ∀ a b, a < b → f a < f b :=
    lt_of_succ_lt_values f (arrivalEmbeddingFrom_succ_lt bound arrivals h)
  constructor
  · exact hmono left right
  · intro hvalues
    apply Classical.byContradiction
    intro hnot
    have hle : right ≤ left := by omega
    rcases Nat.eq_or_lt_of_le hle with heq | hlt
    · subst right
      exact (Nat.lt_irrefl _ hvalues)
    · exact (Nat.not_lt_of_ge (Nat.le_of_lt (hmono right left hlt))) hvalues

theorem arrivalEmbeddingFrom_append_next (bound : Nat) :
    ∀ (before : List Nat) (arrival : Nat) (after : List Nat),
      arrivalEmbeddingFrom bound (before ++ arrival :: after)
          (before.length + 1) = arrival := by
  intro before
  induction before generalizing bound with
  | nil =>
      intro arrival after
      simp only [List.nil_append, List.length_nil, Nat.zero_add,
        arrivalEmbeddingFrom]
      exact arrivalEmbeddingFrom_zero arrival after
  | cons head before ih =>
      intro arrival after
      simp only [List.cons_append, List.length_cons, arrivalEmbeddingFrom]
      simpa [Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using
        ih head arrival after

theorem timedBy_arrivalEmbeddingFrom {α : Type}
    (ops : List (TimedOp α)) :
    TimedBy (arrivalEmbeddingFrom 0 (timedArrivals ops)) 0 ops := by
  have aux : ∀ (rest : List (TimedOp α)) (before : List Nat),
      TimedBy
        (arrivalEmbeddingFrom 0 (before ++ timedArrivals rest))
        before.length rest := by
    intro rest
    induction rest with
    | nil => intros; trivial
    | cons op rest ih =>
        intro before
        cases op with
        | push pkt arrival =>
          constructor
          · exact (arrivalEmbeddingFrom_append_next 0 before arrival
              (timedArrivals rest)).symm
          · have htail := ih (before ++ [arrival])
            simpa [timedArrivals, List.append_assoc] using htail
        | pop => exact ih before
  simpa using aux ops []

theorem interEquiv_runTimedAfter {k : Nat} {S₁ S₂ : Scheduler k}
    (hinter : interEquiv S₁ S₂) (ops : List (TimedOp (Fin k)))
    (htimed : TimedAfter 0 ops) :
    runTimedFrom S₁.assign (emptyTree S₁.topo) ops =
      runTimedFrom S₂.assign (emptyTree S₂.topo) ops := by
  let rearrange := arrivalEmbeddingFrom 0 (timedArrivals ops)
  apply interEquiv_runTimedFrom hinter rearrange
      (arrivalEmbeddingFrom_lt_iff 0 (timedArrivals ops)
        (timedAfter_arrivalsAfter ops 0 htimed)) ops
  exact timedBy_arrivalEmbeddingFrom ops



def TimedEquivAfter {α : Type} (P : α → Prop) (bound : Nat)
    (assign₁ : α → Path) (tree₁ : Tree α)
    (assign₂ : α → Path) (tree₂ : Tree α) : Prop :=
  ∀ ops, TimedOpsOn P ops → TimedAfter bound ops →
    runTimedFrom assign₁ tree₁ ops = runTimedFrom assign₂ tree₂ ops

theorem timedAfter_of_le {α : Type} {oldBound newBound : Nat}
    (hbound : oldBound ≤ newBound) : ∀ ops : List (TimedOp α),
    TimedAfter newBound ops → TimedAfter oldBound ops := by
  intro ops
  induction ops generalizing oldBound newBound with
  | nil => intros; trivial
  | cons op ops ih =>
      intro htimed
      cases op with
      | push pkt arrival =>
        simp only [TimedAfter] at htimed ⊢
        exact ⟨by omega, ih (Nat.le_refl _) htimed.2⟩
      | pop =>
        simp only [TimedAfter] at htimed ⊢
        exact ih hbound htimed

theorem timedEquivAfter_weaken {α : Type} {P : α → Prop}
    {oldBound newBound : Nat} {assign₁ assign₂ : α → Path}
    {tree₁ tree₂ : Tree α}
    (h : TimedEquivAfter P oldBound assign₁ tree₁ assign₂ tree₂)
    (hbound : oldBound ≤ newBound) :
    TimedEquivAfter P newBound assign₁ tree₁ assign₂ tree₂ := by
  intro ops hops htimed
  apply h ops hops
  exact timedAfter_of_le hbound ops htimed

theorem timedEquivAfter_push {α : Type} {P : α → Prop}
    {bound : Nat} {assign₁ assign₂ : α → Path} {tree₁ tree₂ : Tree α}
    (h : TimedEquivAfter P bound assign₁ tree₁ assign₂ tree₂)
    (pkt : α) (arrival : Nat) (hp : P pkt) (htime : bound < arrival) :
    TimedEquivAfter P arrival assign₁
      (treePush pkt arrival tree₁ (assign₁ pkt)) assign₂
      (treePush pkt arrival tree₂ (assign₂ pkt)) := by
  intro ops hops htimed
  have hall := h (.push pkt arrival :: ops) ⟨hp, hops⟩ ⟨htime, htimed⟩
  exact hall

theorem timedEquivAfter_pop {α : Type} {P : α → Prop}
    {bound : Nat} {assign₁ assign₂ : α → Path} {tree₁ tree₂ : Tree α}
    (h : TimedEquivAfter P bound assign₁ tree₁ assign₂ tree₂) :
    (treePop tree₁ = none ∧ treePop tree₂ = none) ∨
      ∃ pkt tree₁' tree₂', treePop tree₁ = some (pkt, tree₁') ∧
        treePop tree₂ = some (pkt, tree₂') ∧
        TimedEquivAfter P bound assign₁ tree₁' assign₂ tree₂' := by
  cases hp₁ : treePop tree₁ with
  | none =>
    cases hp₂ : treePop tree₂ with
    | none => exact Or.inl ⟨rfl, rfl⟩
    | some result₂ =>
      obtain ⟨pkt₂, tree₂'⟩ := result₂
      have hrun := h [.pop] trivial trivial
      simp [runTimedFrom, hp₁, hp₂] at hrun
  | some result₁ =>
    obtain ⟨pkt₁, tree₁'⟩ := result₁
    cases hp₂ : treePop tree₂ with
    | none =>
      have hrun := h [.pop] trivial trivial
      simp [runTimedFrom, hp₁, hp₂] at hrun
    | some result₂ =>
      obtain ⟨pkt₂, tree₂'⟩ := result₂
      have hpkt : pkt₁ = pkt₂ := by
        have hrun := h [.pop] trivial trivial
        simpa [runTimedFrom, hp₁, hp₂] using hrun
      subst pkt₂
      right
      refine ⟨pkt₁, tree₁', tree₂', rfl, rfl, ?_⟩
      intro ops hops htimed
      have hrun := h (.pop :: ops) hops htimed
      simpa [runTimedFrom, hp₁, hp₂] using hrun

def ForestTimedEquivAfterAt {α : Type} (P : α → Prop)
    (color : α → Nat) (tail₁ tail₂ : α → Path) (bound : Nat) :
    Nat → List (Tree α) → List (Tree α) → Prop
  | _, [], [] => True
  | base, tree₁ :: trees₁, tree₂ :: trees₂ =>
      TimedEquivAfter (fun pkt => P pkt ∧ color pkt = base) bound
        tail₁ tree₁ tail₂ tree₂ ∧
      ForestTimedEquivAfterAt P color tail₁ tail₂ bound
        (base + 1) trees₁ trees₂
  | _, _, _ => False

theorem forestTimedEquivAfterAt_weaken {α : Type} {P : α → Prop}
    (color : α → Nat) (tail₁ tail₂ : α → Path)
    (oldBound newBound base : Nat) (trees₁ trees₂ : List (Tree α))
    (h : ForestTimedEquivAfterAt P color tail₁ tail₂ oldBound
      base trees₁ trees₂) (hbound : oldBound ≤ newBound) :
    ForestTimedEquivAfterAt P color tail₁ tail₂ newBound
      base trees₁ trees₂ := by
  induction trees₁ generalizing base trees₂ with
  | nil =>
      cases trees₂ <;> simp_all [ForestTimedEquivAfterAt]
  | cons tree₁ trees₁ ih =>
      cases trees₂ with
      | nil => exact False.elim h
      | cons tree₂ trees₂ =>
        exact ⟨timedEquivAfter_weaken h.1 hbound,
          ih (base + 1) trees₂ h.2⟩

theorem forestTimedEquivAfterAt_push {α : Type} {P : α → Prop}
    (color : α → Nat) (tail₁ tail₂ : α → Path)
    (pkt : α) (bound arrival base child : Nat) (trees₁ trees₂ : List (Tree α))
    (h : ForestTimedEquivAfterAt P color tail₁ tail₂ bound
      base trees₁ trees₂) (hp : P pkt)
    (hcolor : color pkt = base + child) (htime : bound < arrival) :
    ForestTimedEquivAfterAt P color tail₁ tail₂ arrival base
      (treePushAt pkt arrival trees₁ child (tail₁ pkt))
      (treePushAt pkt arrival trees₂ child (tail₂ pkt)) := by
  cases trees₁ with
  | nil =>
    cases trees₂ with
    | nil => trivial
    | cons tree₂ trees₂ => exact False.elim h
  | cons tree₁ trees₁ =>
    cases trees₂ with
    | nil => exact False.elim h
    | cons tree₂ trees₂ =>
      obtain ⟨htree, htrees⟩ := h
      cases child with
      | zero =>
        constructor
        · apply timedEquivAfter_push htree pkt arrival
          · exact ⟨hp, by simpa using hcolor⟩
          · exact htime
        · exact forestTimedEquivAfterAt_weaken color tail₁ tail₂ bound
            arrival (base + 1) trees₁ trees₂ htrees (Nat.le_of_lt htime)
      | succ child =>
        constructor
        · exact timedEquivAfter_weaken htree (Nat.le_of_lt htime)
        · apply forestTimedEquivAfterAt_push color tail₁ tail₂ pkt bound
            arrival (base + 1) child trees₁ trees₂ htrees hp
          · omega
          · exact htime

theorem forestTimedEquivAfterAt_pop {α : Type} {P : α → Prop}
    (color : α → Nat) (tail₁ tail₂ : α → Path) (bound base child : Nat)
    (trees₁ trees₂ : List (Tree α))
    (h : ForestTimedEquivAfterAt P color tail₁ tail₂ bound
      base trees₁ trees₂) :
    (treePopAt trees₁ child = none ∧ treePopAt trees₂ child = none) ∨
      ∃ pkt trees₁' trees₂', treePopAt trees₁ child = some (pkt, trees₁') ∧
        treePopAt trees₂ child = some (pkt, trees₂') ∧
        ForestTimedEquivAfterAt P color tail₁ tail₂ bound
          base trees₁' trees₂' := by
  cases trees₁ with
  | nil =>
    cases trees₂ with
    | nil => exact Or.inl ⟨rfl, rfl⟩
    | cons tree₂ trees₂ => exact False.elim h
  | cons tree₁ trees₁ =>
    cases trees₂ with
    | nil => exact False.elim h
    | cons tree₂ trees₂ =>
      obtain ⟨htree, htrees⟩ := h
      cases child with
      | zero =>
        rcases timedEquivAfter_pop htree with
          ⟨hp₁, hp₂⟩ | ⟨pkt, tree₁', tree₂', hp₁, hp₂, htree'⟩
        · left
          simp [treePopAt, hp₁, hp₂]
        · right
          exact ⟨pkt, tree₁' :: trees₁, tree₂' :: trees₂, by
            simp [treePopAt, hp₁, hp₂, ForestTimedEquivAfterAt,
              htree', htrees]⟩
      | succ child =>
        rcases forestTimedEquivAfterAt_pop color tail₁ tail₂ bound
            (base + 1) child trees₁ trees₂ htrees with
          ⟨hp₁, hp₂⟩ | ⟨pkt, trees₁', trees₂', hp₁, hp₂, htrees'⟩
        · left
          simp [treePopAt, hp₁, hp₂]
        · right
          exact ⟨pkt, tree₁ :: trees₁', tree₂ :: trees₂', by
            simp [treePopAt, hp₁, hp₂, ForestTimedEquivAfterAt,
              htree, htrees']⟩

theorem nodeTimedEquivAfter {α : Type} {P : α → Prop}
    (assign₁ assign₂ : α → Path) (color rank : α → Nat)
    (tail₁ tail₂ : α → Path)
    (hassign₁ : ∀ pkt, P pkt →
      assign₁ pkt = .node (color pkt) (rank pkt) (tail₁ pkt))
    (hassign₂ : ∀ pkt, P pkt →
      assign₂ pkt = .node (color pkt) (rank pkt) (tail₂ pkt))
    (bound : Nat) (q : Queue Nat) (trees₁ trees₂ : List (Tree α))
    (hforest : ForestTimedEquivAfterAt P color tail₁ tail₂ bound
      0 trees₁ trees₂) :
    TimedEquivAfter P bound assign₁ (.node q trees₁)
      assign₂ (.node q trees₂) := by
  intro ops hops htimed
  induction ops generalizing bound q trees₁ trees₂ with
  | nil => rfl
  | cons op ops ih =>
    cases op with
    | push pkt arrival =>
      obtain ⟨hp, hops⟩ := hops
      obtain ⟨htime, htimed⟩ := htimed
      simp only [runTimedFrom]
      rw [hassign₁ pkt hp, hassign₂ pkt hp]
      simp only [treePush]
      apply ih arrival _ _ _ _ hops htimed
      apply forestTimedEquivAfterAt_push color tail₁ tail₂ pkt bound
          arrival 0 (color pkt) trees₁ trees₂ hforest hp
      · simp
      · exact htime
    | pop =>
      simp only [TimedOpsOn] at hops
      simp only [TimedAfter] at htimed
      simp only [runTimedFrom]
      cases hq : qpop q with
      | none =>
        simp only [treePop, hq]
        exact congrArg (fun xs => none :: xs)
          (ih bound q trees₁ trees₂ hforest hops htimed)
      | some result =>
        obtain ⟨e, rest⟩ := result
        rcases forestTimedEquivAfterAt_pop color tail₁ tail₂ bound 0 e.val
            trees₁ trees₂ hforest with
          ⟨hp₁, hp₂⟩ | ⟨pkt, trees₁', trees₂', hp₁, hp₂, hforest'⟩
        · simp only [treePop, hq, hp₁, hp₂]
          exact congrArg (fun xs => none :: xs)
            (ih bound q trees₁ trees₂ hforest hops htimed)
        · simp only [treePop, hq, hp₁, hp₂, List.cons.injEq, true_and]
          exact ih bound rest trees₁' trees₂' hforest' hops htimed

theorem timedAfter_mapTimedVal {α β : Type} (f : α → β) :
    ∀ (ops : List (TimedOp α)) (bound : Nat), TimedAfter bound ops →
      TimedAfter bound (mapTimedVal f ops) := by
  intro ops
  induction ops with
  | nil => intros; trivial
  | cons op ops ih =>
      intro bound htimed
      cases op with
      | push pkt arrival => exact ⟨htimed.1, ih arrival htimed.2⟩
      | pop => exact ih bound htimed

theorem mapTimedVal_eq_self_of_on {α β : Type} {P : α → Prop}
    (project : α → β) (embed : β → α)
    (hinverse : ∀ pkt, P pkt → embed (project pkt) = pkt) :
    ∀ ops : List (TimedOp α), TimedOpsOn P ops →
      mapTimedVal embed (mapTimedVal project ops) = ops := by
  intro ops
  induction ops with
  | nil => intros; rfl
  | cons op ops ih =>
      intro hops
      cases op with
      | push pkt arrival =>
        simp only [TimedOpsOn] at hops
        simp [mapTimedVal, hinverse pkt hops.1, ih hops.2]
      | pop => simp [mapTimedVal, ih hops]

theorem timedEquivAfter_empty_of_restrict {m : Nat} {α : Type}
    {P : α → Prop} (S₁ S₂ : Scheduler m) (embed : Fin m → α)
    (project : α → Fin m) (hinverse : ∀ pkt, P pkt → embed (project pkt) = pkt)
    (assign₁ assign₂ : α → Path)
    (hassign₁ : ∀ pkt, assign₁ (embed pkt) = S₁.assign pkt)
    (hassign₂ : ∀ pkt, assign₂ (embed pkt) = S₂.assign pkt)
    (hinter : interEquiv S₁ S₂) (bound : Nat) :
    TimedEquivAfter P bound assign₁ (emptyTree S₁.topo)
      assign₂ (emptyTree S₂.topo) := by
  intro ops hops htimed
  let localOps := mapTimedVal project ops
  have hlocalTimed : TimedAfter 0 localOps := by
    apply timedAfter_of_le (Nat.zero_le bound) localOps
    exact timedAfter_mapTimedVal project ops bound htimed
  have hlocal := interEquiv_runTimedAfter hinter localOps hlocalTimed
  have hmapped := congrArg (List.map (Option.map embed)) hlocal
  have hroundtrip := mapTimedVal_eq_self_of_on project embed hinverse ops hops
  have hrun₁ := runTimedFrom_mapVal embed S₁.assign assign₁ hassign₁
    localOps (emptyTree S₁.topo)
  have hrun₂ := runTimedFrom_mapVal embed S₂.assign assign₂ hassign₂
    localOps (emptyTree S₂.topo)
  dsimp [localOps] at hrun₁ hrun₂
  rw [hroundtrip] at hrun₁ hrun₂
  simpa only [mapValTree_emptyTree] using
    hrun₁.trans (hmapped.trans hrun₂.symm)

theorem timedEquivAfter_empty_of_no_values {α : Type} {P : α → Prop}
    (hnone : ∀ pkt, ¬ P pkt) (bound : Nat)
    (assign₁ assign₂ : α → Path) (topology₁ topology₂ : Topology) :
    TimedEquivAfter P bound assign₁ (emptyTree topology₁)
      assign₂ (emptyTree topology₂) := by
  intro ops hops htimed
  induction ops with
  | nil => rfl
  | cons op ops ih =>
      cases op with
      | push pkt arrival => exact False.elim (hnone pkt hops.1)
      | pop =>
        simp only [runTimedFrom, treePop_emptyTree]
        exact congrArg (fun values => none :: values) (ih hops htimed)

theorem emptyForestTimedEquivAfterAt_of_children {α : Type}
    {P : α → Prop} (color : α → Nat) (tail₁ tail₂ : α → Path)
    (bound base : Nat) :
    ∀ (topologies₁ topologies₂ : List Topology),
      topologies₁.length = topologies₂.length →
      (∀ index topology₁ topology₂,
        ListAt topology₁ topologies₁ index →
        ListAt topology₂ topologies₂ index →
        TimedEquivAfter
          (fun pkt => P pkt ∧ color pkt = base + index) bound
          tail₁ (emptyTree topology₁) tail₂ (emptyTree topology₂)) →
      ForestTimedEquivAfterAt P color tail₁ tail₂ bound base
        (emptyForest topologies₁) (emptyForest topologies₂) := by
  intro topologies₁
  induction topologies₁ generalizing base with
  | nil =>
      intro topologies₂ hlength hchildren
      cases topologies₂ with
      | nil => trivial
      | cons topology₂ topologies₂ => simp at hlength
  | cons topology₁ topologies₁ ih =>
      intro topologies₂ hlength hchildren
      cases topologies₂ with
      | nil => simp at hlength
      | cons topology₂ topologies₂ =>
        simp only [List.length_cons, Nat.add_left_inj] at hlength
        constructor
        · simpa using hchildren 0 topology₁ topology₂ (.zero _) (.zero _)
        · apply ih (base + 1) topologies₂ hlength
          intro index child₁ child₂ hat₁ hat₂
          have hchild := hchildren (index + 1) child₁ child₂
            (.succ topology₁ hat₁) (.succ topology₂ hat₂)
          simpa [Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using hchild

noncomputable def fiberProjection {k : Nat} (color : Fin k → Nat)
    (member pkt : Fin k) : Fin (fiberList color (color member)).length :=
  if h : color pkt = color member then
    Classical.choose (List.get_of_mem ((mem_fiberList color (color member) pkt).mpr h))
  else
    Classical.choose (List.get_of_mem
      ((mem_fiberList color (color member) member).mpr rfl))

theorem fiberProjection_rightInverse {k : Nat} (color : Fin k → Nat)
    (member pkt : Fin k) (h : color pkt = color member) :
    fiberEmbedding color (color member) (fiberProjection color member pkt) = pkt := by
  simp only [fiberProjection, dif_pos h, fiberEmbedding]
  exact Classical.choose_spec
    (List.get_of_mem ((mem_fiberList color (color member) pkt).mpr h))

theorem timedOpsFrom_after {k : Nat} : ∀ (ops : List (Op k)) (count : Nat),
    TimedAfter count (timedOpsFrom count ops) := by
  intro ops
  induction ops with
  | nil => intros; trivial
  | cons op ops ih =>
      intro count
      cases op with
      | push pkt => exact ⟨by omega, ih (count + 1)⟩
      | pop => exact ih count

theorem interEquiv_of_timedEquivAfter_empty {k : Nat} (S₁ S₂ : Scheduler k)
    (h : TimedEquivAfter (fun _ : Fin k => True) 0 S₁.assign
      (emptyTree S₁.topo) S₂.assign (emptyTree S₂.topo)) :
    interEquiv S₁ S₂ := by
  intro ops
  unfold run
  rw [runFrom_eq_runTimedFrom, runFrom_eq_runTimedFrom]
  exact h (timedOpsFrom 0 ops) (timedOpsFrom_on_true 0 ops)
    (timedOpsFrom_after ops 0)



def InterleavedTheoremAt (k : Nat) : Prop :=
  ∀ (S₁ S₂ : Scheduler k), S₁.Valid → S₂.Valid →
    flushEquiv S₁ S₂ → interEquiv S₁ S₂

theorem interleavedTheoremAt_of_lt_three (k : Nat) (hk : k < 3) :
    InterleavedTheoremAt k := by
  intro S₁ S₂ hvalid₁ hvalid₂ hflush
  have hcases : k = 0 ∨ k = 1 ∨ k = 2 := by omega
  rcases hcases with rfl | rfl | rfl
  · exact interEquiv_fin_zero S₁ S₂
  · exact interEquiv_fin_one S₁ S₂ hvalid₁ hvalid₂
  · exact interEquiv_fin_two S₁ S₂ hvalid₁ hvalid₂ hflush

theorem commonChildren_interEquiv {k : Nat}
    (ih : ∀ m, m < k → InterleavedTheoremAt m)
    (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler)
    (member : Fin k) :
    interEquiv ((commonRootOne root₁ root₂).childScheduler member)
      ((commonRootTwo root₁ root₂).childScheduler member) := by
  let common₁ := commonRootOne root₁ root₂
  let common₂ := commonRootTwo root₁ root₂
  apply ih (fiberList common₁.color (common₁.color member)).length
    (common₁.childAlphabet_lt member)
  · exact common₁.childScheduler_valid member
  · exact common₂.childScheduler_valid member
  · exact commonChildren_flushEquiv root₁ root₂ hflush member

theorem commonRoots_interEquiv {k : Nat}
    (ih : ∀ m, m < k → InterleavedTheoremAt m)
    (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler) :
    interEquiv (commonRootOne root₁ root₂).scheduler
      (commonRootTwo root₁ root₂).scheduler := by
  let common₁ := commonRootOne root₁ root₂
  let common₂ := commonRootTwo root₁ root₂
  let color := commonColor root₁ root₂
  have hlength : common₁.topologies.length = common₂.topologies.length := by
    have hleft : (commonTopologies root₁ root₂).length =
        root₁.topologies.length * root₂.topologies.length := by
      unfold commonTopologies
      induction root₁.topologies with
      | nil => simp
      | cons topology topologies ih =>
        simp [ih, Nat.succ_mul, Nat.add_comm]
    have hright : (commonTopologiesTwo root₁ root₂).length =
        root₁.topologies.length * root₂.topologies.length := by
      simp [commonTopologiesTwo]
    change (commonTopologies root₁ root₂).length =
      (commonTopologiesTwo root₁ root₂).length
    rw [hleft, hright]
  have hforest : ForestTimedEquivAfterAt (fun _ : Fin k => True)
      color common₁.tail common₂.tail 0 0
      (emptyForest common₁.topologies) (emptyForest common₂.topologies) := by
    apply emptyForestTimedEquivAfterAt_of_children color common₁.tail
      common₂.tail 0 0 common₁.topologies common₂.topologies hlength
    intro index topology₁ topology₂ hat₁ hat₂
    simp only [Nat.zero_add]
    by_cases hmember : ∃ member : Fin k, color member = index
    · obtain ⟨member, hmember⟩ := hmember
      have htopology₁ : common₁.childTopology member = topology₁ := by
        apply (common₁.childTopology_at member).unique
        have hc : common₁.color member = index := hmember
        rw [hc]
        exact hat₁
      have htopology₂ : common₂.childTopology member = topology₂ := by
        apply (common₂.childTopology_at member).unique
        have hc : common₂.color member = index := hmember
        rw [hc]
        exact hat₂
      have hlocal := commonChildren_interEquiv ih root₁ root₂ hflush member
      have hrestricted := timedEquivAfter_empty_of_restrict
        (common₁.childScheduler member) (common₂.childScheduler member)
        (fiberEmbedding color (color member))
        (fiberProjection color member) (by
          intro pkt (hp : True ∧ color pkt = index)
          exact fiberProjection_rightInverse color member pkt
            (hp.2.trans hmember.symm))
        common₁.tail common₂.tail (fun _ => rfl) (fun _ => rfl)
        hlocal 0
      change TimedEquivAfter (fun pkt => True ∧ color pkt = index) 0
        common₁.tail (emptyTree (common₁.childTopology member))
        common₂.tail (emptyTree (common₂.childTopology member)) at hrestricted
      rw [htopology₁, htopology₂] at hrestricted
      simpa [hmember] using hrestricted
    · apply timedEquivAfter_empty_of_no_values
      · intro pkt hp
        exact hmember ⟨pkt, hp.2⟩
  apply interEquiv_of_timedEquivAfter_empty common₁.scheduler common₂.scheduler
  exact nodeTimedEquivAfter common₁.assign common₂.assign color
    (commonRank root₁ root₂) common₁.tail common₂.tail
    (by intros; rfl) (by intros; rfl) 0 []
    (emptyForest common₁.topologies) (emptyForest common₂.topologies) hforest

theorem NormalRoot.child_rootRestriction_interEquiv {k : Nat}
    (root : NormalRoot k) (member : Fin k) :
    interEquiv (root.childScheduler member)
      (schedulerComap root.scheduler
        (fiberEmbedding root.color (root.color member))) := by
  intro ops
  let embed := fiberEmbedding root.color (root.color member)
  have hblock := root.blockRun member ops
  have hcomap := run_schedulerComap root.scheduler embed ops
  have hmapped :
      (run (root.childScheduler member) ops).map (Option.map embed) =
        (run (schedulerComap root.scheduler embed) ops).map
          (Option.map embed) := hblock.symm.trans hcomap
  exact listMap_injective (Option.map embed)
    (Option.map_injective (fiberEmbedding_injective root.color
      (root.color member))) hmapped

theorem ListAt.mapValue {α β : Type} (f : α → β) {value : α}
    {values : List α} {index : Nat} (h : ListAt value values index) :
    ListAt (f value) (values.map f) index := by
  induction h with
  | zero tail => exact .zero (tail.map f)
  | succ head h ih => exact .succ (f head) ih

noncomputable def tandemOne {k : Nat} (root₁ root₂ : NormalRoot k) :
    Scheduler k :=
  let common := commonRootOne root₁ root₂
  ⟨.node (root₁.topologies.map (fun _ => .node common.topologies)),
    fun pkt => .node (root₁.color pkt) (commonRank root₁ root₂ pkt)
      (.node (commonColor root₁ root₂ pkt) (commonRank root₁ root₂ pkt)
        (root₁.tail pkt))⟩

theorem rootOneRank_tandemOne_interEquiv {k : Nat}
    (ih : ∀ m, m < k → InterleavedTheoremAt m)
    (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler) :
    interEquiv (root₁.rankReplacement (commonRank root₁ root₂))
      (tandemOne root₁ root₂) := by
  let common := commonRootOne root₁ root₂
  let outerTopologies : List Topology :=
    root₁.topologies.map (fun _ => Topology.node common.topologies)
  have hlength : root₁.topologies.length = outerTopologies.length := by
    simp [outerTopologies]
  have hglobalFlush : flushEquiv root₁.scheduler common.scheduler :=
    rootOne_commonRootOne_flushEquiv root₁ root₂ hflush
  have hforest : ForestTimedEquivAfterAt (fun _ : Fin k => True)
      root₁.color root₁.tail common.assign 0 0
      (emptyForest root₁.topologies) (emptyForest outerTopologies) := by
    apply emptyForestTimedEquivAfterAt_of_children root₁.color root₁.tail
      common.assign 0 0 root₁.topologies outerTopologies hlength
    intro index topology₁ topology₂ hat₁ hat₂
    simp only [Nat.zero_add]
    by_cases hmember : ∃ member : Fin k, root₁.color member = index
    · obtain ⟨member, hmember⟩ := hmember
      let embed := fiberEmbedding root₁.color (root₁.color member)
      let originalRestricted := schedulerComap root₁.scheduler embed
      let commonRestricted := schedulerComap common.scheduler embed
      have htopology₁ : root₁.childTopology member = topology₁ := by
        apply (root₁.childTopology_at member).unique
        rw [hmember]
        exact hat₁
      have htopology₂ : common.scheduler.topo = topology₂ := by
        have hmapped : ListAt (Topology.node common.topologies)
            outerTopologies index := by
          change ListAt (Topology.node common.topologies)
            (root₁.topologies.map
              (fun _ => Topology.node common.topologies)) index
          exact hat₁.mapValue (fun _ => Topology.node common.topologies)
        exact hmapped.unique hat₂
      have hchildOriginal := root₁.child_rootRestriction_interEquiv member
      have hrestrictedFlush : flushEquiv originalRestricted commonRestricted :=
        flushEquiv_schedulerComap root₁.scheduler common.scheduler embed
          (fiberEmbedding_injective root₁.color (root₁.color member))
          hglobalFlush
      have hchildFlush : flushEquiv (root₁.childScheduler member)
          commonRestricted := by
        intro word
        exact (hchildOriginal (flushOps word)).trans (hrestrictedFlush word)
      have hchild : interEquiv (root₁.childScheduler member)
          commonRestricted := by
        apply ih (fiberList root₁.color (root₁.color member)).length
          (root₁.childAlphabet_lt member)
        · exact root₁.childScheduler_valid member
        · exact schedulerComap_valid common.scheduler embed common.valid
        · exact hchildFlush
      have hrestricted := timedEquivAfter_empty_of_restrict
        (root₁.childScheduler member) commonRestricted embed
        (fiberProjection root₁.color member) (by
          intro pkt (hp : True ∧ root₁.color pkt = index)
          exact fiberProjection_rightInverse root₁.color member pkt
            (hp.2.trans hmember.symm))
        root₁.tail common.assign (fun _ => rfl) (fun _ => rfl) hchild 0
      change TimedEquivAfter
        (fun pkt => True ∧ root₁.color pkt = index) 0 root₁.tail
        (emptyTree (root₁.childTopology member)) common.assign
        (emptyTree common.scheduler.topo) at hrestricted
      rw [htopology₁, htopology₂] at hrestricted
      exact hrestricted
    · apply timedEquivAfter_empty_of_no_values
      intro pkt hp
      exact hmember ⟨pkt, hp.2⟩
  apply interEquiv_of_timedEquivAfter_empty
    (root₁.rankReplacement (commonRank root₁ root₂)) (tandemOne root₁ root₂)
  change TimedEquivAfter (fun _ : Fin k => True) 0
    (fun pkt => .node (root₁.color pkt) (commonRank root₁ root₂ pkt)
      (root₁.tail pkt)) (emptyTree (.node root₁.topologies))
    (fun pkt => .node (root₁.color pkt) (commonRank root₁ root₂ pkt)
      (common.assign pkt)) (emptyTree (.node outerTopologies))
  exact nodeTimedEquivAfter _ _ root₁.color (commonRank root₁ root₂)
    root₁.tail common.assign (by intros; rfl) (by intros; rfl) 0 []
    (emptyForest root₁.topologies) (emptyForest outerTopologies) hforest



def InnerMatches {α : Type} (parent : Nat → Nat) (block : Nat) :
    Nat → List (Tree α) → List (Tree α) → Prop
  | _, [], [] => True
  | cell, flat :: flats, inner :: inners =>
      (parent cell = block → flat = inner) ∧
        InnerMatches parent block (cell + 1) flats inners
  | _, _, _ => False

theorem innerMatches_pushAt_both {α : Type} (parent : Nat → Nat)
    (block cell : Nat) (flat inner : List (Tree α)) (pkt : α)
    (arrival : Nat) (path : Path)
    (h : InnerMatches parent block cell flat inner) :
    ∀ child, parent (cell + child) = block →
      InnerMatches parent block cell
        (treePushAt pkt arrival flat child path)
        (treePushAt pkt arrival inner child path) := by
  intro child
  induction child generalizing cell flat inner with
  | zero =>
      intro hparent
      cases flat with
      | nil => cases inner <;> trivial
      | cons flat flats =>
        cases inner with
        | nil => exact False.elim h
        | cons inner inners =>
          constructor
          · intro hc
            rw [← h.1 hc]
          · exact h.2
  | succ child ih =>
      intro hparent
      cases flat with
      | nil => cases inner <;> trivial
      | cons flat flats =>
        cases inner with
        | nil => exact False.elim h
        | cons inner inners =>
          constructor
          · exact h.1
          · apply ih (cell + 1) flats inners h.2
            simpa [Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using hparent

theorem innerMatches_pushAt_flat_other {α : Type} (parent : Nat → Nat)
    (block cell : Nat) (flat inner : List (Tree α)) (pkt : α)
    (arrival : Nat) (path : Path)
    (h : InnerMatches parent block cell flat inner) :
    ∀ child, parent (cell + child) ≠ block →
      InnerMatches parent block cell
        (treePushAt pkt arrival flat child path) inner := by
  intro child
  induction child generalizing cell flat inner with
  | zero =>
      intro hparent
      cases flat with
      | nil => cases inner <;> trivial
      | cons flat flats =>
        cases inner with
        | nil => exact False.elim h
        | cons inner inners =>
          exact ⟨fun hc => False.elim (hparent hc), h.2⟩
  | succ child ih =>
      intro hparent
      cases flat with
      | nil => cases inner <;> trivial
      | cons flat flats =>
        cases inner with
        | nil => exact False.elim h
        | cons inner inners =>
          constructor
          · exact h.1
          · apply ih (cell + 1) flats inners h.2
            simpa [Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using hparent

theorem innerMatches_popAt_both {α : Type} (parent : Nat → Nat)
    (block cell : Nat) (flat inner : List (Tree α))
    (h : InnerMatches parent block cell flat inner) :
    ∀ child, parent (cell + child) = block →
      (treePopAt flat child = none ∧ treePopAt inner child = none) ∨
        ∃ pkt flat' inner', treePopAt flat child = some (pkt, flat') ∧
          treePopAt inner child = some (pkt, inner') ∧
          InnerMatches parent block cell flat' inner' := by
  intro child
  induction child generalizing cell flat inner with
  | zero =>
      intro hparent
      cases flat with
      | nil =>
        cases inner with
        | nil => exact Or.inl ⟨rfl, rfl⟩
        | cons inner inners => exact False.elim h
      | cons flat flats =>
        cases inner with
        | nil => exact False.elim h
        | cons inner inners =>
          have heq := h.1 hparent
          subst inner
          cases hp : treePop flat with
          | none => exact Or.inl ⟨by simp [treePopAt, hp], by simp [treePopAt, hp]⟩
          | some result =>
            obtain ⟨pkt, flat'⟩ := result
            right
            exact ⟨pkt, flat' :: flats, flat' :: inners, by
              simp [treePopAt, hp, InnerMatches, h.2]⟩
  | succ child ih =>
      intro hparent
      cases flat with
      | nil =>
        cases inner with
        | nil => exact Or.inl ⟨rfl, rfl⟩
        | cons inner inners => exact False.elim h
      | cons flat flats =>
        cases inner with
        | nil => exact False.elim h
        | cons inner inners =>
          have hparent' : parent ((cell + 1) + child) = block := by
            simpa [Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using hparent
          rcases ih (cell + 1) flats inners h.2 hparent' with
            hnone | ⟨pkt, flats', inners', hp₁, hp₂, hmatches⟩
          · exact Or.inl ⟨by simp [treePopAt, hnone.1], by simp [treePopAt, hnone.2]⟩
          · right
            refine ⟨pkt, flat :: flats', inner :: inners', ?_, ?_, ?_⟩
            · simp [treePopAt, hp₁]
            · simp [treePopAt, hp₂]
            · exact ⟨h.1, hmatches⟩

theorem innerMatches_popAt_flat_other {α : Type} (parent : Nat → Nat)
    (block cell : Nat) (flat inner flat' : List (Tree α)) (pkt : α)
    (h : InnerMatches parent block cell flat inner) (child : Nat)
    (hparent : parent (cell + child) ≠ block)
    (hpop : treePopAt flat child = some (pkt, flat')) :
    InnerMatches parent block cell flat' inner := by
  induction child generalizing cell flat inner flat' with
  | zero =>
      cases flat with
      | nil => simp [treePopAt] at hpop
      | cons flat flats =>
        cases inner with
        | nil => exact False.elim h
        | cons inner inners =>
          cases hp : treePop flat with
          | none => simp [treePopAt, hp] at hpop
          | some result =>
            obtain ⟨value, tree'⟩ := result
            simp only [treePopAt, hp, Option.some.injEq, Prod.mk.injEq] at hpop
            obtain ⟨rfl, rfl⟩ := hpop
            exact ⟨fun hc => False.elim (hparent hc), h.2⟩
  | succ child ih =>
      cases flat with
      | nil => simp [treePopAt] at hpop
      | cons flat flats =>
        cases inner with
        | nil => exact False.elim h
        | cons inner inners =>
          cases hp : treePopAt flats child with
          | none => simp [treePopAt, hp] at hpop
          | some result =>
            obtain ⟨value, flats'⟩ := result
            simp only [treePopAt, hp, Option.some.injEq, Prod.mk.injEq] at hpop
            obtain ⟨rfl, rfl⟩ := hpop
            constructor
            · exact h.1
            · apply ih (cell + 1) flats inners flats' h.2
                (by simpa [Nat.add_assoc, Nat.add_comm, Nat.add_left_comm]
                  using hparent) hp

def TandemForest {α : Type} [BEq α] (rank : α → Nat)
    (outerColor innerColor : α → Nat) (parent : Nat → Nat)
    (state : List (α × Nat)) : Nat → List (Tree α) → List (Tree α) → Prop
  | _, [], _ => True
  | block, .node q innerTrees :: outerTrees, flatTrees =>
      q = selectorQueue rank innerColor
          (stateFilter (fun pkt => outerColor pkt == block) state) ∧
        InnerMatches parent block 0 flatTrees innerTrees ∧
        TandemForest rank outerColor innerColor parent state
          (block + 1) outerTrees flatTrees
  | _, _ :: _, _ => False

theorem stateFilter_append_one {α : Type} (keep : α → Bool)
    (state : List (α × Nat)) (entry : α × Nat) :
    stateFilter keep (state ++ [entry]) =
      stateFilter keep state ++ (if keep entry.1 then [entry] else []) := by
  cases hkeep : keep entry.1 <;>
    simp [stateFilter, List.filter_append, hkeep]

theorem tandemForest_push_after {α : Type} [BEq α] [LawfulBEq α]
    (rank outerColor innerColor : α → Nat) (parent : Nat → Nat)
    (state : List (α × Nat)) (base : Nat) (outerTrees flatTrees : List (Tree α))
    (pkt : α) (arrival : Nat) (tail : Path)
    (hparent : parent (innerColor pkt) = outerColor pkt)
    (hbefore : outerColor pkt < base)
    (h : TandemForest rank outerColor innerColor parent state
      base outerTrees flatTrees) :
    TandemForest rank outerColor innerColor parent (state ++ [(pkt, arrival)])
      base outerTrees
      (treePushAt pkt arrival flatTrees (innerColor pkt) tail) := by
  induction outerTrees generalizing base with
  | nil => trivial
  | cons outerTree outerTrees ih =>
      cases outerTree with
      | leaf q => exact False.elim h
      | node q innerTrees =>
        have hne : outerColor pkt ≠ base := by omega
        have hkeep : (outerColor pkt == base) = false :=
          beq_eq_false_iff_ne.mpr hne
        constructor
        · rw [h.1, stateFilter_append_one]
          simp [hkeep]
        · constructor
          · apply innerMatches_pushAt_flat_other parent base 0 flatTrees
              innerTrees pkt arrival tail h.2.1 (innerColor pkt)
            simpa [hparent] using hne
          · apply ih (base + 1) (by omega) h.2.2

theorem tandemForest_push {α : Type} [BEq α] [LawfulBEq α]
    (rank outerColor innerColor : α → Nat) (parent : Nat → Nat)
    (state : List (α × Nat)) (base : Nat) (outerTrees flatTrees : List (Tree α))
    (pkt : α) (arrival : Nat) (tail : Path)
    (hparent : parent (innerColor pkt) = outerColor pkt)
    (h : TandemForest rank outerColor innerColor parent state
      base outerTrees flatTrees) :
    ∀ child, outerColor pkt = base + child →
      TandemForest rank outerColor innerColor parent
        (state ++ [(pkt, arrival)]) base
        (treePushAt pkt arrival outerTrees child
          (.node (innerColor pkt) (rank pkt) tail))
        (treePushAt pkt arrival flatTrees (innerColor pkt) tail) := by
  intro child
  induction child generalizing base outerTrees with
  | zero =>
      intro hcolor
      cases outerTrees with
      | nil => trivial
      | cons outerTree outerTrees =>
        cases outerTree with
        | leaf q => exact False.elim h
        | node q innerTrees =>
          have hkeep : (outerColor pkt == base) = true :=
            beq_iff_eq.mpr (by simpa using hcolor)
          constructor
          · rw [h.1, stateFilter_append_one]
            simp [hkeep, selectorQueue_push]
          · constructor
            · apply innerMatches_pushAt_both parent base 0 flatTrees innerTrees
                pkt arrival tail h.2.1 (innerColor pkt)
              simpa [hparent] using hcolor
            · apply tandemForest_push_after rank outerColor innerColor parent
                state (base + 1) outerTrees flatTrees pkt arrival tail
                hparent (by omega) h.2.2
  | succ child ih =>
      intro hcolor
      cases outerTrees with
      | nil => trivial
      | cons outerTree outerTrees =>
        cases outerTree with
        | leaf q => exact False.elim h
        | node q innerTrees =>
          have hne : outerColor pkt ≠ base := by omega
          have hkeep : (outerColor pkt == base) = false :=
            beq_eq_false_iff_ne.mpr hne
          constructor
          · rw [h.1, stateFilter_append_one]
            simp [hkeep]
          · constructor
            · apply innerMatches_pushAt_flat_other parent base 0 flatTrees
                innerTrees pkt arrival tail h.2.1 (innerColor pkt)
              simpa [hparent] using hne
            · apply ih (base + 1) outerTrees h.2.2
              omega

theorem qpop_selectorQueue_stateFilter {α : Type} [DecidableEq α]
    (rank innerColor : α → Nat) (keep : α → Bool)
    (state : List (α × Nat)) (picked : α × Nat)
    (hd : PifoGeneral.DistinctArr state) (hmem : picked ∈ state)
    (hminimum : ∀ other ∈ state, other ≠ picked →
      better (PifoGeneral.embedP rank picked)
        (PifoGeneral.embedP rank other) = true)
    (hkeep : keep picked.1 = true) :
    qpop (selectorQueue rank innerColor (stateFilter keep state)) =
      some (⟨innerColor picked.1, rank picked.1, picked.2⟩,
        selectorQueue rank innerColor
          (stateFilter keep (PifoGeneral.removeArr state picked.2))) := by
  have hpickedFiltered : picked ∈ stateFilter keep state :=
    (stateFilter_mem keep).mpr ⟨hmem, hkeep⟩
  have hdFiltered := stateFilter_distinctArr keep hd
  cases hfiltered : stateFilter keep state with
  | nil => rw [hfiltered] at hpickedFiltered; contradiction
  | cons head tail =>
    obtain ⟨filteredPicked, hfilteredMem, hfilteredPop, hfilteredMin⟩ :=
      PifoGeneral.qpop_state rank head tail (by
        simpa [hfiltered] using hdFiltered)
    have hpickedEq : filteredPicked = picked := by
      apply Classical.byContradiction
      intro hne
      have hfilteredOriginal : filteredPicked ∈ state :=
        ((stateFilter_mem keep).mp (by
          simpa [hfiltered] using hfilteredMem)).1
      have hforward := hminimum filteredPicked hfilteredOriginal hne
      have hbackward := hfilteredMin picked (by
        simpa [hfiltered] using hpickedFiltered) (Ne.symm hne)
      rw [PifoGeneral.better_iff] at hforward hbackward
      omega
    subst filteredPicked
    have hqueue := qpop_selectorQueue rank innerColor hfilteredPop
    have hcommute := stateFilter_removeArr keep state picked.2
    rw [hfiltered] at hcommute
    simpa [hcommute] using hqueue

theorem tandemForest_pop_after {α : Type} [DecidableEq α]
    (rank outerColor innerColor : α → Nat) (parent : Nat → Nat)
    (state : List (α × Nat)) (base : Nat) (outerTrees flatTrees : List (Tree α))
    (picked : α × Nat) (pkt : α) (flatTrees' : List (Tree α))
    (hd : PifoGeneral.DistinctArr state) (hmem : picked ∈ state)
    (hparent : parent (innerColor picked.1) = outerColor picked.1)
    (hbefore : outerColor picked.1 < base)
    (hflatPop : treePopAt flatTrees (innerColor picked.1) =
      some (pkt, flatTrees'))
    (h : TandemForest rank outerColor innerColor parent state
      base outerTrees flatTrees) :
    TandemForest rank outerColor innerColor parent
      (PifoGeneral.removeArr state picked.2) base outerTrees flatTrees' := by
  induction outerTrees generalizing base with
  | nil => trivial
  | cons outerTree outerTrees ih =>
      cases outerTree with
      | leaf q => exact False.elim h
      | node q innerTrees =>
        have hne : outerColor picked.1 ≠ base := by omega
        have hkeep : (outerColor picked.1 == base) = false :=
          beq_eq_false_iff_ne.mpr hne
        constructor
        · rw [h.1]
          exact congrArg (selectorQueue rank innerColor)
            (stateFilter_removeArr_other
              (fun pkt => outerColor pkt == base) hd hmem hkeep).symm
        · constructor
          · apply innerMatches_popAt_flat_other parent base 0 flatTrees
              innerTrees flatTrees' pkt h.2.1 (innerColor picked.1)
            · simpa [hparent] using hne
            · exact hflatPop
          · apply ih (base + 1) (by omega) h.2.2

theorem tandemForest_pop {α : Type} [DecidableEq α]
    (rank outerColor innerColor : α → Nat) (parent : Nat → Nat)
    (state : List (α × Nat)) (base : Nat) (outerTrees flatTrees : List (Tree α))
    (picked : α × Nat)
    (hd : PifoGeneral.DistinctArr state) (hmem : picked ∈ state)
    (hminimum : ∀ other ∈ state, other ≠ picked →
      better (PifoGeneral.embedP rank picked)
        (PifoGeneral.embedP rank other) = true)
    (hparent : parent (innerColor picked.1) = outerColor picked.1)
    (h : TandemForest rank outerColor innerColor parent state
      base outerTrees flatTrees) :
    ∀ child, child < outerTrees.length → outerColor picked.1 = base + child →
      (treePopAt flatTrees (innerColor picked.1) = none ∧
        treePopAt outerTrees child = none) ∨
      ∃ pkt flatTrees' outerTrees',
        treePopAt flatTrees (innerColor picked.1) = some (pkt, flatTrees') ∧
        treePopAt outerTrees child = some (pkt, outerTrees') ∧
        TandemForest rank outerColor innerColor parent
          (PifoGeneral.removeArr state picked.2) base outerTrees' flatTrees' := by
  intro child
  induction child generalizing base outerTrees with
  | zero =>
      intro hchild hcolor
      cases outerTrees with
      | nil => simp at hchild
      | cons outerTree outerTrees =>
        cases outerTree with
        | leaf q => exact False.elim h
        | node q innerTrees =>
          let keep : α → Bool := fun pkt => outerColor pkt == base
          have hkeep : keep picked.1 = true :=
            beq_iff_eq.mpr (by simpa [keep] using hcolor)
          have hqpop := qpop_selectorQueue_stateFilter rank innerColor keep
            state picked hd hmem hminimum hkeep
          rw [← h.1] at hqpop
          rcases innerMatches_popAt_both parent base 0 flatTrees innerTrees
              h.2.1 (innerColor picked.1) (by simpa [hparent] using hcolor) with
            hnone | ⟨pkt, flatTrees', innerTrees', hpFlat, hpInner, hinner⟩
          · left
            exact ⟨hnone.1, by simp [treePopAt, treePop, hqpop, hnone.2]⟩
          · right
            let restState := PifoGeneral.removeArr state picked.2
            let restQueue := selectorQueue rank innerColor
              (stateFilter keep restState)
            have htail := tandemForest_pop_after rank outerColor innerColor
              parent state (base + 1) outerTrees flatTrees picked pkt
              flatTrees' hd hmem hparent (by omega) hpFlat h.2.2
            refine ⟨pkt, flatTrees', .node restQueue innerTrees' :: outerTrees,
              hpFlat, ?_, ?_⟩
            · simp [treePopAt, treePop, hqpop, hpInner, restQueue, restState,
                keep]
            · exact ⟨rfl, hinner, htail⟩
  | succ child ih =>
      intro hchild hcolor
      cases outerTrees with
      | nil => simp at hchild
      | cons outerTree outerTrees =>
        cases outerTree with
        | leaf q => exact False.elim h
        | node q innerTrees =>
          have hne : outerColor picked.1 ≠ base := by omega
          have hkeep : (outerColor picked.1 == base) = false :=
            beq_eq_false_iff_ne.mpr hne
          have hcolor' : outerColor picked.1 = (base + 1) + child := by omega
          have hchild' : child < outerTrees.length := by
            simpa using hchild
          rcases ih (base + 1) outerTrees h.2.2 hchild' hcolor' with
            hnone | ⟨pkt, flatTrees', outerTrees', hpFlat, hpOuter, htail⟩
          · exact Or.inl ⟨hnone.1, by simp [treePopAt, hnone.2]⟩
          · right
            refine ⟨pkt, flatTrees', .node q innerTrees :: outerTrees',
              hpFlat, by simp [treePopAt, hpOuter], ?_⟩
            constructor
            · rw [h.1]
              exact congrArg (selectorQueue rank innerColor)
                (stateFilter_removeArr_other
                  (fun pkt => outerColor pkt == base) hd hmem hkeep).symm
            · constructor
              · apply innerMatches_popAt_flat_other parent base 0 flatTrees
                  innerTrees flatTrees' pkt h.2.1 (innerColor picked.1)
                · simpa [hparent] using hne
                · exact hpFlat
              · exact htail

theorem distinctArr_append_of_above {α : Type} {state : List (α × Nat)}
    {bound arrival : Nat} {pkt : α}
    (hd : PifoGeneral.DistinctArr state)
    (hbelow : PifoGeneral.AllBelow state bound) (htime : bound < arrival) :
    PifoGeneral.DistinctArr (state ++ [(pkt, arrival)]) := by
  refine List.pairwise_append.mpr ⟨hd, ?_, ?_⟩
  · exact List.Pairwise.cons (fun other hmem => nomatch hmem) List.Pairwise.nil
  · intro old hold fresh hfresh
    cases hfresh with
    | head =>
      intro heq
      have hle := hbelow old hold
      simp only at heq
      omega
    | tail fresh h => cases h

theorem treePushAt_length {α : Type} (pkt : α) (arrival : Nat)
    (trees : List (Tree α)) (child : Nat) (path : Path) :
    (treePushAt pkt arrival trees child path).length = trees.length := by
  induction trees generalizing child with
  | nil => rfl
  | cons tree trees ih =>
      cases child with
      | zero => rfl
      | succ child => simp [treePushAt, ih]

theorem treePopAt_length {α : Type} {trees trees' : List (Tree α)}
    {child : Nat} {pkt : α} (hpop : treePopAt trees child = some (pkt, trees')) :
    trees'.length = trees.length := by
  induction trees generalizing child trees' with
  | nil => simp [treePopAt] at hpop
  | cons tree trees ih =>
      cases child with
      | zero =>
        cases hp : treePop tree with
        | none => simp [treePopAt, hp] at hpop
        | some result =>
          obtain ⟨value, tree'⟩ := result
          simp only [treePopAt, hp, Option.some.injEq, Prod.mk.injEq] at hpop
          obtain ⟨rfl, rfl⟩ := hpop
          rfl
      | succ child =>
        cases hp : treePopAt trees child with
        | none => simp [treePopAt, hp] at hpop
        | some result =>
          obtain ⟨value, trees''⟩ := result
          simp only [treePopAt, hp, Option.some.injEq, Prod.mk.injEq] at hpop
          obtain ⟨rfl, rfl⟩ := hpop
          simp [ih hp]

theorem runTimedFrom_tandemCollapse {α : Type} [DecidableEq α]
    (rank outerColor innerColor : α → Nat) (parent : Nat → Nat)
    (tail : α → Path)
    (hparent : ∀ pkt, parent (innerColor pkt) = outerColor pkt) :
    ∀ (ops : List (TimedOp α)) (bound : Nat) (state : List (α × Nat))
      (outerTrees flatTrees : List (Tree α)),
      PifoGeneral.DistinctArr state →
      PifoGeneral.AllBelow state bound →
      TimedOpsAbove bound ops →
      (∀ pkt, outerColor pkt < outerTrees.length) →
      TandemForest rank outerColor innerColor parent state
        0 outerTrees flatTrees →
      runTimedFrom
        (fun pkt => .node (outerColor pkt) (rank pkt)
          (.node (innerColor pkt) (rank pkt) (tail pkt)))
        (.node (selectorQueue rank outerColor state) outerTrees) ops =
      runTimedFrom
        (fun pkt => .node (innerColor pkt) (rank pkt) (tail pkt))
        (.node (selectorQueue rank innerColor state) flatTrees) ops := by
  intro ops
  induction ops with
  | nil => intros; rfl
  | cons op ops ih =>
      intro bound state outerTrees flatTrees hd hbelow hops houter hforest
      cases op with
      | push pkt arrival =>
        obtain ⟨htime, hops⟩ := hops
        simp only [runTimedFrom, treePush]
        rw [selectorQueue_push, selectorQueue_push]
        apply ih arrival (state ++ [(pkt, arrival)])
          (treePushAt pkt arrival outerTrees (outerColor pkt)
            (.node (innerColor pkt) (rank pkt) (tail pkt)))
          (treePushAt pkt arrival flatTrees (innerColor pkt) (tail pkt))
        · exact distinctArr_append_of_above hd hbelow htime
        · exact PifoGeneral.allBelow_push hbelow htime
        · exact hops
        · intro value
          rw [treePushAt_length]
          exact houter value
        · apply tandemForest_push rank outerColor innerColor parent state 0
            outerTrees flatTrees pkt arrival (tail pkt) (hparent pkt) hforest
            (outerColor pkt)
          simp
      | pop =>
        simp only [runTimedFrom]
        cases state with
        | nil =>
          simp only [selectorQueue, PifoGeneral.qstate, List.map, treePop, qpop]
          exact congrArg (fun output => none :: output)
            (ih bound [] outerTrees flatTrees hd hbelow hops houter hforest)
        | cons first rest =>
          obtain ⟨picked, hmem, hrootPop, hminimum⟩ :=
            PifoGeneral.qpop_state rank first rest hd
          have houterPop := qpop_selectorQueue rank outerColor hrootPop
          have hflatPop := qpop_selectorQueue rank innerColor hrootPop
          rw [treePop, houterPop, treePop, hflatPop]
          rcases tandemForest_pop rank outerColor innerColor parent
              (first :: rest) 0 outerTrees flatTrees picked hd hmem hminimum
              (hparent picked.1) hforest (outerColor picked.1)
              (houter picked.1) (by simp) with
            hnone | ⟨pkt, flatTrees', outerTrees', hpFlat, hpOuter, hforest'⟩
          · simp only [hnone.1, hnone.2]
            exact congrArg (fun output => none :: output)
              (ih bound (first :: rest) outerTrees flatTrees hd hbelow hops
                houter hforest)
          · simp only [hpFlat, hpOuter, List.cons.injEq, true_and]
            apply ih bound (PifoGeneral.removeArr (first :: rest) picked.2)
              outerTrees' flatTrees'
            · exact PifoGeneral.distinctArr_removeArr hd
            · exact PifoGeneral.allBelow_removeArr hbelow
            · exact hops
            · intro value
              rw [treePopAt_length hpOuter]
              exact houter value
            · exact hforest'

theorem innerMatches_refl {α : Type} (parent : Nat → Nat) (block : Nat) :
    ∀ (cell : Nat) (trees : List (Tree α)),
      InnerMatches parent block cell trees trees := by
  intro cell trees
  induction trees generalizing cell with
  | nil => trivial
  | cons tree trees ih =>
    exact ⟨fun _ => rfl, ih (cell + 1)⟩

theorem tandemForest_empty {α : Type} [BEq α]
    (rank outerColor innerColor : α → Nat) (parent : Nat → Nat)
    (outerTopologies flatTopologies : List Topology) :
    TandemForest rank outerColor innerColor parent [] 0
      (emptyForest
        (outerTopologies.map (fun _ => Topology.node flatTopologies)))
      (emptyForest flatTopologies) := by
  have aux : ∀ (topologies : List Topology) (base : Nat),
      TandemForest rank outerColor innerColor parent [] base
        (emptyForest
          (topologies.map (fun _ => Topology.node flatTopologies)))
        (emptyForest flatTopologies) := by
    intro topologies
    induction topologies with
    | nil => intros; trivial
    | cons topology topologies ih =>
      intro base
      constructor
      · rfl
      · exact ⟨innerMatches_refl parent base 0 (emptyForest flatTopologies),
          ih (base + 1)⟩
  exact aux outerTopologies 0

theorem emptyForest_length {α : Type} (topologies : List Topology) :
    (emptyForest (α := α) topologies).length = topologies.length := by
  induction topologies with
  | nil => rfl
  | cons topology topologies ih => simp [emptyForest, ih]

theorem tandemOne_commonRootOne_interEquiv {k : Nat}
    (root₁ root₂ : NormalRoot k) :
    interEquiv (tandemOne root₁ root₂)
      (commonRootOne root₁ root₂).scheduler := by
  intro ops
  let common := commonRootOne root₁ root₂
  let rank := commonRank root₁ root₂
  let color := commonColor root₁ root₂
  let parent : Nat → Nat := fun cell => cell / root₂.topologies.length
  let outerTopologies : List Topology :=
    root₁.topologies.map (fun _ => Topology.node common.topologies)
  unfold run
  rw [runFrom_eq_runTimedFrom, runFrom_eq_runTimedFrom]
  change runTimedFrom
      (fun pkt => .node (root₁.color pkt) (rank pkt)
        (.node (color pkt) (rank pkt) (root₁.tail pkt)))
      (.node [] (emptyForest outerTopologies)) (timedOpsFrom 0 ops) =
    runTimedFrom
      (fun pkt => .node (color pkt) (rank pkt) (root₁.tail pkt))
      (.node [] (emptyForest common.topologies)) (timedOpsFrom 0 ops)
  have hrun := runTimedFrom_tandemCollapse rank root₁.color color parent
    root₁.tail (fun pkt => by
      exact commonColor_div root₁ root₂ pkt)
    (timedOpsFrom 0 ops) 0 [] (emptyForest outerTopologies)
    (emptyForest common.topologies) List.Pairwise.nil (by
      intro entry hmem
      cases hmem) (timedOpsFrom_above 0 ops) (by
      intro pkt
      rw [emptyForest_length]
      simpa [outerTopologies] using root₁.color_lt pkt) (by
      change TandemForest rank root₁.color color parent [] 0
        (emptyForest
          (root₁.topologies.map (fun _ => Topology.node common.topologies)))
        (emptyForest common.topologies)
      exact tandemForest_empty rank root₁.color color parent
        root₁.topologies common.topologies)
  simpa [selectorQueue, PifoGeneral.qstate] using hrun

noncomputable def tandemTwo {k : Nat} (root₁ root₂ : NormalRoot k) :
    Scheduler k :=
  let common := commonRootTwo root₁ root₂
  ⟨.node (root₂.topologies.map (fun _ => .node common.topologies)),
    fun pkt => .node (root₂.color pkt) (commonRank root₁ root₂ pkt)
      (.node (commonColor root₁ root₂ pkt) (commonRank root₁ root₂ pkt)
        (root₂.tail pkt))⟩

theorem rootTwoRank_tandemTwo_interEquiv {k : Nat}
    (ih : ∀ m, m < k → InterleavedTheoremAt m)
    (root₁ root₂ : NormalRoot k)
    (hflush : flushEquiv root₁.scheduler root₂.scheduler) :
    interEquiv (root₂.rankReplacement (commonRank root₁ root₂))
      (tandemTwo root₁ root₂) := by
  let common := commonRootTwo root₁ root₂
  let outerTopologies : List Topology :=
    root₂.topologies.map (fun _ => Topology.node common.topologies)
  have hlength : root₂.topologies.length = outerTopologies.length := by
    simp [outerTopologies]
  have hglobalFlush : flushEquiv root₂.scheduler common.scheduler :=
    rootTwo_commonRootTwo_flushEquiv root₁ root₂ hflush
  have hforest : ForestTimedEquivAfterAt (fun _ : Fin k => True)
      root₂.color root₂.tail common.assign 0 0
      (emptyForest root₂.topologies) (emptyForest outerTopologies) := by
    apply emptyForestTimedEquivAfterAt_of_children root₂.color root₂.tail
      common.assign 0 0 root₂.topologies outerTopologies hlength
    intro index topology₁ topology₂ hat₁ hat₂
    simp only [Nat.zero_add]
    by_cases hmember : ∃ member : Fin k, root₂.color member = index
    · obtain ⟨member, hmember⟩ := hmember
      let embed := fiberEmbedding root₂.color (root₂.color member)
      let originalRestricted := schedulerComap root₂.scheduler embed
      let commonRestricted := schedulerComap common.scheduler embed
      have htopology₁ : root₂.childTopology member = topology₁ := by
        apply (root₂.childTopology_at member).unique
        rw [hmember]
        exact hat₁
      have htopology₂ : common.scheduler.topo = topology₂ := by
        have hmapped : ListAt (Topology.node common.topologies)
            outerTopologies index := by
          change ListAt (Topology.node common.topologies)
            (root₂.topologies.map
              (fun _ => Topology.node common.topologies)) index
          exact hat₁.mapValue (fun _ => Topology.node common.topologies)
        exact hmapped.unique hat₂
      have hchildOriginal := root₂.child_rootRestriction_interEquiv member
      have hrestrictedFlush : flushEquiv originalRestricted commonRestricted :=
        flushEquiv_schedulerComap root₂.scheduler common.scheduler embed
          (fiberEmbedding_injective root₂.color (root₂.color member))
          hglobalFlush
      have hchildFlush : flushEquiv (root₂.childScheduler member)
          commonRestricted := by
        intro word
        exact (hchildOriginal (flushOps word)).trans (hrestrictedFlush word)
      have hchild : interEquiv (root₂.childScheduler member)
          commonRestricted := by
        apply ih (fiberList root₂.color (root₂.color member)).length
          (root₂.childAlphabet_lt member)
        · exact root₂.childScheduler_valid member
        · exact schedulerComap_valid common.scheduler embed common.valid
        · exact hchildFlush
      have hrestricted := timedEquivAfter_empty_of_restrict
        (root₂.childScheduler member) commonRestricted embed
        (fiberProjection root₂.color member) (by
          intro pkt (hp : True ∧ root₂.color pkt = index)
          exact fiberProjection_rightInverse root₂.color member pkt
            (hp.2.trans hmember.symm))
        root₂.tail common.assign (fun _ => rfl) (fun _ => rfl) hchild 0
      change TimedEquivAfter
        (fun pkt => True ∧ root₂.color pkt = index) 0 root₂.tail
        (emptyTree (root₂.childTopology member)) common.assign
        (emptyTree common.scheduler.topo) at hrestricted
      rw [htopology₁, htopology₂] at hrestricted
      exact hrestricted
    · apply timedEquivAfter_empty_of_no_values
      intro pkt hp
      exact hmember ⟨pkt, hp.2⟩
  apply interEquiv_of_timedEquivAfter_empty
    (root₂.rankReplacement (commonRank root₁ root₂)) (tandemTwo root₁ root₂)
  change TimedEquivAfter (fun _ : Fin k => True) 0
    (fun pkt => .node (root₂.color pkt) (commonRank root₁ root₂ pkt)
      (root₂.tail pkt)) (emptyTree (.node root₂.topologies))
    (fun pkt => .node (root₂.color pkt) (commonRank root₁ root₂ pkt)
      (common.assign pkt)) (emptyTree (.node outerTopologies))
  exact nodeTimedEquivAfter _ _ root₂.color (commonRank root₁ root₂)
    root₂.tail common.assign (by intros; rfl) (by intros; rfl) 0 []
    (emptyForest root₂.topologies) (emptyForest outerTopologies) hforest

theorem tandemTwo_commonRootTwo_interEquiv {k : Nat}
    (root₁ root₂ : NormalRoot k) :
    interEquiv (tandemTwo root₁ root₂)
      (commonRootTwo root₁ root₂).scheduler := by
  intro ops
  let common := commonRootTwo root₁ root₂
  let rank := commonRank root₁ root₂
  let color := commonColor root₁ root₂
  let parent : Nat → Nat := fun cell => cell % root₂.topologies.length
  let outerTopologies : List Topology :=
    root₂.topologies.map (fun _ => Topology.node common.topologies)
  unfold run
  rw [runFrom_eq_runTimedFrom, runFrom_eq_runTimedFrom]
  change runTimedFrom
      (fun pkt => .node (root₂.color pkt) (rank pkt)
        (.node (color pkt) (rank pkt) (root₂.tail pkt)))
      (.node [] (emptyForest outerTopologies)) (timedOpsFrom 0 ops) =
    runTimedFrom
      (fun pkt => .node (color pkt) (rank pkt) (root₂.tail pkt))
      (.node [] (emptyForest common.topologies)) (timedOpsFrom 0 ops)
  have hrun := runTimedFrom_tandemCollapse rank root₂.color color parent
    root₂.tail (fun pkt => by
      exact commonColor_mod root₁ root₂ pkt)
    (timedOpsFrom 0 ops) 0 [] (emptyForest outerTopologies)
    (emptyForest common.topologies) List.Pairwise.nil (by
      intro entry hmem
      cases hmem) (timedOpsFrom_above 0 ops) (by
      intro pkt
      rw [emptyForest_length]
      simpa [outerTopologies] using root₂.color_lt pkt) (by
      change TandemForest rank root₂.color color parent [] 0
        (emptyForest
          (root₂.topologies.map (fun _ => Topology.node common.topologies)))
        (emptyForest common.topologies)
      exact tandemForest_empty rank root₂.color color parent
        root₂.topologies common.topologies)
  simpa [selectorQueue, PifoGeneral.qstate] using hrun

theorem interleavedTheoremAt_all : ∀ k, InterleavedTheoremAt k := by
  intro k
  refine Nat.strongRecOn (motive := InterleavedTheoremAt) k ?_
  intro k ih
  by_cases hsmall : k < 3
  · exact interleavedTheoremAt_of_lt_three k hsmall
  · intro S₁ S₂ hvalid₁ hvalid₂ hflush
    have hk : 2 ≤ k := by omega
    obtain ⟨root₁, hnormalize₁⟩ :=
      normalizeRootScheduler hk S₁ hvalid₁
    obtain ⟨root₂, hnormalize₂⟩ :=
      normalizeRootScheduler hk S₂ hvalid₂
    have hrootFlush : flushEquiv root₁.scheduler root₂.scheduler := by
      intro word
      exact (hnormalize₁ (flushOps word)).symm.trans
        ((hflush word).trans (hnormalize₂ (flushOps word)))
    have hroot₁Common : interEquiv root₁.scheduler
        (commonRootOne root₁ root₂).scheduler :=
      interEquiv_trans (rootOne_commonRank_interEquiv root₁ root₂ hrootFlush)
        (interEquiv_trans
          (rootOneRank_tandemOne_interEquiv ih root₁ root₂ hrootFlush)
          (tandemOne_commonRootOne_interEquiv root₁ root₂))
    have hroot₂Common : interEquiv root₂.scheduler
        (commonRootTwo root₁ root₂).scheduler :=
      interEquiv_trans (rootTwo_commonRank_interEquiv root₁ root₂ hrootFlush)
        (interEquiv_trans
          (rootTwoRank_tandemTwo_interEquiv ih root₁ root₂ hrootFlush)
          (tandemTwo_commonRootTwo_interEquiv root₁ root₂))
    have hcommon := commonRoots_interEquiv ih root₁ root₂ hrootFlush
    exact interEquiv_trans hnormalize₁
      (interEquiv_trans hroot₁Common
        (interEquiv_trans hcommon
          (interEquiv_trans (interEquiv_symm hroot₂Common)
            (interEquiv_symm hnormalize₂))))



theorem answer : PifoStatement.statelessFlushImpliesInterleaved := by
  intro k S₁ S₂ hvalid₁ hvalid₂ hflush
  exact interleavedTheoremAt_all k S₁ S₂ hvalid₁ hvalid₂ hflush

end PifoFull
