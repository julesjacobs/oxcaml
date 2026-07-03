(* A little separation-logic universe, with no dedicated checker
   support: one unforgeable, unduplicable token OWNS a heap of
   allocated references, and the token's refinement carries a
   separation-logic assertion about that heap, REIFIED as an ordinary
   variant ([hprop]) -- assertions are first-class values, so the
   frame rule is an explicit [hprop] parameter the caller
   instantiates.

   The model lives wholly in the block below: heaps are partial maps
   [Int -> Option Int]; [sat] reads an assertion as the EXACT
   footprint it describes ([Pts (r, v)] is the singleton heap at [r]);
   [Star] splits the heap into disjoint halves.  Every law the demo
   leans on -- commutativity, the rotation that reaches a buried
   chunk, and pointer distinctness out of disjointness -- is a THEOREM
   proved by unfolding; nothing is axiomatized.  [rid] (a reference's
   location) and [hp] (the heap a token snapshot witnesses) are the
   opaque bridge, and the implementation's [assume_unchecked_] at each
   operation is the whole trust story, exactly as in pcell_lib.

   Rearrangement is the caller's job, discharged by the exported
   lemmas; a deeper shuffle than the demo's may need its own
   [%%vox.lean] lemma.  Sharp edges, not bugs.

   TRUSTED: [href]/[htoken] abstract (unforgeable), tokens boxed and
   consumed @ unique -- the mode checker is the ownership checker. *)

type href
type htoken

type hprop =
  | Emp
  | Pts of href * int
  | Star of hprop * hprop

[%%vox.lean {lean|
opaque rid : VoxU -> Int
abbrev HeapM := Int -> Option Int
opaque hp : VoxU -> HeapM

@[grind] def sat : Vox_Sep_lib_hprop -> HeapM -> Prop
  | .Emp, h => ∀ x, h x = none
  | .Pts r v, h => h (rid r) = some v ∧ ∀ x, x ≠ rid r → h x = none
  | .Star p q, h =>
      ∃ h₁ h₂,
        (∀ x, h₁ x = none ∨ h₂ x = none)
        ∧ (∀ x, h x = match h₁ x with | some v => some v | none => h₂ x)
        ∧ sat p h₁ ∧ sat q h₂

theorem sat_star_comm (p q : Vox_Sep_lib_hprop) (h : HeapM) :
    sat (.Star p q) h ↔ sat (.Star q p) h := by
  constructor <;> rintro ⟨h₁, h₂, hd, hu, s₁, s₂⟩ <;>
    exact ⟨h₂, h₁, fun x => (hd x).symm,
           fun x => by have := hd x; have := hu x;
                       cases hx : h₁ x <;> cases hx' : h₂ x <;> grind,
           s₂, s₁⟩
grind_pattern sat_star_comm => sat (.Star p q) h

theorem sat_star_rot (a b q : Vox_Sep_lib_hprop) (h : HeapM) :
    sat (.Star a (.Star b q)) h ↔ sat (.Star b (.Star a q)) h := by
  constructor <;>
    rintro ⟨ha, hbq, hd, hu, sa, hb, hq, hd', hu', sb, sq⟩ <;>
    refine ⟨hb, fun x => match ha x with | some v => some v | none => hq x,
            fun x => ?_, fun x => ?_, sb, ha, hq,
            fun x => ?_, fun x => rfl, sa, sq⟩ <;>
    have := hd x <;> have := hu x <;> have := hd' x <;> have := hu' x <;>
    cases hx : ha x <;> cases hx' : hb x <;> cases hx'' : hq x <;> grind
grind_pattern sat_star_rot => sat (.Star a (.Star b q)) h

theorem sat_pts_neq (r r' : VoxU) (a b : Int) (q : Vox_Sep_lib_hprop)
    (h : HeapM) :
    sat (.Star (.Pts r a) (.Star (.Pts r' b) q)) h → ¬ (r = r') := by
  rintro ⟨h₁, h₂, hd, hu, ⟨s₁, _⟩, h₃, h₄, hd', hu', ⟨s₃, _⟩, _⟩ heq
  subst heq
  have := hd (rid r); have := hu' (rid r)
  grind
grind_pattern sat_pts_neq => sat (.Star (.Pts r a) (.Star (.Pts r' b) q)) h
|lean}]

(* The empty universe: a fresh token owning nothing.  (Each [init]
   mints its OWN ghost heap; tokens from different universes cannot
   prove anything about each other's references.) *)
val init : unit -> htoken{ sat Emp (hp _) } @ unique

(* Allocation extends the assertion by a fresh chunk; freshness (the
   disjointness [Star] demands) is part of the trusted bridge,
   justified by the real allocator. *)
val alloc :
  (p : hprop) -> (v : int) ->
  htoken{ sat p (hp _) } @ unique ->
  (href * htoken){ sat (Star (Pts (fst _, v), p)) (hp (snd _)) } @ unique

val get :
  (r : href) -> (k : int) -> (p : hprop) ->
  htoken{ sat (Star (Pts (r, k), p)) (hp _) } @ unique ->
  (int{ _ = k } * htoken{ sat (Star (Pts (r, k), p)) (hp _) }) @ unique

val set :
  (r : href) -> (old : int) -> (v : int) -> (p : hprop) ->
  htoken{ sat (Star (Pts (r, old), p)) (hp _) } @ unique ->
  htoken{ sat (Star (Pts (r, v), p)) (hp _) } @ unique
