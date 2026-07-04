(* Linked lists in the separation-logic universe (sep_lib's recipe,
   one level up): heap cells are NODES (a value and a next link), and
   the assertion language gains a RECURSIVE constructor -- [Lseg (l,
   vs)] owns exactly the cells of a null-terminated segment holding
   the model list [vs].  Assertions stay first-class values, the frame
   stays an explicit parameter, and the model stays wholly in the
   block below with every law a theorem: folding a node onto a
   segment, discarding an empty segment, minting one, rotating, and
   reaching a fold PAST an intervening chunk.

   [uncons] is separation logic's existential elimination as an
   ordinary unpack: a non-empty segment's next pointer exists
   existentially inside [Lseg], and the operation returns it as a
   VALUE, rebinding the assertion to the unfolded [Node ... * Lseg
   ...] -- the client gets a name where the logic had a witness.

   The model list [ilist] is an ordinary variant, so specs can apply
   reflected functions to it ([total_ rev_append] in the demo: in-place
   reversal against the textbook specification).

   TRUSTED: [href]/[htoken] abstract, tokens consumed @ unique; the
   implementation's [assume_unchecked_] at each operation is the whole
   trust story ([alloc_node]'s carries freshness). *)

type href
type htoken

type link =
  | Null
  | Ptr of href

type ilist =
  | INil
  | ICons of int * ilist

type hprop =
  | Emp
  | Node of href * int * link
  | Lseg of link * ilist
  | Star of hprop * hprop

(* Result records: the token field stays unique; a link or node is a
   shareable NAME (ownership lives in the assertion), so those fields
   carry the aliased modality. *)
type alloced =
  { nod : href @@ aliased
  ; ntok : htoken
  }

type stepped =
  { lnk : link @@ aliased
  ; stok : htoken
  }

[%%vox.lean {lean|
public opaque rid : VoxU -> Int
public abbrev HeapM := Int -> Option (Int × Vox_Seplist_lib_link)
public opaque hp : VoxU -> HeapM

public abbrev hsplit (h h₁ h₂ : HeapM) : Prop :=
  (∀ x, h₁ x = none ∨ h₂ x = none)
  ∧ (∀ x, h x = match h₁ x with | some c => some c | none => h₂ x)

public abbrev cell (r : VoxU) (v : Int) (nxt : Vox_Seplist_lib_link)
    (h : HeapM) : Prop :=
  h (rid r) = some (v, nxt) ∧ ∀ x, x ≠ rid r → h x = none

@[grind, expose] public def lseg :
    Vox_Seplist_lib_link -> Vox_Seplist_lib_ilist -> HeapM -> Prop
  | .Null, .INil, h => ∀ x, h x = none
  | .Ptr r, .ICons v vs, h =>
      ∃ nxt h₁ h₂, hsplit h h₁ h₂ ∧ cell r v nxt h₁ ∧ lseg nxt vs h₂
  | _, _, _ => False

@[grind, expose] public def sat : Vox_Seplist_lib_hprop -> HeapM -> Prop
  | .Emp, h => ∀ x, h x = none
  | .Node r v nxt, h => cell r v nxt h
  | .Lseg l vs, h => lseg l vs h
  | .Star p q, h => ∃ h₁ h₂, hsplit h h₁ h₂ ∧ sat p h₁ ∧ sat q h₂

public theorem sat_star_rot (a b q : Vox_Seplist_lib_hprop) (h : HeapM) :
    sat (.Star a (.Star b q)) h ↔ sat (.Star b (.Star a q)) h := by
  constructor <;>
    rintro ⟨ha, hbq, ⟨hd, hu⟩, sa, hb, hq, ⟨hd', hu'⟩, sb, sq⟩ <;>
    refine ⟨hb, fun x => match ha x with | some c => some c | none => hq x,
            ⟨fun x => ?_, fun x => ?_⟩, sb, ha, hq,
            ⟨fun x => ?_, fun x => rfl⟩, sa, sq⟩ <;>
    have := hd x <;> have := hu x <;> have := hd' x <;> have := hu' x <;>
    cases hx : ha x <;> cases hx' : hb x <;> cases hx'' : hq x <;> grind
grind_pattern sat_star_rot => sat (.Star a (.Star b q)) h

-- An empty segment owns nothing: mint one in front, or discard one.
public theorem lseg_nil_intro (p : Vox_Seplist_lib_hprop) (h : HeapM) :
    sat p h → sat (.Star (.Lseg .Null .INil) p) h := by
  intro s
  exact ⟨fun _ => none, h, ⟨fun x => .inl rfl, fun x => rfl⟩,
         fun x => rfl, s⟩
grind_pattern lseg_nil_intro => sat (.Star (.Lseg .Null .INil) p) h

public theorem lseg_nil_elim (l : Vox_Seplist_lib_link)
    (q : Vox_Seplist_lib_hprop) (h : HeapM) :
    sat (.Star (.Lseg l .INil) q) h → sat q h := by
  rintro ⟨h₁, h₂, ⟨hd, hu⟩, s₁, s₂⟩
  cases l with
  | Null =>
      have hh : h = h₂ :=
        funext (fun x => by have := hu x; have := s₁ x; grind)
      exact hh ▸ s₂
  | Ptr r => exact absurd s₁ (by simp [sat, lseg])
grind_pattern lseg_nil_elim => sat (.Star (.Lseg l .INil) q) h

-- A node pointing nowhere is a singleton segment.
public theorem lseg_singleton (r : VoxU) (v : Int) (p : Vox_Seplist_lib_hprop)
    (h : HeapM) :
    sat (.Star (.Node r v .Null) p) h →
    sat (.Star (.Lseg (.Ptr r) (.ICons v .INil)) p) h := by
  rintro ⟨hn, hpp, hs, sn, sp⟩
  exact ⟨hn, hpp, hs,
         ⟨.Null, hn, fun _ => none,
          ⟨fun x => .inr rfl, fun x => by cases hx : hn x <;> rfl⟩,
          sn, fun x => rfl⟩, sp⟩
grind_pattern lseg_singleton => sat (.Star (.Node r v .Null) p) h

-- Folding a node onto the segment it points at...
public theorem lseg_fold (r : VoxU) (v : Int) (l : Vox_Seplist_lib_link)
    (vs : Vox_Seplist_lib_ilist) (p : Vox_Seplist_lib_hprop) (h : HeapM) :
    sat (.Star (.Node r v l) (.Star (.Lseg l vs) p)) h →
    sat (.Star (.Lseg (.Ptr r) (.ICons v vs)) p) h := by
  rintro ⟨hn, hrest, ⟨hd, hu⟩, sn, hl, hpp, ⟨hd', hu'⟩, sl, sp⟩
  refine ⟨fun x => match hn x with | some c => some c | none => hl x, hpp,
          ⟨fun x => ?_, fun x => ?_⟩,
          ⟨l, hn, hl, ⟨fun x => ?_, fun x => rfl⟩, sn, sl⟩, sp⟩ <;>
    have := hd x <;> have := hu x <;> have := hd' x <;> have := hu' x <;>
    obtain ⟨sn1, sn2⟩ := sn <;> have := sn2 x <;>
    cases hx : hn x <;> cases hx' : hl x <;> cases hx'' : hpp x <;> grind
grind_pattern lseg_fold => sat (.Star (.Node r v l) (.Star (.Lseg l vs) p)) h

-- ... and the same fold reaching PAST an intervening chunk, the shape
-- one write step leaves behind.
public theorem lseg_fold_deep (r : VoxU) (v : Int) (acc : Vox_Seplist_lib_link)
    (ws : Vox_Seplist_lib_ilist) (q p : Vox_Seplist_lib_hprop) (h : HeapM) :
    sat (.Star (.Node r v acc) (.Star q (.Star (.Lseg acc ws) p))) h →
    sat (.Star (.Lseg (.Ptr r) (.ICons v ws)) (.Star q p)) h := by
  intro s
  have s' : sat (.Star q (.Star (.Node r v acc) (.Star (.Lseg acc ws) p))) h :=
    (sat_star_rot _ _ _ h).mp s
  obtain ⟨hq, hrest, hs, sq, srest⟩ := s'
  have sfold : sat (.Star (.Lseg (.Ptr r) (.ICons v ws)) p) hrest :=
    lseg_fold r v acc ws p hrest srest
  have : sat (.Star q (.Star (.Lseg (.Ptr r) (.ICons v ws)) p)) h :=
    ⟨hq, hrest, hs, sq, sfold⟩
  exact (sat_star_rot _ _ _ h).mp this
grind_pattern lseg_fold_deep =>
  sat (.Star (.Node r v acc) (.Star q (.Star (.Lseg acc ws) p))) h
|lean}]

(* The empty universe. *)
val init : unit -> htoken{ sat Emp (hp _) } @ unique

val alloc_node :
  (p : hprop) -> (v : int) -> (nxt : link) ->
  htoken{ sat p (hp _) } @ unique ->
  alloced{ sat (Star (Node (_.nod, v, nxt), p)) (hp _.ntok) } @ unique

(* Existential elimination: a non-empty segment unfolds, and the
   witness next pointer comes back as a value. *)
val uncons :
  (r : href) -> (v : int) -> (vs : ilist) -> (p : hprop) ->
  htoken{ sat (Star (Lseg (Ptr r, ICons (v, vs)), p)) (hp _) } @ unique ->
  stepped{ sat (Star (Node (r, v, _.lnk), Star (Lseg (_.lnk, vs), p)))
               (hp _.stok) } @ unique

val get_val :
  (r : href) -> (v : int) -> (nxt : link) -> (p : hprop) ->
  htoken{ sat (Star (Node (r, v, nxt), p)) (hp _) } @ unique ->
  (int{ _ = v } * htoken{ sat (Star (Node (r, v, nxt), p)) (hp _) }) @ unique

val set_next :
  (r : href) -> (v : int) -> (old : link) -> (nxt : link) -> (p : hprop) ->
  htoken{ sat (Star (Node (r, v, old), p)) (hp _) } @ unique ->
  htoken{ sat (Star (Node (r, v, nxt), p)) (hp _) } @ unique
