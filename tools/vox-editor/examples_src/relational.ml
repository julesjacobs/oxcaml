(* Higher-order refinement, EXACT output.  A relation supplied as a call-site
   lambda specifies a combinator by RELATING its result to its input; the
   callback f is never modeled, only its per-element contract -- the relation.
   Picking the relation to be the callback's GRAPH (y = x + 1) turns the
   relational spec into a COMPLETE one: at a concrete count grind proves the
   exact value directly; at a symbolic count the induction law
   relIter_succ_exact -- proved right in the block below -- closes it. *)

[@@@warning "-6-32-26-27"]

[%%vox.lean {lean|
abbrev IntRel := Int -> Int -> Prop
@[grind, expose] def rHolds (r : IntRel) (a b : Int) : Prop := r a b

-- relIterN r n x y : y is reached from x by n r-steps (prepend fixpoint, so a
-- forward recursion matches it definitionally); relIter feeds it n.toNat.
@[grind, expose] def relIterN (r : IntRel) : Nat -> Int -> Int -> Prop
  | 0,     x, y => x = y
  | (k+1), x, y => exists z, r x z /\ relIterN r k z y
@[grind, expose] def relIter (r : IntRel) (n : Int) (x y : Int) : Prop :=
  relIterN r n.toNat x y

-- toNat bridges: reduce the Int fuel at the two shapes the forward recursion
-- hits (a non-positive count clamps to zero steps; a positive one peels one).
@[grind] theorem toNat_nonpos (m : Int) (h : m <= 0) : m.toNat = 0 := by omega
@[grind] theorem toNat_succ (m : Int) (h : 1 <= m) :
    m.toNat = (m - 1).toNat + 1 := by omega

-- The EXACT law: over an ABSTRACT r whose graph is (b = a + 1), n steps land
-- exactly at x + n.  Stated with the graph as a PREMISE and a variable-r
-- grind_pattern -- a lambda in the pattern would NEVER fire, because grind
-- arithmetic-normalizes lambda bodies at indexing.  The premise then
-- discharges by beta against the reflected call-site lambda.
theorem relIterN_succ_exact (r : IntRel) (hr : ∀ a b, r a b → b = a + 1) :
    ∀ (n : Nat) (x y : Int), relIterN r n x y → y = x + n := by
  intro n
  induction n with
  | zero => intro x y h; simp [relIterN] at h; omega
  | succ m ih =>
      intro x y h
      simp only [relIterN] at h
      obtain ⟨z, hz, hrest⟩ := h
      have h1 := hr x z hz
      have h2 := ih z y hrest
      omega
theorem relIter_succ_exact (r : IntRel) (k x y : Int)
    (hr : ∀ a b, r a b → b = a + 1) (hk : k ≥ 0)
    (h : relIter r k x y) : y = x + k := by
  have hn := relIterN_succ_exact r hr k.toNat x y (by simpa [relIter] using h)
  omega
grind_pattern relIter_succ_exact => relIter r k x y
|lean}]

(* iter r f x0 n : apply f n times from x0; the result is n r-steps from x0.
   The relation r is a DEPENDENT parameter of function type (parenthesised --
   the binder grammar needs it); f's contract rHolds r x _ ties each output to
   its input through r. *)
let iter :
      (r : (int -> int -> bool)) ->
      (f : ((x : int) -> int{ rHolds r x _ })) ->
      (x0 : int) -> (n : int) -> int{ relIter r n x0 _ } =
  fun r f x0 n ->
    ignore r;
    let rec go : (a : int) -> (m : int) -> int{ relIter r m a _ } =
      fun a m ->
        if m <= 0 then (a : int{ relIter r m a _ })
        else
          let b = f a in
          let res = go b (m - 1) in
          (res : int{ relIter r m a _ })
    in
    go x0 n

(* CONCRETE count: three (+1) steps land exactly at x0 + 3.  grind unfolds
   relIter at the literal fuel -- no induction law needed here. *)
let plus3 (x0 : int) : int{ _ = x0 + 3 } =
  iter (fun x y -> y = x + 1) (fun a -> a + 1) x0 3

(* SYMBOLIC count: relIter_succ_exact fires on the relIter postcondition and
   discharges the graph premise by beta against the call-site lambda -- the
   COMPLETE spec for an unknown number of steps. *)
let plusk (x0 : int) (k : int) : int{ k >= 0 -> _ = x0 + k } =
  iter (fun x y -> y = x + 1) (fun a -> a + 1) x0 k
