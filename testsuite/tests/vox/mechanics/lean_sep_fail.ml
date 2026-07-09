(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* sep_lib soundness probes: the attacks that must NOT verify.  The
   API is ascribed inline (same signatures as sep_lib, which the
   positive test lean_sep.ml exercises); the assertion layer is
   enforced by Lean (an unowned chunk has no supporting fact; two
   starred chunks are provably distinct) and the ownership layer by
   the uniqueness mode checker (a token cannot be used twice). *)

[%%vox.lean {lean|
opaque rid : VoxU -> Int
abbrev HeapM := Int -> Option Int
opaque hp : VoxU -> HeapM

@[grind] def sat : Vox_S_hprop -> HeapM -> Prop
  | .Emp, h => ∀ x, h x = none
  | .Pts r v, h => h (rid r) = some v ∧ ∀ x, x ≠ rid r → h x = none
  | .Star p q, h =>
      ∃ h₁ h₂,
        (∀ x, h₁ x = none ∨ h₂ x = none)
        ∧ (∀ x, h x = match h₁ x with | some v => some v | none => h₂ x)
        ∧ sat p h₁ ∧ sat q h₂

theorem sat_pts_neq (r r' : VoxU) (a b : Int) (q : Vox_S_hprop)
    (h : HeapM) :
    sat (.Star (.Pts r a) (.Star (.Pts r' b) q)) h → ¬ (r = r') := by
  rintro ⟨h₁, h₂, hd, hu, ⟨s₁, _⟩, h₃, h₄, hd', hu', ⟨s₃, _⟩, _⟩ heq
  subst heq
  have := hd (rid r); have := hu' (rid r)
  grind
grind_pattern sat_pts_neq => sat (.Star (.Pts r a) (.Star (.Pts r' b) q)) h
|lean}]
[%%expect{|
|}]

module S : sig
  type href
  type htoken

  type hprop =
    | Emp
    | Pts of href * int
    | Star of hprop * hprop

  val alloc :
    (p : hprop) -> (v : int) ->
    htoken{ sat p (hp _) } @ unique ->
    (href * htoken){ sat (Star (Pts (fst _, v), p)) (hp (snd _)) } @ unique

  val get :
    (r : href) -> (k : int) -> (p : hprop) ->
    htoken{ sat (Star (Pts (r, k), p)) (hp _) } @ unique ->
    (int{ _ = k } * htoken{ sat (Star (Pts (r, k), p)) (hp _) }) @ unique
end = struct
  type href = { mutable c_ : int }
  type htoken = Tok of { g_ : unit }

  type hprop =
    | Emp
    | Pts of href * int
    | Star of hprop * hprop

  let alloc :
    (p : hprop) -> (v : int) ->
    htoken{ sat p (hp _) } @ unique ->
    (href * htoken){ sat (Star (Pts (fst _, v), p)) (hp (snd _)) } @ unique =
    fun p v t ->
      ignore p; ignore t;
      assume_unchecked_ ({ c_ = v }, Tok { g_ = () })

  let get :
    (r : href) -> (k : int) -> (p : hprop) ->
    htoken{ sat (Star (Pts (r, k), p)) (hp _) } @ unique ->
    (int{ _ = k } * htoken{ sat (Star (Pts (r, k), p)) (hp _) }) @ unique =
    fun r k p t ->
      ignore k; ignore p; ignore t;
      ( (assume_unchecked_ r.c_ : int{ _ = k }),
        (assume_unchecked_ (Tok { g_ = () })
          : htoken{ sat (Star (Pts (r, k), p)) (hp _) }) )
end
[%%expect{|
module S :
  sig
    type href
    type htoken
    type hprop = Emp | Pts of href * int | Star of hprop * hprop
    val alloc :
      (p : hprop) ->
      (v : int) ->
      htoken{ sat p (hp _) } @ unique ->
      (href * htoken){ sat (Star (Pts (fst _, v), p)) (hp (snd _)) } @ unique
    val get :
      (r : href) ->
      (k : int) ->
      (p : hprop) ->
      htoken{ sat (Star (Pts (r, k), p)) (hp _) } @ unique ->
      int{ _ = k } * htoken{ sat (Star (Pts (r, k), p)) (hp _) } @ unique
  end
|}]

open S

(* Reading a chunk the token does not own: no supporting fact. *)
let steal :
  (r1 : href) -> (r2 : href) -> (a : int) ->
  htoken{ sat (Star (Pts (r1, a), Emp)) (hp _) } @ unique -> int =
  fun r1 r2 a t ->
  ignore r1;
  let e = Emp in
  let pr = get r2 a e t in
  let (x, t2) = pr in
  ignore t2;
  x
[%%expect{|
Line 10, characters 22-23:
10 |   let pr = get r2 a e t in
                           ^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: sat (Star (Pts (r2, a), e)) (hp t)
Hypotheses:
  e = Emp
  sat (Star (Pts (r1, a), Emp)) (hp t)
(lean: error: `grind` failed)
|}]

(* Claiming two starred chunks alias: they are provably distinct. *)
let alias :
  (r1 : href) -> (r2 : href) -> (a : int) -> (b : int) -> (p : hprop) ->
  htoken{ sat (Star (Pts (r1, a), Star (Pts (r2, b), p))) (hp _) } @ unique ->
  (bool{ _ = (r1 = r2) } * htoken) @ unique =
  fun r1 r2 a b p t ->
  ignore a; ignore b; ignore p;
  (true, t)
[%%expect{|
Line 7, characters 3-7:
7 |   (true, t)
       ^^^^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: true = (r1 = r2)
Hypotheses:
  sat (Star (Pts (r1, a), Star (Pts (r2, b), p))) (hp t)
(lean: error: `grind` failed)
|}]

(* Using the token twice: the mode checker is the ownership checker. *)
let dup :
  (r : href) -> (a : int) ->
  htoken{ sat (Star (Pts (r, a), Emp)) (hp _) } @ unique -> int =
  fun r a t ->
  let e = Emp in
  let pr = get r a e t in
  let (x, _) = pr in
  let pr2 = get r a e t in
  let (y, _) = pr2 in
  x + y
[%%expect{|
Line 8, characters 22-23:
8 |   let pr2 = get r a e t in
                          ^
Error: This value is used here, but it has already been used as unique at:
Line 6, characters 21-22:
6 |   let pr = get r a e t in
                         ^

|}]
