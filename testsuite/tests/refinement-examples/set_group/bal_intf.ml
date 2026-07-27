(* A depth that a client can reason about, and the two bounds that pin it.

   [SET]'s four operations are membership-observable, so no law written
   over exactly those four says anything about the shape of a value.
   Balance is a shape property and is not determined by the member set
   even on well-formed values, so forcing it needs a new observation
   rather than a new law.  [depth] is that observation, and it is the
   one addition here that has to be argued for: [LEAST_SET] and
   [REMOVING_SET] below also enlarge the algebra, but their operations
   are fixed by the member set on well-formed values and expose nothing
   a client could not already compute.

   Exporting [depth] alone moves the problem rather than solving it: an
   implementation would be free to define it however it liked, and a
   linked list claiming [depth = Z] satisfies any lower bound on size.
   The two bounds below pin it from both sides.  [size_depth_bound] says
   a value of depth h holds fewer than 2^(h+1) keys, so depth cannot be
   understated; [depth_size_bound] says it holds at least [fib h] of
   them, so depth cannot be overstated.  Together they place [depth]
   within a constant factor of the logarithm of the size, and [size] is
   itself anchored to membership by [size_empty] and [size_insert].

   Depths are unary because [@vox.def] requires structural recursion:
   an integer or [Bigint.t] recursion is refused, and a
   [@vox.decreases] measure buys totality without buying definitional
   equations, so a measured [fib] would be an uninterpreted symbol. *)

type nat =
  | Z
  | S of nat

let[@vox.def] rec nle (a : nat @ logical) (b : nat @ logical) : bool =
  match a with
  | Z -> true
  | S p -> (match b with Z -> false | S q -> nle p q)

let[@vox.def] rec nmax (a : nat @ logical) (b : nat @ logical) : nat =
  match a with
  | Z -> b
  | S p -> (match b with Z -> a | S q -> S (nmax p q))

(* The AVL minimum-size function: a height-balanced tree of height h
   holds at least [fib h] keys.  It separates balanced from unbalanced
   without needing logarithms or exponentiation on the balanced side. *)
let[@vox.def] rec fib (n : nat @ logical) : Bigint.t =
  match n with
  | Z -> Bigint.zero
  | S m ->
    (match m with
     | Z -> Bigint.one
     | S p -> Bigint.add (fib m) (fib p))

(* The opposite-direction anchor: a binary tree of height h holds fewer
   than 2^(h+1) keys, whether or not it is balanced. *)
let[@vox.def] rec pow2 (n : nat @ logical) : Bigint.t =
  match n with
  | Z -> Bigint.one
  | S m -> Bigint.add (pow2 m) (pow2 m)

(* ------------------------------------------------------------------ *)
(* Arithmetic on the two bound functions, shared by implementations.   *)
(* ------------------------------------------------------------------ *)

let rec fib_nonneg (n : nat @ logical)
    : unit{ Bigint.le Bigint.zero (fib n) = true } =
  match n with
  | Z -> fib_def Z; ()
  | S m ->
    fib_def (S m);
    (match m with
     | Z -> ()
     | S p -> fib_nonneg m; fib_nonneg p; ())

let rec fib_pos (n : nat @ logical)
    : unit{ Bigint.le Bigint.one (fib (S n)) = true } =
  match n with
  | Z -> fib_def (S Z); ()
  | S m -> fib_def (S (S m)); fib_pos m; fib_nonneg m; ()

let rec fib_mono (a : nat @ logical) (b : nat @ logical)
    (_le : unit{ nle a b = true })
    : unit{ Bigint.le (fib a) (fib b) = true } =
  match a with
  | Z -> fib_def Z; fib_nonneg b; ()
  | S p ->
    nle_def (S p) b;
    (match b with
     | Z -> ()
     | S q ->
       fib_def (S p);
       fib_def (S q);
       (match p with
        | Z -> (match q with Z -> () | S qq -> fib_pos qq; fib_nonneg qq; ())
        | S pp ->
          nle_def p q;
          (match q with
           | Z -> ()
           | S qq -> fib_mono p q (); fib_mono pp qq (); ())))

let rec pow2_pos (n : nat @ logical)
    : unit{ Bigint.le Bigint.one (pow2 n) = true } =
  match n with
  | Z -> pow2_def Z; ()
  | S m -> pow2_def (S m); pow2_pos m; ()

let rec pow2_mono (a : nat @ logical) (b : nat @ logical)
    (_le : unit{ nle a b = true })
    : unit{ Bigint.le (pow2 a) (pow2 b) = true } =
  match a with
  | Z ->
    pow2_def Z;
    pow2_pos b;
    ()
  | S p ->
    nle_def (S p) b;
    (match b with
     | Z -> ()
     | S q -> pow2_def (S p); pow2_def (S q); pow2_mono p q (); ())

let rec nle_total (a : nat @ logical) (b : nat @ logical)
    : unit{ (nle a b = true) || (nle b a = true) } =
  match a with
  | Z -> nle_def Z b; ()
  | S p ->
    (match b with
     | Z -> nle_def (S p) Z; nle_def Z (S p); ()
     | S q -> nle_def (S p) (S q); nle_def (S q) (S p); nle_total p q; ())

let rec nmax_right (a : nat @ logical) (b : nat @ logical)
    (_le : unit{ nle a b = true }) : unit{ nmax a b = b } =
  match a with
  | Z -> nmax_def Z b; ()
  | S p ->
    nle_def (S p) b;
    (match b with
     | Z -> ()
     | S q -> nmax_def (S p) (S q); nmax_right p q (); ())

let rec nmax_left (a : nat @ logical) (b : nat @ logical)
    (_le : unit{ nle b a = true }) : unit{ nmax a b = a } =
  match b with
  | Z -> nmax_def a Z; (match a with Z -> () | S _ -> ())
  | S q ->
    nle_def (S q) a;
    (match a with
     | Z -> ()
     | S p -> nmax_def (S p) (S q); nmax_left p q (); ())

let rec nle_refl (a : nat @ logical) : unit{ nle a a = true } =
  match a with
  | Z -> nle_def Z Z; ()
  | S p -> nle_def (S p) (S p); nle_refl p; ()

let rec nle_nmax_left (a : nat @ logical) (b : nat @ logical)
    : unit{ nle a (nmax a b) = true } =
  match a with
  | Z -> nmax_def Z b; nle_def Z (nmax Z b); ()
  | S p ->
    nmax_def (S p) b;
    (match b with
     | Z -> nle_refl (S p); ()
     | S q -> nle_def (S p) (S (nmax p q)); nle_nmax_left p q; ())

let rec nle_nmax_right (a : nat @ logical) (b : nat @ logical)
    : unit{ nle b (nmax a b) = true } =
  match a with
  | Z -> nmax_def Z b; nle_refl b; ()
  | S p ->
    nmax_def (S p) b;
    (match b with
     | Z -> nle_def Z (S p); ()
     | S q -> nle_def (S q) (S (nmax p q)); nle_nmax_right p q; ())

(* ------------------------------------------------------------------ *)
(* Bridge to [Bigint.t].  An implementation that already states its      *)
(* balance condition in [Bigint] arithmetic keeps it and converts.       *)
(* ------------------------------------------------------------------ *)

let[@vox.def] rec nat_to_big (n : nat @ logical) : Bigint.t =
  match n with
  | Z -> Bigint.zero
  | S m -> Bigint.add Bigint.one (nat_to_big m)

let rec nat_to_big_nonneg (n : nat @ logical)
    : unit{ Bigint.le Bigint.zero (nat_to_big n) = true } =
  match n with
  | Z -> nat_to_big_def Z; ()
  | S m -> nat_to_big_def (S m); nat_to_big_nonneg m; ()

let rec nle_iff (a : nat @ logical) (b : nat @ logical)
    : unit{ nle a b = Bigint.le (nat_to_big a) (nat_to_big b) } =
  match a with
  | Z ->
    nle_def Z b;
    nat_to_big_def Z;
    nat_to_big_nonneg b;
    ()
  | S p ->
    nle_def (S p) b;
    nat_to_big_def (S p);
    nat_to_big_nonneg p;
    (match b with
     | Z -> nat_to_big_def Z; ()
     | S q -> nat_to_big_def (S q); nat_to_big_nonneg q; nle_iff p q; ())

(* ------------------------------------------------------------------ *)

(* The counting layer.  [size_empty] and [size_insert] pin [size] on
   every value a client can build from [empty] by [insert], and the
   increment law is false for any insert that adds a node for a key it
   already holds *while [size] counts nodes*.

   That qualification is the whole of what this layer forces, and it is
   worth being exact about, because the law constrains the pair
   ([insert], [size]) rather than [insert] on its own.  An
   implementation that conses unconditionally still carries the layer if
   it defines [size] as the number of distinct keys it holds: that is a
   structural recursion over the same list, and [size_insert] then
   follows by one unfolding rather than by induction.  Measured against
   the node-counting version it is nine changed lines, and the resulting
   proof is shorter, not longer.  So the layer refuses a change to
   [insert] alone and does not refuse a coordinated change to both.

   Every implementation in the family can carry this. *)
module type COUNTED_SET = sig
  include Set_intf.SET

  val size : t @ local logical -> Bigint.t @@ total

  val size_empty : unit{ size empty = Bigint.zero } @@ total

  val size_insert :
    inserted:int ->
    tree:t @ logical ->
    well_formed:unit{ invariant tree = true } ->
    unit{
      size (insert inserted tree)
      = (if member inserted tree
         then size tree
         else Bigint.add (size tree) Bigint.one)
    } @@ total
end

(* The least-element layer.  [least tree fallback] is the least key the
   value holds, or [fallback] when it holds none; a standard set library
   exports it as [min_elt].  It exposes no shape --- on a well-formed
   value it is the minimum of the member set --- and it nevertheless
   forces an ordering invariant that the four [SET] operations cannot
   reach.

   The reason is the general rule, and it is worth stating here rather
   than at the one implementation that uses it.  An operation separates
   ill-formed values from well-formed ones when its own recursion reads
   the structure the invariant constrains, even when the value it is
   *specified* to return is a function of the member set alone.  [least]
   descends the left spine, so on an unordered value it can return a key
   that the one-spine [member] never finds, and [least_law] is then
   false rather than merely unproved.  Being membership-determined on
   well-formed values is not the same as being membership-determined,
   and it is the second that would leave an operation unable to
   separate.  [depth] below is not membership-determined even on
   well-formed values, which is why enlarging the algebra with it needs
   a justification and enlarging it with [least] does not. *)
module type LEAST_SET = sig
  include COUNTED_SET

  val least : t @ local logical -> int -> int @@ total

  (* The least element of a value is a member of it, unless the value
     holds nothing, in which case [least] returns the fallback. *)
  val least_law :
    tree:t @ logical ->
    fallback:int ->
    well_formed:unit{ invariant tree = true } ->
    unit{
      member (least tree fallback) tree = true
      || least tree fallback = fallback
    } @@ total
end

(* The deletion layer: the set operation this family was otherwise
   missing.  It forces a uniqueness invariant the way [least] forces an
   ordering one, and for the same reason.  [remove] deletes the first
   occurrence of a key, so on a value holding two copies the second
   survives and [remove_law] is false; the law is stated for every
   query, so a client sees the failure without knowing how the value is
   laid out.  Like [least] it exposes no shape. *)
module type REMOVING_SET = sig
  include COUNTED_SET

  val remove : int -> t @ logical -> t @@ total

  val remove_law :
    removed:int ->
    tree:t @ logical ->
    query:int ->
    well_formed:unit{ invariant tree = true } ->
    unit{
      member query (remove removed tree)
      = ((query <> removed) && member query tree)
    } @@ total
end

(* The balance layer, on top of the counting one.  The two bounds below
   place the exported [depth] within a constant factor of the logarithm
   of [size].  They do not place it at the value's real path depth:
   nothing here relates [depth] to constructors.  Any h satisfying
   [fib h <= size < 2^(h+1)] passes --- zero at cardinality zero and the
   bit length of the cardinality otherwise is one such choice, available
   whatever the real shape is --- so this layer refuses a defective
   implementation only while [depth] is the honest structural one.

   What it forces there is real rather than a proof artefact: with the
   structural [depth] in place, dropping the AVL balance conjunct leaves
   an ordered six-node right spine satisfying [invariant], and that value
   disproves the [fib] bound outright. *)
module type BALANCED_SET = sig
  include COUNTED_SET

  (* The new observation.  Anchored by the two bounds below. *)
  val depth : t @ local logical -> nat @@ total

  (* Depth is not understated: a tree of depth h holds fewer than
     2^(h+1) keys.  True of any binary tree; it is what stops an
     implementation claiming a depth of zero. *)
  val size_depth_bound :
    tree:t @ logical ->
    well_formed:unit{ invariant tree = true } ->
    unit{ Bigint.lt (size tree) (pow2 (S (depth tree))) = true } @@ total

  (* Depth is not overstated: a tree of depth h holds at least [fib h]
     keys.  False for an unbalanced tree under a structural [depth],
     which is what makes the balance component of [invariant]
     load-bearing there.  This is the AVL bound specifically; a
     red-black tree satisfies only the weaker 2^(h/2) bound and would
     need a different bound function here. *)
  val depth_size_bound :
    tree:t @ logical ->
    well_formed:unit{ invariant tree = true } ->
    unit{ Bigint.le (fib (depth tree)) (size tree) = true } @@ total
end
