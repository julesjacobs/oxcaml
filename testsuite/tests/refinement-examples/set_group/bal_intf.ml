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

(* What "forces" means below, and what it does not.  Every layer in this
   file claimed more than this once, and each claim was withdrawn only
   after someone built the counterexample.

   These layers are measured by mutation: delete a conjunct from an
   implementation's invariant, leave the rest of that implementation
   exactly as it was, and see whether the module still seals.  A refusal
   means something real --- with the rest held fixed, the conjunct is
   load-bearing, and in each case here the exported law is disproved at a
   ground value the weakened invariant admits, so no amount of proof
   engineering recovers it.  It does not mean the interface admits only
   implementations with that property.  Those two readings were run
   together in the first version of every comment here.

   The gap is not hypothetical for any of the four layers, and all four
   escapes are built and accepted:

   - [size] is escaped by counting distinct keys instead of cells;
   - [depth] is escaped by exposing the Fibonacci rank of the
     cardinality --- a linked list ascribes [BALANCED_SET] that way, with
     its honest deduplicating insert and its [unique] intact;
   - [least] is escaped by returning the fallback always, which satisfies
     the second disjunct of its law for every value;
   - [remove] is escaped by deleting every occurrence rather than the
     first, which makes its law hold whether or not the value is unique.

   Every one of those changes the invariant AND the operation the law is
   about, together.  So the accurate general statement is: each law
   refuses a change to the invariant alone, and none of them refuses a
   coordinated change to the invariant and the operation.  Where a
   comment below says a component is forced, read it with that
   qualification. *)

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
   a justification and enlarging it with [least] does not.

   Subject to the note above: the law does not pin [least] itself.
   [least tree fallback = fallback] for every value satisfies the second
   disjunct, so an implementation that weakens its invariant and gives up
   on returning a least element together carries this layer.  What is
   refused is dropping [ordered] while keeping the honest [least]. *)
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

(* The cardinality layer, and the cheapest thing in this file: it adds a
   law and no operation at all.  [equal] and [size] are both already in
   [COUNTED_SET], and saying that extensionally equal well-formed values
   have equal size is enough to make the unique list's [unique]
   load-bearing --- without [least], without [remove], without any new
   thing for a client to know about.  Where a new operation was thought
   to be unavoidable, one law over the existing ones did the job.

   The disproof is a two-element instance: [equal (Cons (3, Cons (3,
   Nil))) (Cons (3, Nil))] is [true], because [equal] is extensional and
   both hold exactly the key 3, while their sizes are 2 and 1.  The same
   instance rejects the search tree's trivial invariant, since
   [Node (Node (Empty, 5, Empty), 3, Empty)] and [Node (Empty, 3, Empty)]
   are equal and differently sized.

   Only the unique list carries this so far.  The tree side is
   half-answered: the rejection is measured, but the honest proof needs a
   tree [remove] and the theorem that cardinality is determined by the
   member set on ordered trees, which is a substantially bigger piece of
   work than the list's and has not been done.

   Adding no operation does not make this escape-proof, it only leaves
   one thing to redefine instead of two.  A [size] that counts distinct
   keys is a function of the member set by construction, so it satisfies
   this law whatever the invariant says.  Unlike the [size_insert]
   escape, which is nine lines and a shorter proof, that one has not been
   built here: it would still have to prove that the distinct count is
   determined by the member set, which is most of the work this law's
   honest proof does.  Cheap in principle, unmeasured in practice. *)
module type CARDINAL_SET = sig
  include COUNTED_SET

  val equal_size :
    t1:t @ logical ->
    t2:t @ logical ->
    well_formed_1:unit{ invariant t1 = true } ->
    well_formed_2:unit{ invariant t2 = true } ->
    equal_trees:unit{ equal t1 t2 = true } ->
    unit{ size t1 = size t2 } @@ total
end

(* The deletion layer: the set operation this family was otherwise
   missing.  It forces a uniqueness invariant the way [least] forces an
   ordering one, and for the same reason.  [remove] deletes the first
   occurrence of a key, so on a value holding two copies the second
   survives and [remove_law] is false; the law is stated for every
   query, so a client sees the failure without knowing how the value is
   laid out.  Like [least] it exposes no shape.

   Subject to the note above: the law does not pin [remove] itself.  A
   [remove] that deletes every occurrence of the key satisfies it for
   every value, unique or not.  What is refused is dropping [unique]
   while keeping the first-occurrence [remove].

   This includes [CARDINAL_SET] because the unique list carries both, not
   because the two are related: they are independent, and either one on
   its own makes [unique] load-bearing. *)
module type REMOVING_SET = sig
  include CARDINAL_SET

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
   [fib h <= size < 2^(h+1)] passes, so this layer refuses a defective
   implementation only while [depth] is the honest structural one.

   That is not a theoretical reservation.  A LINKED LIST carries this
   signature: the unique list, unchanged, with its deduplicating insert
   and its [unique] intact, exposing as [depth] the Fibonacci rank of its
   own cardinality --- [Z] at the empty list, and one more than the rank
   of the tail exactly when the next Fibonacci threshold is reached.  A
   thousand-element chain then reports depth 16 and both bounds hold, by
   one induction whose only lemma is [fib n <= pow2 n].  So the earlier
   claim here, that only a genuinely height-balanced implementation could
   carry this layer, was false, and the refutation is the most unbalanced
   structure there is rather than a contrived one.

   The rank recursion works because a list is a chain: one constructor
   per cardinality step.  A tree node sums two subtree cardinalities, so
   the same trick would need a binary counter or a flattening detour.
   Whether an ordered-but-unbalanced tree can carry this layer is
   therefore open --- nobody has built it, and nothing here should be
   read as saying it cannot be built.

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
