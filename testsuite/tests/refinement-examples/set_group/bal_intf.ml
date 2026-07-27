(* A depth that a client can reason about, and the two bounds that pin it.

   [SET] exposes only membership-observable behaviour, so no law over
   membership can say anything about the shape of a value: two
   implementations with the same membership semantics are
   indistinguishable through it.  Balance is a shape property, so forcing
   it needs a new observation.  [depth] is that observation.

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

module type BALANCED_SET = sig
  include Set_intf.SET

  (* The number of keys held.  Anchored to membership by the two laws
     below, so an implementation has no freedom in what it means. *)
  val size : t @ local logical -> Bigint.t @@ total

  (* The new observation.  Anchored by the two bounds below. *)
  val depth : t @ local logical -> nat @@ total

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

  (* Depth is not understated: a tree of depth h holds fewer than
     2^(h+1) keys.  True of any binary tree; it is what stops an
     implementation claiming a depth of zero. *)
  val size_depth_bound :
    tree:t @ logical ->
    well_formed:unit{ invariant tree = true } ->
    unit{ Bigint.lt (size tree) (pow2 (S (depth tree))) = true } @@ total

  (* Depth is not overstated: a tree of depth h holds at least [fib h]
     keys.  False for an unbalanced tree, which is what makes the
     balance component of [invariant] load-bearing. *)
  val depth_size_bound :
    tree:t @ logical ->
    well_formed:unit{ invariant tree = true } ->
    unit{ Bigint.le (fib (depth tree)) (size tree) = true } @@ total
end
