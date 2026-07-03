(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/ia_lib.mli ../lib/ia_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Demo: binary search over [int iarray], after "Binary Search a
   Little Simpler & More Generic" (julesjacobs.com): keep l < r with
   p(l) false and p(r) true, halve until r = l + 1.  Starting from the
   virtual sentinels l = -1 and r = length a (where p(-1) = false and
   p (length a) = true artificially), the loop probes only indices
   STRICTLY between them -- so every access is in bounds, which is
   here a THEOREM: [Ia_lib.get] demands 0 <= i < Iarray.length a.  The
   invariant states p's endpoint values as guarded implications
   ([0 <= l -> a.(l) < x]): at the sentinels the guard is false and
   the fact vacuous, which is exactly the artificial extension.

   The array is the BUILT-IN theory: [Iarray.length a] and [a.(i)]
   reflect in expressions and appear in predicates directly, with
   length nonnegativity the theory's one (compiler-owned) axiom.
   NOTHING in this file is assumed: the midpoint arithmetic (reflected
   T-division), the in-bounds obligations, the invariant, and the
   flip-point postcondition r = l + 1 are all proved.  Sortedness is
   never needed: on an unsorted array the result is still a point
   where p flips.

   Preconditions and the loop invariant ride on the PARAMETERS
   (contracts): the body assumes them, each call site discharges them
   at its own bare arguments -- the sentinel [-1] included, a literal
   naming itself. *)

(* The search: p(i) is a.(i) >= x, extended by p(-1) = false and
   p(length a) = true.  The parameter contracts carry the invariant;
   the result is the adjacent flip PAIR (a native tuple), inside the
   input bracket -- each recursive call is a bare [search a x l m] /
   [search a x m r], its halved invariant discharged from the path
   facts.  Each call is also the bare TAIL: its postcondition speaks
   of the SMALLER bracket, and the re-proof at the enclosing
   instantiation happens inline, from the call's own selfified result.
   Termination is not checked (partial correctness), but r - l shrinks
   each call. *)
let rec search
  : (a : int iarray) -> (x : int) -> (l : int{ -1 <= _ })
    -> (r : int{ l < _ && _ <= Iarray.length a
                 && (0 <= l -> a.(l) < x)
                 && (_ < Iarray.length a -> a.(_) >= x) })
    -> (int * int){ snd _ = fst _ + 1
            && l <= fst _ && snd _ <= r
            && (0 <= fst _ -> a.(fst _) < x)
            && (snd _ < Iarray.length a -> a.(snd _) >= x) }
  =
  fun a x l r ->
    if r - l > 1
    then begin
      (* The classic midpoint overflow sits exactly on DESIGN.md's
         caveat: the logic's ints are unbounded, so l + r is proved
         ideal, not wrapping.  Harmless here -- both are bounded by a
         real array's length -- but outside the model. *)
      let m = (l + r) / 2 in
      (* l < m < r, hence 0 <= m < Iarray.length a: the probe is in
         bounds. *)
      let v = Ia_lib.get a m in
      if v >= x
      then search a x l m
      else search a x m r
    end
    else (l, r)

(* The note's Q2 -- on a sorted array, the first index whose element
   is >= x, or length a if there is none.  What is proved is the
   flip-point characterization below, which holds sorted or not.
   [Iarray.length a] selfifies, and the initial bracket's -1 < n is
   the theory's nonnegativity axiom firing. *)
let lower_bound
  : (a : int iarray) -> (x : int)
    -> int{ 0 <= _ && _ <= Iarray.length a
            && (_ < Iarray.length a -> a.(_) >= x)
            && (0 < _ -> a.(_ - 1) < x) }
  =
  fun a x ->
    let n = Iarray.length a in
    let q = search a x (-1) n in
    let (_, hi) = q in
    hi

(* Client side: the note's example array.  The application carries no
   obligations; its refined result is unpacked by the plain [let]. *)
let a8 : int iarray = [: 2; 3; 3; 3; 6; 8; 8; 9 :]
let six : int = 6

let idx6 : int = lower_bound a8 six
