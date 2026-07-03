(* TEST
 flags = "-vox-prelude binsearch_lib.lean";
 script = "sh ${test_source_directory}/../has-lean.sh";
 readonly_files = "binsearch_lib.lean";
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
   here a THEOREM: [get] demands 0 <= i < len a.

   The array reaches the logic as two uninterpreted spec functions,
   [len] and [elem] (binsearch_lib.lean, no axioms); [length] and
   [get] below are the only assumed bridge to the real [Iarray] --
   unchecked by necessity, since a runtime check cannot evaluate an
   uninterpreted function.  Everything else is really proved: the
   midpoint arithmetic (via the RUNTIME CHECKED flooring of [asr]),
   the in-bounds obligations, the invariant, and the flip-point
   postcondition r = l + 1.  Sortedness is never needed: on an
   unsorted array the result is still a point where p flips.

   Preconditions and the loop invariant ride on the PARAMETERS
   (contracts): the body assumes them, each call site discharges them
   at its own bare arguments -- the sentinel [-1] included, a literal
   naming itself. *)

let length : (a : int iarray) -> int{ _ = len a && 0 <= _ } =
  fun a -> assume_unchecked_ (Iarray.length a)

let get : (a : int iarray) -> (i : int{ 0 <= _ && _ < len a })
          -> int{ _ = elem a i } =
  fun a i -> assume_unchecked_ (Iarray.get a i)

(* Floor halving: [asr] is outside the reflected fragment, so its
   specification is assumed -- and RUNTIME CHECKED. *)
let half : (s : int) -> int{ s = 2 * _ || s = 2 * _ + 1 } =
  fun s -> assume_ (s asr 1)

type ans =
  { lo : int
  ; hi : int
  }

(* The search: p(i) is elem a i >= x, extended by p(-1) = false and
   p(len a) = true.  The parameter contracts carry the invariant; the
   result is the adjacent flip pair, inside the input bracket -- each
   recursive call is a bare [search a x l m] / [search a x m r], its
   halved invariant discharged from the path facts.  The result is
   still unpacked and re-proved at the enclosing instantiation (the
   recursive call's postcondition speaks of the SMALLER bracket).
   Termination is not checked (partial correctness), but r - l shrinks
   each call. *)
let rec search
  : (a : int iarray) -> (x : int) -> (l : int{ -1 <= _ })
    -> (r : int{ l < _ && _ <= len a
                 && (l = -1 || elem a l < x)
                 && (_ = len a || elem a _ >= x) })
    -> ans{ _.hi = _.lo + 1
            && l <= _.lo && _.hi <= r
            && (_.lo = -1 || elem a _.lo < x)
            && (_.hi = len a || elem a _.hi >= x) }
  =
  fun a x l r ->
    if r - l > 1
    then begin
      (* The classic midpoint overflow sits exactly on DESIGN.md's
         caveat: the logic's ints are unbounded, so l + r is proved
         ideal, not wrapping.  Harmless here -- both are bounded by a
         real array's length -- but outside the model. *)
      let refine_ s = refine_ (l + r) in
      let refine_ m = half s in
      (* l < m < r, hence 0 <= m < len a: the probe is in bounds. *)
      let refine_ v = get a m in
      if v >= x
      then begin
        let refine_ q = search a x l m in
        refine_ q
      end
      else begin
        let refine_ q = search a x m r in
        refine_ q
      end
    end
    else refine_ { lo = l; hi = r }

(* The note's Q2 -- on a sorted array, the first index whose element
   is >= x, or len a if there is none.  What is proved is the
   flip-point characterization below, which holds sorted or not. *)
let lower_bound
  : (a : int iarray) -> (x : int)
    -> int{ 0 <= _ && _ <= len a
            && (_ = len a || elem a _ >= x)
            && (_ = 0 || elem a (_ - 1) < x) }
  =
  fun a x ->
    let refine_ n = length a in
    let refine_ q = search a x (-1) n in
    let { lo = _; hi } = q in
    refine_ hi

(* Client side: the note's example array.  The application carries no
   obligations; its refined result is unpacked before export. *)
let a8 : int iarray = [: 2; 3; 3; 3; 6; 8; 8; 9 :]
let six : int = 6

let idx6 : int =
  let refine_ i = lower_bound a8 six in
  i
