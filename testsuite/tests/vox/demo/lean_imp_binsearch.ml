(* TEST
 flags = "-vox-prelude binsearch_lib.lean";
 script = "sh ${test_source_directory}/../has-lean.sh";
 readonly_files = "binsearch_lib.lean";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* IMPERATIVE binary search over [int iarray]: the same flip-point
   specification as lean_binsearch.ml (recursive, ghost-parameter
   invariant), verified in while-loop style instead.

   The trick that makes the imperative version work: the loop invariant
   RELATES the two ends of the bracket (lo < hi, and the flip
   conditions), but a mutable variable's declared refinement cannot
   mention another mutable variable -- so the bracket lives in ONE
   mutable variable of (immutable-field) record type, whose declared
   refinement relates its own fields.  Every write re-proves the whole
   invariant; havoc at the loop head keeps it; the reflected loop
   condition [(b :> bracket).hi - (b :> bracket).lo > 1] -- the
   coercion erases the rigid refinement so the projection typechecks,
   and field projections reflect through the variable's SSA version --
   supplies the progress facts inside the body and its negation after
   the loop, which together with the invariant yields the flip-pair
   postcondition [hi = lo + 1].

   As in the recursive demo, the array reaches the logic as the
   uninterpreted [len]/[elem] of binsearch_lib.lean (no axioms);
   [length]/[get] are the assumed bridge, [half] is RUNTIME CHECKED,
   and every other obligation -- in-bounds accesses included -- is
   really proved.  Sortedness is never needed; termination is not
   checked. *)

type bracket =
  { lo : int
  ; hi : int
  }

let length : (a : int iarray) -> int{ _ = len a && 0 <= _ } =
  fun a -> assume_unchecked_ (Iarray.length a)

let get
  : (a : int iarray) -> (i : int) -> (g : unit{ 0 <= i && i < len a })
    -> int{ _ = elem a i }
  =
  fun a i g -> assume_unchecked_ (Iarray.get a i)

(* Floor halving: [asr] is outside the reflected fragment, so its
   specification is assumed -- and RUNTIME CHECKED. *)
let half : (s : int) -> int{ s = 2 * _ || s = 2 * _ + 1 } =
  fun s -> assume_ (s asr 1)

let search
  : (a : int iarray) -> (x : int)
    -> bracket{ _.hi = _.lo + 1
                && -1 <= _.lo && _.hi <= len a
                && (_.lo = -1 || elem a _.lo < x)
                && (_.hi = len a || elem a _.hi >= x) }
  =
  fun a x ->
  let refine_ n = length a in
  let mutable b
    : bracket{ -1 <= _.lo && _.lo < _.hi && _.hi <= len a
               && (_.lo = -1 || elem a _.lo < x)
               && (_.hi = len a || elem a _.hi >= x) }
    = refine_ { lo = -1; hi = n }
  in
  while (b :> bracket).hi - (b :> bracket).lo > 1 do
    let refine_ br = b in
    let { lo = l; hi = h } = br in
    let refine_ s = (refine_ (h - l) : {v:int | v = h - l}) in
    let refine_ d = half s in
    let refine_ mid = (refine_ (l + d) : {v:int | v = l + d}) in
    let refine_ v = get a mid (refine_ ()) in
    if v >= x
    then b <- (refine_ { lo = l; hi = mid })
    else b <- (refine_ { lo = mid; hi = h })
  done;
  let refine_ res = b in
  refine_ res

(* The same search with TWO INDEPENDENT, UNREFINED mutable variables and
   ONE declared loop invariant.  Philosophy: refinements live at the
   edges (the signatures of [length], [get], [half], [search2]); inside
   the code everything is unpacked to plain values plus the logical
   environment, and the loop invariant is a FORMULA in that environment
   -- exactly the ghost [unit{...}] parameter of the recursive version,
   worn as a loop annotation.  The walker follows the classical
   quadruple: the formula is ASSERTED over the entry state, havoc,
   ASSUMED at the head, ASSERTED again at the back-edge, and after the
   loop the head assumption stands with the negated guard -- which
   yields the flip-pair postcondition fully statically.  Note the body:
   plain reads, plain writes, no coercions -- every obligation lives at
   the two invariant assertions and [get]'s edge precondition. *)

let search2
  : (a : int iarray) -> (x : int)
    -> bracket{ _.hi = _.lo + 1
                && -1 <= _.lo && _.hi <= len a
                && (_.lo = -1 || elem a _.lo < x)
                && (_.hi = len a || elem a _.hi >= x) }
  =
  fun a x ->
  let refine_ n = length a in
  let mutable lo = -1 in
  let mutable hi = n in
  (while hi - lo > 1 do
     let l = lo in
     let h = hi in
     let refine_ s = (refine_ (h - l) : {v:int | v = h - l}) in
     let refine_ d = half s in
     let refine_ mid = (refine_ (l + d) : {v:int | v = l + d}) in
     let refine_ v = get a mid (refine_ ()) in
     if v >= x then hi <- mid else lo <- mid
   done)
  [@vox.invariant
    -1 <= lo && lo < hi && hi <= len a
    && (lo = -1 || elem a lo < x)
    && (hi = len a || elem a hi >= x)];
  let l = lo in
  let h = hi in
  refine_ { lo = l; hi = h }
