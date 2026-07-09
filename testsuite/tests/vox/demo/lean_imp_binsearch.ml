(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/ia_lib.mli ../lib/ia_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* IMPERATIVE binary search over [int iarray]: the same flip-point
   specification as lean_binsearch.ml (recursive, contract-parameter
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

   As in the recursive demo, the array is the BUILT-IN theory
   ([Iarray.length a], [a.(i)], length nonnegativity as the one
   compiler-owned axiom) and NOTHING in this file is assumed: the
   midpoint is reflected T-division, reads go through the PROVED
   bounds-contract [Ia_lib.get], and every obligation -- in-bounds accesses
   included -- is really proved.  Sortedness is never needed;
   termination is not checked. *)

type bracket =
  { lo : int
  ; hi : int
  }

let search
  : (a : int iarray) -> (x : int)
    -> bracket{ _.hi = _.lo + 1
                && -1 <= _.lo && _.hi <= Iarray.length a
                && (0 <= _.lo -> a.(_.lo) < x)
                && (_.hi < Iarray.length a -> a.(_.hi) >= x) }
  =
  fun a x ->
  let n = Iarray.length a in
  let mutable b
    : bracket{ -1 <= _.lo && _.lo < _.hi && _.hi <= Iarray.length a
               && (0 <= _.lo -> a.(_.lo) < x)
               && (_.hi < Iarray.length a -> a.(_.hi) >= x) }
    = { lo = -1; hi = n }
  in
  while (b :> bracket).hi - (b :> bracket).lo > 1 do
    let br = b in
    let { lo = l; hi = h } = br in
    let mid = (l + h) / 2 in
    let v = Ia_lib.get a mid in
    if v >= x
    then b <- { lo = l; hi = mid }
    else b <- { lo = mid; hi = h }
  done;
  b

(* The same search with TWO INDEPENDENT, UNREFINED mutable variables and
   ONE declared loop invariant.  Philosophy: refinements live at the
   edges (the signatures of [Ia_lib.get] and [search2]); inside the code
   everything is plain values plus the logical environment, and the
   loop invariant is a FORMULA in that environment -- exactly the
   contract of the recursive version, worn as a loop annotation.  The
   walker follows the classical quadruple: the formula is ASSERTED
   over the entry state, havoc, ASSUMED at the head, ASSERTED again at
   the back-edge, and after the loop the head assumption stands with
   the negated guard -- which yields the flip-pair postcondition fully
   statically.  Note the body: plain reads, plain writes, no
   coercions -- every obligation lives at the two invariant assertions
   and [Ia_lib.get]'s edge precondition.  (The invariant ATTRIBUTE's payload
   rides the expression grammar, hence the qualified [Iarray.get]
   spelling and sentinel disjunctions there.) *)

let search2
  : (a : int iarray) -> (x : int)
    -> bracket{ _.hi = _.lo + 1
                && -1 <= _.lo && _.hi <= Iarray.length a
                && (0 <= _.lo -> a.(_.lo) < x)
                && (_.hi < Iarray.length a -> a.(_.hi) >= x) }
  =
  fun a x ->
  let n = Iarray.length a in
  let mutable lo = -1 in
  let mutable hi = n in
  (while hi - lo > 1 do
     let l = lo in
     let h = hi in
     let mid = (l + h) / 2 in
     let v = Ia_lib.get a mid in
     if v >= x then hi <- mid else lo <- mid
   done)
  [@vox.invariant
    -1 <= lo && lo < hi && hi <= Iarray.length a
    && (lo = -1 || Iarray.get a lo < x)
    && (hi = Iarray.length a || Iarray.get a hi >= x)];
  let l = lo in
  let h = hi in
  { lo = l; hi = h }
