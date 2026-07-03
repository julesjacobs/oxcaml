(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "sep_lib.mli sep_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A separation-logic universe (sep_lib), really proved: ONE unique
   token owns the heap, its refinement carries a reified assertion
   ([hprop]), and the frame is an explicit parameter.  Rearrangements
   go through the exported rotation/commutation lemmas; distinctness
   of two live references falls out of [Star]'s disjointness.  Frame
   arguments are let-bound (a dependent argument must be nameable);
   every obligation below is really proved through grind. *)

open Sep_lib

(* Swap two references' contents THROUGH the assertion: reads recover
   the ghost parameters, writes rewrite the points-to chunks, and the
   result assertion is reached by rotation and congruence. *)
let swap :
  (r1 : href) -> (r2 : href) -> (a : int) -> (b : int) -> (p : hprop) ->
  htoken{ sat (Star (Pts (r1, a), Star (Pts (r2, b), p))) (hp _) } @ unique ->
  htoken{ sat (Star (Pts (r1, b), Star (Pts (r2, a), p))) (hp _) } @ unique =
  fun r1 r2 a b p t ->
  let f1 = Star (Pts (r2, b), p) in
  let (x, t) = get r1 a f1 t in
  let f2 = Star (Pts (r1, a), p) in
  let (y, t) = get r2 b f2 t in
  let t = set r1 a y f1 t in
  let f3 = Star (Pts (r1, y), p) in
  let t = set r2 b x f3 t in
  ignore x; ignore y;
  t

(* Two live chunks are distinct references: [Star] means disjointness,
   and the lemma surfaces it as an ordinary fact about the values. *)
let distinct :
  (r1 : href) -> (r2 : href) -> (a : int) -> (b : int) -> (p : hprop) ->
  htoken{ sat (Star (Pts (r1, a), Star (Pts (r2, b), p))) (hp _) } @ unique ->
  (bool{ _ = not (r1 = r2) }
   * htoken{ sat (Star (Pts (r1, a), Star (Pts (r2, b), p))) (hp _) }) @ unique =
  fun r1 r2 a b p t ->
  ignore a; ignore b; ignore p;
  (true, t)

(* Drive it: allocate two references under one token, swap them, and
   prove the final contents from the assertion alone. *)
let demo () : int{ _ = 23 } =
  let e = Emp in
  let t = init () in
  let pr1 = alloc e 2 t in
  let (r1, t) = pr1 in
  let f1 = Star (Pts (r1, 2), e) in
  let pr2 = alloc f1 3 t in
  let (r2, t) = pr2 in
  let t = swap r2 r1 3 2 e t in
  let g1 = Star (Pts (r1, 3), e) in
  let (x, t) = get r2 2 g1 t in
  let g2 = Star (Pts (r2, 2), e) in
  let (y, t) = get r1 3 g2 t in
  ignore t;
  x * 10 + y
