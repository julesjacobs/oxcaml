(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Native tuples in refinements, really proved through Lean.  Tuples
   are STRUCTURAL: each arity is modelled by one polymorphic product
   structure (VoxT2, VoxT3, ...), so construction [(a, b)] and the
   pair projections [fst]/[snd] appear in predicates with no
   instantiation info and the predicate language stays untyped.
   Matching a variable against a tuple pattern contributes [xi =
   proj_i s] per variable component, exactly like simple records.
   NOTE the binder spelling [(p : (int * int))]: the inner parentheses
   are required -- [(p : int * int)] is the LABELED TUPLE type
   [p:int * int], not a binder (the LR(1) ambiguity in DESIGN.md).
   No intro or elim forms anywhere: obligations arise implicitly at
   the annotations and applications. *)

(* Construction and projection, round trip. *)
let swap (p : (int * int)) : (int * int){ _ = (snd p, fst p) } =
  match p with
  | (x, y) -> (y, x)

let mkpair (a : int) (b : int) : (int * int){ _ = (a, b) } = (a, b)

(* A refinement speaking about the bound pair's components. *)
let first_pos (p : (int * int){ fst _ > 0 }) : {r:int | r > 0} =
  match p with
  | (x, _) -> x

(* Destructuring let gets the same per-component facts a match would. *)
let sum_swap (p : (int * int)) : int{ _ = fst p + snd p } =
  let (x, y) = p in
  y + x

(* Triples: construction in predicates; projections beyond pairs have
   no surface syntax and arise from match facts only. *)
let rot3 (a : int) (b : int) (c : int) : (int * int * int){ _ = (b, c, a) } =
  (b, c, a)

let third (t : (int * int * int){ _ = (1, 2, 3) }) : {r:int | r = 3} =
  match t with
  | (_, _, z) -> z

(* A bool component: sorted [Prop] on the Lean side; the product
   structure is Sort-polymorphic (the shape of PProd), so the
   instantiation is legal. *)
let tag (n : int) : (int * bool){ _ = (n, n > 0) } = (n, n > 0)

(* Nested pairs. *)
let nest (a : int) (b : int) : ((int * int) * int){ _ = ((a, b), a + b) } =
  ((a, b), a + b)

(* A pair as a simple variant's payload: the datatype's field is
   tuple-sorted, and the facts compose through both layers. *)
type w = W of (int * int)

let unw (v : w{ _ = W (3, 4) }) : {r:int | r = 3} =
  match v with
  | W p -> (match p with (x, _) -> x)

(* The polymorphic EQUALITY translates at tuples of int/bool (nested
   included): the path fact [p = q] is the product equality, so
   component equalities follow by congruence -- the else-arm below is
   proved dead.  (Order comparisons do not translate: OCaml's tuple
   order is lexicographic; the logic has none at product sorts.) *)
let eq_components (p : (int * int)) (q : (int * int)) : int{ _ = 1 } =
  if p = q
  then (if fst p = fst q then 1 else 0)
  else 1

(* Reflexivity at a nested tuple with a bool (Prop) component. *)
let refl_eq (x : ((int * bool) * int)) : bool{ _ } = x = x
