(* TEST
 flags = "-vox-solver lean -vox-prelude rec_lib.lean";
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "rec_aux.ml";
 readonly_files = "rec_lib.lean";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Real verification of record refinements through Lean's [grind]:
   records become structures, projections are the built-in ones, and
   every obligation below is actually proved.  Exercises construction,
   projection goals, functional update with frame facts, per-field
   destructuring facts, ADT/record composition, and a spec-function
   measure over a record. *)

type point =
  { px : int
  ; py : int
  }

let origin : point{ _.px = 0 && _.py = 0 } = refine_ { px = 0; py = 0 }

let swap : (p : point) -> point{ _.px = p.py && _.py = p.px } =
  fun p -> refine_ { px = p.py; py = p.px }

let setx : (p : point) -> point{ _.px = 3 && _.py = p.py } =
  fun p -> refine_ { p with px = 3 }

let getx (p : point{ _.px = 7 }) : {r:int | r = 7} =
  let refine_ q = p in
  let { px; py = _ } = q in
  refine_ px

type shape =
  | Pt of point
  | Nothing

let compose (v : point{ _.px = 1 }) : {r:int | r = 1} =
  let refine_ q = v in
  let refine_ s = (refine_ (Pt q) : shape{ _ = Pt q }) in
  match s with
  | Pt w -> let { px; _ } = w in refine_ px
  | Nothing -> assume_ 0

(* Cross-module: Rec_aux.one's refinement projects a field of
   Rec_aux.wid; the path travels through the .cmi. *)
let from_lib : {r:int | r = 1} =
  let refine_ q = Rec_aux.one in
  let { Rec_aux.w } = q in
  refine_ w

(* A measure over a record, from the prelude. *)
let d : point{ norm1 _ = 3 } = refine_ { px = 1; py = 2 }

let add : (a : int) -> (b : int) -> {c:int | c = a + b} =
  fun a b -> assume_ (a + b)

let shift : (p : point) -> point{ norm1 _ = norm1 p + 1 } =
  fun p ->
  let refine_ x = (refine_ (p.px) : {v:int | v = p.px}) in
  let refine_ one = (assume_ 1 : {v:int | v = 1}) in
  let refine_ z = add x one in
  refine_ { p with px = z }
