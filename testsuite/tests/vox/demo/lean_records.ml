(* TEST
 flags = "-vox-prelude rec_lib.lean";
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "rec_aux.ml";
 readonly_files = "rec_lib.lean";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Demo: simple records.  Real verification of record
   refinements through Lean's [grind]: records become structures,
   projections are the built-in ones, and every obligation below is
   actually proved.  Exercises construction, projection goals,
   functional update with frame facts, per-field destructuring facts,
   ADT/record composition, and a spec-function measure over a record
   (rec_lib.lean).  No intro or elim forms: record literals are
   introduced implicitly at the annotations, and destructuring a
   carrier-bound name yields its per-field facts directly. *)

type point =
  { px : int
  ; py : int
  }

let origin : point{ _.px = 0 && _.py = 0 } = { px = 0; py = 0 }

let swap : (p : point) -> point{ _.px = p.py && _.py = p.px } =
  fun p -> { px = p.py; py = p.px }

let setx : (p : point) -> point{ _.px = 3 && _.py = p.py } =
  fun p -> { p with px = 3 }

let getx (p : point{ _.px = 7 }) : {r:int | r = 7} =
  let { px; py = _ } = p in
  px

type shape =
  | Pt of point
  | Nothing

(* The binder equation [s = Pt v] plus injectivity prove [w = v]. *)
let compose (v : point{ _.px = 1 }) : {r:int | r = 1} =
  let s = Pt v in
  match s with
  | Pt w -> let { px; _ } = w in px
  (* Dead arm, proved dead: s = Pt v and s = Nothing contradict. *)
  | Nothing -> 0

(* Cross-module: Rec_aux.one's refinement projects a field of
   Rec_aux.wid; the path travels through the .cmi, and the import is
   destructured DIRECTLY -- its name receives the per-field facts. *)
let from_lib : {r:int | r = 1} =
  let { Rec_aux.w } = Rec_aux.one in
  w

(* A measure over a record, from the prelude. *)
let d : point{ norm1 _ = 3 } = { px = 1; py = 2 }

(* Field reads reflect like arithmetic does: the update value names
   [p.px + 1], and the kept field projects out of the base. *)
let shift : (p : point) -> point{ norm1 _ = norm1 p + 1 } =
  fun p -> { p with px = p.px + 1 }
