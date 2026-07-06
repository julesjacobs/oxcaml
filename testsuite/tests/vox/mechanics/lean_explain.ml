(* TEST
 flags = "-vox-explain-proofs -vox-dump-vc-provenance";
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* vox: [-vox-explain-proofs] reports, for each PROVED obligation, which
   lemmas [grind] used to close it -- emitted (under
   [-vox-dump-vc-provenance]) as a "used:" line per VC.  Internally each
   [Prove] VC is discharged with [grind?] instead of [grind]; the two
   have identical proving power, so verdicts are unchanged.  A VC closed
   by an [@@vox.lemma] names it (the solver-side name is the OCaml
   identifier); one closed by arithmetic/logic alone shows
   "<arithmetic>". *)

type ilist =
  | Nil
  | Cons of int * ilist

let rec total_ len (l : ilist) : int =
  match l with
  | Nil -> 0
  | Cons (_, t) -> 1 + len t

(* A structural lemma, exported as an ambient grind fact. *)
let rec lemma_len_nonneg (l : ilist) : unit{ len l >= 0 } =
  match l with
  | Nil -> ()
  | Cons (_, t) -> lemma_len_nonneg t
[@@vox.lemma]

(* Discharged by the ambient lemma: "used: lemma_len_nonneg". *)
let use_nonneg (l : ilist) : int{ _ >= 0 } = refine_ (len l)

(* Pure arithmetic, no user fact: "used: <arithmetic>". *)
let arith (x : int{ _ > 0 }) : int{ _ >= 0 } = refine_ (x + 1)
