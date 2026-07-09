(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* SOUNDNESS: an ordinary recursive function with a FALSE postcondition
   verifies its own BODY under partial correctness (the recursive call
   assumes the false contract as its induction hypothesis, and the call
   diverges so the fact only reaches dead code).  Exporting it as an
   ambient [@@vox.lemma] would register the false universal
   [forall l, 1 = 2] -- so the export RE-PROVES it in Lean, where the
   induction has no diverging escape hatch: [grind] cannot close the
   base case, and verification fails CLOSED. *)

type ilist =
  | Nil
  | Cons of int * ilist

let rec bad (l : ilist) : unit{ 1 = 2 } =
  match l with
  | Nil -> bad l
  | Cons (_, t) -> bad t
[@@vox.lemma]
