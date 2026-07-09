(* TEST
 flags = "-vox-explain-proofs -vox-dump-vc-provenance";
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* vox: when a solve FAILS, the compiler still attributes a per-VC
   VERDICT before raising (under [-vox-dump-vc-provenance]): a "verdict:"
   line marks each obligation "proved", "disproved" (a counterexample was
   validated) or "unproved".  Only the theorem lines that carried a Lean
   error are failures -- the rest genuinely proved -- so an editor can keep
   badging the obligations that still hold when a sibling fails.  Here two
   goals are false ([bad1], [bad2]) and two hold ([ok1], [ok2]): the dump
   marks the first failing VC "disproved" (a ground false goal, so its
   error needs no assignment) and the second "unproved" (the classifier
   runs on the first failure only), and the other two "proved".  The
   failing goals are wrapped in functions so their false postconditions do
   not leak as module hypotheses into the later obligations. *)

let ok1 (x : int{ _ > 0 }) : int{ _ >= 0 } = refine_ (x + 1)

let bad1 (u : unit) : int{ _ = 5 } = refine_ 0

let ok2 (y : int{ _ >= 3 }) : int{ _ >= 2 } = refine_ (y - 1)

let bad2 (u : unit) : int{ _ = 7 } = refine_ 0
