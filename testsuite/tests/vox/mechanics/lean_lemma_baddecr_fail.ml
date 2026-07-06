(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* SOUNDNESS (v2): an int-indexed lemma whose [@@vox.decreases] metric
   does NOT decrease.  v2 emits [termination_by (0).toNat], whose
   [decreasing_by] goal is unprovable, so Lean rejects the recursive
   proof: a bogus measure cannot smuggle in a non-terminating
   "induction". *)

let rec total_ dbl (n : int) : int =
  if n <= 0 then 0 else 2 + dbl (n - 1)
[@@vox.decreases n]

let rec lemma_dbl (n : int{ 0 <= _ }) : unit{ dbl n = 2 * n } =
  if n <= 0 then () else lemma_dbl (n - 1)
[@@vox.decreases 0]
[@@vox.lemma]
