(* TEST
 flags = "-vox-solver lean";
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* An error inside an embedded prelude block is reported at the
   BLOCK's location (with the line within the block), not blamed on
   some verification condition.  (The obligation must apply a spec
   function: prelude text only reaches solver inputs that use one.) *)

[%%vox.prelude.lean {lean|
@[grind] def spec_id (n : Int) : Int := n
def broken : Int := unknown_identifier
|lean}]

let x : int{ spec_id _ = 0 } = refine_ 0
