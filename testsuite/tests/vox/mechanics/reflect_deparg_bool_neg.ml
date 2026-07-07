(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Task #67 SOUNDNESS: the connective model must be a threaded formula
   over the operands, not an unconstrained unknown -- so a genuinely
   FALSE connective goal is DISPROVED (with the operand facts visible),
   never mis-accepted. *)

type vopt = Vnone | Vsome of int

let rec total_ has (o : vopt) : bool =
  match o with
  | Vnone -> false
  | Vsome _ -> true

let hasb : (o : vopt) -> bool{ _ = has o } = fun o -> has o

let bad : bool{ _ = true } = hasb Vnone && hasb Vnone
