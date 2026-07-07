(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "lean_mopt.mli lean_mopt.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Task #63 lock: a type-PARAMETERIZED payload does NOT share the
   via-field universe bug -- an [S_param] field is a bound [Type] binder,
   so the compiler auto-generates a correct [(a0 : Type)]-parameterized
   Lean inductive with no forward-referenced ghost sort.  Guards the
   emission-order fix against ever breaking the [(v : Type)] generation. *)
type 'v mopt = MMiss | MFound of 'v

[%%vox.lean {lean|
@[grind, expose] public def is_found : {a0 : Type} -> Vox_Lean_mopt_mopt a0 -> Prop
  | _, .MMiss => False
  | _, .MFound _ => True
|lean}]

val found : (x : int) -> int mopt{ is_found _ }
