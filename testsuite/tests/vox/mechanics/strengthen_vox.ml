(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "strengthen_vox_lib.mli strengthen_vox_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* D8 regression: signature strengthening (mtype.ml) is vox-blind but sound by
   the [{ decl with ... }] rebuild -- the [refines] rides in the jkind, the
   added [= M.t] manifest is ignored by inclusion (which reads the jkind first),
   and [Sig_value] contracts pass through untouched.  This pins that accident so
   a future [strengthen_decl] change that drops a vox payload fails loudly. *)

(* [module A = M] strengthens: A.t = Strengthen_vox_lib.M.t, the [refines int]
   sort and [get]'s contract must survive. *)
module A = Strengthen_vox_lib.M

(* The strengthened [A.get] still carries its refined return contract. *)
let use_get : (x : A.t) -> int{ _ = 0 -> true } = fun x -> A.get x

(* Re-ascribe the strengthened module against a signature that DEMANDS the sort
   and the contract: exercises inclusion of the strengthened declaration. *)
module A2 : sig
  type t [@@vox.sort int]
  val get : (x : t) -> int{ _ = 0 -> true }
end = Strengthen_vox_lib.M
