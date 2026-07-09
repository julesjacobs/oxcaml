(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "lean_viadep.mli lean_viadep.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Task #53 TIER 2 (auto-name a call by its exact result contract),
   cross-unit: the inner [add k2 v2 m] is not reflectable (a regular
   function, not a [total_]), but its result contract is [_ = vadd k2 v2
   m], so the call names [vadd k2 v2 m] and instantiates the OUTER add's
   dependent binder -- mechanizing [let m' = add k2 v2 m in add k v m'].
   Exercises the cross-unit dependent-binder substitution path (the F-1
   territory) with a foreign provider. *)
open Lean_viadep

let nest (k : int) (v : int) (k2 : int) (v2 : int) (m : t)
  : t{ _ = vadd k v (vadd k2 v2 m) } =
  add k v (add k2 v2 m)
