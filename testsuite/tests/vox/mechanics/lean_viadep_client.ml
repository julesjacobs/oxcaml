(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "lean_viadep.mli lean_viadep.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* F-1 regression (task #41): two literal scalars before the via
   argument.  With the Subst binder-freshening collision, [add 1 10 m]'s
   result modelled [vadd 1 10 1] (the via arg aliased to the first
   literal), so this obligation FAILED; with the fix it models
   [vadd 1 10 m] and flows through with no obligation. *)
open Lean_viadep

let two_literals (m : t) : t{ _ = vadd 1 10 m } = add 1 10 m
