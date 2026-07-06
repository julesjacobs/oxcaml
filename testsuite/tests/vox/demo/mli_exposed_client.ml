(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "mli_exposed.mli mli_exposed.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* With equations EXPOSED (mli_exposed.mli), the same literal-list
   assertion that is UNPROVABLE under a name-only export succeeds here:
   grind unfolds [len].  [len] is named unqualified, as an imported
   block spec function. *)
let ok : Mli_exposed.ilist{ len _ = 2 } =
  Mli_exposed.Cons (5, Mli_exposed.Cons (6, Mli_exposed.Nil))
