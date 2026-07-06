(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* SCOPING (variable-shadowing family), FIXED.  A let-binding's OWN
   refinement annotation used to be elaborated with the bound name
   treated as the value being defined (a [`Self] alias for [_]), so a
   free occurrence of that name SELF-CAPTURED instead of resolving, by
   ordinary OCaml scoping, to an outer binding of the same name.  The
   self-name now yields to any binding in scope, applying only when the
   name is otherwise unbound.

   Here the [x] in [int{ _ = x }] on the second binding is the FIRST [x]
   (a module-level value), NOT the value being defined.  Before the fix
   it self-captured to the tautology [3 = 3] and this ACCEPTED (a
   definition whose written refinement is false).  Now [x] resolves to
   the outer module value, so -- like any module-level type mentioning
   another module value -- it is REJECTED (a module-level type must be
   self-contained; the crisp [3 = 7] VC is the analogous LOCAL exhibit,
   see scope_selfcapture_ok.ml's companion for the accepted dual). *)

let x : int{ _ = 7 } = 7
let x : int{ _ = x } = 3
