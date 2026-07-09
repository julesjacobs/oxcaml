(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "mli_totalspec.mli mli_totalspec.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Client of a NAME-ONLY spec export.  [Mli_totalspec.len] is named as
   an ordinary qualified value path (a reflected [total_] function is a
   real value), and every proof below uses ONLY the exported contracts
   -- never [len]'s hidden equations. *)

(* The measure rides through a contract application: append's exported
   law [len (append a b) = len a + len b] discharges this with no
   unfolding. *)
let chain (a : Mli_totalspec.ilist{ Mli_totalspec.len _ = 2 })
  (b : Mli_totalspec.ilist{ Mli_totalspec.len _ = 3 }) :
  Mli_totalspec.ilist{ Mli_totalspec.len _ = 5 } =
  Mli_totalspec.append a b

(* A client's OWN len-mentioning refinement, proved from the imported
   value's contract ([two : {len _ = 2}]) and append's law alone. *)
let four : Mli_totalspec.ilist{ Mli_totalspec.len _ = 4 } =
  Mli_totalspec.append Mli_totalspec.two Mli_totalspec.two
