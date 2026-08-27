(* TEST
 setup-ocamlc.byte-build-env;
 src = "${test_source_directory}/../../../verification/vox_smt.mli ${test_source_directory}/../../../verification/vox_smt.ml";
 dst = "./";
 copy;
 module = "vox_smt.mli";
 ocamlc.byte;
 module = "vox_smt.ml";
 ocamlc.byte;
 binary_modules = "vox_smt";
 run-expect;
 check-program-output;
*)

open Vox_smt;;
let n = Symbol.create ~label:"n" Bv63
let next = App (Sub, [Var n; Integer 1L])
let countdown =
  { symbols = [n];
    facts = [{label = "positive"; term = App (Gt, [Var n; Integer 0L])}];
    goal = {label = "decreases"; term = App (Lt, [next; Var n])} };;
[%%expect{|
val n : Vox_smt.Symbol.t = <abstr>
val next : Vox_smt.term = App (Sub, [Var <abstr>; Integer 1L])
val countdown : Vox_smt.query =
  {symbols = [<abstr>];
   facts = [{label = "positive"; term = App (Gt, [Var <abstr>; Integer 0L])}];
   goal =
    {label = "decreases";
     term = App (Lt, [App (Sub, [Var <abstr>; Integer 1L]); Var <abstr>])}}
|}]

let () =
  Format.printf "%s%!" (to_smtlib ~int_width:63 ~timeout_ms:5000 countdown);;
[%%expect{|
(set-option :print-success false)
(set-option :produce-models true)
(set-option :timeout 5000)
(set-logic QF_BV)
(declare-fun v0 () (_ BitVec 63))
(assert (bvsgt v0 (_ bv0 63)))
(assert (not (bvslt (bvsub v0 (_ bv1 63)) v0)))
(check-sat)
|}]

let () = check ~int_width:63
  {countdown with goal = {label = "not a proposition"; term = next}};;
[%%expect{|
Exception:
Vox_smt.Sort_error "not a proposition: Expected Bool, got (_ BitVec 63)".
|}]
