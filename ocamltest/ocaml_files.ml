(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Locations of files in the OCaml source tree *)

open Ocamltest_stdlib

type runtime_variant =
  | Normal
  | Debug
  | Instrumented

let runtime_variant() =
  let use_runtime = Sys.safe_getenv "USE_RUNTIME" in
  if use_runtime="d" then Debug
  else if use_runtime="i" then Instrumented
  else Normal

let ocamlrun =
  let runtime = match runtime_variant () with
    | Normal -> "ocamlrun"
    | Debug -> "ocamlrund"
    | Instrumented -> "ocamlruni" in
  let ocamlrunfile = Filename.mkexe runtime in
  Filename.make_path [Ocaml_directories.srcdir; "runtime"; ocamlrunfile]

let file_with_override variable default =
  let override = Sys.safe_getenv variable in
  if override = "" then default, false else override, true

let ocamlc, ocamlc_is_overridden =
  file_with_override "OCAMLTEST_OCAMLC_BYTE"
    (Filename.make_path [Ocaml_directories.srcdir; Filename.mkexe "ocamlc"])

let ocamlc_uses_runtime = not ocamlc_is_overridden

let ocaml, ocaml_is_overridden =
  file_with_override "OCAMLTEST_OCAML"
    (Filename.make_path [Ocaml_directories.srcdir; Filename.mkexe "ocaml"])

let ocaml_uses_runtime = not ocaml_is_overridden

let ocamlc_dot_opt, _ =
  file_with_override "OCAMLTEST_OCAMLC_OPT"
    (Filename.make_path
       [Ocaml_directories.srcdir; Filename.mkexe "ocamlc.opt"])

let ocamlopt, ocamlopt_is_overridden =
  file_with_override "OCAMLTEST_OCAMLOPT_BYTE"
    (Filename.make_path [Ocaml_directories.srcdir; Filename.mkexe "ocamlopt"])

let ocamlopt_uses_runtime = not ocamlopt_is_overridden

let ocamlopt_dot_opt, _ =
  file_with_override "OCAMLTEST_OCAMLOPT_OPT"
    (Filename.make_path
       [Ocaml_directories.srcdir; Filename.mkexe "ocamlopt.opt"])

let ocamlnat =
  Filename.make_path [Ocaml_directories.srcdir; Filename.mkexe "ocamlnat"]

let cmpbyt =
  Filename.make_path
    [Ocaml_directories.srcdir; "tools"; Filename.mkexe "cmpbyt"]

let expect, _ =
  file_with_override "OCAMLTEST_EXPECT"
    (Filename.make_path
       [Ocaml_directories.srcdir; "testsuite"; "tools";
        Filename.mkexe "expect"])

let expectnat, _ =
  file_with_override "OCAMLTEST_EXPECTNAT"
    (Filename.make_path
       [Ocaml_directories.srcdir; "testsuite"; "tools";
        Filename.mkexe "expectnat"])

let ocamllex =
  Filename.make_path
    [Ocaml_directories.srcdir; "lex"; Filename.mkexe "ocamllex"]

let ocamlyacc =
  Filename.make_path
    [Ocaml_directories.srcdir; "yacc"; Filename.mkexe "ocamlyacc"]

let ocamldoc =
  Filename.make_path
    [Ocaml_directories.srcdir; "ocamldoc"; Filename.mkexe "ocamldoc"]

let ocamldebug =
  Filename.make_path
    [Ocaml_directories.srcdir; "debugger"; Filename.mkexe "ocamldebug"]

let ocamlobjinfo =
  Filename.make_path
    [Ocaml_directories.srcdir; "tools"; Filename.mkexe "ocamlobjinfo"]

let ocamlmklib =
  Filename.make_path
    [Ocaml_directories.srcdir; "tools"; Filename.mkexe "ocamlmklib"]

let codegen =
  Filename.make_path
    [Ocaml_directories.srcdir; "testsuite"; "tools"; Filename.mkexe "codegen"]

let fexprc =
  Filename.make_path
    [Ocaml_directories.srcdir; "testsuite"; "tools"; Filename.mkexe "fexprc"]

let asmgen_archmod =
  let objname =
    "asmgen_" ^ Ocamltest_config.arch ^ "." ^ Ocamltest_config.objext
  in
  Filename.make_path [Ocaml_directories.srcdir; "testsuite"; "tools"; objname]
