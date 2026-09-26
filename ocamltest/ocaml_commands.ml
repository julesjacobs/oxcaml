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

(* Helper functions to build OCaml-related commands *)

let ocamlrun program =
  Ocaml_files.ocamlrun ^ " " ^ program

let with_optional_runtime ~uses_runtime program =
  if uses_runtime then ocamlrun program else program

let ocamlrun_ocamlc =
  with_optional_runtime
    ~uses_runtime:Ocaml_files.ocamlc_uses_runtime Ocaml_files.ocamlc

let ocamlrun_ocamlopt =
  with_optional_runtime
    ~uses_runtime:Ocaml_files.ocamlopt_uses_runtime Ocaml_files.ocamlopt

let ocamlrun_ocaml =
  with_optional_runtime
    ~uses_runtime:Ocaml_files.ocaml_uses_runtime Ocaml_files.ocaml

let expect =
  Ocaml_files.expect

let expectnat =
  Ocaml_files.expectnat

let ocamlrun_ocamllex = ocamlrun Ocaml_files.ocamllex

let ocamlrun_ocamldoc =
  ocamlrun Ocaml_files.ocamldoc

let ocamlrun_ocamldebug =
  ocamlrun Ocaml_files.ocamldebug

let ocamlrun_ocamlobjinfo =
  ocamlrun Ocaml_files.ocamlobjinfo

let ocamlrun_ocamlmklib =
  ocamlrun Ocaml_files.ocamlmklib

let codegen =
  Ocaml_files.codegen

let fexprc =
  Ocaml_files.fexprc
