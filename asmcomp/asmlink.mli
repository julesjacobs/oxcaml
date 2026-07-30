(**************************************************************************)
(* *)
(* OCaml *)
(* *)
(* Xavier Leroy, projet Cristal, INRIA Rocquencourt *)
(* *)
(* Copyright 1996 Institut National de Recherche en Informatique et *)
(* en Automatique. *)
(* *)
(* All rights reserved. This file is distributed under the terms of *)
(* the GNU Lesser General Public License version 2.1, with the *)
(* special exception on linking described in the file LICENSE. *)
(* *)
(**************************************************************************)

(* Link a set of .cmx/.o files and produce an executable or a plugin *)

open Misc
open Format

val link
  :  (module Compiler_owee.Unix_intf.S)
  -> Linkenv.t
  -> string list
  -> string
  -> cached_genfns_imports:Generic_fns.Partition.Set.t
  -> genfns:Generic_fns.Tbl.t
  -> units_tolink:Linkenv.unit_link_info list
  -> uses_eval:bool
  -> quoted_cmi:Compilation_unit.Name.Set.t
  -> quoted_cmx:Compilation_unit.Set.t
  -> ppf_dump:Format.formatter
  -> unit

(** Emit only the stable startup object described by [keyfile] (written by
    [ocamlopt -emit-startup-key]) to [output_name]. Reads no .cmx/.cmxa: the object is a
    pure function of the key file and the codegen flags on the command line. *)
val compile_startup_stable
  :  (module Compiler_owee.Unix_intf.S)
  -> keyfile:string
  -> output_name:string
  -> ppf_dump:Format.formatter
  -> unit

(** Emit the stable startup object from a plan-stable file (no scan, no CRCs). *)
val compile_startup_stable_from_plan
  :  (module Compiler_owee.Unix_intf.S)
  -> stable:Link_plan.stable
  -> output_name:string
  -> ppf_dump:Format.formatter
  -> unit

(** Emit the volatile startup object from a plan-volatile file (replays the plan's stored
    globals_map; byte-identical to the scan-driven volatile object). *)
val compile_startup_volatile_from_plan
  :  (module Compiler_owee.Unix_intf.S)
  -> volatile:Link_plan.volatile
  -> output_name:string
  -> ppf_dump:Format.formatter
  -> unit

val link_shared
  :  (module Compiler_owee.Unix_intf.S)
  -> string list
  -> string
  -> genfns:Generic_fns.Tbl.t
  -> units_tolink:Linkenv.unit_link_info list
  -> ppf_dump:Format.formatter
  -> unit

val call_linker_shared : ?native_toplevel:bool -> string list -> string -> unit
