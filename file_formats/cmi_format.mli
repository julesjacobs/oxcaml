(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc

(* vox: the solver-side spec exported by a unit: its embedded
   [%%vox.lean] blocks plus pre-rendered declarations for the
   datatypes its exported refinements are about (a client may never
   mention those types itself, yet the blocks reference them). *)
type vox_prelude_export = {
  vp_datatypes : (string * string) list;
      (* (stable solver-side name, Lean declaration), in dependency
         order; clients deduplicate by name across imports *)
  vp_needs_voxu : bool;
      (* a datatype field uses the uninterpreted sort *)
  vp_blocks : string list;
      (* block text, in source order *)
}

type pers_flags =
  | Rectypes
  | Alerts of alerts
  | Opaque
  | Vox_prelude of vox_prelude_export
      (* collected by Typemod, consumed by Vox_verify in every client.
         Appending a constructor keeps old .cmis readable. *)

type kind =
  | Normal of {
      cmi_impl : Compilation_unit.t;
        (* If this module takes parameters, [cmi_impl] will be the functor that
           generates instances *)
      cmi_arg_for : Global_module.Parameter_name.t option;
    }
  | Parameter

type 'sg cmi_infos_generic = {
    cmi_name : Compilation_unit.Name.t;
    cmi_kind : kind;
    cmi_globals : Global_module.With_precision.t array;
    cmi_sign : 'sg;
    cmi_params : Global_module.Parameter_name.t list;
    cmi_crcs : Import_info.t array;
    cmi_flags : pers_flags list;
}

type cmi_infos_lazy = Subst.Lazy.persistent_signature cmi_infos_generic
type cmi_infos = Types.persistent_signature cmi_infos_generic

(* write the magic + the cmi information *)
val output_cmi : string -> out_channel -> cmi_infos_lazy -> Digest.t

(* read the cmi information (the magic is supposed to have already been read) *)
val input_cmi : in_channel -> cmi_infos
val input_cmi_lazy : in_channel -> cmi_infos_lazy

(* read a cmi from a filename, checking the magic *)
val read_cmi : string -> cmi_infos
val read_cmi_lazy : string -> cmi_infos_lazy

(* Error report *)

type error =
  | Not_an_interface of filepath
  | Wrong_version_interface of filepath * string
  | Corrupted_interface of filepath

exception Error of error

val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer
