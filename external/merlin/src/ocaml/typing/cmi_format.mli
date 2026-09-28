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

(* Vox: what a unit's verification assumed rather than checked. Compilers
   with refinement types record it in the .cmi, .cmo and .cmx files of every
   unit compiled with -extension refinement_types; see typing/vox_trust.mli. *)
type vox_status =
  | Vox_interface     (* compiled from an .mli; nothing to verify *)
  | Vox_verified      (* verified, possibly by another compilation of the
                         same source (see -smt-assume-verified) *)
  | Vox_not_verified  (* compiled with -smt-assume-verified *)

type vox_item_kind =
  | Vox_refined_external  (* an external whose type states a refinement *)
  | Vox_total_external    (* an external declared total *)
  | Vox_total_cast        (* a %identity external whose result is total *)
  | Vox_builtin           (* an external the verifier gives a built-in
                             meaning *)
  | Vox_cast_use          (* a definition that applies such a cast *)
  | Vox_external          (* another external, outside the library *)
  | Vox_unsafe            (* Obj, Marshal, an unsafe primitive, an external
                             that returns a value of any type, -unsafe or an
                             unsafe attribute *)

type vox_item = {
  vox_kind : vox_item_kind;
  vox_name : string;
  vox_location : string;  (* of the first occurrence *)
  vox_count : int;
}

type vox_unit = {
  vox_status : vox_status;
  vox_library : bool;       (* compiled with -vox-library *)
  vox_source : string;      (* hex digest of the parsed source, or "" *)
  vox_config : string;      (* the flags that change what was verified *)
  vox_solver : string;      (* the solver's version, if it was not the
                               expected one (-smt-solver-any-version) *)
  vox_imports : string list;
      (* the units the implementation imports, which the interface may not *)
  vox_items : vox_item list;
}

type pers_flags =
  | Rectypes
  | Alerts of alerts
  | Opaque
  | Vox of vox_unit

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
    cmi_sign : 'sg * Mode.Staticity.Const.t;
    cmi_params : Global_module.Parameter_name.t list;
    cmi_crcs : Import_info.t array;
    cmi_flags : pers_flags list;
}

type cmi_infos_lazy = Subst.Lazy.signature cmi_infos_generic
type cmi_infos = Types.signature cmi_infos_generic

(* write the magic + the cmi information *)
val output_cmi : string -> out_channel -> cmi_infos_lazy -> Digest.t

(* read the cmi information (the magic is supposed to have already been read) *)
val input_cmi : in_channel -> cmi_infos
val input_cmi_lazy : in_channel -> cmi_infos_lazy

(* read a cmi from a filename, checking the magic *)
val read_cmi : string -> cmi_infos
val read_cmi_lazy : string -> cmi_infos_lazy

(* Error report moved to {!Magic_numbers.Cmi} *)
