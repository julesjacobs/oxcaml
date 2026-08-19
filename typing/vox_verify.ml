(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street                             *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* A pending obligation, as the walk produces them in source order; each
   becomes one [Vox_logic.Obligation.t] through the lowering. *)
type pending =
  { subject : Typedtree.expression
  ; imposed : Types.type_expr  (* refined; head = the predicate *)
  ; facts : Vox_fact.t  (* in scope at the subject *)
  ; loc : Location.t
  }
[@@warning "-34-69"] (* becomes live with the walk *)

let implementation ~backend:_ ~dump_only:_ ~config:_ _structure =
  Misc.fatal_error "Vox_verify.implementation: not yet implemented"
