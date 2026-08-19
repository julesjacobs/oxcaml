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

(** Verification-condition generation: one walk of the typed structure
    after typechecking, collecting the obligations refinement-flow
    recorded, assembling facts ({!Vox_fact}), lowering them
    ({!Vox_lower}) and discharging each through the selected backend
    (design-docs/vc-generation.md).

    The driver calls this behind [-vox-backend]; under the default
    ([none]) the pass does not run at all and obligations stay
    recorded-and-accepted. *)

(** Verify one implementation unit.  Obligations are discharged
    sequentially, in source order; the pass continues past a failed
    obligation (whose goal becomes a fact) and raises one located error
    refusing the unit at exit.  [dump_only] is {!Vox_backend.plan}'s
    [Dump] arm: the whole pass runs and the printing backend's expected
    non-verdict ([Ok (Unknown _)]) is suppressed rather than refusing the
    unit — a [discharge] error still refuses it. *)
val implementation :
  backend:(module Vox_backend.BACKEND) ->
  dump_only:bool ->
  config:Vox_backend.Config.t ->
  Typedtree.structure ->
  unit
