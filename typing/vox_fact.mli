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

(** The fact environment: the boolean hypotheses in scope at an obligation
    site (design-docs/vc-generation.md, "The fact environment").

    Persistent, not mutable: the walk passes a value down, so entering a
    branch with the current environment gives scoping for free — whatever
    the branch added vanishes when the walk resumes with the parent's
    value.  Facts fail open (a source may decline what it cannot lower — a
    completeness gap, never a soundness gap); obligations fail closed. *)

type t

val empty : t

(** Add a fact: a boolean term of the sorted IR, with the one label and
    location that constitute its provenance. *)
val add : t -> Vox_lower.Ir.t -> label:string -> loc:Location.t -> t

(** Snapshot for an obligation: hypothesis ids are assigned here, in
    insertion order, so they are stable across runs and usable as
    unsat-core currency.  Terms cross the one emitter
    ({!Vox_lower.emit}). *)
val hypotheses : t -> Vox_logic.Obligation.hypothesis list
