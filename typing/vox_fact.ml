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

type fact =
  { term : Vox_lower.Ir.t
  ; label : string
  ; loc : Location.t
  }

(* Newest first; [hypotheses] restores insertion order. *)
type t = fact list

let empty = []

let add t term ~label ~loc = { term; label; loc } :: t

let hypotheses t =
  List.rev t
  |> List.mapi (fun i { term; label; loc } ->
    { Vox_logic.Obligation.id = i + 1
    ; term = Vox_lower.emit term
    ; origin = { Vox_logic.Origin.label; location = loc }
    })

let terms t = List.rev_map (fun { term; _ } -> term) t
