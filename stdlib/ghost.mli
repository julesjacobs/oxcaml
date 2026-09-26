(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                            The vox authors                             *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Ghost values.

    ['a Ghost.t] erases a value from the runtime representation entirely:
    the [ghost] field occupies no slot, so the wrapper has kind [void] —
    no register, no block slot, nothing at run time. A function taking an
    ['a Ghost.t] parameter has the same calling convention as one without
    it, and a record field of type ['a Ghost.t] occupies no space. (Being
    void, it cannot inhabit value-polymorphic containers such as ['a list];
    store it as a record field instead.)

    The field value must be total, since it remains usable by total ghost
    computations.

    Construct and project directly: [{ Ghost.ghost = v }] evaluates [v]
    and discards it (write [ghost_ v] to skip the evaluation too); reading
    [x.Ghost.ghost] gives a placeholder at mode [ghost], usable only in
    ghost positions such as under [ghost_]. *)

type 'a t = { ghost : 'a @@ ghost }
