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

    ['a Ghost.t] erases a value from the runtime representation: the
    [ghost] field occupies no slot, so the wrapper is the immediate [0]
    whatever it was built from, and the wrapped value is never stored.

    Construct and project directly: [{ Ghost.ghost = v }] evaluates [v]
    and discards it (write [ghost_ v] to skip the evaluation too); reading
    [x.Ghost.ghost] gives a placeholder at mode [ghost], usable only in
    ghost positions such as under [ghost_]. *)

type 'a t = { ghost : 'a @@ ghost }
