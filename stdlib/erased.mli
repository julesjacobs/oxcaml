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

(** Erased values.

    ['a Erased.t] erases a value from the runtime representation: the
    [erased] field occupies no slot, so the wrapper is the immediate [0]
    whatever it was built from, and the wrapped value is never stored.

    Construct and project directly: [{ Erased.erased = v }] evaluates [v]
    and discards it (write [erased_ v] to skip the evaluation too); reading
    [x.Erased.erased] gives a placeholder at mode [erased], usable only in
    erased positions such as under [erased_]. *)

type 'a t = { erased : 'a @@ erased }
