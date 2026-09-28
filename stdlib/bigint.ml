(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

type t : immutable_data

external of_int : int -> t @@ total = "caml_bigint_of_int"
external to_int_opt : t -> int option @@ total = "caml_bigint_to_int_opt"
external neg : t -> t @@ total = "caml_bigint_neg"
external add : t -> t -> t @@ total = "caml_bigint_add"
external sub : t -> t -> t @@ total = "caml_bigint_sub"
external mul : t -> t -> t @@ total = "caml_bigint_mul"
external div : t -> t -> t @@ total = "caml_bigint_div"
external modulo : t -> t -> t @@ total = "caml_bigint_modulo"
external equal : t -> t -> bool @@ total = "%equal"
external compare : t -> t -> int @@ total = "%compare"
external to_string : t -> string @@ total = "caml_bigint_to_string"

let zero = of_int 0
let one = of_int 1
let pp fmt n = Format.pp_print_string fmt (to_string n)

external ( ~- ) : t -> t @@ total = "caml_bigint_neg"
external ( + ) : t -> t -> t @@ total = "caml_bigint_add"
external ( - ) : t -> t -> t @@ total = "caml_bigint_sub"
external ( * ) : t -> t -> t @@ total = "caml_bigint_mul"
external ( / ) : t -> t -> t @@ total = "caml_bigint_div"
external ( mod ) : t -> t -> t @@ total = "caml_bigint_modulo"
external ( = ) : t -> t -> bool @@ total = "%equal"
external ( <> ) : t -> t -> bool @@ total = "%notequal"
external ( < ) : t -> t -> bool @@ total = "%lessthan"
external ( <= ) : t -> t -> bool @@ total = "%lessequal"
external ( > ) : t -> t -> bool @@ total = "%greaterthan"
external ( >= ) : t -> t -> bool @@ total = "%greaterequal"
