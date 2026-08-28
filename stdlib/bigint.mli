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

@@ portable

open! Stdlib

(** Unbounded signed integers. *)

type t : immutable_data

val zero : t
val one : t

external of_int : int -> t @@ total = "caml_bigint_of_int"
(** Preserve the signed value. *)

external to_int_opt : t -> int option @@ total = "caml_bigint_to_int_opt"
(** Return [None] outside the signed machine-integer range. *)

external neg : t -> t @@ total = "caml_bigint_neg"
external add : t -> t -> t @@ total = "caml_bigint_add"
external sub : t -> t -> t @@ total = "caml_bigint_sub"
external mul : t -> t -> t @@ total = "caml_bigint_mul"

external div : t -> t -> t @@ total = "caml_bigint_div"
(** Euclidean quotient: for nonzero [b], [a = b * div a b + modulo a b]
    and [0 <= modulo a b < abs b]. At zero, [div a zero = zero]. *)

external modulo : t -> t -> t @@ total = "caml_bigint_modulo"
(** Euclidean remainder. At zero, [modulo a zero = a]. *)

external equal : t -> t -> bool @@ total = "%equal"
external compare : t -> t -> int @@ total = "%compare"
(** Numeric comparison, returning [-1], [0], or [1]. Polymorphic comparison
    and hashing also use the numeric value. *)

external to_string : t -> string @@ total = "caml_bigint_to_string"
(** Canonical decimal representation, without leading zeros or a [+] sign. *)

val pp : Format.formatter -> t -> unit
(** Print the decimal representation. *)

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
