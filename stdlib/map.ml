# 2 "map.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

include CamlinternalMap

module type EquatableType = sig
  type t : logical_data
  val equal : t -> t -> bool @@ total
  val reflexive : (x : t) -> {u : unit | equal x x} @@ total
  val symmetric : (x : t) -> (y : t) ->
    {u : unit | equal x y = equal y x} @@ total
  val transitive : (x : t) -> (y : t) -> (z : t) ->
    {u : unit | not (equal x y && equal y z) || equal x z} @@ total
end

module MakeLogical (Key : EquatableType) = struct
  type ('a : immutable_data) t : logical_data with 'a

  external empty : ('a : immutable_data). unit -> 'a t @ ghost
    @@ total = "caml_logical_map_empty"
  external find_opt : ('a : immutable_data).
    Key.t -> 'a t -> 'a option @ ghost
    @@ total = "caml_logical_map_find_opt"
  external mem : ('a : immutable_data). Key.t -> 'a t -> bool @ ghost
    @@ total = "caml_logical_map_mem"
  external add : ('a : immutable_data).
    Key.t -> 'a -> 'a t -> 'a t @ ghost
    @@ total = "caml_logical_map_add"
  external remove : ('a : immutable_data). Key.t -> 'a t -> 'a t @ ghost
    @@ total = "caml_logical_map_remove"
  external cardinal : ('a : immutable_data).
    'a t -> {n : Bigint.t | 0Z <= n} @ ghost
    @@ total = "caml_logical_map_cardinal"

  module Proof = struct
    (** A distinguishing key for unequal maps. Matching on the result lets
        a pointwise proof establish map equality. *)
    external difference : ('a : immutable_data).
      (left : 'a t) -> (right : 'a t) ->
      {result : Key.t option | match result with
        | None -> left === right
        | Some key -> not (find_opt key left === find_opt key right)} @ ghost
      @@ total = "caml_logical_map_difference"
  end
end
