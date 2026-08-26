(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Interpretation of TSL blocks and operations on test trees *)

open Tsl_ast

val apply_modifiers : Environments.t -> string located -> Environments.t

val interpret_environment_statement :
  Environments.t -> Tsl_ast.environment_statement Tsl_ast.located ->
  Environments.t

exception No_such_test_or_action of string
val lookup_test : string located -> Tests.t

type ('state, 'result) step = Continue of 'state | Stop of 'result

(** Split arms inherit the incoming state and each run the remaining
    statements and subtrees. Only terminal results are joined. *)
val walk_tree :
  environment:(int list -> 'state -> environment_statement located ->
    ('state, 'result) step) ->
  test:(int list -> 'state -> string located -> string located list ->
    ('state, 'result) step) ->
  split:(int -> int -> unit) ->
  finish:('state -> 'result) ->
  join:('result list -> 'result) -> 'state -> Tsl_ast.t -> 'result

type test_tree =
  | Node of
    (Tsl_ast.environment_statement located list) *
    Tests.t *
    string located list *
    (test_tree list)

val test_trees_of_tsl_block :
  Tsl_ast.tsl_item list ->
  Tsl_ast.environment_statement located list * test_tree list

val tsl_ast_of_test_trees :
  Tsl_ast.environment_statement located list * test_tree list ->
  Tsl_ast.t

val tests_in_tree : Tsl_ast.t -> Tests.TestSet.t

val actions_in_test : Tests.t -> Actions.ActionSet.t

val actions_in_tests : Tests.TestSet.t -> Actions.ActionSet.t

val print_tsl_ast : compact:bool -> out_channel -> Tsl_ast.t -> unit
