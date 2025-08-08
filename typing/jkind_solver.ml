(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street, New York                   *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Jkind solver using fixpoint computation *)

type key =
  | Path of Path.t
  | Type of Types.type_expr

type context = {
  env : Env.t;
  jkind_of_type : Types.type_expr -> Types.jkind_l option;
  is_abstract : Path.t -> bool;
}

let make_context env =
  let jkind_context = Ctype.mk_jkind_context env (fun ty ->
    Some (Ctype.type_jkind env ty)) in
  { env;
    jkind_of_type = jkind_context.jkind_of_type;
    is_abstract = jkind_context.is_abstract;
  }

module Key = struct
  type t = key

  let equal k1 k2 =
    match k1, k2 with
    | Path p1, Path p2 -> Path.same p1 p2
    | Type t1, Type t2 -> Types.TypeOps.equal t1 t2
    | _ -> false

  let hash = function
    | Path p -> Hashtbl.hash (0, Path.name p)
    | Type t -> Hashtbl.hash (1, Types.TypeOps.hash t)
end

module JkindValue = struct
  type t = Jkind.t
  let equal = Jkind.equal
end

module Solver = Fixpoint_solver.Make(Key)(JkindValue)

let priority_and_init context key =
  match key with
  | Path p ->
      if context.is_abstract p then
        (false, Jkind.any_non_null)  (* LOW priority, pessimistic *)
      else
        (true, Jkind.any)             (* HIGH priority, optimistic *)
  | Type _ty ->
      (* Types are always concrete: HIGH priority, optimistic *)
      (true, Jkind.any)

let jkind_equations context f_rec key =
  match key with
  | Path _p ->
      (* TODO: Implement jkind computation for type declarations *)
      failwith "Not implemented: jkind_equations for Path"
  | Type _ty ->
      (* TODO: Implement jkind computation for type expressions *)
      failwith "Not implemented: jkind_equations for Type"

let solve context =
  Solver.fix
    (priority_and_init context)
    (jkind_equations context)


(* TODO:
- We should probably not compute jkinds but with/mod bounds only. The layout is fixed.
- We should check how with/mod bounds are represented.
- We should ensure that with-bounds only have type variables in them.
- When encountering a constructor, we should recursively compute the jkind (or bounds)
  and also make the recursive call for the path, and then combine them into one.
- We should correctly handle bound type variables and unification variables (differ by
  level -- need to better understand this)
*)
