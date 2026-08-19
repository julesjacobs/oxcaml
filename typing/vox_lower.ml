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

module Ir = struct
  type t =
    { desc : desc
    ; sort : Vox_logic.Sort.t
    ; loc : Location.t
    }

  and desc =
    | Var of string
    | Const of Vox_logic.Literal.t
    | App of Vox_logic.Op.t * t list
    | Call of string * t list
    | Ite of t * t * t
    | Construct of string * t list
    | Select of string * int * t
    | Test of string * t
    | Hole
    | Let of string * t * t
    | Lambda of string list * t
end

module Symbols = struct
  type t = { mutable next_opaque : int }

  let create () = { next_opaque = 0 }

  let value _t _path ~sort:_ =
    Misc.fatal_error "Vox_lower.Symbols.value: not yet implemented"

  let func _t _path ~params:_ ~result:_ =
    Misc.fatal_error "Vox_lower.Symbols.func: not yet implemented"

  let fresh_opaque t ~sort:_ =
    let n = t.next_opaque in
    t.next_opaque <- n + 1;
    Printf.sprintf "result/%d" n

  let to_signature _t =
    Misc.fatal_error "Vox_lower.Symbols.to_signature: not yet implemented"
end

let sort_of_type _env _ty =
  Misc.fatal_error "Vox_lower.sort_of_type: not yet implemented"

let lower_subject _symbols _expr =
  Misc.fatal_error "Vox_lower.lower_subject: not yet implemented"

let lower_predicate _symbols ~env:_ ~hole_sort:_ _rexp =
  Misc.fatal_error "Vox_lower.lower_predicate: not yet implemented"

let rec substitute_hole (ir : Ir.t) ~hole =
  let subst t = substitute_hole t ~hole in
  let desc : Ir.desc =
    match ir.desc with
    | Hole -> hole.Ir.desc
    | (Var _ | Const _) as desc -> desc
    | App (op, args) -> App (op, List.map subst args)
    | Call (f, args) -> Call (f, List.map subst args)
    | Ite (c, a, b) -> Ite (subst c, subst a, subst b)
    | Construct (c, args) -> Construct (c, List.map subst args)
    | Select (c, i, t) -> Select (c, i, subst t)
    | Test (c, t) -> Test (c, subst t)
    | Let (x, e, body) -> Let (x, subst e, subst body)
    | Lambda (xs, body) -> Lambda (xs, subst body)
  in
  { ir with desc }

let rec emit (ir : Ir.t) : Vox_logic.Term.t =
  match ir.desc with
  | Var name -> Var name
  | Const literal -> Const literal
  | App (op, args) -> App (op, List.map emit args)
  | Call (f, args) -> Call (f, List.map emit args)
  | Ite (c, a, b) -> Ite (emit c, emit a, emit b)
  | Construct (c, args) -> Construct (c, List.map emit args)
  | Select (c, i, t) -> Select (c, i, emit t)
  | Test (c, t) -> Test (c, emit t)
  | Hole -> Misc.fatal_error "Vox_lower.emit: residual hole"
  | Let _ -> Misc.fatal_error "Vox_lower.emit: residual let binder"
  | Lambda _ -> Misc.fatal_error "Vox_lower.emit: residual lambda binder"

let canonicalise _obligation =
  Misc.fatal_error "Vox_lower.canonicalise: not yet implemented"
