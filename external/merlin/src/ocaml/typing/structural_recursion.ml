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

open Asttypes
open Types
open Typedtree

type descent = Root | Smaller

exception Not_structural of Location.t * string

let reject loc reason = raise (Not_structural (loc, reason))

let variable exp =
  match exp.exp_desc with
  | Texp_ident { path = Path.Pident id; _ } -> Some id
  | _ -> None

let status facts exp =
  Option.bind (variable exp) (fun id -> Ident.Map.find_opt id facts)

let intersection left right =
  Ident.Map.merge (fun _ a b -> if a = b then a else None) left right

let constructor_path env ty =
  match get_desc (Ctype.expand_head env ty) with
  | Tconstr (path, _, _) -> Some path
  | _ -> None

let rec bind_pattern facts descent pat =
  match pat.pat_desc with
  | Tpat_var { id; _ } ->
      (match descent with
       | None -> facts
       | Some descent -> Ident.Map.add id descent facts)
  | Tpat_alias { pattern = pat; id; _ } ->
      let facts = bind_pattern facts descent pat in
      (match descent with
       | None -> facts
       | Some descent -> Ident.Map.add id descent facts)
  | Tpat_or (left, right, _) ->
      intersection (bind_pattern facts descent left)
        (bind_pattern facts descent right)
  | Tpat_construct (_, desc, _, args, _) when Option.is_some descent ->
      if not (Ctype.is_inductive pat.pat_env desc.cstr_res) then facts
      else
        let owner = constructor_path pat.pat_env desc.cstr_res in
        (* Use the declaration's fields, not instantiated parameter payloads. *)
        List.fold_left2
          (fun facts (arg : Types.constructor_argument) (_, pat) ->
            bind_field facts owner arg.ca_type pat)
          facts desc.cstr_args args
  | _ -> facts

and bind_field facts owner ty pat =
  let ty = Ctype.expand_head pat.pat_env ty in
  match get_desc ty with
  | Tconstr (path, _, _)
    when Misc.Stdlib.Option.exists (Path.same path) owner ->
      bind_pattern facts (Some Smaller) pat
  | Ttuple fields ->
      begin match pat.pat_desc with
      | Tpat_tuple pats ->
          List.fold_left2 (fun facts (_, ty) (_, pat) ->
            bind_field facts owner ty pat) facts fields pats
      | Tpat_alias { pattern = pat; _ } -> bind_field facts owner ty pat
      | Tpat_or (left, right, _) ->
          intersection (bind_field facts owner ty left)
            (bind_field facts owner ty right)
      | _ -> facts
      end
  | _ -> facts

let check_parameter self body index root =
  let rec iterator facts : Tast_iterator.iterator =
    let default = Tast_iterator.default_iterator in
    { default with
      expr = (fun it exp ->
        match exp.exp_desc with
        | Texp_apply (fn, args, _, _, _, _)
          when Misc.Stdlib.Option.exists (Ident.same self) (variable fn) ->
            let actuals = List.map (function
              | Nolabel, Arg (arg, _) -> arg
              | _ -> assert false) args
            in
            if status facts (List.nth actuals index) <> Some Smaller then
              reject exp.exp_loc
                "the recursive argument is not a known proper descendant";
            List.iter (it.expr it) actuals
        | Texp_let (Nonrecursive, bindings, body) ->
            List.iter (it.value_binding it) bindings;
            let facts = List.fold_left (fun facts binding ->
              bind_pattern facts (status facts binding.vb_expr) binding.vb_pat)
                facts bindings
            in
            let it = iterator facts in
            it.expr it body
        | Texp_match (scrutinee, _, cases, effects, _) ->
            it.expr it scrutinee;
            List.iter (fun case ->
              let pat, exception_pat = split_pattern case.c_lhs in
              let facts = match pat, exception_pat with
                | Some pat, None ->
                    bind_pattern facts (status facts scrutinee) pat
                | _ -> facts
              in
              let it = iterator facts in
              it.case it case) cases;
            List.iter (it.case it) effects
        | _ -> default.expr it exp)
    }
  in
  let it = iterator (Ident.Map.singleton root Root) in
  it.expr it body

let check self exp =
  try
    let params, body = Recursive_function.parameters exp in
    let candidates =
      List.mapi (fun index (id, pat) -> index, id, pat) params
      |> List.filter_map (fun (index, id, pat) ->
        if Ctype.is_inductive pat.pat_env pat.pat_type
        then Some (index, id) else None)
    in
    if candidates <> [] then Recursive_function.check_uses self exp;
    let rec try_parameters failure = function
      | [] -> Error failure
      | (index, id) :: rest ->
          match check_parameter self body index id with
          | () -> Ok ()
          | exception Not_structural (loc, reason) ->
              try_parameters (loc, reason) rest
    in
    try_parameters
      (exp.exp_loc, "no parameter has a checked inductive datatype")
      candidates
  with
  | Not_structural (loc, reason)
  | Recursive_function.Invalid (loc, reason) -> Error (loc, reason)
