let sub_jkind_l
    ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context)
    (sub : Types.jkind_l)
    (super : Types.jkind_l)
    : (unit, Jkind.Violation.t) result =
  print_endline "sub_jkind_l";
  Jkind.sub_jkind_l ~type_equal ~context sub super

(* Minimal kind constructor over real Types.type_expr, inspired by infer6. *)

module TyM = struct
  type t = Types.type_expr

  let compare (t1 : t) (t2 : t) = Int.compare (Types.get_id t1) (Types.get_id t2)

  (* Avoid depending on Printtyp to prevent module cycles. *)
  let to_string (t : t) : string = Printf.sprintf "ty#%d" (Types.get_id t)
end

module ConstrM = struct
  type t = Path.t

  let compare = Stdlib.compare

  let to_string (p : t) : string = Format.asprintf "%a" Path.print p
end

module JK = Ldd_jkind_solver.Make (Axis_lattice) (TyM) (ConstrM)

let kind_of (ty : Types.type_expr) : JK.ckind =
 fun (ops : JK.ops) ->
  match Types.get_desc ty with
  | Types.Tvar {name=_name; jkind=_jkind} | Types.Tunivar {name=_name; jkind=_jkind} -> ops.rigid ty
  | Types.Tconstr (p, args, _abbrev_memo) ->
    let arg_kinds = List.map (fun t -> ops.kind_of t) args in
    ops.constr p arg_kinds
  | Types.Ttuple elts ->
    elts |> List.map (fun (_lbl, t) -> ops.kind_of t) |> ops.join
  | Types.Tunboxed_tuple elts ->
    elts |> List.map (fun (_lbl, t) -> ops.kind_of t) |> ops.join
  | Types.Tarrow (_lbl, _t1, _t2, _commu) -> failwith "kind_of: unhandled type"
  | Types.Tlink t -> ops.kind_of t
  | Types.Tsubst (_, _) -> failwith "kind_of: unhandled type"
  | Types.Tpoly (_, _) -> failwith "kind_of: unhandled type"
  | Types.Tof_kind _ -> failwith "kind_of: unhandled type"
  | Types.Tobject (_, _) -> failwith "kind_of: unhandled type"
  | Types.Tfield (_, _, _, _) -> failwith "kind_of: unhandled type"
  | Types.Tnil -> failwith "kind_of: unhandled type"  
  | Types.Tvariant _ -> failwith "kind_of: unhandled type"
  | Types.Tpackage _ -> failwith "kind_of: unhandled type"

(* Build a JK environment lookup from a Jkind context. This mirrors infer6's
   lookup over a parsed program, but uses the real typing context. *)
let lookup_of_context ~(context : Jkind.jkind_context) (p : Path.t)
    : JK.constr_decl =
  match context.lookup_type p with
  | None -> failwith (Format.asprintf "Ikind.lookup: unknown constructor %a" Path.print p)
  | Some decl -> (
      match decl.type_manifest with
      | None ->
        (* Abstract: no manifest. *)
        (* TODO: use the jkind in the decl instead, convert it to a JK.ckind *)
        let kind : JK.ckind = failwith "lookup_of_context: abstract type" in
        JK.Ty { args = decl.type_params; kind; abstract=true }
      | Some body_ty ->
        let args = decl.type_params in
        let kind : JK.ckind = fun ops -> ops.kind_of body_ty in
        JK.Ty { args; kind; abstract=false})

(* Package the above into a full solver environment. *)
let env_of_context ~(context : Jkind.jkind_context) : JK.env =
  let kind_of = kind_of in
  let lookup = lookup_of_context ~context in
  { JK.kind_of; lookup }

let make_solver ~(context : Jkind.jkind_context) : JK.solver =
  JK.make_solver (env_of_context ~context)
