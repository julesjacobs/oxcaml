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

let ckind_of_jkind_l (j : Types.jkind_l) : JK.ckind =
  fun (ops : JK.ops) ->
    (* Base is the modality bounds stored on this jkind. *)
    let base = ops.const (Axis_lattice.of_mod_bounds j.jkind.mod_bounds) in
    (* For each with-bound (ty, axes), contribute modality(axes_mask, kind_of ty). *)
    let contribs =
      Jkind.With_bounds.to_seq j.jkind.with_bounds
      |> List.of_seq
      |> List.map (fun (ty, info) ->
             let axes = Jkind.With_bounds.type_info_relevant_axes info in
             let mask = Axis_lattice.of_axis_set axes in
             let kty = ops.kind_of ty in
             ops.modality mask kty)
    in
    ops.join (base :: contribs)

(* Compute the effective left-hand upper-bounds used by the subjkind check,
   taking into account the right-hand with-bounds: for each type present on the
   right, subtract its relevant axes from the left's relevant axes. This matches
   the intent of MB_EXPAND_L in Jkind.sub_jkind_l. *)
(* (intentionally left without the "effective" variant; we model with-bounds
   as a join of masked contributions, independent of the right-hand side) *)

(* Build a JK environment lookup from a Jkind context. This mirrors infer6's
   lookup over a parsed program, but uses the real typing context. *)
let lookup_of_context ~(context : Jkind.jkind_context) (p : Path.t)
    : JK.constr_decl =
  match context.lookup_type p with
  | None -> failwith (Format.asprintf "Ikind.lookup: unknown constructor %a" Path.print p)
  | Some decl -> (
      match decl.type_manifest with
      | None ->
        (* Abstract: no manifest. Model as top over axes; params from decl. *)
        let kind : JK.ckind = ckind_of_jkind_l decl.type_jkind in
        JK.Ty { args = decl.type_params; kind; abstract = true }
      | Some body_ty ->
        (* Concrete: compute kind of body. *)
        let args = decl.type_params in
        let kind : JK.ckind = fun ops -> ops.kind_of body_ty in
        JK.Ty { args; kind; abstract = false })

(* Package the above into a full solver environment. *)
let env_of_context ~(context : Jkind.jkind_context) : JK.env =
  let kind_of = kind_of in
  let lookup = lookup_of_context ~context in
  { JK.kind_of; lookup }

let make_solver ~(context : Jkind.jkind_context) : JK.solver =
  JK.make_solver (env_of_context ~context)

let sub_jkind_l
    ?allow_any_crossing
    ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context)
    (sub : Types.jkind_l)
    (super : Types.jkind_l)
    : (unit, Jkind.Violation.t) result =
  print_endline "sub_jkind_l";
  (match allow_any_crossing with
   | Some true -> print_endline "allow_any_crossing=true"
   | _ -> ());
  let solver = make_solver ~context in
  let sub_poly = JK.normalize solver (ckind_of_jkind_l sub) in
  let super_poly = JK.normalize solver (ckind_of_jkind_l super) in
  let sub_poly_pp = JK.pp sub_poly in
  let super_poly_pp = JK.pp super_poly in
  (* Print sub: <sub_poly_pp>, super: <super_poly_pp> *)
  print_endline (Format.asprintf "sub: %s, super: %s" sub_poly_pp super_poly_pp);
  let res = Jkind.sub_jkind_l ?allow_any_crossing ~type_equal ~context sub super in
  (match res with
   | Ok () -> print_endline "jkind: ok"
   | Error _ -> print_endline "jkind: error");
  res
