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

(* Monotonic counter to identify individual subjkind checks within a process. *)
let __ikind_call_counter = ref 0

(* Optional ambient tag to disambiguate higher-level call sites (e.g. includecore).
   Other modules can bracket calls with [with_origin_tag] to add this suffix. *)
let __ikind_origin_tag : string option ref = ref None

let with_origin_tag (tag : string) (f : unit -> 'a) : 'a =
  let prev = !__ikind_origin_tag in
  __ikind_origin_tag := Some tag;
  Fun.protect ~finally:(fun () -> __ikind_origin_tag := prev) f

(* IK-only: compute relevant axes of a constant modality, mirroring
   Jkind.relevant_axes_of_modality. *)
let ik_relevant_axes_of_modality
    ~(relevant_for_shallow:[`Relevant | `Irrelevant])
    (modality : Mode.Modality.Const.t)
  : Jkind_axis.Axis_set.t =
  Jkind_axis.Axis_set.create ~f:(fun ~axis:(Jkind_axis.Axis.Pack axis) ->
      match axis with
      | Jkind_axis.Axis.Modal axis ->
        let m = Mode.Modality.Const.proj axis modality in
        not (Mode.Modality.Atom.is_constant m)
      | Jkind_axis.Axis.Nonmodal Jkind_axis.Axis.Nonmodal.Externality -> true
      | Jkind_axis.Axis.Nonmodal Jkind_axis.Axis.Nonmodal.Nullability -> (
          match relevant_for_shallow with `Relevant -> true | `Irrelevant -> false)
      | Jkind_axis.Axis.Nonmodal Jkind_axis.Axis.Nonmodal.Separability -> (
          match relevant_for_shallow with `Relevant -> true | `Irrelevant -> false))

let ik_base_bounds_nonfloat () : Types.Jkind_mod_bounds.t =
  Types.Jkind_mod_bounds.create
    ~areality:Mode.Regionality.Const.max
    ~linearity:Mode.Linearity.Const.max
    ~uniqueness:Mode.Uniqueness.Const_op.max
    ~portability:Mode.Portability.Const.max
    ~contention:Mode.Contention.Const_op.max
    ~yielding:Mode.Yielding.Const.max
    ~statefulness:Mode.Statefulness.Const.max
    ~visibility:Mode.Visibility.Const_op.max
    ~externality:Jkind_axis.Externality.max
    ~nullability:Jkind_axis.Nullability.Non_null
    ~separability:Jkind_axis.Separability.Non_float

let has_mutable_label lbls =
  List.exists
    (fun (lbl : Types.label_declaration) ->
      match lbl.ld_mutable with Immutable -> false | Mutable _ -> true)
    lbls

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
  | Types.Tarrow (_lbl, _t1, _t2, _commu) ->
    (* Arrows are values; axes only: approximate with non-float base. *)
    ops.const (Axis_lattice.of_mod_bounds (ik_base_bounds_nonfloat ()))
  | Types.Tlink t -> ops.kind_of t
  | Types.Tsubst (t, _) -> ops.kind_of t
  | Types.Tpoly (t, _) -> ops.kind_of t
  | Types.Tof_kind _ -> ops.const (Axis_lattice.of_mod_bounds (ik_base_bounds_nonfloat ()))
  | Types.Tobject (_t, _nm) -> ops.const (Axis_lattice.of_mod_bounds (ik_base_bounds_nonfloat ()))
  | Types.Tfield (_name, _fk, _ty1, ty2) -> ops.kind_of ty2
  | Types.Tnil -> ops.const (Axis_lattice.of_mod_bounds (ik_base_bounds_nonfloat ()))
  | Types.Tvariant _ -> ops.const (Axis_lattice.of_mod_bounds (ik_base_bounds_nonfloat ()))
  | Types.Tpackage _ -> ops.const (Axis_lattice.of_mod_bounds (ik_base_bounds_nonfloat ()))

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
        (* No manifest: may still be concrete (record/variant/...). Build ckind. *)
        begin match decl.type_kind with
        | Types.Type_abstract _ ->
          let kind : JK.ckind = ckind_of_jkind_l decl.type_jkind in
          JK.Ty { args = decl.type_params; kind; abstract = true }
        | Types.Type_record (lbls, _rep, _umc_opt) ->
          (* Build from components: base (non-float value) + per-label contributions. *)
          let base_lat = 
            if has_mutable_label lbls then
              Axis_lattice.mutable_data
            else
              Axis_lattice.immutable_data in
          let kind : JK.ckind =
            fun (ops : JK.ops) ->
              let base = ops.const base_lat in
              let contribs =
                List.map
                  (fun (lbl : Types.label_declaration) ->
                     let axes =
                       ik_relevant_axes_of_modality ~relevant_for_shallow:`Irrelevant
                         lbl.ld_modalities
                     in
                     let mask = Axis_lattice.of_axis_set axes in
                     ops.modality mask (ops.kind_of lbl.ld_type))
                  lbls
              in
              ops.join (base :: contribs)
          in
          JK.Ty { args = decl.type_params; kind; abstract = false }
        | Types.Type_record_unboxed_product (_lbls, _rep, _umc_opt) ->
          failwith "Type_record_unboxed_product not implemented"
        | Types.Type_variant (_cstrs, _rep, _umc_opt) ->
          failwith "Type_variant not implemented"
        | Types.Type_open ->
          failwith "Type_open not implemented"
        end
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
    ?origin
    ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context)
    (sub : Types.jkind_l)
    (super : Types.jkind_l)
    : (unit, Jkind.Violation.t) result =
  let solver = make_solver ~context in
  let sub_poly = JK.normalize solver (ckind_of_jkind_l sub) in
  let super_poly = JK.normalize solver (ckind_of_jkind_l super) in
  let sub_poly_pp = JK.pp sub_poly in
  let super_poly_pp = JK.pp super_poly in
  let ik_leq = JK.leq solver (ckind_of_jkind_l sub) (ckind_of_jkind_l super) in
  let res = Jkind.sub_jkind_l ?allow_any_crossing ~type_equal ~context sub super in
  let allow_any = match allow_any_crossing with Some true -> true | _ -> false in
  let jk_ok = match res with Ok () -> true | Error _ -> false in
  let axes_info =
    if ik_leq then None
    else (
      (* Compare the IK-normalized lattice values that we also print above, so the
         axes listed match the shown vectors. *)
      let sub_lv = Axis_lattice.decode (JK.round_up solver (ckind_of_jkind_l sub)) in
      let sup_lv = Axis_lattice.decode (JK.round_up solver (ckind_of_jkind_l super)) in
      let names =
        [| "areality"; "linearity"; "uniqueness"; "portability"; "contention";
           "yielding"; "statefulness"; "visibility"; "externality"; "nullability"; "separability" |]
      in
      let parts = ref [] in
      for i = 0 to Array.length names - 1 do
        if sub_lv.(i) > sup_lv.(i) then parts := names.(i) :: !parts
      done;
      let parts = List.rev !parts in
      if parts = [] then None else Some (" axes=[" ^ String.concat "," parts ^ "]"))
  in
  let allow_any_str = if allow_any then " allowAny" else "" in
  let origin_str = match origin with None -> None | Some s -> Some ("[" ^ s ^ "]") in
  let axes_str = match axes_info with None -> "" | Some s -> s in
  let ik_str = if ik_leq then "T" else "F" in
  let jk_str = if jk_ok then "T" else "F" in
  let origin_str =
    match origin_str, !__ikind_origin_tag with
    | None, None -> None
    | Some o, None -> Some o
    | None, Some tag -> Some ("[" ^ tag ^ "]")
    | Some o, Some tag -> Some (o ^ " {" ^ tag ^ "}")
  in
  (match origin_str with None -> () | Some o -> print_endline o);
  (* Print the jkinds first for context *)
  (try
     let jk_sub = Format.asprintf "%a" Jkind.format sub in
     let jk_sup = Format.asprintf "%a" Jkind.format super in
     print_endline (Format.asprintf "  %s <: %s" jk_sub jk_sup)
   with _ -> ());
  (* Print the IK round_up (axes lattice) as well *)
  (try
     let sub_ru_lat = JK.round_up solver (ckind_of_jkind_l sub) in
     let sup_ru_lat = JK.round_up solver (ckind_of_jkind_l super) in
     let sub_ru_pp = Axis_lattice.to_string sub_ru_lat in
     let sup_ru_pp = Axis_lattice.to_string sup_ru_lat in
     print_endline (Format.asprintf "  ik_round_up %s <: %s" sub_ru_pp sup_ru_pp)
   with _ -> ());
  let summary =
    Format.asprintf
      "  %s <: %s ik/jk=%s/%s%s%s"
      sub_poly_pp super_poly_pp ik_str jk_str allow_any_str axes_str
  in
  let is_mismatch = (not ik_leq) || (not jk_ok) in
  if is_mismatch then
    (* Print mismatches in red *)
    print_endline ("\027[31m" ^ summary ^ "\027[0m")
  else
    print_endline summary;
  res
