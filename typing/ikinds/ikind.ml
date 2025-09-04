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

(* Optional ambient tag to disambiguate higher-level call sites (e.g. includecore).
   Other modules can bracket calls with [with_origin_tag] to add this suffix. *)
let __ikind_origin_tag : string option ref = ref None

let with_origin_tag (tag : string) (f : unit -> 'a) : 'a =
  let prev = !__ikind_origin_tag in
  __ikind_origin_tag := Some tag;
  Fun.protect ~finally:(fun () -> __ikind_origin_tag := prev) f


let kind_of (ty : Types.type_expr) : JK.ckind =
 fun (ops : JK.ops) ->
  match Types.get_desc ty with
  | Types.Tvar {name=_name; jkind=_jkind} | Types.Tunivar {name=_name; jkind=_jkind} -> ops.rigid ty
  | Types.Tconstr (p, args, _abbrev_memo) ->
    let arg_kinds = List.map (fun t -> ops.kind_of t) args in
    ops.constr p arg_kinds
  | Types.Ttuple elts ->
    (* Boxed tuples: immutable_data base + per-element contributions under id modality. *)
    let base = ops.const Axis_lattice.immutable_data in
    let contribs =
      List.map
        (fun (_lbl, t) ->
           let mask =
             Axis_lattice.mask_of_modality ~relevant_for_shallow:`Irrelevant
               Mode.Modality.Const.id
           in
           ops.modality mask (ops.kind_of t))
        elts
    in
    ops.join (base :: contribs)
  | Types.Tunboxed_tuple elts ->
    (* Unboxed tuples: non-float base + per-element contributions with shallow axes relevant. *)
    let base = ops.const Axis_lattice.nonfloat_value in
    let contribs =
      List.map
        (fun (_lbl, t) ->
           let mask =
             Axis_lattice.mask_of_modality ~relevant_for_shallow:`Relevant
               Mode.Modality.Const.id
           in
           ops.modality mask (ops.kind_of t))
        elts
    in
    ops.join (base :: contribs)
  | Types.Tarrow (_lbl, _t1, _t2, _commu) ->
    (* Arrows use the dedicated per-axis bounds (no with-bounds). *)
    ops.const Axis_lattice.arrow
  | Types.Tlink t -> ops.kind_of t
  | Types.Tsubst _ -> failwith "Tsubst shouldn't appear in kind_of"
  | Types.Tpoly _ -> failwith "Tpoly not yet implemented in kind_of"
  | Types.Tof_kind _ -> failwith "Tof_kind not yet implemented in kind_of"
  | Types.Tobject _ -> failwith "Tobject not yet implemented in kind_of"
  | Types.Tfield _ -> failwith "Tfield not yet implemented in kind_of"
  | Types.Tnil -> failwith "Tnil not yet implemented in kind_of"
  | Types.Tvariant _ -> failwith "Tvariant not yet implemented in kind_of"
  | Types.Tpackage _ -> failwith "Tpackage not yet implemented in kind_of"

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


let has_mutable_label lbls =
  List.exists
    (fun (lbl : Types.label_declaration) ->
      match lbl.ld_mutable with Immutable -> false | Mutable _ -> true)
    lbls
    
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
                     let mask =
                       Axis_lattice.mask_of_modality
                         ~relevant_for_shallow:`Irrelevant
                         lbl.ld_modalities
                     in
                     ops.modality mask (ops.kind_of lbl.ld_type))
                  lbls
              in
              ops.join (base :: contribs)
          in
          JK.Ty { args = decl.type_params; kind; abstract = false }
        | Types.Type_record_unboxed_product (lbls, _rep, _umc_opt) ->
          (* Similar to boxed record for axes: base + per-label contributions. *)
          let base_lat = Axis_lattice.nonfloat_value in
          let kind : JK.ckind =
            fun (ops : JK.ops) ->
              let base = ops.const base_lat in
              let contribs =
                List.map
                  (fun (lbl : Types.label_declaration) ->
                     let mask =
                       Axis_lattice.mask_of_modality
                         ~relevant_for_shallow:`Relevant
                         lbl.ld_modalities
                     in
                     ops.modality mask (ops.kind_of lbl.ld_type))
                  lbls
              in
              ops.join (base :: contribs)
          in
          JK.Ty { args = decl.type_params; kind; abstract = false }
        | Types.Type_variant (cstrs, _rep, _umc_opt) ->
          (* Base: use a non-float value base; per-constructor contributions. *)
          let base_lat = Axis_lattice.nonfloat_value in
          let kind : JK.ckind =
            fun (ops : JK.ops) ->
              let base = ops.const base_lat in
              let contribs =
                List.concat_map
                  (fun (c : Types.constructor_declaration) ->
                     match c.cd_args with
                     | Types.Cstr_tuple args ->
                       List.map
                         (fun (arg : Types.constructor_argument) ->
                           let mask =
                             Axis_lattice.mask_of_modality
                               ~relevant_for_shallow:`Irrelevant
                               arg.ca_modalities
                           in
                           ops.modality mask (ops.kind_of arg.ca_type))
                         args
                     | Types.Cstr_record lbls ->
                       List.map
                         (fun (lbl : Types.label_declaration) ->
                           let mask =
                             Axis_lattice.mask_of_modality
                               ~relevant_for_shallow:`Irrelevant
                               lbl.ld_modalities
                           in
                           ops.modality mask (ops.kind_of lbl.ld_type))
                         lbls)
                  cstrs
              in
              ops.join (base :: contribs)
          in
          JK.Ty { args = decl.type_params; kind; abstract = false }
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
  (* One-shot developer probe when requested via env var. *)
  (let ran = ref false in
   match Sys.getenv_opt "IKIND_POLY_PROBE" with
   | Some v when (v = "1" || String.lowercase_ascii v = "true") && not !ran ->
     ran := true;
     let open Axis_lattice in
     let a = encode ~levels:[| 0;1;0;1;2;1;2;2;1;0;0 |] in
     let b = encode ~levels:[| 2;0;1;0;0;0;0;0;2;0;0 |] in
     let residual = co_sub a b in
     let joined = join b residual in
     Printf.printf "a = %s\n" (to_string a);
     Printf.printf "b = %s\n" (to_string b);
     Printf.printf "co_sub a b = %s\n" (to_string residual);
     Printf.printf "join b (co_sub a b) = %s\n" (to_string joined);
     let module V = struct type t = string let compare = String.compare let to_string s = s end in
     let module P = Ldd.Make (Axis_lattice) (V) in
    let v = P.rigid "<ty#211>" in
    let term = P.meet (P.const a) (P.var v) in
     let base = P.const b in
     let poly = P.join term base in
     Printf.printf "poly = %s\n" (P.pp poly);
     Printf.printf "round_up poly = %s\n" (to_string (P.round_up poly));
     exit 0
   | _ -> ());
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
  let disagreement = (ik_leq && not jk_ok) || ((not ik_leq) && jk_ok) in
  if disagreement then
    (* Only disagreements (IK!=JK) printed in red *)
    print_endline ("\027[31m" ^ summary ^ "\027[0m")
  else
    (* Agreements (T/T) or (F/F) printed normally *)
    print_endline summary;
  res

(* Developer probe: build a lattice polynomial (a ⊓ <ty>) ⊔ b and print
   co_sub results and round_up. Triggered by IKIND_POLY_PROBE env var. *)
let () =
  match Sys.getenv_opt "IKIND_POLY_PROBE" with
  | None -> ()
  | Some v when v = "0" || String.lowercase_ascii v = "false" -> ()
  | Some _ ->
    let open Axis_lattice in
    let a_levels =
      (* [0,1,0,1,2,1,2,2,1,0,0] *)
      [| 0; 1; 0; 1; 2; 1; 2; 2; 1; 0; 0 |]
    in
    let b_levels =
      (* [2,0,1,0,0,0,0,0,2,0,0] *)
      [| 2; 0; 1; 0; 0; 0; 0; 0; 2; 0; 0 |]
    in
    let a = encode ~levels:a_levels in
    let b = encode ~levels:b_levels in
    let residual = co_sub a b in
    let joined = join b residual in
    Printf.printf "a = %s\n" (to_string a);
    Printf.printf "b = %s\n" (to_string b);
    Printf.printf "co_sub a b = %s\n" (to_string residual);
    Printf.printf "join b (co_sub a b) = %s\n" (to_string joined);
    (* Also test a single-axis lattice: [|1|] co_sub [|2|] *)
    let module Single = Product_lattice.Make(struct let axis_sizes = [| 3 |] end) in
    let a1 = Single.encode ~levels:[| 1 |] in
    let b1 = Single.encode ~levels:[| 2 |] in
    let r1 = Single.co_sub a1 b1 in
    let j1 = Single.join b1 r1 in
    Printf.printf "single a1 = %s\n" (Single.to_string a1);
    Printf.printf "single b1 = %s\n" (Single.to_string b1);
    Printf.printf "single co_sub a1 b1 = %s\n" (Single.to_string r1);
    Printf.printf "single join b1 (co_sub a1 b1) = %s\n" (Single.to_string j1);
    (* Scan a few axis shapes to narrow co_sub behavior *)
    let scan shape target =
      let module S = Product_lattice.Make(struct let axis_sizes = Array.of_list shape end) in
      let levels_a = Array.make (List.length shape) 0 in
      let levels_b = Array.make (List.length shape) 0 in
      levels_a.(target) <- 1;
      levels_b.(target) <- (List.nth shape target) - 1;
      let aa = S.encode ~levels:levels_a in
      let bb = S.encode ~levels:levels_b in
      let rr = S.co_sub aa bb in
      let jj = S.join bb rr in
      Printf.printf "shape %s target=%d\n"
        (String.concat "," (List.map string_of_int shape)) target;
      Printf.printf "  a=%s b=%s\n" (S.to_string aa) (S.to_string bb);
      Printf.printf "  co_sub=%s join=%s\n" (S.to_string rr) (S.to_string jj)
    in
    scan [3] 0;
    scan [3;3] 1;
    scan [3;2] 0;
    scan [3;2;3] 2;
    (* Full Axis_lattice shape, externality is index 8 *)
    scan [3;2;2;2;3;2;3;3;3;2;3] 8;
    (* Build polynomial: (a ⊓ <ty#211>) ⊔ b *)
    let module V = struct
      type t = string
      let compare = String.compare
      let to_string s = s
    end in
    let module P = Ldd.Make (Axis_lattice) (V) in
    let v = P.rigid "<ty#211>" in
    let term = P.meet (P.const a) (P.var v) in
    let base = P.const b in
    let poly = P.join term base in
    Printf.printf "poly = %s\n" (P.pp poly);
    Printf.printf "round_up poly = %s\n" (to_string (P.round_up poly));
    exit 0
