open Types

type t = Int | Bool | Bigint

let bigint_path name =
  Path.Pdot (Path.Pident (Ident.create_persistent "Stdlib__Bigint"), name)

let classify env ty =
  match get_desc (Ctype.expand_head env ty) with
  | Tconstr (path, [], _) when Path.same path Predef.path_int -> Some Int
  | Tconstr (path, [], _) when Path.same path Predef.path_bool -> Some Bool
  | Tconstr (path, [], _) when Path.same path (bigint_path "t") -> Some Bigint
  | _ -> None

let rec classify_payload env ty =
  match get_desc (Ctype.expand_head env ty) with
  | Tpoly (ty, []) -> classify_payload env ty
  | Trefine refinement -> classify_payload env refinement.ref_payload
  | _ -> classify env ty

(* The C primitives the checker gives a built-in meaning, the compilation
   units whose declarations carry it, and the native name those declarations
   give when it differs from the bytecode name. Any [external] may name any C
   symbol, with any native name, so the meaning belongs to the library's
   declarations rather than to the symbol. Primitives starting with [%] need
   no such check: the compiler implements them whatever the declaration
   says. *)
let builtin_c_primitives =
  let owned ?native units names =
    List.map
      (fun name -> name, (units, Option.value native ~default:name))
      names
  in
  List.concat
    [ owned ["Stdlib__Bigint"]
        [ "caml_bigint_of_int";
          "caml_bigint_to_int_opt";
          "caml_bigint_neg";
          "caml_bigint_add";
          "caml_bigint_sub";
          "caml_bigint_mul";
          "caml_bigint_div";
          "caml_bigint_modulo" ];
      owned ["Stdlib__Map"]
        [ "caml_logical_map_empty";
          "caml_logical_map_find_opt";
          "caml_logical_map_mem";
          "caml_logical_map_add";
          "caml_logical_map_remove";
          "caml_logical_map_cardinal" ];
      owned ["Stdlib__Iarray"] ["caml_array_append"];
      owned ["Vox_table_bits"] ~native:"caml_vox_int_ctz_untagged"
        ["caml_vox_int_ctz"];
      owned ["Vox_iarray"] ["caml_vox_iarray_sub"; "caml_vox_iarray_set"];
      owned ["Vox_sequence"] ["caml_vox_sequence_length"];
      owned ["Pref"; "Ghost_pref"] ~native:"caml_pref_own"
        ["caml_pref_own_bytecode"];
      owned ["Pref"; "Ghost_pref"]
        [ "caml_pref_heap_empty";
          "caml_pref_heap_mem";
          "caml_pref_heap_at";
          "caml_pref_heap_put";
          "caml_pref_heap_union";
          "caml_pref_heap_restrict";
          "caml_pref_heap_exclude";
          "caml_pref_heap_disjoint";
          "caml_pref_heap_same_domain" ];
      owned ["Borrow"; "Borrow_iarray"; "Vox_string_view"]
        [ "caml_borrow_open";
          "caml_borrow_restore";
          "caml_borrow_split";
          "caml_borrow_recombine";
          "caml_borrow_finish";
          "caml_borrow_transfer";
          "caml_borrow_length";
          "caml_borrow_contents";
          "caml_borrow_current";
          "caml_borrow_final";
          "caml_borrow_frame_final";
          "caml_borrow_frame_left";
          "caml_borrow_frame_right" ] ]

let is_builtin_c_primitive (p : Primitive.description) =
  List.mem_assoc p.prim_name builtin_c_primitives

let carries_builtin_meaning uid (p : Primitive.description) =
  match List.assoc_opt p.prim_name builtin_c_primitives with
  | None -> true
  | Some (units, native) -> (
    String.equal (Primitive.native_name p) native
    &&
    match (uid : Shape.Uid.t) with
    | Item { comp_unit; _ } -> List.mem comp_unit units
    | _ -> false)
