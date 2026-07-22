(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

open Types

let value_description (type_ : Types.type_expr) =
  { val_type = type_;
    val_modalities = Mode.Modality.undefined;
    val_kind =
      Val_reg Jkind.Sort.(of_const Const.for_continuation);
    val_lpoly = Lpoly.determined [];
    val_loc = Location.none;
    val_zero_alloc = Zero_alloc.default;
    val_attributes = [];
    val_uid = Uid.internal_not_actually_unique;
  }

let free_x__2 = Ident.create_local "x__2"

let env =
  Compmisc.init_path ();
  Compmisc.initial_env ()
  |> Env.add_value ~mode:Mode.Value.legacy free_x__2
       (value_description Predef.type_int)
  |> Env.add_refinement_stable_values [free_x__2]

let translate source =
  let parsed = Parse.core_type (Lexing.from_string source) in
  let typed =
    Typetexp.transl_simple_type env
      ~new_var_jkind:Typetexp.Sort
      ~closed:false
      Mode.Alloc.Const.legacy
      parsed
  in
  typed.Typedtree.ctyp_type

let rename_both_to_x type_ =
  match get_desc type_ with
  | Tarrow
      ( (label1, argument_mode1, return_mode1, Some outer),
        domain1,
        codomain1,
        commutable1 ) ->
    let renamed_outer = Ident.create_local "x" in
    let codomain1 =
      Vox_dependent.rename
        ~binder:outer ~as_:renamed_outer codomain1
    in
    begin match get_desc codomain1 with
    | Tarrow
        ( (label2, argument_mode2, return_mode2, Some inner),
          domain2,
          result,
          commutable2 ) ->
      let renamed_inner = Ident.create_local "x" in
      let result =
        Vox_dependent.rename
          ~binder:inner ~as_:renamed_inner result
      in
      let inner_arrow =
        Btype.newgenty
          (Tarrow
             ( (label2, argument_mode2, return_mode2, Some renamed_inner),
               domain2,
               result,
               commutable2 ))
      in
      Btype.newgenty
        (Tarrow
           ( (label1, argument_mode1, return_mode1, Some renamed_outer),
             domain1,
             inner_arrow,
             commutable1 ))
    | _ -> failwith "expected inner dependent arrow"
    end
  | _ -> failwith "expected outer dependent arrow"

let rename_outer_to name type_ =
  match get_desc type_ with
  | Tarrow
      ( (label, argument_mode, return_mode, Some binder),
        domain,
        codomain,
        commutable ) ->
    let renamed = Ident.create_local name in
    let codomain =
      Vox_dependent.rename ~binder ~as_:renamed codomain
    in
    Btype.newgenty
      (Tarrow
         ( (label, argument_mode, return_mode, Some renamed),
           domain,
           codomain,
           commutable ))
  | _ -> failwith "expected dependent arrow"

let nested_binders_and_refinement type_ =
  match get_desc type_ with
  | Tarrow ((_, _, _, Some outer), _, inner_arrow, _) ->
    begin match get_desc inner_arrow with
    | Tarrow ((_, _, _, Some inner), _, result, _) ->
      begin match get_desc result with
      | Trefine refinement -> outer, inner, refinement
      | _ -> failwith "expected refined result"
      end
    | _ -> failwith "expected inner dependent arrow"
    end
  | _ -> failwith "expected outer dependent arrow"

let predicate_operand_binders refinement =
  match refinement.ref_pred.rexp_desc with
  | Rexp_apply
      ( _,
        [ Nolabel, { rexp_desc = Rexp_ident (Rbound view); _ };
          ( Nolabel,
            { rexp_desc =
                Rexp_apply
                  ( _,
                    [ Nolabel,
                      { rexp_desc = Rexp_ident (Rbound outer); _ };
                      Nolabel,
                      { rexp_desc = Rexp_ident (Rbound inner); _ };
                    ] );
              _ } );
        ] ) ->
    view, outer, inner
  | _ -> failwith "unexpected equality/addition predicate"

let print type_ =
  Printtyp.wrap_printing_env ~error:false env (fun () ->
    Format.asprintf "%a" Printtyp.type_expr type_)

let alpha_print_observations =
  ignore Vox_lean.render_predicate;
  let original =
    translate
      "(outer : int) -> (inner : int) -> int{ _ = outer + inner }"
    |> rename_both_to_x
  in
  let printed = print original in
  let reparsed = translate printed in
  let outer, inner, refinement = nested_binders_and_refinement reparsed in
  let view_occurrence, outer_occurrence, inner_occurrence =
    predicate_operand_binders refinement
  in
  assert (Ident.same view_occurrence refinement.ref_view.rb_id);
  assert (Ident.same outer_occurrence outer);
  assert (Ident.same inner_occurrence inner);
  assert (not (Ident.same outer inner));
  assert (Ctype.is_equal env false [original] [reparsed]);
  let keyword =
    translate "(outer : int) -> int{ _ = outer }"
    |> rename_outer_to "let"
  in
  let keyword_printed = print keyword in
  let keyword_reparsed = translate keyword_printed in
  assert (Ctype.is_equal env false [keyword] [keyword_reparsed]);
  let higher_order =
    translate "(function_ : (int -> int)) -> int{ _ = function_ 0 }"
  in
  let higher_order_printed = print higher_order in
  let higher_order_reparsed = translate higher_order_printed in
  assert (Ctype.is_equal env false [higher_order] [higher_order_reparsed]);
  let renamed_higher_order = rename_outer_to "x" higher_order in
  let renamed_higher_order_printed = print renamed_higher_order in
  let renamed_higher_order_reparsed =
    translate renamed_higher_order_printed
  in
  assert
    (Ctype.is_equal env false
       [renamed_higher_order] [renamed_higher_order_reparsed]);
  let free_name_collision =
    translate
      "(outer : int) -> (inner : int) -> int{ _ = outer + inner + x__2 }"
    |> rename_both_to_x
  in
  let free_name_collision_printed = print free_name_collision in
  let free_name_collision_reparsed = translate free_name_collision_printed in
  assert
    (Ctype.is_equal env false
       [free_name_collision] [free_name_collision_reparsed]);
  [ "printed: " ^ printed;
    "round trip: alpha-equivalent";
    "keyword-safe: " ^ keyword_printed;
    "higher-order: " ^ higher_order_printed;
    "renamed higher-order: " ^ renamed_higher_order_printed;
    "free-name collision: " ^ free_name_collision_printed;
  ]

[%%expect {|
val value_description : Types.type_expr -> Types.value_description = <fun>
val free_x__2 : Ident.t = <abstr>
val env : Env.t = <abstr>
val translate : string -> Types.type_expr = <fun>
val rename_both_to_x : Types.type_expr -> Types.type_expr = <fun>
val rename_outer_to : string -> Types.type_expr -> Types.type_expr = <fun>
val nested_binders_and_refinement :
  Types.type_expr -> Ident.t * Ident.t * Types.refinement_desc = <fun>
val predicate_operand_binders :
  Types.refinement_desc -> Ident.t * Ident.t * Ident.t = <fun>
val print : Types.type_expr -> string = <fun>
val alpha_print_observations : string list =
  ["printed: (x : int) -> (x__2 : int) -> int{ _ = x + x__2 }";
   "round trip: alpha-equivalent";
   "keyword-safe: (value : int) -> int{ _ = value }";
   "higher-order: (function_ : (int -> int)) -> int{ _ = function_ 0 }";
   "renamed higher-order: (x : (int -> int)) -> int{ _ = x 0 }";
   "free-name collision: (x : int) -> (x__3 : int) -> int{ _ = x + x__3 + x__2 }"]
|}]
