(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

open Types

let env =
  Compmisc.init_path ();
  Compmisc.initial_env ()

let translate source =
  let parsed = Parse.core_type (Lexing.from_string source) in
  (Typetexp.transl_simple_type env
     ~new_var_jkind:Typetexp.Sort
     ~closed:false
     Mode.Alloc.Const.legacy
     parsed).Typedtree.ctyp_type

let duplicate_inner_binder type_ =
  match get_desc type_ with
  | Tarrow
      ( outer_description,
        outer_domain,
        inner_arrow,
        outer_commutable ) ->
    let _, _, _, outer = outer_description in
    begin match outer, get_desc inner_arrow with
    | Some outer,
      Tarrow
        ( (label, argument_mode, return_mode, Some inner),
          inner_domain,
          result,
          inner_commutable ) ->
      let result =
        Vox_dependent.rename ~binder:inner ~as_:outer result
      in
      let inner_arrow =
        Btype.newgenty
          (Tarrow
             ( (label, argument_mode, return_mode, Some outer),
               inner_domain,
               result,
               inner_commutable ))
      in
      Btype.newgenty
        (Tarrow
           ( outer_description,
             outer_domain,
             inner_arrow,
             outer_commutable ))
    | _ -> failwith "expected two dependent arrows"
    end
  | _ -> failwith "expected outer dependent arrow"

let escaped_codomain type_ =
  match get_desc type_ with
  | Tarrow (_, _, codomain, _) -> codomain
  | _ -> failwith "expected dependent arrow"

let report name = function
  | Ok () -> name ^ ": valid"
  | Error binder -> name ^ ": rejected " ^ Ident.name binder

let scope_observations =
  let nested =
    translate
      "(outer : int) -> (inner : int) -> int{ _ = outer + inner }"
  in
  let arrow = translate "(x : int) -> int{ _ = x }" in
  [ report "well-scoped" (Vox_dependent.validate_scopes nested);
    report "duplicate"
      (Vox_dependent.validate_scopes (duplicate_inner_binder nested));
    report "escaped"
      (Vox_dependent.validate_scopes (escaped_codomain arrow));
  ]

[%%expect {|
val env : Env.t = <abstr>
val translate : string -> Types.type_expr = <fun>
val duplicate_inner_binder : Types.type_expr -> Types.type_expr = <fun>
val escaped_codomain : Types.type_expr -> Types.type_expr = <fun>
val report : string -> (unit, Ident.t) result -> string = <fun>
val scope_observations : string list =
  ["well-scoped: valid"; "duplicate: rejected outer"; "escaped: rejected x"]
|}]
