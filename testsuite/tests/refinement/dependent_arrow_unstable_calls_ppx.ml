module Mapper = Ast_mapper

let generated_expression loc =
  let parsed =
    Parse.expression
      (Lexing.from_string
         "if next () = 0 then let _ = (next () : int{ _ = 0 }) in () else ()")
  in
  let super = Mapper.default_mapper in
  let rec same_location =
    { super with
      expr =
        (fun mapper expression ->
          let expression = super.expr mapper expression in
          { expression with pexp_loc = loc });
    }
  in
  same_location.expr same_location parsed

let super = Mapper.default_mapper

let expr mapper expression =
  match expression.Parsetree.pexp_desc with
  | Pexp_extension ({ txt = "same_location_calls"; loc }, _) ->
    generated_expression loc
  | _ -> super.expr mapper expression

let () =
  Mapper.register "dependent-arrow-unstable-calls"
    (fun _ -> { super with expr })
