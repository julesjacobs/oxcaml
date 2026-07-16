(* TEST
 include ocamlcommon;
*)

open Parsetree

let parse source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf "refinement_surface.ml";
  Parse.implementation lexbuf

let print structure = Format.asprintf "%a" Pprintast.structure structure

let contains string substring =
  let substring_length = String.length substring in
  let rec loop index =
    index + substring_length <= String.length string
    && (String.sub string index substring_length = substring
        || loop (index + 1))
  in
  loop 0

let remove_locations =
  let open Ast_mapper in
  { default_mapper with location = (fun _ _ -> Location.none) }

let without_locations structure =
  remove_locations.Ast_mapper.structure remove_locations structure

let source =
  {|
type positive = int{ _ > 0 }

type nested = ((int * int){ let (x, _) = _ in x > 0 }) list

type rich = int{
  let value = { contents = 1 } in
  match value.contents with
  | n when n > 0 -> (try _ = n with _ -> false)
  | _ -> false
}

let refine_ value = value
let assume_ = refine_ 1
let assume_unchecked_ = refine_ 2
|}

let extension_names = ref []
let holes = ref 0

let iterator =
  let open Ast_iterator in
  let super = default_iterator in
  { super with
    typ =
      (fun self typ ->
         (match typ.ptyp_desc with
          | Ptyp_extension ({ txt; _ }, _) ->
            extension_names := txt :: !extension_names
          | _ -> ());
         super.typ self typ);
    expr =
      (fun self expr ->
         (match expr.pexp_desc with
          | Pexp_extension ({ txt; _ }, _) ->
            extension_names := txt :: !extension_names
          | Pexp_hole -> incr holes
          | _ -> ());
         super.expr self expr);
  }

let () =
  let parsed = parse source in
  iterator.Ast_iterator.structure iterator parsed;
  let printed = print parsed in
  if not (contains printed "int{ _ > 0 }")
     || contains printed "[%vox2.refinement"
  then
    failwith "Pprintast did not preserve the refinement surface syntax";
  let reparsed = parse printed in
  if without_locations parsed <> without_locations reparsed then
    failwith "parse -> print -> parse changed the refinement parsetree";
  List.rev !extension_names |> List.iter print_endline;
  Printf.printf "refined-value holes: %d\n" !holes;
  print_endline "surface printer: postfix braces";
  print_endline "former intro names: ordinary identifiers";
  print_endline "round-trip: stable"

let malformed source =
  match parse source with
  | _ -> print_endline "unexpectedly accepted"
  | exception Syntaxerr.Error (Syntaxerr.Expecting (_, description)) ->
    Printf.printf "expecting %s\n" description
  | exception Syntaxerr.Error (Syntaxerr.Unclosed (_, left, _, right)) ->
    Printf.printf "unclosed %s (expected %s)\n" left right
  | exception exn ->
    Printf.printf "unexpected error: %s\n" (Printexc.to_string exn)

let () =
  malformed "type bad = int{}";
  malformed "type bad = int{ _ > 0"
