(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing \
          -I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

let source =
  {|
type r = { f : int{ _ > 0 } }
let rec loop () : 'a = loop ()
let bottom () = { f = loop () }
|}

let typed_structure () =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf "imposition_metadata_source.ml";
  let parsed = Parse.implementation lexbuf in
  Compmisc.init_path ();
  let env = Compmisc.initial_env () in
  let structure, _, _, _, _, _ = Typemod.type_structure env parsed in
  structure

let strip_application_metadata structure =
  let retained = ref 0 in
  let mapper =
    { Tast_mapper.default with
      expr =
        (fun self expression ->
          let expression = Tast_mapper.default.expr self expression in
          let exp_extra =
            List.filter
              (fun (extra, _, _) ->
                match extra with
                | Typedtree.Texp_refinement_application _ ->
                  incr retained;
                  false
                | _ -> true)
              expression.Typedtree.exp_extra
          in
          { expression with exp_extra });
    }
  in
  let structure = mapper.structure mapper structure in
  structure, !retained

let () =
  let structure, retained =
    typed_structure () |> strip_application_metadata
  in
  if retained = 0 then failwith "application metadata was not retained";
  Format.printf "retained application metadata: %d@." retained;
  match Vox_verify.verify_structure structure with
  | () -> print_endline "missing metadata: unexpectedly accepted"
  | exception Location.Error report ->
    Format.printf "%a@." Location.print_report report

[%%expect {|
val source : string =
  "\ntype r = { f : int{ _ > 0 } }\nlet rec loop () : 'a = loop ()\nlet bottom () = { f = loop () }\n"
val typed_structure : unit -> Typedtree.structure = <fun>
val strip_application_metadata :
  Typedtree.structure -> Typedtree.structure * int = <fun>
retained application metadata: 1
File "imposition_metadata_source.ml", line 4, characters 22-29:
Error: missing refinement application metadata

|}]
