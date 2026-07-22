(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-c";
 compiler_output = "dependent_arrow_compound_wrong.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/dependent_arrow_compound_wrong.reference";
 check-ocamlc.byte-output;
 flags = "-principal -c";
 compiler_output = "dependent_arrow_compound_wrong.principal.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/dependent_arrow_compound_wrong.reference";
 check-ocamlc.byte-output;
*)

module Api : sig
  val relation : int @ logical -> int @ logical -> bool @@ total
  val plus_one : int @ logical -> int @@ total
  val law :
    x:int @ logical ->
    y:int @ logical ->
    unit{ relation x y = true } @@ total
end = struct
  let[@vox.def] relation
      (_x : int @ logical) (_y : int @ logical) =
    true

  let plus_one (x : int @ logical) = x + 1

  let law ~(x : int @ logical) ~(y : int @ logical)
      : unit{ relation x y = true } =
    let _ = relation_def x y in
    ()
end

let compound_wrong =
  let () = Api.law ~x:(Api.plus_one 10) ~y:22 in
  (Api.relation (Api.plus_one 11) 22 : bool{ _ = true })
