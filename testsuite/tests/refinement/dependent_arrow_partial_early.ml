(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-c";
 compiler_output = "dependent_arrow_partial_early.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/dependent_arrow_partial_early.reference";
 check-ocamlc.byte-output;
 flags = "-principal -c";
 compiler_output = "dependent_arrow_partial_early.principal.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/dependent_arrow_partial_early.reference";
 check-ocamlc.byte-output;
*)

module Api : sig
  val relation : int @ logical -> int @ logical -> bool @@ total
  val law :
    x:int @ logical ->
    y:int @ logical ->
    unit{ relation x y = true } @@ total
end = struct
  let[@vox.def] relation
      (_x : int @ logical) (_y : int @ logical) =
    true

  let law ~(x : int @ logical) ~(y : int @ logical)
      : unit{ relation x y = true } =
    let _ = relation_def x y in
    ()
end

let before_saturation =
  let _partial = Api.law ~x:11 in
  (Api.relation 11 22 : bool{ _ = true })
