(* Checks that five e-graph modules call nothing in the proof modules; see
   egraph_boundary.ml. Argument: the directory of their Lambda. *)
open Emitted_code

let () =
  List.iter
    (fun unit ->
      let lambda = read (Filename.concat Sys.argv.(1) (unit ^ ".lambda")) in
      check (occurs "(function" lambda) (unit ^ " was dumped");
      List.iter
        (fun proof ->
          check
            (not
               (occurs
                  ("(apply%s(field_imm%s%d%s(global%sVox_egraph_" ^ proof
                   ^ "!)")
                  lambda))
            (Printf.sprintf "%s calls nothing in Vox_egraph_%s" unit proof))
        [ "derivation"; "match_evidence"; "match_observation";
          "snapshot_proof"; "preservation_proof" ])
    [ "match_subst"; "rule_rewrite"; "rule_saturate"; "model_evidence";
      "rule_handle" ];
  finish ()
