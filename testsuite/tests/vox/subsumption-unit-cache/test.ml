(* TEST
 has-z3;
 readonly_files = "impl.ml weak.mli wrong.mli run.sh";
 arguments = "${ocamlrun} ${ocamlc_byte} ${ocamlsrcdir}/stdlib";
 bytecode;
*)

(* The unit verification cache (vox_verify.enabled.ml, unit_cache_file) is
   keyed by the unit's own interface: since refinement subsumption, an
   interface-only change can change the outcome of verification.  run.sh
   compiles the same implementation against a weak interface, a wrong one
   and the weak one again, with the cache enabled; the second compilation
   must not replay the first one's success.  (./dev test does not run
   ocamltest scripts, so this program runs it.) *)

let () =
  let arguments = List.tl (Array.to_list Sys.argv) in
  exit
    (Sys.command
       (Filename.quote_command "sh" ("run.sh" :: Sys.getcwd () :: arguments)))
