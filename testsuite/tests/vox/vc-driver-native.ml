(* TEST
 flags = "-vox-backend nonsense";
 toplevel.opt;
*)

(* The native toplevel's half of the driver glue: ocamlnat types phrases
   itself ([Typemod.type_toplevel_phrase] in opttoploop.ml) and never goes
   through [Topcommon.typecheck_phrase], where the bytecode toplevel runs
   [Vox_verify.run_if_enabled].  The invalid backend name must fail at
   selection exactly as vc-driver-nonsense.ml pins for the bytecode
   toplevel; a phrase that is ACCEPTED below means ocamlnat never
   consulted [-vox-backend] at all, and a [-vox-backend z3] session there
   would silently verify nothing. *)

let f x = x;;
