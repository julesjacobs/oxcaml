(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "lean_totalsig.mli lean_totalsig.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Client of a total-parameter interface: the [@vox.total] marker on
   [apply_step]'s relation rides Lean_totalsig's .cmi, so this reflectable
   call-site lambda is accepted and its concrete relation flows -- the
   restriction is enforced across the unit boundary yet usable. *)

[@@@warning "-6-32"]

let good (x : int) : int{ x <= _ } =
  Lean_totalsig.apply_step (fun p q -> p <= q) (fun a -> a + 1) x
