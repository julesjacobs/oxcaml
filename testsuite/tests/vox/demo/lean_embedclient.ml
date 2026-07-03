(* TEST
 flags = "-vox-solver lean";
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "lean_embed.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Client of an .mli-LESS spec unit: lean_embed.ml has no interface,
   so its .cmi comes from the implementation, and its embedded blocks
   (and the datatypes they are about) must travel just the same. *)

let five : Lean_embed.ilist{ len _ = 5 } =
  let refine_ l2 = Lean_embed.l2 in
  let refine_ l4 = Lean_embed.append l2 l2 in
  refine_ (Lean_embed.Cons (0, l4))
