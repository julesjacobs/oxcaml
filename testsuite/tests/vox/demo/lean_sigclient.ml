(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "lean_sig.mli lean_sig.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Client of a specced module: no -vox-prelude flag and no local
   blocks -- [len]'s definition arrives through Lean_sig's .cmi, so
   this client can never verify against a DIFFERENT version of the
   spec than the one Lean_sig was verified with. *)

(* Prove a refinement of our own against the imported spec... *)
let mine : Lean_sig.ilist{ len _ = 2 } =
  Lean_sig.Cons (5, Lean_sig.Cons (6, Lean_sig.Nil))

(* ...consume the module's contract: [mine] is a module-level
   package, unpacked by a plain local [let] and passed BARE -- the
   application discharges [len m = 2] from the binder fact... *)
let longer : Lean_sig.ilist{ len _ = 3 } =
  let m = mine in
  Lean_sig.push m

(* ...and do a real proof mixing the contract with a constructor. *)
let four : Lean_sig.ilist{ len _ = 4 } =
  let t = Lean_sig.two in
  let l = Lean_sig.push t in
  Lean_sig.Cons (0, l)
