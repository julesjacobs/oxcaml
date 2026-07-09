(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "reflectbits.mli reflectbits.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Client of a library that declares a reflected primitive: no flag, no
   local block.  [Reflectbits.imin]'s [@@vox.reflect "bmin"] binding and
   the [bmin] model both arrive through Reflectbits's .cmi / VoxSig
   module.  A qualified call and a qualified predicate mention both
   reflect to [bmin], so this client verifies against the SAME model
   Reflectbits was checked with. *)

(* Qualified call in a checked position + qualified predicate mention. *)
let m : int{ _ = Reflectbits.imin 3 5 } = Reflectbits.imin 3 5

(* Uses the imported law [bmin_idem]: imin x x = x. *)
let idem (x : int) : int{ _ = x } = Reflectbits.imin x x

(* total_ COMPOSITION across units: [dmin]'s reflected body calls the
   imported reflected [imin].  Imported blocks precede this unit's
   reflected definitions in the solver input, so [dmin]'s defining
   equation ([dmin x y = bmin x y]) elaborates against the imported
   [bmin].  [dmin] then names itself in a refinement. *)
let total_ dmin (x : int) (y : int) = Reflectbits.imin x y

let dmin_idem (x : int) : int{ _ = x } = dmin x x
