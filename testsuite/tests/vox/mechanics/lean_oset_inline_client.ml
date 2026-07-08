(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/oset_inline.mli ../lib/oset_inline.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Cross-unit client of a set whose interface uses the INLINE
   [refines ([%lean "MyT"])] form (oset_inline.mli, no ghost-sort
   intermediary type).  The client sorts [t] at the block-declared
   [MyT] read from the imported artifact and the imported law [mem_ins]
   fires -- identical to the two-line ghost-sort spelling. *)

open Oset_inline

let after : (x : int) -> (s : t) -> t{ _ = ins x s } = fun x s -> add x s
