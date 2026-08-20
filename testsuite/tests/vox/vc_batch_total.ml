(* TEST
 readonly_files = "has_z3.sh";
 script = "sh ${test_source_directory}/has_z3.sh";
 script;
 setup-ocamlc.byte-build-env;
 flags = "-extension let_mutable -vox-backend z3";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Batch structures thread total-binder evidence across items: a recursive
   [let rec f @ total]'s occurrences cannot read Total from their mode (the
   annotation caps the checking mode without pinning a rec binder's mode
   variable), and the batch compiler — unlike the toplevel, which pins
   bindings at phrase end — leaves it unpinned at walk time, so without the
   threaded [Texp_mode] evidence both calls below lowered to per-node
   opaques (result/1 - result/2) and the unit was refused with two
   Unknowns.  The expect route cannot show this (the toplevel pinning
   proves it either way), hence this ocamlc action; the query bytes — one
   shared Call symbol — are pinned in vc-printing.ml's rec-total block.
   Compiling silently with exit 0 IS the Proved verdict. *)

let rec batch_add @ total = fun x y -> x + y
let batch_total : int{ _ = 0 } = batch_add 1 2 - batch_add 1 2
