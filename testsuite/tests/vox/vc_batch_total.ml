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

(* The evidence is also in scope while a recursive group's OWN right-hand
   sides are walked — a peer may call the group's total binder (below,
   [rg_use] calls [rg_tot]) — and the same rule holds on the local let-rec
   route ([walk_bindings]), exercised inside [local_group].  Pre-fix both
   obligations refused with per-call opaques.  Query bytes: vc-printing.ml's
   rec-group-total and local-rec-group-total blocks. *)

let rec rg_use =
  fun x ->
    let a = rg_tot x in
    let b = rg_tot x in
    (a - b : int{ _ = 0 })
and rg_tot @ total = fun x -> x

let local_group =
  let rec f = fun x -> let a = g x in let b = g x in (a - b : int{ _ = 0 })
  and g @ total = fun x -> x in
  ignore (f 7); 0

(* The recorded-annotation route is item-local: it does not cross module
   boundaries (module-interior totals at Pdot occurrences are refused,
   pinned in vc-z3.ml's module-total-boundary block; the supported route
   is a signature carrying [@@ total]).  The one unsigned-module shape
   that proves is batch-route NONRECURSIVE, via the occurrence mode: *)

module P = struct let p_add @ total = fun x y -> x + y end
let pdot_nonrec_batch : int{ _ = 0 } = P.p_add 1 2 - P.p_add 1 2
