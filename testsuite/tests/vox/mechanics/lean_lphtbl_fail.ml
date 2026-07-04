(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/pslice.mli ../lib/pslice.ml ../lib/lphtbl.mli ../lib/lphtbl.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Linear-probing soundness probe, pinned at the LEAN layer: after an
   in-place insert the keys array's ghost is the model insert
   [pinsk], NOT the old contents -- the stale claim below is refuted
   (the model witnesses a table where the write changed slot
   [home k]).  The probe-loop and borrow-discipline rejections are
   pinned by lean_htbl_mut_fail; this one pins the MODEL layer of
   the probing table. *)

open Pslice
open Lphtbl

let stale : unit -> unit =
  fun () ->
    let (ks, vs) = create () in
    let (ks1, vs1) = add 3 7 ks vs in
    let ks2 = (ks1 : int varr{ pcts _ = pcts ks }) in
    ignore ks2;
    ignore vs1
