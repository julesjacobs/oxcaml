(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml";
 prebuilt_modules += " vox_iarray.mli vox_iarray.ml vox_string_view.mli vox_string_view.ml";
 readonly_files = "talk_soundness_shift_read.ml";
 {
   setup-ocamlc.opt-build-env;
   run-expect;
   check-program-output;
 }
*)

(* Talk, section 6 ("how we hunt soundness bugs"), row "out-of-range
   shifts": a verified string read that segfaulted natively (the
   investigation's oob.ml). The verifier used to model a shift by a count
   outside [0, 63] as one uninterpreted function, so it equated [1 lsl n]
   and [1 lsl 64] when [n = 64]; native code constant-folds [1 lsl 64]
   differently from the run-time shift, the index was then far out of
   bounds, and the unchecked read crashed. Fixed on trunk
   (jujacobs/vox/shift-encoding-20260927): a shift's count must be proved
   in range. Verdict only; the plain equation [(1 lsl n) = (1 lsl 64)] is
   in int_shift_range.ml. *)

#load "vox_sequence.cmo";;
#load "vox_int_sequence.cmo";;
#load "vox_iarray.cmo";;
#load "vox_string_view.cmo";;

(* The reproducer: rejected, because the count 64 is out of range. *)
let read (s : string) (n : {n : int | n = 64}) : char =
  let i = ((1 lsl n) - (1 lsl 64)) * 100_000_000 in
  if Vox_string_view.length s = 1 then Vox_string_view.get s i else '?';;
[%%expect{|
Line 2, characters 30-32:
2 |   let i = ((1 lsl n) - (1 lsl 64)) * 100_000_000 in
                                  ^^
Error: Refinement could not be proved (counterexample: n = 64)
Line 2, characters 23-33:
2 |   let i = ((1 lsl n) - (1 lsl 64)) * 100_000_000 in
                           ^^^^^^^^^^
  A shift count must be between 0 and 63: outside that range OCaml leaves the result unspecified.
|}]

(* The accepted control: the same read with a count in range. *)
let read_in_range (s : string) (n : {n : int | n = 62}) : char =
  let i = ((1 lsl n) - (1 lsl 62)) * 100_000_000 in
  if Vox_string_view.length s = 1 then Vox_string_view.get s i else '?';;
[%%expect{|
val read_in_range : string -> {n : int | n = 62} -> char = <fun>
|}]

let c = read_in_range "a" 62;;
[%%expect{|
val c : char = 'a'
|}]
