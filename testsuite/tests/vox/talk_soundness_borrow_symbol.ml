(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_sequence.mli vox_sequence.ml borrow.mli borrow.ml";
 readonly_files = "talk_soundness_borrow_symbol.ml";
 {
   setup-ocamlc.opt-build-env;
   run-expect;
   check-program-output;
 }
*)

(* Talk, section 6 ("how we hunt soundness bugs"), row "name-keyed
   built-ins", second half: the borrow transitions. The first half, a user
   external named "caml_bigint_add" "caml_bigint_sub" that proves
   2 + 3 = 5 while native code subtracts, is
   builtin_declaration_identity.ml.

   The forged program below was accepted before the name-keyed built-ins
   fix (commit b5f3e0c50e, "Vox: key built-in meanings of C primitives by
   their declaration", merged in a187b9ab9c). With the fix it is rejected;
   the expected output records the rejection and the accepted control.


   The verifier gave the borrow transitions their meaning by C symbol name,
   so a client external bound to "caml_borrow_finish" with a borrowed,
   non-consuming receiver still produced [final s = current s], before the
   slice was written: a contradiction. From the investigation's
   e1_forged.ml. Verdict only, with an accepted control that uses the
   library's own [Slice.finish]. *)

#load "vox_sequence.cmo";;
#load "borrow.cmo";;

open Borrow;;
[%%expect{|
|}]

external finish_shared : int Slice.t @ local -> unit = "caml_borrow_finish";;
[%%expect{|
external finish_shared : int Borrow.Slice.t @ local -> unit
  = "caml_borrow_finish"
|}]

let unsound (s : int Slice.t @ local unique) =
  let n = Slice.length (borrow_ s) in
  if n > 0 then begin
    let before = ghost_ (Slice.current (borrow_ s)) in
    let x = Slice.get (borrow_ s) 0 in
    ghost_ (Model.length_def before);
    ghost_ (Model.at_def before 0Z);
    ghost_ (Model.set_def before 0Z (x + 1));
    finish_shared (borrow_ s);          (* final s  = current s  *)
    let s1 = Slice.set s 0 (x + 1) in   (* final s1 = final s    *)
    let _ = Slice.finish s1 in          (* final s1 = current s1 *)
    let u = () in
    let _ = (u : {u : unit | false}) in ()
  end;;
[%%expect{|
Line 13, characters 13-14:
13 |     let _ = (u : {u : unit | false}) in ()
                  ^
Error: Refinement could not be proved (counterexample)
Line 13, characters 29-34:
13 |     let _ = (u : {u : unit | false}) in ()
                                  ^^^^^
  The refinement is stated here.
|}]

(* Control: without the forged call, the library's [Slice.finish] resolves
   the prophecy to the written contents, and that is what is proved. *)
let resolved (s : int Slice.t @ local unique) =
  let n = Slice.length (borrow_ s) in
  if n > 0 then begin
    let before = ghost_ (Slice.current (borrow_ s)) in
    let prophecy = ghost_ (Slice.final (borrow_ s)) in
    let x = Slice.get (borrow_ s) 0 in
    ghost_ (Model.length_def before);
    ghost_ (Model.at_def before 0Z);
    let s1 = Slice.set s 0 (x + 1) in
    let _ = Slice.finish s1 in
    let u = () in
    let _ = (u : {u : unit | prophecy === Model.set before 0Z (x + 1)}) in ()
  end;;
[%%expect{|
val resolved : int Borrow.Slice.t @ local unique -> unit = <fun>
|}]
