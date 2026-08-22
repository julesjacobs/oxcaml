(* TEST
 readonly_files = "has_z3.sh";
 script = "sh ${test_source_directory}/has_z3.sh";
 script;
 flags = "-noassert -vox-backend z3";
 expect;
*)

(* The -noassert gate on assert path conditions (design-docs/
   vc-generation.md, path conditions): translcore erases [assert e] under
   -noassert, so a fact from the erased test would be unsound — the shape
   that is Proved in vc-z3.ml (assert-fact) must refuse here.  Syntactic
   [assert false] is the ungated positive control: translcore keeps it
   raising under -noassert, so its fact survives and proves the
   unreachable tail's goal — and its Proved shows the pass itself still
   runs under the flag. *)

let noassert_gated (c : int) : int =
  assert (c > 0);
  (c : int{ _ > 0 });;
[%%expect{|
Line 3, characters 3-4:
3 |   (c : int{ _ > 0 });;
       ^
Error: This refinement obligation could not be verified (prove query: sat; disprove query: sat).
Line 3, characters 3-4:
3 |   (c : int{ _ > 0 });;
       ^
Error: 1 refinement obligation was not verified.
|}]

let noassert_false (c : int) : int =
  assert false;
  (c : int{ _ > 0 });;
[%%expect{|
Line 2, characters 2-14:
2 |   assert false;
      ^^^^^^^^^^^^
Warning 21 [nonreturning-statement]: this statement never returns (or has an unsound type.)

val noassert_false : int -> int = <fun>
|}]
