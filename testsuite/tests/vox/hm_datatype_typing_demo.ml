(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_elaboration_check.ml hm_datatype_typing_demo.ml";
 { bytecode; }
*)
open Hm_declarative
let check expected term ty proof =
  if Hm_elaboration_check.check Z Empty_context term ty proof <> expected then
    failwith "unexpected source datatype typing"
let one = Word {Hmc_word64.lo = 1; hi = 0}
let words = Cons (one, Nil)
let words_proof = List_cons (Word64, Word_constant, Empty_list Word64)
let () =
  check true False Boolean Constant;
  check true one Word64 Word_constant;
  check false one Boolean Constant;
  check true Nil (List_type Word64) (Empty_list Word64);
  check true words (List_type Word64) words_proof;
  check false (Cons (Truth, words)) (List_type Word64)
    (List_cons (Word64, Constant, words_proof));
  check true (CaseList (words, one, Bound Z)) Word64
    (List_case (Word64, words_proof, Word_constant, Variable No_arguments));
  check true (CaseList (words, Nil, Bound (S Z))) (List_type Word64)
    (List_case (Word64, words_proof, Empty_list Word64, Variable No_arguments));
  check false (CaseList (words, one, Bound (S Z))) Word64
    (List_case (Word64, words_proof, Word_constant, Variable No_arguments));
  check true (If (False, words, Nil)) (List_type Word64)
    (Conditional (Constant, words_proof, Empty_list Word64));
  check false (If (one, words, Nil)) (List_type Word64)
    (Conditional (Word_constant, words_proof, Empty_list Word64));
  check true (Primitive (Add, one, one)) Word64
    (Word_primitive (Word_constant, Word_constant));
  check true (Primitive (Unsigned_less, one, one)) Boolean
    (Word_primitive (Word_constant, Word_constant));
  check false (Primitive (Equal_word, Truth, one)) Boolean
    (Word_primitive (Constant, Word_constant))
