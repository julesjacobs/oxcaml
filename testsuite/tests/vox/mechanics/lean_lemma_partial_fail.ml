(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* SOUNDNESS: a lemma whose OCaml recursion covers only the [Cons] arm
   verifies its body (the [Nil] "arm" here just loops), but the claim
   [len l >= 5] is false at [Nil].  The re-proof runs [induction] over
   ALL constructors, so the missing [Nil] case has no proof and grind
   fails: the partial-match false lemma is rejected. *)

type ilist =
  | Nil
  | Cons of int * ilist

let rec total_ len (l : ilist) : int =
  match l with
  | Nil -> 0
  | Cons (_, t) -> 1 + len t

let rec lemma_partial (l : ilist) : unit{ len l >= 5 } =
  match l with
  | Cons (_, t) -> lemma_partial t
  | Nil -> lemma_partial l
[@@vox.lemma]
