(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* vox: [@@vox.lemma] v2 -- lemmas whose recursion carries an
   ACCUMULATOR, which the v1 tactic re-proof (blind [induction] /
   [fun_induction]) cannot discharge: [induction] on the recursion
   variable fixes the accumulator, giving the wrong induction
   hypothesis, and there is no reflected function with the lemma's
   recursion to borrow.  v2 translates the OCaml body into a genuine
   Lean recursive proof, so the recursive call's IH arrives at the
   EXACT accumulator the body used ([Cons (h, acc)], [acc + 1]).  Both
   the datatype-accumulator and the int-accumulator lemma export as
   ambient facts and discharge their clients with no explicit call. *)

type ilist =
  | Nil
  | Cons of int * ilist

let rec total_ len (l : ilist) : int =
  match l with
  | Nil -> 0
  | Cons (_, t) -> 1 + len t

(* Datatype accumulator: recursion on the SECOND argument [l], IH used
   at the changed accumulator [Cons (h, acc)]. *)
let rec total_ rev_append (acc : ilist) (l : ilist) : ilist =
  match l with
  | Nil -> acc
  | Cons (h, t) -> rev_append (Cons (h, acc)) t

let rec lemma_len_rev (acc : ilist) (l : ilist)
  : unit{ len (rev_append acc l) = len acc + len l } =
  match l with
  | Nil -> ()
  | Cons (h, t) ->
    let a2 = Cons (h, acc) in
    lemma_len_rev a2 t
[@@vox.lemma]

let use_len_rev (a : ilist) (b : ilist) : int{ _ = len a + len b } =
  refine_ (len (rev_append a b))

(* Int accumulator: recursion on [l], IH used at [acc + 1]. *)
let rec total_ lenacc (acc : int) (l : ilist) : int =
  match l with
  | Nil -> acc
  | Cons (_, t) -> lenacc (acc + 1) t

let rec lemma_lenacc (acc : int) (l : ilist)
  : unit{ lenacc acc l = acc + len l } =
  match l with
  | Nil -> ()
  | Cons (_, t) -> lemma_lenacc (acc + 1) t
[@@vox.lemma]

let use_lenacc (a : int) (l : ilist) : int{ _ = a + len l } =
  refine_ (lenacc a l)
