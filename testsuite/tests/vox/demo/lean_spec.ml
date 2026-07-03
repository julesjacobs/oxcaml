(* TEST
 flags = "-vox-solver lean -vox-prelude spec_lib.lean";
 script = "sh ${test_source_directory}/../has-lean.sh";
 readonly_files = "spec_lib.lean";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Demo 5/7 -- spec functions from a prelude.  Where [@@vox.reflect]
   (demo 4) makes the program its own spec, a [-vox-prelude] file
   defines spec functions on the SOLVER side (spec_lib.lean) -- the
   spelling for specs beyond the reflectable fragment, or that should
   not live in the program.  [mem] returns Prop, which no OCaml
   function can.  Every obligation is really proved.  The recursive
   functions are verified INDUCTIVELY: each recursive call
   re-instantiates the dependent signature at the actual arguments, so
   its refined result is the induction hypothesis. *)

type ilist =
  | Nil
  | Cons of int * ilist

let nil0 : ilist{ len _ = 0 } = refine_ Nil

let l2 : ilist{ len _ = 2 } = refine_ (Cons (1, Cons (2, Nil)))

let has2 : ilist{ mem 2 _ } = refine_ (Cons (1, Cons (2, Nil)))

(* Pushing increments the measure. *)
let push (l : ilist{ len _ = 2 }) : ilist{ len _ = 3 } =
  let refine_ l = l in
  refine_ (Cons (9, l))

(* append: len distributes over it -- the textbook inductive proof. *)
let rec append : (a : ilist) -> (b : ilist) -> ilist{ len _ = len a + len b } =
  fun a b ->
    match a with
    | Nil -> refine_ b
    | Cons (h, t) ->
      let refine_ r = append t b in
      refine_ (Cons (h, r))

(* rev via accumulator: length is preserved. *)
let rec rev_append
  : (acc : ilist) -> (l : ilist) -> ilist{ len _ = len acc + len l }
  =
  fun acc l ->
  match l with
  | Nil -> refine_ acc
  | Cons (h, t) ->
    let refine_ acc2 = (refine_ (Cons (h, acc)) : ilist{ len _ = len acc + 1 }) in
    let refine_ r = rev_append acc2 t in
    refine_ r

let rev : (l : ilist) -> ilist{ len _ = len l } =
  fun l ->
  let refine_ nil = (refine_ Nil : ilist{ _ = Nil }) in
  let refine_ r = rev_append nil l in
  refine_ r
