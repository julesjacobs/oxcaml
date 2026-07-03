(* TEST
 flags = "-vox-prelude spec_lib.lean";
 script = "sh ${test_source_directory}/../has-lean.sh";
 readonly_files = "spec_lib.lean";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Demo: spec functions from a prelude.  Where [total_]
   (lean_reflect.ml) makes the program its own spec, a [-vox-prelude] file
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

(* No intro forms anywhere below: constructors and variables are
   introduced implicitly at the annotations, binders bind at the
   skeleton with their refinements as facts, and a plain [let] of a
   constructor term contributes its defining equation. *)

let nil0 : ilist{ len _ = 0 } = Nil

let l2 : ilist{ len _ = 2 } = Cons (1, Cons (2, Nil))

let has2 : ilist{ mem 2 _ } = Cons (1, Cons (2, Nil))

(* Pushing increments the measure. *)
let push (l : ilist{ len _ = 2 }) : ilist{ len _ = 3 } = Cons (9, l)

(* append: len distributes over it -- the textbook inductive proof.
   The recursive call's refined result is the induction hypothesis,
   unpacked by the [let] that names it. *)
let rec append : (a : ilist) -> (b : ilist) -> ilist{ len _ = len a + len b } =
  fun a b ->
    match a with
    | Nil -> b
    | Cons (h, t) ->
      let r = append t b in
      Cons (h, r)

(* rev via accumulator: length is preserved.  [acc2]'s binder equation
   [acc2 = Cons (h, acc)] carries the invariant; the recursive call is
   the bare tail, re-proved inline at this call's instantiation. *)
let rec rev_append
  : (acc : ilist) -> (l : ilist) -> ilist{ len _ = len acc + len l }
  =
  fun acc l ->
  match l with
  | Nil -> acc
  | Cons (h, t) ->
    let acc2 = Cons (h, acc) in
    rev_append acc2 t

let rev : (l : ilist) -> ilist{ len _ = len l } =
  fun l ->
  let nil = Nil in
  rev_append nil l
