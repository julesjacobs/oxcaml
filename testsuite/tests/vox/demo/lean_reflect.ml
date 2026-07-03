(* TEST
 flags = "-vox-solver lean";
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Demo 4/7 -- the program as its own spec.  TOTAL (reflected)
   recursive functions [let rec total_ f ...]: the compiler
   translates the definition into the logic and emits it as a Lean
   [@[grind] def] (Lean checks termination -- structural recursion here
   needs no metric).  A saturated call of a reflected function then
   NAMES ITSELF, and an applied identifier in a predicate denotes it,
   so every obligation below is proved with no [-vox-prelude] at all:
   the program is its own spec library. *)

type ilist =
  | Nil
  | Cons of int * ilist

let rec total_ len l =
  match l with
  | Nil -> 0
  | Cons (_, t) -> 1 + len t

let rec total_ mem x l =
  match l with
  | Nil -> false
  | Cons (h, t) -> x = h || mem x t

(* The [function] form reflects too. *)
let rec total_ depth = function
  | Nil -> 0
  | Cons (_, t) -> 1 + depth t

let nil0 : ilist{ len _ = 0 } = refine_ Nil
let l2 : ilist{ len _ = 2 } = refine_ (Cons (1, Cons (2, Nil)))
let has2 : ilist{ mem 2 _ } = refine_ (Cons (1, Cons (2, Nil)))
let d1 : ilist{ depth _ = 1 } = refine_ (Cons (5, Nil))

(* A program call of a reflected function names itself: the goal is
   [len l = 2] from the unpacked fact [len l = 2]. *)
let len_of (l : ilist{ len _ = 2 }) : int{ _ = 2 } =
  let refine_ l = l in
  refine_ (len l)

(* Exact synthesis composes with reflected calls: [m] gets the fact
   [m = mem 3 l]. *)
let mem3 : (l : ilist) -> bool{ _ = mem 3 l } =
  fun l ->
    let refine_ m = refine_ (mem 3 l) in
    refine_ m

(* The textbook inductive proof, against the REFLECTED len: no prelude
   measure.  Each recursive call re-instantiates the dependent
   signature (the induction hypothesis); grind unfolds [len] via the
   definitional equations. *)
let rec append : (a : ilist) -> (b : ilist) -> ilist{ len _ = len a + len b } =
  fun a b ->
    match a with
    | Nil -> refine_ b
    | Cons (h, t) ->
      let refine_ r = append t b in
      refine_ (Cons (h, r))

(* rev via accumulator, length-preserving, same recipe. *)
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
