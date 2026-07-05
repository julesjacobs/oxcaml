(* Demo: the program as its own spec.  TOTAL (reflected)
   recursive functions [let rec total_ f ...]: the compiler
   translates the definition into the logic and emits it as a Lean
   [@[grind] def] (Lean checks termination -- structural recursion here
   needs no metric).  A saturated call of a reflected function then
   NAMES ITSELF, and an applied identifier in a predicate denotes it,
   so every obligation below is proved with no [-vox-prelude] at all:
   the program is its own spec library.  No intro or elim forms
   anywhere. *)

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

let nil0 : ilist{ len _ = 0 } = Nil
let l2 : ilist{ len _ = 2 } = Cons (1, Cons (2, Nil))
let has2 : ilist{ mem 2 _ } = Cons (1, Cons (2, Nil))
let d1 : ilist{ depth _ = 1 } = Cons (5, Nil)

(* A program call of a reflected function names itself: the goal is
   [len l = 2] from the binder fact [len l = 2]. *)
let len_of (l : ilist{ len _ = 2 }) : int{ _ = 2 } = len l

(* A reflected call in checking position is definitionally exact. *)
let mem3 (l : ilist) : bool{ _ = mem 3 l } = mem 3 l

(* The textbook inductive proof, against the REFLECTED len: no prelude
   measure.  Each recursive call re-instantiates the dependent
   signature -- the induction hypothesis, unpacked by the [let] that
   names it; grind unfolds [len] via the definitional equations. *)
let rec append (a : ilist) (b : ilist) : ilist{ len _ = len a + len b } =
  match a with
  | Nil -> b
  | Cons (h, t) ->
    let r = append t b in
    Cons (h, r)

(* rev via accumulator, length-preserving, same recipe: [acc2]'s
   binder equation carries the invariant, and the recursive call is
   the bare tail, re-proved inline at this call's instantiation. *)
let rec rev_append (acc : ilist) (l : ilist) : ilist{ len _ = len acc + len l }
  =
  match l with
  | Nil -> acc
  | Cons (h, t) ->
    let acc2 = Cons (h, acc) in
    rev_append acc2 t

let rev (l : ilist) : ilist{ len _ = len l } =
  let nil = Nil in
  rev_append nil l
