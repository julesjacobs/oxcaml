(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* vox: LEMMAS AS FUNCTIONS.  An ordinary recursive function whose
   refined result is a PROPOSITION over its parameters is a proof by
   induction -- the recursive call's refined result is the induction
   hypothesis.  [@@vox.lemma] EXPORTS that proposition as an ambient
   grind fact [forall params, contracts -> Q]: it is emitted as a Lean
   theorem, RE-PROVED by structural / functional induction + grind (so
   soundness is Lean's -- see the *_fail companions), and registered
   with a [grind_pattern] so it fires wherever its spec functions
   appear, with NO explicit call at the use site. *)

type ilist =
  | Nil
  | Cons of int * ilist

let rec total_ len (l : ilist) : int =
  match l with
  | Nil -> 0
  | Cons (_, t) -> 1 + len t

(* Structural lemma: [len] is nonnegative.  Proved by recursion on the
   list; the [Cons] arm's recursive call is the induction hypothesis. *)
let rec lemma_len_nonneg (l : ilist) : unit{ len l >= 0 } =
  match l with
  | Nil -> ()
  | Cons (_, t) -> lemma_len_nonneg t
[@@vox.lemma]

(* The fact is AMBIENT: this client never calls the lemma, yet
   [len l >= 0] discharges its obligation. *)
let use_nonneg (l : ilist) : int{ _ >= 0 } = refine_ (len l)

let rec total_ app (a : ilist) (b : ilist) : ilist =
  match a with
  | Nil -> b
  | Cons (h, t) -> Cons (h, app t b)

(* Associativity of append, proved by recursion on the first list. *)
let rec lemma_app_assoc (a : ilist) (b : ilist) (c : ilist)
  : unit{ app (app a b) c = app a (app b c) } =
  match a with
  | Nil -> ()
  | Cons (_, t) -> lemma_app_assoc t b c
[@@vox.lemma]

let use_assoc (a : ilist) (b : ilist) (c : ilist)
  : ilist{ _ = app a (app b c) } =
  refine_ (app (app a b) c)

(* Int-indexed lemma about a reflected function: [dbl n = 2 * n] for
   [n >= 0].  Re-proved by [fun_induction dbl], borrowing [dbl]'s own
   (termination-checked) induction principle. *)
let rec total_ dbl (n : int) : int =
  if n <= 0 then 0 else 2 + dbl (n - 1)
[@@vox.decreases n]

let rec lemma_dbl (n : int{ 0 <= _ }) : unit{ dbl n = 2 * n } =
  if n <= 0 then () else lemma_dbl (n - 1)
[@@vox.lemma]

let use_dbl (n : int{ 0 <= _ }) : int{ _ = 2 * n } = refine_ (dbl n)
