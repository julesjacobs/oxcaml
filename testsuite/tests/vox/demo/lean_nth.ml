(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Demo: the page's lead example.  An ordinary datatype and ordinary
   recursive functions, in the direct spelling
   [let rec f (a : t) (b : t) : r = ...] (the compiler hoists the
   annotations into the dependent arrow [(a : t) -> (b : t) -> r],
   which is the one spelling dependency has).  [append] is the
   textbook inductive proof with no proof text; [nth] is SAFE -- no
   option, no default value, no exception: the bounds ride the second
   parameter as a contract, so the [Nil] arm is dead code, and
   calling [unreachable_] there turns that into a PROOF OBLIGATION
   ([false] under the arm's path facts -- [len Nil = 0] contradicts
   [0 <= i < len l]) rather than a programmer promise.  [unreachable_]
   is not a primitive but an ordinary library function whose argument
   type [unit{ false }] is uninhabited; its diverging body needs no
   trust keyword (the recursive call's [false] obligation is
   discharged from the [false] hypothesis). *)

type ilist =
  | Nil
  | Cons of int * ilist

let rec total_ len l =
  match l with
  | Nil -> 0
  | Cons (_, t) -> 1 + len t

(* Each obligation is a Lean theorem, proved automatically by grind;
   the file contains no proof text. *)
let rec append (a : ilist) (b : ilist) : ilist{ len _ = len a + len b } =
  match a with
  | Nil -> b  (* obligation: len b = len Nil + len b *)
  | Cons (h, t) ->
    let r = append t b in  (* induction hypothesis: len r = len t + len b *)
    (* obligation: len (Cons (h, r)) = len (Cons (h, t)) + len b *)
    Cons (h, r)

(* Not a primitive: the argument type is uninhabited (see header). *)
let rec unreachable_ (u : unit{ false }) : 'a = unreachable_ u

let rec nth (l : ilist) (i : int{ 0 <= _ && _ < len l }) : int =
  match l with
  | Nil -> unreachable_ ()   (* dead code by the precondition *)
  | Cons (h, t) -> if i = 0 then h else nth t (i - 1)
