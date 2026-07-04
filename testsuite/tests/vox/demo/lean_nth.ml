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
   [unreachable_] turns that into a PROOF OBLIGATION ([false] under
   the arm's path facts -- [len Nil = 0] contradicts [0 <= i < len l])
   rather than a programmer promise. *)

type ilist =
  | Nil
  | Cons of int * ilist

let rec total_ len l =
  match l with
  | Nil -> 0
  | Cons (_, t) -> 1 + len t

(* Each recursive call re-instantiates the declared signature at its
   arguments -- the induction hypothesis, unpacked by the [let] that
   names it. *)
let rec append (a : ilist) (b : ilist) : ilist{ len _ = len a + len b } =
  match a with
  | Nil -> b
  | Cons (h, t) ->
    let r = append t b in
    Cons (h, r)

(* In-bounds access: callers must prove the bounds, and the [Nil] arm
   must prove it is unreachable. *)
let rec nth (l : ilist) (i : int{ 0 <= _ && _ < len l }) : int =
  match l with
  | Nil -> unreachable_
  | Cons (h, t) -> if i = 0 then h else nth t (i - 1)
