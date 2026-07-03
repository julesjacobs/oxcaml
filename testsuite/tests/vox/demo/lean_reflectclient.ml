(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "reflect_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Reflected functions ACROSS MODULES: Reflect_lib's total_ [len] and
   [fib] carry their marker in val_attributes and their definitions in
   the .cmi's spec export, so a client names their calls, applies them
   in predicates (qualified or via open), and even reflects its own
   functions in terms of them -- with no prelude anywhere. *)

(* Qualified: predicate and program call. *)
let l2 : Reflect_lib.ilist{ Reflect_lib.len _ = 2 } =
  refine_ (Reflect_lib.Cons (1, Reflect_lib.Cons (2, Reflect_lib.Nil)))

let f5 : int{ _ = Reflect_lib.fib 5 } = refine_ (Reflect_lib.fib 5)

open Reflect_lib

(* Unqualified after [open]: the applied identifier resolves to the
   same imported definition. *)
let l1 : ilist{ len _ = 1 } = refine_ (Cons (7, Nil))

(* A client's own reflected function may call an imported one: its
   definition is emitted after the imported spec blocks. *)
let total_ fib2 n = fib n + fib n

let d3 : int{ _ = fib2 3 } = refine_ (fib2 3)

let fib2_spec : (n : int) -> int{ _ = fib2 n } = fun n -> refine_ (fib2 n)

(* The textbook inductive proof, against the IMPORTED len. *)
let rec append : (a : ilist) -> (b : ilist) -> ilist{ len _ = len a + len b } =
  fun a b ->
    match a with
    | Nil -> refine_ b
    | Cons (h, t) ->
      let refine_ r = append t b in
      refine_ (Cons (h, r))

(* And against the imported fib: the loop invariant (parameter
   contracts) crosses the module boundary. *)
let rec fib_loop
  : (n : int) -> (i : int) -> (a : int{ _ = fib i && i >= 0 })
    -> (b : int{ _ = fib (i + 1) }) -> int{ _ = fib n }
  =
  fun n i a b ->
    if i = n
    then refine_ a
    else begin
      let refine_ j = refine_ (i + 1) in
      let refine_ r = fib_loop n j b (a + b) in
      refine_ r
    end
