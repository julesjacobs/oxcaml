(* TEST
   include stdlib_stable;
   flags = "-w -220";
   expect;
*)

(* Positive submoding and total primitive calls. *)

let increment @ total = fun x -> x + 1
let use_partial (f @ partial) = f 41
let total_at_partial = use_partial increment

let ghost_constant @ logic = 42
let program_at_logic @ logic = 42

let add_one @ total = fun x -> x + 1
let add_two @ total = fun x -> add_one (add_one x)
let expects_total (f @ total) = f
let nonrecursive_f_at_total @ total =
  let rec f = fun x -> x in
  (f : _ @ total)
let total_with_unused_recursion @ total =
  fun () -> let rec _loop () = _loop () in 0
let recursive_nonoccurrence @ total =
  fun () -> let rec f = fun x -> x in f 42
(* A later milestone restricts this construct syntactically; modes deliberately
   do not catch it. *)
let cyclic_list_crosses @ total =
  fun () -> let rec xs = 1 :: xs in (xs : _ @ total)

let takes_logic @ total = fun (x @ logic) -> x
let program_argument_at_logic @ logic = takes_logic 42
[%%expect{|
val increment : int -> int = <fun>
val use_partial : (int -> 'a) -> 'a = <fun>
val total_at_partial : int = 42
val ghost_constant : int @@ logic = 42
val program_at_logic : int @@ logic = 42
val add_one : int -> int = <fun>
val add_two : int -> int = <fun>
val expects_total : 'a @ total -> 'a = <fun>
val nonrecursive_f_at_total : 'a -> 'a = <fun>
val total_with_unused_recursion : unit -> int = <fun>
val recursive_nonoccurrence : unit -> int = <fun>
val cyclic_list_crosses : unit -> int list = <fun>
val takes_logic : 'a @ logic -> 'a @ logic = <fun>
val program_argument_at_logic : int @@ logic = 42
|}]

(* Logic values cannot flow back into program positions. *)

let logic_value @ logic = 42
let _ @ program = logic_value
[%%expect{|
val logic_value : int @@ logic = 42
Line 2, characters 18-29:
2 | let _ @ program = logic_value
                      ^^^^^^^^^^^
Error: This value is "logic" but is expected to be "program".
|}]

let takes_program (x @ program) = x
let _ = takes_program logic_value
[%%expect{|
val takes_program : 'a -> 'a = <fun>
Line 2, characters 22-33:
2 | let _ = takes_program logic_value
                          ^^^^^^^^^^^
Error: This value is "logic" but is expected to be "program".
|}]

type program_record = { program_field : int }
let _ @ program = { program_field = logic_value }
[%%expect{|
type program_record = { program_field : int; }
Line 2, characters 36-47:
2 | let _ @ program = { program_field = logic_value }
                                        ^^^^^^^^^^^
Error: This value is "logic"
       but is expected to be "program"
         because it is the field "program_field" of the record at line 2, characters 18-49
         which is expected to be "program".
|}]

(* A closure that captures logic data is itself logic. *)

let captured_logic @ logic = 42
let logic_closure = fun () -> captured_logic
let _ @ program = logic_closure
[%%expect{|
val captured_logic : int @@ logic = 42
val logic_closure : unit -> int @ logic @@ logic = <fun>
Line 3, characters 18-31:
3 | let _ @ program = logic_closure
                      ^^^^^^^^^^^^^
Error: This value is "logic" but is expected to be "program".
|}]

(* A total closure cannot capture or call a partial function. *)

let partial_identity x = x
let _ @ total = fun x -> partial_identity x
[%%expect{|
val partial_identity : 'a -> 'a = <fun>
Line 2, characters 25-41:
2 | let _ @ total = fun x -> partial_identity x
                             ^^^^^^^^^^^^^^^^
Error: The value "partial_identity" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 16-43
         which is expected to be "total".
|}]

(* Recursive bindings are partial within the recursive right-hand sides. *)

let rec f @ total = fun x -> f x
[%%expect{|
Line 1, characters 29-30:
1 | let rec f @ total = fun x -> f x
                                 ^
Error: The value "f" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 20-32
         which is expected to be "total".
|}]

let _ =
  let rec f = fun x -> f x in
  expects_total f
[%%expect{|
Line 3, characters 16-17:
3 |   expects_total f
                    ^
Error: This value is "partial"
         because it closes over the value "f" at line 2, characters 23-24
         which is "partial".
       However, the highlighted expression is expected to be "total".
|}]

let rec f = fun x -> f x
let _ = expects_total f
[%%expect{|
val f : 'a -> 'b = <fun>
Line 2, characters 22-23:
2 | let _ = expects_total f
                          ^
Error: This value is "partial" but is expected to be "total".
|}]

let rec recursive_inside x = expects_total recursive_inside x
[%%expect{|
Line 1, characters 43-59:
1 | let rec recursive_inside x = expects_total recursive_inside x
                                               ^^^^^^^^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

let rec recursive_after x = recursive_after x
let _ = expects_total recursive_after
[%%expect{|
val recursive_after : 'a -> 'b = <fun>
Line 2, characters 22-37:
2 | let _ = expects_total recursive_after
                          ^^^^^^^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

let rec recursive_value = 1
let _ = expects_total recursive_value
[%%expect{|
val recursive_value : int = 1
- : int = 1
|}]

let rec ops = ((fun x -> (fst ops) x), 0)
let _ = expects_total (fst ops)
[%%expect{|
val ops : ('a -> 'b) * int = (<fun>, 0)
Line 2, characters 22-31:
2 | let _ = expects_total (fst ops)
                          ^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

let rec even_inside n =
  if n = 0 then true else expects_total odd_inside (n - 1)
and odd_inside n =
  if n = 0 then false else even_inside (n - 1)
[%%expect{|
Line 2, characters 40-50:
2 |   if n = 0 then true else expects_total odd_inside (n - 1)
                                            ^^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

let rec even_inside_odd n =
  if n = 0 then true else odd_inside_odd (n - 1)
and odd_inside_odd n =
  if n = 0 then false else expects_total even_inside_odd (n - 1)
[%%expect{|
Line 4, characters 41-56:
4 |   if n = 0 then false else expects_total even_inside_odd (n - 1)
                                             ^^^^^^^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

let rec even = fun n ->
  if n = 0 then true else odd (n - 1)
and odd = fun n ->
  if n = 0 then false else even (n - 1)
[%%expect{|
val even : int -> bool = <fun>
val odd : int -> bool = <fun>
|}]

let _ = expects_total even
[%%expect{|
Line 1, characters 22-26:
1 | let _ = expects_total even
                          ^^^^
Error: This value is "partial" but is expected to be "total".
|}]

let _ = expects_total odd
[%%expect{|
Line 1, characters 22-25:
1 | let _ = expects_total odd
                          ^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* Hand-written total-context restrictions. *)

let _ @ total = fun x -> ref x
[%%expect{|
Line 1, characters 25-28:
1 | let _ @ total = fun x -> ref x
                             ^^^
Error: This expression is not allowed at mode total: mutable state is not permitted in total code.
|}]

let cell = ref 0
let _ @ total = fun () -> !cell
[%%expect{|
val cell : int ref = {contents = 0}
Line 2, characters 26-27:
2 | let _ @ total = fun () -> !cell
                              ^
Error: This expression is not allowed at mode total: mutable state is not permitted in total code.
|}]

let _ @ total = fun () -> cell := 1
[%%expect{|
Line 1, characters 31-33:
1 | let _ @ total = fun () -> cell := 1
                                   ^^
Error: This expression is not allowed at mode total: mutable state is not permitted in total code.
|}]

let array = [| 0 |]
let _ @ total = fun () -> Array.set array 0 1
[%%expect{|
val array : int array = [|0|]
Line 2, characters 26-35:
2 | let _ @ total = fun () -> Array.set array 0 1
                              ^^^^^^^^^
Error: This expression is not allowed at mode total: mutable state is not permitted in total code.
|}]

let _ @ total = fun () -> Array.get array 0
[%%expect{|
Line 1, characters 26-35:
1 | let _ @ total = fun () -> Array.get array 0
                              ^^^^^^^^^
Error: This expression is not allowed at mode total: mutable state is not permitted in total code.
|}]

let _ @ total = fun () -> raise Exit
[%%expect{|
Line 1, characters 26-31:
1 | let _ @ total = fun () -> raise Exit
                              ^^^^^
Error: This expression is not allowed at mode total: exceptions are not permitted in total code.
|}]

let _ @ total = fun () -> try 0 with _ -> 1
[%%expect{|
Line 1, characters 26-43:
1 | let _ @ total = fun () -> try 0 with _ -> 1
                              ^^^^^^^^^^^^^^^^^
Error: This expression is not allowed at mode total: exceptions are not permitted in total code.
|}]

let _ @ total = fun () -> print_string "not total"
[%%expect{|
Line 1, characters 26-38:
1 | let _ @ total = fun () -> print_string "not total"
                              ^^^^^^^^^^^^
Error: The value "print_string" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 16-50
         which is expected to be "total".
|}]

let _ @ total = fun () -> lazy 0
[%%expect{|
Line 1, characters 26-32:
1 | let _ @ total = fun () -> lazy 0
                              ^^^^^^
Error: This expression is not allowed at mode total: lazy evaluation is not permitted in total code.
|}]

let delayed = lazy 0
let _ @ total = fun () -> Lazy.force delayed
[%%expect{|
val delayed : int lazy_t = lazy 0
Line 2, characters 26-36:
2 | let _ @ total = fun () -> Lazy.force delayed
                              ^^^^^^^^^^
Error: This expression is not allowed at mode total: lazy evaluation is not permitted in total code.
|}]

type mutable_record = { mutable mutable_field : int }

let _ @ total = fun x -> { mutable_field = x }
[%%expect{|
type mutable_record = { mutable mutable_field : int; }
Line 3, characters 25-46:
3 | let _ @ total = fun x -> { mutable_field = x }
                             ^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed at mode total: mutable state is not permitted in total code.
|}]

let _ @ total = fun record -> record.mutable_field
[%%expect{|
Line 1, characters 30-50:
1 | let _ @ total = fun record -> record.mutable_field
                                  ^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed at mode total: mutable state is not permitted in total code.
|}]

let _ @ total = fun record -> record.mutable_field <- 1
[%%expect{|
Line 1, characters 30-55:
1 | let _ @ total = fun record -> record.mutable_field <- 1
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed at mode total: mutable state is not permitted in total code.
|}]

let _ @ total = fun x -> assert (x = 0)
[%%expect{|
Line 1, characters 25-39:
1 | let _ @ total = fun x -> assert (x = 0)
                             ^^^^^^^^^^^^^^
Error: This expression is not allowed at mode total: exceptions are not permitted in total code.
|}]

let _ @ total =
  fun () ->
    let exception Local_exception in
    0
[%%expect{|
Lines 3-4, characters 4-5:
3 | ....let exception Local_exception in
4 |     0
Error: This expression is not allowed at mode total: exceptions are not permitted in total code.
|}]

let _ @ total = fun () -> while false do () done
[%%expect{|
Line 1, characters 26-48:
1 | let _ @ total = fun () -> while false do () done
                              ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed at mode total: loops are not permitted in total code.
|}]

let _ @ total = fun () -> for _ = 0 to 1 do () done
[%%expect{|
Line 1, characters 26-51:
1 | let _ @ total = fun () -> for _ = 0 to 1 do () done
                              ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed at mode total: loops are not permitted in total code.
|}]

(* Pure primitive totality survives aliasing, and ordinary pure functions may
   be used in logic expressions. *)

let plus @ total = ( + )
let add_with_alias @ total = fun x y -> plus x y
let compare_int @ total = fun (x : int) -> x = x
let logic_list_length @ logic = List.length [1; 2; 3]
[%%expect{|
val plus : int -> int -> int = <fun>
val add_with_alias : int -> int -> int = <fun>
val compare_int : int -> bool = <fun>
val logic_list_length : int @@ logic = 3
|}]

(* Total-context restrictions apply in every subexpression position. *)

let _ @ total =
  fun () -> if (try true with _ -> false) then 1 else 0
[%%expect{|
Line 2, characters 15-41:
2 |   fun () -> if (try true with _ -> false) then 1 else 0
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed at mode total: exceptions are not permitted in total code.
|}]

let logic_cell = ref 0
let _ @ logic = if !logic_cell = 0 then 1 else 2
[%%expect{|
val logic_cell : int ref = {contents = 0}
Line 2, characters 19-20:
2 | let _ @ logic = if !logic_cell = 0 then 1 else 2
                       ^
Error: This expression is not allowed at mode logic: mutable state is not permitted in logic code.
|}]

let _ @ logic = (print_string "hi"; 42)
[%%expect{|
Line 1, characters 17-29:
1 | let _ @ logic = (print_string "hi"; 42)
                     ^^^^^^^^^^^^
Error: This expression is not allowed at mode logic: this primitive or external call is not on the pure primitive allowlist.
|}]

let _ @ logic = match (try Some 1 with _ -> None) with
  | Some x -> x
  | None -> 0
[%%expect{|
Line 1, characters 22-49:
1 | let _ @ logic = match (try Some 1 with _ -> None) with
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed at mode logic: exceptions are not permitted in logic code.
|}]

let _ @ logic =
  let _cell = ref 0 in
  42
[%%expect{|
Line 2, characters 14-17:
2 |   let _cell = ref 0 in
                  ^^^
Error: This expression is not allowed at mode logic: mutable state is not permitted in logic code.
|}]

let identity @ total = fun x -> x
let _ @ logic = identity (ref 0)
[%%expect{|
val identity : 'a -> 'a = <fun>
Line 2, characters 26-29:
2 | let _ @ logic = identity (ref 0)
                              ^^^
Error: This expression is not allowed at mode logic: mutable state is not permitted in logic code.
|}]

(* Polymorphic comparisons are total only for known-immediate operands. *)

let _ @ total = fun (x : int) (y : int) -> x = y
[%%expect{|
- : int -> int -> bool = <fun>
|}]

let _ @ total = fun f -> f = f
[%%expect{|
Line 1, characters 27-28:
1 | let _ @ total = fun f -> f = f
                               ^
Error: This expression is not allowed at mode total: this primitive or external call is not on the pure primitive allowlist.
|}]
