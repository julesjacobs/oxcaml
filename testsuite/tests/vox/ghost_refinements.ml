(* TEST
 has-z3;
 {
   flags = "-extension refinement_types";
   { expect; }
   { expect.opt; }
 }{
   flags = "-extension refinement_types -principal";
   { expect; }
   { expect.opt; }
 }
*)

module Nonnegative : sig
  val value : int -> int @@ total
  val lemma : (x : int) -> {u : unit | value x >= 0} @@ total
end = struct
  let[@def] value (x : int) = if x < 0 then 0 else x
  let (lemma @ total) (x : int) : {u : unit | value x >= 0} =
    let refine_ equation = value_def x in
    let u = () in refine_ u
end;;
[%%expect{|
module Nonnegative :
  sig
    val value : int -> int @@ total
    val lemma : (x : int) -> {u : unit | (value x) >= 0} @@ total
  end
|}]

module Proofs = struct
  let (identity @ total) (x : int) : {y : int | y === x} =
    let y = x in refine_ y

  let (erased @ total) (x : int) : {y : int | y === x} =
    let refine_ proof = ghost_ (identity x) in
    let y = x in refine_ y

  let (observed @ total) (x : int) : {y : int | y === x} =
    let hidden = ghost_ x in
    let p : {u : unit | hidden === x} =
      let u = () in refine_ u
    in
    let refine_ p = p in
    let y = x in refine_ y
end;;
[%%expect{|
module Proofs :
  sig
    val identity : (x : int) -> {y : int | y === x}
    val erased : (x : int) -> {y : int | y === x}
    val observed : (x : int) -> {y : int | y === x}
  end
|}]

let (proved @ total) x : {y : int | y >= 0} =
  let y = Nonnegative.value x in
  let refine_ proof = ghost_ (Nonnegative.lemma x) in
  refine_ y;;
[%%expect{|
val proved : int @ total -> {y : int | y >= 0} = <fun>
|}]

let missing_proof x : {y : int | y >= 0} =
  let y = Nonnegative.value x in
  refine_ y;;
[%%expect{|
Line 3, characters 2-11:
3 |   refine_ y;;
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let rejected_effect () = ghost_ (print_endline "erased");;
[%%expect{|
Line 1, characters 33-46:
1 | let rejected_effect () = ghost_ (print_endline "erased");;
                                     ^^^^^^^^^^^^^
Error: The value "print_endline" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 1, characters 25-56).
|}]

let rejected_raise () = ghost_ (failwith "erased");;
[%%expect{|
Line 1, characters 32-40:
1 | let rejected_raise () = ghost_ (failwith "erased");;
                                    ^^^^^^^^
Error: The value "failwith" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 1, characters 24-50).
|}]

let rejected_partial () = ghost_ (fun () -> failwith "erased");;
[%%expect{|
Line 1, characters 44-52:
1 | let rejected_partial () = ghost_ (fun () -> failwith "erased");;
                                                ^^^^^^^^
Error: The value "failwith" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 1, characters 26-62).
|}]

let rejected_runtime_predicate x =
  let hidden = ghost_ 42 in
  (assume_ x : {y : int | y === hidden});;
[%%expect{|
Line 3, characters 32-38:
3 |   (assume_ x : {y : int | y === hidden});;
                                    ^^^^^^
Error: This value is "ghost" but is expected to be "real".
|}]

let rejected_runtime_operand () =
  let hidden = ghost_ 42 in
  (assume_ hidden : {y : int | y === 42});;
[%%expect{|
Line 3, characters 11-17:
3 |   (assume_ hidden : {y : int | y === 42});;
               ^^^^^^
Error: This value is "ghost" but is expected to be "real".
|}]

let rejected_ghost_field () =
  let record = { Ghost.ghost = (fun () -> failwith "partial") } in
  ghost_ (record.Ghost.ghost ());;
[%%expect{|
Line 2, characters 42-50:
2 |   let record = { Ghost.ghost = (fun () -> failwith "partial") } in
                                              ^^^^^^^^
Error: The value "failwith" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 31-61
         which is expected to be "total".
|}]

module Ghost_record = struct
  type t = { data : int; proof : int @@ ghost }

  let compare () =
    let left = { data = 0; proof = 1 } in
    let right = { data = 0; proof = 2 } in
    match (assume_ left : {r : t | r === right}) with
    | _ -> false
    | exception Invalid_argument _ -> true
end;;
[%%expect{|
module Ghost_record :
  sig
    type t = { data : int; proof : int @@ ghost; }
    val compare : unit -> bool
  end
|}]

let () = assert (Ghost_record.compare ());;
[%%expect{|
|}]
