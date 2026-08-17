(* TEST
 {
   reference = "${test_source_directory}/erasure_fields_runtime.byte.reference";
   bytecode;
 }{
   reference = "${test_source_directory}/erasure_fields_runtime.reference";
   native;
 }
*)

(* Runtime behaviour of erased record fields: the field occupies no slot,
   construction evaluates retained field expressions for their effects only,
   erased_ expressions in fields are not evaluated, and an all-erased record
   is the immediate 0.

   The slot elision is a native-code guarantee: bytecode represents mixed
   records as ordinary blocks and keeps a placeholder word per erased field
   (exactly as it does for void-typed fields), hence the separate reference
   for the block size below. The all-erased record is the immediate 0 in
   both backends. *)

type r = { a : int; p : string @@ erased; b : int }

let () =
  (* the erased field occupies no slot: the block has two words *)
  let r = { a = 1; p = "gone"; b = 2 } in
  Printf.printf "size %d\n" (Obj.size (Obj.repr r));
  Printf.printf "a=%d b=%d\n" r.a r.b;
  (* a retained field expression is evaluated for its effects, then dropped *)
  let r2 = { a = 3; p = (print_string "field effect\n"; "x"); b = 4 } in
  Printf.printf "a=%d b=%d\n" r2.a r2.b;
  (* an erased_ field expression is never evaluated *)
  let r3 = { a = 5; p = (erased_ (print_string "DELETED\n"; "y")); b = 6 } in
  Printf.printf "a=%d b=%d\n" r3.a r3.b;
  (* functional update over an erased field *)
  let r4 = { r3 with a = 7 } in
  Printf.printf "a=%d b=%d\n" r4.a r4.b;
  let r5 = { r with p = (print_string "update effect\n"; "z") } in
  Printf.printf "a=%d b=%d\n" r5.a r5.b;
  (* pattern matching binds placeholders without reading *)
  let { a; p = _; b } = r in
  Printf.printf "a=%d b=%d\n" a b

(* the all-erased wrapper is the immediate 0 *)
type 'a box = { erased : 'a @@ erased }

let () =
  let b = { erased = (print_string "wrap effect\n"; "payload") } in
  Printf.printf "is_int %b\n" (Obj.is_int (Obj.repr b));
  let b2 = { erased = (erased_ (failwith "never")) } in
  Printf.printf "is_int %b\n" (Obj.is_int (Obj.repr b2));
  (* wrappers are ordinary values: they go in data structures *)
  let l = [ b; b2 ] in
  Printf.printf "len %d\n" (List.length l);
  (* projection at an erased position; the placeholder is never read *)
  let _hidden = erased_ (String.length b.erased) in
  print_string "done\n"

(* Stdlib.Erased *)
let () =
  let e : int Erased.t = { erased = 42 } in
  Printf.printf "stdlib is_int %b\n" (Obj.is_int (Obj.repr e));
  (* statement position discards the value, so an erased projection is fine
     there; [ignore] is not, since its parameter is retained *)
  (erased_ (ignore e.Erased.erased));
  print_string "stdlib done\n"
