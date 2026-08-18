(* TEST
 {
   reference = "${test_source_directory}/ghost_fields_runtime.byte.reference";
   bytecode;
 }{
   reference = "${test_source_directory}/ghost_fields_runtime.reference";
   native;
 }
*)

(* Runtime behaviour of ghost record fields: the field occupies no slot,
   construction evaluates real field expressions for their effects only,
   ghost_ expressions in fields are not evaluated, and an all-ghost record
   is the immediate 0.

   The slot elision is a native-code guarantee: bytecode represents mixed
   records as ordinary blocks and keeps a placeholder word per ghost field
   (exactly as it does for void-typed fields), hence the separate reference
   for the block size below. The all-ghost record is the immediate 0 in
   both backends. *)

type r = { a : int; p : string @@ ghost; b : int }

let () =
  (* the ghost field occupies no slot: the block has two words *)
  let r = { a = 1; p = "gone"; b = 2 } in
  Printf.printf "size %d\n" (Obj.size (Obj.repr r));
  Printf.printf "a=%d b=%d\n" r.a r.b;
  (* a real field expression is evaluated for its effects, then dropped *)
  let r2 = { a = 3; p = (print_string "field effect\n"; "x"); b = 4 } in
  Printf.printf "a=%d b=%d\n" r2.a r2.b;
  (* an ghost_ field expression is never evaluated *)
  let r3 = { a = 5; p = (ghost_ (print_string "DELETED\n"; "y")); b = 6 } in
  Printf.printf "a=%d b=%d\n" r3.a r3.b;
  (* functional update over a ghost field *)
  let r4 = { r3 with a = 7 } in
  Printf.printf "a=%d b=%d\n" r4.a r4.b;
  let r5 = { r with p = (print_string "update effect\n"; "z") } in
  Printf.printf "a=%d b=%d\n" r5.a r5.b;
  (* pattern matching binds placeholders without reading *)
  let { a; p = _; b } = r in
  Printf.printf "a=%d b=%d\n" a b

(* the all-ghost wrapper is the immediate 0 *)
type 'a box = { ghost : 'a @@ ghost }

let () =
  let b = { ghost = (print_string "wrap effect\n"; "payload") } in
  Printf.printf "is_int %b\n" (Obj.is_int (Obj.repr b));
  let b2 = { ghost = (ghost_ (failwith "never")) } in
  Printf.printf "is_int %b\n" (Obj.is_int (Obj.repr b2));
  (* wrappers are ordinary values: they go in data structures *)
  let l = [ b; b2 ] in
  Printf.printf "len %d\n" (List.length l);
  (* projection at a ghost position; the placeholder is never read *)
  let _hidden = ghost_ (String.length b.ghost) in
  print_string "done\n"

(* Stdlib.Ghost *)
let () =
  let e : int Ghost.t = { ghost = 42 } in
  Printf.printf "stdlib is_int %b\n" (Obj.is_int (Obj.repr e));
  (* statement position discards the value, so a ghost projection is fine
     there; [ignore] is not, since its parameter is real *)
  (ghost_ (ignore e.Ghost.ghost));
  print_string "stdlib done\n"
