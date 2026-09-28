(* TEST
 flags = "-extension layouts_beta";
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
   has kind void: no value exists at run time.

   The slot elision is a native-code guarantee: bytecode represents mixed
   records as ordinary blocks and keeps a placeholder word per ghost field
   (exactly as it does for void-typed fields), hence the separate reference
   for the block sizes below. An all-ghost record has kind void in both
   backends: no value exists at run time. *)

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
  let r3 = { a = 5; p = (ghost_ "y"); b = 6 } in
  Printf.printf "a=%d b=%d\n" r3.a r3.b;
  (* functional update over a ghost field *)
  let r4 = { r3 with a = 7 } in
  Printf.printf "a=%d b=%d\n" r4.a r4.b;
  let r5 = { r with p = (print_string "update effect\n"; "z") } in
  Printf.printf "a=%d b=%d\n" r5.a r5.b;
  (* pattern matching binds placeholders without reading *)
  let { a; p = _; b } = r in
  Printf.printf "a=%d b=%d\n" a b
  ;
  (* the record operand of a ghost-field projection in real code is still
     evaluated; the ghost result is bound without being read *)
  let mk_r () = print_string "proj operand\n"; r in
  let _placeholder = (mk_r ()).p in
  ()

(* the all-ghost wrapper has kind void: no value exists at run time *)
type 'a box = { ghost : 'a @@ ghost }
type holder = { id : int; hidden : string box }

let () =
  let b = { ghost = (print_string "wrap effect\n"; "payload") } in
  let b2 = { ghost = (ghost_ "payload") } in
  (* a void-typed field takes no slot, with no modality on the field *)
  let h = { id = 9; hidden = b } in
  Printf.printf "holder size %d\n" (Obj.size (Obj.repr h));
  Printf.printf "id %d\n" h.id;
  (* void parameters vanish from the calling convention *)
  let use (_x : string box) (n : int) = n + 1 in
  Printf.printf "use %d\n" (use b2 1);
  (* projection at a ghost position; the placeholder is never read *)
  let _hidden = ghost_ b.ghost in
  print_string "done\n"

(* Stdlib.Ghost *)
let () =
  let e : int Ghost.t = { ghost = 42 } in
  (* statement position discards the value, so a ghost projection is fine
     there; [ignore] is not, since its parameter is real *)
  (ghost_ (ignore e.Ghost.ghost));
  print_string "stdlib done\n"

type ('a : any) polymorphic = { hidden : 'a @@ ghost; live : int }

let use_float (_ : float# @ ghost) n = n
let read_float (r : float# polymorphic) =
  let { hidden; live } = r in
  use_float hidden live

let () = assert (read_float { hidden = #1.0; live = 42 } = 42)
