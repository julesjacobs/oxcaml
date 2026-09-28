module B = Wasm_u32
module D = Hm_declarative
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module Guard = Hmc_wasm_allocation_guard
module T = Wasm_control
module I = Wasm_instruction
module Select = Hmc_wasm_allocation_select
module Advance = Hmc_wasm_allocation_advance
type fixture = {cursor : B.u32; limit : B.u32; bytes : B.u32; accepted : bool; guard : Wasm_code.t; advance : Wasm_code.t option; selection : Wasm_code.t}
let rec fuel n = if n = 0 then Wasm_code.Zero else Wasm_code.Succ (fuel (n - 1))
let fixture (cursor : B.u32) (limit : B.u32) (bytes : B.u32) =
  let locals = S.Push (S.I32 cursor, S.Push (S.I32 limit, S.Empty)) in
  let state = {X.memory = B.Byte (42, B.End); machine = {E.locals; stack = S.Empty}} in
  ghost_ (Wasm_locals.get_def locals 0; Wasm_locals.get_def locals 1;
    Wasm_locals.get_def (S.Push (S.I32 limit, S.Empty)) 0;
    Guard.correct bytes 0 1 cursor limit state ());
  let accepted = cursor + bytes <= limit in
  let guard = Guard.emit bytes 0 1 in
  (match X.run guard state with
  | X.Done after -> if after.X.memory <> state.X.memory || after.X.machine.E.locals <> locals
      || after.X.machine.E.stack <> S.Push (S.I32 (S.boolean accepted), S.Empty) then failwith "allocation guard result"
  | _ -> failwith "allocation guard trap");
  let advance = if accepted then (
    let updated = Advance.correct bytes 0 cursor limit state () in
    let code = Advance.emit bytes 0 in
    match X.run code state with
    | X.Done after -> if after.X.memory <> state.X.memory || after.X.machine.E.locals <> updated || after.X.machine.E.stack <> S.Empty then failwith "allocation advance state" else Some code
    | _ -> failwith "allocation advance trap") else None in
  let success = Wasm_control_lift.embed (Advance.emit bytes 0) T.Empty in
  let exhausted = T.Empty in
  let tail = T.Instruction (I.Local_get 0, T.Empty) in
  ghost_ (Select.correct bytes 0 1 cursor limit state T.No_labels success exhausted tail ());
  let selection = Select.emit bytes 0 1 success exhausted tail in
  (match T.run (fuel 32) {T.code = selection; labels = T.No_labels; state} with
  | T.Finished after ->
    if after.X.memory <> state.X.memory || after.X.machine.E.stack <> S.Push (S.I32 (if accepted then cursor + bytes else cursor), S.Empty)
      then failwith "allocation branch result"
  | _ -> failwith "allocation branch did not finish");
  {cursor; limit; bytes; accepted; guard; advance; selection = T.flatten selection Wasm_code.Empty}
let cells_fixture (number : Hmc_wasm_reservation.count) (cursor : B.u32) (limit : B.u32) =
  if number > 4 then failwith "fixture cell count" else
  let cells = Hmc_wasm_control_fixture.index number in
  let reserved = Hmc_wasm_reservation.reserve cells number cursor limit () in
  let fixture = fixture cursor limit (16 * number) in
  (match reserved with
  | None -> if fixture.accepted then failwith "source exhausted but guard accepted"
  | Some stop -> if not fixture.accepted || stop <> cursor + fixture.bytes then failwith "source reservation differs");
  fixture
let fixtures () =
  [cells_fixture 0 0 0; cells_fixture 0 1 0;
   cells_fixture 1 64 80; cells_fixture 1 64 79;
   cells_fixture 2 64 96; cells_fixture 2 64 95;
   cells_fixture 3 64 112; cells_fixture 3 64 111;
   cells_fixture 4 0 64; cells_fixture 4 0 63;
   cells_fixture 1 4294967279 4294967295;
   cells_fixture 1 4294967280 4294967295;
   cells_fixture 1 4294967295 4294967295;
   cells_fixture 0 4294967295 4294967295;
   fixture 0 4294967295 4294967295;
   fixture 1 4294967295 4294967295;
   fixture 4294967295 4294967295 4294967295;
   fixture 2147483648 4294967295 2147483648]
