module B = Wasm_u32
module W = Hmc_word64
module V = Hmc_tagged_cell
module H = Hmc_heap_objects
module Wire = Hmc_heap_wire
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module Read = Wasm_frame_snapshot
module Write = Wasm_frame_write
module Moves = Wasm_frame_moves
module F = Hmc_wasm_frame_fixture
let word (lo : W.limb) : W.t = {W.lo; hi = 0}
type fixture = {memory : B.bytes; base : B.u32; first : W.t; second : W.t;
  code : Wasm_code.t; expected : (B.u32 * W.t) list}
let expected_memory (base : B.u32) (pc : W.limb) accumulator first second =
  let cells = H.Cell (V.Closure_pointer 64, H.Cell (V.Word accumulator,
    H.Cell (V.Word first, H.Cell (V.Word second, H.Empty)))) in
  F.prefix base (Wire.encode (Wire.Closure (pc, cells)) (B.Byte (42, B.End)))
let move (base : B.u32) writes expected_first expected_second =
  let source = F.env_fixture base in
  let reads = Read.Read (56, 1, Read.Read (72, 2, Read.End)) in
  let locals = S.Push (S.I32 base, S.Push (S.I64 (word 0), S.Push (S.I64 (word 0), S.Empty))) in
  let state = {X.memory = source.F.memory; machine = {E.locals; stack = S.Push (S.I32 91, S.Empty)}} in
  ghost_ (Wasm_locals.get_def locals 0);
  if Read.separate reads 0 then
    match Read.project reads state.X.memory base locals with
    | None -> failwith "move reads"
    | Some snapshot -> (match Write.apply writes state.X.memory base snapshot with
      | None -> failwith "move writes"
      | Some memory ->
        ghost_ (Moves.correct reads writes 0 state base snapshot memory ());
        let code = Moves.emit reads writes 0 in
        (match X.run code state with
        | X.Done after ->
          if after.X.memory <> expected_memory base 7 (word 42) expected_first expected_second
            || after.X.machine.E.stack <> state.X.machine.E.stack then failwith "source environment move"
        | _ -> failwith "move execution");
        {memory = source.F.memory; base; first = word 0; second = word 0; code;
          expected = [56, expected_first; 72, expected_second]})
  else failwith "move base alias"
let update (base : B.u32) =
  let source = F.env_fixture base in
  let first = word 11 in
  let second = {W.lo = 305419896; hi = 2882400001} in
  let locals = S.Push (S.I32 base, S.Push (S.I64 first, S.Push (S.I64 second, S.Empty))) in
  let state = {X.memory = source.F.memory; machine = {E.locals; stack = S.Push (S.I32 91, S.Empty)}} in
  let writes = Write.Write (8, 1, Write.Write (40, 2, Write.End)) in
  ghost_ (Wasm_locals.get_def locals 0);
  match Write.apply writes state.X.memory base locals with
  | None -> failwith "frame update"
  | Some memory ->
    ghost_ (Write.correct writes 0 state base memory ());
    let code = Write.emit writes 0 in
    (match X.run code state with
    | X.Done after ->
      if after.X.memory <> expected_memory base 11 second (word 10) (word 20)
        || after.X.machine <> state.X.machine then failwith "source PC/accumulator update"
    | _ -> failwith "update execution");
    {memory = source.F.memory; base; first; second; code;
      expected = [8, first; 24, word 64; 40, second; 56, word 10; 72, word 20]}
let fixtures () =
  let swap = Write.Write (56, 2, Write.Write (72, 1, Write.End)) in
  let duplicate = Write.Write (56, 2, Write.Write (56, 1, Write.End)) in
  let identity = Write.Write (56, 1, Write.Write (72, 2, Write.End)) in
  [move 0 swap (word 20) (word 10); move 7 swap (word 20) (word 10);
   move 0 duplicate (word 10) (word 20); move 7 duplicate (word 10) (word 20);
   move 0 identity (word 10) (word 20); move 7 identity (word 10) (word 20);
   update 0; update 7]
