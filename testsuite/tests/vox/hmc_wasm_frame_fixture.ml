module D = Hm_declarative
module B = Wasm_u32
module W = Hmc_word64
module V = Hmc_tagged_cell
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module G = Hmc_cfg_ir
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module L = Hmc_linear_bytes
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module Header = Hmc_wasm_frame_header
module Snapshot = Wasm_frame_snapshot
let rec prefix : (n : B.u32) -> (tail : B.bytes) @ immutable ->
    {out : B.bytes | L.drop out n === Some tail} @ immutable = fun n tail ->
  if n = 0 then (ghost_ (L.drop_def tail n); tail) else
    let rest = prefix (n - 1) tail in
    let out = B.Byte (0, rest) in ghost_ (L.drop_def out n); out
let rec index n = if n = 0 then D.Z else D.S (index (n - 1))
let rec word_locals n tail = if n = 0 then tail else S.Push (S.I64 {W.lo = 0; hi = 0}, word_locals (n - 1) tail)
type fixture = {memory : B.bytes; base : B.u32; code : Wasm_code.t; expected : W.t list}
let make : (base : B.u32) -> (signature : G.signature) @ immutable -> (activation : F.activation) @ immutable ->
    {u : unit | Codec.shape signature activation} -> fixture = fun base signature activation premise ->
  let cells = Codec.encode signature activation H.Empty () in
  let wire = Wire.Closure (7, cells) in
  let bytes = Wire.encode wire (B.Byte (42, B.End)) in
  let memory = prefix base bytes in
  if base <= 4294967247 then (
    ghost_ (Wire.schema_def wire; Header.correct memory base bytes (H.length cells) 7 cells (B.Byte (42, B.End)) signature activation H.Empty ());
    if Header.pc memory base <> Some (S.I64 (Header.number 7))
      || Header.current_tag memory base <> Some (S.I64 (V.tag activation.F.current))
      || Header.current_payload memory base <> Some (S.I64 (V.payload activation.F.current))
      || Header.accumulator_tag memory base <> Some (S.I64 (V.tag activation.F.accumulator))
      || Header.accumulator_payload memory base <> Some (S.I64 (V.payload activation.F.accumulator)) then failwith "source frame header";
    let reads = Snapshot.Read (8, 1, Snapshot.Read (16, 2, Snapshot.Read (24, 3,
      Snapshot.Read (32, 4, Snapshot.Read (40, 5, Snapshot.End))))) in
    let locals = S.Push (S.I32 base, word_locals 5 S.Empty) in
    let state = {X.memory; machine = {E.locals; stack = S.Push (S.I32 91, S.Empty)}} in
    ghost_ (Wasm_locals.get_def locals 0);
    if Snapshot.separate reads 0 then
      match Snapshot.project reads memory base locals with
      | None -> failwith "frame snapshot projection"
      | Some expected_locals ->
        ghost_ (Snapshot.correct reads 0 state base expected_locals ());
        let code = Snapshot.emit reads 0 in
        let expected = [Header.number 7; V.tag activation.F.current; V.payload activation.F.current;
          V.tag activation.F.accumulator; V.payload activation.F.accumulator] in
        (match X.run code state with
        | X.Done after ->
          if after.X.memory <> memory || after.X.machine.E.stack <> state.X.machine.E.stack then failwith "snapshot preservation";
          let rec check (i : B.u32) = function
            | [] -> ()
            | word :: rest ->
              if Wasm_locals.get after.X.machine.E.locals i <> Some (S.I64 word) then failwith "snapshot source value";
              if i < 4294967295 then check (i + 1) rest else failwith "fixture local limit" in
          check 1 expected
        | _ -> failwith "frame snapshot execution");
        {memory; base; code; expected}
    else failwith "base alias")
  else failwith "fixture memory address"
let fixtures () =
  let current = V.Closure_pointer 64 in
  let one (base : B.u32) accumulator ty =
    let signature = {G.locals = D.Empty_context; temporaries = G.Empty_temporaries; accumulator = Some ty} in
    let activation = {F.pc = index 7; env = H.Empty; temporaries = F.Empty; current; accumulator} in
    ghost_ (Codec.shape_def signature activation; Codec.environment_def D.Empty_context H.Empty;
      Codec.temporaries_shape_def G.Empty_temporaries F.Empty);
    make base signature activation () in
  [one 0 (V.Word {W.lo = 305419896; hi = 2882400001}) D.Word64;
   one 7 (V.Word {W.lo = 4294967295; hi = 4294967295}) D.Word64;
   one 7 (V.Boolean true) D.Boolean]

let env_fixture (base : B.u32) =
  let ty = D.Forall (D.Z, D.Word64) in
  let rest = D.Binding (ty, D.Empty_context) in
  let context = D.Binding (ty, rest) in
  let env_tail = H.Cell (V.Word {W.lo = 20; hi = 0}, H.Empty) in
  let env = H.Cell (V.Word {W.lo = 10; hi = 0}, env_tail) in
  let signature = {G.locals = context; temporaries = G.Empty_temporaries; accumulator = Some D.Word64} in
  let activation = {F.pc = index 7; env; temporaries = F.Empty;
    current = V.Closure_pointer 64; accumulator = V.Word {W.lo = 42; hi = 0}} in
  ghost_ (Codec.shape_def signature activation; Codec.environment_def context env;
    Codec.environment_def rest env_tail; Codec.environment_def D.Empty_context H.Empty;
    Codec.temporaries_shape_def G.Empty_temporaries F.Empty);
  make base signature activation ()
