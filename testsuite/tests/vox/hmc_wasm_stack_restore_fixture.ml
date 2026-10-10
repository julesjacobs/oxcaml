module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module H = Hmc_heap_objects
module W = Hmc_heap_wire
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module F = Hmc_wasm_call_save
module R = Hmc_wasm_relayout
module C = Wasm_cross_copy
module I = Wasm_instruction
module Header = Hmc_wasm_header_update
type fixture = {memory : B.bytes; expected : B.bytes; code : Wasm_code.t; prefix : I.t list; cursor : B.u32}
let rec cells = function [] -> H.Empty | value :: rest -> H.Cell (value, cells rest)
let rec fill n = if n = 0 then B.End else B.Byte (173, fill (n - 1))
let rec prefix n tail = if n = 0 then tail else B.Byte (173, prefix (n - 1) tail)
let rec context = function [] -> D.Empty_context | _ :: rest -> D.Binding (D.Forall (D.Z, D.Word64), context rest)
module Pad = Hmc_wasm_frame_padding
module Padded = Hmc_wasm_call_save_padded
let fixture (base : B.u32) padding_length env saved rest =
  match Hmc_u32_index.encode 8 padding_length with
  | None -> failwith "padding count"
  | Some padding_count ->
  if base > 7 then failwith "save base" else
  let temporary = if rest = [] then G.Empty_temporaries else G.Environment (context rest, G.Empty_temporaries) in
  let signature = {G.locals = context env; temporaries = G.Value (context saved, D.Function (D.Word64, D.Word64), temporary); accumulator = Some D.Word64} in
  let capacity = 3 + List.length env + List.length saved + List.length rest in
  if capacity < 3 || capacity > 32 then failwith "save capacity" else (
  if F.build signature (D.S D.Z) (capacity - 1) 10 <> None then failwith "save capacity accepted";
  if F.build signature (D.S D.Z) capacity 0 <> None then failwith "save PC accepted";
  match F.build signature (D.S D.Z) capacity 10 with
  | None -> failwith "save plan rejected"
  | Some fragment ->
    let current = V.Closure_pointer 1024 in
    let accumulator = V.Word {Hmc_word64.lo = 42; hi = 37} in
    let source = cells ([V.Word (Header.number 9); current; accumulator] @ env @ [V.Closure_pointer 2048] @ saved @ rest) in
    let source_size = 16 * (4 + List.length env + List.length saved + List.length rest) in
    let destination : B.u32 = 512 + base in
    let expected_cells = cells ([V.Word (Header.number 1); current; accumulator] @ saved @ rest) in
    let expected_size = 16 * (3 + List.length saved + List.length rest) in
    let memory = prefix base (W.encode_cells source (fill (1024 - base - source_size))) in
    let expected = prefix base (W.encode_cells source (prefix (512 - source_size)
      (W.encode_cells (Hmc_frame_segments.append expected_cells (Pad.cells padding_length)) (fill (1024 - destination - expected_size - 16 * padding_count))))) in
    if expected_size < 48 || expected_size > 256 then failwith "padding live extent" else (
    let padding_position : Hmc_wasm_relayout.count = expected_size / 16 in
    let padding = Pad.build padding_position padding_count padding_length () in
    let locals_tail = S.Push (S.I32 destination, S.Empty) in
    let locals = S.Push (S.I32 base, locals_tail) in
    let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
    let (code, cursor, expected) : Wasm_code.t * B.u32 * B.bytes =
      let remaining = cells (saved @ rest) in
      match signature.G.temporaries with
      | G.Value (saved_context, ty, schema) ->
        (match Hmc_u32_index.encode 32 (Hmc_pointer_frame_codec.locals_size signature.G.locals), Hmc_u32_index.encode 32 (H.length remaining) with
        | Some env_count, Some count ->
          if env_count > 32 || count > 12 then failwith "save size encoding" else (
          if not (Hm_elaboration_check.index_equal (H.length remaining)
            (D.add (Hmc_pointer_frame_codec.locals_size saved_context) (Hmc_pointer_frame_codec.temporaries_size schema))) then failwith "save remaining layout" else
          let tail = Hmc_wasm_call_save_layout.correct fragment signature (D.S D.Z) capacity saved_context ty schema env_count remaining 9 current accumulator () in
          if not (Hmc_wasm_read_check.words memory base 0 (Hmc_wire_word_sequence.words (Hmc_wasm_call_save_memory.header 9 current accumulator))) then failwith "save header reads" else
          if not (Hmc_wasm_read_check.cells memory base (3 + env_count) remaining) then failwith "save remaining reads" else
          match Hmc_linear_bytes.drop memory 1024 with
          | None -> failwith "save destination coverage"
          | Some _ ->
            ghost_ (Hmc_linear_bounds.covers_def memory 1024; Wasm_scatter_memory.zero_def ();
              L.get_def locals 0; L.get_def locals 1; L.get_def locals_tail 0);
            if padding_position <> 3 + count then failwith "padding position" else
            let result = Padded.correct fragment tail padding padding_count padding_length 9 current accumulator remaining (3 + env_count) count state base destination 1024 0 1 () in
            if result.Hmc_wasm_frame_pad_finish.memory <> expected then failwith "save decoded memory" else
            let payload = H.Cell (current, H.Cell (accumulator, remaining)) in
            let padded_payload = Hmc_frame_segments.append payload (Pad.cells padding_length) in
            let target_signature = {G.locals = saved_context; temporaries = schema; accumulator = None} in
            let target = {G.signature = target_signature; instruction = G.Jump (D.S D.Z)} in
            let capacity_signature = {G.locals = context (saved @ rest @ List.init padding_count (fun _ -> V.Nil)); temporaries = G.Empty_temporaries; accumulator = None} in
            let other = {G.signature = capacity_signature; instruction = G.Jump D.Z} in
            let blocks = G.Add (target, G.Add (other, G.Empty)) in
            if not (Hm_elaboration_check.index_equal (H.length padded_payload) (Hmc_frame_capacity.capacity blocks)) then failwith "stack capacity layout" else
            (match G.lookup blocks (D.S D.Z) with
            | None -> failwith "stack label lookup"
            | Some target ->
            (match Hmc_u32_index.encode 64 (Hmc_frame_capacity.capacity blocks), Hmc_pointer_frame_codec.decode target.G.signature (D.S D.Z) payload with
            | Some frame_capacity, Some (activation, H.Empty) ->
              if frame_capacity > 64 then failwith "stack capacity encoding" else
              ghost_ (Hmc_pointer_frame_codec.decode_def target.G.signature (D.S D.Z) payload;
                G.lookup_def blocks (D.S D.Z); G.size_def (G.Add (other, G.Empty)); G.size_def G.Empty;
                Hmc_u32_index.represents_def (D.S D.Z) 1; Hmc_u32_index.represents_def D.Z 0;
                F.matches_def signature (D.S D.Z) capacity fragment;
                Hmc_u32_index.unique (D.S D.Z) fragment.F.pc 1 ();
                Hmc_wasm_call_save_memory.cells_def fragment.F.pc current accumulator remaining;
                Hmc_frame_segments.append_def (H.Cell (V.Word (Header.number 1), payload)) (Pad.cells padding_length));
              ghost_ (Hmc_wasm_saved_frame.correct blocks target activation 1 payload (Pad.cells padding_length) frame_capacity
                result.Hmc_wasm_frame_pad_finish.memory destination result.Hmc_wasm_frame_pad_finish.bytes result.Hmc_wasm_frame_pad_finish.suffix ();
                Hmc_memory_saved_frame.slots_def blocks;
                Hmc_u32_index.represents_def (D.S (Hmc_frame_capacity.capacity blocks)) (frame_capacity + 1));
              (match Hmc_wasm_reservation.reserve (Hmc_memory_saved_frame.slots blocks) (frame_capacity + 1) destination 1024 () with
              | None -> failwith "stack full"
              | Some stop ->
                let width : B.u32 = 16 * (frame_capacity + 1) in
                let body = Padded.emit fragment padding 0 1 in
                ghost_ (Hmc_memory_stack.related_def blocks width memory destination destination Hmc_heap_state.Halt);
                let updated_locals = Hmc_wasm_stack_push.correct blocks width destination destination stop 1024 Hmc_heap_state.Halt activation body state result.Hmc_wasm_frame_pad_finish.memory 1 () in
                let code = Hmc_wasm_stack_push.emit body width 1 in
                if X.run code state <> X.Done {X.memory = expected; machine = {E.locals = updated_locals; stack = S.Empty}} then failwith "stack push execution";
                let pushed = {X.memory = result.Hmc_wasm_frame_pad_finish.memory; machine = {E.locals = updated_locals; stack = S.Empty}} in
                let popped_locals = Hmc_wasm_stack_retreat.correct width 1 stop pushed () in
                ghost_ (S.sub32_def stop width;
                  L.other_local locals 1 (S.I32 stop) updated_locals 0 ();
                  L.other_local updated_locals 1 (S.I32 destination) popped_locals 0 ());
                let popped = {X.memory = pushed.X.memory; machine = {E.locals = popped_locals; stack = S.Empty}} in
                let restore = Hmc_wasm_frame_restore.build (H.length padded_payload) frame_capacity () in
                let restored = Hmc_wasm_frame_restore.correct restore 1 padded_payload frame_capacity popped destination base 1024
                  result.Hmc_wasm_frame_pad_finish.bytes result.Hmc_wasm_frame_pad_finish.suffix 1 0 () in
                (match Hmc_linear_bytes.drop expected (base + width) with
                | None -> failwith "restored suffix"
                | Some retained ->
                  let expected = prefix base (W.encode_cells (H.Cell (V.Word (Header.number 1), padded_payload)) retained) in
                  if restored.Wasm_scatter_memory.memory <> expected then failwith "restored independent frame";
                  let retreat = Hmc_wasm_stack_retreat.emit width 1 in
                  let copy = Wasm_cross_copy.emit restore 1 0 in
                  let tail = E.append retreat copy in
                  let combined = E.append code tail in
                  ghost_ (X.append_correct retreat copy pushed; X.append_correct code tail state);
                  if X.run combined state <> X.Done {X.memory = expected; machine = popped.X.machine} then failwith "stack restore execution";
                  combined, destination, expected))
            | _ -> failwith "stack frame decode")))
        | _ -> failwith "save size encoding")
      | _ -> failwith "save operand shape"
    in
    {memory; expected; code;
      prefix = [I.I32_const base; I.Local_set 0; I.I32_const destination; I.Local_set 1]; cursor}))
let fixtures () =
  let word n = V.Word (Header.number n) in
  List.concat_map (fun padding_length ->
    List.concat_map (fun base -> [fixture base padding_length [] [] []; fixture base padding_length [word 11; word 12] [word 21] [word 31; word 32]]) [0; 7])
    [D.Z; D.S D.Z; D.S (D.S (D.S D.Z))]
