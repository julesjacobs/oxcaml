module B = Wasm_u32
module V = Hmc_tagged_cell
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module M = Wasm_memory
module Read = Wasm_frame_snapshot
module Values = Wasm_snapshot_values
type slots = {head_tag : B.u32; head_payload : B.u32; tail_tag : B.u32; tail_payload : B.u32}
let[@def] (distinct @ total) (slots : slots @ immutable) =
  slots.head_tag <> slots.head_payload && slots.head_tag <> slots.tail_tag && slots.head_tag <> slots.tail_payload
  && slots.head_payload <> slots.tail_tag && slots.head_payload <> slots.tail_payload && slots.tail_tag <> slots.tail_payload
let[@def] (separate @ total) (slots : slots @ immutable) (local : B.u32) =
  slots.head_tag <> local && slots.head_payload <> local && slots.tail_tag <> local && slots.tail_payload <> local
let[@def] (word_slot @ total) (locals : S.stack @ immutable) (local : B.u32) = match L.get locals local with
  | Some (S.I64 _) -> true | _ -> false
let[@def] (writable @ total) (slots : slots @ immutable) (locals : S.stack @ immutable) =
  word_slot locals slots.head_tag && word_slot locals slots.head_payload && word_slot locals slots.tail_tag && word_slot locals slots.tail_payload
let[@def] (reads @ total) (head_tag : B.u32) (head_payload : B.u32) (slots : slots @ immutable) =
  Read.Read (head_tag, slots.head_tag, Read.Read (head_payload, slots.head_payload,
    Read.Read (32, slots.tail_tag, Read.Read (40, slots.tail_payload, Read.End))))
let[@def] (emit @ total) (head_tag : B.u32) (head_payload : B.u32) (slots : slots @ immutable) (frame_local : B.u32) =
  Read.emit (reads head_tag head_payload slots) frame_local
let (layout @ total) : (head_tag : B.u32) -> (head_payload : B.u32) -> (slots : slots) @ immutable -> (local : B.u32) ->
    {u : unit | separate slots local} -> {u : unit | Read.separate (reads head_tag head_payload slots) local} @ ghost =
  fun head_tag head_payload slots local premise -> ghost_ (
    separate_def slots local; reads_def head_tag head_payload slots;
    Read.separate_def (reads head_tag head_payload slots) local;
    Read.separate_def (Read.Read (head_payload, slots.head_payload, Read.Read (32, slots.tail_tag, Read.Read (40, slots.tail_payload, Read.End)))) local;
    Read.separate_def (Read.Read (32, slots.tail_tag, Read.Read (40, slots.tail_payload, Read.End))) local;
    Read.separate_def (Read.Read (40, slots.tail_payload, Read.End)) local; Read.separate_def Read.End local)
let (selection @ total) : (head_tag : B.u32) -> (head_payload : B.u32) -> (slots : slots) @ immutable -> (local : B.u32) ->
    {u : unit | Values.selected (reads head_tag head_payload slots) local ===
      (if local = slots.tail_payload then Some (Hmc_wasm_primitive_payload.offset ())
       else if local = slots.tail_tag then Some (Hmc_wasm_primitive_write.tag_offset ())
       else if local = slots.head_payload then Some head_payload else if local = slots.head_tag then Some head_tag else None)} @ ghost =
  fun head_tag head_payload slots local -> ghost_ (
    reads_def head_tag head_payload slots; Hmc_wasm_primitive_payload.offset_def (); Hmc_wasm_primitive_write.tag_offset_def ();
    Values.selected_def (reads head_tag head_payload slots) local;
    Values.selected_def (Read.Read (head_payload, slots.head_payload, Read.Read (32, slots.tail_tag, Read.Read (40, slots.tail_payload, Read.End)))) local;
    Values.selected_def (Read.Read (32, slots.tail_tag, Read.Read (40, slots.tail_payload, Read.End))) local;
    Values.selected_def (Read.Read (40, slots.tail_payload, Read.End)) local; Values.selected_def Read.End local)
let (correct @ total) : (head : V.value) @ immutable -> (tail : V.value) @ immutable -> (state : X.state) @ immutable ->
    (base : B.u32) -> (frame_local : B.u32) -> (heap_local : B.u32) -> (limit_local : B.u32) ->
    (head_tag : B.u32) -> (head_payload : B.u32) -> (slots : slots) @ immutable ->
    {u : unit | distinct slots && separate slots frame_local && separate slots heap_local && separate slots limit_local
      && writable slots state.X.machine.E.locals && L.get state.X.machine.E.locals frame_local === Some (S.I32 base)
      && M.load state.X.memory base head_tag M.W64 === Some (S.I64 (V.tag head))
      && M.load state.X.memory base head_payload M.W64 === Some (S.I64 (V.payload head))
      && M.load state.X.memory base (Hmc_wasm_primitive_write.tag_offset ()) M.W64 === Some (S.I64 (V.tag tail))
      && M.load state.X.memory base (Hmc_wasm_primitive_payload.offset ()) M.W64 === Some (S.I64 (V.payload tail))} ->
    {out : S.stack | X.run (emit head_tag head_payload slots frame_local) state === X.Done {X.memory = state.X.memory; machine = {E.locals = out; stack = state.X.machine.E.stack}}
      && L.get out slots.head_tag === Some (S.I64 (V.tag head)) && L.get out slots.head_payload === Some (S.I64 (V.payload head))
      && L.get out slots.tail_tag === Some (S.I64 (V.tag tail)) && L.get out slots.tail_payload === Some (S.I64 (V.payload tail))
      && L.get out frame_local === Some (S.I32 base)
      && L.get out heap_local === L.get state.X.machine.E.locals heap_local && L.get out limit_local === L.get state.X.machine.E.locals limit_local} @ immutable =
  fun head tail state base frame_local heap_local limit_local head_tag head_payload slots premise ->
    let plan = reads head_tag head_payload slots in
    ghost_ (distinct_def slots; writable_def slots state.X.machine.E.locals;
      word_slot_def state.X.machine.E.locals slots.head_tag; word_slot_def state.X.machine.E.locals slots.head_payload;
      word_slot_def state.X.machine.E.locals slots.tail_tag; word_slot_def state.X.machine.E.locals slots.tail_payload;
      L.can_set_def state.X.machine.E.locals slots.head_tag (S.I64 (V.tag head)); L.can_set_def state.X.machine.E.locals slots.head_payload (S.I64 (V.payload head));
      L.can_set_def state.X.machine.E.locals slots.tail_tag (S.I64 (V.tag tail)); L.can_set_def state.X.machine.E.locals slots.tail_payload (S.I64 (V.payload tail));
      (match L.get state.X.machine.E.locals slots.head_tag with Some old -> S.same_type_def old (S.I64 (V.tag head)) | _ -> ());
      (match L.get state.X.machine.E.locals slots.head_payload with Some old -> S.same_type_def old (S.I64 (V.payload head)) | _ -> ());
      (match L.get state.X.machine.E.locals slots.tail_tag with Some old -> S.same_type_def old (S.I64 (V.tag tail)) | _ -> ());
      (match L.get state.X.machine.E.locals slots.tail_payload with Some old -> S.same_type_def old (S.I64 (V.payload tail)) | _ -> ());
      reads_def head_tag head_payload slots; Hmc_wasm_primitive_write.tag_offset_def (); Hmc_wasm_primitive_payload.offset_def ();
      Values.ready_def plan state.X.memory base state.X.machine.E.locals;
      Values.ready_def (Read.Read (head_payload, slots.head_payload, Read.Read (32, slots.tail_tag, Read.Read (40, slots.tail_payload, Read.End)))) state.X.memory base state.X.machine.E.locals;
      Values.ready_def (Read.Read (32, slots.tail_tag, Read.Read (40, slots.tail_payload, Read.End))) state.X.memory base state.X.machine.E.locals;
      Values.ready_def (Read.Read (40, slots.tail_payload, Read.End)) state.X.memory base state.X.machine.E.locals;
      Values.ready_def Read.End state.X.memory base state.X.machine.E.locals);
    let out = Values.project plan state.X.memory base state.X.machine.E.locals () in
    ghost_ (layout head_tag head_payload slots frame_local (); layout head_tag head_payload slots heap_local (); layout head_tag head_payload slots limit_local ();
      Read.correct plan frame_local state base out ();
      selection head_tag head_payload slots slots.head_tag; selection head_tag head_payload slots slots.head_payload;
      selection head_tag head_payload slots slots.tail_tag; selection head_tag head_payload slots slots.tail_payload;
      Values.get plan state.X.memory base state.X.machine.E.locals out slots.head_tag ();
      Values.get plan state.X.memory base state.X.machine.E.locals out slots.head_payload ();
      Values.get plan state.X.memory base state.X.machine.E.locals out slots.tail_tag ();
      Values.get plan state.X.memory base state.X.machine.E.locals out slots.tail_payload ();
      Values.unchanged plan state.X.memory base state.X.machine.E.locals out heap_local ();
      Values.unchanged plan state.X.memory base state.X.machine.E.locals out limit_local ();
      emit_def head_tag head_payload slots frame_local);
    out
