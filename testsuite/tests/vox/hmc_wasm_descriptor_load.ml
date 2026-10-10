module B = Wasm_u32
module R = Hmc_runtime_closures
module Descriptor = Hmc_runtime_descriptor
module Read = Hmc_wasm_descriptor_reads
module Load = Wasm_limb_read
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
type slots = {start : B.u32; captures : B.u32; recursive : B.u32}
let[@def] (distinct @ total) (slots : slots @ immutable) (base_local : B.u32) =
  slots.start <> slots.captures && slots.start <> slots.recursive && slots.captures <> slots.recursive
  && slots.start <> base_local && slots.captures <> base_local && slots.recursive <> base_local
let[@def] (limb_slot @ total) (locals : S.stack @ immutable) (local : B.u32) =
  match L.get locals local with Some (S.I32 _) -> true | _ -> false
let[@def] (writable @ total) (slots : slots @ immutable) (locals : S.stack @ immutable) =
  limb_slot locals slots.start && limb_slot locals slots.captures && limb_slot locals slots.recursive
let[@def] (emit @ total) (base_local : B.u32) (slots : slots @ immutable) =
  E.append (Load.emit (Read.start_offset ()) base_local slots.start)
    (E.append (Load.emit (Read.captures_offset ()) base_local slots.captures)
      (Load.emit (Read.recursive_offset ()) base_local slots.recursive))
let (ready @ total) : (locals : S.stack) @ immutable -> (local : B.u32) -> (value : B.u32) ->
    {u : unit | limb_slot locals local} -> {u : unit | L.can_set locals local (S.I32 value)} @ ghost =
  fun locals local value premise -> ghost_ (
    limb_slot_def locals local; L.can_set_def locals local (S.I32 value);
    match L.get locals local with Some old -> S.same_type_def old (S.I32 value) | _ -> ())
let (correct @ total) : (descriptor : R.descriptor) @ immutable -> (state : X.state) @ immutable -> (address : B.u32) ->
    (base_local : B.u32) -> (slots : slots) @ immutable ->
    {u : unit | address <= 4294967263 && Descriptor.load state.X.memory address === Some descriptor
      && L.get state.X.machine.E.locals base_local === Some (S.I32 address)
      && distinct slots base_local && writable slots state.X.machine.E.locals} ->
    {out : S.stack | X.run (emit base_local slots) state === X.Done {X.memory = state.X.memory; machine = {E.locals = out; stack = state.X.machine.E.stack}}
      && L.get out base_local === Some (S.I32 address)
      && L.get out slots.start === Some (S.I32 descriptor.R.start)
      && L.get out slots.captures === Some (S.I32 descriptor.R.captures)
      && L.get out slots.recursive === Some (S.I32 (S.boolean descriptor.R.recursive))} @ immutable =
  fun descriptor state address base_local slots premise ->
    ghost_ (Read.correct state.X.memory address descriptor (); distinct_def slots base_local;
      writable_def slots state.X.machine.E.locals; ready state.X.machine.E.locals slots.start descriptor.R.start ());
    let first = Load.correct (Read.start_offset ()) base_local slots.start state address descriptor.R.start () in
    ghost_ (L.other_local state.X.machine.E.locals slots.start (S.I32 descriptor.R.start) first base_local ();
      L.other_local state.X.machine.E.locals slots.start (S.I32 descriptor.R.start) first slots.captures ();
      L.other_local state.X.machine.E.locals slots.start (S.I32 descriptor.R.start) first slots.recursive ();
      limb_slot_def first slots.captures; limb_slot_def state.X.machine.E.locals slots.captures;
      ready first slots.captures descriptor.R.captures ());
    let middle = {X.memory = state.X.memory; machine = {E.locals = first; stack = state.X.machine.E.stack}} in
    let second = Load.correct (Read.captures_offset ()) base_local slots.captures middle address descriptor.R.captures () in
    ghost_ (L.other_local first slots.captures (S.I32 descriptor.R.captures) second base_local ();
      L.other_local first slots.captures (S.I32 descriptor.R.captures) second slots.start ();
      L.other_local first slots.captures (S.I32 descriptor.R.captures) second slots.recursive ();
      limb_slot_def second slots.recursive; limb_slot_def state.X.machine.E.locals slots.recursive;
      ready second slots.recursive (S.boolean descriptor.R.recursive) ());
    let last = {X.memory = state.X.memory; machine = {E.locals = second; stack = state.X.machine.E.stack}} in
    let out = Load.correct (Read.recursive_offset ()) base_local slots.recursive last address (S.boolean descriptor.R.recursive) () in
    ghost_ (L.other_local second slots.recursive (S.I32 (S.boolean descriptor.R.recursive)) out base_local ();
      L.other_local second slots.recursive (S.I32 (S.boolean descriptor.R.recursive)) out slots.start ();
      L.other_local second slots.recursive (S.I32 (S.boolean descriptor.R.recursive)) out slots.captures ();
      X.append_correct (Load.emit (Read.captures_offset ()) base_local slots.captures) (Load.emit (Read.recursive_offset ()) base_local slots.recursive) middle;
      X.append_correct (Load.emit (Read.start_offset ()) base_local slots.start)
        (E.append (Load.emit (Read.captures_offset ()) base_local slots.captures) (Load.emit (Read.recursive_offset ()) base_local slots.recursive)) state;
      emit_def base_local slots);
    out
