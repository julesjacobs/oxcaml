module B = Wasm_u32
module D = Hm_declarative
module K = Hmc_closure_ir
module C = Hmc_cfg_program
module F = Hmc_frame_codec
module R = Hmc_runtime_closures
module Descriptor = Hmc_runtime_descriptor
module Table = Hmc_runtime_descriptor_table
module Source = Hmc_wasm_descriptor_source
module Load = Hmc_wasm_descriptor_load
module Index = Hmc_u32_index
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
type result = Source.result = {entry : K.entry; function_ : C.function_entry; locals : S.stack}
let (correct @ total) : (source : K.table) @ immutable -> (functions : C.functions) @ immutable -> (runtime : R.table) @ immutable ->
    (id : D.index) @ immutable -> (code : Table.count) -> (descriptor : R.descriptor) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (count : Table.count) -> (base_local : B.u32) -> (slots : Load.slots) @ immutable ->
    {u : unit | R.related source functions runtime && Index.represents id code && R.lookup runtime code === Some descriptor
      && base + 32 * count <= 4294967295 && Table.related runtime state.X.memory base count
      && L.get state.X.machine.E.locals base_local === Some (S.I32 (Table.address base code))
      && Load.distinct slots base_local && Load.writable slots state.X.machine.E.locals} ->
    {out : result | K.lookup source id === Some out.entry && C.lookup functions id === Some out.function_
      && Index.represents out.function_.C.start descriptor.R.start
      && Index.represents (F.locals_size out.entry.K.captured) descriptor.R.captures
      && descriptor.R.recursive = out.entry.K.recursive
      && X.run (Load.emit base_local slots) state === X.Done {X.memory = state.X.memory; machine = {E.locals = out.locals; stack = state.X.machine.E.stack}}
      && L.get out.locals base_local === Some (S.I32 (Table.address base code))
      && L.get out.locals slots.Load.start === Some (S.I32 descriptor.R.start)
      && L.get out.locals slots.Load.captures === Some (S.I32 descriptor.R.captures)
      && L.get out.locals slots.Load.recursive === Some (S.I32 (S.boolean out.entry.K.recursive))} @ immutable =
  fun source functions runtime id code descriptor state base count base_local slots premise ->
    ghost_ (Table.lookup runtime state.X.memory base count code descriptor ();
      Table.address_def base code; S.add32_def base (32 * code));
    Source.correct source functions runtime id code descriptor state (Table.address base code) base_local slots ()
