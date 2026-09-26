module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module Heap = Hmc_heap_objects
module Image = Hmc_heap_image
module Index = Hmc_u32_index
module Read = Hmc_wasm_closure_read
module Load = Wasm_pointer_read
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
let[@def] (emit @ total) (object_local : B.u32) (code_local : B.u32) = Load.emit (Read.offset ()) object_local code_local
type result = {code : W.limb; locals : S.stack}
let (correct @ total) : (table : Hmc_closure_ir.table) @ immutable -> (heap : Heap.heap) @ immutable ->
    (state : X.state) @ immutable -> (address : B.u32) -> (id : D.index) @ immutable -> (captures : Heap.cells) @ immutable -> (count : Hmc_wasm_relayout.count) ->
    (object_local : B.u32) -> (code_local : B.u32) ->
    {u : unit | Heap.valid table heap && Image.related state.X.memory heap && Index.represents (Heap.length captures) count
      && Hmc_heap_preservation.lookup_object heap address === Some (Heap.Closure (id, captures))
      && L.get state.X.machine.E.locals object_local === Some (S.I32 address)
      && (match L.get state.X.machine.E.locals code_local with Some (S.I32 _) -> true | _ -> false)} ->
    {out : result | Index.represents id out.code
      && X.run (emit object_local code_local) state === X.Done {X.memory = state.X.memory; machine = {E.locals = out.locals; stack = state.X.machine.E.stack}}
      && L.get out.locals code_local === Some (S.I32 out.code)
      && L.replaced state.X.machine.E.locals code_local (S.I32 out.code) out.locals
      && Hmc_wasm_range_copy.reads state.X.memory address (Read.position ()) captures} @ immutable =
  fun table heap state address id captures count object_local code_local premise ->
    let read = Read.correct table heap state.X.memory address id captures count () in
    ghost_ (L.can_set_def state.X.machine.E.locals code_local (S.I32 read.Read.code);
      match L.get state.X.machine.E.locals code_local with
      | Some old -> S.same_type_def old (S.I32 read.Read.code)
      | _ -> ());
    let locals = Load.correct (Read.offset ()) object_local code_local state address read.Read.code () in
    ghost_ (emit_def object_local code_local);
    {code = read.Read.code; locals}
