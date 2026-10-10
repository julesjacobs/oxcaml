module D = Hm_declarative
module W = Hmc_word64
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module K = Hmc_closure_ir
module C = Hmc_cfg_program
module I = Hmc_tail_ir
module G = Hmc_cfg_ir
module F = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Seg = Hmc_frame_segments
module Machine = Hmc_heap_machine
let[@def] (signature @ total) (entry : K.entry @ immutable) : G.signature @ immutable =
  {G.locals = K.context entry; temporaries = G.Empty_temporaries; accumulator = None}
let[@def] (activation @ total) (entry : K.entry @ immutable) (start : D.index @ immutable)
    (closure : V.value @ immutable) (argument : V.value @ immutable) (captures : Heap.cells @ immutable) : F.activation @ immutable =
  {F.pc = start; env = Heap.Cell (argument, (if entry.K.recursive then Heap.Cell (closure, captures) else captures));
    accumulator = V.Nil; temporaries = F.Empty; current = closure}
let (shape @ total) : (entry : K.entry) @ immutable -> (start : D.index) @ immutable ->
    (closure : V.value) @ immutable -> (argument : V.value) @ immutable -> (captures : Heap.cells) @ immutable ->
    {u : unit | Codec.environment entry.K.captured captures} ->
    {u : unit | Codec.shape (signature entry) (activation entry start closure argument captures)} @ ghost =
  fun entry start closure argument captures premise -> ghost_ (
    signature_def entry; activation_def entry start closure argument captures; K.context_def entry;
    Codec.shape_def (signature entry) (activation entry start closure argument captures);
    Codec.temporaries_shape_def G.Empty_temporaries F.Empty;
    Codec.environment_def (K.context entry) (activation entry start closure argument captures).F.env;
    if entry.K.recursive then
      Codec.environment_def (D.Binding (D.Forall (D.Z, D.Function (entry.K.argument, entry.K.result)), entry.K.captured)) (Heap.Cell (closure, captures)) else ();
    ())
type result = {entered : F.activation; cells : Heap.cells}
let (correct @ total) : (program : I.program) @ immutable -> (heap : Heap.heap) @ immutable -> (address : W.limb) ->
    (id : D.index) @ immutable -> (captures : Heap.cells) @ immutable -> (entry : K.entry) @ immutable -> (code : C.function_entry) @ immutable ->
    (argument : V.value) @ immutable -> (padding : Heap.cells) @ immutable ->
    {u : unit | Hmc_heap_preservation.lookup_object heap address === Some (Heap.Closure (id, captures))
      && K.lookup program.I.origin.C.origin.Hmc_closure_program.table id === Some entry
      && C.lookup program.I.origin.C.functions id === Some code
      && Codec.environment entry.K.captured captures} ->
    {out : result | Machine.invoke program heap (V.Closure_pointer address) argument === Some out.entered
      && out.entered === activation entry code.C.start (V.Closure_pointer address) argument captures
      && Codec.decode (signature entry) code.C.start out.cells === Some (out.entered, padding)
      && Heap.length out.cells === D.add (Codec.size (signature entry)) (Heap.length padding)} @ immutable =
  fun program heap address id captures entry code argument padding premise ->
    let entered = activation entry code.C.start (V.Closure_pointer address) argument captures in
    ghost_ (activation_def entry code.C.start (V.Closure_pointer address) argument captures;
      Hmc_heap_call_transition.invoke program heap address id captures entry code argument ();
      shape entry code.C.start (V.Closure_pointer address) argument captures ());
    let cells = Codec.encode (signature entry) entered padding () in
    {entered; cells}
