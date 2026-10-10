module D = Hm_declarative
module K = Hmc_closure_ir
module O = Hmc_closure_program
module N = Hmc_monomorphic
module M = Hmc_heap_objects
module R = Hmc_closure_semantics
module H = Hmc_frame_shape
module V = Hmc_frame_values
module P = Hmc_heap_preservation
module Model_alloc = Hmc_heap_allocate
module A = Hmc_memory_allocate
module X = Hmc_heap_machine
module J = Hmc_heap_globals
module E = Hmc_heap_extent
module Math = Hmc_heap_extent_math

module Old = Hmc_heap_static
module Bytes = Wasm_u32
module W = Hmc_word64
module Image = Hmc_heap_image
module Bounds = Hmc_linear_bounds
module Index = Hmc_u32_index
module L = Hmc_linear_bytes
module Cell = Hmc_tagged_cell

type image = {memory : Bytes.bytes; frontier : W.limb; heap : M.heap @@ ghost}
type result = Ready of image * X.globals | Full of image * D.index [@@inductive]
let[@def] (correct @ total) (table : K.table @ immutable) (source : O.globals @ immutable) (initial : M.heap @ immutable)
    (memory : Bytes.bytes @ immutable) (limit : W.limb) (out : result @ immutable) = ghost_ (
  let image = match out with Ready (image, _) | Full (image, _) -> image in
  Image.related image.memory image.heap && image.frontier = M.used image.heap && Bounds.covers image.memory limit
  && Cell.length image.memory === Cell.length memory && L.drop memory limit === L.drop image.memory limit
  && Old.correct table source initial limit (match out with
    | Ready (image, globals) -> Old.Ready (image.heap, globals)
    | Full (image, code) -> Old.Full (image.heap, code)))
let rec (initialize @ total) : (table : K.table) @ ghost -> (capacity : W.limb) -> (source : O.globals) @ immutable ->
    (memory : Bytes.bytes) @ immutable -> (frontier : W.limb) -> (heap : M.heap) @ ghost -> (limit : W.limb) ->
    {u : unit | Index.fits (K.size table) capacity && Old.closed table source && M.valid table heap && frontier = M.used heap && frontier <= limit
      && Image.related memory heap && Bounds.covers memory limit} ->
    {out : result | correct table source heap memory limit out} @ immutable = fun table capacity source memory frontier heap limit premise ->
  ghost_ (Old.closed_def table source; O.size_def source);
  match source with
  | O.No_globals ->
    let out = Ready ({memory; frontier; heap}, X.Empty_globals) in
    ghost_ (E.span_def D.Z (M.used heap) (M.used heap); P.extends_def heap heap; J.related_def heap source X.Empty_globals;
      Old.correct_def table source heap limit (Old.Ready (heap, X.Empty_globals)); correct_def table source heap memory limit out); out
  | O.Global (code, rest) ->
    let previous = initialize table capacity rest memory frontier heap limit () in
    ghost_ (correct_def table rest heap memory limit previous);
    match previous with
    | Full (image, pending) ->
      let out = Full (image, pending) in ghost_ (
        Old.correct_def table rest heap limit (Old.Full (image.heap, pending)); Math.add_one (O.size rest);
        if E.fits (O.size source) (M.used heap) limit then Math.prefix (O.size rest) (D.S D.Z) (M.used heap) limit () else ();
        Old.correct_def table source heap limit (Old.Full (image.heap, pending)); correct_def table source heap memory limit out); out
    | Ready (image, globals) ->
      let partial = ghost_ image.heap in
      ghost_ (Old.correct_def table rest heap limit (Old.Ready (partial, globals)); Old.object_valid table partial code ());
      let allocation = A.allocate table capacity image.memory partial image.frontier limit (M.Closure (code, M.Empty)) () in
      ghost_ (A.correct_def table image.memory partial limit (M.Closure (code, M.Empty)) allocation);
      match allocation with
      | A.Exhausted ->
        let out = Full (image, code) in ghost_ (
          Math.add_one (O.size rest); M.slots_def (M.Closure (code, M.Empty)); M.length_def M.Empty;
          if E.fits (O.size source) (M.used heap) limit then Math.consume (O.size rest) (D.S D.Z) (M.used heap) (M.used partial) limit () else ();
          Old.correct_def table source heap limit (Old.Full (partial, code)); correct_def table source heap memory limit out); out
      | A.Allocated allocated ->
        let next = ghost_ (M.Allocate ({M.address = image.frontier; stop = allocated.A.frontier; object_ = M.Closure (code, M.Empty)}, partial)) in
        let out_globals = X.Global (O.size rest, allocated.A.reference, globals) in
        ghost_ (Model_alloc.correct_def table partial limit (M.Closure (code, M.Empty)) (Model_alloc.Allocated {Model_alloc.heap = next; reference = allocated.A.reference});
          P.transitive next partial heap ();
          M.slots_def (M.Closure (code, M.Empty)); M.length_def M.Empty; Math.add_one (O.size rest);
          Math.join (O.size rest) (D.S D.Z) (M.used heap) (M.used partial) (M.used next) ();
          J.preserve table next partial rest globals ();
          M.decode_object_def (M.view partial) (M.Closure (code, M.Empty)); M.decode_environment_def (M.view partial) M.Empty;
          J.related_def next source out_globals; Old.correct_def table source heap limit (Old.Ready (next, out_globals)));
        let out = Ready ({memory = allocated.A.memory; frontier = allocated.A.frontier; heap = next}, out_globals) in
        ghost_ (correct_def table source heap memory limit out); out
