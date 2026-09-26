module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module V = Hmc_tagged_cell
module M = Hmc_heap_objects
module F = Hmc_heap_frame
module G = Hmc_cfg_ir
module Codec = Hmc_pointer_frame_codec
module Cap = Hmc_frame_capacity
module Index = Hmc_u32_index
module Wire = Hmc_heap_wire
module Memory = Hmc_memory_object
module Lookup = Hmc_memory_block_lookup
module Bounds = Hmc_linear_bounds
module Preserve = Hmc_linear_preservation
module E = Hmc_heap_extent
module L = Hmc_linear_bytes

let[@def] (slots @ total) (blocks : G.table @ immutable) = D.S (Cap.capacity blocks)
let[@def] (load @ total) (blocks : G.table @ immutable) (memory : B.bytes @ immutable) (address : W.limb) =
  match Memory.load memory address (Wire.Closure_schema (Cap.capacity blocks)) with
  | Some (Wire.Closure (code, cells)) ->
    (match Lookup.lookup blocks code with None -> None | Some (pc, block) ->
      match Codec.decode block.G.signature pc cells with None -> None | Some (a, _) -> Some a)
  | _ -> None
let rec (padding @ total) : (count : D.index) @ immutable -> {out : M.cells | M.length out === count} @ immutable = fun count ->
  match count with
  | D.Z -> ghost_ (M.length_def M.Empty); M.Empty
  | D.S rest -> let tail = padding rest in let out = M.Cell (V.Nil, tail) in ghost_ (M.length_def out); out
let rec (label_bound @ total) : (blocks : G.table) @ immutable -> (id : D.index) @ immutable -> (block : G.block) @ immutable ->
    {u : unit | G.lookup blocks id === Some block} -> {u : unit | Cap.le id (G.size blocks)} @ ghost = fun blocks id block premise -> ghost_ (
  G.lookup_def blocks id; G.size_def blocks;
  match blocks with G.Empty -> () | G.Add (_, rest) ->
    if Hm_elaboration_check.index_equal id (G.size rest) then (Cap.reflexive id; Hmc_heap_code_bounds.weaken id id ())
    else (label_bound rest id block (); Hmc_heap_code_bounds.weaken id (G.size rest) ()))
let (store @ total) : (blocks : G.table) @ immutable -> (code_capacity : W.limb) ->
    (memory : B.bytes) @ immutable -> (limit : W.limb) -> (address : W.limb) -> (stop : W.limb) ->
    (a : F.activation) @ immutable ->
    {u : unit | Index.fits (G.size blocks) code_capacity && Bounds.covers memory limit
      && E.span (slots blocks) address stop && stop <= limit
      && (match G.lookup blocks a.F.pc with None -> false | Some block -> Codec.shape block.G.signature a)} ->
    {out : B.bytes | load blocks out address === Some a && V.length out === V.length memory
      && Bounds.covers out limit && Preserve.equal_prefix address memory out && L.drop memory stop === L.drop out stop} @ immutable =
  fun blocks code_capacity memory limit address stop a premise ->
    match G.lookup blocks a.F.pc with
    | None -> unreachable_ ()
    | Some block ->
      ghost_ (label_bound blocks a.F.pc block (); Hmc_heap_code_bounds.fits_smaller a.F.pc (G.size blocks) code_capacity ();
        Cap.lookup blocks a.F.pc block (); Hmc_pointer_frame_shape.size block.G.signature);
      let padding = padding (Cap.remaining (Cap.capacity blocks) (Codec.size block.G.signature) ()) in
      let cells = Codec.encode block.G.signature a padding () in
      (match Index.encode code_capacity a.F.pc with
      | None -> unreachable_ ()
      | Some code ->
        let wire = Wire.Closure (code, cells) in
        ghost_ (slots_def blocks; Wire.slots_def wire; Lookup.correct blocks a.F.pc code ());
        let out = Memory.store memory limit address stop wire () in
        ghost_ (load_def blocks out address; Wire.schema_def wire); out)
let (preserve @ total) : (blocks : G.table) @ immutable -> (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (boundary : W.limb) -> (address : W.limb) -> (stop : W.limb) ->
    {u : unit | Preserve.equal_prefix boundary before after && E.span (slots blocks) address stop && stop <= boundary} ->
    {u : unit | load blocks before address === load blocks after address} @ ghost = fun blocks before after boundary address stop premise -> ghost_ (
  slots_def blocks; Memory.slots_def (Wire.Closure_schema (Cap.capacity blocks));
  Memory.preserve before after boundary address stop (Wire.Closure_schema (Cap.capacity blocks)) ();
  load_def blocks before address; load_def blocks after address)
let (preserve_suffix @ total) : (blocks : G.table) @ immutable -> (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (boundary : W.limb) -> (address : W.limb) ->
    {u : unit | Bounds.covers before boundary && Bounds.covers after boundary && boundary <= address
      && L.drop before boundary === L.drop after boundary} ->
    {u : unit | load blocks before address === load blocks after address} @ ghost = fun blocks before after boundary address premise -> ghost_ (
  Hmc_memory_suffix.load before after boundary address (Wire.bytes_size (slots blocks) D.Z) ();
  slots_def blocks; Memory.slots_def (Wire.Closure_schema (Hmc_frame_capacity.capacity blocks));
  Memory.load_def before address (Wire.Closure_schema (Hmc_frame_capacity.capacity blocks));
  Memory.load_def after address (Wire.Closure_schema (Hmc_frame_capacity.capacity blocks));
  load_def blocks before address; load_def blocks after address)
