module B = Wasm_u32
module G = Hmc_cfg_ir
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module Index = Hmc_u32_index
module Codec = Hmc_pointer_frame_codec
module Extent = Hmc_heap_extent
module Bounds = Hmc_linear_bounds
module Stack = Hmc_memory_stack
module Saved = Hmc_memory_saved_frame
module Prefix = Hmc_linear_preservation
module Image = Hmc_heap_image_prefix
let rec (materialize @ total) : (blocks : G.table) @ immutable -> (code_capacity : B.u32) -> (width : B.u32) ->
    (memory : B.bytes) @ immutable -> (base : B.u32) -> (limit : B.u32) -> (frames : Q.frames) @ immutable ->
    {u : unit | Index.fits (G.size blocks) code_capacity && width > 0
      && Extent.span (Saved.slots blocks) (Stack.zero ()) width && base <= limit && Bounds.covers memory limit} ->
    {out : Stack.stack option | match out with None -> true | Some out ->
      Stack.related blocks width out.Stack.memory base out.Stack.top frames
      && base <= out.Stack.top && out.Stack.top <= limit && Bounds.covers out.Stack.memory limit
      && Prefix.equal_prefix base memory out.Stack.memory
      && Hmc_tagged_cell.length out.Stack.memory === Hmc_tagged_cell.length memory
      && Hmc_linear_bytes.drop memory out.Stack.top === Hmc_linear_bytes.drop out.Stack.memory out.Stack.top} @ immutable =
  fun blocks code_capacity width memory base limit frames premise ->
    match frames with
    | Q.Halt ->
      ghost_ (Stack.related_def blocks width memory base base frames;
        Image.reflexive memory limit (); Prefix.shrink limit base memory memory ());
      Some {Stack.memory; top = base}
    | Q.Frame (activation, rest) ->
      (match G.lookup blocks activation.F.pc with
      | None -> None
      | Some block ->
        if not (Codec.shape block.G.signature activation) then None else
        match materialize blocks code_capacity width memory base limit rest () with
        | None -> None
        | Some previous ->
          match Stack.push blocks code_capacity width previous.Stack.memory base previous.Stack.top limit rest activation () with
          | None -> None
          | Some out ->
            ghost_ (let _ = Bounds.suffix memory limit previous.Stack.top () in
              Hmc_heap_image_suffix.seek memory previous.Stack.memory previous.Stack.top out.Stack.top ();
              Prefix.shrink previous.Stack.top base previous.Stack.memory out.Stack.memory ();
              Image.transitive memory previous.Stack.memory out.Stack.memory base ());
            Some out)
