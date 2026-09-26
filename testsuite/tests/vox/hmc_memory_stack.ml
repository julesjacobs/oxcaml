module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module V = Hmc_tagged_cell
module G = Hmc_cfg_ir
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module Saved = Hmc_memory_saved_frame
module Codec = Hmc_pointer_frame_codec
module Index = Hmc_u32_index
module E = Hmc_heap_extent
module Bounds = Hmc_linear_bounds
module Preserve = Hmc_linear_preservation
module L = Hmc_linear_bytes

let[@def] (zero @ total) (u : unit) : W.limb = 0
let rec (equal_width @ total) : (cells : D.index) @ immutable ->
    (a : W.limb) -> (b : W.limb) -> (c : W.limb) -> (d : W.limb) ->
    {u : unit | E.span cells a b && E.span cells c d} -> {u : unit | b - a = d - c} @ ghost = fun cells a b c d premise -> ghost_ (
  E.span_def cells a b; E.span_def cells c d;
  match cells with D.Z -> () | D.S rest -> equal_width rest (a + 16) b (c + 16) d ())
let[@def] (previous @ total) (width : W.limb) (top : W.limb) : W.limb = if top < width then 0 else top - width
let[@def] rec (related @ total) (blocks : G.table @ immutable) (width : W.limb) (memory : B.bytes @ immutable)
    (base : W.limb) (top : W.limb) (frames : Q.frames @ immutable) = ghost_ (match frames with
  | Q.Halt -> top = base
  | Q.Frame (a, rest) -> top >= width && base <= previous width top
      && E.span (Saved.slots blocks) (previous width top) top
      && Saved.load blocks memory (previous width top) === Some a
      && related blocks width memory base (previous width top) rest)
let rec (preserve @ total) : (blocks : G.table) @ immutable -> (width : W.limb) ->
    (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (base : W.limb) -> (top : W.limb) ->
    (frames : Q.frames) @ immutable -> (boundary : W.limb) ->
    {u : unit | related blocks width before base top frames && top <= boundary && Preserve.equal_prefix boundary before after} ->
    {u : unit | related blocks width after base top frames} @ ghost = fun blocks width before after base top frames boundary premise -> ghost_ (
  related_def blocks width before base top frames; related_def blocks width after base top frames;
  match frames with
  | Q.Halt -> ()
  | Q.Frame (_, rest) -> previous_def width top;
    Saved.preserve blocks before after boundary (previous width top) top ();
    preserve blocks width before after base (previous width top) rest boundary ())
type stack = {memory : B.bytes; top : W.limb}
let (push @ total) : (blocks : G.table) @ immutable -> (code_capacity : W.limb) -> (width : W.limb) ->
    (memory : B.bytes) @ immutable -> (base : W.limb) -> (top : W.limb) -> (limit : W.limb) ->
    (frames : Q.frames) @ ghost -> (a : F.activation) @ immutable ->
    {u : unit | Index.fits (G.size blocks) code_capacity && width > 0 && E.span (Saved.slots blocks) (zero ()) width
      && Bounds.covers memory limit && base <= top && top <= limit && related blocks width memory base top frames
      && (match G.lookup blocks a.F.pc with None -> false | Some block -> Codec.shape block.G.signature a)} ->
    {out : stack option | match out with
      | None -> not (E.fits (Saved.slots blocks) top limit)
      | Some out -> E.fits (Saved.slots blocks) top limit && related blocks width out.memory base out.top (Q.Frame (a, frames)) && out.top <= limit
          && out.top - top = width && Bounds.covers out.memory limit && V.length out.memory === V.length memory
          && Preserve.equal_prefix top memory out.memory && L.drop memory out.top === L.drop out.memory out.top} @ immutable =
  fun blocks code_capacity width memory base top limit frames a premise ->
    match E.reserve (Saved.slots blocks) top limit with
    | None -> None
    | Some stop ->
      let updated = Saved.store blocks code_capacity memory limit top stop a () in
      ghost_ (zero_def (); equal_width (Saved.slots blocks) 0 width top stop (); previous_def width stop;
        preserve blocks width memory updated base top frames top ();
        related_def blocks width updated base stop (Q.Frame (a, frames)));
      Some {memory = updated; top = stop}
type popped = Empty | Popped of F.activation * W.limb | Invalid [@@inductive]
let[@def] (pop @ total) (blocks : G.table @ immutable) (width : W.limb) (memory : B.bytes @ immutable)
    (base : W.limb) (top : W.limb) =
  if top = base then Empty
  else if width = 0 || top < width || previous width top < base then Invalid
  else match Saved.load blocks memory (previous width top) with None -> Invalid | Some a -> Popped (a, previous width top)
let (pop_correct @ total) : (blocks : G.table) @ immutable -> (width : W.limb) -> (memory : B.bytes) @ immutable ->
    (base : W.limb) -> (top : W.limb) -> (frames : Q.frames) @ immutable ->
    {u : unit | width > 0 && related blocks width memory base top frames} ->
    {u : unit | match frames with
      | Q.Halt -> pop blocks width memory base top === Empty
      | Q.Frame (a, rest) -> pop blocks width memory base top === Popped (a, previous width top)
          && related blocks width memory base (previous width top) rest} @ ghost = fun blocks width memory base top frames premise -> ghost_ (
  related_def blocks width memory base top frames; previous_def width top; pop_def blocks width memory base top)
let rec (preserve_suffix @ total) : (blocks : G.table) @ immutable -> (width : W.limb) ->
    (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (base : W.limb) -> (top : W.limb) ->
    (frames : Q.frames) @ immutable -> (boundary : W.limb) ->
    {u : unit | related blocks width before base top frames && boundary <= base
      && Bounds.covers before boundary && Bounds.covers after boundary && L.drop before boundary === L.drop after boundary} ->
    {u : unit | related blocks width after base top frames} @ ghost = fun blocks width before after base top frames boundary premise -> ghost_ (
  related_def blocks width before base top frames; related_def blocks width after base top frames;
  match frames with
  | Q.Halt -> ()
  | Q.Frame (_, rest) ->
    Saved.preserve_suffix blocks before after boundary (previous width top) ();
    preserve_suffix blocks width before after base (previous width top) rest boundary ())
