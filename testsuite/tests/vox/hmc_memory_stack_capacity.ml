module D = Hm_declarative
module W = Hmc_word64
module G = Hmc_cfg_ir
module Q = Hmc_heap_state
module E = Hmc_heap_extent
module Stack = Hmc_memory_stack
module Saved = Hmc_memory_saved_frame

let[@def] (fits @ total) (width : W.limb) (top : W.limb) (limit : W.limb) = top <= limit && width <= limit - top
let[@def] rec (region @ total) (width : W.limb) (count : D.index @ immutable) (base : W.limb) (top : W.limb) =
  match count with
  | D.Z -> top = base
  | D.S rest -> top >= width && base <= Stack.previous width top && region width rest base (Stack.previous width top)
let rec (ordered @ total) : (width : W.limb) -> (count : D.index) @ immutable -> (base : W.limb) -> (top : W.limb) ->
    {u : unit | region width count base top} -> {u : unit | base <= top} @ ghost = fun width count base top premise -> ghost_ (
  region_def width count base top; Stack.previous_def width top)
let rec (depth @ total) : (blocks : G.table) @ immutable -> (width : W.limb) -> (memory : Wasm_u32.bytes) @ immutable ->
    (base : W.limb) -> (top : W.limb) -> (frames : Q.frames) @ immutable ->
    {u : unit | Stack.related blocks width memory base top frames} ->
    {u : unit | region width (Q.depth frames) base top} @ ghost = fun blocks width memory base top frames premise -> ghost_ (
  Stack.related_def blocks width memory base top frames; Q.depth_def frames; region_def width (Q.depth frames) base top;
  match frames with Q.Halt -> () | Q.Frame (_, rest) -> depth blocks width memory base (Stack.previous width top) rest ())
let rec (available @ total) : (width : W.limb) -> (capacity : D.index) @ immutable -> (count : D.index) @ immutable ->
    (base : W.limb) -> (top : W.limb) -> (limit : W.limb) ->
    {u : unit | width > 0 && region width capacity base limit && region width count base top} ->
    {u : unit | D.present capacity count = fits width top limit} @ ghost = fun width capacity count base top limit premise -> ghost_ (
  region_def width capacity base limit; region_def width count base top;
  D.present_def capacity count; fits_def width top limit;
  Stack.previous_def width top; Stack.previous_def width limit;
  match capacity, count with
  | D.S remaining, D.S used ->
    available width remaining used base (Stack.previous width top) (Stack.previous width limit) ();
    fits_def width (Stack.previous width top) (Stack.previous width limit)
  | _ -> ())
let rec (translate @ total) : (cells : D.index) @ immutable -> (a : W.limb) -> (b : W.limb) -> (c : W.limb) -> (d : W.limb) ->
    {u : unit | E.span cells a b && c <= d && b - a = d - c} -> {u : unit | E.span cells c d} @ ghost =
  fun cells a b c d premise -> ghost_ (
    E.ordered cells a b (); E.span_def cells a b; E.span_def cells c d;
    match cells with D.Z -> () | D.S rest -> translate rest (a + 16) b (c + 16) d ())
let (fits_extent @ total) : (cells : D.index) @ immutable -> (width : W.limb) -> (top : W.limb) -> (limit : W.limb) ->
    {u : unit | E.span cells (Stack.zero ()) width} -> {u : unit | fits width top limit = E.fits cells top limit} @ ghost =
  fun cells width top limit premise -> ghost_ (
    Stack.zero_def (); fits_def width top limit;
    if fits width top limit then (
      translate cells 0 width top (top + width) (); E.sufficient cells top (top + width) limit ())
    else match E.reserve cells top limit with
      | None -> ()
      | Some stop -> Stack.equal_width cells 0 width top stop ())
let (remaining @ total) : (blocks : G.table) @ immutable -> (width : W.limb) -> (memory : Wasm_u32.bytes) @ immutable ->
    (base : W.limb) -> (top : W.limb) -> (limit : W.limb) -> (capacity : D.index) @ immutable -> (frames : Q.frames) @ immutable ->
    {u : unit | width > 0 && E.span (Saved.slots blocks) (Stack.zero ()) width
      && region width capacity base limit && Stack.related blocks width memory base top frames} ->
    {u : unit | D.present capacity (Q.depth frames) = E.fits (Saved.slots blocks) top limit} @ ghost =
  fun blocks width memory base top limit capacity frames premise -> ghost_ (
    depth blocks width memory base top frames (); available width capacity (Q.depth frames) base top limit ();
    fits_extent (Saved.slots blocks) width top limit ())
let[@def] rec (remaining_capacity @ total) (width : W.limb) (count : D.index @ immutable) (space : int) : int option =
  match count with
  | D.Z -> Some space
  | D.S rest -> match remaining_capacity width rest space with
    | None -> None | Some remaining -> if width > remaining then None else Some (remaining - width)
let rec (reserve @ total) : (width : W.limb) -> (count : D.index) @ immutable -> (base : W.limb) -> (limit : W.limb) ->
    {u : unit | base <= limit} -> {out : W.limb option | match out with
      | None -> remaining_capacity width count (limit - base) === None
      | Some top -> region width count base top && base <= top && top <= limit &&
        remaining_capacity width count (limit - base) === Some (limit - top)} @ immutable =
  fun width count base limit premise ->
    ghost_ (remaining_capacity_def width count (limit - base));
    match count with
    | D.Z -> ghost_ (region_def width count base base); Some base
    | D.S rest ->
      (match reserve width rest base limit () with
      | None -> None
      | Some top ->
        if width > limit - top then None else
        let stop : W.limb = top + width in
        ghost_ (ordered width rest base top (); Stack.previous_def width stop; region_def width count base stop);
        Some stop)
