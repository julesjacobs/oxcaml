module B = Wasm_u32
module D = Hm_declarative
module R = Hmc_runtime_closures
module Descriptor = Hmc_runtime_descriptor
module Index = Hmc_u32_index
module S = Wasm_scalar
module P = Hmc_linear_preservation
module L = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module V = Hmc_tagged_cell
type count = {n : B.u32 | n <= 134217727}
let[@def] (address @ total) (base : B.u32) (index : count) : B.u32 = S.add32 base (32 * index)
let[@def] rec (related @ total) (table : R.table @ immutable) (memory : B.bytes @ immutable) (base : B.u32) (count : count) = ghost_ (
  match table with
  | R.Empty -> count = 0
  | R.Add (descriptor, rest) -> count > 0 && base + 32 * count <= 4294967295
      && Descriptor.load memory (address base (count - 1)) === Some descriptor
      && related rest memory base (count - 1))
let rec (preserve @ total) : (table : R.table) @ immutable -> (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (base : B.u32) -> (count : count) -> (boundary : B.u32) ->
    {u : unit | related table before base count && base + 32 * count <= boundary && P.equal_prefix boundary before after} ->
    {u : unit | related table after base count} @ ghost =
  fun table before after base count boundary premise -> ghost_ (
    related_def table before base count; related_def table after base count;
    match table with
    | R.Empty -> ()
    | R.Add (descriptor, rest) ->
      address_def base (count - 1); S.add32_def base (32 * (count - 1));
      Descriptor.slots_def ();
      Index.represents_def (D.S (D.S D.Z)) 2; Index.represents_def (D.S D.Z) 1; Index.represents_def D.Z 0;
      (match Hmc_wasm_reservation.reserve (Descriptor.slots ()) 2 (address base (count - 1)) boundary () with
      | None -> unreachable_ ()
      | Some stop ->
        Hmc_memory_cells.preserve before after boundary (address base (count - 1)) stop (Descriptor.slots ()) ());
      Descriptor.load_def before (address base (count - 1)); Descriptor.load_def after (address base (count - 1));
      preserve rest before after base (count - 1) boundary ())
let rec (size @ total) : (table : R.table) @ immutable -> (memory : B.bytes) @ immutable -> (base : B.u32) -> (count : count) ->
    {u : unit | related table memory base count} -> {u : unit | Index.represents (R.size table) count} @ ghost =
  fun table memory base count premise -> ghost_ (
    related_def table memory base count; R.size_def table; Index.represents_def (R.size table) count;
    match table with R.Empty -> () | R.Add (_, rest) -> size rest memory base (count - 1) ())
let rec (lookup @ total) : (table : R.table) @ immutable -> (memory : B.bytes) @ immutable -> (base : B.u32) -> (count : count) ->
    (code : count) -> (descriptor : R.descriptor) @ immutable ->
    {u : unit | related table memory base count && R.lookup table code === Some descriptor} ->
    {u : unit | code < count && base + 32 * code + 32 <= base + 32 * count
      && Descriptor.load memory (address base code) === Some descriptor} @ ghost =
  fun table memory base count code descriptor premise -> ghost_ (
    related_def table memory base count; R.lookup_def table code;
    match table with
    | R.Empty -> ()
    | R.Add (_, rest) ->
      size rest memory base (count - 1) ();
      if Index.represents (R.size rest) code then Index.unique (R.size rest) code (count - 1) ()
      else lookup rest memory base (count - 1) code descriptor ())
let rec (store @ total) : (table : R.table) @ immutable -> (memory : B.bytes) @ immutable ->
    (base : B.u32) -> (count : count) -> (limit : B.u32) ->
    {u : unit | Index.represents (R.size table) count && base + 32 * count <= limit && Bounds.covers memory limit} ->
    {out : B.bytes | related table out base count && V.length out === V.length memory && Bounds.covers out limit
      && P.equal_prefix base memory out && L.drop out (address base count) === L.drop memory (address base count)} @ immutable =
  fun table memory base count limit premise ->
    ghost_ (R.size_def table; Index.represents_def (R.size table) count; address_def base count; S.add32_def base (32 * count));
    match table with
    | R.Empty ->
      ghost_ (related_def table memory base count; Hmc_heap_image_prefix.reflexive memory limit (); P.shrink limit base memory memory ());
      memory
    | R.Add (descriptor, rest) ->
      let previous = store rest memory base (count - 1) limit () in
      let at = address base (count - 1) in
      ghost_ (address_def base (count - 1); S.add32_def base (32 * (count - 1));
        Descriptor.slots_def ();
        Index.represents_def (D.S (D.S D.Z)) 2; Index.represents_def (D.S D.Z) 1; Index.represents_def D.Z 0);
      match Hmc_wasm_reservation.reserve (Descriptor.slots ()) 2 at limit () with
      | None -> unreachable_ ()
      | Some stop ->
        let out = Descriptor.store previous limit at stop descriptor () in
        ghost_ (preserve rest previous out base (count - 1) at ();
          P.shrink at base previous out ();
          Hmc_heap_image_prefix.transitive memory previous out base ();
          let _ = Bounds.suffix memory limit at () in
          Hmc_heap_image_suffix.seek memory previous at stop ();
          related_def table out base count);
        out
