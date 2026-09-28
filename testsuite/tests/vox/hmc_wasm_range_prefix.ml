module B = Wasm_u32
module D = Hm_declarative
module Plan = Wasm_parallel_copy
module Range = Hmc_wasm_relayout
module Index = Hmc_u32_index
module Copy = Wasm_cross_copy_prefix
module Access = Wasm_memory_region

let rec (bounded @ total) : (plan : Plan.plan) @ immutable -> (tail : Plan.plan) @ immutable ->
    (source : int) -> (destination : int) -> (length : D.index) @ immutable -> (count : B.u32) ->
    (base : B.u32) -> (low : B.u32) -> (high : B.u32) ->
    {u : unit | Range.range_is plan source destination length tail && Index.represents length count
      && low <= base + 16 + 16 * destination && base + 16 + 16 * (destination + count) <= high
      && Copy.bounded tail base low high} ->
    {u : unit | Copy.bounded plan base low high} @ ghost =
  fun plan tail source destination length count base low high premise -> ghost_ (
    Range.range_is_def plan source destination length tail; Index.represents_def length count;
    match length with
    | D.Z -> ()
    | D.S rest_length -> match plan with
      | Plan.Copy (_, _, (Plan.Copy (_, _, rest) as second)) ->
        bounded rest tail (source + 1) (destination + 1) rest_length (count - 1) base low high ();
        Copy.bounded_def plan base low high; Copy.bounded_def second base low high
      | _ -> ())

let rec (access_bounds @ total) : (plan : Plan.plan) @ immutable -> (tail : Plan.plan) @ immutable ->
    (source : int) -> (destination : int) -> (length : D.index) @ immutable -> (count : B.u32) ->
    (source_base : B.u32) -> (target_base : B.u32) -> (bounds : Access.bounds) @ immutable ->
    {u : unit | Range.range_is plan source destination length tail && Index.represents length count
      && bounds.Access.read_low <= source_base + 16 + 16 * source
      && source_base + 16 + 16 * (source + count) <= bounds.Access.read_high
      && bounds.Access.write_low <= target_base + 16 + 16 * destination
      && target_base + 16 + 16 * (destination + count) <= bounds.Access.write_high
      && Copy.access_bounds tail source_base target_base bounds} ->
    {u : unit | Copy.access_bounds plan source_base target_base bounds} @ ghost =
  fun plan tail source destination length count source_base target_base bounds premise -> ghost_ (
    Range.range_is_def plan source destination length tail; Index.represents_def length count;
    match length with
    | D.Z -> ()
    | D.S rest_length -> match plan with
      | Plan.Copy (_, _, (Plan.Copy (_, _, rest) as second)) ->
        access_bounds rest tail (source + 1) (destination + 1) rest_length (count - 1) source_base target_base bounds ();
        Copy.access_bounds_def plan source_base target_base bounds; Copy.access_bounds_def second source_base target_base bounds
      | _ -> ())
