module B = Wasm_u32
module R = Hmc_runtime_closures
module T = Hmc_runtime_descriptor_table
module I = Hmc_u32_index
let rec (correct @ total) : (table : R.table) @ immutable -> (memory : B.bytes) @ immutable -> (base : B.u32) -> (count : T.count) ->
    (code : B.u32) -> (descriptor : R.descriptor) @ immutable ->
    {u : unit | T.related table memory base count && R.lookup table code === Some descriptor} ->
    {u : unit | code < count} @ ghost =
  fun table memory base count code descriptor premise -> ghost_ (
    T.related_def table memory base count; R.lookup_def table code;
    match table with
    | R.Empty -> ()
    | R.Add (_, rest) ->
      T.size rest memory base (count - 1) ();
      if I.represents (R.size rest) code then I.unique (R.size rest) code (count - 1) ()
      else correct rest memory base (count - 1) code descriptor ())
