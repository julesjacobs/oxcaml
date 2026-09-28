module W = Hmc_word64
module B = Wasm_u32
module C = Hmc_tagged_cell
module M = Hmc_heap_objects
module K = Hmc_closure_ir
module P = Hmc_heap_preservation
module E = Hmc_heap_extent
module Image = Hmc_heap_image
module Bounds = Hmc_linear_bounds

let rec (growth @ total) : (table : K.table) @ immutable -> (larger : M.heap) @ immutable -> (smaller : M.heap) @ immutable ->
    {u : unit | M.valid table larger && M.valid table smaller && P.extends larger smaller} ->
    {u : unit | M.used smaller <= M.used larger && (M.used smaller <> M.used larger || smaller === larger)} @ ghost =
  fun table larger smaller premise -> ghost_ (
    P.extends_def larger smaller;
    if larger === smaller then () else (
      M.valid_def table larger; M.used_def larger;
      match larger with M.Empty_heap _ -> () | M.Allocate (a, rest) ->
        growth table rest smaller (); M.slots_def a.M.object_;
        E.ordered (M.slots a.M.object_) a.M.address a.M.stop ()))
let rec (synchronize @ total) : (table : K.table) @ immutable -> (capacity : W.limb) -> (memory : B.bytes) @ immutable ->
    (larger : M.heap) @ immutable -> (smaller : M.heap) @ immutable -> (limit : W.limb) ->
    {u : unit | M.valid table larger && M.valid table smaller && P.extends larger smaller && Image.related memory smaller
      && M.used larger <= limit && Image.encodable capacity larger && Bounds.covers memory limit} ->
    {out : B.bytes | Image.related out larger && Bounds.covers out limit && C.length out === C.length memory} @ immutable =
  fun table capacity memory larger smaller limit premise ->
    ghost_ (growth table larger smaller ());
    if M.used larger = M.used smaller then memory else (
      ghost_ (M.valid_def table larger; M.used_def larger; P.extends_def larger smaller; Image.encodable_def capacity larger);
      match larger with
      | M.Empty_heap _ -> unreachable_ ()
      | M.Allocate (a, rest) ->
        ghost_ (E.ordered (M.slots a.M.object_) a.M.address a.M.stop ());
        let previous = synchronize table capacity memory rest smaller limit () in
        Image.store table capacity previous rest limit a ())
