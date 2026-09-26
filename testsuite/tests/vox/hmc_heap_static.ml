module D = Hm_declarative
module K = Hmc_closure_ir
module O = Hmc_closure_program
module N = Hmc_monomorphic
module M = Hmc_heap_objects
module R = Hmc_closure_semantics
module H = Hmc_frame_shape
module V = Hmc_frame_values
module P = Hmc_heap_preservation
module A = Hmc_heap_allocate
module X = Hmc_heap_machine
module J = Hmc_heap_globals
module E = Hmc_heap_extent
module Math = Hmc_heap_extent_math

let[@def] rec (closed @ total) (table : K.table @ immutable) (globals : O.globals @ immutable) = ghost_ (match globals with
  | O.No_globals -> true
  | O.Global (code, rest) -> H.value table (R.V.Closure (code, R.V.Empty)) && closed table rest)
let rec (mapped_closed @ total) : (interface : Hmc_manifest.table) @ immutable -> (table : K.table) @ immutable ->
    (source : N.definitions) @ immutable -> (globals : O.globals) @ immutable ->
    {u : unit | O.mapped interface table source globals} -> {u : unit | closed table globals} @ ghost =
  fun interface table source globals premise -> ghost_ (
    closed_def table globals; O.mapped_def interface table source globals;
    match source, globals with
    | N.Definition (_, remaining), O.Global (code, rest) ->
      O.lookup_def globals (O.size rest);
      let _ = Hm_elaboration_check.index_equal (O.size rest) (O.size rest) in
      V.global interface table source globals (O.size rest) code ();
      mapped_closed interface table remaining rest ()
    | _ -> ())
let (object_valid @ total) : (table : K.table) @ immutable -> (heap : M.heap) @ immutable -> (code : D.index) @ immutable ->
    {u : unit | H.value table (R.V.Closure (code, R.V.Empty))} ->
    {u : unit | M.object_valid table (M.view heap) (M.Closure (code, M.Empty))} @ ghost = fun table heap code premise -> ghost_ (
  M.object_valid_def table (M.view heap) (M.Closure (code, M.Empty));
  M.decode_object_def (M.view heap) (M.Closure (code, M.Empty)); M.decode_environment_def (M.view heap) M.Empty)
type result = Ready of M.heap * X.globals | Full of M.heap * D.index [@@inductive]
let[@def] (correct @ total) (table : K.table @ immutable) (source : O.globals @ immutable) (initial : M.heap @ immutable)
    (limit : Hmc_word64.limb) (result : result @ immutable) = ghost_ (match result with
  | Ready (heap, globals) -> M.valid table heap && P.extends heap initial && M.used heap <= limit && J.related heap source globals
    && E.span (O.size source) (M.used initial) (M.used heap)
  | Full (heap, code) -> M.valid table heap && P.extends heap initial && M.used heap <= limit
    && not (E.fits (O.size source) (M.used initial) limit)
    && H.value table (R.V.Closure (code, R.V.Empty)) && not (E.fits (M.slots (M.Closure (code, M.Empty))) (M.used heap) limit))
let rec (initialize @ total) : (table : K.table) @ immutable -> (source : O.globals) @ immutable ->
    (heap : M.heap) @ immutable -> (limit : Hmc_word64.limb) ->
    {u : unit | closed table source && M.valid table heap && M.used heap <= limit} ->
    {out : result | correct table source heap limit out} @ immutable = fun table source heap limit premise ->
  ghost_ (closed_def table source; O.size_def source);
  match source with
  | O.No_globals ->
    let out = Ready (heap, X.Empty_globals) in
    ghost_ (E.span_def D.Z (M.used heap) (M.used heap); P.extends_def heap heap; J.related_def heap source X.Empty_globals; correct_def table source heap limit out); out
  | O.Global (code, rest) ->
    let previous = initialize table rest heap limit () in
    ghost_ (correct_def table rest heap limit previous);
    match previous with
    | Full (partial, pending) ->
      let out = Full (partial, pending) in ghost_ (
        Math.add_one (O.size rest);
        if E.fits (O.size source) (M.used heap) limit then Math.prefix (O.size rest) (D.S D.Z) (M.used heap) limit () else ();
        correct_def table source heap limit out); out
    | Ready (partial, globals) ->
      ghost_ (object_valid table partial code ());
      let allocation = A.allocate table partial limit (M.Closure (code, M.Empty)) () in
      ghost_ (A.correct_def table partial limit (M.Closure (code, M.Empty)) allocation);
      match allocation with
      | A.Exhausted ->
        let out = Full (partial, code) in ghost_ (
          Math.add_one (O.size rest); M.slots_def (M.Closure (code, M.Empty)); M.length_def M.Empty;
          if E.fits (O.size source) (M.used heap) limit then Math.consume (O.size rest) (D.S D.Z) (M.used heap) (M.used partial) limit () else ();
          correct_def table source heap limit out); out
      | A.Allocated allocated ->
        let out_globals = X.Global (O.size rest, allocated.A.reference, globals) in
        ghost_ (P.transitive allocated.A.heap partial heap ();
          M.slots_def (M.Closure (code, M.Empty)); M.length_def M.Empty; Math.add_one (O.size rest);
          Math.join (O.size rest) (D.S D.Z) (M.used heap) (M.used partial) (M.used allocated.A.heap) ();
          J.preserve table allocated.A.heap partial rest globals ();
          M.decode_object_def (M.view partial) (M.Closure (code, M.Empty)); M.decode_environment_def (M.view partial) M.Empty;
          J.related_def allocated.A.heap source out_globals);
        let out = Ready (allocated.A.heap, out_globals) in ghost_ (correct_def table source heap limit out); out
