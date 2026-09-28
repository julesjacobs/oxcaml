module D = Hm_declarative
module W = Hmc_word64
module M = Hmc_heap_objects
module V = Hmc_tagged_cell
module R = Hmc_closure_semantics
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module X = Hmc_heap_machine
module L = Hmc_heap_machine_proofs
module B = Hmc_heap_static
module A = Hmc_heap_allocate
module P = Hmc_heap_preservation
module J = Hmc_heap_globals
module H = Hmc_heap_invariant
module K = Hmc_closure_ir
module C = Hmc_cfg_program
module O = Hmc_closure_program
module N = Hmc_monomorphic
module T = Hmc_templates
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module S = Hmc_cfg_semantics
module E = Hmc_heap_extent
module Math = Hmc_heap_extent_math

let[@def] (cells @ total) (program : I.program @ immutable) = D.S (O.size program.I.origin.C.origin.O.globals)

type start = {globals : X.globals; configuration : X.configuration}
type result = Initialized of start | Heap_exhausted of M.heap * D.index [@@inductive]
let[@def] (correct @ total) (program : I.program @ immutable) (base : W.limb) (limit : W.limb)
    (input : W.t @ immutable) (out : result @ immutable) = ghost_ (match out with
  | Initialized start -> H.valid program start.globals limit start.configuration (U.initial program input)
    && (match start.configuration.X.state with Q.Running (_, Q.Halt) -> true | _ -> false)
    && P.extends start.configuration.X.heap (M.Empty_heap base)
    && E.span (cells program) base (M.used start.configuration.X.heap)
  | Heap_exhausted (heap, code) -> M.valid program.I.origin.C.origin.O.table heap
    && P.extends heap (M.Empty_heap base) && M.used heap <= limit
    && not (E.fits (cells program) base limit)
    && Hmc_frame_shape.value program.I.origin.C.origin.O.table (R.V.Closure (code, R.V.Empty))
    && not (E.fits (M.slots (M.Closure (code, M.Empty))) (M.used heap) limit))
let (initialize @ total) : (program : I.program) @ immutable -> (base : W.limb) -> (limit : W.limb) ->
    (input : W.t) @ immutable -> {u : unit | base <= limit} ->
    {out : result | correct program base limit input out} @ immutable = fun program base limit input premise ->
  let closure = program.I.origin.C.origin in
  let table = closure.O.table in
  let heap = M.Empty_heap base in
  ghost_ (cells_def program; Math.add_one (O.size closure.O.globals); O.valid_def closure; M.valid_def table heap; M.used_def heap;
    B.mapped_closed (N.manifest closure.O.origin.N.definitions) table closure.O.origin.N.definitions closure.O.globals ());
  let globals = B.initialize table closure.O.globals heap limit () in
  ghost_ (B.correct_def table closure.O.globals heap limit globals);
  match globals with
  | B.Full (partial, code) ->
    let out = Heap_exhausted (partial, code) in ghost_ (
      if E.fits (cells program) base limit then Math.prefix (O.size closure.O.globals) (D.S D.Z) base limit () else ();
      correct_def program base limit input out); out
  | B.Ready (partial, globals) ->
    let id = closure.O.entry in
    ghost_ (Hmc_frame_values.closure (N.manifest closure.O.origin.N.definitions) table D.Empty_context id
      (D.Function (D.Word64, D.Word64)) closure.O.origin.N.source.T.derivation ();
      Hmc_frame_shape.value_def table (R.V.Closure (id, R.V.Empty));
      Hmc_frame_shape.valid_def table (R.V.Closure (id, R.V.Empty)); Hmc_frame_shape.first_class_def (R.V.Closure (id, R.V.Empty));
      Hmc_frame_shape.valid_def table R.V.Empty; Hmc_frame_shape.environment_def D.Empty_context R.V.Empty;
      B.object_valid table partial id ());
    let allocation = A.allocate table partial limit (M.Closure (id, M.Empty)) () in
    ghost_ (A.correct_def table partial limit (M.Closure (id, M.Empty)) allocation);
    match allocation with
    | A.Exhausted ->
      let out = Heap_exhausted (partial, id) in ghost_ (
        M.slots_def (M.Closure (id, M.Empty)); M.length_def M.Empty;
        if E.fits (cells program) base limit then Math.consume (O.size closure.O.globals) (D.S D.Z) base (M.used partial) limit () else ();
        correct_def program base limit input out); out
    | A.Allocated allocated ->
      ghost_ (M.slots_def (M.Closure (id, M.Empty)); M.length_def M.Empty;
        Math.join (O.size closure.O.globals) (D.S D.Z) base (M.used partial) (M.used allocated.A.heap) ();
        M.decode_object_def (M.view partial) (M.Closure (id, M.Empty)); M.decode_environment_def (M.view partial) M.Empty;
        M.decode_def allocated.A.heap (V.Word input); M.decode_value_def (M.view allocated.A.heap) (V.Word input);
        L.invoke program allocated.A.heap allocated.A.reference (V.Word input) (R.V.Closure (id, R.V.Empty)) (R.V.Word input) ();
        L.abstract_invoke_def program (R.V.Closure (id, R.V.Empty)) (R.V.Word input);
        C.valid_def program.I.origin;
        (match K.lookup table id with None -> () | Some entry ->
          let _ = C.lookup_origin program.I.origin.C.blocks table program.I.origin.C.functions id entry () in ());
        U.initial_def program input; S.initial_def program.I.origin input);
      match X.invoke program allocated.A.heap allocated.A.reference (V.Word input) with
      | None -> unreachable_ ()
      | Some entered ->
        let configuration = {X.heap = allocated.A.heap; state = Q.Running (entered, Q.Halt)} in
        ghost_ (Q.decode_def allocated.A.heap configuration.X.state; Q.decode_frames_def allocated.A.heap Q.Halt;
          J.preserve table allocated.A.heap partial closure.O.globals globals ();
          P.transitive allocated.A.heap partial heap ();
          Hmc_frame_reachable.initial program input;
          H.valid_def program globals limit configuration (U.initial program input));
        let out = Initialized {globals; configuration} in ghost_ (correct_def program base limit input out); out

let (sufficient @ total) : (program : I.program) @ immutable -> (base : W.limb) -> (limit : W.limb) ->
    (input : W.t) @ immutable -> {u : unit | base <= limit && E.fits (cells program) base limit} ->
    {out : start | correct program base limit input (Initialized out)} @ immutable = fun program base limit input premise ->
  let out = initialize program base limit input () in
  ghost_ (correct_def program base limit input out);
  match out with Initialized start -> start | Heap_exhausted _ -> unreachable_ ()
