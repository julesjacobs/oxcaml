module D = Hm_declarative
module M = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module X = Hmc_heap_machine
module B = Hmc_heap_step
module A = Hmc_heap_allocating
module J = Hmc_heap_globals
module H = Hmc_frame_shape
module V = Hmc_frame_values
module R = Hmc_closure_semantics
module S = Hmc_cfg_semantics
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module C = Hmc_cfg_program
module O = Hmc_closure_program
module N = Hmc_monomorphic

let (request_valid @ total) : (program : I.program) @ immutable -> (heap : M.heap) @ immutable ->
    (state : Q.state) @ immutable -> (abstract : S.state) @ immutable ->
    {u : unit | Q.decode heap state === Some abstract && H.state program.I.origin.C.origin.O.table program.I.origin.C.blocks abstract} ->
    {u : unit | match B.request program state with None -> true | Some object_ ->
      M.object_valid program.I.origin.C.origin.O.table (M.view heap) object_} @ ghost = fun program heap state abstract premise -> ghost_ (
  let table = program.I.origin.C.origin.O.table in
  let blocks = program.I.origin.C.blocks in
  let interface = N.manifest program.I.origin.C.origin.O.origin.N.definitions in
  B.request_def program state; Q.decode_def heap state; H.state_def table blocks abstract;
  match state with
  | Q.Done _ | Q.Stuck -> ()
  | Q.Running (a, _) ->
    F.decode_def heap a; F.decode_temporaries_def heap a.F.temporaries;
    (match F.decode heap a with None -> () | Some source_a ->
      H.at_label_def table blocks source_a;
      match G.lookup blocks a.F.pc with
      | None -> ()
      | Some block ->
        H.activation_def table block.G.signature source_a;
        H.temporaries_def table block.G.signature.G.temporaries source_a.S.temporaries;
        I.valid_def program; I.lookup_related blocks program.I.code program.I.sites a.F.pc ();
        I.select_def program.I.sites a.F.pc block.G.instruction;
        A.request_def block.G.instruction a;
        C.valid_def program.I.origin;
        Hmc_cfg_extension.lookup_valid interface table blocks a.F.pc block ();
        G.block_valid_def interface table blocks block;
        match block.G.instruction with
        | G.Load (G.Closure id, ty, derivation, _) ->
          let object_ = M.Closure (id, a.F.env) in
          M.object_valid_def table (M.view heap) object_; M.decode_object_def (M.view heap) object_;
          S.load_def program.I.origin.C.origin.O.globals source_a.S.env (G.Closure id);
          V.load program.I.origin block.G.signature.G.locals source_a.S.env (G.Closure id) ty derivation (R.V.Closure (id, source_a.S.env)) ()
        | G.Cons _ ->
          (match a.F.temporaries with
          | F.Value (head, _, _) ->
            let object_ = M.Cons (head, a.F.accumulator) in
            M.object_valid_def table (M.view heap) object_; M.decode_object_def (M.view heap) object_;
            M.decode_def heap head; M.decode_def heap a.F.accumulator;
            (match M.decode heap head, M.decode heap a.F.accumulator with
            | Some h, Some t -> H.value_def table h; H.value_def table t;
              H.value_def table (R.V.Cons (h, t)); H.valid_def table (R.V.Cons (h, t)); H.first_class_def (R.V.Cons (h, t))
            | _ -> ())
          | _ -> ())
        | _ -> ()))
let[@def] (valid @ total) (program : I.program @ immutable) (globals : X.globals @ immutable)
    (limit : Hmc_word64.limb) (configuration : X.configuration @ immutable) (abstract : S.state @ immutable) = ghost_ (
  M.valid program.I.origin.C.origin.O.table configuration.X.heap && M.used configuration.X.heap <= limit
  && J.related configuration.X.heap program.I.origin.C.origin.O.globals globals
  && Q.decode configuration.X.heap configuration.X.state === Some abstract
  && H.state program.I.origin.C.origin.O.table program.I.origin.C.blocks abstract)
let (step @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable ->
    (heap_limit : Hmc_word64.limb) -> (stack_limit : D.index) @ immutable -> (configuration : X.configuration) @ immutable ->
    (abstract : S.state) @ immutable -> {u : unit | valid program globals heap_limit configuration abstract} ->
    {u : unit | match X.step program globals heap_limit stack_limit configuration with
      | X.Exhausted reason -> B.exhausted program heap_limit stack_limit configuration reason
      | X.Advanced next -> valid program globals heap_limit next (U.step program abstract)} @ ghost =
  fun program globals heap_limit stack_limit configuration abstract premise -> ghost_ (
    valid_def program globals heap_limit configuration abstract;
    request_valid program configuration.X.heap configuration.X.state abstract ();
    B.ready_def program globals heap_limit configuration;
    B.step program globals heap_limit stack_limit configuration abstract ();
    Hmc_frame_reachable.step program abstract ();
    match X.step program globals heap_limit stack_limit configuration with
    | X.Exhausted _ -> ()
    | X.Advanced next -> valid_def program globals heap_limit next (U.step program abstract))
