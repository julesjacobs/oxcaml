module D = Hm_declarative
module K = Hmc_closure_ir
module P = Hmc_closure_program
module M = Hmc_monomorphic
module T = Hmc_templates
module R = Hmc_closure_semantics
module G = Hmc_cfg_ir
module C = Hmc_cfg_program
module S = Hmc_cfg_semantics
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module H = Hmc_frame_shape
module V = Hmc_frame_values
module E = Hmc_frame_edges

let (step @ total) : (p : I.program) @ immutable -> (state : S.state) @ immutable ->
    {u : unit | H.state p.I.origin.C.origin.P.table p.I.origin.C.blocks state} ->
    {u : unit | H.state p.I.origin.C.origin.P.table p.I.origin.C.blocks (U.step p state)} @ ghost = fun p state premise -> ghost_ (
  let origin = p.I.origin in
  let table = origin.C.origin.P.table in
  let blocks = origin.C.blocks in
  I.valid_def p; H.state_def table blocks state;
  match state with
  | S.Done _ | S.Stuck -> U.step_def p state
  | S.Running (a, frames) ->
    (match I.find p.I.sites a.S.pc with
    | None ->
      Hmc_tail_execution.unselected_def p state; Hmc_tail_execution.ordinary p state ();
      Hmc_frame_step.step origin state ()
    | Some exit ->
      I.find_valid blocks p.I.sites a.S.pc exit ();
      Hmc_cfg_origin.instruction_def blocks a.S.pc (G.Call (Hmc_tail_sites.entry exit));
      H.at_label_def table blocks a;
      (match G.lookup blocks a.S.pc with None -> () | Some block ->
        C.valid_def origin;
        Hmc_cfg_extension.lookup_valid (M.manifest origin.C.origin.P.origin.M.definitions) table blocks a.S.pc block ();
        G.block_valid_def (M.manifest origin.C.origin.P.origin.M.definitions) table blocks block;
        H.activation_def table block.G.signature a;
        H.temporaries_def table block.G.signature.G.temporaries a.S.temporaries;
        I.lookup_related blocks p.I.code p.I.sites a.S.pc (); I.select_def p.I.sites a.S.pc block.G.instruction;
        U.step_def p state;
        (match block.G.signature.G.temporaries, a.S.temporaries with
        | G.Value _, S.Value (R.V.Closure (id, captured), _, _) ->
          (match K.lookup table id with None -> () | Some callee ->
            let code = C.lookup_origin blocks table origin.C.functions id callee () in
            let _entered = E.enter origin id captured a.S.accumulator callee code () in ())
        | _ -> ());
        H.state_def table blocks (U.step p state))))
let (initial @ total) : (p : I.program) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    {u : unit | H.state p.I.origin.C.origin.P.table p.I.origin.C.blocks (U.initial p input)} @ ghost = fun p input -> ghost_ (
  let origin = p.I.origin in let closure = origin.C.origin in
  let table = closure.P.table in let id = closure.P.entry in
  C.valid_def origin; P.valid_def closure;
  V.closure (M.manifest closure.P.origin.M.definitions) table D.Empty_context id
    (D.Function (D.Word64, D.Word64)) closure.P.origin.M.source.T.derivation ();
  H.value_def table (R.V.Closure (id, R.V.Empty)); H.first_class_def (R.V.Closure (id, R.V.Empty));
  H.valid_def table (R.V.Closure (id, R.V.Empty)); H.valid_def table R.V.Empty; H.environment_def D.Empty_context R.V.Empty;
  H.value_def table (R.V.Word input); H.first_class_def (R.V.Word input); H.valid_def table (R.V.Word input);
  U.initial_def p input; S.initial_def origin input;
  (match K.lookup table id with None -> () | Some callee ->
    let code = C.lookup_origin origin.C.blocks table origin.C.functions id callee () in
    let _entered = E.enter origin id R.V.Empty (R.V.Word input) callee code () in ());
  H.frames_def table origin.C.blocks S.Halt; H.state_def table origin.C.blocks (U.initial p input))
let rec (prefix @ total) : (p : I.program) @ immutable -> (fuel : D.index) @ immutable -> (state : S.state) @ immutable ->
    {u : unit | H.state p.I.origin.C.origin.P.table p.I.origin.C.blocks state} ->
    {u : unit | H.state p.I.origin.C.origin.P.table p.I.origin.C.blocks (U.advance p fuel state)} @ ghost = fun p fuel state premise -> ghost_ (
  U.advance_def p fuel state; match fuel with D.Z -> () | D.S n -> step p state (); prefix p n (U.step p state) ())
let (run @ total) : (p : I.program) @ immutable -> (input : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {state : S.state | H.state p.I.origin.C.origin.P.table p.I.origin.C.blocks state && state === U.advance p fuel (U.initial p input)} @ immutable =
  fun p input fuel -> ghost_ (initial p input; prefix p fuel (U.initial p input) ()); U.advance p fuel (U.initial p input)
