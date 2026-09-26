module D = Hm_declarative
module R = Hmc_closure_semantics
module G = Hmc_cfg_ir
module C = Hmc_cfg_program
module P = Hmc_closure_program
module S = Hmc_cfg_semantics
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module H = Hmc_frame_shape
module F = Hmc_frame_codec
module Cap = Hmc_frame_capacity
module Demand = Hmc_heap_demand
module Height = Hmc_cfg_height

let rec (le_agrees @ total) : (a : D.index) @ immutable -> (b : D.index) @ immutable ->
    {u : unit | Height.le a b = Cap.le a b} @ ghost = fun a b -> ghost_ (
  Height.le_def a b; Cap.le_def a b;
  match a, b with D.S a, D.S b -> le_agrees a b | _ -> ())

let rec (environment_size @ total) : (context : D.context) @ immutable -> (env : R.V.value) @ immutable ->
    {u : unit | H.environment context env} ->
    {u : unit | Demand.environment_size env === F.locals_size context} @ ghost = fun context env premise -> ghost_ (
  H.environment_def context env; Demand.environment_size_def env; F.locals_size_def context;
  match context, env with D.Binding (_, rest), R.V.Bind (_, tail) -> environment_size rest tail () | _ -> ())

let (cells @ total) : (program : I.program) @ immutable -> (state : S.state) @ immutable ->
    {u : unit | H.state program.I.origin.C.origin.P.table program.I.origin.C.blocks state} ->
    {u : unit | Height.le (Demand.cells program state) (Cap.capacity program.I.origin.C.blocks)} @ ghost =
  fun program state premise -> ghost_ (
    let table = program.I.origin.C.origin.P.table in
    let blocks = program.I.origin.C.blocks in
    H.state_def table blocks state; Demand.cells_def program state;
    Height.le_def D.Z (Cap.capacity blocks);
    match state with S.Done _ | S.Stuck -> () | S.Running (a, _) ->
      H.at_label_def table blocks a;
      match G.lookup blocks a.S.pc with None -> () | Some block ->
        Cap.lookup blocks a.S.pc block ();
        le_agrees (F.size block.G.signature) (Cap.capacity blocks);
        H.activation_def table block.G.signature a;
        H.locals_def table block.G.signature.G.locals a.S.env;
        environment_size block.G.signature.G.locals a.S.env ();
        F.size_def block.G.signature;
        let locals = F.locals_size block.G.signature.G.locals in
        let temporaries = F.temporaries_size block.G.signature.G.temporaries in
        Height.prefix_le locals temporaries;
        Height.weaken locals (D.add locals temporaries) ();
        Height.le_def (D.S locals) (F.size block.G.signature);
        Height.le_def (D.S (D.S D.Z)) (F.size block.G.signature);
        Height.le_def (D.S D.Z) (D.S (D.add locals temporaries));
        Height.le_def D.Z (D.add locals temporaries);
        match I.lookup program.I.code a.S.pc with
        | Some (I.Keep (G.Load (G.Closure _, _, _, _))) ->
          Height.transitive (D.S locals) (F.size block.G.signature) (Cap.capacity blocks) ()
        | Some (I.Keep (G.Cons _)) -> (match a.S.temporaries with S.Value _ ->
          Height.transitive (D.S (D.S D.Z)) (F.size block.G.signature) (Cap.capacity blocks) () | _ -> ())
        | _ -> ())

let[@def] rec (product @ total) (steps : D.index @ immutable) (width : D.index @ immutable) = ghost_ (
  match steps with D.Z -> D.Z | D.S rest -> D.add width (product rest width))

let (add_both @ total) : (a : D.index) @ immutable -> (b : D.index) @ immutable ->
    (c : D.index) @ immutable -> (d : D.index) @ immutable ->
    {u : unit | Height.le a b && Height.le c d} ->
    {u : unit | Height.le (D.add a c) (D.add b d)} @ ghost = fun a b c d premise -> ghost_ (
  Height.add_monotone a c d ();
  Height.add_monotone d a b ();
  Height.add_commute d a; Height.add_commute d b;
  Height.transitive (D.add a c) (D.add a d) (D.add b d) ())

let rec (heap_plan @ total) : (program : I.program) @ immutable -> (fuel : D.index) @ immutable ->
    (state : S.state) @ immutable ->
    {u : unit | H.state program.I.origin.C.origin.P.table program.I.origin.C.blocks state} ->
    {u : unit | Height.le (Demand.heap_plan program fuel state)
      (product fuel (Cap.capacity program.I.origin.C.blocks))} @ ghost = fun program fuel state premise -> ghost_ (
  Demand.heap_plan_def program fuel state; product_def fuel (Cap.capacity program.I.origin.C.blocks);
  match fuel with
  | D.Z -> Height.le_def D.Z D.Z
  | D.S rest ->
    cells program state ();
    Hmc_frame_reachable.step program state ();
    heap_plan program rest (U.step program state) ();
    add_both (Demand.cells program state) (Cap.capacity program.I.origin.C.blocks)
      (Demand.heap_plan program rest (U.step program state))
      (product rest (Cap.capacity program.I.origin.C.blocks)) ())

let[@def] (depth @ total) (state : S.state @ immutable) = ghost_ (
  match state with S.Running (_, frames) -> S.depth frames | _ -> D.Z)

let (stack_step @ total) : (program : I.program) @ immutable -> (state : S.state) @ immutable ->
    {u : unit | Height.le (Demand.stack program state) (D.S (depth state))
      && Height.le (depth (U.step program state)) (D.S (depth state))} @ ghost = fun program state -> ghost_ (
  depth_def state; Demand.stack_def program state; U.step_def program state;
  Height.grow (depth state); Height.reflexive (D.S (depth state));
  Height.le_def D.Z (D.S (depth state));
  match state with
  | S.Done _ | S.Stuck -> depth_def (U.step program state)
  | S.Running (a, frames) ->
    (match frames with S.Halt -> S.depth_def frames | S.Frame (_, rest) ->
      S.depth_def frames; Height.grow (S.depth rest);
      Height.weaken (S.depth rest) (S.depth frames) ());
    match I.lookup program.I.code a.S.pc with
    | None -> depth_def S.Stuck
    | Some instruction ->
      (match instruction with
      | I.Keep (G.Call next) ->
        (match a.S.temporaries with S.Value (_, env, rest) ->
          S.depth_def (S.Frame ({a with pc = next; env; temporaries = rest}, frames)) | _ -> ())
      | _ -> ());
      depth_def (U.step program state))

let rec (max_le @ total) : (a : D.index) @ immutable -> (b : D.index) @ immutable ->
    (limit : D.index) @ immutable -> {u : unit | Height.le a limit && Height.le b limit} ->
    {u : unit | Height.le (Cap.max a b) limit} @ ghost = fun a b limit premise -> ghost_ (
  Cap.max_def a b; Height.le_def a limit; Height.le_def b limit;
  Height.le_def (Cap.max a b) limit;
  match a, b, limit with D.S a, D.S b, D.S limit -> max_le a b limit () | _ -> ())

let rec (stack_plan @ total) : (program : I.program) @ immutable -> (fuel : D.index) @ immutable ->
    (state : S.state) @ immutable ->
    {u : unit | Height.le (Demand.stack_plan program fuel state) (D.add fuel (depth state))} @ ghost =
  fun program fuel state -> ghost_ (
    Demand.stack_plan_def program fuel state; D.add_def fuel (depth state);
    match fuel with
    | D.Z -> Height.le_def D.Z (depth state)
    | D.S rest ->
      stack_step program state;
      stack_plan program rest (U.step program state);
      Height.add_monotone rest (depth (U.step program state)) (D.S (depth state)) ();
      Height.add_successor rest (depth state);
      Height.transitive (Demand.stack_plan program rest (U.step program state))
        (D.add rest (depth (U.step program state))) (D.add fuel (depth state)) ();
      Height.prefix_le (depth state) rest;
      Height.add_commute (depth state) rest;
      Height.le_def (D.S (depth state)) (D.add fuel (depth state));
      Height.transitive (Demand.stack program state) (D.S (depth state)) (D.add fuel (depth state)) ();
      max_le (Demand.stack program state) (Demand.stack_plan program rest (U.step program state))
        (D.add fuel (depth state)) ())

let rec (product_monotone @ total) : (a : D.index) @ immutable -> (b : D.index) @ immutable ->
    (width : D.index) @ immutable -> {u : unit | Height.le a b} ->
    {u : unit | Height.le (product a width) (product b width)} @ ghost = fun a b width premise -> ghost_ (
  Height.le_def a b; product_def a width; product_def b width;
  match a, b with
  | D.Z, _ -> Height.le_def D.Z (product b width)
  | D.S a, D.S b -> product_monotone a b width (); Height.add_monotone width (product a width) (product b width) ()
  | _ -> ())

let (initial_depth @ total) : (program : I.program) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    {u : unit | depth (U.initial program input) === D.Z} @ ghost = fun program input -> ghost_ (
  U.initial_def program input; S.initial_def program.I.origin input;
  S.depth_def S.Halt; depth_def (U.initial program input))

let (initial_plans @ total) : (program : I.program) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    (fuel : D.index) @ immutable -> (limit : D.index) @ immutable -> {u : unit | Height.le fuel limit} ->
    {u : unit | Height.le (Demand.heap_plan program fuel (U.initial program input))
        (product limit (Cap.capacity program.I.origin.C.blocks))
      && Height.le (Demand.stack_plan program fuel (U.initial program input)) limit} @ ghost =
  fun program input fuel limit premise -> ghost_ (
    Hmc_frame_reachable.initial program input;
    heap_plan program fuel (U.initial program input) ();
    product_monotone fuel limit (Cap.capacity program.I.origin.C.blocks) ();
    Height.transitive (Demand.heap_plan program fuel (U.initial program input))
      (product fuel (Cap.capacity program.I.origin.C.blocks))
      (product limit (Cap.capacity program.I.origin.C.blocks)) ();
    initial_depth program input;
    stack_plan program fuel (U.initial program input);
    Height.add_zero fuel;
    Height.transitive (Demand.stack_plan program fuel (U.initial program input)) fuel limit ())
