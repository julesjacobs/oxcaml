module D = Hm_declarative
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module G = Hmc_cfg_ir
module S = Hmc_cfg_semantics
module T = Hmc_tail_stack
module N = Hmc_heap_demand
module Cap = Hmc_frame_capacity

let[@def] (allocates @ total) (instruction : I.instruction @ immutable) = match instruction with
  | I.Keep (G.Load (G.Closure _, _, _, _)) | I.Keep (G.Cons _) -> true | _ -> false
let[@def] rec (no_allocations @ total) (code : I.table @ immutable) = match code with
  | I.Empty -> true | I.Add (instruction, rest) -> not (allocates instruction) && no_allocations rest
let rec (lookup @ total) : (code : I.table) @ immutable -> (label : D.index) @ immutable ->
    {u : unit | no_allocations code} ->
    {u : unit | match I.lookup code label with None -> true | Some instruction -> not (allocates instruction)} @ ghost =
  fun code label premise -> ghost_ (
    no_allocations_def code; I.lookup_def code label;
    match code with I.Empty -> () | I.Add (_, rest) ->
      if Hm_elaboration_check.index_equal label (I.size rest) then () else lookup rest label ())
let rec (heap_plan @ total) : (program : I.program) @ immutable -> (fuel : D.index) @ immutable -> (state : S.state) @ immutable ->
    {u : unit | no_allocations program.I.code} -> {u : unit | N.heap_plan program fuel state === D.Z} @ ghost =
  fun program fuel state premise -> ghost_ (
    N.heap_plan_def program fuel state;
    match fuel with D.Z -> () | D.S rest ->
      N.cells_def program state;
      (match state with
      | S.Running (a, _) -> lookup program.I.code a.S.pc ();
        (match I.lookup program.I.code a.S.pc with None -> () | Some op -> allocates_def op)
      | _ -> ());
      heap_plan program rest (U.step program state) (); D.add_def D.Z D.Z)
let rec (stack_plan @ total) : (program : I.program) @ immutable -> (fuel : D.index) @ immutable -> (state : S.state) @ immutable ->
    {u : unit | T.no_calls program.I.code && T.stack_bound D.Z state} ->
    {u : unit | N.stack_plan program fuel state === D.Z} @ ghost = fun program fuel state premise -> ghost_ (
  N.stack_plan_def program fuel state;
  match fuel with D.Z -> () | D.S rest ->
    T.stack_bound_def D.Z state; N.stack_def program state;
    (match state with S.Running (a, frames) ->
      T.bounded_def D.Z frames; S.depth_def frames;
      T.lookup_no_calls program.I.code a.S.pc ();
      (match I.lookup program.I.code a.S.pc with None -> () | Some op -> T.ordinary_call_def op)
    | _ -> ());
    T.step program D.Z state (); stack_plan program rest (U.step program state) (); Cap.max_def D.Z D.Z)
