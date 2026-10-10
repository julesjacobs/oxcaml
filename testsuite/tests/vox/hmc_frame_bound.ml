module D = Hm_declarative
module C = Hmc_cfg_program
module S = Hmc_cfg_semantics
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module T = Hmc_tail_stack
module F = Hmc_frame_codec
module B = Hmc_frame_capacity
module M = Hmc_frame_storage

let[@def] rec (stack_cells @ total) (stack : M.stack @ immutable) = match stack with
  | M.Empty -> D.Z | M.Saved (frame, rest) -> D.add (F.length frame.M.slots) (stack_cells rest)
let[@def] (live_cells @ total) (state : M.state @ immutable) = match state with
  | M.Running (frame, stack) -> D.add (F.length frame.M.slots) (stack_cells stack)
  | _ -> D.Z
let (zero_frames @ total) : (capacity : D.index) @ immutable -> (state : M.state) @ immutable ->
    {u : unit | M.fits capacity state && M.saved_depth state === D.Z} ->
    {u : unit | B.le (live_cells state) capacity} @ ghost = fun capacity state premise -> ghost_ (
  M.fits_def capacity state; M.saved_depth_def state; live_cells_def state;
  match state with
  | M.Running (_, stack) ->
    M.depth_def stack;
    (match stack with
    | M.Empty -> stack_cells_def stack; Hm_abstraction_proofs.add_zero capacity; B.reflexive capacity
    | M.Saved _ -> ())
  | _ -> B.le_def D.Z capacity)
let (constant_slots @ total) : (p : I.program) @ immutable -> (input : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | T.no_calls p.I.code} ->
    {out : M.state | M.decode_state p.I.origin.C.blocks out === Some (U.advance p fuel (U.initial p input))
      && B.le (live_cells out) (B.capacity p.I.origin.C.blocks)} @ immutable = fun p input fuel premise ->
  let out = M.run p input fuel in
  ghost_ (T.constant_stack p input fuel ();
    T.stack_bound_def D.Z (U.advance p fuel (U.initial p input));
    M.runtime_depth_def (U.advance p fuel (U.initial p input));
    (match U.advance p fuel (U.initial p input) with
    | S.Running (_, frames) -> T.bounded_def D.Z frames; S.depth_def frames
    | _ -> ());
    zero_frames (B.capacity p.I.origin.C.blocks) out ());
  out
