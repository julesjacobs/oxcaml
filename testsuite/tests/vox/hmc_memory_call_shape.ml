module D = Hm_declarative
module M = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module X = Hmc_heap_machine
module S = Hmc_cfg_semantics
module G = Hmc_cfg_ir
module C = Hmc_cfg_program
module P = Hmc_closure_program
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module H = Hmc_heap_invariant
module Shape = Hmc_frame_shape
module Codec = Hmc_pointer_frame_codec
let rec (successor @ total) : (n : D.index) @ immutable -> {u : unit | D.present (D.S n) n} @ ghost = fun n -> ghost_ (
  D.present_def (D.S n) n; match n with D.Z -> () | D.S rest -> successor rest)
let (saved @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable ->
    (limit : Hmc_word64.limb) -> (heap : M.heap) @ immutable -> (a : F.activation) @ immutable -> (frames : Q.frames) @ immutable ->
    (abstract : S.state) @ immutable -> (next : D.index) @ immutable -> (closure : Hmc_tagged_cell.value) @ immutable ->
    (env : M.cells) @ immutable -> (rest : F.temporaries) @ immutable -> (entered : F.activation) @ immutable ->
    {u : unit | H.valid program globals limit {X.heap; state = Q.Running (a, frames)} abstract
      && I.lookup program.I.code a.F.pc === Some (I.Keep (G.Call next)) && a.F.temporaries === F.Value (closure, env, rest)
      && X.invoke program heap closure a.F.accumulator === Some entered} ->
    {u : unit | match G.lookup program.I.origin.C.blocks next with None -> false | Some block ->
      Codec.shape block.G.signature {a with F.pc = next; env; temporaries = rest}} @ ghost =
  fun program globals limit heap a frames abstract next closure env rest entered premise -> ghost_ (
    let configuration = {X.heap; state = Q.Running (a, frames)} in
    let saved = {a with F.pc = next; env; temporaries = rest} in
    let after = {X.heap; state = Q.Running (entered, Q.Frame (saved, frames))} in
    successor (Q.depth frames);
    H.step program globals limit (D.S (Q.depth frames)) configuration abstract ();
    X.step_def program globals limit (D.S (Q.depth frames)) configuration;
    H.valid_def program globals limit after (U.step program abstract);
    Q.decode_def heap after.X.state; Q.decode_frames_def heap (Q.Frame (saved, frames));
    Shape.state_def program.I.origin.C.origin.P.table program.I.origin.C.blocks (U.step program abstract);
    match F.decode heap saved, Q.decode_frames heap frames with
    | Some source_saved, Some source_frames ->
      Shape.frames_def program.I.origin.C.origin.P.table program.I.origin.C.blocks (S.Frame (source_saved, source_frames));
      Shape.at_label_def program.I.origin.C.origin.P.table program.I.origin.C.blocks source_saved;
      F.decode_def heap saved;
      (match G.lookup program.I.origin.C.blocks next with None -> () | Some block ->
        Hmc_frame_codec.shaped program.I.origin.C.origin.P.table block.G.signature source_saved ();
        Hmc_pointer_frame_shape.shape heap block.G.signature saved source_saved ())
    | _ -> ())
