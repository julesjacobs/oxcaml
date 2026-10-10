module F = Hmc_heap_frame
module Q = Hmc_heap_state
module X = Hmc_heap_machine
module G = Hmc_cfg_ir
module C = Hmc_cfg_program
module P = Hmc_closure_program
module I = Hmc_tail_ir
module H = Hmc_heap_invariant
module Shape = Hmc_frame_shape
module Codec = Hmc_pointer_frame_codec
let (current @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable -> (limit : Hmc_word64.limb) ->
    (model : X.configuration) @ immutable -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    {u : unit | H.valid program globals limit model abstract} ->
    {u : unit | match model.X.state with Q.Running (a, _) ->
      (match G.lookup program.I.origin.C.blocks a.F.pc with None -> false | Some block -> Codec.shape block.G.signature a)
      | _ -> true} @ ghost = fun program globals limit model abstract premise -> ghost_ (
  H.valid_def program globals limit model abstract; Q.decode_def model.X.heap model.X.state;
  Shape.state_def program.I.origin.C.origin.P.table program.I.origin.C.blocks abstract;
  match model.X.state with
  | Q.Running (a, _) ->
    F.decode_def model.X.heap a;
    (match F.decode model.X.heap a with None -> () | Some source_a ->
      Shape.at_label_def program.I.origin.C.origin.P.table program.I.origin.C.blocks source_a;
      match G.lookup program.I.origin.C.blocks a.F.pc with None -> () | Some block ->
        Hmc_frame_codec.shaped program.I.origin.C.origin.P.table block.G.signature source_a ();
        Hmc_pointer_frame_shape.shape model.X.heap block.G.signature a source_a ())
  | _ -> ())
