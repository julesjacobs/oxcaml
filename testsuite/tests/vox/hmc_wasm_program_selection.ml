module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module F = Wasm_functions
module Index = Hmc_u32_index
module Machine = Hmc_heap_machine
module Lower = Hmc_wasm_program_lower
module Table = Hmc_wasm_program_table
module Block = Hmc_wasm_program_block
module Assembly = Hmc_wasm_program_functions
module Runtime = Hmc_wasm_program_runtime
module Dispatch = Hmc_wasm_program_dispatch
let rec (bound @ total) : (blocks : G.table) @ immutable -> (id : D.index) @ immutable -> (block : G.block) @ immutable ->
    (pc : B.u32) -> (count : B.u32) ->
    {u : unit | G.lookup blocks id === Some block && Index.represents id pc && Index.represents (G.size blocks) count} ->
    {u : unit | pc < count} @ ghost = fun blocks id block pc count premise -> ghost_ (
    G.lookup_def blocks id; G.size_def blocks; Index.represents_def (G.size blocks) count;
    match blocks with
    | G.Empty -> ()
    | G.Add (_, rest) ->
      if Hm_elaboration_check.index_equal id (G.size rest) then Index.unique id pc (count - 1) ()
      else bound rest id block pc (count - 1) ())
type result = {instruction : I.instruction; fragment : Block.fragment; index : B.u32}
let (select @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (max_pc : B.u32) -> (count : B.u32) ->
    (id : D.index) @ immutable -> (pc : B.u32) -> (block : G.block) @ immutable -> (table_base : B.u32) -> (stack_base : B.u32) ->
    {u : unit | Lower.corresponds program globals max_pc lowered && Index.represents (G.size program.I.origin.Hmc_cfg_program.blocks) count
      && G.lookup program.I.origin.Hmc_cfg_program.blocks id === Some block && Index.represents id pc} ->
    {out : result | pc < count && out.index = Assembly.reverse_index count pc
      && I.lookup program.I.code id === Some out.instruction && Table.lookup lowered.Lower.blocks pc === Some out.fragment
      && Block.corresponds globals block.G.signature out.instruction lowered.Lower.capacity max_pc out.fragment
      && F.signature (Assembly.assemble lowered (G.size program.I.origin.Hmc_cfg_program.blocks) count (Runtime.config table_base stack_base) (Runtime.dispatcher ())).F.signatures
        (Dispatch.void_signature ()) === Some F.Void
      && F.element (Assembly.assemble lowered (G.size program.I.origin.Hmc_cfg_program.blocks) count (Runtime.config table_base stack_base) (Runtime.dispatcher ())).F.table pc === Some out.index
      && F.lookup (Assembly.assemble lowered (G.size program.I.origin.Hmc_cfg_program.blocks) count (Runtime.config table_base stack_base) (Runtime.dispatcher ())).F.functions out.index ===
        Some (Assembly.function_ lowered out.fragment (Runtime.config table_base stack_base))} @ immutable =
  fun program globals lowered max_pc count id pc block table_base stack_base premise ->
    ghost_ (I.valid_def program; Lower.corresponds_def program globals max_pc lowered;
      bound program.I.origin.Hmc_cfg_program.blocks id block pc count ();
      Table.lookup_correct globals program.I.origin.Hmc_cfg_program.blocks program.I.code program.I.sites lowered.Lower.blocks lowered.Lower.capacity max_pc id pc ();
      Assembly.source_order globals program.I.origin.Hmc_cfg_program.blocks program.I.code lowered.Lower.blocks lowered.Lower.capacity max_pc count ());
    match I.lookup program.I.code id, Table.lookup lowered.Lower.blocks pc with
    | Some instruction, Some fragment ->
      ghost_ (Assembly.dispatch_target lowered (G.size program.I.origin.Hmc_cfg_program.blocks) count (Runtime.config table_base stack_base) (Runtime.dispatcher ()) pc fragment ();
        Assembly.assemble_def lowered (G.size program.I.origin.Hmc_cfg_program.blocks) count (Runtime.config table_base stack_base) (Runtime.dispatcher ());
        Dispatch.void_signature_def (); F.signature_def (F.Signature (F.Void, F.Signature ((Runtime.dispatcher ()).F.result, F.No_signatures))) 0);
      {instruction; fragment; index = Assembly.reverse_index count pc}
    | _ -> unreachable_ ()

module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module State = Hmc_heap_state
module Shape = Hmc_frame_shape
module Semantics = Hmc_cfg_semantics
module Invariant = Hmc_heap_invariant
let (reachable_label @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (limit : B.u32) -> (configuration : Machine.configuration) @ immutable -> (abstract : Semantics.state) @ immutable ->
    (activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    {u : unit | Invariant.valid program globals limit configuration abstract
      && configuration.Machine.state === State.Running (activation, frames)} ->
    {u : unit | match G.lookup program.I.origin.Hmc_cfg_program.blocks activation.Frame.pc with None -> false | Some _ -> true} @ ghost =
  fun program globals limit configuration abstract activation frames premise -> ghost_ (
    Invariant.valid_def program globals limit configuration abstract;
    State.decode_def configuration.Machine.heap configuration.Machine.state;
    Frame.decode_def configuration.Machine.heap activation;
    match Frame.decode configuration.Machine.heap activation, State.decode_frames configuration.Machine.heap frames with
    | Some source, Some rest ->
      Shape.state_def program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.table program.I.origin.Hmc_cfg_program.blocks abstract;
      Shape.at_label_def program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.table program.I.origin.Hmc_cfg_program.blocks source
    | _ -> ())
let (frame_shape @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (limit : B.u32) -> (configuration : Machine.configuration) @ immutable -> (abstract : Semantics.state) @ immutable ->
    (activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable -> (block : G.block) @ immutable ->
    {u : unit | Invariant.valid program globals limit configuration abstract
      && configuration.Machine.state === State.Running (activation, frames)
      && G.lookup program.I.origin.Hmc_cfg_program.blocks activation.Frame.pc === Some block} ->
    {u : unit | Hmc_pointer_frame_codec.shape block.G.signature activation} @ ghost =
  fun program globals limit configuration abstract activation frames block premise -> ghost_ (
    Invariant.valid_def program globals limit configuration abstract;
    State.decode_def configuration.Machine.heap configuration.Machine.state;
    Frame.decode_def configuration.Machine.heap activation;
    match Frame.decode configuration.Machine.heap activation, State.decode_frames configuration.Machine.heap frames with
    | Some source, Some rest ->
      let table = program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.table in
      Shape.state_def table program.I.origin.Hmc_cfg_program.blocks abstract;
      Shape.at_label_def table program.I.origin.Hmc_cfg_program.blocks source;
      Hmc_frame_codec.shaped table block.G.signature source ();
      Hmc_pointer_frame_shape.shape configuration.Machine.heap block.G.signature activation source ()
    | _ -> ())
type reachable = {block : G.block; selected : result}
let (select_reachable @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (max_pc : B.u32) -> (count : B.u32) ->
    (limit : B.u32) -> (configuration : Machine.configuration) @ immutable -> (abstract : Semantics.state) @ immutable ->
    (activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (pc : B.u32) -> (table_base : B.u32) -> (stack_base : B.u32) ->
    {u : unit | Lower.corresponds program globals max_pc lowered
      && Index.represents (G.size program.I.origin.Hmc_cfg_program.blocks) count
      && Invariant.valid program globals limit configuration abstract
      && configuration.Machine.state === State.Running (activation, frames)
      && Index.represents activation.Frame.pc pc} ->
    {out : reachable | G.lookup program.I.origin.Hmc_cfg_program.blocks activation.Frame.pc === Some out.block
      && Hmc_pointer_frame_codec.shape out.block.G.signature activation
      && pc < count && out.selected.index = Assembly.reverse_index count pc
      && I.lookup program.I.code activation.Frame.pc === Some out.selected.instruction
      && Table.lookup lowered.Lower.blocks pc === Some out.selected.fragment
      && Block.corresponds globals out.block.G.signature out.selected.instruction lowered.Lower.capacity max_pc out.selected.fragment
      && F.signature (Assembly.assemble lowered (G.size program.I.origin.Hmc_cfg_program.blocks) count (Runtime.config table_base stack_base) (Runtime.dispatcher ())).F.signatures
        (Dispatch.void_signature ()) === Some F.Void
      && F.element (Assembly.assemble lowered (G.size program.I.origin.Hmc_cfg_program.blocks) count (Runtime.config table_base stack_base) (Runtime.dispatcher ())).F.table pc === Some out.selected.index
      && F.lookup (Assembly.assemble lowered (G.size program.I.origin.Hmc_cfg_program.blocks) count (Runtime.config table_base stack_base) (Runtime.dispatcher ())).F.functions out.selected.index ===
        Some (Assembly.function_ lowered out.selected.fragment (Runtime.config table_base stack_base))} @ immutable =
  fun program globals lowered max_pc count limit configuration abstract activation frames pc table_base stack_base premise ->
    ghost_ (reachable_label program globals limit configuration abstract activation frames ());
    match G.lookup program.I.origin.Hmc_cfg_program.blocks activation.Frame.pc with
    | Some block ->
      ghost_ (frame_shape program globals limit configuration abstract activation frames block ());
      let selected = select program globals lowered max_pc count activation.Frame.pc pc block table_base stack_base () in
      {block; selected}
    | None -> unreachable_ ()
