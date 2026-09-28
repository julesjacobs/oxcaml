module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module C = Hmc_cfg_program
module K = Hmc_closure_program
module M = Hmc_monomorphic
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module V = Hmc_tagged_cell
module S = Hmc_cfg_semantics
module U = Hmc_tail_semantics
module Machine = Hmc_heap_machine
module Index = Hmc_u32_index
module Block = Hmc_wasm_program_block
module Branch = Hmc_wasm_program_source_branch
module Structured = Hmc_wasm_structured_block
module Straight = Hmc_wasm_block_lower
module Simple = Hmc_wasm_simple_lower
module Operands = Hmc_heap_operand_shapes
type result = {condition : bool; next : D.index; pc : B.u32; block : G.block}
let (prepare @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (heap : H.heap) @ immutable -> (activation : F.activation) @ immutable -> (frames : Q.frames) @ immutable ->
    (abstract : S.state) @ immutable -> (block : G.block) @ immutable ->
    (capacity : Hmc_wasm_relayout.count) -> (max_pc : B.u32) ->
    (yes : D.index) @ immutable -> (no : D.index) @ immutable -> (yes_pc : B.u32) -> (no_pc : B.u32) ->
    {u : unit | Q.decode heap (Q.Running (activation, frames)) === Some abstract
      && Operands.not_stuck (U.step program abstract)
      && G.lookup program.I.origin.C.blocks activation.F.pc === Some block
      && I.lookup program.I.code activation.F.pc === Some (I.Keep (G.Branch (yes, no)))
      && Block.corresponds globals block.G.signature (I.Keep (G.Branch (yes, no))) capacity max_pc (Branch.fragment yes_pc no_pc)} ->
    {out : result | activation.F.accumulator === V.Boolean out.condition
      && out.next === (if out.condition then yes else no) && out.pc = (if out.condition then yes_pc else no_pc)
      && Index.represents yes yes_pc && Index.represents no no_pc && Index.represents out.next out.pc
      && G.lookup program.I.origin.C.blocks out.next === Some out.block
      && out.block.G.signature.G.locals === block.G.signature.G.locals
      && out.block.G.signature.G.temporaries === block.G.signature.G.temporaries
      && out.block.G.signature.G.accumulator === None} @ immutable =
  fun program globals heap activation frames abstract block capacity max_pc yes no yes_pc no_pc premise ->
    ghost_ (
      I.valid_def program;
      I.lookup_related program.I.origin.C.blocks program.I.code program.I.sites activation.F.pc ();
      I.select_def program.I.sites activation.F.pc block.G.instruction;
      C.valid_def program.I.origin;
      Hmc_cfg_extension.lookup_valid (M.manifest program.I.origin.C.origin.K.origin.M.definitions)
        program.I.origin.C.origin.K.table program.I.origin.C.blocks activation.F.pc block ();
      G.block_valid_def (M.manifest program.I.origin.C.origin.K.origin.M.definitions)
        program.I.origin.C.origin.K.table program.I.origin.C.blocks block;
      Branch.fragment_def yes_pc no_pc;
      Block.corresponds_def globals block.G.signature (I.Keep (G.Branch (yes, no))) capacity max_pc (Branch.fragment yes_pc no_pc);
      Structured.corresponds_def globals block.G.signature (G.Branch (yes, no)) capacity max_pc
        (Structured.Straight (Straight.Simple (Simple.Branch (yes_pc, no_pc))));
      Straight.corresponds_def globals block.G.signature (G.Branch (yes, no)) capacity max_pc (Straight.Simple (Simple.Branch (yes_pc, no_pc)));
      Simple.corresponds_def (G.Branch (yes, no)) (Simple.Branch (yes_pc, no_pc)));
    let condition = Operands.branch program heap activation frames abstract yes no () in
    let next = if condition then yes else no in
    let pc = if condition then yes_pc else no_pc in
    ghost_ (G.accepts_def program.I.origin.C.blocks next block.G.signature.G.locals block.G.signature.G.temporaries None);
    match G.lookup program.I.origin.C.blocks next with
    | Some successor -> {condition; next; pc; block = successor}
    | None -> unreachable_ ()
