module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module Block = Hmc_wasm_program_block
module Structured = Hmc_wasm_structured_block
module Straight = Hmc_wasm_block_lower
module Simple = Hmc_wasm_simple_lower
let[@def] (matches @ total) (instruction : I.instruction @ immutable) (fragment : Block.fragment @ immutable) = ghost_ (
  match instruction, fragment with
  | I.Keep (G.Load (G.Local _, _, _, _)), Block.Structured (Structured.Straight (Straight.Simple (Simple.Local _)))
  | I.Keep (G.Load _), Block.Structured (Structured.Straight (Straight.Simple (Simple.Literal _)))
  | I.Keep (G.Jump _), Block.Structured (Structured.Straight (Straight.Simple (Simple.Jump _)))
  | I.Keep (G.Branch _), Block.Structured (Structured.Straight (Straight.Simple (Simple.Branch _)))
  | I.Keep (G.Load (G.Global _, _, _, _)), Block.Structured (Structured.Straight (Straight.Global _))
  | I.Keep (G.Load (G.Closure _, _, _, _)), Block.Structured (Structured.Closure _)
  | I.Keep (G.Save_environment _ | G.Save_value _ | G.Bind _ | G.Restore _), Block.Structured (Structured.Straight (Straight.Relayout _))
  | I.Keep (G.Primitive _), Block.Structured (Structured.Straight (Straight.Primitive _))
  | I.Keep (G.Cons _), Block.Structured (Structured.Cons _)
  | I.Keep (G.List_branch _), Block.Structured (Structured.List_branch _)
  | I.Keep (G.Call _), Block.Call _
  | I.Tail_call, Block.Tail_call _
  | I.Keep G.Return, Block.Return -> true
  | _ -> false)
let (correct @ total) : (globals : Hmc_heap_machine.globals) @ immutable -> (signature : G.signature) @ immutable ->
    (instruction : I.instruction) @ immutable -> (capacity : Hmc_wasm_relayout.count) -> (max_pc : Wasm_u32.u32) ->
    (fragment : Block.fragment) @ immutable ->
    {u : unit | Block.corresponds globals signature instruction capacity max_pc fragment} ->
    {u : unit | matches instruction fragment} @ ghost = fun globals signature instruction capacity max_pc fragment premise -> ghost_ (
  Block.corresponds_def globals signature instruction capacity max_pc fragment; matches_def instruction fragment;
  match instruction, fragment with
  | I.Keep op, Block.Structured body ->
    Structured.corresponds_def globals signature op capacity max_pc body;
    (match body with
    | Structured.Straight straight ->
      Straight.corresponds_def globals signature op capacity max_pc straight;
      (match straight with
      | Straight.Simple simple -> Simple.corresponds_def op simple
      | Straight.Global global -> Hmc_wasm_global_lower.corresponds_def globals op global
      | Straight.Relayout plan -> Hmc_wasm_relayout_geometry.matches_def signature op capacity max_pc
          plan.Hmc_wasm_relayout.copies plan.Hmc_wasm_relayout.pc plan.Hmc_wasm_relayout.required
      | Straight.Primitive plan -> Hmc_wasm_primitive_lower.matches_def signature op capacity max_pc plan)
    | _ -> ())
  | _ -> ())
