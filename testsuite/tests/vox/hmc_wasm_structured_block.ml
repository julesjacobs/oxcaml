module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module Machine = Hmc_heap_machine
module Straight = Hmc_wasm_block_lower
module Pop = Hmc_wasm_value_pop
module Relayout = Hmc_wasm_relayout
module Capture = Hmc_wasm_cons_capture
module Success = Hmc_wasm_cons_success
module Exit = Hmc_wasm_allocation_exit
module T = Wasm_control
module Lift = Wasm_control_lift
module List = Hmc_wasm_list_lower
module List_capture = Hmc_wasm_list_capture
module List_branch = Hmc_wasm_list_conditional
module Closure = Hmc_wasm_closure_lower
module Closure_success = Hmc_wasm_closure_success
type fragment = Straight of Straight.fragment | Cons of Pop.fragment | List_branch of List.fragment | Closure of Closure.fragment [@@inductive]
type locals = {frame : B.u32; heap : B.u32; limit : B.u32; object_ : B.u32; scratch : Capture.slots}
let[@def] (corresponds @ total) (globals : Machine.globals @ immutable) (signature : G.signature @ immutable)
    (instruction : G.instruction @ immutable) (capacity : Relayout.count) (max_pc : W.limb) (fragment : fragment @ immutable) = ghost_ (
  match fragment with
  | Straight code -> Straight.corresponds globals signature instruction capacity max_pc code
  | Cons pop -> (match instruction, signature.G.accumulator, signature.G.temporaries with
    | G.Cons next, Some (D.List_type element), G.Value (_, head, _) -> element === head && Pop.matches signature next capacity max_pc pop
    | _ -> false)
  | Closure closure -> (match instruction with
    | G.Load (G.Closure id, _, _, next) -> Closure.matches signature id next capacity max_pc closure
    | _ -> false)
  | List_branch list -> (match instruction with
    | G.List_branch (empty, next) -> List.matches signature empty next capacity max_pc list
    | _ -> false))
let[@def] (encodable @ total) (globals : Machine.globals @ immutable) (signature : G.signature @ immutable) (instruction : G.instruction @ immutable)
    (capacity : Relayout.count) (max_pc : W.limb) = ghost_ (
  match instruction with
  | G.Cons next -> (match signature.G.accumulator, signature.G.temporaries with
    | Some (D.List_type element), G.Value (_, head, _) -> element === head && Pop.encodable signature next capacity max_pc
    | _ -> false)
  | G.Load (G.Closure id, _, _, next) -> Closure.encodable signature id next capacity max_pc
  | G.List_branch (empty, next) -> List.encodable signature empty next capacity max_pc
  | _ -> Straight.encodable globals signature instruction capacity max_pc)
let (lower @ total) : (globals : Machine.globals) @ immutable -> (signature : G.signature) @ immutable ->
    (instruction : G.instruction) @ immutable -> (capacity : Relayout.count) -> (max_pc : W.limb) ->
    {out : fragment option | match out with None -> not (encodable globals signature instruction capacity max_pc) | Some fragment -> encodable globals signature instruction capacity max_pc && corresponds globals signature instruction capacity max_pc fragment} @ immutable =
  fun globals signature instruction capacity max_pc ->
    ghost_ (encodable_def globals signature instruction capacity max_pc);
    match instruction with
    | G.Cons next -> (match signature.G.accumulator, signature.G.temporaries with
      | Some (D.List_type element), G.Value (_, head, _) ->
        if not (Hm_elaboration_check.mono_equal element head) then None else
        (match Pop.build signature next capacity max_pc with
        | None -> None
        | Some pop -> let fragment = Cons pop in
          ghost_ (corresponds_def globals signature instruction capacity max_pc fragment); Some fragment)
      | _ -> None)
    | G.Load (G.Closure id, _, _, next) -> (match Closure.build signature id next capacity max_pc with
      | None -> None
      | Some closure -> let fragment = Closure closure in
        ghost_ (corresponds_def globals signature instruction capacity max_pc fragment); Some fragment)
    | G.List_branch (empty, next) -> (match List.build signature empty next capacity max_pc with
      | None -> None
      | Some list -> let fragment = List_branch list in
        ghost_ (corresponds_def globals signature instruction capacity max_pc fragment); Some fragment)
    | _ -> (match Straight.lower globals signature instruction capacity max_pc with
      | None -> None
      | Some code -> let fragment = Straight code in
        ghost_ (corresponds_def globals signature instruction capacity max_pc fragment); Some fragment)
let[@def] (emit @ total) (fragment : fragment @ immutable) (locals : locals @ immutable) (exhaustion_depth : B.u32) =
  match fragment with
  | Straight code -> Lift.embed (Straight.emit code locals.frame) T.Empty
  | Cons pop ->
    let slots = locals.scratch in
    let capture = Capture.emit pop.Pop.head_tag pop.Pop.head_payload slots locals.frame in
    let success = Success.emit pop locals.frame locals.heap slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload in
    Lift.embed capture (Exit.emit (Wasm_four_words.width ()) locals.heap locals.limit success exhaustion_depth T.Empty)
  | Closure closure ->
    Exit.emit closure.Closure.object_.Hmc_wasm_closure_write.bytes locals.heap locals.limit
      (Closure_success.emit closure.Closure.object_ closure.Closure.pc locals.frame locals.heap) exhaustion_depth T.Empty
  | List_branch list ->
    let slots = locals.scratch in
    let scratch = {List_capture.head_tag = slots.Capture.head_tag; head_payload = slots.Capture.head_payload;
      tail_tag = slots.Capture.tail_tag; tail_payload = slots.Capture.tail_payload} in
    List_branch.emit list.List.full list.List.empty_pc locals.frame locals.object_ scratch T.Empty
