module D = Hm_declarative
module W = Hmc_word64
module G = Hmc_cfg_ir
module K = Hmc_closure_ir
module C = Hmc_cfg_program
module O = Hmc_closure_program
module I = Hmc_tail_ir
module E = Hmc_heap_extent
module Index = Hmc_u32_index
module Saved = Hmc_memory_saved_frame
module Stack = Hmc_memory_stack
module Capacity = Hmc_memory_stack_capacity
module Start = Hmc_memory_start

type failure = Code_capacity | Heap_region | Frame_width | Stack_region | Active_region [@@inductive]
type result = Layout of Start.layout | Rejected of failure [@@inductive]
let (make @ total) : (program : I.program) @ immutable -> (code_capacity : W.limb) ->
    (heap_base : W.limb) -> (heap_limit : W.limb) -> (stack_base : W.limb) -> (frames : D.index) @ immutable -> (memory_limit : W.limb) ->
    {out : result | match out with Rejected _ -> true | Layout layout -> Start.valid program layout
      && layout.Start.code_capacity = code_capacity && layout.Start.heap_base = heap_base && layout.Start.heap_limit = heap_limit
      && layout.Start.stack_base = stack_base && layout.Start.memory_limit = memory_limit
      && Capacity.region layout.Start.width frames stack_base layout.Start.stack_limit} @ immutable =
  fun program code_capacity heap_base heap_limit stack_base frames memory_limit ->
    if heap_base > heap_limit || heap_limit > stack_base || stack_base > memory_limit then Rejected Heap_region else
    match Index.encode code_capacity (K.size program.I.origin.C.origin.O.table), Index.encode code_capacity (G.size program.I.origin.C.blocks) with
    | None, _ | _, None -> Rejected Code_capacity
    | Some _, Some _ ->
      match E.reserve (Saved.slots program.I.origin.C.blocks) 0 memory_limit with
      | None -> Rejected Frame_width
      | Some width ->
        ghost_ (Saved.slots_def program.I.origin.C.blocks; E.ordered (Saved.slots program.I.origin.C.blocks) 0 width ());
        match Capacity.reserve width frames stack_base memory_limit () with
        | None -> Rejected Stack_region
        | Some stack_limit ->
          match E.reserve (Saved.slots program.I.origin.C.blocks) stack_limit memory_limit with
          | None -> Rejected Active_region
          | Some active_end ->
            let layout = {Start.code_capacity; width; heap_base; heap_limit; stack_base; stack_limit;
              active = stack_limit; active_end; memory_limit} in
            ghost_ (Stack.zero_def (); Capacity.ordered width frames stack_base stack_limit (); Start.valid_def program layout);
            Layout layout
