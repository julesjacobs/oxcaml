module A = Hmc_heap_initialize
module B = Hmc_heap_resource_execute
module R = Hmc_heap_execute
module E = Hmc_heap_extent

module D = Hm_declarative
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module T = Hmc_tail_stack
module Cap = Hmc_frame_capacity

let (execute @ total) : (program : I.program) @ immutable -> (base : Hmc_word64.limb) -> (limit : Hmc_word64.limb) ->
    (input : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | base <= limit && E.fits (A.cells program) base limit && Hmc_constant_resources.no_allocations program.I.code && T.no_calls program.I.code} ->
    {out : R.result | R.correct program base limit D.Z input fuel out && not (R.exhausted out)} @ immutable =
  fun program base limit input fuel premise ->
    ghost_ (Hmc_constant_resources.heap_plan program fuel (U.initial program input) ();
      T.initial program input; Hmc_constant_resources.stack_plan program fuel (U.initial program input) ();
      B.heap_cells_def program input fuel; B.stack_frames_def program input fuel;
      Hm_abstraction_proofs.add_zero (A.cells program); Cap.le_def D.Z D.Z);
    B.execute program base limit D.Z input fuel ()
