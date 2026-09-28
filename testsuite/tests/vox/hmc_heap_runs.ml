module D = Hm_declarative
module X = Hmc_heap_machine
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module S = Hmc_cfg_semantics
module H = Hmc_heap_invariant
module B = Hmc_heap_step

type result = Finished of X.configuration | Blocked of X.configuration * X.exhaustion * D.index [@@inductive]
let[@def] rec (run @ total) (program : I.program @ immutable) (globals : X.globals @ immutable)
    (heap_limit : Hmc_word64.limb) (stack_limit : D.index @ immutable) (fuel : D.index @ immutable)
    (configuration : X.configuration @ immutable) = match fuel with
  | D.Z -> Finished configuration
  | D.S rest -> (match X.step program globals heap_limit stack_limit configuration with
    | X.Exhausted reason -> Blocked (configuration, reason, D.Z)
    | X.Advanced next -> (match run program globals heap_limit stack_limit rest next with
      | Finished out -> Finished out | Blocked (out, reason, steps) -> Blocked (out, reason, D.S steps)))
let rec (correct @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable ->
    (heap_limit : Hmc_word64.limb) -> (stack_limit : D.index) @ immutable -> (fuel : D.index) @ immutable ->
    (configuration : X.configuration) @ immutable -> (abstract : S.state) @ immutable ->
    {u : unit | H.valid program globals heap_limit configuration abstract} ->
    {u : unit | match run program globals heap_limit stack_limit fuel configuration with
      | Finished out -> H.valid program globals heap_limit out (U.advance program fuel abstract)
      | Blocked (out, reason, steps) -> H.valid program globals heap_limit out (U.advance program steps abstract)
        && B.exhausted program heap_limit stack_limit out reason && D.present fuel steps} @ ghost =
  fun program globals heap_limit stack_limit fuel configuration abstract premise -> ghost_ (
    run_def program globals heap_limit stack_limit fuel configuration; U.advance_def program fuel abstract;
    match fuel with
    | D.Z -> ()
    | D.S rest ->
      H.step program globals heap_limit stack_limit configuration abstract ();
      match X.step program globals heap_limit stack_limit configuration with
      | X.Exhausted _ -> U.advance_def program D.Z abstract; D.present_def fuel D.Z
      | X.Advanced next ->
        correct program globals heap_limit stack_limit rest next (U.step program abstract) ();
        match run program globals heap_limit stack_limit rest next with
        | Finished _ -> ()
        | Blocked (_, _, steps) -> U.advance_def program (D.S steps) abstract; D.present_def fuel (D.S steps))
