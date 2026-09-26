module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module State = Hmc_heap_state
module G = Hmc_cfg_ir
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module R = Hmc_wasm_moves_fixture
module F = Hmc_wasm_frame_fixture
module Step = Hmc_wasm_control_step
let word (lo : W.limb) : W.t = {W.lo; hi = 0}
let rec index : (number : W.limb) -> {out : D.index | Hmc_u32_index.represents out number} @ immutable = fun number ->
  if number = 0 then (ghost_ (Hmc_u32_index.represents_def D.Z number); D.Z)
  else let out = D.S (index (number - 1)) in ghost_ (Hmc_u32_index.represents_def out number); out
let fixture (base : B.u32) branch condition (yes_pc : W.limb) (no_pc : W.limb) =
  if base > 4294967247 then failwith "fixture address" else
  let yes = index yes_pc in let no = index no_pc in
  let next = if branch && not condition then no else yes in
  let pc = if branch && not condition then no_pc else yes_pc in
  let current = V.Closure_pointer 64 in let accumulator = V.Boolean condition in
  let env_tail = Heap.Cell (V.Word (word 20), Heap.Empty) in
  let env = Heap.Cell (V.Word (word 10), env_tail) in
  let ty = D.Forall (D.Z, D.Word64) in
  let context_tail = D.Binding (ty, D.Empty_context) in
  let context = D.Binding (ty, context_tail) in
  let signature = {G.locals = context; temporaries = G.Empty_temporaries; accumulator = Some D.Boolean} in
  let activation = {Frame.pc = index 7; current; accumulator; env; temporaries = Frame.Empty} in
  ghost_ (Codec.shape_def signature activation; Codec.environment_def context env;
    Codec.environment_def context_tail env_tail; Codec.environment_def D.Empty_context Heap.Empty;
    Codec.temporaries_shape_def G.Empty_temporaries Frame.Empty);
  let frame_cells = Codec.encode signature activation Heap.Empty () in
  ghost_ (Codec.decode_def signature activation.Frame.pc frame_cells);
  match frame_cells with
  | Heap.Cell (_, Heap.Cell (_, cells)) ->
    let before_object = Wire.Closure (7, frame_cells) in
    let after_object = Wire.Closure (pc, frame_cells) in
    let tail = B.Byte (42, B.End) in
    let before_frame = Wire.encode before_object tail in
    let after_frame = Wire.encode after_object tail in
    let memory = F.prefix base before_frame in
    let expected = Wasm_memory_splice.replace memory base before_frame after_frame () in
    let locals = S.Push (S.I32 base, S.Push (S.I64 (word 37), S.Push (S.I64 (word 99), S.Empty))) in
    let state = {X.memory; machine = {E.locals; stack = S.Push (S.I32 91, S.Empty)}} in
    ghost_ (Wire.schema_def before_object; Wire.schema_def after_object;
      Heap.length_def frame_cells; Heap.length_def (Heap.Cell (accumulator, cells));
      Wasm_locals.get_def locals 0);
    let direct = if branch then (
      ghost_ (Step.branch signature signature activation State.Halt Heap.Empty cells (Heap.length cells)
        yes no yes_pc no_pc condition next 7 pc state expected tail 0 base before_frame after_frame ());
      Hmc_wasm_branch.emit yes_pc no_pc 0)
    else (
      ghost_ (Step.jump signature signature activation State.Halt Heap.Empty cells (Heap.length cells)
        next 7 pc state expected tail 0 base before_frame after_frame ());
      Hmc_wasm_pc_update.emit pc 0) in
    let instruction = if branch then G.Branch (yes, no) else G.Jump next in
    let capacity = if yes_pc > no_pc then yes_pc else no_pc in
    let code = match Hmc_wasm_simple_lower.lower instruction capacity with
      | None -> failwith "supported control instruction rejected"
      | Some fragment ->
        ghost_ (if branch then Hmc_wasm_fragment_code.branch yes no yes_pc no_pc fragment 0 ()
          else Hmc_wasm_fragment_code.jump next pc fragment 0 ());
        let code : {code : Wasm_code.t | X.run code state === X.Done {X.memory = expected; machine = state.X.machine}} @ immutable
          = Hmc_wasm_simple_lower.emit fragment 0 in
        code in
    if code <> direct then failwith "CFG successor lowering";
    (match X.run code state with
    | X.Done after -> if after.X.memory <> expected || after.X.machine <> state.X.machine then failwith "control frame encoding"
    | _ -> failwith "control execution");
    {R.memory; base; first = word 37; second = word 99; code;
      expected = [8, word pc; 24, word 64; 32, V.tag accumulator; 40, V.payload accumulator; 56, word 10; 72, word 20]}
  | _ -> failwith "compiler frame encoding"
let fixtures () =
  let zero = index 0 in let high = index 13 in
  if Hmc_wasm_simple_lower.lower (G.Jump high) 12 <> None
    || Hmc_wasm_simple_lower.lower (G.Branch (zero, high)) 12 <> None
    || Hmc_wasm_simple_lower.lower (G.Branch (high, zero)) 12 <> None
    || Hmc_wasm_simple_lower.lower (G.Call zero) 13 <> None
    || Hmc_wasm_simple_lower.lower (G.Load (G.Global zero, D.Word64, D.Constant, zero)) 13 <> None
    then failwith "unsupported or out-of-range CFG instruction accepted";
  (match Hmc_wasm_simple_lower.lower (G.Jump zero) 0 with
    | Some (Hmc_wasm_simple_lower.Jump 0) -> () | _ -> failwith "zero PC rejected");
  [fixture 0 false false 0 13; fixture 7 false true 0 13;
   fixture 0 true true 11 13; fixture 7 true true 11 13;
   fixture 0 true false 11 13; fixture 7 true false 11 13;
   fixture 0 true false 11 11; fixture 7 true true 11 11]
