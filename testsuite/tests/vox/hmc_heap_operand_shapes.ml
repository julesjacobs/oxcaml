module D = Hm_declarative
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module S = Hmc_cfg_semantics
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module V = Hmc_tagged_cell
module R = Hmc_closure_semantics
let rec (capture_size @ total) : (context : D.context) @ immutable -> (view : H.view) @ immutable ->
    (captures : H.cells) @ immutable -> (env : R.V.value) @ immutable ->
    {u : unit | H.decode_environment view captures === Some env && Hmc_frame_shape.environment context env} ->
    {u : unit | H.length captures === Hmc_frame_codec.locals_size context} @ ghost = fun context view captures env premise -> ghost_ (
  H.decode_environment_def view captures; Hmc_frame_shape.environment_def context env; H.length_def captures; Hmc_frame_codec.locals_size_def context;
  match context, captures with
  | D.Binding (_, rest), H.Cell (_, tail) ->
    (match H.decode_environment view tail with None -> () | Some remaining -> capture_size rest view tail remaining ())
  | _ -> ())
let[@def] (not_stuck @ total) (state : S.state @ immutable) =
  match state with S.Stuck -> false | _ -> true
let rec (stuck_advance @ total) : (program : I.program) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | U.advance program fuel S.Stuck === S.Stuck} @ ghost = fun program fuel -> ghost_ (
  U.advance_def program fuel S.Stuck; U.step_def program S.Stuck;
  match fuel with D.Z -> () | D.S rest -> stuck_advance program rest)
let (returning_step @ total) : (program : I.program) @ immutable -> (fuel : D.index) @ immutable ->
    (activation : S.activation) @ immutable -> (frames : S.frames) @ immutable -> (value : R.V.value) @ immutable ->
    {u : unit | U.advance program fuel (S.Running (activation, frames)) === S.Done value} ->
    {u : unit | not_stuck (U.step program (S.Running (activation, frames)))} @ ghost =
  fun program fuel activation frames value premise -> ghost_ (
    U.advance_def program fuel (S.Running (activation, frames));
    not_stuck_def (U.step program (S.Running (activation, frames)));
    match fuel with
    | D.Z -> ()
    | D.S rest -> stuck_advance program rest)
let (branch @ total) : (program : I.program) @ immutable -> (heap : H.heap) @ immutable ->
    (activation : F.activation) @ immutable -> (frames : Q.frames) @ immutable -> (abstract : S.state) @ immutable ->
    (yes : D.index) @ immutable -> (no : D.index) @ immutable ->
    {u : unit | Q.decode heap (Q.Running (activation, frames)) === Some abstract
      && I.lookup program.I.code activation.F.pc === Some (I.Keep (G.Branch (yes, no)))
      && not_stuck (U.step program abstract)} ->
    {condition : bool | activation.F.accumulator === V.Boolean condition} =
  fun program heap activation frames abstract yes no premise ->
    ghost_ (
      Q.decode_def heap (Q.Running (activation, frames)); F.decode_def heap activation;
      H.decode_def heap activation.F.accumulator; H.decode_value_def (H.view heap) activation.F.accumulator;
      U.step_def program abstract; not_stuck_def (U.step program abstract));
    match activation.F.accumulator with
    | V.Boolean condition -> condition
    | _ -> unreachable_ ()
type primitive = {left : Hmc_word64.t; right : Hmc_word64.t; env : H.cells; rest : F.temporaries}
let (primitive @ total) : (program : I.program) @ immutable -> (heap : H.heap) @ immutable ->
    (activation : F.activation) @ immutable -> (frames : Q.frames) @ immutable -> (abstract : S.state) @ immutable ->
    (operation : D.word_operation) @ immutable -> (next : D.index) @ immutable ->
    {u : unit | Q.decode heap (Q.Running (activation, frames)) === Some abstract
      && I.lookup program.I.code activation.F.pc === Some (I.Keep (G.Primitive (operation, next)))
      && not_stuck (U.step program abstract)} ->
    {out : primitive | activation.F.accumulator === V.Word out.right
      && activation.F.temporaries === F.Value (V.Word out.left, out.env, out.rest)} @ immutable =
  fun program heap activation frames abstract operation next premise ->
    ghost_ (
      Q.decode_def heap (Q.Running (activation, frames)); F.decode_def heap activation;
      F.decode_temporaries_def heap activation.F.temporaries;
      H.decode_def heap activation.F.accumulator; H.decode_value_def (H.view heap) activation.F.accumulator;
      U.step_def program abstract; not_stuck_def (U.step program abstract);
      match activation.F.temporaries with
      | F.Value (head, _, _) -> H.decode_def heap head; H.decode_value_def (H.view heap) head
      | _ -> ());
    match activation.F.temporaries, activation.F.accumulator with
    | F.Value (V.Word left, env, rest), V.Word right -> {left; right; env; rest}
    | _ -> unreachable_ ()
type list_fields = {address : Wasm_u32.u32; head : V.value; tail : V.value}
let (list @ total) : (program : I.program) @ immutable -> (heap : H.heap) @ immutable ->
    (activation : F.activation) @ immutable -> (frames : Q.frames) @ immutable -> (abstract : S.state) @ immutable ->
    (empty : D.index) @ immutable -> (full : D.index) @ immutable ->
    {u : unit | Q.decode heap (Q.Running (activation, frames)) === Some abstract
      && H.valid program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.table heap
      && I.lookup program.I.code activation.F.pc === Some (I.Keep (G.List_branch (empty, full)))
      && not_stuck (U.step program abstract)} ->
    {out : list_fields |
      (activation.F.accumulator === V.Nil || activation.F.accumulator === V.Cons_pointer out.address)
      && (activation.F.accumulator === V.Nil ||
        Hmc_heap_preservation.lookup_object heap out.address === Some (H.Cons (out.head, out.tail)))} @ immutable =
  fun program heap activation frames abstract empty full premise ->
    ghost_ (
      Q.decode_def heap (Q.Running (activation, frames)); F.decode_def heap activation;
      H.decode_def heap activation.F.accumulator; H.decode_value_def (H.view heap) activation.F.accumulator;
      U.step_def program abstract; not_stuck_def (U.step program abstract));
    match activation.F.accumulator with
    | V.Nil -> {address = 0; head = V.Nil; tail = V.Nil}
    | V.Cons_pointer address ->
      ghost_ (match H.lookup (H.view heap) address with
        | Some value ->
          let object_ = Hmc_heap_preservation.fetch program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.table heap address value () in
          H.decode_object_def (H.view heap) object_
        | None -> ());
      (match Hmc_heap_preservation.lookup_object heap address with
      | Some (H.Cons (head, tail)) -> {address; head; tail}
      | _ -> unreachable_ ())
    | _ -> unreachable_ ()
module K = Hmc_closure_ir
module C = Hmc_cfg_program
module P = Hmc_heap_preservation
let[@def] (call_instruction @ total) (instruction : I.instruction @ immutable) =
  match instruction with I.Tail_call | I.Keep (G.Call _) -> true | _ -> false
type call_fields = {address : Wasm_u32.u32; id : D.index; captured : H.cells;
  env : H.cells; rest : F.temporaries; entry : K.entry; code : C.function_entry}
let (call @ total) : (program : I.program) @ immutable -> (heap : H.heap) @ immutable ->
    (activation : F.activation) @ immutable -> (frames : Q.frames) @ immutable -> (abstract : S.state) @ immutable ->
    (instruction : I.instruction) @ immutable ->
    {u : unit | Q.decode heap (Q.Running (activation, frames)) === Some abstract
      && H.valid program.I.origin.C.origin.Hmc_closure_program.table heap
      && I.lookup program.I.code activation.F.pc === Some instruction
      && call_instruction instruction && not_stuck (U.step program abstract)} ->
    {out : call_fields |
      activation.F.temporaries === F.Value (V.Closure_pointer out.address, out.env, out.rest)
      && P.lookup_object heap out.address === Some (H.Closure (out.id, out.captured))
      && K.lookup program.I.origin.C.origin.Hmc_closure_program.table out.id === Some out.entry
      && C.lookup program.I.origin.C.functions out.id === Some out.code
      && out.address < H.used heap
      && Hmc_pointer_frame_codec.environment out.entry.K.captured out.captured
      && Hmc_pointer_frame_codec.locals_size out.entry.K.captured === H.length out.captured} @ immutable =
  fun program heap activation frames abstract instruction premise ->
    ghost_ (
      Q.decode_def heap (Q.Running (activation, frames)); F.decode_def heap activation;
      F.decode_temporaries_def heap activation.F.temporaries;
      call_instruction_def instruction; U.step_def program abstract; not_stuck_def (U.step program abstract);
      match activation.F.temporaries with
      | F.Value (head, _, _) -> H.decode_def heap head; H.decode_value_def (H.view heap) head
      | _ -> ());
    match activation.F.temporaries with
    | F.Value (V.Closure_pointer address, env, rest) ->
      ghost_ (match H.lookup (H.view heap) address with
        | Some value ->
          let table = program.I.origin.C.origin.Hmc_closure_program.table in
          let object_ = P.fetch table heap address value () in
          P.lookup_valid table heap address value ();
          Hmc_frame_shape.value_def table value; Hmc_frame_shape.valid_def table value;
          H.decode_object_def (H.view heap) object_
        | None -> ());
      (match P.lookup_object heap address with
      | Some (H.Closure (id, captured)) ->
        (match K.lookup program.I.origin.C.origin.Hmc_closure_program.table id, C.lookup program.I.origin.C.functions id with
        | Some entry, Some code ->
          ghost_ (match H.decode_environment (H.view heap) captured with
            | Some values ->
              Hmc_pointer_frame_shape.environment heap entry.K.captured captured values ();
              capture_size entry.K.captured (H.view heap) captured values ();
              Hmc_pointer_frame_shape.locals_size entry.K.captured
            | None -> ());
          {address; id; captured; env; rest; entry; code}
        | _ -> unreachable_ ())
      | _ -> unreachable_ ())
    | _ -> unreachable_ ()
