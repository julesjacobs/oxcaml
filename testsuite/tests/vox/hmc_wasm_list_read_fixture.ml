module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module State = Hmc_heap_state
module Machine = Hmc_heap_machine
module Program = Hmc_tail_ir
module Inv = Hmc_heap_invariant
module Abstract = Hmc_tail_semantics
module Header = Hmc_wasm_header_update
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module I = Wasm_instruction
module C = Wasm_code
module Capture = Hmc_wasm_list_capture
module Image = Hmc_heap_image
module L = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
type fixture = {memory : B.bytes; code : C.t; expected : W.t}
let[@def] (bound @ total) (u : unit) : B.u32 = 8192
let fixture : (table : Hmc_closure_ir.table) @ immutable -> (heap : Heap.heap) @ immutable -> (address : B.u32) ->
    (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    {u : unit | Heap.valid table heap && Heap.used heap <= bound ()
      && Hmc_heap_preservation.lookup_object heap address === Some (Heap.Cons (head, tail))} -> fixture list =
  fun table heap address head tail premise ->
    ghost_ (bound_def ());
    if not (Image.encodable 1000 heap) then failwith "list read heap encoding" else
    let initial = Hmc_wasm_reached_cons_fixture.filler 8192 in
    match L.drop initial 8192 with
    | None -> failwith "list read coverage"
    | Some _ ->
      ghost_ (Bounds.covers_def initial 8192);
      let memory = Image.materialize table 1000 initial heap 8192 () in
      let slots = {Capture.head_tag = 3; head_payload = 4; tail_tag = 5; tail_payload = 6} in
      let scratch = S.Push (S.I64 (Header.number 999), S.Push (S.I64 (Header.number 999), S.Push (S.I64 (Header.number 999), S.Push (S.I64 (Header.number 999), S.Empty)))) in
      let locals = S.Push (S.I32 address, S.Push (S.I32 7, S.Push (S.I32 (Heap.used heap), scratch))) in
      if not (Capture.distinct slots && Capture.separate slots 0 && Capture.separate slots 1 && Capture.separate slots 2 && Capture.writable slots locals) then failwith "list read scratch" else
      let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
      ghost_ (Wasm_locals.get_def locals 0);
      let captured = Capture.object_fields table heap address head tail state 0 1 2 slots () in
      let code = Capture.emit 0 8 slots 0 in
      ghost_ (Hmc_wasm_list_memory.zero_def (); Hmc_wasm_list_memory.eight_def ());
      (match X.run code state with
      | X.Done actual -> if actual <> {X.memory; machine = {E.locals = captured; stack = S.Empty}} then failwith "list read captured state"
      | _ -> failwith "list read execution");
      let prefix = C.Next (I.I32_const address, C.Next (I.Local_set 0,
        C.Next (I.I32_const 7, C.Next (I.Local_set 1,
        C.Next (I.I32_const (Heap.used heap), C.Next (I.Local_set 2, C.Empty)))))) in
      List.map (fun (local, expected) -> {memory; expected; code = E.append prefix (E.append code (C.Next (I.Local_get local, C.Empty)))})
        [3, V.tag head; 4, V.payload head; 5, V.tag tail; 6, V.payload tail]
let rec collect : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (configuration : Machine.configuration) @ immutable -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    int -> {u : unit | Inv.valid program globals (bound ()) configuration abstract} -> fixture list =
  fun program globals configuration abstract fuel premise ->
    ghost_ (bound_def (); Inv.valid_def program globals 8192 configuration abstract);
    match configuration.Machine.state with
    | State.Done _ -> []
    | State.Stuck -> failwith "list read source stuck"
    | State.Running (activation, _) ->
      if fuel = 0 then failwith "list read source fuel" else
      let cases = match Program.lookup program.Program.code activation.Frame.pc, activation.Frame.accumulator with
        | Some (Program.Keep (G.List_branch _)), V.Cons_pointer address ->
          (match Hmc_heap_preservation.lookup_object configuration.Machine.heap address with
          | Some (Heap.Cons (head, tail)) -> fixture program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.table configuration.Machine.heap address head tail ()
          | _ -> failwith "list read source object")
        | _ -> [] in
      ghost_ (Inv.step program globals 8192 (D.S (D.S D.Z)) configuration abstract ());
      match Machine.step program globals 8192 (D.S (D.S D.Z)) configuration with
      | Machine.Exhausted _ -> failwith "list read source exhausted"
      | Machine.Advanced next -> cases @ collect program globals next (Abstract.step program abstract) (fuel - 1) ()
let cases source (base : B.u32) =
  if base > 1031 then failwith "list read initial base" else
  let program = Hmc_wasm_global_fixture.build source in
  let input = Header.number 42 in
  match Hmc_heap_initialize.initialize program base 8192 input () with
  | Hmc_heap_initialize.Heap_exhausted _ -> failwith "list read initialization"
  | Hmc_heap_initialize.Initialized start ->
    ghost_ (bound_def (); Hmc_heap_initialize.correct_def program base 8192 input (Hmc_heap_initialize.Initialized start));
    collect program start.Hmc_heap_initialize.globals start.Hmc_heap_initialize.configuration (Abstract.initial program input) 200 ()
let fixtures () =
  let words = D.Lambda (D.CaseList (D.Cons (D.Bound D.Z, D.Cons (D.Word (Header.number 2), D.Nil)), D.Word (Header.number 0), D.Bound D.Z)) in
  let closures = D.Lambda (D.CaseList (D.Cons (D.Lambda (D.Bound (D.S D.Z)), D.Cons (D.Lambda (D.Bound D.Z), D.Nil)), D.Word (Header.number 0), D.Word (Header.number 1))) in
  let fixtures = cases words 1024 @ cases words 1031 @ cases closures 1024 @ cases closures 1031 in
  if List.length fixtures <> 16 then failwith "list read coverage";
  fixtures
