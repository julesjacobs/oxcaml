module B = Wasm_u32
module I = Wasm_instruction
module T = Wasm_control
module F = Wasm_functions
module G = Wasm_globals
module S = Wasm_scalar
module Check = Wasm_static_control
let word = {Hmc_word64.lo = 0; hi = 0}
let globals = {G.values = S.Push (S.I64 word, S.Push (S.I64 word, S.Empty));
  permissions = G.Global (true, G.Global (false, G.Empty))}
let rec code = function [] -> T.Empty | op :: tail -> T.Instruction (op, code tail)
type case = {name : string; expected : bool; module_ : F.module_}
let case name expected result locals body =
  {name; expected; module_ = {F.functions = F.Function ({F.result; locals; code = body}, F.No_functions);
    signatures = F.Signature (F.Void, F.Signature (F.I32, F.Signature (F.I64, F.No_signatures)));
    table = F.No_elements}}
let cases () =
  let void name expected body = case name expected F.Void F.No_locals body in
  let ops name expected instructions = void name expected (code instructions) in
  [ops "empty" true [];
   ops "underflow" false [I.Plain I.Drop];
   ops "unreachable-pop" true [I.Plain I.Unreachable; I.Plain I.Drop];
   ops "unreachable-extra-value" false [I.Plain I.Unreachable; I.I32_const 0];
   ops "numeric-mismatch" false [I.I64_const word; I.Plain I.I32_eqz; I.Plain I.Drop];
   ops "mutable-global" true [I.I64_const word; I.Global_set 0];
   ops "immutable-global" false [I.I64_const word; I.Global_set 1];
   ops "dead-immutable-global" false [I.Plain I.Unreachable; I.Global_set 1];
   ops "dead-missing-local" false [I.Plain I.Unreachable; I.Local_get 0; I.Plain I.Drop];
   ops "dead-missing-function" false [I.Plain I.Unreachable; I.Call 1];
   ops "dead-missing-signature" false [I.Plain I.Unreachable; I.Call_indirect 3];
   ops "load" true [I.I32_const 0; I.I64_load (3, 0); I.Plain I.Drop];
   ops "store-order" false [I.I64_const word; I.I32_const 0; I.I64_store (3, 0)];
   ops "root-branch" true [I.Br 0];
   ops "missing-label" false [I.Br 1];
   void "block-extra-value" false (T.Block (code [I.I32_const 0], T.Empty));
   void "block-cannot-pop-prefix" false (T.Instruction (I.I32_const 0, T.Block (code [I.Plain I.Drop], code [I.Plain I.Drop])));
   void "block-preserves-prefix" true (T.Instruction (I.I32_const 0, T.Block (T.Empty, code [I.Plain I.Drop])));
   void "dead-block-is-checked" false (T.Instruction (I.Plain I.Unreachable, T.Block (code [I.Plain I.Drop], T.Empty)));
   void "untaken-branch-is-checked" false (T.Instruction (I.I32_const 1, T.If (T.Empty, code [I.Plain I.Drop], T.Empty)));
   void "loop-label" true (T.Loop (code [I.Br 0], T.Empty));
   case "return-value" true F.I32 F.No_locals (code [I.I32_const 0; I.Plain I.Return]);
   case "wrong-return-value" false F.I32 F.No_locals (code [I.I64_const word; I.Plain I.Return]);
   case "root-branch-value" true F.I32 F.No_locals (code [I.I32_const 0; I.Br 0]);
   case "conditional-root-branch" true F.I32 F.No_locals (code [I.I32_const 3; I.I32_const 1; I.Br_if 0]);
   case "conditional-branch-refines-unknown" false F.I32 F.No_locals
     (code [I.Plain I.Unreachable; I.Br_if 0; I.Plain I.I64_eqz]);
   case "local-tee" true F.I32 (F.Local32 F.No_locals) (code [I.I32_const 7; I.Local_tee 0]);
   case "local-wrong-type" false F.I32 (F.Local64 F.No_locals) (code [I.I32_const 7; I.Local_tee 0]);
   ops "unknown-select" true [I.Plain I.Unreachable; I.Plain I.Select; I.Plain I.Drop]]
let fixtures () = List.iter (fun case ->
  if Check.function_bodies case.module_ globals <> case.expected then failwith ("static typing: " ^ case.name)) (cases ())
module Binary = Wasm_binary_module
module Static = Wasm_static_module
type image_case = {image_name : string; image_expected : bool; image : Binary.image}
let image_of_module module_ =
  {Binary.module_; globals; table_limits = {Wasm_limits_section.minimum = 0; maximum = 0};
    memory_limits = {Wasm_limits_section.minimum = 0; maximum = 0};
    exports = {Wasm_export_section.run = 0; memory = 0; tag = 0; payload = 1}; data = B.End}
let image_cases () =
  let bodies = List.map (fun case -> {image_name = case.name; image_expected = case.expected;
    image = image_of_module case.module_}) (cases ()) in
  let base = image_of_module (case "base" true F.Void F.No_locals T.Empty).module_ in
  let altered name expected image = {image_name = name; image_expected = expected; image} in
  bodies @ [
    altered "missing-run-export" false {base with Binary.exports = {base.Binary.exports with Wasm_export_section.run = 1}};
    altered "missing-memory-export" false {base with Binary.exports = {base.Binary.exports with Wasm_export_section.memory = 1}};
    altered "missing-tag-export" false {base with Binary.exports = {base.Binary.exports with Wasm_export_section.tag = 2}};
    altered "missing-payload-export" false {base with Binary.exports = {base.Binary.exports with Wasm_export_section.payload = 2}};
    altered "missing-table-function" false {base with Binary.module_ = {base.Binary.module_ with F.table = F.Element (Some 1, F.No_elements)}};
    altered "table-segment-beyond-initial" true {base with Binary.module_ = {base.Binary.module_ with F.table = F.Element (Some 0, F.No_elements)}};
    altered "data-segment-beyond-initial" true {base with Binary.data = B.Byte (0, B.End)}]
let module_fixtures () =
  List.iter (fun case ->
    if Static.valid case.image <> case.image_expected then failwith ("module typing: " ^ case.image_name);
    match Binary.encode case.image B.End, Static.encode case.image with
    | Some raw, Static.Encoded checked ->
      if not case.image_expected || raw <> checked || not (Static.bytes_valid checked)
        then failwith ("certified module encoding: " ^ case.image_name)
    | Some _, Static.Invalid -> if case.image_expected then failwith ("valid module rejected: " ^ case.image_name)
    | _ -> failwith ("module fixture encoding: " ^ case.image_name)) (image_cases ());
  let base = image_of_module (case "base" true F.Void F.No_locals T.Empty).module_ in
  let bad_globals = {base with Binary.globals = {globals with G.permissions = G.Empty}} in
  let bad_limits = {base with Binary.memory_limits = {Wasm_limits_section.minimum = 2; maximum = 1}} in
  let bad_signature = {base with Binary.module_ = {base.Binary.module_ with F.signatures = F.No_signatures}} in
  List.iter (fun image -> if Static.valid image || Static.encode image <> Static.Invalid then failwith "malformed image accepted")
    [bad_globals; bad_limits; bad_signature]
let emitter_fixtures () =
  let module V = Wasm_static_types in
  let module Q = Wasm_static_sequence in
  let module Copy = Wasm_parallel_copy in
  let module Mixed = Wasm_mixed_write in
  let module Registers = Wasm_global_registers in
  let module R = Wasm_static_registers in
  let module Memory = Wasm_static_memory in
  let module_ = (case "emitters" true F.Void F.No_locals T.Empty).module_ in
  let context = {V.module_; globals; locals = F.Local32 (F.Local32 (F.Local64 F.No_locals)); result = F.Void} in
  let plan = Copy.Copy (0, 8, Copy.Copy (8, 0, Copy.End)) in
  let writes = Mixed.Write (0, Mixed.Constant word,
    Mixed.Write (8, Mixed.Word_local 2, Mixed.Write (16, Mixed.Pointer_local 1, Mixed.End))) in
  let registers = Registers.Binding (0, 2, Registers.End) in
  let readonly = Registers.Binding (1, 2, Registers.End) in
  if R.writable context readonly then failwith "readonly register transfer accepted";
  match V.local context.V.locals 0, V.local context.V.locals 1, V.local context.V.locals 2 with
  | Some V.I32, Some V.I32, Some V.I64 ->
    if not (Memory.mixed_sources context writes && R.bindings context registers && R.writable context registers)
      then failwith "emitter typing premises" else
    let run (state : V.state @ immutable) =
      let _ = ghost_ (
        Hmc_wasm_static_allocation.guard context 32 0 1 state ();
        Hmc_wasm_static_simple.local context 48 56 1 0 state ();
        Hmc_wasm_static_simple.branch context 1 2 0 state ();
        Hmc_wasm_static_block.payload context Hm_declarative.Add 56 0 state ();
        Hmc_wasm_static_block.payload context Hm_declarative.Subtract 56 0 state ();
        Hmc_wasm_static_block.payload context Hm_declarative.Equal_word 56 0 state ();
        Hmc_wasm_static_block.payload context Hm_declarative.Unsigned_less 56 0 state ();
        Wasm_static_copy.cross context plan 0 1 state ();
        Wasm_static_copy.parallel context plan 0 state ();
        Memory.ty_def Wasm_memory.W32; Memory.ty_def Wasm_memory.W64;
        Memory.read context Wasm_memory.W32 0 0 1 state ();
        Memory.read context Wasm_memory.W64 0 0 2 state ();
        Memory.write context Wasm_memory.W32 0 0 1 state ();
        Memory.write context Wasm_memory.W64 0 0 2 state ();
        Memory.mixed context writes 0 state ();
        R.load context registers state (); R.store context registers state ()) in
      if Q.check context (Hmc_wasm_allocation_guard.emit 32 0 1) state <> Some (V.push V.I32 state)
        then failwith "allocation guard stack types";
      let codes = [Hmc_wasm_local_load.emit 48 56 1 0;
        Hmc_wasm_branch.emit 1 2 0;
        Hmc_wasm_primitive_payload.emit Hm_declarative.Add 56 0;
        Hmc_wasm_primitive_payload.emit Hm_declarative.Subtract 56 0;
        Hmc_wasm_primitive_payload.emit Hm_declarative.Equal_word 56 0;
        Hmc_wasm_primitive_payload.emit Hm_declarative.Unsigned_less 56 0;
        Wasm_cross_copy.emit plan 0 1; Copy.emit plan 0;
        Wasm_memory_lowering.read_code Wasm_memory.W32 0 0 1;
        Wasm_memory_lowering.read_code Wasm_memory.W64 0 0 2;
        Wasm_memory_lowering.write_code Wasm_memory.W32 0 0 1;
        Wasm_memory_lowering.write_code Wasm_memory.W64 0 0 2;
        Mixed.emit writes 0; Registers.load_code registers; Registers.store_code registers] in
      List.iter (fun code -> if Q.check context code state <> Some state then failwith "emitter changed stack types") codes;
      let empty = V.initial () in
      let _ = ghost_ (Check.check_def context (Check.Label (Check.Root F.Void)) T.Empty empty;
        V.finish_def F.Void empty; V.consume_result_def F.Void empty; V.initial_def ();
        R.wrapper context (Check.Root F.Void) registers T.Empty registers T.Empty state empty ()) in
      if Check.check context (Check.Root F.Void) (Wasm_register_block.emit registers T.Empty registers T.Empty) state <> Some state
        then failwith "register wrapper changed stack types" in
    run (V.initial ());
    run {V.stack = V.Push (V.I64, V.Push (V.I32, V.Empty)); unreachable = false};
    run {V.stack = V.Push (V.Unknown, V.Empty); unreachable = true}
  | _ -> failwith "emitter local types"
let object_emitter_fixtures () =
  let module V = Wasm_static_types in
  let module Q = Wasm_static_sequence in
  let module Structured = Hmc_wasm_structured_block in
  let module Capture = Hmc_wasm_cons_capture in
  let module Copy = Wasm_parallel_copy in
  let module Pop = Hmc_wasm_value_pop in
  let module Closure = Hmc_wasm_closure_lower in
  let module Write = Hmc_wasm_closure_write in
  let module Cons = Hmc_wasm_static_cons in
  let module Objects = Hmc_wasm_static_objects in
  let module_ = (case "object-emitters" true F.Void F.No_locals T.Empty).module_ in
  let locals = F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 (F.Local64 F.No_locals)))))) in
  let context = {V.module_; globals; locals; result = F.Void} in
  let slots = {Capture.head_tag = 3; head_payload = 4; tail_tag = 5; tail_payload = 6} in
  let runtime = {Structured.frame = 0; heap = 1; limit = 2; object_ = 0; scratch = slots} in
  let copies = Copy.Copy (48, 16, Copy.Copy (56, 24, Copy.End)) in
  let pop = {Pop.head_tag = 48; head_payload = 56; copies; pc = 1; required = 4} in
  let closure = {Closure.object_ = {Write.copies; code = 1; bytes = 32}; pc = 2} in
  let branch = {Hmc_wasm_list_lower.empty_pc = 3; full = {Hmc_wasm_relayout.copies; pc = 4; required = 4}} in
  let labels = Check.Root F.Void in
  match V.local locals 0, V.local locals 1, V.local locals 2,
    V.local locals 3, V.local locals 4, V.local locals 5, V.local locals 6 with
  | Some V.I32, Some V.I32, Some V.I32, Some V.I64, Some V.I64, Some V.I64, Some V.I64 ->
    let run (state : V.state @ immutable) =
      let _ = ghost_ (
        Hmc_wasm_static_return.retreat context 32 0 state ();
        Hmc_wasm_static_return.frame context copies 32 0 1 state ();
        Hmc_wasm_static_return.root_reads context 0 3 4 state ();
        Cons.slots_typed_def context slots;
        Check.label_def (Check.Label labels) 0;
        Objects.success context closure.Closure.object_ closure.Closure.pc 0 1 state ();
        Cons.success context pop slots 0 1 state ();
        Objects.closure context labels closure runtime 0 state ();
        Cons.cons context labels pop runtime 0 state ();
        Hmc_wasm_static_structured.locals_typed_def context runtime;
        Hmc_wasm_static_structured.block context labels (Structured.List_branch branch) runtime 0 state ()) in
      if Check.check context labels (Structured.emit (Structured.List_branch branch) runtime 0) state <> Some state
        then failwith "list branch typing";
      List.iter (fun code -> if Q.check context code state <> Some state then failwith "return sequence typing")
        [Hmc_wasm_return_frame.emit copies 32 0 1;
         Wasm_frame_snapshot.emit (Hmc_wasm_root_return.reads 3 4) 0];
      if Q.check context (Hmc_wasm_closure_success.emit closure.Closure.object_ closure.Closure.pc 0 1) state <> Some state
        then failwith "closure success typing";
      if Q.check context (Hmc_wasm_cons_success.emit pop 0 1 3 4 5 6) state <> Some state
        then failwith "cons success typing";
      List.iter (fun fragment ->
        if Check.check context labels (Structured.emit fragment runtime 0) state <> Some state then failwith "object block typing";
        if Check.check context labels (Structured.emit fragment runtime 2) state <> None then failwith "invalid exhaustion label accepted")
        [Structured.Closure closure; Structured.Cons pop] in
    run (V.initial ());
    run {V.stack = V.Push (V.I64, V.Push (V.I32, V.Empty)); unreachable = false};
    run {V.stack = V.Push (V.Unknown, V.Empty); unreachable = true};
    let wrong = {context with V.locals = F.Local32 (F.Local32 (F.Local32 (F.Local32 (F.Local64 (F.Local64 (F.Local64 F.No_locals))))))} in
    if Check.check wrong labels (Structured.emit (Structured.Cons pop) runtime 0) (V.initial ()) <> None
      then failwith "invalid cons scratch type accepted";
    if Check.check wrong labels (Structured.emit (Structured.List_branch branch) runtime 0) (V.initial ()) <> None
      then failwith "invalid list scratch type accepted";
    let wrong_object = {runtime with Structured.object_ = 3} in
    if Check.check context labels (Structured.emit (Structured.List_branch branch) wrong_object 0) (V.initial ()) <> None
      then failwith "invalid list object type accepted"
  | _ -> failwith "object emitter local types"
let call_emitter_fixtures () =
  let module V = Wasm_static_types in
  let module Runtime = Hmc_wasm_program_runtime in
  let module Assembly = Hmc_wasm_program_functions in
  let module Emit = Hmc_wasm_program_emit in
  let module Proof = Hmc_wasm_static_emit in
  let module Block = Hmc_wasm_program_block in
  let module Lower = Hmc_wasm_program_lower in
  let module Plans = Hmc_wasm_call_plan_table in
  let module Captures = Hmc_wasm_call_captures in
  let module Copy = Wasm_parallel_copy in
  let module Registers = Hmc_wasm_program_registers in
  let module_ = (case "call-emitters" true F.Void F.No_locals T.Empty).module_ in
  let registers = {Registers.frame = 0; heap = 256; heap_limit = 1024; top = 128;
    stack_limit = 256; status = 0; tag = word; payload = word} in
  let config = Runtime.config 0 128 in
  let context = {V.module_; globals = Registers.globals registers; locals = Runtime.local_types (); result = F.Void} in
  let copies = Copy.Copy (32, 64, Copy.Copy (40, 72, Copy.End)) in
  let plans = Plans.Add (1, {Captures.copies; recursive = false},
    Plans.Add (0, {Captures.copies; recursive = true}, Plans.Empty)) in
  let call = Block.Call {Block.save = {Hmc_wasm_call_save.copies; pc = 1};
    padding = Wasm_mixed_write.End; padding_length = Hm_declarative.Z; saved = 0; environment = 0} in
  let run (plans : Plans.table @ immutable) (state : V.state @ immutable) =
    let program = {Lower.blocks = Hmc_wasm_program_table.Empty; calls = plans; restore = copies; capacity = 4; width = 80} in
    let _ = ghost_ (Hmc_wasm_static_config.config module_ registers 0 128) in
    let check (fragment : Block.fragment @ immutable) =
      let _ = ghost_ (Proof.padding_typed_def context fragment) in
      if not (match fragment with Block.Call call -> Wasm_static_memory.mixed_sources context call.Block.padding | _ -> true)
        then failwith "call padding premise" else
      let _ = ghost_ (Proof.emit context (Check.Root F.Void) program fragment config.Assembly.locals 0 128 state ()) in
      if Check.check context (Check.Root F.Void) (Emit.emit program fragment config.Assembly.locals 0 128) state <> Some state
        then failwith "call emitter typing" in
    check call; check (Block.Tail_call 0); check Block.Return in
  let check_plans (plans : Plans.table @ immutable) =
    run plans (V.initial ());
    run plans {V.stack = V.Push (V.I64, V.Push (V.I32, V.Empty)); unreachable = false};
    run plans {V.stack = V.Push (V.Unknown, V.Empty); unreachable = true} in
  check_plans Plans.Empty; check_plans plans;
  let program = {Lower.blocks = Hmc_wasm_program_table.Empty; calls = plans; restore = copies; capacity = 4; width = 80} in
  let wrong = {config.Assembly.locals with Emit.code = 14} in
  if Check.check context (Check.Root F.Void) (Emit.emit program (Block.Tail_call 0) wrong 0 128) (V.initial ()) <> None
    then failwith "invalid call code type accepted";
  let wrong = {config.Assembly.locals with Emit.stack_limit = 14} in
  if Check.check context (Check.Root F.Void) (Emit.emit program call wrong 0 128) (V.initial ()) <> None
    then failwith "invalid call stack limit type accepted"
