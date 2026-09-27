module B = Wasm_u32
module D = Hm_declarative
module W = Hmc_word64
module Init = Hmc_compilation_model
module Registers = Hmc_wasm_program_registers

let rec zeros n = if n = 0 then B.End else B.Byte (0, zeros (n - 1))
let rec index n = if n = 0 then D.Z else D.S (index (n - 1))
let word n = D.Word {W.lo = n; hi = 0}
let rec source_result fuel state =
  if fuel = 0 then failwith "source execution budget" else
  match state with
  | Hmc_source_semantics.Done (Hm_interpreter_typing.Word word) -> word
  | Hmc_source_semantics.Running _ -> source_result (fuel - 1) (Hmc_source_semantics.step state)
  | _ -> failwith "source did not return Word64"
let rec target_result fuel module_ configuration =
  if fuel = 0 then failwith "target execution budget" else
  match Wasm_calls.step module_ configuration with
  | Wasm_calls.Running next -> target_result (fuel - 1) module_ next
  | Wasm_calls.Finished after -> after
  | _ -> failwith "target execution failed"
let hex bytes =
  let buffer = Buffer.create 131072 in
  let rec add = function B.End -> () | B.Byte (byte, rest) ->
    Buffer.add_string buffer (Printf.sprintf "%02x" byte); add rest in
  add bytes; Buffer.contents buffer
let rec write channel = function B.End -> () | B.Byte (byte, rest) -> output_byte channel byte; write channel rest

let emit_with_stack : int -> int -> D.term @ immutable -> W.t @ immutable -> unit = fun stack_capacity case source input ->
  let memory = zeros 65536 in
  let layout = {Init.table_base = 0; frame_base = 2048; stack_base = 4096;
    heap_base = 32768; heap_limit = 60000; max_pc = 10000;
    stack_capacity = index stack_capacity; host_capacity = Wasm_code.Zero} in
  let expected = source_result 100000 (Hmc_source_semantics.initial (D.Apply (source, D.Word input))) in
  if expected <> {W.lo = (if case = 0 then 12 else 0); hi = 0} then
    failwith "worked example source result";
  match Hmc_linear_bytes.drop memory layout.Init.heap_limit with
  | None -> failwith "source fixture memory bounds"
  | Some _ ->
    ghost_ (Hmc_linear_bounds.covers_def memory layout.Init.heap_limit; Init.valid_layout_def layout memory);
    match Hmc_compilation.compile source layout input memory 1 () with
    | Hmc_compilation.Compiled artifact ->
      ghost_ (Hmc_compilation.static_validity artifact);
      let bytes = Hmc_compilation.bytes artifact in
      let image = match Wasm_binary_module.decode bytes with
        | Some (image, B.End) -> image | _ -> failwith "public binary decode" in
      let after = match Wasm_calls.start image.Wasm_binary_module.module_
          image.Wasm_binary_module.exports.Wasm_export_section.run image.Wasm_binary_module.data
          image.Wasm_binary_module.globals (Wasm_code.Succ layout.Init.host_capacity) with
        | Wasm_calls.Running configuration -> target_result 3000000 image.Wasm_binary_module.module_ configuration
        | _ -> failwith "public binary start" in
      let registers = match Registers.read after.Wasm_global_execution.globals with
        | Some registers -> registers | None -> failwith "public binary registers" in
      if registers.Registers.status <> 1 || registers.Registers.tag <> {W.lo = 1; hi = 0}
        || registers.Registers.payload <> expected then failwith "source/public binary result disagreement";
      let name = Printf.sprintf "program_%d" case in
      let output = open_out_bin (Sys.argv.(1) ^ "/" ^ name ^ ".wasm") in
      write output bytes; close_out output;
      Printf.printf "%s %d %d %d %d %d %s\n" name registers.Registers.status
        registers.Registers.tag.W.lo registers.Registers.tag.W.hi
        expected.W.lo expected.W.hi
        (hex after.Wasm_global_execution.execution.Wasm_memory_execution.memory)
    | Hmc_compilation.Rejected _ -> failwith "public compilation rejected"

let emit : int -> D.term @ immutable -> W.t @ immutable -> unit =
  fun case source input -> emit_with_stack 16 case source input

let () =
  let bound n = D.Bound (index n) in
  let id = D.Lambda (bound 0) in
  let map = D.Lambda (D.Recursive (D.CaseList (bound 0, D.Nil,
    D.Cons (D.Apply (bound 4, bound 0), D.Apply (bound 3, bound 1))))) in
  let add = D.Lambda (D.Lambda (D.Primitive (D.Add, bound 1, bound 0))) in
  let sum = D.Recursive (D.CaseList (bound 0, word 0,
    D.Primitive (D.Add, bound 0, D.Apply (bound 3, bound 1)))) in
  let first = D.Lambda (D.CaseList (bound 0, D.False, bound 0)) in
  let seed = D.Apply (bound 5, bound 0) in
  let ys = D.Apply (D.Apply (bound 5, D.Apply (bound 4, word 3)),
    D.Cons (bound 0, D.Cons (word 2, D.Nil))) in
  let predicate = D.Lambda (D.Primitive (D.Unsigned_less, bound 0, word 10)) in
  let bs = D.Apply (bound 7, D.Apply (D.Apply (bound 6, predicate), bound 0)) in
  let body = D.If (D.Apply (bound 4, bound 0), D.Apply (bound 5, bound 1), word 0) in
  let entry = D.Lambda (D.Let (seed, D.Let (ys, D.Let (bs, body)))) in
  let source = D.Let (id, D.Let (map, D.Let (add, D.Let (sum, D.Let (first, entry))))) in
  emit 0 source {W.lo = 4; hi = 0};
  emit 1 source {W.lo = 8; hi = 0}
