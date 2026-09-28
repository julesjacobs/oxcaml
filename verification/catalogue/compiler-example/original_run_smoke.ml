module B = Wasm_u32
module D = Hm_declarative
module W = Hmc_word64
module Init = Hmc_compilation_model
module Registers = Hmc_wasm_program_registers

let rec zeros n = if n = 0 then B.End else B.Byte (0, zeros (n - 1))
let rec index n = if n = 0 then D.Z else D.S (index (n - 1))
let word n = D.Word {W.lo = n; hi = 0}
let rec steps n acc = if n = 0 then acc else steps (n - 1) (Wasm_code.Succ acc)
let rec source_result fuel state =
  if fuel = 0 then failwith "source execution budget" else
  match state with
  | Hmc_source_semantics.Done (Hm_interpreter_typing.Word word) -> word
  | Hmc_source_semantics.Running _ -> source_result (fuel - 1) (Hmc_source_semantics.step state)
  | _ -> failwith "source did not return Word64"
let rec write channel = function B.End -> () | B.Byte (byte, rest) -> output_byte channel byte; write channel rest

(* Compiles the worked example once and runs the one module on each input:
   the host sets the exported global [payload] to the input and calls [run]. *)
let emit_with_stack : int -> D.term @ immutable -> (W.t * W.t) list -> unit = fun stack_capacity source inputs ->
  let memory = zeros 65536 in
  let layout = {Init.table_base = 0; frame_base = 2048; stack_base = 4096;
    heap_base = 32768; heap_limit = 60000; max_pc = 10000;
    stack_capacity = index stack_capacity; host_capacity = Wasm_code.Zero} in
  match Hmc_linear_bytes.drop memory layout.Init.heap_limit with
  | None -> failwith "source fixture memory bounds"
  | Some _ ->
    ghost_ (Hmc_linear_bounds.covers_def memory layout.Init.heap_limit; Init.valid_layout_def layout memory);
    match Hmc_compilation.compile source layout memory 1 () with
    | Hmc_compilation.Compiled artifact ->
      ghost_ (Hmc_compilation.static_validity artifact);
      let bytes = Hmc_compilation.bytes artifact in
      let output = open_out_bin (Sys.argv.(1) ^ "/program.wasm") in
      write output bytes; close_out output;
      List.iter (fun (wanted, input) ->
        let expected = source_result 100000 (Hmc_source_semantics.initial (D.Apply (source, D.Word input))) in
        if expected <> wanted then failwith "worked example source result";
        let after = match Wasm_binary_execution.run (steps 3000000 Wasm_code.Zero) bytes input
            (Wasm_code.Succ layout.Init.host_capacity) with
          | Wasm_binary_execution.Result (Wasm_calls.Finished after) -> after
          | _ -> failwith "public binary run" in
        let registers = match Registers.read after.Wasm_global_execution.globals with
          | Some registers -> registers | None -> failwith "public binary registers" in
        if registers.Registers.status <> 1 || registers.Registers.tag <> {W.lo = 1; hi = 0}
          || registers.Registers.payload <> expected then failwith "source/public binary result disagreement";
        let memory = after.Wasm_global_execution.execution.Wasm_memory_execution.memory in
        let output = open_out_bin (Printf.sprintf "%s/input-%d.memory.bin" Sys.argv.(1) input.W.lo) in
        write output memory; close_out output;
        Printf.printf "input %d: status %d, tag %d %d, result %d %d\n" input.W.lo registers.Registers.status
          registers.Registers.tag.W.lo registers.Registers.tag.W.hi
          registers.Registers.payload.W.lo registers.Registers.payload.W.hi) inputs
    | Hmc_compilation.Rejected _ -> failwith "public compilation rejected"

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
  emit_with_stack 16 source [({W.lo = 12; hi = 0}, {W.lo = 4; hi = 0}); ({W.lo = 0; hi = 0}, {W.lo = 8; hi = 0})]
