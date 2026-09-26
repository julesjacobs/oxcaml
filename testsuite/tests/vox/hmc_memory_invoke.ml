module B = Wasm_u32
module V = Hmc_tagged_cell
module M = Hmc_heap_objects
module K = Hmc_closure_ir
module P = Hmc_closure_program
module C = Hmc_cfg_program
module I = Hmc_tail_ir
module X = Hmc_heap_machine
module F = Hmc_heap_frame
module R = Hmc_closure_semantics
module Image = Hmc_heap_image
module Ops = Hmc_memory_operations

let[@def] (invoke @ total) (program : I.program @ immutable) (memory : B.bytes @ immutable)
    (reference : V.value @ immutable) (argument : V.value @ immutable) =
  match Ops.read_closure program.I.origin.C.origin.P.table memory reference with
  | None -> None
  | Some closure ->
    (match K.lookup program.I.origin.C.origin.P.table closure.Ops.code, C.lookup program.I.origin.C.functions closure.Ops.code with
    | Some entry, Some code ->
      let captures = if entry.K.recursive then M.Cell (reference, closure.Ops.captures) else closure.Ops.captures in
      Some {F.pc = code.C.start; env = M.Cell (argument, captures); accumulator = V.Nil; temporaries = F.Empty; current = reference}
    | _ -> None)
let (correct @ total) : (program : I.program) @ immutable -> (memory : B.bytes) @ immutable -> (heap : M.heap) @ immutable ->
    (reference : V.value) @ immutable -> (argument : V.value) @ immutable -> (value : R.V.value) @ immutable ->
    {u : unit | M.valid program.I.origin.C.origin.P.table heap && Image.related memory heap && M.decode heap reference === Some value} ->
    {u : unit | invoke program memory reference argument === X.invoke program heap reference argument} @ ghost =
  fun program memory heap reference argument value premise -> ghost_ (
    invoke_def program memory reference argument; X.invoke_def program heap reference argument;
    M.decode_def heap reference; M.decode_value_def (M.view heap) reference;
    match reference with
    | V.Closure_pointer _ -> (match value with
      | R.V.Closure (code, env) -> Ops.closure_correct program.I.origin.C.origin.P.table heap memory reference code env ()
      | _ -> ())
    | _ -> Ops.read_closure_def program.I.origin.C.origin.P.table memory reference)
