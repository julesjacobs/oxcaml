module B = Wasm_u32
module W = Hmc_word64
module I = Wasm_instruction
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module T = Wasm_control
module L = Wasm_locals

type resource = Heap | Stack [@@inductive]
let[@def] (cursor_local @ total) (resource : resource @ immutable) : B.u32 =
  match resource with Heap -> 1 | Stack -> 4
let[@def] (limit_local @ total) (resource : resource @ immutable) : B.u32 =
  match resource with Heap -> 2 | Stack -> 5
let[@def] (status @ total) (resource : resource @ immutable) : B.u32 =
  match resource with Heap -> 2 | Stack -> 3

let[@def] (frame_local @ total) (unit : unit) : B.u32 = 0
let[@def] (status_local @ total) (unit : unit) : B.u32 = 11
let[@def] (failure_depth @ total) (unit : unit) : B.u32 = 2

let[@def] (failed @ total) (resource : resource @ immutable) (before : T.configuration @ immutable) = ghost_ (
  before.T.state.X.machine.E.stack === S.Empty
  && L.get before.T.state.X.machine.E.locals (status_local ()) === Some (S.I32 (status resource))
  && match before.T.code with
  | T.Instruction (I.Local_get limit,
      T.Instruction (I.Plain I.I64_extend_i32_u,
      T.Instruction (I.Local_get cursor,
      T.Instruction (I.Plain I.I64_extend_i32_u,
      T.Instruction (I.I64_const requested,
      T.Instruction (I.Plain I.I64_add,
      T.Instruction (I.Plain I.I64_lt_u,
      T.Instruction (I.Plain I.I32_eqz,
      T.If (_, T.Instruction (I.Br 2, T.Empty), _))))))))) ->
    cursor = cursor_local resource && limit = limit_local resource && requested.W.hi = 0
    && (match L.get before.T.state.X.machine.E.locals cursor, L.get before.T.state.X.machine.E.locals limit with
      | Some (S.I32 used), Some (S.I32 available) -> used + requested.W.lo > available
      | _ -> false)
  | _ -> false)
