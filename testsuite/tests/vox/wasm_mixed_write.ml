module B = Wasm_u32
module W = Hmc_word64
module S = Wasm_scalar
module L = Wasm_locals
module M = Wasm_memory
module E = Wasm_execution
module X = Wasm_memory_execution
module C = Wasm_code
module Frame = Wasm_frame_write
module Immediate = Wasm_immediate_write
module Pointer = Wasm_pointer_store
module Header = Hmc_wasm_header_update
type value = Constant of W.t | Word_local of B.u32 | Pointer_local of B.u32 [@@inductive]
type writes = End | Write of B.u32 * value * writes [@@inductive]
let[@def] (read @ total) (value : value @ immutable) (locals : S.stack @ immutable) = match value with
  | Constant word -> Some word
  | Word_local local -> (match L.get locals local with Some (S.I64 word) -> Some word | _ -> None)
  | Pointer_local local -> (match L.get locals local with Some (S.I32 pointer) -> Some (Header.number pointer) | _ -> None)
let[@def] (one @ total) (offset : B.u32) (value : value @ immutable) (base_local : B.u32) = match value with
  | Constant word -> Immediate.emit offset base_local word
  | Word_local local -> Frame.emit (Frame.Write (offset, local, Frame.End)) base_local
  | Pointer_local local -> Pointer.emit offset base_local local
let[@def] rec (emit @ total) (writes : writes @ immutable) (base_local : B.u32) = match writes with
  | End -> C.Empty | Write (offset, value, rest) -> E.append (one offset value base_local) (emit rest base_local)
let[@def] rec (apply @ total) (writes : writes @ immutable) (memory : B.bytes @ immutable) (base : B.u32) (locals : S.stack @ immutable) = match writes with
  | End -> Some memory
  | Write (offset, value, rest) -> (match read value locals with
    | None -> None | Some word -> (match M.store memory base offset (S.I64 word) with None -> None | Some next -> apply rest next base locals))
let (single @ total) : (offset : B.u32) -> (value : value) @ immutable -> (base_local : B.u32) ->
    (state : X.state) @ immutable -> (base : B.u32) -> (word : W.t) @ immutable -> (after : B.bytes) @ immutable ->
    {u : unit | read value state.X.machine.E.locals === Some word && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && M.store state.X.memory base offset (S.I64 word) === Some after} ->
    {u : unit | X.run (one offset value base_local) state === X.Done {X.memory = after; machine = state.X.machine}
      && Hmc_tagged_cell.length after === Hmc_tagged_cell.length state.X.memory} @ ghost =
  fun offset value base_local state base word after premise -> ghost_ (
    read_def value state.X.machine.E.locals; one_def offset value base_local;
    match value with
    | Constant word -> Immediate.correct offset base_local word state base after ()
    | Word_local local ->
      Frame.apply_def (Frame.Write (offset, local, Frame.End)) state.X.memory base state.X.machine.E.locals;
      Frame.apply_def Frame.End after base state.X.machine.E.locals;
      Frame.correct (Frame.Write (offset, local, Frame.End)) base_local state base after ()
    | Pointer_local local -> (match L.get state.X.machine.E.locals local with
      | Some (S.I32 pointer) -> Pointer.correct offset base_local local state base pointer after ()
      | _ -> unreachable_ ()))
let rec (correct @ total) : (writes : writes) @ immutable -> (base_local : B.u32) -> (state : X.state) @ immutable -> (base : B.u32) -> (after : B.bytes) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base) && apply writes state.X.memory base state.X.machine.E.locals === Some after} ->
    {u : unit | X.run (emit writes base_local) state === X.Done {X.memory = after; machine = state.X.machine}
      && Hmc_tagged_cell.length after === Hmc_tagged_cell.length state.X.memory} @ ghost =
  fun writes base_local state base after premise -> ghost_ (
    apply_def writes state.X.memory base state.X.machine.E.locals; emit_def writes base_local;
    match writes with
    | End -> X.run_def C.Empty state
    | Write (offset, value, rest) -> (match read value state.X.machine.E.locals with
      | None -> unreachable_ ()
      | Some word -> (match M.store state.X.memory base offset (S.I64 word) with
        | None -> unreachable_ ()
        | Some memory ->
          single offset value base_local state base word memory ();
          correct rest base_local {X.memory; machine = state.X.machine} base after ();
          X.append_correct (one offset value base_local) (emit rest base_local) state)))
