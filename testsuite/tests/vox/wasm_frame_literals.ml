module B = Wasm_u32
module W = Hmc_word64
module C = Wasm_code
module S = Wasm_scalar
module L = Wasm_locals
module E = Wasm_execution
module M = Wasm_memory
module X = Wasm_memory_execution
module Write = Wasm_immediate_write

type writes = End | Write of B.u32 * W.t * writes [@@inductive]
let[@def] rec (apply @ total) (writes : writes @ immutable) (memory : B.bytes @ immutable) (base : B.u32)
    : B.bytes option @ immutable = match writes with
  | End -> Some memory
  | Write (offset, word, rest) -> (match M.store memory base offset (S.I64 word) with
    | None -> None | Some next -> apply rest next base)
let[@def] rec (emit @ total) (writes : writes @ immutable) (base_local : B.u32) : C.t @ immutable =
  match writes with End -> C.Empty
  | Write (offset, word, rest) -> E.append (Write.emit offset base_local word) (emit rest base_local)
let rec (correct @ total) : (writes : writes) @ immutable -> (base_local : B.u32) ->
    (state : X.state) @ immutable -> (base : B.u32) -> (memory : B.bytes) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && apply writes state.X.memory base === Some memory} ->
    {u : unit | X.run (emit writes base_local) state === X.Done {X.memory; machine = state.X.machine}
      && Hmc_tagged_cell.length memory === Hmc_tagged_cell.length state.X.memory} @ ghost =
  fun writes base_local state base memory premise -> ghost_ (
    emit_def writes base_local; apply_def writes state.X.memory base;
    match writes with
    | End -> X.run_def C.Empty state
    | Write (offset, word, rest) -> (match M.store state.X.memory base offset (S.I64 word) with
      | None -> ()
      | Some next ->
        Write.correct offset base_local word state base next ();
        correct rest base_local {X.memory = next; machine = state.X.machine} base memory ();
        X.append_correct (Write.emit offset base_local word) (emit rest base_local) state))
