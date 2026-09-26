module B = Wasm_u32
module C = Wasm_code
module S = Wasm_scalar
module L = Wasm_locals
module E = Wasm_execution
module M = Wasm_memory
module X = Wasm_memory_execution
module V = Hmc_tagged_cell
module Lower = Wasm_memory_lowering
module Unique = Wasm_memory_unique

type writes = End | Write of B.u32 * B.u32 * writes [@@inductive]
let[@def] rec (apply @ total) (writes : writes @ immutable) (memory : B.bytes @ immutable) (base : B.u32)
    (locals : S.stack @ immutable) : B.bytes option @ immutable =
  match writes with
  | End -> Some memory
  | Write (offset, source, rest) -> (match L.get locals source with
    | Some (S.I64 word) -> (match M.store memory base offset (S.I64 word) with
      | None -> None | Some next -> apply rest next base locals)
    | _ -> None)
let[@def] rec (emit @ total) (writes : writes @ immutable) (base_local : B.u32) : C.t @ immutable =
  match writes with
  | End -> C.Empty
  | Write (offset, source, rest) -> E.append (Lower.write_code M.W64 offset base_local source) (emit rest base_local)
let rec (compile @ total) : (writes : writes) @ immutable -> (base_local : B.u32) ->
    (state : X.state) @ ghost -> (base : B.u32) @ ghost -> (expected : B.bytes) @ ghost ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && apply writes state.X.memory base state.X.machine.E.locals === Some expected} ->
    {code : C.t | code === emit writes base_local
      && X.run code state === X.Done {X.memory = expected; machine = state.X.machine}
      && V.length expected === V.length state.X.memory} @ immutable =
  fun writes base_local state base expected premise ->
    ghost_ (emit_def writes base_local; apply_def writes state.X.memory base state.X.machine.E.locals);
    match writes with
    | End -> ghost_ (X.run_def C.Empty state); C.Empty
    | Write (offset, source, rest) ->
      let word = ghost_ (match L.get state.X.machine.E.locals source with
        | Some (S.I64 word) -> word | _ -> unreachable_ ()) in
      let next = ghost_ (match M.store state.X.memory base offset (S.I64 word) with
        | Some memory -> memory | None -> unreachable_ ()) in
      ghost_ (M.width_def (S.I64 word));
      let first = Lower.store M.W64 offset base_local source state base (S.I64 word) () in
      let after = ghost_ (match X.run first state with X.Done after -> after | _ -> unreachable_ ()) in
      ghost_ (match M.address base offset M.W64 with
        | None -> ()
        | Some address -> Unique.updated state.X.memory address (M.encode (S.I64 word)) after.X.memory next ());
      let tail = compile rest base_local after base expected () in
      let code = E.append first tail in
      ghost_ (X.append_correct first tail state); code
let (correct @ total) : (writes : writes) @ immutable -> (base_local : B.u32) ->
    (state : X.state) @ immutable -> (base : B.u32) -> (expected : B.bytes) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && apply writes state.X.memory base state.X.machine.E.locals === Some expected} ->
    {u : unit | X.run (emit writes base_local) state === X.Done {X.memory = expected; machine = state.X.machine}
      && V.length expected === V.length state.X.memory} @ ghost = fun writes base_local state base expected premise -> ghost_ (
  let _code = compile writes base_local state base expected () in ())
