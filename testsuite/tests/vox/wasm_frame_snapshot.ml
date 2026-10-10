module B = Wasm_u32
module C = Wasm_code
module S = Wasm_scalar
module L = Wasm_locals
module E = Wasm_execution
module M = Wasm_memory
module X = Wasm_memory_execution
module Lower = Wasm_memory_lowering
module Unique = Wasm_local_unique

type reads = End | Read of B.u32 * B.u32 * reads [@@inductive]
let[@def] rec (separate @ total) (reads : reads @ immutable) (base_local : B.u32) =
  match reads with End -> true | Read (_, destination, rest) -> destination <> base_local && separate rest base_local
let[@def] rec (project @ total) (reads : reads @ immutable) (memory : B.bytes @ immutable) (base : B.u32)
    (locals : S.stack @ immutable) : S.stack option @ immutable =
  match reads with
  | End -> Some locals
  | Read (offset, destination, rest) -> (match M.load memory base offset M.W64 with
    | None -> None
    | Some value -> (match L.set locals destination value with
      | None -> None | Some next -> project rest memory base next))
let[@def] rec (emit @ total) (reads : reads @ immutable) (base_local : B.u32) : C.t @ immutable =
  match reads with
  | End -> C.Empty
  | Read (offset, destination, rest) ->
    E.append (Lower.read_code M.W64 offset base_local destination) (emit rest base_local)
let rec (compile @ total) : (reads : reads) @ immutable -> (base_local : B.u32) ->
    (state : X.state) @ ghost -> (base : B.u32) @ ghost -> (expected : S.stack) @ ghost ->
    {u : unit | separate reads base_local
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && project reads state.X.memory base state.X.machine.E.locals === Some expected} ->
    {code : C.t | code === emit reads base_local
      && L.get expected base_local === Some (S.I32 base) && X.run code state === X.Done {X.memory = state.X.memory;
      machine = {E.locals = expected; stack = state.X.machine.E.stack}}} @ immutable =
  fun reads base_local state base expected premise ->
    ghost_ (emit_def reads base_local; separate_def reads base_local; project_def reads state.X.memory base state.X.machine.E.locals);
    match reads with
    | End -> ghost_ (X.run_def C.Empty state); C.Empty
    | Read (offset, destination, rest) ->
      let value = ghost_ (match M.load state.X.memory base offset M.W64 with
        | Some value -> value | None -> unreachable_ ()) in
      let updated = ghost_ (match L.set state.X.machine.E.locals destination value with
        | Some locals -> locals | None -> unreachable_ ()) in
      let first = Lower.load M.W64 offset base_local destination state base value () in
      let after = ghost_ (match X.run first state with X.Done after -> after | _ -> unreachable_ ()) in
      ghost_ (
        Unique.replaced state.X.machine.E.locals destination value after.X.machine.E.locals updated ();
        L.other_local state.X.machine.E.locals destination value after.X.machine.E.locals base_local ());
      let tail = compile rest base_local after base expected () in
      let code = E.append first tail in
      ghost_ (X.append_correct first tail state);
      code

let (correct @ total) : (reads : reads) @ immutable -> (base_local : B.u32) ->
    (state : X.state) @ immutable -> (base : B.u32) -> (expected : S.stack) @ immutable ->
    {u : unit | separate reads base_local
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && project reads state.X.memory base state.X.machine.E.locals === Some expected} ->
    {u : unit | L.get expected base_local === Some (S.I32 base)
      && X.run (emit reads base_local) state === X.Done {X.memory = state.X.memory;
      machine = {E.locals = expected; stack = state.X.machine.E.stack}}} @ ghost =
  fun reads base_local state base expected premise -> ghost_ (
    let _code = compile reads base_local state base expected () in ())
