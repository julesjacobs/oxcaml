module B = Wasm_u32
module C = Wasm_code
module I = Wasm_instruction
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module M = Wasm_memory
module L = Wasm_locals
module Plan = Wasm_parallel_copy
module Copy = Wasm_cross_copy
module Prefix = Wasm_instruction_prefix
module Region = Wasm_frame_write_prefix
module P = Hmc_linear_preservation
module Bytes = Hmc_linear_bytes
module V = Hmc_tagged_cell
module Bounds = Hmc_linear_bounds

let[@def] rec (partial @ total) (plan : Plan.plan @ immutable) (before : B.bytes @ immutable)
    (source_base : B.u32) (target_base : B.u32) (current : B.bytes @ immutable) = ghost_ (
  current === before || Copy.apply plan before source_base target_base === Some current
  || match plan with Plan.End -> false
    | Plan.Copy (_, _, rest) -> partial rest before source_base target_base current)

let[@def] rec (bounded @ total) (plan : Plan.plan @ immutable) (base : B.u32)
    (low : B.u32) (high : B.u32) = ghost_ (match plan with
  | Plan.End -> true
  | Plan.Copy (_, offset, rest) -> low <= base + offset && base + offset + 8 <= high
    && bounded rest base low high)

let rec (complete_region @ total) : (plan : Plan.plan) @ immutable -> (before : B.bytes) @ immutable ->
    (source_base : B.u32) -> (target_base : B.u32) -> (after : B.bytes) @ immutable ->
    (low : B.u32) -> (high : B.u32) ->
    {u : unit | Copy.apply plan before source_base target_base === Some after
      && bounded plan target_base low high && Bounds.covers before low} ->
    {u : unit | P.equal_prefix low before after && Bytes.drop before high === Bytes.drop after high
      && V.length after === V.length before} @ ghost = fun plan before source_base target_base after low high premise -> ghost_ (
      Copy.apply_def plan before source_base target_base; bounded_def plan target_base low high;
      match plan with
      | Plan.End -> Region.reflexive before low ()
      | Plan.Copy (source, destination, rest) -> match M.load before source_base source M.W64 with
        | Some (S.I64 word) -> (match Copy.apply rest before source_base target_base with
          | None -> ()
          | Some middle ->
            complete_region rest before source_base target_base middle low high ();
            Region.store_region middle after target_base destination word low high ();
            Region.prefix_transitive low before middle after ())
        | _ -> ())

let rec (partial_region @ total) : (plan : Plan.plan) @ immutable -> (before : B.bytes) @ immutable ->
    (source_base : B.u32) -> (target_base : B.u32) -> (current : B.bytes) @ immutable ->
    (low : B.u32) -> (high : B.u32) ->
    {u : unit | partial plan before source_base target_base current
      && bounded plan target_base low high && Bounds.covers before low} ->
    {u : unit | P.equal_prefix low before current && Bytes.drop before high === Bytes.drop current high
      && V.length current === V.length before} @ ghost = fun plan before source_base target_base current low high premise -> ghost_ (
      partial_def plan before source_base target_base current; bounded_def plan target_base low high;
      if current === before then Region.reflexive before low ()
      else if Copy.apply plan before source_base target_base === Some current then
        complete_region plan before source_base target_base current low high ()
      else match plan with Plan.End -> ()
      | Plan.Copy (_, _, rest) -> partial_region rest before source_base target_base current low high ())

let rec (prefix @ total) : (plan : Plan.plan) @ immutable -> (source_local : B.u32) -> (target_local : B.u32) ->
    (state : X.state) @ immutable -> (source_base : B.u32) -> (target_base : B.u32) ->
    (expected : B.bytes) @ immutable -> (fuel : C.count) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals source_local === Some (S.I32 source_base)
      && L.get state.X.machine.E.locals target_local === Some (S.I32 target_base)
      && Copy.apply plan state.X.memory source_base target_base === Some expected} ->
    {u : unit | match X.run (Prefix.take fuel (Copy.emit plan source_local target_local)) state with
      | X.Done current -> current.X.machine.E.locals === state.X.machine.E.locals
        && partial plan state.X.memory source_base target_base current.X.memory
      | _ -> false} @ ghost = fun plan source_local target_local state source_base target_base expected fuel premise -> ghost_ (
      Copy.emit_def plan source_local target_local; Copy.apply_def plan state.X.memory source_base target_base;
      partial_def plan state.X.memory source_base target_base state.X.memory;
      match plan with
      | Plan.End -> Prefix.take_def fuel C.Empty; X.run_def C.Empty state
      | Plan.Copy (source, destination, rest) ->
        let locals = state.X.machine.E.locals in let stack = state.X.machine.E.stack in
        let child = Copy.emit rest source_local target_local in
        let write = C.Next (I.I64_store (3, destination), C.Empty) in
        let tail = E.append child write in
        let read = C.Next (I.I64_load (3, source), tail) in
        let address = C.Next (I.Local_get source_local, read) in
        Prefix.take_def fuel (Copy.emit plan source_local target_local);
        match fuel with
        | C.Zero -> X.run_def C.Empty state
        | C.Succ fuel1 ->
          let addressed = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (S.I32 target_base, stack)}} in
          X.run_def (Prefix.take fuel (Copy.emit plan source_local target_local)) state;
          X.step_def (I.Local_get target_local) state; E.step_def (I.Local_get target_local) state.X.machine;
          Prefix.take_def fuel1 address;
          match fuel1 with
          | C.Zero -> X.run_def C.Empty addressed
          | C.Succ fuel2 ->
            let reading = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (S.I32 source_base, addressed.X.machine.E.stack)}} in
            X.run_def (Prefix.take fuel1 address) addressed;
            X.step_def (I.Local_get source_local) addressed; E.step_def (I.Local_get source_local) addressed.X.machine;
            Prefix.take_def fuel2 read;
            match fuel2 with
            | C.Zero -> X.run_def C.Empty reading
            | C.Succ fuel3 -> match M.load state.X.memory source_base source M.W64 with
              | Some (S.I64 word) ->
                let captured = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (S.I64 word, addressed.X.machine.E.stack)}} in
                X.run_def (Prefix.take fuel2 read) reading;
                X.step_def (I.I64_load (3, source)) reading; X.read_def M.W64 source reading;
                (match Copy.apply rest state.X.memory source_base target_base with
                | None -> ()
                | Some memory ->
                  let restored = {X.memory; machine = captured.X.machine} in
                  Copy.correct rest source_local target_local captured source_base target_base memory ();
                  Prefix.run_append fuel3 child write captured restored ();
                  match Prefix.remaining fuel3 child with
                  | None ->
                    prefix rest source_local target_local captured source_base target_base memory fuel3 ();
                    (match X.run (Prefix.take fuel3 child) captured with
                    | X.Done current -> partial_def plan state.X.memory source_base target_base current.X.memory
                    | _ -> ())
                  | Some fuel4 ->
                    Prefix.take_def fuel4 write;
                    match fuel4 with
                    | C.Zero -> X.run_def C.Empty restored;
                      partial_def plan state.X.memory source_base target_base memory;
                      partial_def rest state.X.memory source_base target_base memory
                    | C.Succ fuel5 ->
                      Prefix.take_def fuel5 C.Empty;
                      X.run_def write restored; X.step_def (I.I64_store (3, destination)) restored;
                      X.write_def M.W64 destination restored; X.compatible_def (S.I64 word) M.W64; M.width_def (S.I64 word);
                      X.run_def C.Empty {X.memory = expected; machine = state.X.machine};
                      partial_def plan state.X.memory source_base target_base expected)
              | _ -> ())

let (region @ total) : (plan : Plan.plan) @ immutable -> (source_local : B.u32) -> (target_local : B.u32) ->
    (state : X.state) @ immutable -> (source_base : B.u32) -> (target_base : B.u32) ->
    (expected : B.bytes) @ immutable -> (low : B.u32) -> (high : B.u32) -> (fuel : C.count) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals source_local === Some (S.I32 source_base)
      && L.get state.X.machine.E.locals target_local === Some (S.I32 target_base)
      && Copy.apply plan state.X.memory source_base target_base === Some expected
      && bounded plan target_base low high && Bounds.covers state.X.memory low} ->
    {u : unit | match X.run (Prefix.take fuel (Copy.emit plan source_local target_local)) state with
      | X.Done current -> current.X.machine.E.locals === state.X.machine.E.locals
        && partial plan state.X.memory source_base target_base current.X.memory
        && P.equal_prefix low state.X.memory current.X.memory
        && Bytes.drop state.X.memory high === Bytes.drop current.X.memory high
        && V.length current.X.memory === V.length state.X.memory
      | _ -> false} @ ghost = fun plan source_local target_local state source_base target_base expected low high fuel premise -> ghost_ (
        prefix plan source_local target_local state source_base target_base expected fuel ();
        match X.run (Prefix.take fuel (Copy.emit plan source_local target_local)) state with
        | X.Done current -> partial_region plan state.X.memory source_base target_base current.X.memory low high ()
        | _ -> ())

module Access = Wasm_memory_region

let[@def] rec (access_bounds @ total) (plan : Plan.plan @ immutable) (source_base : B.u32)
    (target_base : B.u32) (bounds : Access.bounds @ immutable) = ghost_ (match plan with
  | Plan.End -> true
  | Plan.Copy (source, destination, rest) -> bounds.Access.read_low <= source_base + source
    && source_base + source + 8 <= bounds.Access.read_high
    && bounds.Access.write_low <= target_base + destination
    && target_base + destination + 8 <= bounds.Access.write_high
    && access_bounds rest source_base target_base bounds)

let rec (accesses @ total) : (plan : Plan.plan) @ immutable -> (source_local : B.u32) -> (target_local : B.u32) ->
    (state : X.state) @ immutable -> (source_base : B.u32) -> (target_base : B.u32) ->
    (expected : B.bytes) @ immutable -> (bounds : Access.bounds) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals source_local === Some (S.I32 source_base)
      && L.get state.X.machine.E.locals target_local === Some (S.I32 target_base)
      && Copy.apply plan state.X.memory source_base target_base === Some expected
      && access_bounds plan source_base target_base bounds} ->
    {u : unit | Access.trace (Copy.emit plan source_local target_local) state bounds} @ ghost =
  fun plan source_local target_local state source_base target_base expected bounds premise -> ghost_ (
    Copy.emit_def plan source_local target_local; Copy.apply_def plan state.X.memory source_base target_base;
    access_bounds_def plan source_base target_base bounds;
    Access.trace_def (Copy.emit plan source_local target_local) state bounds;
    match plan with
    | Plan.End -> ()
    | Plan.Copy (source, destination, rest) -> match M.load state.X.memory source_base source M.W64 with
      | Some (S.I64 word) -> (match Copy.apply rest state.X.memory source_base target_base with
        | None -> ()
        | Some memory ->
          let locals = state.X.machine.E.locals in let stack = state.X.machine.E.stack in
          let addressed = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (S.I32 target_base, stack)}} in
          let reading = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (S.I32 source_base, addressed.X.machine.E.stack)}} in
          let captured = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (S.I64 word, addressed.X.machine.E.stack)}} in
          let restored = {X.memory; machine = captured.X.machine} in
          let write = C.Next (I.I64_store (3, destination), C.Empty) in
          let child = Copy.emit rest source_local target_local in
          let tail = E.append child write in
          let read = C.Next (I.I64_load (3, source), tail) in
          let address = C.Next (I.Local_get source_local, read) in
          Access.access_def (I.Local_get target_local) state bounds;
          X.step_def (I.Local_get target_local) state; E.step_def (I.Local_get target_local) state.X.machine;
          Access.trace_def address addressed bounds; Access.access_def (I.Local_get source_local) addressed bounds;
          X.step_def (I.Local_get source_local) addressed; E.step_def (I.Local_get source_local) addressed.X.machine;
          Access.trace_def read reading bounds; Access.access_def (I.I64_load (3, source)) reading bounds;
          Access.read_def M.W64 3 source reading bounds; M.size_def M.W64;
          X.step_def (I.I64_load (3, source)) reading; X.read_def M.W64 source reading;
          accesses rest source_local target_local captured source_base target_base memory bounds ();
          Copy.correct rest source_local target_local captured source_base target_base memory ();
          Access.trace_def write restored bounds; Access.access_def (I.I64_store (3, destination)) restored bounds;
          Access.write_def M.W64 3 destination restored bounds; M.width_def (S.I64 word);
          X.step_def (I.I64_store (3, destination)) restored; X.write_def M.W64 destination restored;
          X.compatible_def (S.I64 word) M.W64;
          Access.trace_def C.Empty {X.memory = expected; machine = state.X.machine} bounds;
          Access.append child write captured restored bounds ())
      | _ -> ())

let (prefix_accesses @ total) : (plan : Plan.plan) @ immutable -> (source_local : B.u32) -> (target_local : B.u32) ->
    (state : X.state) @ immutable -> (source_base : B.u32) -> (target_base : B.u32) ->
    (expected : B.bytes) @ immutable -> (bounds : Access.bounds) @ immutable -> (fuel : C.count) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals source_local === Some (S.I32 source_base)
      && L.get state.X.machine.E.locals target_local === Some (S.I32 target_base)
      && Copy.apply plan state.X.memory source_base target_base === Some expected
      && access_bounds plan source_base target_base bounds} ->
    {u : unit | Access.trace (Prefix.take fuel (Copy.emit plan source_local target_local)) state bounds} @ ghost =
  fun plan source_local target_local state source_base target_base expected bounds fuel premise -> ghost_ (
    accesses plan source_local target_local state source_base target_base expected bounds ();
    Access.prefix fuel (Copy.emit plan source_local target_local) state bounds ())

let rec (reflect @ total) : (plan : Plan.plan) @ immutable -> (source_local : B.u32) -> (target_local : B.u32) ->
    (state : X.state) @ immutable -> (source_base : B.u32) -> (target_base : B.u32) -> (after : X.state) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals source_local === Some (S.I32 source_base)
      && L.get state.X.machine.E.locals target_local === Some (S.I32 target_base)
      && X.run (Copy.emit plan source_local target_local) state === X.Done after} ->
    {u : unit | Copy.apply plan state.X.memory source_base target_base === Some after.X.memory
      && after.X.machine === state.X.machine} @ ghost =
  fun plan source_local target_local state source_base target_base after premise -> ghost_ (
    Copy.emit_def plan source_local target_local; Copy.apply_def plan state.X.memory source_base target_base;
    match plan with
    | Plan.End -> X.run_def C.Empty state
    | Plan.Copy (source, destination, rest) ->
      let locals = state.X.machine.E.locals in let stack = state.X.machine.E.stack in
      let addressed = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (S.I32 target_base, stack)}} in
      let reading = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (S.I32 source_base, addressed.X.machine.E.stack)}} in
      let child = Copy.emit rest source_local target_local in
      let write = C.Next (I.I64_store (3, destination), C.Empty) in
      let tail = E.append child write in
      let read = C.Next (I.I64_load (3, source), tail) in
      let address = C.Next (I.Local_get source_local, read) in
      X.run_def (Copy.emit plan source_local target_local) state;
      X.step_def (I.Local_get target_local) state; E.step_def (I.Local_get target_local) state.X.machine;
      X.run_def address addressed;
      X.step_def (I.Local_get source_local) addressed; E.step_def (I.Local_get source_local) addressed.X.machine;
      X.run_def read reading; X.step_def (I.I64_load (3, source)) reading; X.read_def M.W64 source reading;
      match M.load state.X.memory source_base source M.W64 with
      | Some (S.I64 word) ->
        let captured = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (S.I64 word, addressed.X.machine.E.stack)}} in
        X.append_correct child write captured;
        (match X.run child captured with
        | X.Done middle ->
          reflect rest source_local target_local captured source_base target_base middle ();
          X.run_def write middle; X.step_def (I.I64_store (3, destination)) middle;
          X.write_def M.W64 destination middle; X.compatible_def (S.I64 word) M.W64; M.width_def (S.I64 word);
          (match M.store middle.X.memory target_base destination (S.I64 word) with
          | None -> ()
          | Some memory -> X.run_def C.Empty {X.memory; machine = state.X.machine})
        | _ -> ())
      | _ ->
        M.load_def state.X.memory source_base source M.W64;
        match M.address source_base source M.W64 with
        | None -> ()
        | Some address -> match Bytes.load state.X.memory address (M.count M.W64) with
          | None -> ()
          | Some bytes -> M.decode_def M.W64 bytes)

