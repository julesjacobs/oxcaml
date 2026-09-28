module B = Wasm_u32
module C = Wasm_code
module Lower = Hmc_wasm_relayout
module Plan = Wasm_parallel_copy
module Copy = Wasm_cross_copy_prefix
module Parallel = Wasm_parallel_copy_prefix
module Write = Wasm_immediate_write
module Write_prefix = Wasm_immediate_write_prefix
module PC = Hmc_wasm_pc_update
module Header = Hmc_wasm_header_update
module Prefix = Wasm_instruction_prefix
module Region = Wasm_frame_write_prefix
module Access = Wasm_memory_region
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module M = Wasm_memory
module Bounds = Hmc_linear_bounds
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation
module V = Hmc_tagged_cell

let[@def] (partial @ total) (fragment : Lower.fragment @ immutable) (before : B.bytes @ immutable)
    (base : B.u32) (current : B.bytes @ immutable) = ghost_ (
  Copy.partial fragment.Lower.copies before base base current
  || match Plan.apply fragment.Lower.copies before base with
    | None -> false | Some copied -> Lower.finish copied base fragment.Lower.pc === Some current)

let (stores @ total) : (fragment : Lower.fragment) @ immutable -> (base_local : B.u32) ->
    (state : X.state) @ immutable -> (base : B.u32) -> (after : X.state) @ immutable ->
    (low : B.u32) -> (high : B.u32) -> (fuel : C.count) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && X.run (Lower.emit fragment base_local) state === X.Done after
      && Copy.bounded fragment.Lower.copies base low high
      && low <= base + 8 && base + 16 <= high && Bounds.covers state.X.memory low} ->
    {u : unit | match X.run (Prefix.take fuel (Lower.emit fragment base_local)) state with
      | X.Done current -> current.X.machine.E.locals === state.X.machine.E.locals
        && partial fragment state.X.memory base current.X.memory
        && P.equal_prefix low state.X.memory current.X.memory
        && Bytes.drop state.X.memory high === Bytes.drop current.X.memory high
        && V.length current.X.memory === V.length state.X.memory
      | _ -> false} @ ghost = fun fragment base_local state base after low high fuel premise -> ghost_ (
    let copies = Plan.emit fragment.Lower.copies base_local in
    let code = Write.emit 8 base_local (Header.number fragment.Lower.pc) in
    Lower.emit_def fragment base_local; PC.emit_def fragment.Lower.pc base_local; PC.offset_def ();
    X.append_correct copies code state;
    Parallel.emit fragment.Lower.copies base_local;
    Parallel.apply fragment.Lower.copies state.X.memory base;
    match X.run copies state with
    | X.Done copied ->
      Copy.reflect fragment.Lower.copies base_local base_local state base base copied ();
      Write_prefix.reflect 8 base_local (Header.number fragment.Lower.pc) copied base after ();
      Prefix.run_append fuel copies code state copied ();
      (match Prefix.remaining fuel copies with
      | None ->
        Parallel.region fragment.Lower.copies base_local state base copied low high fuel ();
        (match X.run (Prefix.take fuel copies) state with
        | X.Done current -> partial_def fragment state.X.memory base current.X.memory
        | _ -> ())
      | Some rest ->
        Copy.complete_region fragment.Lower.copies state.X.memory base base copied.X.memory low high ();
        Bounds.same_length state.X.memory copied.X.memory low ();
        Write_prefix.region 8 base_local (Header.number fragment.Lower.pc) copied base after.X.memory low high rest ();
        (match X.run (Prefix.take rest code) copied with
        | X.Done current ->
          Copy.partial_def fragment.Lower.copies state.X.memory base base copied.X.memory;
          Lower.finish_def copied.X.memory base fragment.Lower.pc;
          partial_def fragment state.X.memory base current.X.memory;
          Region.prefix_transitive low state.X.memory copied.X.memory current.X.memory ()
        | _ -> ()))
    | _ -> ())

let (accesses @ total) : (fragment : Lower.fragment) @ immutable -> (base_local : B.u32) ->
    (state : X.state) @ immutable -> (base : B.u32) -> (after : X.state) @ immutable ->
    (bounds : Access.bounds) @ immutable -> (fuel : C.count) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && X.run (Lower.emit fragment base_local) state === X.Done after
      && Copy.access_bounds fragment.Lower.copies base base bounds
      && bounds.Access.write_low <= base + 8 && base + 16 <= bounds.Access.write_high} ->
    {u : unit | Access.trace (Prefix.take fuel (Lower.emit fragment base_local)) state bounds} @ ghost =
  fun fragment base_local state base after bounds fuel premise -> ghost_ (
    let copies = Plan.emit fragment.Lower.copies base_local in
    let code = Write.emit 8 base_local (Header.number fragment.Lower.pc) in
    Lower.emit_def fragment base_local; PC.emit_def fragment.Lower.pc base_local; PC.offset_def ();
    X.append_correct copies code state; Parallel.emit fragment.Lower.copies base_local;
    match X.run copies state with
    | X.Done copied ->
      Copy.reflect fragment.Lower.copies base_local base_local state base base copied ();
      Write_prefix.reflect 8 base_local (Header.number fragment.Lower.pc) copied base after ();
      Copy.accesses fragment.Lower.copies base_local base_local state base base copied.X.memory bounds ();
      Write_prefix.accesses 8 base_local (Header.number fragment.Lower.pc) copied base after.X.memory bounds ();
      Access.append copies code state copied bounds ();
      Access.prefix fuel (Lower.emit fragment base_local) state bounds ()
    | _ -> ())

let (correct @ total) : (signature : Hmc_cfg_ir.signature) @ immutable -> (instruction : Hmc_cfg_ir.instruction) @ immutable ->
    (capacity : Lower.count) -> (max_pc : B.u32) -> (fragment : Lower.fragment) @ immutable -> (base_local : B.u32) ->
    (state : X.state) @ immutable -> (base : B.u32) -> (after : X.state) @ immutable ->
    (stop : B.u32) -> (fuel : C.count) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && X.run (Lower.emit fragment base_local) state === X.Done after
      && Hmc_wasm_relayout_geometry.matches signature instruction capacity max_pc fragment.Lower.copies fragment.Lower.pc fragment.Lower.required
      && stop = base + 16 + 16 * capacity && Bounds.covers state.X.memory base} ->
    {u : unit | match X.run (Prefix.take fuel (Lower.emit fragment base_local)) state with
      | X.Done current -> current.X.machine.E.locals === state.X.machine.E.locals
        && partial fragment state.X.memory base current.X.memory
        && P.equal_prefix base state.X.memory current.X.memory
        && Bytes.drop state.X.memory stop === Bytes.drop current.X.memory stop
        && V.length current.X.memory === V.length state.X.memory
      | _ -> false} @ ghost =
  fun signature instruction capacity max_pc fragment base_local state base after stop fuel premise -> ghost_ (
    Hmc_wasm_relayout_prefix_bounds.correct signature instruction capacity max_pc fragment base stop ();
    stores fragment base_local state base after base stop fuel ())
