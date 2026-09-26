module B = Wasm_u32
module C = Wasm_code
module V = Hmc_tagged_cell
module Lower = Hmc_wasm_closure_write
module Copy = Wasm_cross_copy
module Copy_prefix = Wasm_cross_copy_prefix
module Write = Wasm_immediate_write
module Write_prefix = Wasm_immediate_write_prefix
module Header = Hmc_wasm_header_update
module PC = Hmc_wasm_pc_update
module Tag = Hmc_wasm_header_words
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module M = Wasm_memory
module Prefix = Wasm_instruction_prefix
module Region = Wasm_frame_write_prefix
module Access = Wasm_memory_region
module Bounds = Hmc_linear_bounds
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation

let[@def] (partial @ total) (fragment : Lower.fragment @ immutable) (before : B.bytes @ immutable)
    (source : B.u32) (target : B.u32) (current : B.bytes @ immutable) = ghost_ (
  Copy_prefix.partial fragment.Lower.copies before source target current
  || match Copy.apply fragment.Lower.copies before source target with
    | None -> false
    | Some copied -> match M.store copied target (Lower.zero ()) (S.I64 (Tag.tag ())) with
      | None -> false
      | Some tagged -> current === tagged
        || M.store tagged target (PC.offset ()) (S.I64 (Header.number fragment.Lower.code)) === Some current)

let (stores @ total) : (fragment : Lower.fragment) @ immutable -> (frame_local : B.u32) -> (heap_local : B.u32) ->
    (state : X.state) @ immutable -> (source : B.u32) -> (target : B.u32) ->
    (copied : B.bytes) @ immutable -> (tagged : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (low : B.u32) -> (high : B.u32) -> (fuel : C.count) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals frame_local === Some (S.I32 source)
      && L.get state.X.machine.E.locals heap_local === Some (S.I32 target)
      && Copy.apply fragment.Lower.copies state.X.memory source target === Some copied
      && M.store copied target (Lower.zero ()) (S.I64 (Tag.tag ())) === Some tagged
      && M.store tagged target (PC.offset ()) (S.I64 (Header.number fragment.Lower.code)) === Some after
      && Copy_prefix.bounded fragment.Lower.copies target low high
      && low <= target && target + 16 <= high && Bounds.covers state.X.memory low} ->
    {u : unit | match X.run (Prefix.take fuel (Lower.emit fragment frame_local heap_local)) state with
      | X.Done current -> current.X.machine.E.locals === state.X.machine.E.locals
        && partial fragment state.X.memory source target current.X.memory
        && P.equal_prefix low state.X.memory current.X.memory
        && Bytes.drop state.X.memory high === Bytes.drop current.X.memory high
        && V.length current.X.memory === V.length state.X.memory
      | _ -> false} @ ghost =
  fun fragment frame_local heap_local state source target copied tagged after low high fuel premise -> ghost_ (
    let copies = Copy.emit fragment.Lower.copies frame_local heap_local in
    let tag = Write.emit 0 heap_local (Tag.tag ()) in
    let code = Write.emit 8 heap_local (Header.number fragment.Lower.code) in
    let copied_state = {X.memory = copied; machine = state.X.machine} in
    let tagged_state = {X.memory = tagged; machine = state.X.machine} in
    let tail = E.append tag code in
    Lower.emit_def fragment frame_local heap_local; Lower.zero_def (); PC.emit_def fragment.Lower.code heap_local; PC.offset_def ();
    Copy.correct fragment.Lower.copies frame_local heap_local state source target copied ();
    Write.correct 0 heap_local (Tag.tag ()) copied_state target tagged ();
    Prefix.run_append fuel copies tail state copied_state ();
    match Prefix.remaining fuel copies with
    | None ->
      Copy_prefix.region fragment.Lower.copies frame_local heap_local state source target copied low high fuel ();
      (match X.run (Prefix.take fuel copies) state with
      | X.Done current -> partial_def fragment state.X.memory source target current.X.memory
      | _ -> ())
    | Some rest ->
      Copy_prefix.complete_region fragment.Lower.copies state.X.memory source target copied low high ();
      Bounds.same_length state.X.memory copied low ();
      Region.store_region copied tagged target 0 (Tag.tag ()) low high ();
      Bounds.same_length copied tagged low ();
      Region.prefix_transitive low state.X.memory copied tagged ();
      Prefix.run_append rest tag code copied_state tagged_state ();
      match Prefix.remaining rest tag with
      | None ->
        Write_prefix.region 0 heap_local (Tag.tag ()) copied_state target tagged low high rest ();
        (match X.run (Prefix.take rest tag) copied_state with
        | X.Done current ->
          Copy_prefix.partial_def fragment.Lower.copies state.X.memory source target copied;
          partial_def fragment state.X.memory source target current.X.memory;
          Region.prefix_transitive low state.X.memory copied current.X.memory ()
        | _ -> ())
      | Some remaining ->
        Write_prefix.region 8 heap_local (Header.number fragment.Lower.code) tagged_state target after low high remaining ();
        (match X.run (Prefix.take remaining code) tagged_state with
        | X.Done current ->
          partial_def fragment state.X.memory source target current.X.memory;
          Region.prefix_transitive low state.X.memory tagged current.X.memory ()
        | _ -> ()))

let (correct @ total) : (fragment : Lower.fragment) @ immutable -> (frame_local : B.u32) -> (heap_local : B.u32) ->
    (state : X.state) @ immutable -> (source : B.u32) -> (target : B.u32) -> (after : X.state) @ immutable ->
    (low : B.u32) -> (high : B.u32) -> (fuel : C.count) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals frame_local === Some (S.I32 source)
      && L.get state.X.machine.E.locals heap_local === Some (S.I32 target)
      && X.run (Lower.emit fragment frame_local heap_local) state === X.Done after
      && Copy_prefix.bounded fragment.Lower.copies target low high
      && low <= target && target + 16 <= high && Bounds.covers state.X.memory low} ->
    {u : unit | match X.run (Prefix.take fuel (Lower.emit fragment frame_local heap_local)) state with
      | X.Done current -> current.X.machine.E.locals === state.X.machine.E.locals
        && partial fragment state.X.memory source target current.X.memory
        && P.equal_prefix low state.X.memory current.X.memory
        && Bytes.drop state.X.memory high === Bytes.drop current.X.memory high
        && V.length current.X.memory === V.length state.X.memory
      | _ -> false} @ ghost =
  fun fragment frame_local heap_local state source target after low high fuel premise -> ghost_ (
    let copies = Copy.emit fragment.Lower.copies frame_local heap_local in
    let tag = Write.emit 0 heap_local (Tag.tag ()) in
    let code = Write.emit 8 heap_local (Header.number fragment.Lower.code) in
    Lower.emit_def fragment frame_local heap_local; Lower.zero_def (); PC.emit_def fragment.Lower.code heap_local; PC.offset_def ();
    X.append_correct copies (E.append tag code) state;
    match X.run copies state with
    | X.Done copied ->
      Copy_prefix.reflect fragment.Lower.copies frame_local heap_local state source target copied ();
      X.append_correct tag code copied;
      (match X.run tag copied with
      | X.Done tagged ->
        Write_prefix.reflect 0 heap_local (Tag.tag ()) copied target tagged ();
        Write_prefix.reflect 8 heap_local (Header.number fragment.Lower.code) tagged target after ();
        stores fragment frame_local heap_local state source target copied.X.memory tagged.X.memory after.X.memory low high fuel ()
      | _ -> ())
    | _ -> ())

let (accesses @ total) : (fragment : Lower.fragment) @ immutable -> (frame_local : B.u32) -> (heap_local : B.u32) ->
    (state : X.state) @ immutable -> (source : B.u32) -> (target : B.u32) -> (after : X.state) @ immutable ->
    (bounds : Access.bounds) @ immutable -> (fuel : C.count) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals frame_local === Some (S.I32 source)
      && L.get state.X.machine.E.locals heap_local === Some (S.I32 target)
      && X.run (Lower.emit fragment frame_local heap_local) state === X.Done after
      && Copy_prefix.access_bounds fragment.Lower.copies source target bounds
      && bounds.Access.write_low <= target && target + 16 <= bounds.Access.write_high} ->
    {u : unit | Access.trace (Prefix.take fuel (Lower.emit fragment frame_local heap_local)) state bounds} @ ghost =
  fun fragment frame_local heap_local state source target after bounds fuel premise -> ghost_ (
    let copies = Copy.emit fragment.Lower.copies frame_local heap_local in
    let tag = Write.emit 0 heap_local (Tag.tag ()) in
    let code = Write.emit 8 heap_local (Header.number fragment.Lower.code) in
    Lower.emit_def fragment frame_local heap_local; Lower.zero_def (); PC.emit_def fragment.Lower.code heap_local; PC.offset_def ();
    X.append_correct copies (E.append tag code) state;
    match X.run copies state with
    | X.Done copied ->
      Copy_prefix.reflect fragment.Lower.copies frame_local heap_local state source target copied ();
      X.append_correct tag code copied;
      (match X.run tag copied with
      | X.Done tagged ->
        Write_prefix.reflect 0 heap_local (Tag.tag ()) copied target tagged ();
        Write_prefix.reflect 8 heap_local (Header.number fragment.Lower.code) tagged target after ();
        Copy_prefix.accesses fragment.Lower.copies frame_local heap_local state source target copied.X.memory bounds ();
        Write_prefix.accesses 0 heap_local (Tag.tag ()) copied target tagged.X.memory bounds ();
        Write_prefix.accesses 8 heap_local (Header.number fragment.Lower.code) tagged target after.X.memory bounds ();
        Access.append tag code copied tagged bounds ();
        Access.append copies (E.append tag code) state copied bounds ();
        Access.prefix fuel (Lower.emit fragment frame_local heap_local) state bounds ()
      | _ -> ())
    | _ -> ())
