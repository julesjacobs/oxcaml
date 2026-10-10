module B = Wasm_u32
module W = Hmc_word64
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module M = Wasm_memory
module T = Wasm_control
module Lift = Wasm_control_lift
module Fuel = Wasm_control_compose
module Table = Hmc_wasm_dispatch_code
module Block = Hmc_wasm_block_lower
module Probe = Wasm_local_probe
module Header = Hmc_wasm_header_update
let[@def] (trap @ total) (unit : unit) = T.Instruction (I.Plain I.Unreachable, T.Empty)
let[@def] (label @ total) (labels : T.labels @ immutable) = T.Label ({T.restart = None; continuation = T.Empty; saved = S.Empty}, labels)
let[@def] rec (emit @ total) (table : Table.table @ immutable) (code_local : B.u32) = match table with
  | Table.Empty -> trap ()
  | Table.Add (number, fragment, rest) ->
    Lift.embed (Probe.emit number code_local) (T.If (fragment, emit rest code_local, T.Empty))
let[@def] rec (cost @ total) (table : Table.table @ immutable) (pc : W.limb) (code_local : B.u32) = match table with
  | Table.Empty -> C.Zero
  | Table.Add (number, _, rest) -> Fuel.add (C.length (Probe.emit number code_local)) (C.Succ (if pc = number then C.Zero else cost rest pc code_local))
let[@def] rec (selection @ total) (table : Table.table @ immutable) (pc : W.limb) (code_local : B.u32)
    (labels : T.labels @ immutable) (state : X.state @ immutable) : T.configuration @ immutable = match table with
  | Table.Empty -> {T.code = trap (); labels; state}
  | Table.Add (number, fragment, rest) ->
    if pc = number then {T.code = fragment; labels = label labels; state}
    else selection rest pc code_local (label labels) state
let rec (correct @ total) : (table : Table.table) @ immutable -> (pc : W.limb) -> (code_local : B.u32) ->
    (labels : T.labels) @ immutable -> (state : X.state) @ immutable ->
    {u : unit | state.X.machine.E.stack === S.Empty && Wasm_locals.get state.X.machine.E.locals code_local === Some (S.I32 pc)} ->
    {u : unit | T.run (cost table pc code_local) {T.code = emit table code_local; labels; state}
      === T.Running (selection table pc code_local labels state)} @ ghost = fun table pc code_local labels state premise -> ghost_ (
    emit_def table code_local; cost_def table pc code_local; selection_def table pc code_local labels state;
    match table with
    | Table.Empty -> T.run_def C.Zero {T.code = trap (); labels; state}
    | Table.Add (number, fragment, rest) ->
      let yes = fragment in
      let no = emit rest code_local in
      let branch = T.If (yes, no, T.Empty) in
      let condition = S.boolean (pc = number) in
      let tested = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 condition, S.Empty)}} in
      let start = {T.code = emit table code_local; labels; state} in
      let pending = {T.code = branch; labels; state = tested} in
      let remaining = if pc = number then C.Zero else cost rest pc code_local in
      Probe.correct number pc code_local state (); Probe.straight number code_local;
      Lift.correct (Probe.emit number code_local) branch labels state tested ();
      Fuel.correct (C.length (Probe.emit number code_local)) (C.Succ remaining) start;
      T.run_def (C.Succ remaining) pending; T.step_def pending; S.boolean_def (pc = number);
      T.stack_def tested S.Empty;
      let entering = {T.code = branch; labels; state} in
      T.enter_def (if condition <> 0 then yes else no) T.Empty None entering;
      T.stack_def state S.Empty; label_def labels;
      if pc = number then T.run_def C.Zero {T.code = yes; labels = label labels; state}
      else correct rest pc code_local (label labels) state ())
let rec (selected_code @ total) : (table : Table.table) @ immutable -> (pc : W.limb) -> (code_local : B.u32) ->
    (labels : T.labels) @ immutable -> (state : X.state) @ immutable ->
    {u : unit | (selection table pc code_local labels state).T.code ===
      (match Table.lookup table pc with None -> trap () | Some fragment -> fragment)
      && (selection table pc code_local labels state).T.state === state} @ ghost = fun table pc code_local labels state -> ghost_ (
    selection_def table pc code_local labels state; Table.lookup_def table pc;
    match table with Table.Empty -> () | Table.Add (number, _, rest) -> if pc = number then () else selected_code rest pc code_local (label labels) state)
let (reject @ total) : (table : Table.table) @ immutable -> (pc : W.limb) -> (code_local : B.u32) ->
    (labels : T.labels) @ immutable -> (state : X.state) @ immutable ->
    {u : unit | Table.lookup table pc === None && state.X.machine.E.stack === S.Empty
      && Wasm_locals.get state.X.machine.E.locals code_local === Some (S.I32 pc)} ->
    {u : unit | T.run (Fuel.add (cost table pc code_local) (C.Succ C.Zero)) {T.code = emit table code_local; labels; state} === T.Trap} @ ghost =
  fun table pc code_local labels state premise -> ghost_ (
    correct table pc code_local labels state (); selected_code table pc code_local labels state;
    Fuel.correct (cost table pc code_local) (C.Succ C.Zero) {T.code = emit table code_local; labels; state};
    let selected = selection table pc code_local labels state in
    T.run_def (C.Succ C.Zero) selected; trap_def (); T.step_def selected)
