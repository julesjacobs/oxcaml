module B = Wasm_u32
module Table = Hmc_runtime_descriptor_table
module Double = Wasm_local_double
module Advance = Hmc_wasm_allocation_advance
module Replace = Wasm_local_replace_compose
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
let[@def] (emit @ total) (base : B.u32) (source : B.u32) (destination : B.u32) =
  E.append (Double.emit source destination) (E.append (Double.emit destination destination) (E.append (Double.emit destination destination) (E.append (Double.emit destination destination) (E.append (Double.emit destination destination) (Advance.emit base destination)))))
let (correct @ total) : (base : B.u32) -> (code : Table.count) -> (source : B.u32) -> (destination : B.u32) ->
    (state : X.state) @ immutable ->
    {u : unit | base + 32 * code <= 4294967295 && L.get state.X.machine.E.locals source === Some (S.I32 code)
      && (match L.get state.X.machine.E.locals destination with Some (S.I32 _) -> true | _ -> false)} ->
    {out : S.stack | X.run (emit base source destination) state === X.Done {X.memory = state.X.memory; machine = {E.locals = out; stack = state.X.machine.E.stack}}
      && L.get out destination === Some (S.I32 (Table.address base code))
      && L.replaced state.X.machine.E.locals destination (S.I32 (Table.address base code)) out} @ immutable =
  fun base code source destination state premise ->
    let state0 = state in
    ghost_ (L.can_set_def state0.X.machine.E.locals destination (S.I32 (S.add32 (code) (code)));
      (match L.get state0.X.machine.E.locals destination with Some old -> S.same_type_def old (S.I32 (S.add32 (code) (code))) | _ -> ()));
    let locals1 = Double.correct source destination (code) state0 () in
    ghost_ (S.add32_def (code) (code));
    let state1 = {X.memory = state.X.memory; machine = {E.locals = locals1; stack = state.X.machine.E.stack}} in
    ghost_ (L.can_set_def state1.X.machine.E.locals destination (S.I32 (S.add32 (2 * code) (2 * code)));
      (match L.get state1.X.machine.E.locals destination with Some old -> S.same_type_def old (S.I32 (S.add32 (2 * code) (2 * code))) | _ -> ()));
    let locals2 = Double.correct destination destination (2 * code) state1 () in
    ghost_ (S.add32_def (2 * code) (2 * code));
    let state2 = {X.memory = state.X.memory; machine = {E.locals = locals2; stack = state.X.machine.E.stack}} in
    ghost_ (Replace.compose state.X.machine.E.locals locals1 locals2 destination (S.I32 (2 * code)) (S.I32 (4 * code)) ());
    ghost_ (L.can_set_def state2.X.machine.E.locals destination (S.I32 (S.add32 (4 * code) (4 * code)));
      (match L.get state2.X.machine.E.locals destination with Some old -> S.same_type_def old (S.I32 (S.add32 (4 * code) (4 * code))) | _ -> ()));
    let locals3 = Double.correct destination destination (4 * code) state2 () in
    ghost_ (S.add32_def (4 * code) (4 * code));
    let state3 = {X.memory = state.X.memory; machine = {E.locals = locals3; stack = state.X.machine.E.stack}} in
    ghost_ (Replace.compose state.X.machine.E.locals locals2 locals3 destination (S.I32 (4 * code)) (S.I32 (8 * code)) ());
    ghost_ (L.can_set_def state3.X.machine.E.locals destination (S.I32 (S.add32 (8 * code) (8 * code)));
      (match L.get state3.X.machine.E.locals destination with Some old -> S.same_type_def old (S.I32 (S.add32 (8 * code) (8 * code))) | _ -> ()));
    let locals4 = Double.correct destination destination (8 * code) state3 () in
    ghost_ (S.add32_def (8 * code) (8 * code));
    let state4 = {X.memory = state.X.memory; machine = {E.locals = locals4; stack = state.X.machine.E.stack}} in
    ghost_ (Replace.compose state.X.machine.E.locals locals3 locals4 destination (S.I32 (8 * code)) (S.I32 (16 * code)) ());
    ghost_ (L.can_set_def state4.X.machine.E.locals destination (S.I32 (S.add32 (16 * code) (16 * code)));
      (match L.get state4.X.machine.E.locals destination with Some old -> S.same_type_def old (S.I32 (S.add32 (16 * code) (16 * code))) | _ -> ()));
    let locals5 = Double.correct destination destination (16 * code) state4 () in
    ghost_ (S.add32_def (16 * code) (16 * code));
    let state5 = {X.memory = state.X.memory; machine = {E.locals = locals5; stack = state.X.machine.E.stack}} in
    ghost_ (Replace.compose state.X.machine.E.locals locals4 locals5 destination (S.I32 (16 * code)) (S.I32 (32 * code)) ());
    let out = Advance.correct base destination (32 * code) 4294967295 state5 () in
    ghost_ (S.add32_def (32 * code) base; Table.address_def base code; S.add32_def base (32 * code);
      Replace.compose state.X.machine.E.locals locals5 out destination (S.I32 (32 * code)) (S.I32 (Table.address base code)) ();
      X.append_correct (Double.emit destination destination) (Advance.emit base destination) state4;
      X.append_correct (Double.emit destination destination) (E.append (Double.emit destination destination) (Advance.emit base destination)) state3;
      X.append_correct (Double.emit destination destination) (E.append (Double.emit destination destination) (E.append (Double.emit destination destination) (Advance.emit base destination))) state2;
      X.append_correct (Double.emit destination destination) (E.append (Double.emit destination destination) (E.append (Double.emit destination destination) (E.append (Double.emit destination destination) (Advance.emit base destination)))) state1;
      X.append_correct (Double.emit source destination) (E.append (Double.emit destination destination) (E.append (Double.emit destination destination) (E.append (Double.emit destination destination) (E.append (Double.emit destination destination) (Advance.emit base destination))))) state0;
      emit_def base source destination);
    out
