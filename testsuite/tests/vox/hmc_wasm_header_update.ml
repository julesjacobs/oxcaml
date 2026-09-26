module W = Hmc_word64
module B = Wasm_u32
module Q = Wasm_word_sequence
module H = Hmc_wasm_header_words
module V = Hmc_tagged_cell
module F = Wasm_frame_literals
module Lit = Hmc_wasm_literal_load
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module U = Wasm_sequence_update
module Splice = Wasm_memory_splice
module P = Hmc_linear_preservation
module Bytes = Hmc_linear_bytes
let[@def] (number @ total) (pc : W.limb) : W.t @ immutable = {W.lo = pc; hi = 0}
let (correct @ total) : (old_pc : W.t) @ immutable -> (pc : W.limb) ->
    (current : V.value) @ immutable -> (old : V.value) @ immutable -> (value : V.value) @ immutable ->
    (state : X.state) @ immutable -> (after : B.bytes) @ immutable -> (tail : B.bytes) @ immutable -> (base_local : B.u32) -> (base : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable ->
    {u : unit | base <= 4294967248 && P.equal_prefix base state.X.memory after
      && Bytes.drop state.X.memory base === Some before_frame && Bytes.drop after base === Some after_frame
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Q.decode (H.layout old_pc (V.tag current) (V.payload current) (V.tag old) (V.payload old)) before_frame === Some tail
      && Q.decode (H.layout (number pc) (V.tag current) (V.payload current) (V.tag value) (V.payload value)) after_frame === Some tail} ->
    {u : unit | F.apply (Lit.writes pc value) state.X.memory base === Some after
      && X.run (Lit.emit pc value base_local) state === X.Done {X.memory = after; machine = state.X.machine}} @ ghost =
  fun old_pc pc current old value state after tail base_local base before_frame after_frame premise -> ghost_ (
    let ct = V.tag current in let cp = V.payload current in
    let ot = V.tag old in let op = V.payload old in
    let nt = V.tag value in let np = V.payload value in
    let next = number pc in
    number_def pc;
    let tagged_frame = Q.encode (H.layout old_pc ct cp nt op) tail in
    let valued_frame = Q.encode (H.layout old_pc ct cp nt np) tail in
    let tagged = Splice.replace state.X.memory base before_frame tagged_frame () in
    let valued = Splice.replace state.X.memory base before_frame valued_frame () in
    Splice.shared state.X.memory tagged valued base (); Splice.shared state.X.memory valued after base ();
    H.expose old_pc ct cp ot op; H.expose old_pc ct cp nt op;
    H.expose old_pc ct cp nt np; H.expose next ct cp nt np;
    H.four_def (); H.five_def (); H.one_def (); U.zero_def ();
    U.at (H.prefix4 old_pc ct cp) (Q.Word (op, Q.End)) state.X.memory tagged before_frame tagged_frame tail ot nt base 32 (base + 32) ();
    U.at (H.prefix5 old_pc ct cp nt) Q.End tagged valued tagged_frame valued_frame tail op np base 40 (base + 40) ();
    U.at (Q.Word (H.tag (), Q.End)) (H.rest ct cp nt np) valued after valued_frame after_frame tail old_pc next base 8 (base + 8) ();
    Lit.writes_def pc value;
    F.apply_def (Lit.writes pc value) state.X.memory base;
    F.apply_def (F.Write (40, np, F.Write (8, next, F.End))) tagged base;
    F.apply_def (F.Write (8, next, F.End)) valued base; F.apply_def F.End after base;
    Lit.correct pc value base_local state base after ())
