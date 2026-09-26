module B = Wasm_u32
module W = Hmc_word64
module I = Wasm_instruction
module C = Wasm_code
module E = Wasm_execution
module V = Wasm_static_types
module Step = Wasm_static_steps
module Q = Wasm_static_sequence
module Memory = Wasm_static_memory
module PC = Hmc_wasm_pc_update
module Literal = Hmc_wasm_literal_load
module Local = Hmc_wasm_local_load
module Branch = Hmc_wasm_branch
module Simple = Hmc_wasm_simple_lower
module Relayout = Hmc_wasm_relayout
let (pc @ total) : (context : V.context) @ immutable -> (next : W.limb) -> (base : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals base === Some V.I32} ->
    {u : unit | Q.check context (PC.emit next base) state === Some state} @ ghost =
  fun context next base state premise -> ghost_ (
    PC.emit_def next base; Memory.immediate context (PC.offset ()) base (Hmc_wasm_header_update.number next) state ())
let (literal @ total) : (context : V.context) @ immutable -> (next : W.limb) -> (value : Hmc_tagged_cell.value) @ immutable ->
    (base : B.u32) -> (state : V.state) @ immutable -> {u : unit | V.local context.V.locals base === Some V.I32} ->
    {u : unit | Q.check context (Literal.emit next value base) state === Some state} @ ghost =
  fun context next value base state premise -> ghost_ (
    Literal.emit_def next value base; Memory.literals context (Literal.writes next value) base state ())
let (local @ total) : (context : V.context) @ immutable -> (tag : B.u32) -> (payload : B.u32) -> (next : W.limb) ->
    (base : B.u32) -> (state : V.state) @ immutable -> {u : unit | V.local context.V.locals base === Some V.I32} ->
    {u : unit | Q.check context (Local.emit tag payload next base) state === Some state} @ ghost =
  fun context tag payload next base state premise -> ghost_ (
    Local.emit_def tag payload next base;
    let a = V.push V.I32 state in
    let b = V.push V.I32 a in
    let c = V.push V.I64 a in
    let d = V.push V.I32 c in
    let e = V.push V.I32 d in
    let f = V.push V.I64 d in
    let c8 = PC.emit next base in
    let c7 = C.Next (I.I64_store (3, 40), c8) in
    let c6 = C.Next (I.I64_store (3, 32), c7) in
    let c5 = C.Next (I.I64_load (3, tag), c6) in
    let c4 = C.Next (I.Local_get base, c5) in
    let c3 = C.Next (I.Local_get base, c4) in
    let c2 = C.Next (I.I64_load (3, payload), c3) in
    let c1 = C.Next (I.Local_get base, c2) in
    Step.local_get context base V.I32 state (); Step.local_get context base V.I32 a ();
    Step.load64 context 3 payload a;
    Step.local_get context base V.I32 c (); Step.local_get context base V.I32 d ();
    Step.load64 context 3 tag d; Step.store64 context 3 32 c; Step.store64 context 3 40 state;
    pc context next base state ();
    Q.check_def context c7 c; Q.check_def context c6 f; Q.check_def context c5 e;
    Q.check_def context c4 d; Q.check_def context c3 c; Q.check_def context c2 b;
    Q.check_def context c1 a; Q.check_def context (Local.emit tag payload next base) state)
let (branch @ total) : (context : V.context) @ immutable -> (yes : W.limb) -> (no : W.limb) ->
    (base : B.u32) -> (state : V.state) @ immutable -> {u : unit | V.local context.V.locals base === Some V.I32} ->
    {u : unit | Q.check context (Branch.emit yes no base) state === Some state} @ ghost =
  fun context yes no base state premise -> ghost_ (
    Branch.emit_def yes no base;
    let a = V.push V.I32 state in let b = V.push V.I64 a in let c = V.push V.I64 b in
    let d = V.push V.I32 c in let e = V.push V.I64 c in
    let c7 = C.Next (I.I64_store (3, 8), C.Empty) in
    let c6 = C.Next (I.Plain I.Select, c7) in
    let c5 = C.Next (I.Plain I.I32_wrap_i64, c6) in
    let c4 = C.Next (I.I64_load (3, 40), c5) in
    let c3 = C.Next (I.Local_get base, c4) in
    let c2 = C.Next (I.I64_const (Hmc_wasm_header_update.number no), c3) in
    let c1 = C.Next (I.I64_const (Hmc_wasm_header_update.number yes), c2) in
    Step.local_get context base V.I32 state ();
    Step.constant64 context (Hmc_wasm_header_update.number yes) a;
    Step.constant64 context (Hmc_wasm_header_update.number no) b;
    Step.local_get context base V.I32 c (); Step.load64 context 3 40 c;
    Step.wrap64 context c; Step.select context V.I64 a; Step.store64 context 3 8 state;
    Q.check_def context C.Empty state; Q.check_def context c7 b; Q.check_def context c6 d;
    Q.check_def context c5 e; Q.check_def context c4 d; Q.check_def context c3 c;
    Q.check_def context c2 b; Q.check_def context c1 a; Q.check_def context (Branch.emit yes no base) state)
let (simple @ total) : (context : V.context) @ immutable -> (fragment : Simple.fragment) @ immutable ->
    (base : B.u32) -> (state : V.state) @ immutable -> {u : unit | V.local context.V.locals base === Some V.I32} ->
    {u : unit | Q.check context (Simple.emit fragment base) state === Some state} @ ghost =
  fun context fragment base state premise -> ghost_ (
    Simple.emit_def fragment base;
    match fragment with
    | Simple.Local (slot, next) -> local context (Simple.slot_tag slot) (Simple.slot_payload slot) next base state ()
    | Simple.Literal (value, next) -> literal context next value base state ()
    | Simple.Jump next -> pc context next base state ()
    | Simple.Branch (yes, no) -> branch context yes no base state ())
let (relayout @ total) : (context : V.context) @ immutable -> (fragment : Relayout.fragment) @ immutable ->
    (base : B.u32) -> (state : V.state) @ immutable -> {u : unit | V.local context.V.locals base === Some V.I32} ->
    {u : unit | Q.check context (Relayout.emit fragment base) state === Some state} @ ghost =
  fun context fragment base state premise -> ghost_ (
    Relayout.emit_def fragment base; Wasm_static_copy.parallel context fragment.Relayout.copies base state ();
    pc context fragment.Relayout.pc base state ();
    Q.append context (Wasm_parallel_copy.emit fragment.Relayout.copies base) (PC.emit fragment.Relayout.pc base) state)
