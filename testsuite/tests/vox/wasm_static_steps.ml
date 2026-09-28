module B = Wasm_u32
module I = Wasm_instruction
module V = Wasm_static_types
let (take_push @ total) : (ty : V.value) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.take ty (V.push ty state) === Some state} @ ghost = fun ty state -> ghost_ (
  V.push_def ty state; V.take_def ty (V.push ty state); V.pop_def (V.push ty state); V.compatible_def ty ty)
let (unary_push @ total) : (input : V.value) @ immutable -> (output : V.value) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.unary input output (V.push input state) === Some (V.push output state)} @ ghost =
  fun input output state -> ghost_ (take_push input state; V.unary_def input output (V.push input state))
let (local_get @ total) : (context : V.context) @ immutable -> (local : B.u32) -> (ty : V.value) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals local === Some ty} ->
    {u : unit | V.instruction context (I.Local_get local) state === Some (V.push ty state)} @ ghost =
  fun context local ty state premise -> ghost_ (V.instruction_def context (I.Local_get local) state)
let (local_set @ total) : (context : V.context) @ immutable -> (local : B.u32) -> (ty : V.value) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals local === Some ty} ->
    {u : unit | V.instruction context (I.Local_set local) (V.push ty state) === Some state} @ ghost =
  fun context local ty state premise -> ghost_ (take_push ty state; V.instruction_def context (I.Local_set local) (V.push ty state))
let (constant64 @ total) : (context : V.context) @ immutable -> (word : Hmc_word64.t) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.instruction context (I.I64_const word) state === Some (V.push V.I64 state)} @ ghost =
  fun context word state -> ghost_ (V.instruction_def context (I.I64_const word) state)
let (load64 @ total) : (context : V.context) @ immutable -> (alignment : I.align64) -> (offset : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.instruction context (I.I64_load (alignment, offset)) (V.push V.I32 state) === Some (V.push V.I64 state)} @ ghost =
  fun context alignment offset state -> ghost_ (
    unary_push V.I32 V.I64 state; V.instruction_def context (I.I64_load (alignment, offset)) (V.push V.I32 state))
let (store64 @ total) : (context : V.context) @ immutable -> (alignment : I.align64) -> (offset : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.instruction context (I.I64_store (alignment, offset)) (V.push V.I64 (V.push V.I32 state)) === Some state} @ ghost =
  fun context alignment offset state -> ghost_ (
    take_push V.I64 (V.push V.I32 state); take_push V.I32 state;
    V.store_def V.I64 (V.push V.I64 (V.push V.I32 state));
    V.instruction_def context (I.I64_store (alignment, offset)) (V.push V.I64 (V.push V.I32 state)))
let (extend32 @ total) : (context : V.context) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.instruction context (I.Plain I.I64_extend_i32_u) (V.push V.I32 state) === Some (V.push V.I64 state)} @ ghost =
  fun context state -> ghost_ (
    unary_push V.I32 V.I64 state;
    V.plain_def I.I64_extend_i32_u (V.push V.I32 state);
    V.instruction_def context (I.Plain I.I64_extend_i32_u) (V.push V.I32 state))
let (global_get @ total) : (context : V.context) @ immutable -> (global : B.u32) -> (ty : V.value) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.global context.V.globals global === Some ty} ->
    {u : unit | V.instruction context (I.Global_get global) state === Some (V.push ty state)} @ ghost =
  fun context global ty state premise -> ghost_ (V.instruction_def context (I.Global_get global) state)
let (global_set @ total) : (context : V.context) @ immutable -> (global : B.u32) -> (ty : V.value) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.global context.V.globals global === Some ty && Wasm_globals.writable context.V.globals.Wasm_globals.permissions global} ->
    {u : unit | V.instruction context (I.Global_set global) (V.push ty state) === Some state} @ ghost =
  fun context global ty state premise -> ghost_ (take_push ty state; V.instruction_def context (I.Global_set global) (V.push ty state))
let (load32 @ total) : (context : V.context) @ immutable -> (alignment : I.align32) -> (offset : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.instruction context (I.I32_load (alignment, offset)) (V.push V.I32 state) === Some (V.push V.I32 state)} @ ghost =
  fun context alignment offset state -> ghost_ (
    unary_push V.I32 V.I32 state; V.instruction_def context (I.I32_load (alignment, offset)) (V.push V.I32 state))
let (wrap64 @ total) : (context : V.context) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.instruction context (I.Plain I.I32_wrap_i64) (V.push V.I64 state) === Some (V.push V.I32 state)} @ ghost =
  fun context state -> ghost_ (unary_push V.I64 V.I32 state;
    V.plain_def I.I32_wrap_i64 (V.push V.I64 state); V.instruction_def context (I.Plain I.I32_wrap_i64) (V.push V.I64 state))
let (select @ total) : (context : V.context) @ immutable -> (ty : V.value) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.instruction context (I.Plain I.Select) (V.push V.I32 (V.push ty (V.push ty state))) === Some (V.push ty state)} @ ghost =
  fun context ty state -> ghost_ (
    let first = V.push ty state in let second = V.push ty first in let condition = V.push V.I32 second in
    take_push V.I32 second; V.push_def ty state; V.push_def ty first;
    V.pop_def second; V.pop_def first; V.compatible_def ty ty;
    V.select_def condition; V.plain_def I.Select condition; V.instruction_def context (I.Plain I.Select) condition)
