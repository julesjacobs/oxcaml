module D := Hm_declarative
module W := Hmc_word64
module B := Wasm_u32
module C := Wasm_code
module M := Hmc_compilation_model

type artifact : immutable_data
type error = Unbound_variable | Type_error | Entry_type_mismatch
  | Unsupported_polymorphic_local_let | Non_callable_outer_binding
  | Non_callable_entry | Invalid_annotation | Layout_rejected
  | Initialization_exhausted | Encoding_rejected
type result = Rejected of error | Compiled of artifact

val bytes : artifact @ immutable -> B.bytes @ immutable @@ total
val source : artifact @ immutable -> D.term @ immutable ghost @@ total
val input : artifact @ immutable -> W.t @ immutable ghost @@ total
val layout : artifact @ immutable -> M.layout @ immutable ghost @@ total

val compile : (term : D.term) @ immutable -> (configuration : M.layout) @ immutable ->
  (argument : W.t) @ immutable -> (memory : B.bytes) @ immutable -> (pages : B.u32) ->
  {u : unit | M.valid_layout configuration memory} ->
  {out : result | match out with Rejected _ -> true | Compiled artifact ->
    source artifact === term && input artifact === argument && layout artifact === configuration} @ immutable

val safe : (artifact : artifact) @ immutable -> (prefix : C.count) @ immutable ->
  {u : unit | match Wasm_binary_execution.run prefix (bytes artifact)
      (C.Succ (layout artifact).M.host_capacity) with
    Wasm_binary_execution.Result (Wasm_calls.Running _)
    | Wasm_binary_execution.Result (Wasm_calls.Finished _) -> true | _ -> false} @ ghost @@ total

val static_validity : (artifact : artifact) @ immutable ->
  {u : unit | Wasm_static_module.bytes_valid (bytes artifact)} @ ghost @@ total

val reflection : (artifact : artifact) @ immutable -> (prefix : C.count) @ immutable ->
  (after : Wasm_global_execution.state) @ immutable -> (word : W.t) @ immutable ->
  {u : unit | Wasm_binary_execution.run prefix (bytes artifact)
      (C.Succ (layout artifact).M.host_capacity) === Wasm_binary_execution.Result (Wasm_calls.Finished after)
    && M.returned after word} ->
  {fuel : D.index | M.source_returns (source artifact) (input artifact) fuel word} @ immutable ghost @@ total

val preservation : (artifact : artifact) @ immutable -> (word : W.t) @ immutable ->
  (source_fuel : D.index) @ immutable ->
  {u : unit | M.source_returns (source artifact) (input artifact) source_fuel word} ->
  {out : M.execution | Wasm_binary_execution.run out.M.fuel (bytes artifact)
      (C.Succ (layout artifact).M.host_capacity) === Wasm_binary_execution.Result (Wasm_calls.Finished out.M.after)
    && (M.returned out.M.after word || M.exhausted out.M.after)} @ immutable ghost @@ total

val normal : (artifact : artifact) @ immutable -> (word : W.t) @ immutable ->
  (source_fuel : D.index) @ immutable ->
  {u : unit | M.source_returns (source artifact) (input artifact) source_fuel word
    && M.sufficient (layout artifact) (bytes artifact) source_fuel} ->
  {out : M.execution | Wasm_binary_execution.run out.M.fuel (bytes artifact)
      (C.Succ (layout artifact).M.host_capacity) === Wasm_binary_execution.Result (Wasm_calls.Finished out.M.after)
    && M.returned out.M.after word} @ immutable ghost @@ total

val exhaustion : (artifact : artifact) @ immutable -> (prefix : C.count) @ immutable ->
  (after : Wasm_global_execution.state) @ immutable ->
  {u : unit | Wasm_binary_execution.run prefix (bytes artifact)
      (C.Succ (layout artifact).M.host_capacity) === Wasm_binary_execution.Result (Wasm_calls.Finished after)
    && M.exhausted after} ->
  {witness : M.exhaustion | M.honest_exhaustion (bytes artifact)
    (C.Succ (layout artifact).M.host_capacity) after witness} @ immutable ghost @@ total
