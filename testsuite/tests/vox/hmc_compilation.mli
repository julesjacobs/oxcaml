module D := Hm_declarative
module W := Hmc_word64
module B := Wasm_u32
module C := Wasm_code
module M := Hmc_compilation_model

type artifact : logical_data
type rejection : logical_data
type error = Unbound_variable | Type_error | Entry_type_mismatch
  | Unsupported_polymorphic_local_let | Non_callable_outer_binding
  | Non_callable_entry | Invalid_annotation | Layout_rejected
  | Initialization_exhausted | Encoding_rejected
type result = Rejected of rejection | Compiled of artifact

val bytes : artifact @ immutable -> B.bytes @ immutable @@ total
val source : artifact @ immutable -> D.term @ immutable ghost @@ total
val layout : artifact @ immutable -> M.layout @ immutable ghost @@ total

val reason : rejection @ immutable -> error @@ total
val program : rejection @ immutable -> D.term @ immutable ghost @@ total

(* A rejection of [term] states what is wrong with it.
   [Unsupported_polymorphic_local_let] only shows that [term] has a [let]
   inside a binding or the entry: whether that [let] is polymorphic depends on
   the inferred derivation. [untypable] and [no_entry_type] below give the
   meaning of the two type errors. [Layout_rejected], [Initialization_exhausted]
   and [Encoding_rejected] depend on the compiled program as well as on the
   layout, memory and pages, and are not characterized. *)
val compile : (term : D.term) @ immutable -> (configuration : M.layout) @ immutable ->
  (memory : B.bytes) @ immutable -> (pages : B.u32) ->
  {u : unit | M.valid_layout configuration memory} ->
  {out : result | match out with
    | Rejected rejection -> program rejection === term && (match reason rejection with
      | Unbound_variable -> not (D.scoped_term D.Z term)
      | Non_callable_outer_binding -> not (M.outer_callable term)
      | Non_callable_entry -> not (M.entry_callable term)
      | Unsupported_polymorphic_local_let -> not (M.no_local_let term)
      | Invalid_annotation -> false
      | Type_error | Entry_type_mismatch | Layout_rejected | Initialization_exhausted
      | Encoding_rejected -> true)
    | Compiled artifact ->
      source artifact === term && layout artifact === configuration} @ immutable

(* [Type_error]: the program has no type. *)
val untypable : (rejection : rejection) @ immutable -> (ty : Copy_spec.ty) @ immutable ->
  (typing : D.typing) @ immutable ->
  {u : unit | reason rejection === Type_error
    && D.typed D.Z D.Empty_context (program rejection) (D.embed ty) typing} ->
  {u : unit | false} @ ghost @@ total

(* [Entry_type_mismatch]: the program has no type [Word64 -> Word64]. *)
val no_entry_type : (rejection : rejection) @ immutable -> (typing : D.typing) @ immutable ->
  {u : unit | reason rejection === Entry_type_mismatch
    && D.typed D.Z D.Empty_context (program rejection) (D.Function (D.Word64, D.Word64)) typing} ->
  {u : unit | false} @ ghost @@ total

(* The theorems below hold for every [input]. [Wasm_binary_execution.run]
   sets the module's exported global [payload] to [input] and calls its
   exported function [run]; the source program is [source artifact] applied
   to [input]. *)
val safe : (artifact : artifact) @ immutable -> (input : W.t) @ immutable -> (prefix : C.count) @ immutable ->
  {u : unit | match Wasm_binary_execution.run prefix (bytes artifact) input
      (C.Succ (layout artifact).M.host_capacity) with
    Wasm_binary_execution.Result (Wasm_calls.Running _)
    | Wasm_binary_execution.Result (Wasm_calls.Finished _) -> true | _ -> false} @ ghost @@ total

val static_validity : (artifact : artifact) @ immutable ->
  {u : unit | Wasm_static_module.bytes_valid (bytes artifact)} @ ghost @@ total

val reflection : (artifact : artifact) @ immutable -> (input : W.t) @ immutable -> (prefix : C.count) @ immutable ->
  (after : Wasm_global_execution.state) @ immutable -> (word : W.t) @ immutable ->
  {u : unit | Wasm_binary_execution.run prefix (bytes artifact) input
      (C.Succ (layout artifact).M.host_capacity) === Wasm_binary_execution.Result (Wasm_calls.Finished after)
    && M.returned after word} ->
  {fuel : D.index | M.source_returns (source artifact) input fuel word} @ immutable ghost @@ total

val preservation : (artifact : artifact) @ immutable -> (input : W.t) @ immutable -> (word : W.t) @ immutable ->
  (source_fuel : D.index) @ immutable ->
  {u : unit | M.source_returns (source artifact) input source_fuel word} ->
  {out : M.execution | Wasm_binary_execution.run out.M.fuel (bytes artifact) input
      (C.Succ (layout artifact).M.host_capacity) === Wasm_binary_execution.Result (Wasm_calls.Finished out.M.after)
    && (M.returned out.M.after word || M.exhausted out.M.after)} @ immutable ghost @@ total

val normal : (artifact : artifact) @ immutable -> (input : W.t) @ immutable -> (word : W.t) @ immutable ->
  (source_fuel : D.index) @ immutable ->
  {u : unit | M.source_returns (source artifact) input source_fuel word
    && M.sufficient (layout artifact) (bytes artifact) source_fuel} ->
  {out : M.execution | Wasm_binary_execution.run out.M.fuel (bytes artifact) input
      (C.Succ (layout artifact).M.host_capacity) === Wasm_binary_execution.Result (Wasm_calls.Finished out.M.after)
    && M.returned out.M.after word} @ immutable ghost @@ total

val exhaustion : (artifact : artifact) @ immutable -> (input : W.t) @ immutable -> (prefix : C.count) @ immutable ->
  (after : Wasm_global_execution.state) @ immutable ->
  {u : unit | Wasm_binary_execution.run prefix (bytes artifact) input
      (C.Succ (layout artifact).M.host_capacity) === Wasm_binary_execution.Result (Wasm_calls.Finished after)
    && M.exhausted after} ->
  {witness : M.exhaustion | M.honest_exhaustion (bytes artifact) input
    (C.Succ (layout artifact).M.host_capacity) after witness} @ immutable ghost @@ total
