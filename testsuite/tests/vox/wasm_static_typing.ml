module I = Wasm_instruction
module F = Wasm_functions
module T = Wasm_control
module V = Wasm_static_types
module C = Wasm_static_control

type derivation = End | Step of V.state * derivation
  | Enter of V.state * derivation * derivation
  | Split of V.state * V.state * V.state * derivation * derivation * derivation [@@inductive]
let[@def] (transition @ total) (context : V.context @ immutable) (labels : C.labels @ immutable)
    (op : I.t @ immutable) (before : V.state @ immutable) (after : V.state @ immutable) = ghost_ (
  match op with
  | I.Br depth -> C.branch labels depth before && after === V.dead ()
  | I.Br_if depth -> (match V.take V.I32 before, C.label labels depth with
    | Some rest, Some result -> (match V.consume_result result rest with
      | Some popped -> after === V.produce_result result popped | None -> false)
    | _ -> false)
  | _ -> V.instruction context op before === Some after)
let[@def] rec (typed @ total) (context : V.context @ immutable) (labels : C.labels @ immutable)
    (code : T.code @ immutable) (before : V.state @ immutable) (after : V.state @ immutable)
    (proof : derivation @ immutable) = ghost_ (match code, proof with
  | T.Empty, End -> before === after
  | T.Instruction (op, tail), Step (middle, rest) ->
    transition context labels op before middle && typed context labels tail middle after rest
  | (T.Block (body, tail) | T.Loop (body, tail)), Enter (ended, inner, rest) ->
    typed context (C.Label labels) body (V.initial ()) ended inner && V.finish F.Void ended
    && typed context labels tail before after rest
  | T.If (yes, no, tail), Split (rest, yes_end, no_end, yes_proof, no_proof, continuation) ->
    V.take V.I32 before === Some rest
    && typed context (C.Label labels) yes (V.initial ()) yes_end yes_proof && V.finish F.Void yes_end
    && typed context (C.Label labels) no (V.initial ()) no_end no_proof && V.finish F.Void no_end
    && typed context labels tail rest after continuation
  | _ -> false)
let rec (sound @ total) : (context : V.context) @ immutable -> (labels : C.labels) @ immutable ->
    (code : T.code) @ immutable -> (before : V.state) @ immutable -> (after : V.state) @ immutable ->
    {u : unit | C.check context labels code before === Some after} ->
    {proof : derivation | typed context labels code before after proof} @ immutable ghost =
  fun context labels code before after premise -> ghost_ (
    C.check_def context labels code before;
    match code with
    | T.Empty -> typed_def context labels code before after End; End
    | T.Instruction (op, tail) ->
      let middle = match op with
        | I.Br _ -> V.dead ()
        | I.Br_if depth -> (match V.take V.I32 before, C.label labels depth with
          | Some rest, Some result -> (match V.consume_result result rest with
            | Some popped -> V.produce_result result popped | None -> unreachable_ ())
          | _ -> unreachable_ ())
        | _ -> (match V.instruction context op before with Some state -> state | None -> unreachable_ ()) in
      transition_def context labels op before middle;
      let rest = sound context labels tail middle after () in
      let proof = Step (middle, rest) in
      typed_def context labels code before after proof; proof
    | T.Block (body, tail) | T.Loop (body, tail) ->
      (match C.check context (C.Label labels) body (V.initial ()) with
      | None -> unreachable_ ()
      | Some ended ->
        let inner = sound context (C.Label labels) body (V.initial ()) ended () in
        let rest = sound context labels tail before after () in
        let proof = Enter (ended, inner, rest) in
        typed_def context labels code before after proof; proof)
    | T.If (yes, no, tail) ->
      (match V.take V.I32 before, C.check context (C.Label labels) yes (V.initial ()), C.check context (C.Label labels) no (V.initial ()) with
      | Some rest, Some yes_end, Some no_end ->
        let yes_proof = sound context (C.Label labels) yes (V.initial ()) yes_end () in
        let no_proof = sound context (C.Label labels) no (V.initial ()) no_end () in
        let continuation = sound context labels tail rest after () in
        let proof = Split (rest, yes_end, no_end, yes_proof, no_proof, continuation) in
        typed_def context labels code before after proof; proof
      | _ -> unreachable_ ()))
let rec (complete @ total) : (context : V.context) @ immutable -> (labels : C.labels) @ immutable ->
    (code : T.code) @ immutable -> (before : V.state) @ immutable -> (after : V.state) @ immutable ->
    (proof : derivation) @ immutable -> {u : unit | typed context labels code before after proof} ->
    {u : unit | C.check context labels code before === Some after} @ ghost =
  fun context labels code before after proof premise -> ghost_ (
    typed_def context labels code before after proof; C.check_def context labels code before;
    match code, proof with
    | T.Instruction (op, tail), Step (middle, rest) ->
      transition_def context labels op before middle;
      complete context labels tail middle after rest ()
    | (T.Block (body, tail) | T.Loop (body, tail)), Enter (ended, inner, rest) ->
      complete context (C.Label labels) body (V.initial ()) ended inner ();
      complete context labels tail before after rest ()
    | T.If (yes, no, tail), Split (rest, yes_end, no_end, yes_proof, no_proof, continuation) ->
      complete context (C.Label labels) yes (V.initial ()) yes_end yes_proof ();
      complete context (C.Label labels) no (V.initial ()) no_end no_proof ();
      complete context labels tail rest after continuation ()
    | _ -> ())
type function_derivation = {ended : V.state; proof : derivation}
let[@def] (function_typed @ total) (module_ : F.module_ @ immutable) (globals : Wasm_globals.t @ immutable)
    (function_ : F.function_ @ immutable) (witness : function_derivation @ immutable) = ghost_ (
  let context = {V.module_; globals; locals = function_.F.locals; result = function_.F.result} in
  typed context (C.Root function_.F.result) function_.F.code (V.initial ()) witness.ended witness.proof
  && V.finish function_.F.result witness.ended)
let (function_sound @ total) : (module_ : F.module_) @ immutable -> (globals : Wasm_globals.t) @ immutable ->
    (function_ : F.function_) @ immutable -> {u : unit | C.function_ module_ globals function_} ->
    {witness : function_derivation | function_typed module_ globals function_ witness} @ immutable ghost =
  fun module_ globals function_ premise -> ghost_ (
    C.function__def module_ globals function_;
    let context = {V.module_; globals; locals = function_.F.locals; result = function_.F.result} in
    match C.check context (C.Root function_.F.result) function_.F.code (V.initial ()) with
    | None -> unreachable_ ()
    | Some ended ->
      let proof = sound context (C.Root function_.F.result) function_.F.code (V.initial ()) ended () in
      let witness = {ended; proof} in function_typed_def module_ globals function_ witness; witness)
type module_derivation = No_functions | Function of function_derivation * module_derivation [@@inductive]
let[@def] rec (functions_typed @ total) (module_ : F.module_ @ immutable) (globals : Wasm_globals.t @ immutable)
    (entries : F.functions @ immutable) (proof : module_derivation @ immutable) = ghost_ (
  match entries, proof with
  | F.No_functions, No_functions -> true
  | F.Function (entry, rest), Function (witness, tail) ->
    function_typed module_ globals entry witness && functions_typed module_ globals rest tail
  | _ -> false)
let rec (functions_sound @ total) : (module_ : F.module_) @ immutable -> (globals : Wasm_globals.t) @ immutable ->
    (entries : F.functions) @ immutable -> {u : unit | C.functions module_ globals entries} ->
    {proof : module_derivation | functions_typed module_ globals entries proof} @ immutable ghost =
  fun module_ globals entries premise -> ghost_ (
    C.functions_def module_ globals entries;
    match entries with
    | F.No_functions -> functions_typed_def module_ globals entries No_functions; No_functions
    | F.Function (entry, rest) ->
      let witness = function_sound module_ globals entry () in
      let tail = functions_sound module_ globals rest () in
      let proof = Function (witness, tail) in functions_typed_def module_ globals entries proof; proof)
let (function_bodies_sound @ total) : (module_ : F.module_) @ immutable -> (globals : Wasm_globals.t) @ immutable ->
    {u : unit | C.function_bodies module_ globals} ->
    {proof : module_derivation | functions_typed module_ globals module_.F.functions proof} @ immutable ghost =
  fun module_ globals premise -> ghost_ (
    C.function_bodies_def module_ globals; functions_sound module_ globals module_.F.functions ())
