module B = Wasm_u32
module C = Wasm_code
module I = Wasm_instruction
module T = Wasm_control
module P = Wasm_instance_control
module E = Wasm_execution
module X = Wasm_memory_execution
module S = Wasm_scalar
module F = Wasm_functions
module M = Wasm_calls
module Policy = Wasm_call_policy
let[@def] rec (table @ total) (functions : F.functions @ immutable) (elements : F.table @ immutable) =
  match elements with
  | F.No_elements -> true
  | F.Element (index, rest) ->
    (match index with None -> true | Some index ->
      match F.lookup functions index with None -> false | Some function_ -> Policy.code Policy.No_calls function_.F.code)
    && table functions rest
let rec (lookup @ total) : (functions : F.functions) @ immutable -> (elements : F.table) @ immutable -> (slot : B.u32) ->
    (index : B.u32) -> (function_ : F.function_) @ immutable ->
    {u : unit | table functions elements && F.element elements slot === Some index && F.lookup functions index === Some function_} ->
    {u : unit | Policy.code Policy.No_calls function_.F.code} @ ghost = fun functions elements slot index function_ premise -> ghost_ (
  table_def functions elements; F.element_def elements slot;
  match elements with F.No_elements -> () | F.Element (_, rest) -> if slot = 0 then () else lookup functions rest (slot - 1) index function_ ())
let[@def] (saved @ total) (caller : M.caller @ immutable) =
  Policy.code Policy.Indirect_calls caller.M.code && Policy.labels Policy.Indirect_calls caller.M.labels
let[@def] (bounded @ total) (state : M.configuration @ immutable) = ghost_ (
  match state.M.callers with
  | M.Root -> state.M.capacity === C.Succ C.Zero && Policy.configuration Policy.Indirect_calls state.M.current.P.body
  | M.Caller (caller, M.Root) -> state.M.capacity === C.Zero && saved caller && Policy.configuration Policy.No_calls state.M.current.P.body
  | _ -> false)
let (leave @ total) : (before : M.configuration) @ immutable -> (after : M.configuration) @ immutable ->
    {u : unit | bounded before && M.leave before === M.Running after} ->
    {u : unit | bounded after} @ ghost = fun before after premise -> ghost_ (
  bounded_def before; M.leave_def before; bounded_def after;
  match before.M.callers with
  | M.Caller (caller, M.Root) -> saved_def caller; Policy.configuration_def Policy.Indirect_calls after.M.current.P.body
  | _ -> ())
let (advance @ total) : (before : M.configuration) @ immutable -> (after : M.configuration) @ immutable ->
    {u : unit | bounded before && M.advance before === M.Running after} ->
    {u : unit | bounded after} @ ghost = fun before after premise -> ghost_ (
  M.advance_def before; bounded_def before; bounded_def after;
  match P.step before.M.current with
  | P.Running current ->
    (match before.M.callers with
    | M.Root -> Policy.instance_step Policy.Indirect_calls before.M.current current ()
    | M.Caller (_, M.Root) -> Policy.instance_step Policy.No_calls before.M.current current ()
    | _ -> ())
  | P.Finished _ -> leave before after ()
  | _ -> ())
let (branch @ total) : (depth : B.u32) -> (before : M.configuration) @ immutable -> (after : M.configuration) @ immutable ->
    {u : unit | bounded before && M.branch depth before === M.Running after} ->
    {u : unit | bounded after} @ ghost = fun depth before after premise -> ghost_ (
  M.branch_def depth before;
  match M.target depth before.M.current.P.body.T.labels with
  | M.Function_label -> leave before after ()
  | M.Invalid_label -> ()
  | M.Local_label ->
    let body = {before.M.current.P.body with T.code = T.Instruction (I.Br depth, T.Empty)} in
    let reconstructed = {before with M.current = {before.M.current with P.body = body}} in
    bounded_def before; bounded_def reconstructed;
    (match before.M.callers with
    | M.Root ->
      Policy.configuration_def Policy.Indirect_calls before.M.current.P.body;
      Policy.configuration_def Policy.Indirect_calls body;
      Policy.code_def Policy.Indirect_calls body.T.code; Policy.instruction_def Policy.Indirect_calls (I.Br depth);
      Policy.code_def Policy.Indirect_calls T.Empty
    | M.Caller (_, M.Root) ->
      Policy.configuration_def Policy.No_calls before.M.current.P.body;
      Policy.configuration_def Policy.No_calls body;
      Policy.code_def Policy.No_calls body.T.code; Policy.instruction_def Policy.No_calls (I.Br depth);
      Policy.code_def Policy.No_calls T.Empty
    | _ -> ());
    advance reconstructed after ())
let (enter @ total) : (function_ : F.function_) @ immutable -> (tail : T.code) @ immutable ->
    (before : M.configuration) @ immutable -> (after : M.configuration) @ immutable ->
    {u : unit | before.M.callers === M.Root && bounded before && Policy.code Policy.No_calls function_.F.code
      && Policy.code Policy.Indirect_calls tail && M.enter function_ tail before === M.Running after} ->
    {u : unit | bounded after} @ ghost = fun function_ tail before after premise -> ghost_ (
  bounded_def before; Policy.configuration_def Policy.Indirect_calls before.M.current.P.body;
  M.enter_def function_ tail before; bounded_def after;
  let caller = {M.code = tail; labels = before.M.current.P.body.T.labels;
    locals = before.M.current.P.body.T.state.X.machine.E.locals; stack = before.M.current.P.body.T.state.X.machine.E.stack;
    result = before.M.result} in
  saved_def caller; Policy.configuration_def Policy.No_calls after.M.current.P.body; Policy.labels_def Policy.No_calls T.No_labels)
let (step @ total) : (module_ : F.module_) @ immutable -> (before : M.configuration) @ immutable -> (after : M.configuration) @ immutable ->
    {u : unit | table module_.F.functions module_.F.table && bounded before && M.step module_ before === M.Running after} ->
    {u : unit | bounded after} @ ghost = fun module_ before after premise -> ghost_ (
  M.step_def module_ before; bounded_def before;
  (match before.M.callers with
  | M.Root -> Policy.configuration_def Policy.Indirect_calls before.M.current.P.body; Policy.code_def Policy.Indirect_calls before.M.current.P.body.T.code
  | M.Caller (_, M.Root) -> Policy.configuration_def Policy.No_calls before.M.current.P.body; Policy.code_def Policy.No_calls before.M.current.P.body.T.code
  | _ -> ());
  match before.M.current.P.body.T.code with
  | T.Instruction (I.Call index, _) ->
    Policy.instruction_def Policy.No_calls (I.Call index); Policy.instruction_def Policy.Indirect_calls (I.Call index)
  | T.Instruction (I.Call_indirect signature, tail) ->
    Policy.instruction_def Policy.No_calls (I.Call_indirect signature);
    (match before.M.current.P.body.T.state.X.machine.E.stack with
    | S.Push (S.I32 slot, rest) ->
      (match F.element module_.F.table slot with
      | Some index -> (match F.lookup module_.F.functions index with
        | Some function_ ->
          lookup module_.F.functions module_.F.table slot index function_ ();
          let current = M.with_stack before.M.current rest in
          let popped = {before with M.current = current} in
          M.with_stack_def before.M.current rest;
          bounded_def popped; Policy.configuration_def Policy.Indirect_calls popped.M.current.P.body;
          enter function_ tail popped after ()
        | None -> ())
      | None -> ())
    | _ -> ())
  | T.Instruction (I.Plain I.Return, _) -> leave before after ()
  | T.Instruction (I.Br depth, _) -> branch depth before after ()
  | T.Instruction (I.Br_if depth, _) ->
    (match before.M.current.P.body.T.state.X.machine.E.stack with
    | S.Push (S.I32 condition, rest) ->
      let popped = {before with M.current = M.with_stack before.M.current rest} in
      M.with_stack_def before.M.current rest; bounded_def popped;
      Policy.configuration_def Policy.No_calls popped.M.current.P.body;
      Policy.configuration_def Policy.Indirect_calls popped.M.current.P.body;
      if condition <> 0 then branch depth popped after () else (
        bounded_def after; Policy.configuration_def Policy.No_calls after.M.current.P.body;
        Policy.configuration_def Policy.Indirect_calls after.M.current.P.body)
    | _ -> ())
  | _ -> advance before after ())
let rec (run @ total) : (fuel : C.count) @ immutable -> (module_ : F.module_) @ immutable ->
    (before : M.configuration) @ immutable -> (after : M.configuration) @ immutable ->
    {u : unit | table module_.F.functions module_.F.table && bounded before && M.run fuel module_ before === M.Running after} ->
    {u : unit | bounded after} @ ghost = fun fuel module_ before after premise -> ghost_ (
  M.run_def fuel module_ before;
  match fuel with
  | C.Zero -> ()
  | C.Succ rest -> match M.step module_ before with
    | M.Running next -> step module_ before next (); run rest module_ next after ()
    | _ -> ())
let (advance_available @ total) : (before : M.configuration) @ immutable ->
    {u : unit | not (M.advance before === M.Host_limit)} @ ghost = fun before -> ghost_ (
  M.advance_def before; M.leave_def before)
let (branch_available @ total) : (depth : B.u32) -> (before : M.configuration) @ immutable ->
    {u : unit | not (M.branch depth before === M.Host_limit)} @ ghost = fun depth before -> ghost_ (
  M.branch_def depth before; M.leave_def before;
  let reconstructed = {before with M.current = {before.M.current with P.body =
    {before.M.current.P.body with T.code = T.Instruction (I.Br depth, T.Empty)}}} in
  advance_available reconstructed)
let (available @ total) : (module_ : F.module_) @ immutable -> (before : M.configuration) @ immutable ->
    {u : unit | bounded before} -> {u : unit | not (M.step module_ before === M.Host_limit)} @ ghost =
  fun module_ before premise -> ghost_ (
  bounded_def before; M.step_def module_ before;
  (match before.M.callers with
  | M.Root -> Policy.configuration_def Policy.Indirect_calls before.M.current.P.body; Policy.code_def Policy.Indirect_calls before.M.current.P.body.T.code
  | M.Caller (_, M.Root) -> Policy.configuration_def Policy.No_calls before.M.current.P.body; Policy.code_def Policy.No_calls before.M.current.P.body.T.code
  | _ -> ());
  match before.M.current.P.body.T.code with
  | T.Instruction (I.Call index, _) ->
    Policy.instruction_def Policy.No_calls (I.Call index); Policy.instruction_def Policy.Indirect_calls (I.Call index)
  | T.Instruction (I.Call_indirect signature, tail) ->
    Policy.instruction_def Policy.No_calls (I.Call_indirect signature);
    (match before.M.current.P.body.T.state.X.machine.E.stack with
    | S.Push (S.I32 slot, rest) ->
      (match F.element module_.F.table slot with
      | Some index -> (match F.lookup module_.F.functions index with
        | Some function_ ->
          M.with_stack_def before.M.current rest;
          M.enter_def function_ tail {before with M.current = M.with_stack before.M.current rest}
        | None -> ())
      | None -> ())
    | _ -> ())
  | T.Instruction (I.Plain I.Return, _) -> M.leave_def before
  | T.Instruction (I.Br depth, _) -> branch_available depth before
  | T.Instruction (I.Br_if depth, _) ->
    (match before.M.current.P.body.T.state.X.machine.E.stack with
    | S.Push (S.I32 _, rest) -> branch_available depth {before with M.current = M.with_stack before.M.current rest}
    | _ -> ())
  | _ -> advance_available before)
let rec (run_available @ total) : (fuel : C.count) @ immutable -> (module_ : F.module_) @ immutable ->
    (before : M.configuration) @ immutable ->
    {u : unit | table module_.F.functions module_.F.table && bounded before} ->
    {u : unit | not (M.run fuel module_ before === M.Host_limit)} @ ghost = fun fuel module_ before premise -> ghost_ (
  M.run_def fuel module_ before;
  match fuel with
  | C.Zero -> ()
  | C.Succ rest ->
    available module_ before ();
    match M.step module_ before with
    | M.Running next -> step module_ before next (); run_available rest module_ next ()
    | _ -> ())
