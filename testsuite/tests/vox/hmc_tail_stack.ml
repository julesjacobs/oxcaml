module D = Hm_declarative
module G = Hmc_cfg_ir
module C = Hmc_cfg_program
module S = Hmc_cfg_semantics
module I = Hmc_tail_ir
module U = Hmc_tail_semantics

let[@def] (ordinary_call @ total) (op : I.instruction @ immutable) = match op with I.Keep (G.Call _) -> true | _ -> false
let[@def] rec (no_calls @ total) (code : I.table @ immutable) = match code with
  | I.Empty -> true | I.Add (op, rest) -> not (ordinary_call op) && no_calls rest
let rec (lookup_no_calls @ total) : (code : I.table) @ immutable -> (label : D.index) @ immutable ->
    {u : unit | no_calls code} ->
    {u : unit | match I.lookup code label with None -> true | Some op -> not (ordinary_call op)} @ ghost =
  fun code label premise -> ghost_ (
    no_calls_def code; I.lookup_def code label;
    match code with I.Empty -> () | I.Add (_, rest) ->
      if Hm_elaboration_check.index_equal label (I.size rest) then () else lookup_no_calls rest label ())
let[@def] rec (bounded @ total) (bound : D.index @ immutable) (frames : S.frames @ immutable) = match frames with
  | S.Halt -> true | S.Frame (_, rest) -> match bound with D.Z -> false | D.S n -> bounded n rest
let rec (weaken @ total) : (bound : D.index) @ immutable -> (frames : S.frames) @ immutable ->
    {u : unit | bounded bound frames} -> {u : unit | bounded (D.S bound) frames} @ ghost =
  fun bound frames premise -> ghost_ (
    bounded_def bound frames; bounded_def (D.S bound) frames;
    match frames, bound with S.Frame (_, rest), D.S n -> weaken n rest () | _ -> ())
let (pop @ total) : (bound : D.index) @ immutable -> (saved : S.activation) @ immutable -> (rest : S.frames) @ immutable ->
    {u : unit | bounded bound (S.Frame (saved, rest))} -> {u : unit | bounded bound rest} @ ghost =
  fun bound saved rest premise -> ghost_ (
    bounded_def bound (S.Frame (saved, rest)); match bound with D.Z -> () | D.S n -> weaken n rest ())
let[@def] (stack_bound @ total) (bound : D.index @ immutable) (state : S.state @ immutable) = match state with
  | S.Running (_, frames) -> bounded bound frames | _ -> true
let (step @ total) : (p : I.program) @ immutable -> (bound : D.index) @ immutable -> (state : S.state) @ immutable ->
    {u : unit | no_calls p.I.code && stack_bound bound state} ->
    {u : unit | stack_bound bound (U.step p state)} @ ghost = fun p bound state premise -> ghost_ (
  stack_bound_def bound state; U.step_def p state;
  (match state with
  | S.Running (a, frames) ->
    lookup_no_calls p.I.code a.S.pc ();
    (match I.lookup p.I.code a.S.pc with None -> () | Some op -> ordinary_call_def op);
    (match frames with S.Frame (saved, rest) -> pop bound saved rest () | S.Halt -> ())
  | _ -> ());
  stack_bound_def bound (U.step p state))
let rec (prefix @ total) : (p : I.program) @ immutable -> (bound : D.index) @ immutable ->
    (fuel : D.index) @ immutable -> (state : S.state) @ immutable ->
    {u : unit | no_calls p.I.code && stack_bound bound state} ->
    {u : unit | stack_bound bound (U.advance p fuel state)} @ ghost = fun p bound fuel state premise -> ghost_ (
  U.advance_def p fuel state;
  match fuel with D.Z -> () | D.S n -> step p bound state (); prefix p bound n (U.step p state) ())
let (initial @ total) : (p : I.program) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    {u : unit | stack_bound D.Z (U.initial p input)} @ ghost = fun p input -> ghost_ (
  U.initial_def p input; S.initial_def p.I.origin input;
  stack_bound_def D.Z (U.initial p input); bounded_def D.Z S.Halt)
let (constant_stack @ total) : (p : I.program) @ immutable -> (input : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | no_calls p.I.code} ->
    {u : unit | stack_bound D.Z (U.advance p fuel (U.initial p input))} @ ghost = fun p input fuel premise -> ghost_ (
  initial p input; prefix p D.Z fuel (U.initial p input) ())
