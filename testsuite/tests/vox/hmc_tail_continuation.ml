module Height = Hmc_cfg_height
module D = Hm_declarative
module G = Hmc_cfg_ir
module O = Hmc_cfg_origin
module R = Hmc_closure_semantics
module S = Hmc_cfg_semantics
module W = Hmc_cfg_states
module T = Hmc_tail_sites

let[@def] (opcode @ total) (k : W.continuation @ immutable) = match k with
  | W.Halt _ | W.Call_return _ -> G.Return
  | W.Left (_, _, trace, _, _, _) -> G.Save_value (O.entry trace)
  | W.Right (op, _, _, rest) -> W.operation op (W.resume rest)
  | W.Let_body (_, trace, _, _, _) -> G.Bind (O.entry trace)
  | W.Scope (_, _, rest) -> G.Restore (W.resume rest)
  | W.Conditional (_, _, yes, no, _, _) -> G.Branch (O.entry yes, O.entry no)
  | W.List_cases (_, _, empty, full, _, _, _) -> G.List_branch (O.entry empty, O.entry full)
let (instruction @ total) : (blocks : G.table) @ immutable -> (k : W.continuation) @ immutable ->
    {u : unit | W.continuation_valid blocks k} ->
    {u : unit | O.instruction blocks (W.resume k) (opcode k)} @ ghost = fun blocks k premise -> ghost_ (
  W.continuation_valid_def blocks k; W.resume_def k; opcode_def k)
let (unique @ total) : (blocks : G.table) @ immutable -> (label : D.index) @ immutable ->
    (left : G.instruction) @ immutable -> (right : G.instruction) @ immutable ->
    {u : unit | O.instruction blocks label left && O.instruction blocks label right} ->
    {u : unit | left === right} @ ghost = fun blocks label left right premise -> ghost_ (
  O.instruction_def blocks label left; O.instruction_def blocks label right)
let[@def] rec (tail @ total) (k : W.continuation @ immutable) = match k with
  | W.Halt _ | W.Call_return _ -> true | W.Scope (_, _, rest) -> tail rest | _ -> false
let rec (tail_of_exit @ total) : (blocks : G.table) @ immutable -> (exit : T.exit) @ immutable ->
    (k : W.continuation) @ immutable ->
    {u : unit | T.exit_valid blocks exit && W.continuation_valid blocks k && W.resume k === T.entry exit} ->
    {u : unit | tail k} @ ghost = fun blocks exit k premise -> ghost_ (
  instruction blocks k (); T.exit_valid_def blocks exit; T.entry_def exit;
  opcode_def k; tail_def k; W.continuation_valid_def blocks k;
  (match k with W.Right (op, _, _, rest) -> W.operation_def op (W.resume rest) | _ -> ());
  match exit with
  | T.Return _ -> unique blocks (W.resume k) (opcode k) G.Return ()
  | T.Restore (_, next) ->
    unique blocks (W.resume k) (opcode k) (G.Restore (T.entry next)) ();
    (match k with W.Scope (_, _, rest) -> tail_of_exit blocks next rest () | _ -> ()))

let rec (retarget @ total) : (blocks : G.table) @ immutable -> (k : W.continuation) @ immutable ->
    (env : R.V.value) @ immutable -> (current : R.V.value) @ immutable -> (label : D.index) @ immutable ->
    {u : unit | tail k && W.continuation_valid blocks k && O.instruction blocks label G.Return} ->
    {out : W.continuation | Height.le (Height.continuation out) (Height.continuation k)
      && W.continuation_valid blocks out && W.resume out === label
      && W.environment out === env && W.current out === current && W.temporaries out === S.Empty
      && W.frames out === W.frames k && W.source_continuation out === W.source_continuation k} @ immutable =
  fun blocks k env current label premise ->
    ghost_ (Height.continuation_def k; tail_def k; W.continuation_valid_def blocks k; W.frames_def k; W.source_continuation_def k);
    match k with
    | W.Halt _ ->
      let out = W.Halt (env, current, label) in
      ghost_ (Height.continuation_def out; Height.reflexive (Height.continuation out); W.continuation_valid_def blocks out; W.resume_def out; W.environment_def out;
        W.current_def out; W.temporaries_def out; W.frames_def out; W.source_continuation_def out);
      out
    | W.Call_return (_, _, _, argument, rest) ->
      let out = W.Call_return (env, current, label, argument, rest) in
      ghost_ (Height.continuation_def out; Height.reflexive (Height.continuation out); W.continuation_valid_def blocks out; W.resume_def out; W.environment_def out;
        W.current_def out; W.temporaries_def out; W.frames_def out; W.source_continuation_def out);
      out
    | W.Scope (_, _, rest) ->
      let out = retarget blocks rest env current label () in
      ghost_ (Height.weaken (Height.continuation out) (Height.continuation rest) ()); out
    | _ -> unreachable_ ()
