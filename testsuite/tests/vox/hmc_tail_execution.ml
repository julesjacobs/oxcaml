module D = Hm_declarative
module G = Hmc_cfg_ir
module C = Hmc_cfg_program
module O = Hmc_cfg_origin
module S = Hmc_cfg_semantics
module W = Hmc_cfg_states
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module T = Hmc_tail_sites
module K = Hmc_tail_continuation

let[@def] (noncall @ total) (op : G.instruction @ immutable) = match op with G.Call _ -> false | _ -> true
let (not_selected @ total) : (p : I.program) @ immutable -> (label : D.index) @ immutable -> (op : G.instruction) @ immutable ->
    {u : unit | O.instruction p.I.origin.C.blocks label op && noncall op} ->
    {u : unit | I.find p.I.sites label === None} @ ghost = fun p label op premise -> ghost_ (
  I.valid_def p; noncall_def op;
  match I.find p.I.sites label with None -> () | Some exit ->
    I.find_valid p.I.origin.C.blocks p.I.sites label exit ();
    K.unique p.I.origin.C.blocks label op (G.Call (T.entry exit)) ())
let[@def] (unselected @ total) (p : I.program @ immutable) (state : S.state @ immutable) = ghost_ (match state with
  | S.Running (a, _) -> I.find p.I.sites a.S.pc === None | _ -> true)
let (ordinary @ total) : (p : I.program) @ immutable -> (state : S.state) @ immutable ->
    {u : unit | unselected p state} ->
    {u : unit | U.step p state === S.step p.I.origin state} @ ghost = fun p state premise -> ghost_ (
  I.valid_def p; unselected_def p state; U.step_def p state; S.step_def p.I.origin state;
  match state with S.Running (a, _) ->
    I.lookup_related p.I.origin.C.blocks p.I.code p.I.sites a.S.pc ();
    (match G.lookup p.I.origin.C.blocks a.S.pc with None -> () | Some block -> I.select_def p.I.sites a.S.pc block.G.instruction)
  | _ -> ())
let (evaluate @ total) : (p : I.program) @ immutable -> (term : Hmc_closure_ir.term) @ immutable ->
    (trace : O.t) @ immutable -> (k : W.continuation) @ immutable -> (acc : Hmc_closure_semantics.V.value) @ immutable ->
    {u : unit | W.valid p.I.origin.C.blocks (W.Running (W.Evaluate (term, trace), k, acc))} ->
    {u : unit | unselected p (W.target (W.Running (W.Evaluate (term, trace), k, acc)))} @ ghost =
  fun p term trace k acc premise -> ghost_ (
    let control = W.Evaluate (term, trace) in
    let state = W.Running (control, k, acc) in
    W.valid_def p.I.origin.C.blocks state; W.target_def state; W.activation_def control k acc;
    unselected_def p (W.target state); O.entry_def trace;
    O.generated_def p.I.origin.C.blocks term (W.resume k) trace;
    match trace with
    | O.Leaf (id, atom, ty, d) ->
      noncall_def (G.Load (atom, ty, d, W.resume k)); not_selected p id (G.Load (atom, ty, d, W.resume k)) ()
    | O.Binary (id, _, _, left, _) | O.Binding (id, _, _, left, _) ->
      noncall_def (G.Save_environment (O.entry left)); not_selected p id (G.Save_environment (O.entry left)) ()
    | O.Conditional (id, _, condition, _, _) | O.Matching (id, _, _, condition, _, _) ->
      noncall_def (G.Jump (O.entry condition)); not_selected p id (G.Jump (O.entry condition)) ())
let[@def] (call @ total) (k : W.continuation @ immutable) = match k with W.Right (W.Apply, _, _, _) -> true | _ -> false
let (returning @ total) : (p : I.program) @ immutable -> (k : W.continuation) @ immutable ->
    (acc : Hmc_closure_semantics.V.value) @ immutable ->
    {u : unit | W.valid p.I.origin.C.blocks (W.Running (W.Returning, k, acc)) && not (call k)} ->
    {u : unit | unselected p (W.target (W.Running (W.Returning, k, acc)))} @ ghost = fun p k acc premise -> ghost_ (
  let state = W.Running (W.Returning, k, acc) in
  W.valid_def p.I.origin.C.blocks state; W.target_def state; W.activation_def W.Returning k acc;
  unselected_def p (W.target state); call_def k;
  K.instruction p.I.origin.C.blocks k (); K.opcode_def k;
  (match k with W.Right (op, _, _, rest) -> W.operation_def op (W.resume rest) | _ -> ());
  noncall_def (K.opcode k); not_selected p (W.resume k) (K.opcode k) ())
