module D = Hm_declarative
module K = Hmc_closure_ir
module P = Hmc_closure_program
module R = Hmc_closure_semantics
module C = Hmc_cfg_program
module G = Hmc_cfg_ir
module O = Hmc_cfg_origin
module S = Hmc_cfg_semantics
module E = Hmc_cfg_execution

let[@def] (compound @ total) (term : K.term @ immutable) = match term with
  | K.Apply _ | K.Cons _ | K.Primitive _ | K.Let _ | K.If _ | K.CaseList _ -> true
  | _ -> false

type result = {term : K.term; trace : O.t; next : D.index;
  continuation : R.continuation; activation : S.activation}
let (descend @ total) : (program : C.program) @ immutable -> (a : S.activation) @ immutable ->
    (frames : S.frames) @ immutable -> (source : K.term) @ immutable -> (trace : O.t) @ immutable ->
    (next : D.index) @ immutable -> (k : R.continuation) @ immutable ->
    {u : unit | compound source && O.generated program.C.blocks source next trace && a.S.pc === O.entry trace} ->
    {r : result | O.generated program.C.blocks r.term r.next r.trace
      && r.activation.S.pc === O.entry r.trace && r.activation.S.env === a.S.env
      && S.step program (S.Running (a, frames)) === S.Running (r.activation, frames)
      && R.step program.C.origin.P.table program.C.origin.P.globals (R.Running (R.Evaluate (a.S.env, source), k))
        === R.Running (R.Evaluate (r.activation.S.env, r.term), r.continuation)} @ immutable =
  fun program a frames source trace next k premise ->
    ghost_ (compound_def source; O.generated_def program.C.blocks source next trace; O.entry_def trace;
      R.step_def program.C.origin.P.table program.C.origin.P.globals (R.Running (R.Evaluate (a.S.env, source), k)));
    match source, trace with
    | _, O.Leaf (_, atom, _, _) -> ghost_ (G.term_def atom); unreachable_ ()
    | K.Apply (left, right), O.Binary (_, save, _, l, _) ->
      ghost_ (E.save_environment program a frames (O.entry l) ());
      {term = left; trace = l; next = save; continuation = R.Apply_function (a.S.env, right, k);
        activation = {a with S.pc = O.entry l; temporaries = S.Environment (a.S.env, a.S.temporaries)}}
    | K.Cons (left, right), O.Binary (_, save, _, l, _) ->
      ghost_ (E.save_environment program a frames (O.entry l) ());
      {term = left; trace = l; next = save; continuation = R.Cons_head (a.S.env, right, k);
        activation = {a with S.pc = O.entry l; temporaries = S.Environment (a.S.env, a.S.temporaries)}}
    | K.Primitive (op, left, right), O.Binary (_, save, _, l, _) ->
      ghost_ (E.save_environment program a frames (O.entry l) ());
      {term = left; trace = l; next = save; continuation = R.Primitive_left (op, a.S.env, right, k);
        activation = {a with S.pc = O.entry l; temporaries = S.Environment (a.S.env, a.S.temporaries)}}
    | K.Let (left, right), O.Binding (_, bind, _, l, _) ->
      ghost_ (E.save_environment program a frames (O.entry l) ());
      {term = left; trace = l; next = bind; continuation = R.Let_body (a.S.env, right, k);
        activation = {a with S.pc = O.entry l; temporaries = S.Environment (a.S.env, a.S.temporaries)}}
    | K.If (condition, yes, no), O.Conditional (_, branch, c, _, _) ->
      ghost_ (E.jump program a frames (O.entry c) ());
      {term = condition; trace = c; next = branch; continuation = R.Conditional (a.S.env, yes, no, k);
        activation = {a with S.pc = O.entry c}}
    | K.CaseList (scrutinee, empty, full), O.Matching (_, branch, _, s, _, _) ->
      ghost_ (E.jump program a frames (O.entry s) ());
      {term = scrutinee; trace = s; next = branch; continuation = R.List_cases (a.S.env, empty, full, k);
        activation = {a with S.pc = O.entry s}}
    | _ -> unreachable_ ()
