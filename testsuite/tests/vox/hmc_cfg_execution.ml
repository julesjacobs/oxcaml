module D = Hm_declarative
module K = Hmc_closure_ir
module P = Hmc_closure_program
module R = Hmc_closure_semantics
module G = Hmc_cfg_ir
module C = Hmc_cfg_program
module O = Hmc_cfg_origin
module S = Hmc_cfg_semantics

let (jump @ total) : (program : C.program) @ immutable -> (a : S.activation) @ immutable ->
    (frames : S.frames) @ immutable -> (next : D.index) @ immutable -> 
    {u : unit | O.instruction program.C.blocks a.S.pc (G.Jump next)} ->
    {u : unit | S.step program (S.Running (a, frames)) === S.Running ({a with S.pc = next}, frames)} @ ghost =
  fun program a frames next premise -> ghost_ (
    O.instruction_def program.C.blocks a.S.pc (G.Jump next);
    S.step_def program (S.Running (a, frames)))

let (save_environment @ total) : (program : C.program) @ immutable -> (a : S.activation) @ immutable ->
    (frames : S.frames) @ immutable -> (next : D.index) @ immutable -> 
    {u : unit | O.instruction program.C.blocks a.S.pc (G.Save_environment next)} ->
    {u : unit | S.step program (S.Running (a, frames)) === S.Running ({a with S.pc = next; temporaries = S.Environment (a.S.env, a.S.temporaries)}, frames)} @ ghost =
  fun program a frames next premise -> ghost_ (
    O.instruction_def program.C.blocks a.S.pc (G.Save_environment next);
    S.step_def program (S.Running (a, frames)))

let (load @ total) : (program : C.program) @ immutable -> (a : S.activation) @ immutable ->
    (frames : S.frames) @ immutable -> (atom : G.atom) @ immutable -> (ty : D.mono) @ immutable -> (derivation : D.typing) @ immutable -> (next : D.index) @ immutable -> (value : R.V.value) @ immutable -> 
    {u : unit | O.instruction program.C.blocks a.S.pc (G.Load (atom, ty, derivation, next)) && S.load program.C.origin.P.globals a.S.env atom === Some value} ->
    {u : unit | S.step program (S.Running (a, frames)) === S.Running ({a with S.pc = next; accumulator = value}, frames)} @ ghost =
  fun program a frames atom ty derivation next value premise -> ghost_ (
    O.instruction_def program.C.blocks a.S.pc (G.Load (atom, ty, derivation, next));
    S.step_def program (S.Running (a, frames)))

let (save_value @ total) : (program : C.program) @ immutable -> (a : S.activation) @ immutable ->
    (frames : S.frames) @ immutable -> (next : D.index) @ immutable -> (env : R.V.value) @ immutable -> (rest : S.temporaries) @ immutable -> 
    {u : unit | O.instruction program.C.blocks a.S.pc (G.Save_value next) && a.S.temporaries === S.Environment (env, rest)} ->
    {u : unit | S.step program (S.Running (a, frames)) === S.Running ({a with S.pc = next; env; temporaries = S.Value (a.S.accumulator, env, rest)}, frames)} @ ghost =
  fun program a frames next env rest premise -> ghost_ (
    O.instruction_def program.C.blocks a.S.pc (G.Save_value next);
    S.step_def program (S.Running (a, frames)))

let (bind @ total) : (program : C.program) @ immutable -> (a : S.activation) @ immutable ->
    (frames : S.frames) @ immutable -> (next : D.index) @ immutable -> (env : R.V.value) @ immutable -> (rest : S.temporaries) @ immutable -> 
    {u : unit | O.instruction program.C.blocks a.S.pc (G.Bind next) && a.S.temporaries === S.Environment (env, rest)} ->
    {u : unit | S.step program (S.Running (a, frames)) === S.Running ({a with S.pc = next; env = R.V.Bind (a.S.accumulator, env)}, frames)} @ ghost =
  fun program a frames next env rest premise -> ghost_ (
    O.instruction_def program.C.blocks a.S.pc (G.Bind next);
    S.step_def program (S.Running (a, frames)))

let (restore @ total) : (program : C.program) @ immutable -> (a : S.activation) @ immutable ->
    (frames : S.frames) @ immutable -> (next : D.index) @ immutable -> (env : R.V.value) @ immutable -> (rest : S.temporaries) @ immutable -> 
    {u : unit | O.instruction program.C.blocks a.S.pc (G.Restore next) && a.S.temporaries === S.Environment (env, rest)} ->
    {u : unit | S.step program (S.Running (a, frames)) === S.Running ({a with S.pc = next; env; temporaries = rest}, frames)} @ ghost =
  fun program a frames next env rest premise -> ghost_ (
    O.instruction_def program.C.blocks a.S.pc (G.Restore next);
    S.step_def program (S.Running (a, frames)))

let (primitive @ total) : (program : C.program) @ immutable -> (a : S.activation) @ immutable ->
    (frames : S.frames) @ immutable -> (op : D.word_operation) @ immutable -> (next : D.index) @ immutable -> (left : Hmc_word64.t) @ immutable -> (right : Hmc_word64.t) @ immutable -> (env : R.V.value) @ immutable -> (rest : S.temporaries) @ immutable -> 
    {u : unit | O.instruction program.C.blocks a.S.pc (G.Primitive (op, next)) && a.S.temporaries === S.Value (R.V.Word left, env, rest) && a.S.accumulator === R.V.Word right} ->
    {u : unit | S.step program (S.Running (a, frames)) === S.Running ({a with S.pc = next; env; temporaries = rest; accumulator = R.primitive op left right}, frames)} @ ghost =
  fun program a frames op next left right env rest premise -> ghost_ (
    O.instruction_def program.C.blocks a.S.pc (G.Primitive (op, next));
    S.step_def program (S.Running (a, frames)))

let (cons @ total) : (program : C.program) @ immutable -> (a : S.activation) @ immutable ->
    (frames : S.frames) @ immutable -> (next : D.index) @ immutable -> (head : R.V.value) @ immutable -> (env : R.V.value) @ immutable -> (rest : S.temporaries) @ immutable -> 
    {u : unit | O.instruction program.C.blocks a.S.pc (G.Cons next) && a.S.temporaries === S.Value (head, env, rest)} ->
    {u : unit | S.step program (S.Running (a, frames)) === S.Running ({a with S.pc = next; env; temporaries = rest; accumulator = R.V.Cons (head, a.S.accumulator)}, frames)} @ ghost =
  fun program a frames next head env rest premise -> ghost_ (
    O.instruction_def program.C.blocks a.S.pc (G.Cons next);
    S.step_def program (S.Running (a, frames)))

let (branch_true @ total) : (program : C.program) @ immutable -> (a : S.activation) @ immutable ->
    (frames : S.frames) @ immutable -> (yes : D.index) @ immutable -> (no : D.index) @ immutable -> 
    {u : unit | O.instruction program.C.blocks a.S.pc (G.Branch (yes, no)) && a.S.accumulator === R.V.True} ->
    {u : unit | S.step program (S.Running (a, frames)) === S.Running ({a with S.pc = yes}, frames)} @ ghost =
  fun program a frames yes no premise -> ghost_ (
    O.instruction_def program.C.blocks a.S.pc (G.Branch (yes, no));
    S.step_def program (S.Running (a, frames)))

let (branch_false @ total) : (program : C.program) @ immutable -> (a : S.activation) @ immutable ->
    (frames : S.frames) @ immutable -> (yes : D.index) @ immutable -> (no : D.index) @ immutable -> 
    {u : unit | O.instruction program.C.blocks a.S.pc (G.Branch (yes, no)) && a.S.accumulator === R.V.False} ->
    {u : unit | S.step program (S.Running (a, frames)) === S.Running ({a with S.pc = no}, frames)} @ ghost =
  fun program a frames yes no premise -> ghost_ (
    O.instruction_def program.C.blocks a.S.pc (G.Branch (yes, no));
    S.step_def program (S.Running (a, frames)))

let (list_empty @ total) : (program : C.program) @ immutable -> (a : S.activation) @ immutable ->
    (frames : S.frames) @ immutable -> (empty : D.index) @ immutable -> (full : D.index) @ immutable -> 
    {u : unit | O.instruction program.C.blocks a.S.pc (G.List_branch (empty, full)) && a.S.accumulator === R.V.Nil} ->
    {u : unit | S.step program (S.Running (a, frames)) === S.Running ({a with S.pc = empty}, frames)} @ ghost =
  fun program a frames empty full premise -> ghost_ (
    O.instruction_def program.C.blocks a.S.pc (G.List_branch (empty, full));
    S.step_def program (S.Running (a, frames)))

let (list_full @ total) : (program : C.program) @ immutable -> (a : S.activation) @ immutable ->
    (frames : S.frames) @ immutable -> (empty : D.index) @ immutable -> (full : D.index) @ immutable -> (head : R.V.value) @ immutable -> (tail : R.V.value) @ immutable -> 
    {u : unit | O.instruction program.C.blocks a.S.pc (G.List_branch (empty, full)) && a.S.accumulator === R.V.Cons (head, tail)} ->
    {u : unit | S.step program (S.Running (a, frames)) === S.Running ({a with S.pc = full; env = R.V.Bind (head, R.V.Bind (tail, a.S.env)); temporaries = S.Environment (a.S.env, a.S.temporaries)}, frames)} @ ghost =
  fun program a frames empty full head tail premise -> ghost_ (
    O.instruction_def program.C.blocks a.S.pc (G.List_branch (empty, full));
    S.step_def program (S.Running (a, frames)))

let (return_halt @ total) : (program : C.program) @ immutable -> (a : S.activation) @ immutable ->
    (frames : S.frames) @ immutable -> 
    {u : unit | O.instruction program.C.blocks a.S.pc (G.Return) && a.S.temporaries === S.Empty && frames === S.Halt} ->
    {u : unit | S.step program (S.Running (a, frames)) === S.Done a.S.accumulator} @ ghost =
  fun program a frames  premise -> ghost_ (
    O.instruction_def program.C.blocks a.S.pc (G.Return);
    S.step_def program (S.Running (a, frames)))

let (return_frame @ total) : (program : C.program) @ immutable -> (a : S.activation) @ immutable ->
    (frames : S.frames) @ immutable -> (saved : S.activation) @ immutable -> (rest : S.frames) @ immutable -> 
    {u : unit | O.instruction program.C.blocks a.S.pc (G.Return) && a.S.temporaries === S.Empty && frames === S.Frame (saved, rest)} ->
    {u : unit | S.step program (S.Running (a, frames)) === S.Running ({saved with S.accumulator = a.S.accumulator}, rest)} @ ghost =
  fun program a frames saved rest premise -> ghost_ (
    O.instruction_def program.C.blocks a.S.pc (G.Return);
    S.step_def program (S.Running (a, frames)))

let (call @ total) : (program : C.program) @ immutable -> (a : S.activation) @ immutable ->
    (frames : S.frames) @ immutable -> (next : D.index) @ immutable -> (id : D.index) @ immutable -> (captured : R.V.value) @ immutable -> (env : R.V.value) @ immutable -> (rest : S.temporaries) @ immutable -> (callee : K.entry) @ immutable -> (code : C.function_entry) @ immutable -> 
    {u : unit | O.instruction program.C.blocks a.S.pc (G.Call next) && a.S.temporaries === S.Value (R.V.Closure (id, captured), env, rest)
      && K.lookup program.C.origin.P.table id === Some callee && C.lookup program.C.functions id === Some code} ->
    {u : unit | S.step program (S.Running (a, frames)) === S.Running ({S.pc = code.C.start;
      env = R.V.Bind (a.S.accumulator, (if callee.K.recursive then R.V.Bind (R.V.Closure (id, captured), captured) else captured));
      accumulator = R.V.Nil; temporaries = S.Empty; current = R.V.Closure (id, captured)},
      S.Frame ({a with S.pc = next; env; temporaries = rest}, frames))} @ ghost =
  fun program a frames next id captured env rest callee code premise -> ghost_ (
    O.instruction_def program.C.blocks a.S.pc (G.Call next);
    S.step_def program (S.Running (a, frames)))

let (leaf_agreement @ total) : (program : C.program) @ immutable -> (env : R.V.value) @ immutable ->
    (atom : G.atom) @ immutable -> (k : R.continuation) @ immutable ->
    {u : unit | R.step program.C.origin.P.table program.C.origin.P.globals
      (R.Running (R.Evaluate (env, G.term atom), k)) ===
      (match S.load program.C.origin.P.globals env atom with
        | None -> R.Stuck | Some value -> R.Running (R.Return value, k))} @ ghost =
  fun program env atom k -> ghost_ (
    G.term_def atom; S.load_def program.C.origin.P.globals env atom;
    R.step_def program.C.origin.P.table program.C.origin.P.globals
      (R.Running (R.Evaluate (env, G.term atom), k)))

let (leaf @ total) : (program : C.program) @ immutable -> (a : S.activation) @ immutable ->
    (frames : S.frames) @ immutable -> (source : K.term) @ immutable -> (next : D.index) @ immutable ->
    (atom : G.atom) @ immutable -> (ty : D.mono) @ immutable -> (derivation : D.typing) @ immutable ->
    (k : R.continuation) @ immutable -> (value : R.V.value) @ immutable ->
    {u : unit | O.generated program.C.blocks source next (O.Leaf (a.S.pc, atom, ty, derivation))
      && S.load program.C.origin.P.globals a.S.env atom === Some value} ->
    {u : unit | S.step program (S.Running (a, frames)) === S.Running ({a with S.pc = next; accumulator = value}, frames)
      && R.step program.C.origin.P.table program.C.origin.P.globals (R.Running (R.Evaluate (a.S.env, source), k))
        === R.Running (R.Return value, k)} @ ghost =
  fun program a frames source next atom ty derivation k value premise -> ghost_ (
    O.generated_def program.C.blocks source next (O.Leaf (a.S.pc, atom, ty, derivation));
    load program a frames atom ty derivation next value ();
    leaf_agreement program a.S.env atom k)

let rec (advance_add @ total) : (program : C.program) @ immutable -> (first : D.index) @ immutable ->
    (second : D.index) @ immutable -> (state : S.state) @ immutable ->
    {u : unit | S.advance program (D.add first second) state
      === S.advance program second (S.advance program first state)} @ ghost =
  fun program first second state -> ghost_ (
    D.add_def first second; S.advance_def program first state;
    S.advance_def program (D.add first second) state;
    match first with D.Z -> () | D.S n -> advance_add program n second (S.step program state))

let rec (done_stable @ total) : (program : C.program) @ immutable -> (fuel : D.index) @ immutable ->
    (value : R.V.value) @ immutable ->
    {u : unit | S.advance program fuel (S.Done value) === S.Done value} @ ghost =
  fun program fuel value -> ghost_ (
    S.advance_def program fuel (S.Done value); S.step_def program (S.Done value);
    match fuel with D.Z -> () | D.S n -> done_stable program n value)

let (normal_return_extension @ total) : (program : C.program) @ immutable -> (state : S.state) @ immutable ->
    (first : D.index) @ immutable -> (extra : D.index) @ immutable -> (value : R.V.value) @ immutable ->
    {u : unit | S.advance program first state === S.Done value} ->
    {u : unit | S.advance program (D.add first extra) state === S.Done value} @ ghost =
  fun program state first extra value premise -> ghost_ (
    advance_add program first extra state; done_stable program extra value)
