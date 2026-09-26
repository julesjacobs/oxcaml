module D = Hm_declarative
module K = Hmc_closure_ir
module P = Hmc_closure_program
module R = Hmc_closure_semantics
module M = Hmc_monomorphic
module G = Hmc_cfg_ir
module C = Hmc_cfg_program
module S = Hmc_cfg_semantics
module H = Hmc_frame_shape
module V = Hmc_frame_values
module E = Hmc_frame_edges

let (step @ total) : (p : C.program) @ immutable -> (state : S.state) @ immutable ->
    {u : unit | H.state p.C.origin.P.table p.C.blocks state} ->
    {u : unit | H.state p.C.origin.P.table p.C.blocks (S.step p state)} @ ghost = fun p state premise -> ghost_ (
  let table = p.C.origin.P.table in
  let blocks = p.C.blocks in
  H.state_def table blocks state; S.step_def p state;
  (match state with
  | S.Done _ | S.Stuck -> ()
  | S.Running (a, frames) ->
    H.at_label_def table blocks a;
    (match G.lookup blocks a.S.pc with None -> () | Some block ->
      let s = block.G.signature in
      C.valid_def p; H.activation_def table s a;
      Hmc_cfg_extension.lookup_valid (M.manifest p.C.origin.P.origin.M.definitions) table blocks a.S.pc block ();
      G.block_valid_def (M.manifest p.C.origin.P.origin.M.definitions) table blocks block;
      H.temporaries_def table s.G.temporaries a.S.temporaries;
      match block.G.instruction with
      | G.Load (atom, ty, d, next) ->
        (match S.load p.C.origin.P.globals a.S.env atom with None -> () | Some v ->
          V.load p s.G.locals a.S.env atom ty d v ();
          E.edge table blocks s.G.locals s.G.temporaries (Some ty) {a with S.pc = next; accumulator = v} ())
      | G.Jump next -> E.edge table blocks s.G.locals s.G.temporaries None {a with S.pc = next} ()
      | G.Save_environment next ->
        let schema = G.Environment (s.G.locals, s.G.temporaries) in
        let runtime = S.Environment (a.S.env, a.S.temporaries) in
        H.temporaries_def table schema runtime;
        E.edge table blocks s.G.locals schema None {a with S.pc = next; temporaries = runtime} ()
      | G.Save_value next ->
        (match s.G.accumulator, s.G.temporaries, a.S.temporaries with
        | Some ty, G.Environment (g, rest), S.Environment (env, tail) ->
          let schema = G.Value (g, ty, rest) in let runtime = S.Value (a.S.accumulator, env, tail) in
          H.temporaries_def table schema runtime;
          E.edge table blocks g schema None {a with S.pc = next; env; temporaries = runtime} ()
        | _ -> ())
      | G.Bind next ->
        (match s.G.accumulator, s.G.temporaries, a.S.temporaries with
        | Some ty, G.Environment (g, _), S.Environment (env, _) ->
          V.binding table g (D.Forall (D.Z, ty)) a.S.accumulator env ();
          E.edge table blocks (D.Binding (D.Forall (D.Z, ty), g)) s.G.temporaries None
            {a with S.pc = next; env = R.V.Bind (a.S.accumulator, env)} ()
        | _ -> ())
      | G.Restore next ->
        (match s.G.accumulator, s.G.temporaries, a.S.temporaries with
        | Some ty, G.Environment (g, rest), S.Environment (env, tail) ->
          E.edge table blocks g rest (Some ty) {a with S.pc = next; env; temporaries = tail} ()
        | _ -> ())
      | G.Primitive (op, next) ->
        (match s.G.temporaries, a.S.temporaries, a.S.accumulator with
        | G.Value (g, _, rest), S.Value (R.V.Word left, env, tail), R.V.Word right ->
          V.primitive table op left right;
          E.edge table blocks g rest (Some (D.operation_type op))
            {a with S.pc = next; env; temporaries = tail; accumulator = R.primitive op left right} ()
        | _ -> ())
      | G.Cons next ->
        (match s.G.temporaries, a.S.temporaries with
        | G.Value (g, _, rest), S.Value (head, env, tail) ->
          let value = R.V.Cons (head, a.S.accumulator) in
          H.value_def table head; H.value_def table a.S.accumulator;
          H.value_def table value; H.valid_def table value; H.first_class_def value;
          E.edge table blocks g rest s.G.accumulator {a with S.pc = next; env; temporaries = tail; accumulator = value} ()
        | _ -> ())
      | G.Call next ->
        (match s.G.temporaries, a.S.temporaries with
        | G.Value (g, D.Function (_, result), rest), S.Value (R.V.Closure (id, captured), env, tail) ->
          (match K.lookup table id with None -> () | Some callee ->
            let code = C.lookup_origin blocks table p.C.functions id callee () in
            let _entered = E.enter p id captured a.S.accumulator callee code () in
            let saved = {a with S.pc = next; env; temporaries = tail} in
            E.edge table blocks g rest (Some result) saved ();
            H.frames_def table blocks (S.Frame (saved, frames)))
        | _ -> ())
      | G.Branch (yes, no) ->
        (match a.S.accumulator with
        | R.V.True -> E.edge table blocks s.G.locals s.G.temporaries None {a with S.pc = yes} ()
        | R.V.False -> E.edge table blocks s.G.locals s.G.temporaries None {a with S.pc = no} ()
        | _ -> ())
      | G.List_branch (empty, full) ->
        (match s.G.accumulator, a.S.accumulator with
        | Some (D.List_type ty), R.V.Nil -> E.edge table blocks s.G.locals s.G.temporaries None {a with S.pc = empty} ()
        | Some (D.List_type ty), R.V.Cons (head, tail) ->
          H.value_def table a.S.accumulator; H.valid_def table a.S.accumulator;
          H.value_def table head; H.value_def table tail;
          V.binding table s.G.locals (D.Forall (D.Z, D.List_type ty)) tail a.S.env ();
          V.binding table (D.Binding (D.Forall (D.Z, D.List_type ty), s.G.locals)) (D.Forall (D.Z, ty))
            head (R.V.Bind (tail, a.S.env)) ();
          let schema = G.Environment (s.G.locals, s.G.temporaries) in
          let runtime = S.Environment (a.S.env, a.S.temporaries) in
          H.temporaries_def table schema runtime;
          E.edge table blocks (D.Binding (D.Forall (D.Z, ty), D.Binding (D.Forall (D.Z, D.List_type ty), s.G.locals))) schema None
            {a with S.pc = full; env = R.V.Bind (head, R.V.Bind (tail, a.S.env)); temporaries = runtime} ()
        | _ -> ())
      | G.Return ->
        (match a.S.temporaries, frames with
        | S.Empty, S.Frame (saved, rest) -> H.frames_def table blocks frames; E.accumulator table blocks saved a.S.accumulator ()
        | _ -> ())));
  H.state_def table blocks (S.step p state))
