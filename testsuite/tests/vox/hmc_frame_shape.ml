module D = Hm_declarative
module K = Hmc_closure_ir
module R = Hmc_closure_semantics
module G = Hmc_cfg_ir
module S = Hmc_cfg_semantics

let[@def] rec (environment @ total) (context : D.context @ immutable) (env : R.V.value @ immutable) = match context, env with
  | D.Empty_context, R.V.Empty -> true
  | D.Binding (_, rest), R.V.Bind (_, tail) -> environment rest tail
  | _ -> false
let[@def] (first_class @ total) (v : R.V.value @ immutable) = match v with R.V.Empty | R.V.Bind _ -> false | _ -> true
let[@def] rec (valid @ total) (table : K.table @ immutable) (v : R.V.value @ immutable) = ghost_ (match v with
  | R.V.True | R.V.False | R.V.Word _ | R.V.Nil | R.V.Empty -> true
  | R.V.Cons (a, b) -> valid table a && first_class a && valid table b && first_class b
  | R.V.Bind (a, b) -> valid table a && first_class a && valid table b
  | R.V.Closure (id, env) -> valid table env && match K.lookup table id with
    | None -> false | Some entry -> environment entry.K.captured env)
let[@def] (value @ total) (table : K.table @ immutable) (v : R.V.value @ immutable) = ghost_ (valid table v && first_class v)
let[@def] (locals @ total) (table : K.table @ immutable) (context : D.context @ immutable) (env : R.V.value @ immutable) =
  ghost_ (valid table env && environment context env)
let[@def] rec (temporaries @ total) (table : K.table @ immutable) (schema : G.temporaries @ immutable)
    (runtime : S.temporaries @ immutable) = ghost_ (match schema, runtime with
  | G.Empty_temporaries, S.Empty -> true
  | G.Environment (g, rest), S.Environment (env, tail) -> locals table g env && temporaries table rest tail
  | G.Value (g, _, rest), S.Value (v, env, tail) -> value table v && locals table g env && temporaries table rest tail
  | _ -> false)
let[@def] (activation @ total) (table : K.table @ immutable) (signature : G.signature @ immutable) (a : S.activation @ immutable) = ghost_ (
  locals table signature.G.locals a.S.env && temporaries table signature.G.temporaries a.S.temporaries
  && value table a.S.accumulator && value table a.S.current)
let[@def] (at_label @ total) (table : K.table @ immutable) (blocks : G.table @ immutable) (a : S.activation @ immutable) = ghost_ (
  match G.lookup blocks a.S.pc with None -> false | Some block -> activation table block.G.signature a)
let[@def] rec (frames @ total) (table : K.table @ immutable) (blocks : G.table @ immutable) (stack : S.frames @ immutable) = ghost_ (
  match stack with S.Halt -> true | S.Frame (a, rest) -> at_label table blocks a && frames table blocks rest)
let[@def] (state @ total) (table : K.table @ immutable) (blocks : G.table @ immutable) (s : S.state @ immutable) = ghost_ (
  match s with
  | S.Stuck -> true | S.Done v -> value table v
  | S.Running (a, stack) -> at_label table blocks a && frames table blocks stack)
