module D = Hm_declarative
module R = Hmc_closure_semantics
module G = Hmc_cfg_ir
module S = Hmc_cfg_semantics
module H = Hmc_frame_shape

type cells = Empty | Cell of R.V.value * cells [@@inductive]
let[@def] rec (length @ total) (cells : cells @ immutable) = match cells with Empty -> D.Z | Cell (_, rest) -> D.S (length rest)
let[@def] rec (locals_size @ total) (context : D.context @ immutable) = match context with
  | D.Empty_context -> D.Z | D.Binding (_, rest) -> D.S (locals_size rest)
let[@def] rec (temporaries_size @ total) (schema : G.temporaries @ immutable) = match schema with
  | G.Empty_temporaries -> D.Z
  | G.Environment (g, rest) -> D.add (locals_size g) (temporaries_size rest)
  | G.Value (g, _, rest) -> D.S (D.add (locals_size g) (temporaries_size rest))
let[@def] (size @ total) (signature : G.signature @ immutable) =
  D.S (D.S (D.add (locals_size signature.G.locals) (temporaries_size signature.G.temporaries)))
let rec (add_associative @ total) : (a : D.index) @ immutable -> (b : D.index) @ immutable -> (c : D.index) @ immutable ->
    {u : unit | D.add (D.add a b) c === D.add a (D.add b c)} @ ghost = fun a b c -> ghost_ (
  D.add_def a b; D.add_def (D.add a b) c; D.add_def a (D.add b c);
  match a with D.Z -> () | D.S n -> add_associative n b c)

let[@def] rec (temporaries_shape @ total) (schema : G.temporaries @ immutable) (runtime : S.temporaries @ immutable) =
  match schema, runtime with
  | G.Empty_temporaries, S.Empty -> true
  | G.Environment (g, rest), S.Environment (env, tail) -> H.environment g env && temporaries_shape rest tail
  | G.Value (g, _, rest), S.Value (_, env, tail) -> H.environment g env && temporaries_shape rest tail
  | _ -> false
let[@def] (shape @ total) (signature : G.signature @ immutable) (a : S.activation @ immutable) =
  H.environment signature.G.locals a.S.env && temporaries_shape signature.G.temporaries a.S.temporaries
let rec (temporaries_shaped @ total) : (table : Hmc_closure_ir.table) @ immutable -> (schema : G.temporaries) @ immutable ->
    (runtime : S.temporaries) @ immutable -> {u : unit | H.temporaries table schema runtime} ->
    {u : unit | temporaries_shape schema runtime} @ ghost = fun table schema runtime premise -> ghost_ (
  H.temporaries_def table schema runtime; temporaries_shape_def schema runtime;
  match schema, runtime with
  | G.Environment (g, rest), S.Environment (env, tail) | G.Value (g, _, rest), S.Value (_, env, tail) ->
    H.locals_def table g env; temporaries_shaped table rest tail ()
  | _ -> ())
let (shaped @ total) : (table : Hmc_closure_ir.table) @ immutable -> (signature : G.signature) @ immutable -> (a : S.activation) @ immutable ->
    {u : unit | H.activation table signature a} -> {u : unit | shape signature a} @ ghost = fun table signature a premise -> ghost_ (
  H.activation_def table signature a; H.locals_def table signature.G.locals a.S.env; shape_def signature a;
  temporaries_shaped table signature.G.temporaries a.S.temporaries ())

let[@def] rec (decode_environment @ total) (context : D.context @ immutable) (cells : cells @ immutable) =
  match context with
  | D.Empty_context -> Some (R.V.Empty, cells)
  | D.Binding (_, rest) -> (match cells with Empty -> None | Cell (head, tail) ->
    match decode_environment rest tail with None -> None | Some (env, remaining) -> Some (R.V.Bind (head, env), remaining))
let rec (encode_environment @ total) : (context : D.context) @ immutable -> (env : R.V.value) @ immutable -> (tail : cells) @ immutable ->
    {u : unit | H.environment context env} ->
    {out : cells | decode_environment context out === Some (env, tail) && length out === D.add (locals_size context) (length tail)} @ immutable =
  fun context env tail premise ->
    ghost_ (H.environment_def context env; locals_size_def context);
    match context, env with
    | D.Empty_context, R.V.Empty -> ghost_ (decode_environment_def context tail; D.add_def D.Z (length tail)); tail
    | D.Binding (_, rest), R.V.Bind (head, remaining) ->
      let encoded = encode_environment rest remaining tail () in
      let out = Cell (head, encoded) in
      ghost_ (decode_environment_def context out; length_def out; D.add_def (locals_size context) (length tail)); out
    | _ -> unreachable_ ()
let[@def] rec (decode_temporaries @ total) (schema : G.temporaries @ immutable) (cells : cells @ immutable) =
  match schema with
  | G.Empty_temporaries -> Some (S.Empty, cells)
  | G.Environment (g, rest) ->
    (match decode_environment g cells with None -> None | Some (env, after_env) ->
      match decode_temporaries rest after_env with None -> None | Some (tail, remaining) -> Some (S.Environment (env, tail), remaining))
  | G.Value (g, _, rest) -> (match cells with Empty -> None | Cell (value, after_value) ->
    match decode_environment g after_value with None -> None | Some (env, after_env) ->
      match decode_temporaries rest after_env with None -> None | Some (tail, remaining) -> Some (S.Value (value, env, tail), remaining))
let rec (encode_temporaries @ total) : (schema : G.temporaries) @ immutable -> (runtime : S.temporaries) @ immutable ->
    (tail : cells) @ immutable -> {u : unit | temporaries_shape schema runtime} ->
    {out : cells | decode_temporaries schema out === Some (runtime, tail)
      && length out === D.add (temporaries_size schema) (length tail)} @ immutable = fun schema runtime tail premise ->
  ghost_ (temporaries_shape_def schema runtime; temporaries_size_def schema);
  match schema, runtime with
  | G.Empty_temporaries, S.Empty -> ghost_ (decode_temporaries_def schema tail; D.add_def D.Z (length tail)); tail
  | G.Environment (g, rest), S.Environment (env, remaining) ->
    let encoded_tail = encode_temporaries rest remaining tail () in
    let out = encode_environment g env encoded_tail () in
    ghost_ (decode_temporaries_def schema out; add_associative (locals_size g) (temporaries_size rest) (length tail)); out
  | G.Value (g, _, rest), S.Value (value, env, remaining) ->
    let encoded_tail = encode_temporaries rest remaining tail () in
    let encoded_env = encode_environment g env encoded_tail () in
    let out = Cell (value, encoded_env) in
    ghost_ (decode_temporaries_def schema out; length_def out;
      add_associative (locals_size g) (temporaries_size rest) (length tail); D.add_def (temporaries_size schema) (length tail)); out
  | _ -> unreachable_ ()
let[@def] (decode @ total) (signature : G.signature @ immutable) (pc : D.index @ immutable) (cells : cells @ immutable) =
  match cells with
  | Cell (current, Cell (accumulator, rest)) ->
    (match decode_environment signature.G.locals rest with None -> None | Some (env, after_env) ->
      match decode_temporaries signature.G.temporaries after_env with None -> None | Some (temporaries, tail) ->
        Some ({S.pc; env; accumulator; temporaries; current}, tail))
  | _ -> None
let (encode @ total) : (signature : G.signature) @ immutable -> (a : S.activation) @ immutable -> (tail : cells) @ immutable ->
    {u : unit | shape signature a} ->
    {out : cells | decode signature a.S.pc out === Some (a, tail) && length out === D.add (size signature) (length tail)} @ immutable =
  fun signature a tail premise ->
    ghost_ (shape_def signature a);
    let temporaries = encode_temporaries signature.G.temporaries a.S.temporaries tail () in
    let locals = encode_environment signature.G.locals a.S.env temporaries () in
    let out = Cell (a.S.current, Cell (a.S.accumulator, locals)) in
    ghost_ (decode_def signature a.S.pc out; length_def out; length_def (Cell (a.S.accumulator, locals));
      size_def signature; D.add_def (size signature) (length tail);
      D.add_def (D.S (D.add (locals_size signature.G.locals) (temporaries_size signature.G.temporaries))) (length tail);
      add_associative (locals_size signature.G.locals) (temporaries_size signature.G.temporaries) (length tail));
    out
