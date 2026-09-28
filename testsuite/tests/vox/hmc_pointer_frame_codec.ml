module D = Hm_declarative
module G = Hmc_cfg_ir
module M = Hmc_heap_objects
module F = Hmc_heap_frame
let[@def] rec (environment @ total) (context : D.context @ immutable) (env : M.cells @ immutable) =
  match context, env with
  | D.Empty_context, M.Empty -> true
  | D.Binding (_, rest), M.Cell (_, tail) -> environment rest tail
  | _ -> false
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

let[@def] rec (temporaries_shape @ total) (schema : G.temporaries @ immutable) (runtime : F.temporaries @ immutable) =
  match schema, runtime with
  | G.Empty_temporaries, F.Empty -> true
  | G.Environment (g, rest), F.Environment (env, tail) -> environment g env && temporaries_shape rest tail
  | G.Value (g, _, rest), F.Value (_, env, tail) -> environment g env && temporaries_shape rest tail
  | _ -> false
let[@def] (shape @ total) (signature : G.signature @ immutable) (a : F.activation @ immutable) =
  environment signature.G.locals a.F.env && temporaries_shape signature.G.temporaries a.F.temporaries
let[@def] rec (decode_environment @ total) (context : D.context @ immutable) (cells : M.cells @ immutable) =
  match context with
  | D.Empty_context -> Some (M.Empty, cells)
  | D.Binding (_, rest) -> (match cells with M.Empty -> None | M.Cell (head, tail) ->
    match decode_environment rest tail with None -> None | Some (env, remaining) -> Some (M.Cell (head, env), remaining))
let rec (encode_environment @ total) : (context : D.context) @ immutable -> (env : M.cells) @ immutable -> (tail : M.cells) @ immutable ->
    {u : unit | environment context env} ->
    {out : M.cells | decode_environment context out === Some (env, tail) && M.length out === D.add (locals_size context) (M.length tail)} @ immutable =
  fun context env tail premise ->
    ghost_ (environment_def context env; locals_size_def context);
    match context, env with
    | D.Empty_context, M.Empty -> ghost_ (decode_environment_def context tail; D.add_def D.Z (M.length tail)); tail
    | D.Binding (_, rest), M.Cell (head, remaining) ->
      let encoded = encode_environment rest remaining tail () in
      let out = M.Cell (head, encoded) in
      ghost_ (decode_environment_def context out; M.length_def out; D.add_def (locals_size context) (M.length tail)); out
    | _ -> unreachable_ ()
let[@def] rec (decode_temporaries @ total) (schema : G.temporaries @ immutable) (cells : M.cells @ immutable) =
  match schema with
  | G.Empty_temporaries -> Some (F.Empty, cells)
  | G.Environment (g, rest) ->
    (match decode_environment g cells with None -> None | Some (env, after_env) ->
      match decode_temporaries rest after_env with None -> None | Some (tail, remaining) -> Some (F.Environment (env, tail), remaining))
  | G.Value (g, _, rest) -> (match cells with M.Empty -> None | M.Cell (value, after_value) ->
    match decode_environment g after_value with None -> None | Some (env, after_env) ->
      match decode_temporaries rest after_env with None -> None | Some (tail, remaining) -> Some (F.Value (value, env, tail), remaining))
let rec (encode_temporaries @ total) : (schema : G.temporaries) @ immutable -> (runtime : F.temporaries) @ immutable ->
    (tail : M.cells) @ immutable -> {u : unit | temporaries_shape schema runtime} ->
    {out : M.cells | decode_temporaries schema out === Some (runtime, tail)
      && M.length out === D.add (temporaries_size schema) (M.length tail)} @ immutable = fun schema runtime tail premise ->
  ghost_ (temporaries_shape_def schema runtime; temporaries_size_def schema);
  match schema, runtime with
  | G.Empty_temporaries, F.Empty -> ghost_ (decode_temporaries_def schema tail; D.add_def D.Z (M.length tail)); tail
  | G.Environment (g, rest), F.Environment (env, remaining) ->
    let encoded_tail = encode_temporaries rest remaining tail () in
    let out = encode_environment g env encoded_tail () in
    ghost_ (decode_temporaries_def schema out; add_associative (locals_size g) (temporaries_size rest) (M.length tail)); out
  | G.Value (g, _, rest), F.Value (value, env, remaining) ->
    let encoded_tail = encode_temporaries rest remaining tail () in
    let encoded_env = encode_environment g env encoded_tail () in
    let out = M.Cell (value, encoded_env) in
    ghost_ (decode_temporaries_def schema out; M.length_def out;
      add_associative (locals_size g) (temporaries_size rest) (M.length tail); D.add_def (temporaries_size schema) (M.length tail)); out
  | _ -> unreachable_ ()
let[@def] (decode @ total) (signature : G.signature @ immutable) (pc : D.index @ immutable) (cells : M.cells @ immutable) =
  match cells with
  | M.Cell (current, M.Cell (accumulator, rest)) ->
    (match decode_environment signature.G.locals rest with None -> None | Some (env, after_env) ->
      match decode_temporaries signature.G.temporaries after_env with None -> None | Some (temporaries, tail) ->
        Some ({F.pc; env; accumulator; temporaries; current}, tail))
  | _ -> None
let (encode @ total) : (signature : G.signature) @ immutable -> (a : F.activation) @ immutable -> (tail : M.cells) @ immutable ->
    {u : unit | shape signature a} ->
    {out : M.cells | decode signature a.F.pc out === Some (a, tail) && M.length out === D.add (size signature) (M.length tail)} @ immutable =
  fun signature a tail premise ->
    ghost_ (shape_def signature a);
    let temporaries = encode_temporaries signature.G.temporaries a.F.temporaries tail () in
    let locals = encode_environment signature.G.locals a.F.env temporaries () in
    let out = M.Cell (a.F.current, M.Cell (a.F.accumulator, locals)) in
    ghost_ (decode_def signature a.F.pc out; M.length_def out; M.length_def (M.Cell (a.F.accumulator, locals));
      size_def signature; D.add_def (size signature) (M.length tail);
      D.add_def (D.S (D.add (locals_size signature.G.locals) (temporaries_size signature.G.temporaries))) (M.length tail);
      add_associative (locals_size signature.G.locals) (temporaries_size signature.G.temporaries) (M.length tail));
    out
