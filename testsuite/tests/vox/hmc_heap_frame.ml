module D = Hm_declarative
module C = Hmc_tagged_cell
module M = Hmc_heap_objects
module P = Hmc_heap_preservation
module K = Hmc_closure_ir
module R = Hmc_closure_semantics
module S = Hmc_cfg_semantics
module F = Hmc_frame_codec

type temporaries = Empty | Environment of M.cells * temporaries
  | Value of C.value * M.cells * temporaries [@@inductive]
type activation = {pc : D.index; env : M.cells; accumulator : C.value;
  temporaries : temporaries; current : C.value}

let[@def] rec (decode_cells @ total) (heap : M.heap @ immutable) (cells : M.cells @ immutable) =
  match cells with
  | M.Empty -> Some F.Empty
  | M.Cell (head, rest) -> (match M.decode heap head, decode_cells heap rest with
    | Some value, Some tail -> Some (F.Cell (value, tail)) | _ -> None)
let rec (cells_length @ total) : (heap : M.heap) @ immutable -> (cells : M.cells) @ immutable ->
    (values : F.cells) @ immutable -> {u : unit | decode_cells heap cells === Some values} ->
    {u : unit | M.length cells === F.length values} @ ghost = fun heap cells values premise -> ghost_ (
  decode_cells_def heap cells; M.length_def cells;
  match cells with
  | M.Empty -> F.length_def values
  | M.Cell (head, rest) -> (match M.decode heap head, decode_cells heap rest with
    | Some value, Some tail -> F.length_def values; cells_length heap rest tail () | _ -> ()))
let rec (cells_preserve @ total) : (table : K.table) @ immutable -> (larger : M.heap) @ immutable ->
    (smaller : M.heap) @ immutable -> (cells : M.cells) @ immutable -> (values : F.cells) @ immutable ->
    {u : unit | M.valid table larger && M.valid table smaller && P.extends larger smaller
      && decode_cells smaller cells === Some values} ->
    {u : unit | decode_cells larger cells === Some values} @ ghost = fun table larger smaller cells values premise -> ghost_ (
  decode_cells_def smaller cells; decode_cells_def larger cells;
  match cells with M.Empty -> () | M.Cell (head, rest) ->
    (match M.decode smaller head, decode_cells smaller rest with
    | Some value, Some tail -> P.decode_preserve table larger smaller head value (); cells_preserve table larger smaller rest tail ()
    | _ -> ()))
let[@def] rec (decode_temporaries @ total) (heap : M.heap @ immutable) (temporaries : temporaries @ immutable) =
  match temporaries with
  | Empty -> Some S.Empty
  | Environment (env, rest) ->
    (match M.decode_environment (M.view heap) env, decode_temporaries heap rest with
    | Some values, Some tail -> Some (S.Environment (values, tail)) | _ -> None)
  | Value (head, env, rest) ->
    (match M.decode heap head, M.decode_environment (M.view heap) env, decode_temporaries heap rest with
    | Some value, Some values, Some tail -> Some (S.Value (value, values, tail)) | _ -> None)
let rec (temporaries_preserve @ total) : (table : K.table) @ immutable -> (larger : M.heap) @ immutable ->
    (smaller : M.heap) @ immutable -> (temporaries : temporaries) @ immutable -> (values : S.temporaries) @ immutable ->
    {u : unit | M.valid table larger && M.valid table smaller && P.extends larger smaller
      && decode_temporaries smaller temporaries === Some values} ->
    {u : unit | decode_temporaries larger temporaries === Some values} @ ghost = fun table larger smaller temporaries values premise -> ghost_ (
  decode_temporaries_def smaller temporaries; decode_temporaries_def larger temporaries;
  match temporaries with
  | Empty -> ()
  | Environment (env, rest) ->
    (match M.decode_environment (M.view smaller) env, decode_temporaries smaller rest with
    | Some values, Some tail -> P.environment_preserve table larger smaller env values (); temporaries_preserve table larger smaller rest tail ()
    | _ -> ())
  | Value (head, env, rest) ->
    (match M.decode smaller head, M.decode_environment (M.view smaller) env, decode_temporaries smaller rest with
    | Some value, Some values, Some tail -> P.decode_preserve table larger smaller head value ();
      P.environment_preserve table larger smaller env values (); temporaries_preserve table larger smaller rest tail ()
    | _ -> ()))
let[@def] (decode @ total) (heap : M.heap @ immutable) (a : activation @ immutable) =
  match M.decode_environment (M.view heap) a.env, M.decode heap a.accumulator,
    decode_temporaries heap a.temporaries, M.decode heap a.current with
  | Some env, Some accumulator, Some temporaries, Some current -> Some {S.pc = a.pc; env; accumulator; temporaries; current}
  | _ -> None
let (preserve @ total) : (table : K.table) @ immutable -> (larger : M.heap) @ immutable -> (smaller : M.heap) @ immutable ->
    (a : activation) @ immutable -> (values : S.activation) @ immutable ->
    {u : unit | M.valid table larger && M.valid table smaller && P.extends larger smaller && decode smaller a === Some values} ->
    {u : unit | decode larger a === Some values} @ ghost = fun table larger smaller a values premise -> ghost_ (
  decode_def smaller a; decode_def larger a;
  match M.decode_environment (M.view smaller) a.env, M.decode smaller a.accumulator,
    decode_temporaries smaller a.temporaries, M.decode smaller a.current with
  | Some env, Some accumulator, Some temporaries, Some current ->
    P.environment_preserve table larger smaller a.env env ();
    P.decode_preserve table larger smaller a.accumulator accumulator ();
    temporaries_preserve table larger smaller a.temporaries temporaries ();
    P.decode_preserve table larger smaller a.current current ()
  | _ -> ())

let rec (encode_environment @ total) : (heap : M.heap) @ immutable -> (context : D.context) @ immutable ->
    (cells : M.cells) @ immutable -> (env : R.V.value) @ immutable -> (tail : M.cells) @ immutable ->
    (values : F.cells) @ immutable ->
    {u : unit | M.decode_environment (M.view heap) cells === Some env
      && Hmc_frame_shape.environment context env && decode_cells heap tail === Some values} ->
    {out : M.cells | match decode_cells heap out with None -> false | Some encoded ->
      F.decode_environment context encoded === Some (env, values)} @ immutable =
  fun heap context cells env tail values premise ->
    ghost_ (M.decode_environment_def (M.view heap) cells;
      Hmc_frame_shape.environment_def context env);
    match context, cells with
    | D.Empty_context, M.Empty -> ghost_ (F.decode_environment_def context values); tail
    | D.Binding (_, rest), M.Cell (head, remaining) ->
      (match M.decode_value (M.view heap) head, M.decode_environment (M.view heap) remaining with
      | Some value, Some next ->
        let after = encode_environment heap rest remaining next tail values () in
        let out = M.Cell (head, after) in
        ghost_ (decode_cells_def heap out; M.decode_def heap head;
          match decode_cells heap after with None -> () | Some encoded ->
            F.decode_environment_def context (F.Cell (value, encoded))); out
      | _ -> unreachable_ ())
    | _ -> unreachable_ ()
let rec (encode_temporaries @ total) : (heap : M.heap) @ immutable -> (schema : Hmc_cfg_ir.temporaries) @ immutable ->
    (runtime : temporaries) @ immutable -> (abstract : S.temporaries) @ immutable ->
    (tail : M.cells) @ immutable -> (values : F.cells) @ immutable ->
    {u : unit | decode_temporaries heap runtime === Some abstract && F.temporaries_shape schema abstract
      && decode_cells heap tail === Some values} ->
    {out : M.cells | match decode_cells heap out with None -> false | Some encoded ->
      F.decode_temporaries schema encoded === Some (abstract, values)} @ immutable =
  fun heap schema runtime abstract tail values premise ->
    ghost_ (decode_temporaries_def heap runtime; F.temporaries_shape_def schema abstract);
    match schema, runtime with
    | Hmc_cfg_ir.Empty_temporaries, Empty -> ghost_ (F.decode_temporaries_def schema values); tail
    | Hmc_cfg_ir.Environment (context, rest), Environment (env, remaining) ->
      (match M.decode_environment (M.view heap) env, decode_temporaries heap remaining with
      | Some locals, Some abstract_rest ->
        let after = encode_temporaries heap rest remaining abstract_rest tail values () in
        (match decode_cells heap after with
        | None -> unreachable_ ()
        | Some after_values ->
          let out = encode_environment heap context env locals after after_values () in
          ghost_ (match decode_cells heap out with None -> () | Some encoded -> F.decode_temporaries_def schema encoded); out)
      | _ -> unreachable_ ())
    | Hmc_cfg_ir.Value (context, _, rest), Value (head, env, remaining) ->
      (match M.decode heap head, M.decode_environment (M.view heap) env, decode_temporaries heap remaining with
      | Some value, Some locals, Some abstract_rest ->
        let after = encode_temporaries heap rest remaining abstract_rest tail values () in
        (match decode_cells heap after with
        | None -> unreachable_ ()
        | Some after_values ->
          let locals = encode_environment heap context env locals after after_values () in
          let out = M.Cell (head, locals) in
          ghost_ (decode_cells_def heap out;
            match decode_cells heap locals with None -> () | Some encoded -> F.decode_temporaries_def schema (F.Cell (value, encoded))); out)
      | _ -> unreachable_ ())
    | _ -> unreachable_ ()
let (encode @ total) : (heap : M.heap) @ immutable -> (signature : Hmc_cfg_ir.signature) @ immutable ->
    (a : activation) @ immutable -> (abstract : S.activation) @ immutable ->
    (tail : M.cells) @ immutable -> (values : F.cells) @ immutable ->
    {u : unit | decode heap a === Some abstract && F.shape signature abstract && decode_cells heap tail === Some values} ->
    {out : M.cells | match decode_cells heap out with None -> false | Some encoded ->
      F.decode signature a.pc encoded === Some (abstract, values)} @ immutable =
  fun heap signature a abstract tail values premise ->
    ghost_ (decode_def heap a; F.shape_def signature abstract);
    match M.decode_environment (M.view heap) a.env, M.decode heap a.accumulator,
      decode_temporaries heap a.temporaries, M.decode heap a.current with
    | Some env, Some accumulator, Some temporaries, Some current ->
      let after = encode_temporaries heap signature.Hmc_cfg_ir.temporaries a.temporaries temporaries tail values () in
      (match decode_cells heap after with
      | None -> unreachable_ ()
      | Some after_values ->
        let locals = encode_environment heap signature.Hmc_cfg_ir.locals a.env env after after_values () in
        let out = M.Cell (a.current, M.Cell (a.accumulator, locals)) in
        ghost_ (decode_cells_def heap out; decode_cells_def heap (M.Cell (a.accumulator, locals));
          match decode_cells heap locals with None -> () | Some encoded ->
            F.decode_def signature a.pc (F.Cell (current, F.Cell (accumulator, encoded)))); out)
    | _ -> unreachable_ ()
