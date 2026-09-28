module D = Hm_declarative
module M = Hmc_heap_objects
module F = Hmc_heap_frame
module G = Hmc_cfg_ir
module S = Hmc_cfg_semantics
module V = Hmc_closure_semantics.V
module H = Hmc_frame_shape
module C = Hmc_pointer_frame_codec
module A = Hmc_frame_codec

let rec (locals_size @ total) : (context : D.context) @ immutable ->
    {u : unit | C.locals_size context === A.locals_size context} @ ghost = fun context -> ghost_ (
  C.locals_size_def context; A.locals_size_def context;
  match context with D.Empty_context -> () | D.Binding (_, rest) -> locals_size rest)
let rec (temporaries_size @ total) : (schema : G.temporaries) @ immutable ->
    {u : unit | C.temporaries_size schema === A.temporaries_size schema} @ ghost = fun schema -> ghost_ (
  C.temporaries_size_def schema; A.temporaries_size_def schema;
  match schema with
  | G.Empty_temporaries -> ()
  | G.Environment (g, rest) | G.Value (g, _, rest) -> locals_size g; temporaries_size rest)
let (size @ total) : (signature : G.signature) @ immutable ->
    {u : unit | C.size signature === A.size signature} @ ghost = fun signature -> ghost_ (
  C.size_def signature; A.size_def signature; locals_size signature.G.locals; temporaries_size signature.G.temporaries)
let rec (environment @ total) : (heap : M.heap) @ immutable -> (context : D.context) @ immutable ->
    (cells : M.cells) @ immutable -> (values : V.value) @ immutable ->
    {u : unit | M.decode_environment (M.view heap) cells === Some values && H.environment context values} ->
    {u : unit | C.environment context cells} @ ghost = fun heap context cells values premise -> ghost_ (
  M.decode_environment_def (M.view heap) cells; H.environment_def context values; C.environment_def context cells;
  match context, cells with
  | D.Binding (_, rest), M.Cell (_, tail) ->
    (match M.decode_environment (M.view heap) tail with None -> () | Some values -> environment heap rest tail values ())
  | _ -> ())
let rec (temporaries @ total) : (heap : M.heap) @ immutable -> (schema : G.temporaries) @ immutable ->
    (runtime : F.temporaries) @ immutable -> (values : S.temporaries) @ immutable ->
    {u : unit | F.decode_temporaries heap runtime === Some values && A.temporaries_shape schema values} ->
    {u : unit | C.temporaries_shape schema runtime} @ ghost = fun heap schema runtime values premise -> ghost_ (
  F.decode_temporaries_def heap runtime; A.temporaries_shape_def schema values; C.temporaries_shape_def schema runtime;
  match schema, runtime with
  | G.Environment (context, rest), F.Environment (env, tail)
  | G.Value (context, _, rest), F.Value (_, env, tail) ->
    (match M.decode_environment (M.view heap) env, F.decode_temporaries heap tail with
    | Some values, Some remaining -> environment heap context env values (); temporaries heap rest tail remaining ()
    | _ -> ())
  | _ -> ())
let (shape @ total) : (heap : M.heap) @ immutable -> (signature : G.signature) @ immutable ->
    (runtime : F.activation) @ immutable -> (values : S.activation) @ immutable ->
    {u : unit | F.decode heap runtime === Some values && A.shape signature values} ->
    {u : unit | C.shape signature runtime} @ ghost = fun heap signature runtime values premise -> ghost_ (
  F.decode_def heap runtime; A.shape_def signature values; C.shape_def signature runtime;
  environment heap signature.G.locals runtime.F.env values.S.env ();
  temporaries heap signature.G.temporaries runtime.F.temporaries values.S.temporaries ())
