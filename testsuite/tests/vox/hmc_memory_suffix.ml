module W = Hmc_word64
module B = Wasm_u32
module D = Hm_declarative
module L = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds

let rec (drop @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (boundary : W.limb) -> (address : W.limb) ->
    {u : unit | Bounds.covers before boundary && Bounds.covers after boundary && boundary <= address
      && L.drop before boundary === L.drop after boundary} ->
    {u : unit | L.drop before address === L.drop after address} @ ghost = fun before after boundary address premise -> ghost_ (
  Bounds.covers_def before boundary; Bounds.covers_def after boundary;
  L.drop_def before boundary; L.drop_def after boundary;
  L.drop_def before address; L.drop_def after address;
  if boundary = 0 then () else match before, after with
  | B.Byte (_, left), B.Byte (_, right) ->
    Bounds.covers_def left (boundary - 1); Bounds.covers_def right (boundary - 1);
    drop left right (boundary - 1) (address - 1) ()
  | _ -> ())
let (load @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (boundary : W.limb) -> (address : W.limb) -> (count : D.index) @ immutable ->
    {u : unit | Bounds.covers before boundary && Bounds.covers after boundary && boundary <= address
      && L.drop before boundary === L.drop after boundary} ->
    {u : unit | L.load before address count === L.load after address count} @ ghost = fun before after boundary address count premise -> ghost_ (
  drop before after boundary address (); L.load_def before address count; L.load_def after address count)
