module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module I = Hmc_u32_index
module L = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds

let[@def] rec (equal_prefix @ total) (count : W.limb) (left : B.bytes @ immutable) (right : B.bytes @ immutable) = ghost_ (
  if count = 0 then true else match left, right with
  | B.Byte (a, rest), B.Byte (b, tail) -> a = b && equal_prefix (count - 1) rest tail
  | _ -> false)
let rec (before_store @ total) : (before : B.bytes) @ immutable -> (address : W.limb) ->
    (payload : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    {u : unit | L.updated before address payload after} ->
    {u : unit | equal_prefix address before after} @ ghost = fun before address payload after premise -> ghost_ (
  L.updated_def before address payload after; equal_prefix_def address before after;
  if address = 0 then () else match before, after with
  | B.Byte (_, rest), B.Byte (_, tail) -> before_store rest (address - 1) payload tail () | _ -> ())
let rec (shrink @ total) : (large : W.limb) -> (small : W.limb) -> (left : B.bytes) @ immutable -> (right : B.bytes) @ immutable ->
    {u : unit | equal_prefix large left right && small <= large} ->
    {u : unit | equal_prefix small left right} @ ghost = fun large small left right premise -> ghost_ (
  equal_prefix_def large left right; equal_prefix_def small left right;
  if small = 0 then () else match left, right with
  | B.Byte (_, rest), B.Byte (_, tail) -> shrink (large - 1) (small - 1) rest tail () | _ -> ())
let rec (seek @ total) : (left : B.bytes) @ immutable -> (right : B.bytes) @ immutable -> (boundary : W.limb) -> (address : W.limb) ->
    {u : unit | equal_prefix boundary left right && address <= boundary} ->
    {u : unit | match L.drop left address, L.drop right address with
      | Some a, Some b -> equal_prefix (Bounds.distance address boundary) a b | _ -> false} @ ghost =
  fun left right boundary address premise -> ghost_ (
    Bounds.distance_def address boundary; L.drop_def left address; L.drop_def right address;
    equal_prefix_def boundary left right;
    if address = 0 then () else match left, right with
    | B.Byte (_, rest), B.Byte (_, tail) ->
      Bounds.distance_def (address - 1) (boundary - 1); seek rest tail (boundary - 1) (address - 1) ()
    | _ -> ())
let rec (take @ total) : (count : D.index) @ immutable -> (width : W.limb) -> (left : B.bytes) @ immutable -> (right : B.bytes) @ immutable ->
    {u : unit | I.represents count width && equal_prefix width left right} ->
    {u : unit | L.take count left === L.take count right} @ ghost = fun count width left right premise -> ghost_ (
  I.represents_def count width; equal_prefix_def width left right; L.take_def count left; L.take_def count right;
  match count, left, right with
  | D.S n, B.Byte (_, rest), B.Byte (_, tail) -> take n (width - 1) rest tail () | _ -> ())
let (load @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (boundary : W.limb) ->
    (address : W.limb) -> (stop : W.limb) -> (count : D.index) @ immutable ->
    {u : unit | equal_prefix boundary before after && Bounds.range count address stop && stop <= boundary} ->
    {u : unit | L.load before address count === L.load after address count} @ ghost =
  fun before after boundary address stop count premise -> ghost_ (
    Bounds.range_def count address stop; Bounds.distance_def address boundary;
    seek before after boundary address ();
    L.load_def before address count; L.load_def after address count;
    match L.drop before address, L.drop after address with
    | Some left, Some right -> shrink (boundary - address) (stop - address) left right (); take count (stop - address) left right ()
    | _ -> ())
let (disjoint_before @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (written : W.limb) ->
    (payload : B.bytes) @ immutable -> (address : W.limb) -> (stop : W.limb) -> (count : D.index) @ immutable ->
    {u : unit | L.updated before written payload after && Bounds.range count address stop && stop <= written} ->
    {u : unit | L.load before address count === L.load after address count} @ ghost =
  fun before after written payload address stop count premise -> ghost_ (
    before_store before written payload after (); load before after written address stop count ())
let rec (overlay_suffix @ total) : (payload : B.bytes) @ immutable -> (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (width : W.limb) -> {u : unit | L.overlay payload before after && I.represents (Hmc_tagged_cell.length payload) width} ->
    {u : unit | L.drop before width === L.drop after width} @ ghost = fun payload before after width premise -> ghost_ (
  L.overlay_def payload before after; Hmc_tagged_cell.length_def payload;
  I.represents_def (Hmc_tagged_cell.length payload) width; L.drop_def before width; L.drop_def after width;
  match payload, before, after with
  | B.Byte (_, rest), B.Byte (_, left), B.Byte (_, right) -> overlay_suffix rest left right (width - 1) ()
  | _ -> ())
let rec (after_store @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (address : W.limb) ->
    (stop : W.limb) -> (payload : B.bytes) @ immutable ->
    {u : unit | L.updated before address payload after && Bounds.range (Hmc_tagged_cell.length payload) address stop} ->
    {u : unit | L.drop before stop === L.drop after stop} @ ghost = fun before after address stop payload premise -> ghost_ (
  L.updated_def before address payload after; Bounds.range_def (Hmc_tagged_cell.length payload) address stop;
  if address = 0 then overlay_suffix payload before after stop () else (
    L.drop_def before stop; L.drop_def after stop;
    match before, after with
    | B.Byte (_, left), B.Byte (_, right) ->
      Bounds.range_def (Hmc_tagged_cell.length payload) (address - 1) (stop - 1);
      after_store left right (address - 1) (stop - 1) payload ()
    | _ -> ()))
