module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module C = Hmc_tagged_cell
module I = Hmc_u32_index
module L = Hmc_linear_bytes

let[@def] (distance @ total) (start : W.limb) (stop : W.limb) : W.limb = if start > stop then 0 else stop - start
let[@def] (covers @ total) (memory : B.bytes @ immutable) (limit : W.limb) = ghost_ (not (L.drop memory limit === None))
let[@def] (range @ total) (count : D.index @ immutable) (start : W.limb) (stop : W.limb) =
  if start > stop then false else I.represents count (stop - start)
let rec (suffix @ total) : (memory : B.bytes) @ immutable -> (limit : W.limb) -> (address : W.limb) ->
    {u : unit | covers memory limit && address <= limit} ->
    {out : B.bytes | L.drop memory address === Some out && covers out (distance address limit)} @ immutable = fun memory limit address premise ->
  ghost_ (distance_def address limit; covers_def memory limit; L.drop_def memory address; L.drop_def memory limit);
  if address = 0 then memory else match memory with
  | B.End -> unreachable_ ()
  | B.Byte (_, rest) ->
    ghost_ (covers_def rest (limit - 1); distance_def (address - 1) (limit - 1));
    suffix rest (limit - 1) (address - 1) ()
let rec (payload_fits @ total) : (payload : B.bytes) @ immutable -> (memory : B.bytes) @ immutable -> (width : W.limb) ->
    {u : unit | I.represents (C.length payload) width && covers memory width} ->
    {u : unit | L.fits payload memory} @ ghost = fun payload memory width premise -> ghost_ (
  C.length_def payload; I.represents_def (C.length payload) width; covers_def memory width; L.drop_def memory width;
  L.fits_def payload memory;
  match payload, memory with
  | B.Byte (_, rest), B.Byte (_, tail) -> covers_def tail (width - 1); payload_fits rest tail (width - 1) ()
  | _ -> ())
let (fits @ total) : (memory : B.bytes) @ immutable -> (limit : W.limb) -> (start : W.limb) -> (stop : W.limb) ->
    (payload : B.bytes) @ immutable ->
    {u : unit | covers memory limit && range (C.length payload) start stop && stop <= limit} ->
    {u : unit | L.fits_at memory start payload} @ ghost = fun memory limit start stop payload premise -> ghost_ (
  range_def (C.length payload) start stop; distance_def start stop;
  let _ = suffix memory limit stop () in
  covers_def memory stop;
  let tail = suffix memory stop start () in
  payload_fits payload tail (stop - start) ();
  L.fits_at_def memory start payload)
let rec (same_length @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (limit : W.limb) ->
    {u : unit | C.length before === C.length after && covers before limit} ->
    {u : unit | covers after limit} @ ghost = fun before after limit premise -> ghost_ (
  C.length_def before; C.length_def after; covers_def before limit; covers_def after limit;
  L.drop_def before limit; L.drop_def after limit;
  if limit = 0 then () else match before, after with
  | B.Byte (_, rest), B.Byte (_, tail) -> covers_def rest (limit - 1); same_length rest tail (limit - 1) (); covers_def tail (limit - 1)
  | _ -> ())
