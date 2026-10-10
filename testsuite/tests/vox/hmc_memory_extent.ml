module D = Hm_declarative
module W = Hmc_word64
module C = Hmc_tagged_cell
module I = Hmc_u32_index
module E = Hmc_heap_extent
module Wire = Hmc_heap_wire
module Bounds = Hmc_linear_bounds

let (four @ total) : (count : D.index) @ immutable -> (width : W.limb) -> (total : W.limb) ->
    {u : unit | total = width + 4 && I.represents count width} ->
    {u : unit | I.represents (C.four count) total} @ ghost = fun count width total premise -> ghost_ (
  C.four_def count;
  I.represents_def (D.S (D.S (D.S (D.S count)))) total;
  I.represents_def (D.S (D.S (D.S count))) (width + 3);
  I.represents_def (D.S (D.S count)) (width + 2);
  I.represents_def (D.S count) (width + 1))
let (sixteen @ total) : (count : D.index) @ immutable -> (width : W.limb) -> (total : W.limb) ->
    {u : unit | total = width + 16 && I.represents count width} ->
    {u : unit | I.represents (C.sixteen count) total} @ ghost = fun count width total premise -> ghost_ (
  C.sixteen_def count; C.eight_def count; C.eight_def (C.eight count);
  four count width (width + 4) ();
  four (C.four count) (width + 4) (width + 8) ();
  four (C.eight count) (width + 8) (width + 12) ();
  four (C.four (C.eight count)) (width + 12) total ())
let rec (cells @ total) : (count : D.index) @ immutable -> (start : W.limb) -> (stop : W.limb) ->
    {u : unit | E.span count start stop} ->
    {u : unit | Bounds.range (Wire.bytes_size count D.Z) start stop} @ ghost = fun count start stop premise -> ghost_ (
  E.span_def count start stop; E.ordered count start stop ();
  Wire.bytes_size_def count D.Z; Bounds.range_def (Wire.bytes_size count D.Z) start stop;
  match count with
  | D.Z -> I.represents_def D.Z (stop - start)
  | D.S rest ->
    cells rest (start + 16) stop ();
    Bounds.range_def (Wire.bytes_size rest D.Z) (start + 16) stop;
    sixteen (Wire.bytes_size rest D.Z) (stop - (start + 16)) (stop - start) ())
