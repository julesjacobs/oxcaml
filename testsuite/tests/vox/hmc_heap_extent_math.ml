module D = Hm_declarative
module W = Hmc_word64
module E = Hmc_heap_extent

let rec (add_one @ total) : (count : D.index) @ immutable -> {u : unit | D.add count (D.S D.Z) === D.S count} @ ghost = fun count -> ghost_ (
  D.add_def count (D.S D.Z); match count with D.Z -> () | D.S n -> add_one n)
let rec (join @ total) : (left : D.index) @ immutable -> (right : D.index) @ immutable ->
    (start : W.limb) -> (middle : W.limb) -> (stop : W.limb) ->
    {u : unit | E.span left start middle && E.span right middle stop} ->
    {u : unit | E.span (D.add left right) start stop} @ ghost = fun left right start middle stop premise -> ghost_ (
  E.span_def left start middle; D.add_def left right; E.span_def (D.add left right) start stop;
  match left with D.Z -> () | D.S rest -> join rest right (start + 16) middle stop ())
let rec (prefix @ total) : (left : D.index) @ immutable -> (right : D.index) @ immutable ->
    (start : W.limb) -> (limit : W.limb) -> {u : unit | E.fits (D.add left right) start limit} ->
    {u : unit | E.fits left start limit} @ ghost = fun left right start limit premise -> ghost_ (
  D.add_def left right; E.fits_def (D.add left right) start limit; E.fits_def left start limit;
  match left with D.Z -> () | D.S rest -> prefix rest right (start + 16) limit ())
let rec (consume @ total) : (left : D.index) @ immutable -> (right : D.index) @ immutable ->
    (start : W.limb) -> (middle : W.limb) -> (limit : W.limb) ->
    {u : unit | E.span left start middle && E.fits (D.add left right) start limit} ->
    {u : unit | E.fits right middle limit} @ ghost = fun left right start middle limit premise -> ghost_ (
  E.span_def left start middle; D.add_def left right; E.fits_def (D.add left right) start limit;
  match left with D.Z -> () | D.S rest -> consume rest right (start + 16) middle limit ())
