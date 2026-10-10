(* TEST
 has-z3;
 flags = "-extension refinement_types -smt-unused-steps-precise";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "pref.mli pref.ml";
 readonly_files = "heap_extensionality.ml";
 {
   setup-ocamlc.opt-build-env;
   run-expect;
   check-program-output;
 }
*)

module H = Pref.Heap;;
[%%expect{|
module H = Pref.Heap
|}]

(* Heaps that agree at every location are equal. *)
let overwrite (h : int Pref.heap) (p : int Pref.t) (x : int) (y : int) :
    {u : unit | H.put (H.put h p x) p y === H.put h p y} = ();;
[%%expect{|
val overwrite :
  (h : int Pref.heap) ->
  (p : int Pref.t) ->
  (x : int) ->
  (y : int) -> {u : unit | (H.put (H.put h p x) p y) === (H.put h p y)} =
  <fun>
|}]

let unchanged (h : int Pref.heap) (p : int Pref.t) (x : int)
    (present : {u : unit | H.at h p === Some x}) :
    {u : unit | H.put h p x === h} = ();;
[%%expect{|
val unchanged :
  (h : int Pref.heap) ->
  (p : int Pref.t) ->
  (x : int) ->
  {u : unit | (H.at h p) === (Some x)} -> {u : unit | (H.put h p x) === h} =
  <fun>
|}]

let commute (h : int Pref.heap) (p : int Pref.t) (q : int Pref.t)
    (r : int Pref.t) (x : int) (y : int) (z : int)
    (distinct : {u : unit | not (p === q) && not (q === r) && not (p === r)}) :
    {u : unit | H.put (H.put (H.put h p x) q y) r z
      === H.put (H.put (H.put h r z) p x) q y} = ();;
[%%expect{|
val commute :
  (h : int Pref.heap) ->
  (p : int Pref.t) ->
  (q : int Pref.t) ->
  (r : int Pref.t) ->
  (x : int) ->
  (y : int) ->
  (z : int) ->
  {u : unit | (not (p === q)) && ((not (q === r)) && (not (p === r)))} ->
  {u : unit
    | (H.put (H.put (H.put h p x) q y) r z) ===
        (H.put (H.put (H.put h r z) p x) q y)} =
  <fun>
|}]

let empty_union (a : int Pref.heap) :
    {u : unit | H.union (H.empty ()) a === a && H.union a (H.empty ()) === a} =
  ();;
[%%expect{|
val empty_union :
  (a : int Pref.heap) ->
  {u : unit
    | ((H.union (H.empty ()) a) === a) && ((H.union a (H.empty ())) === a)} =
  <fun>
|}]

(* Heaps that differ somewhere stay different. *)
let different (h : int Pref.heap) (p : int Pref.t) (x : int) (y : int) :
    {u : unit | H.put h p x === H.put h p y} = ();;
[%%expect{|
Line 2, characters 47-49:
2 |     {u : unit | H.put h p x === H.put h p y} = ();;
                                                   ^^
Error: Refinement could not be proved (counterexample: y = 0, x = -1)
Line 2, characters 16-43:
2 |     {u : unit | H.put h p x === H.put h p y} = ();;
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

let order_matters (h : int Pref.heap) (p : int Pref.t) (x : int) (y : int) :
    {u : unit | H.put (H.put h p x) p y === H.put (H.put h p y) p x} = ();;
[%%expect{|
Line 2, characters 71-73:
2 |     {u : unit | H.put (H.put h p x) p y === H.put (H.put h p y) p x} = ();;
                                                                           ^^
Error: Refinement could not be proved (counterexample: y = 0, x = -1)
Line 2, characters 16-67:
2 |     {u : unit | H.put (H.put h p x) p y === H.put (H.put h p y) p x} = ();;
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

module Shared_union : sig end = struct
  [@@@warning "+unused-proof-step"]

  let (disjoint_empty @ total) (h : int Pref.heap) : unit @ ghost =
    ghost_ (
      let h1 = H.union h h in
      let h2 = H.union h1 h1 in
      let h3 = H.union h2 h2 in
      let h4 = H.union h3 h3 in
      let h5 = H.union h4 h4 in
      let h6 = H.union h5 h5 in
      let h7 = H.union h6 h6 in
      let h8 = H.union h7 h7 in
      let h9 = H.union h8 h8 in
      let h10 = H.union h9 h9 in
      let h11 = H.union h10 h10 in
      let h12 = H.union h11 h11 in
      let h13 = H.union h12 h12 in
      let h14 = H.union h13 h13 in
      let h15 = H.union h14 h14 in
      let h16 = H.union h15 h15 in
      let h17 = H.union h16 h16 in
      let h18 = H.union h17 h17 in
      let h19 = H.union h18 h18 in
      let h20 = H.union h19 h19 in
      let h21 = H.union h20 h20 in
      let h22 = H.union h21 h21 in
      let h23 = H.union h22 h22 in
      let h24 = H.union h23 h23 in
      let (_ : {b : bool | b}) = H.disjoint h24 (H.empty ()) in
      ())

  let (disjoint_union @ total) (a : int Pref.heap) (b : int Pref.heap)
      (h : int Pref.heap) :
      {u : unit | H.disjoint (H.union a b) h
        = (H.disjoint a h && H.disjoint b h)} @ ghost = ghost_ ()

  let[@def] (wrapped @ total) (h : int Pref.heap) : int Pref.heap @ ghost =
    ghost_ (H.union h h)

  let (overlap @ total) (p : int Pref.t) :
      {u : unit | not (H.disjoint (wrapped (H.put (H.empty ()) p 0))
        (H.put (H.empty ()) p 1))} @ ghost = ghost_ (
    wrapped_def (H.put (H.empty ()) p 0); ())
end;;
[%%expect{|
module Shared_union : sig end
|}]

let (overlapping_union @ total) (p : int Pref.t) :
    {u : unit | H.disjoint
      (H.union (H.put (H.empty ()) p 0) (H.empty ()))
      (H.put (H.empty ()) p 1)} @ ghost = ghost_ ();;
[%%expect{|
Line 4, characters 49-51:
4 |       (H.put (H.empty ()) p 1)} @ ghost = ghost_ ();;
                                                     ^^
Error: Refinement could not be proved (counterexample)
Lines 2-4, characters 16-30:
2 | ................H.disjoint
3 |       (H.union (H.put (H.empty ()) p 0) (H.empty ()))
4 |       (H.put (H.empty ()) p 1).......................
  The refinement is stated here.
|}]
