(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml";
 readonly_files = "heap_extensionality.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
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
Error: Refinement could not be proved (counterexample)
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
Error: Refinement could not be proved (counterexample)
Line 2, characters 16-67:
2 |     {u : unit | H.put (H.put h p x) p y === H.put (H.put h p y) p x} = ();;
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]
