(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml sparse_overlay.mli sparse_overlay.ml sparse_overlay_client.ml";
 { native; }
*)

module Client (Element : sig type t : immutable_data end) = struct
  module S = Sparse_overlay
  module L = S.Laws (Element)

  let (read_after_write @ total) : (source : Element.t S.t) ->
      (index : {i : int | 0 <= i && i < S.length source}) ->
      (value : Element.t) -> {r : Element.t | r === value} =
    fun source index value ->
    let i = index in
    let result = S.set i value source in
    ghost_ (
      L.set_base source i value;
      L.length_equation source;
      L.length_equation result;
      L.set_lookup source i value i);
    let bounded : {i : int | 0 <= i && i < S.length result} = i in
    let actual = S.get result bounded in
    ghost_ (L.get_lookup result bounded);
    actual

  let (last_write_wins @ total) (source : Element.t S.t)
      (index : int) (first : Element.t) (last : Element.t) (query : int) :
      {u : unit | S.lookup query (S.set index last (S.set index first source))
        === S.lookup query (S.set index last source)} =
    let once = S.set index first source in
    L.set_lookup source index first query;
    L.set_lookup once index last query;
    L.set_lookup source index last query;
    L.set_base source index first;
    L.length_equation source;
    L.length_equation once;
    let u = () in u

  let (independent_updates @ total) : (source : Element.t S.t) ->
      (left : int) -> (right : {i : int | i <> left}) ->
      (a : Element.t) -> (b : Element.t) -> (query : int) ->
      {u : unit | let right = right in
        S.lookup query (S.set right b (S.set left a source)) ===
        S.lookup query (S.set left a (S.set right b source))} =
    fun source left right a b query ->
    let right = right in
    let left_first = S.set left a source in
    let right_first = S.set right b source in
    L.set_lookup source left a query;
    L.set_lookup source right b query;
    L.set_lookup left_first right b query;
    L.set_lookup right_first left a query;
    L.set_base source left a;
    L.set_base source right b;
    L.length_equation source;
    L.length_equation left_first;
    L.length_equation right_first;
    let u = () in u

  let (clear_restores_base @ total) (source : Element.t S.t) (index : int) :
      {u : unit | S.lookup index (S.clear index source) ===
        Vox_iarray.at (S.base source) index} =
    L.clear_lookup source index index;
    let u = () in u
end

module Int_client = Client (struct type t = int end)
type item = {label : int}
module Item_client = Client (struct type t = item end)
module Int_laws = Sparse_overlay.Laws (struct type t = int end)

type cell = {mutable value : int}
let mutate (overlay : cell Sparse_overlay.t) (index : int) =
  let size = Sparse_overlay.length overlay in
  if 0 <= index && index < size then
    let bounded : {i : int | 0 <= i && i < Sparse_overlay.length overlay} =
      index in
    (Sparse_overlay.get overlay bounded).value <- 17

let () =
  let base = [: 10; 20; 30 :] in
  let source = Sparse_overlay.empty base in
  let updated = Sparse_overlay.set 1 99 source in
  let updated = Sparse_overlay.set 1 101 updated in
  assert (Sparse_overlay.lookup 1 updated = Some 101);
  assert (Sparse_overlay.lookup 0 updated = Some 10);
  assert (Sparse_overlay.lookup 1 (Sparse_overlay.clear 1 updated) = Some 20);
  assert (Sparse_overlay.lookup 1 source = Some 20);
  List.iter (fun (index : int) ->
    ghost_ (Int_client.last_write_wins source index 70 90 index);
    ghost_ (Int_client.clear_restores_base source index);
    let outside = Sparse_overlay.set index 42 source in
    if index < 0 || index >= 3 then
      assert (Sparse_overlay.lookup index outside = None))
    [min_int; -1; 0; 1; 2; 3; max_int];
  let left = 0 in
  let right = 2 in
  let distinct : {i : int | i <> left} = right in
  let records = Sparse_overlay.empty [: {label = 1}; {label = 2} :] in
  ghost_ (Item_client.independent_updates records left distinct
    {label = 7} {label = 8} 1);
  let cell = {value = 0} in
  let mutable_values = Sparse_overlay.empty [: cell :] in
  mutate mutable_values 0;
  assert (cell.value = 17);
  print_endline "sparse overlay: exact updates, fallback, bounds, mutable access"
