(* TEST
 has-z3;
 flags = "-extension refinement_types -principal";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml collection_surface.ml";
 { bytecode; }
 { native; }
*)

module Nonnegative = struct
  type element = int
  let[@def] test (value : int) = value >= 0
end
module Lists = Vox_sequence.For_all (Nonnegative)
module Arrays = Vox_iarray.For_all (Nonnegative)
module Clamp = struct
  type input = int
  type output = int
  let[@def] apply (value : int) = if value < 0 then 0 else value
end
module Mapped = Vox_sequence.Map (Clamp)
module Addition = struct
  type element = int
  type accumulator = Bigint.t
  let (step @ total) (value : int) (total : Bigint.t @ immutable total)
      : Bigint.t @ immutable total = Bigint.add (Bigint.of_int value) total
end
module Sum = Vox_sequence.Fold (Addition)

let (normalize @ total) : (values : int list) ->
    {result : int list | Lists.holds result &&
      Vox_sequence.length result === Vox_sequence.length values &&
      Lists.filter result === result} @ immutable total = fun values ->
  let result = Mapped.map values in
  ghost_ (
    Mapped.map_length values;
    Lists.intro result (fun index ->
      Mapped.map_at values index;
      match Vox_sequence.at values index with
      | None -> let u = () in refine_ u
      | Some value ->
        let mapped = Clamp.apply value in
        Clamp.apply_def value;
        Nonnegative.test_def mapped;
        let u = () in refine_ u);
    Lists.filter_identity result;
    let u = () in
    (refine_ u : {u : unit | Lists.holds result &&
      Vox_sequence.length result === Vox_sequence.length values &&
      Lists.filter result === result}));
  refine_ result

let (select @ total) : (values : int list) ->
    {result : int list | Lists.holds result &&
      Vox_sequence.length result <= Vox_sequence.length values &&
      Lists.filter result === result} @ immutable total = fun values ->
  let result = Lists.filter values in
  ghost_ (
    Lists.filter_holds values;
    Lists.filter_length values;
    Lists.filter_idempotent values;
    let u = () in
    (refine_ u : {u : unit | Lists.holds result &&
      Vox_sequence.length result <= Vox_sequence.length values &&
      Lists.filter result === result}));
  refine_ result

module A = Vox_iarray
external same_list : ('a : immutable_data).
  'a list @ immutable -> 'a list @ immutable -> bool = "%equal"

let () =
  let input = [-3; 4; -1; 2] in
  let refine_ normalized = normalize input in
  assert (same_list normalized [0; 4; 0; 2]);
  let refine_ selected = select input in
  assert (same_list selected [4; 2]);
  assert (Bigint.equal (Sum.fold [1; 2; 3] 0Z) 6Z)

let (slice_and_update @ total) : (values : int iarray) -> (first : int) ->
    (past : int) -> (index : int) -> (value : int) ->
    {u : unit | Arrays.holds values && 0 <= first && first <= past
      && past <= Iarray.length values && value >= 0} @ ghost ->
    {result : int iarray | Arrays.holds result &&
      Iarray.length result = past - first} @ immutable total =
    fun values first past index value premise ->
  premise;
  let part = A.slice values first past in
  let result = A.updated part index value in
  ghost_ (
    Arrays.slice_holds values first past;
    Nonnegative.test_def value;
    Arrays.updated_holds part index value;
    A.slice_length values first past;
    A.updated_length part index value;
    let u = () in
    (refine_ u : {u : unit | Arrays.holds result &&
      Iarray.length result = past - first}));
  refine_ result

let (compositional @ total) : (left : int list) -> (right : int list) ->
    (initial : Bigint.t) ->
    {u : unit | Mapped.map (Vox_sequence.append left right) ===
      Vox_sequence.append (Mapped.map left) (Mapped.map right) &&
      Lists.filter (Vox_sequence.append left right) ===
      Vox_sequence.append (Lists.filter left) (Lists.filter right) &&
      Sum.fold (Vox_sequence.append left right) initial ===
      Sum.fold left (Sum.fold right initial)} = fun left right initial ->
  Mapped.map_append left right;
  Lists.filter_append left right;
  Sum.fold_append left right initial;
  let u = () in refine_ u

let () =
  let values = [: 0; 1; 2; 3 :] in
  let u = () in
  let refine_ checked = (assume_ u : {u : unit | Arrays.holds values}) in
  let first = 1 in
  let past = 3 in
  let index = 1 in
  let value = 9 in
  let u = () in
  let refine_ result = slice_and_update values first past index value
    (refine_ u) in
  assert (same_list (A.to_list result) [1; 9])
