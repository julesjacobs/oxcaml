open Vox_diff_spec
open Vox_diff

let verified_client : type (a : logical_data).
    equal:a equality @ total ->
    (old : a list) -> (fresh : a list) -> (other : a diff) @ ghost ->
    {s : a diff | source s === old && target s === fresh
      && (if source other === old && target other === fresh
          then cost s <= cost other else true)} =
    fun ~equal old fresh other ->
  let result = diff equal old fresh in
  ghost_ (result.optimality other);
  let edits = result.edits in
  ghost_ (
    let forward = apply equal old edits in
    let inverse = invert edits in
    let backward = apply equal fresh inverse in
    (() : {u : unit | forward === Some fresh && backward === Some old
      && cost inverse = cost edits}));
  edits

let accepted : equal:int equality @ total ->
    (old : int list) -> (fresh : int list) ->
    {s : int diff | source s === old && target s === fresh} =
  fun ~equal old fresh -> (diff equal old fresh).edits
