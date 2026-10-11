(** Minimum-edit diffs with counted keeps. *)

open Vox_compact_diff_spec

type ('a : logical_data) equality =
  (x : 'a) -> (y : 'a) -> {same : bool | same = (x === y)}

(** Any minimum-cost diff is allowed. Raises [Invalid_argument] if either
    input has more than 1,000,000 elements. *)
val diff : 'a equality @ total ->
  (old : 'a list) -> (fresh : 'a list) ->
  {r : 'a optimal_diff | r.old === old && r.fresh === fresh}

(** Negative or oversized keeps, mismatching deletions, and unconsumed input
    fail. Keeps copy the actual source elements. *)
val apply : 'a equality @ total -> (old : 'a list) -> (edits : 'a diff) ->
  {result : 'a list option | result === patch edits old} @@ total

val invert : (edits : 'a diff) ->
  {result : 'a diff | result === inverse edits && cost result = cost edits}
  @@ total

(** Every successful patch can be undone. *)
val invert_correct : (edits : 'a diff) -> (old : 'a list) -> (fresh : 'a list) ->
  {u : unit | if relates edits old fresh
    then relates (inverse edits) fresh old else true} @@ total
