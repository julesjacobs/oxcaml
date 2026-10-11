(** Minimum-edit diff on lists. Any minimum-cost diff is allowed. *)

open Vox_diff_spec

(** The comparison must decide exact equality of elements. *)
type ('a : logical_data) equality =
  (x : 'a) -> (y : 'a) -> {same : bool | same = (x === y)}

(** Return a minimum-cost diff. Raises [Invalid_argument] if either input
    has more than 1,000,000 elements. *)
val diff : 'a equality @ total ->
  (old : 'a list) -> (fresh : 'a list) ->
  {r : 'a optimal_diff | source r.edits === old && target r.edits === fresh}

(** Applying a diff succeeds exactly when its source matches the input. *)
val apply : 'a equality @ total -> (old : 'a list) -> (edits : 'a diff) ->
  {result : 'a list option | result ===
    (if old === source edits then Some (target edits) else None)} @@ total

(** Swap source and target without changing the edit cost. *)
val invert : (edits : 'a diff) ->
  {inverse : 'a diff | source inverse === target edits
    && target inverse === source edits && cost inverse = cost edits}
  @@ total
