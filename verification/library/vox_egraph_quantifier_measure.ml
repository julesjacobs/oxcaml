module L = Vox_egraph_language_spec
let[@def] rec (length @ total) (vars : L.sort list @ immutable) =
  match vars with [] -> 0Z | _ :: rest -> Bigint.add 1Z (length rest)
let rec (nonnegative @ total) : (vars : L.sort list) @ immutable ->
    {u : unit | length vars >= 0Z} @ ghost = fun vars -> ghost_ (
  length_def vars;
  match vars with [] -> () | _ :: rest -> nonnegative rest)
let[@def] (rank @ total) (vars : L.sort list @ immutable) (width : int) (count : int) =
  Bigint.add
    (Bigint.mul (length vars)
      (Bigint.add (Bigint.of_int (if width > 0 then width else 0)) 1Z))
    (Bigint.of_int (if count > 0 then count else 0))
let (rank_nonnegative @ total) : (vars : L.sort list) @ immutable ->
    (width : int) -> (count : int) -> {u : unit | rank vars width count >= 0Z} @ ghost =
  fun vars width count -> ghost_ (rank_def vars width count; nonnegative vars)
