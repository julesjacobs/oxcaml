module L = Vox_egraph_language_spec
let[@def] rec (length @ total) (vars : L.sort list @ immutable) =
  match vars with [] -> 0Z | _ :: rest -> Bigint.add 1Z (length rest)
let rec (nonnegative @ total) : (vars : L.sort list) @ immutable ->
    {u : unit | length vars >= 0Z} @ ghost = fun vars -> ghost_ (
  length_def vars;
  match vars with [] -> () | _ :: rest -> nonnegative rest)
