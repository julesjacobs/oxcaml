module C = Vox_egraph_closure_spec

val closed_bindings : C.Q.graph @ immutable -> C.R.rule @ immutable ->
    C.L.sort list @ immutable -> int list @ immutable -> int -> bool @@ total

val closed_bindings_def : (graph : C.Q.graph) @ immutable -> (rule : C.R.rule) @ immutable ->
    (vars : C.L.sort list) @ immutable -> (prefix : int list) @ immutable -> (count : int) ->
    {u : unit | closed_bindings graph rule vars prefix count =
      (match vars with
       | [] -> C.closed_roots graph rule prefix graph.count
       | _ :: rest ->
         if count <= 0 then closed_bindings graph rule rest (C.snoc prefix (-1)) graph.count
         else closed_bindings graph rule rest (C.snoc prefix (count - 1)) graph.count &&
           closed_bindings graph rule vars prefix (count - 1))} @ ghost @@ total
