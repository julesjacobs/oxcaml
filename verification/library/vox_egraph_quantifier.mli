(* Closure under one rule for every assignment of its variables; part of the
   trusted specification. [closed_bindings graph rule vars prefix count]
   says that [C.closed_roots] holds at every root for every assignment that
   extends [prefix] with one id per sort in [vars]: the first variable takes
   the ids [count - 1] down to 0 and then -1 (no node), and each later one
   the ids [graph.count - 1] down to 0 and then -1. Sorts are not filtered
   here; [C.closed_roots] checks them with [C.binding_valid]. Only the
   defining equation [closed_bindings_def] is visible:
   vox_egraph_quantifier.ml defines the function by recursion on the
   lexicographic measure (length of [vars], [count]) and proves the
   equation, which keeps the measure out of the specification. *)

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
