(* TEST
 { bytecode; }
 { native; }
*)

type tree = Empty | Branch of tree * tree

type wrapped = { value : int; model : tree @@ ghost }

let[@inline never] consume (model : tree @ ghost) value = value + 1

let read wrapped =
  let model = wrapped.model in
  consume model wrapped.value

let () =
  let model = ghost_ (Branch (Empty, Empty)) in
  assert (consume model 41 = 42);
  assert (read {value = 41; model} = 42)
