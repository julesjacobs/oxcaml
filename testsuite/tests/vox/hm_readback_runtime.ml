open Copy_spec
module F = Level_finite_spec
module U = Level_unifier_spec

let rec read :
    (tree : F.tree) @ immutable ghost ->
    (p : node Pref.t) @ immutable ->
    (state : {t : node Pref.token |
      F.finite (Pref.own t) tree && F.tree_root tree === p}) @ local read ->
    {t : ty | t === F.readback tree} @ immutable = fun tree p state ->
  ghost_ (
    let h = Pref.own state in
    F.finite_def h tree; F.tree_root_def tree; F.readback_def tree;
    U.observe_def h p);
  let node = Pref.read p state in
  match node.desc with
  | Var ->
    ghost_ (match tree with
      | F.Free _ -> ()
      | F.Constant_tree _ | F.Word_tree _ | F.List_tree _ | F.Branch _ | F.Alias_tree _ ->
        unreachable_ ());
    Variable p
  | Bool ->
    ghost_ (match tree with
      | F.Constant_tree _ -> ()
      | F.Free _ | F.Word_tree _ | F.List_tree _ | F.Branch _ | F.Alias_tree _ -> unreachable_ ());
    Boolean
  | Word ->
    ghost_ (match tree with F.Word_tree _ -> () | _ -> unreachable_ ());
    Word64
  | List p ->
    let child = ghost_ (match tree with
      | F.List_tree (_, child) -> child | _ -> unreachable_ ()) in
    List_type (read child p state)
  | Arrow (a, b) ->
    let left = ghost_ (match tree with
      | F.Branch (_, left, _) -> left
      | _ -> unreachable_ ()) in
    let right = ghost_ (match tree with
      | F.Branch (_, _, right) -> right
      | _ -> unreachable_ ()) in
    let a = read left a state in
    let b = read right b state in
    Function (a, b)
  | Link q ->
    let child = ghost_ (match tree with
      | F.Alias_tree (_, child) -> child
      | _ -> unreachable_ ()) in
    read child q state
