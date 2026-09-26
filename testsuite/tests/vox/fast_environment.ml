open Copy_spec
module E = Hm_environment_spec
module D = Hm_declarative

type tree = Leaf of node Pref.t | Branch of int * node Pref.t * tree * tree [@@inductive]
type forest = Nil | Cons of tree * forest [@@inductive]

let[@def] (weight @ total) (t : tree @ immutable) =
  match t with Leaf _ -> 1 | Branch (n, _, _, _) -> n
let[@def] rec (valid_tree @ total) (t : tree @ immutable) =
  match t with Leaf _ -> true | Branch (n, _, a, b) ->
    valid_tree a && valid_tree b && weight a = weight b
    && weight a > 0 && n > weight a
    && n = 1 + weight a + weight b
let[@def] rec (valid_forest @ total) (f : forest @ immutable) =
  match f with Nil -> true | Cons (t, rest) -> valid_tree t && valid_forest rest
let[@def] rec (append @ total) (a : E.env @ immutable) (b : E.env @ immutable) =
  match a with E.Empty -> b | E.Bind (p, rest) -> E.Bind (p, append rest b)
let[@def] rec (flatten_tree @ total) (t : tree @ immutable) =
  match t with Leaf p -> E.Bind (p, E.Empty)
  | Branch (_, p, a, b) -> E.Bind (p, append (flatten_tree a) (flatten_tree b))
let[@def] rec (flatten @ total) (f : forest @ immutable) =
  match f with Nil -> E.Empty | Cons (t, rest) -> append (flatten_tree t) (flatten rest)
let[@def] rec (at @ total) (env : E.env @ immutable) (i : int) =
  match env with E.Empty -> None | E.Bind (p, rest) ->
    if i = 0 then Some p else if i > 0 then at rest (i - 1) else None
let[@def] rec (sized @ total) (env : E.env @ immutable) (n : int) =
  match env with E.Empty -> n = 0
  | E.Bind (_, rest) -> n > 0 && sized rest (n - 1)
let[@def] rec (encoded @ total) (i : D.index @ immutable) (n : int) =
  match i with D.Z -> n = 0 | D.S i -> n > 0 && encoded i (n - 1)

let rec (append_assoc @ total) : (a : E.env) @ immutable ->
    (b : E.env) @ immutable -> (c : E.env) @ immutable ->
    {u : unit | append (append a b) c === append a (append b c)} @ ghost =
  fun a b c -> ghost_ (
    append_def a b; let ab = append a b in append_def ab c;
    let bc = append b c in append_def a bc;
    (match a with E.Empty -> () | E.Bind (_, rest) -> append_assoc rest b c; ());
    ())

let rec (append_sized @ total) : (a : E.env) @ immutable ->
    (b : E.env) @ immutable -> (n : int) -> (m : int) ->
    {u : unit | not (sized a n && sized b m && n >= 0 && m >= 0
      && n + m >= n) || sized (append a b) (n + m)} @ ghost =
  fun a b n m -> ghost_ (
    sized_def a n; append_def a b;
    let ab = append a b in let count = n + m in sized_def ab count;
    (match a with E.Empty -> () | E.Bind (_, rest) ->
      if n > 0 then (let next = n - 1 in append_sized rest b next m; ()) else ());
    ())

let rec (tree_sized @ total) : (t : tree) @ immutable ->
    {u : unit | not (valid_tree t) || (weight t > 0 && sized (flatten_tree t) (weight t))} @ ghost =
  fun t -> ghost_ (
    valid_tree_def t; flatten_tree_def t; weight_def t;
    let env = flatten_tree t in let n = weight t in sized_def env n;
    (match t with Leaf _ -> let e = E.Empty in sized_def e 0; ()
    | Branch (_, _, a, b) ->
      tree_sized a; tree_sized b;
      let left = flatten_tree a in let right = flatten_tree b in
      let n = weight a in let m = weight b in append_sized left right n m; ());
    ())

let rec (append_at @ total) : (a : E.env) @ immutable ->
    (b : E.env) @ immutable -> (n : int) -> (i : int) ->
    {u : unit | not (sized a n && i >= 0) ||
      at (append a b) i ===
      (if i < n then at a i else at b (i - n))} @ ghost =
  fun a b n i -> ghost_ (
    append_def a b; sized_def a n;
    let ab = append a b in at_def ab i; at_def a i;
    (match a with E.Empty -> () | E.Bind (_, rest) ->
      if i > 0 then (let j = i - 1 in let count = n - 1 in append_at rest b count j; ()) else ());
    ())

let rec (lookup_encoded @ total) : (env : E.env) @ immutable ->
    (index : D.index) @ immutable -> (n : int) ->
    {u : unit | not (encoded index n) || at env n === E.lookup env index} @ ghost =
  fun env index n -> ghost_ (
    encoded_def index n; at_def env n; E.lookup_def env index;
    (match env with E.Empty -> () | E.Bind (_, rest) -> match index with
      | D.Z -> () | D.S tail -> let next = n - 1 in lookup_encoded rest tail next; ());
    ())

let cons : (p : node Pref.t) @ immutable ->
    (f : {f : forest | valid_forest f}) @ immutable ->
    {out : forest | let refine_ f = f in valid_forest out
      && flatten out === E.Bind (p, flatten f)} @ immutable = fun p f ->
  ghost_ (valid_forest_def f; flatten_def f);
  match f with
  | Cons (a, Cons (b, rest)) ->
    let na = weight a in let nb = weight b in
    if na = nb then (
      let size = 1 + na + nb in
      if size <= na then failwith "environment capacity" else (
        let tree = Branch (size, p, a, b) in
        let out = Cons (tree, rest) in
        ghost_ (let tail = Cons (b, rest) in valid_forest_def tail; flatten_def tail;
          tree_sized a; tree_sized b;
          weight_def tree; valid_tree_def tree; valid_forest_def out;
          flatten_tree_def tree; flatten_def out;
          let left = flatten_tree a in let right = flatten_tree b in
          let suffix = flatten rest in append_assoc left right suffix;
          let both = append left right in let prefix = E.Bind (p, both) in
          append_def prefix suffix; ());
        out))
    else (
      let tree = Leaf p in let out = Cons (tree, f) in
      ghost_ (valid_tree_def tree; valid_forest_def out; flatten_tree_def tree;
        flatten_def out; let prefix = E.Bind (p, E.Empty) in let suffix = flatten f in
        append_def prefix suffix; let empty = E.Empty in append_def empty suffix; ());
      out)
  | Nil | Cons (_, Nil) ->
    let tree = Leaf p in let out = Cons (tree, f) in
    ghost_ (valid_tree_def tree; valid_forest_def out; flatten_tree_def tree;
      flatten_def out; let prefix = E.Bind (p, E.Empty) in let suffix = flatten f in
      append_def prefix suffix; let empty = E.Empty in append_def empty suffix; ());
    out

let rec lookup_tree : (tree : tree) @ immutable -> (index : int) ->
    (premise : ({u : unit | valid_tree tree && index >= 0}) Ghost.t) ->
    {p : node Pref.t option | p === at (flatten_tree tree) index} @ immutable =
  fun tree index premise ->
    ghost_ (let _ = premise.Ghost.ghost in
      valid_tree_def tree; flatten_tree_def tree;
      let env = flatten_tree tree in at_def env index; ());
    match tree with
    | Leaf p ->
      if index = 0 then (let out = Some p in out)
      else (ghost_ (let empty = E.Empty in let next = index - 1 in at_def empty next; ());
        let out = None in out)
    | Branch (_, p, left, right) ->
      if index = 0 then (let out = Some p in out) else (
        let next = index - 1 in let count = weight left in
        ghost_ (tree_sized left; tree_sized right;
          let a = flatten_tree left in let b = flatten_tree right in
          append_at a b count next; ());
        if next < count then (
          let premise : ({u : unit | valid_tree left && next >= 0}) Ghost.t =
            {Ghost.ghost = ghost_ ()} in
          let out = lookup_tree left next premise in out)
        else (
          let next = next - count in
          let premise : ({u : unit | valid_tree right && next >= 0}) Ghost.t =
            {Ghost.ghost = ghost_ ()} in
          let out = lookup_tree right next premise in out))

let rec lookup : (f : forest) @ immutable -> (index : int) ->
    (premise : ({u : unit | valid_forest f && index >= 0}) Ghost.t) ->
    {p : node Pref.t option | p === at (flatten f) index} @ immutable =
  fun f index premise ->
    ghost_ (let _ = premise.Ghost.ghost in valid_forest_def f; flatten_def f);
    match f with
    | Nil -> ghost_ (let env = E.Empty in at_def env index; ()); let out = None in out
    | Cons (tree, rest) ->
      let count = weight tree in
      ghost_ (tree_sized tree;
        let prefix = flatten_tree tree in let suffix = flatten rest in
        append_at prefix suffix count index; ());
      if index < count then (
        let premise : ({u : unit | valid_tree tree && index >= 0}) Ghost.t =
          {Ghost.ghost = ghost_ ()} in
        let out = lookup_tree tree index premise in out)
      else (
        let next = index - count in
        let premise : ({u : unit | valid_forest rest && next >= 0}) Ghost.t =
          {Ghost.ghost = ghost_ ()} in
        let out = lookup rest next premise in out)

let rec compile_work : (goal : E.env Ghost.t) @ immutable ->
    (env : E.env) @ immutable ->
    (use : ((f : {f : forest | valid_forest f && flatten f === env}) @ immutable ->
      {f : forest | valid_forest f && flatten f === goal.Ghost.ghost} @ immutable)) ->
    {f : forest | valid_forest f && flatten f === goal.Ghost.ghost} @ immutable =
  fun goal env use -> match env with
  | E.Empty -> let out = Nil in
    ghost_ (valid_forest_def out; flatten_def out); use (out)
  | E.Bind (p, rest) ->
    let resume : (f : {f : forest | valid_forest f && flatten f === rest}) @ immutable ->
        {f : forest | valid_forest f && flatten f === goal.Ghost.ghost} @ immutable = fun f ->
      let input : {f : forest | valid_forest f} = f in
      let out = cons p input in use (out) in
    compile_work goal rest resume

let compile : (env : E.env) @ immutable ->
    {f : forest | valid_forest f && flatten f === env} @ immutable = fun env ->
  let goal = {Ghost.ghost = ghost_ env} in
  let use : (f : {f : forest | valid_forest f && flatten f === env}) @ immutable ->
      {f : forest | valid_forest f && flatten f === goal.Ghost.ghost} @ immutable = fun f ->
    f in
  let out = compile_work goal env use in out
