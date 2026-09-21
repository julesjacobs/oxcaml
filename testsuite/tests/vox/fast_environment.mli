module E := Hm_environment_spec

module D := Hm_declarative

type tree =
    Leaf of Copy_spec.node Pref.t
  | Branch of int * Copy_spec.node Pref.t * tree * tree
[@@inductive]

type forest = Nil | Cons of tree * forest
[@@inductive]

val weight : tree @ immutable -> int
  @@ total

val weight_def :
  (t : tree) @ immutable ->
  {u : unit
    | (weight t) === (match t with | Leaf _ -> 1 | Branch (n, _, _, _) -> n)}
  @@ total

val valid_tree : tree @ immutable -> bool
  @@ total

val valid_tree_def :
  (t : tree) @ immutable ->
  {u : unit
    | (valid_tree t) ===
        (match t with
         | Leaf _ -> true
         | Branch (n, _, a, b) ->
             (valid_tree a) &&
               ((valid_tree b) &&
                  (((weight a) = (weight b)) &&
                     (((weight a) > 0) &&
                        ((n > (weight a)) &&
                           (n = ((1 + (weight a)) + (weight b))))))))}
  @@ total

val valid_forest : forest @ immutable -> bool
  @@ total

val valid_forest_def :
  (f : forest) @ immutable ->
  {u : unit
    | (valid_forest f) ===
        (match f with
         | Nil -> true
         | Cons (t, rest) -> (valid_tree t) && (valid_forest rest))}
  @@ total

val append : E.env @ immutable -> E.env @ immutable -> E.env
  @@ total

val append_def :
  (a : E.env) @ immutable ->
  (b : E.env) @ immutable ->
  {u : unit
    | (append a b) ===
        (match a with
         | E.Empty -> b
         | E.Bind (p, rest) -> E.Bind (p, (append rest b)))}
  @@ total

val flatten_tree : tree @ immutable -> E.env
  @@ total

val flatten_tree_def :
  (t : tree) @ immutable ->
  {u : unit
    | (flatten_tree t) ===
        (match t with
         | Leaf p' -> E.Bind (p', E.Empty)
         | Branch (_, p, a, b) ->
             E.Bind (p, (append (flatten_tree a) (flatten_tree b))))}
  @@ total

val flatten : forest @ immutable -> E.env
  @@ total

val flatten_def :
  (f : forest) @ immutable ->
  {u : unit
    | (flatten f) ===
        (match f with
         | Nil -> E.Empty
         | Cons (t, rest) -> append (flatten_tree t) (flatten rest))}
  @@ total

val at : E.env @ immutable -> int -> Copy_spec.node Pref.t option
  @@ total

val at_def :
  (env : E.env) @ immutable ->
  (i : int) ->
  {u : unit
    | (at env i) ===
        (match env with
         | E.Empty -> None
         | E.Bind (p, rest) ->
             if i = 0
             then Some p
             else if i > 0 then at rest (i - 1) else None)}
  @@ total

val sized : E.env @ immutable -> int -> bool
  @@ total

val sized_def :
  (env : E.env) @ immutable ->
  (n : int) ->
  {u : unit
    | (sized env n) ===
        (match env with
         | E.Empty -> n = 0
         | E.Bind (_, rest) -> (n > 0) && (sized rest (n - 1)))}
  @@ total

val encoded : D.index @ immutable -> int -> bool
  @@ total

val encoded_def :
  (i' : D.index) @ immutable ->
  (n : int) ->
  {u : unit
    | (encoded i' n) ===
        (match i' with
         | D.Z -> n = 0
         | D.S i -> (n > 0) && (encoded i (n - 1)))}
  @@ total

val lookup_encoded :
  (env : E.env) @ immutable ->
  ((index : D.index) @ immutable ->
   (n : int) ->
   {u : unit
     | (not (encoded index n)) || ((at env n) === (E.lookup env index))} @ ghost) @ total
  @@ total

val cons :
  (p : Copy_spec.node Pref.t) @ immutable ->
  (f : {f : forest | valid_forest f}) @ immutable ->
  {out : forest
    | (valid_forest out) && ((flatten out) === (E.Bind (p, (flatten f))))} @ immutable

val lookup :
  (f : forest) @ immutable ->
  (index : int) ->
  {u : unit | (valid_forest f) && (index >= 0)} Ghost.t ->
  {p : Copy_spec.node Pref.t option | p === (at (flatten f) index)} @ immutable

val compile :
  (env : E.env) @ immutable ->
  {f : forest | (valid_forest f) && ((flatten f) === env)} @ immutable
