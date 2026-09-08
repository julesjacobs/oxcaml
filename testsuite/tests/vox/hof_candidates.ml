open Vox_sequence

let[@def] rec model_map
    (model : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total) @ total)
    (xs : 'a list @ immutable total) =
  match xs with [] -> [] | x :: tail -> model x :: model_map model tail

let rec map_model :
    (model : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total)) @ total ->
    ((x : 'a) @ immutable ->
      {y : 'b | y === model x} @ immutable total) ->
    (xs : 'a list) @ immutable ->
    {ys : 'b list | ys === model_map model xs} @ immutable total =
    fun model f xs ->
  ghost_ (model_map_def model xs);
  match xs with
  | [] -> let ys = [] in refine_ ys
  | x :: tail ->
    let refine_ y = f x in
    let refine_ ys = map_model model f tail in
    let result = y :: ys in refine_ result

let[@def] rec model_fold
    (model : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total -> 'b @ immutable total)
      @ total)
    (xs : 'a list @ immutable total) (initial : 'b @ immutable total) =
  match xs with
  | [] -> initial
  | x :: tail -> model x (model_fold model tail initial)

let rec fold_model :
    (model : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total -> 'b @ immutable total))
      @ total ->
    ((x : 'a) @ immutable -> (acc : 'b) @ immutable ->
      {result : 'b | result === model x acc} @ immutable total) ->
    (xs : 'a list) @ immutable -> (initial : 'b) @ immutable ->
    {result : 'b | result === model_fold model xs initial}
      @ immutable total = fun model f xs initial ->
  ghost_ (model_fold_def model xs initial);
  match xs with
  | [] -> refine_ initial
  | x :: tail ->
    let refine_ acc = fold_model model f tail initial in
    let refine_ result = f x acc in refine_ result

type ('a : immutable_data) traced =
  { value : 'a;
    trace : 'a list @@ ghost
  }

let[@def] rec valid_trace
    (r : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total ->
      'b @ immutable total -> bool) @ total)
    (xs : 'a list @ immutable total) (initial : 'b @ immutable total)
    (result : 'b @ immutable total) (trace : 'b list @ immutable total)
    : bool @ ghost =
  ghost_ (
  match xs with
  | [] -> (match trace with [] -> result === initial | _ :: _ -> false)
  | x :: tail ->
    match trace with
    | [] -> false
    | acc :: rest -> r x acc result && valid_trace r tail initial acc rest)

let rec fold_trace :
    (r : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total ->
      'b @ immutable total -> bool)) @ total ->
    ((x : 'a) @ immutable -> (acc : 'b) @ immutable ->
      {result : 'b | r x acc result} @ immutable total) ->
    (xs : 'a list) @ immutable -> (initial : 'b) @ immutable ->
    {result : 'b traced |
      valid_trace r xs initial result.value result.trace} @ immutable total =
    fun r f xs initial ->
  match xs with
  | [] ->
    let result = { value = initial; trace = ghost_ [] } in
    let trace = ghost_ result.trace in
    ghost_
      (valid_trace_def r xs initial initial trace);
    refine_ result
  | x :: tail ->
    let refine_ rest = fold_trace r f tail initial in
    let acc = rest.value in
    let refine_ value = f x acc in
    let result = { value; trace = ghost_ (acc :: rest.trace) } in
    let trace = ghost_ result.trace in
    ghost_
      (valid_trace_def r xs initial value trace);
    refine_ result

let rec (trace_invariant @ total) :
    (r : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total ->
      'b @ immutable total -> bool)) @ total ->
    (inv : ('a list @ immutable total ->
      'b @ immutable total -> bool)) @ total ->
    ((x : 'a) @ immutable -> (tail : 'a list) @ immutable ->
      (acc : 'b) @ immutable -> (result : 'b) @ immutable ->
      {u : unit | inv tail acc && r x acc result} ->
      {u : unit | inv (x :: tail) result}) @ total ->
    (xs : 'a list) @ immutable -> (initial : 'b) @ immutable ->
    (result : 'b) @ immutable -> (trace : 'b list) @ immutable ->
    {u : unit | inv [] initial && valid_trace r xs initial result trace} ->
    {u : unit | inv xs result} @ ghost =
    fun r inv preserve xs initial result trace premise ->
  ghost_ (
  premise;
  valid_trace_def r xs initial result trace;
  match xs with
  | [] -> let u = () in refine_ u
  | x :: tail ->
    match trace with
    | [] -> let u = () in refine_ u
    | acc :: rest ->
      let u = () in
      trace_invariant r inv preserve tail initial acc rest (refine_ u);
      preserve x tail acc result (refine_ u);
      refine_ u)

let rec (model_invariant @ total) :
    (model : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total -> 'b @ immutable total))
      @ total ->
    (inv : ('a list @ immutable total ->
      'b @ immutable total -> bool)) @ total ->
    ((x : 'a) @ immutable -> (tail : 'a list) @ immutable ->
      (acc : 'b) @ immutable -> {u : unit | inv tail acc} ->
      {u : unit | inv (x :: tail) (model x acc)}) @ total ->
    (xs : 'a list) @ immutable -> (initial : 'b) @ immutable ->
    {u : unit | inv [] initial} ->
    {u : unit | inv xs (model_fold model xs initial)} =
    fun model inv preserve xs initial premise ->
  premise;
  model_fold_def model xs initial;
  match xs with
  | [] -> let u = () in refine_ u
  | x :: tail ->
    let u = () in
    model_invariant model inv preserve tail initial
      (refine_ u);
    let acc = model_fold model tail initial in
    preserve x tail acc (refine_ u);
    refine_ u

type ('a : immutable_data) control = Stop of 'a | Continue of 'a

let[@def] rec model_until
    (model : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total -> 'b control @ immutable total)
      @ total)
    (xs : 'a list @ immutable total) (initial : 'b @ immutable total) =
  match xs with
  | [] -> initial
  | x :: tail ->
    match model x initial with
    | Stop result -> result
    | Continue next -> model_until model tail next

let rec fold_until_model :
    (model : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total -> 'b control @ immutable total))
      @ total ->
    ((x : 'a) @ immutable -> (acc : 'b) @ immutable ->
      {result : 'b control | result === model x acc} @ immutable total) ->
    (xs : 'a list) @ immutable -> (initial : 'b) @ immutable ->
    {result : 'b | result === model_until model xs initial}
      @ immutable total = fun model f xs initial ->
  ghost_ (model_until_def model xs initial);
  match xs with
  | [] -> refine_ initial
  | x :: tail ->
    let refine_ step = f x initial in
    match step with
    | Stop result -> refine_ result
    | Continue next ->
      let refine_ result = fold_until_model model f tail next in
      refine_ result

let[@def] rec model_map_accum
    (model : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total ->
      ('b * ('c : immutable_data)) @ immutable total) @ total)
    (xs : 'a list @ immutable total) (initial : 'b @ immutable total) =
  match xs with
  | [] -> initial, []
  | x :: tail ->
    let acc, y = model x initial in
    let final, ys = model_map_accum model tail acc in
    final, y :: ys

let rec map_accum_model :
    (model : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total ->
      ('b * ('c : immutable_data)) @ immutable total)) @ total ->
    ((x : 'a) @ immutable -> (acc : 'b) @ immutable ->
      {result : 'b * 'c | result === model x acc} @ immutable total) ->
    (xs : 'a list) @ immutable -> (initial : 'b) @ immutable ->
    {result : 'b * 'c list | result === model_map_accum model xs initial}
      @ immutable total = fun model f xs initial ->
  ghost_ (model_map_accum_def model xs initial);
  match xs with
  | [] -> let result : 'b * 'c list = initial, [] in refine_ result
  | x :: tail ->
    let refine_ step = f x initial in
    let acc, y = step in
    let refine_ rest = map_accum_model model f tail acc in
    let final, ys = rest in
    let result = final, y :: ys in refine_ result

type ('a : immutable_data) tree = Leaf | Node of 'a * 'a tree * 'a tree
[@@inductive]

let[@def] rec tree_model
    (model : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total) @ total)
    (input : 'a tree @ immutable total) =
  match input with
  | Leaf -> Leaf
  | Node (x, left, right) ->
    Node (model x, tree_model model left, tree_model model right)

let rec tree_map :
    (model : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total)) @ total ->
    ((x : 'a) @ immutable ->
      {y : 'b | y === model x} @ immutable total) ->
    (input : 'a tree) @ immutable ->
    {output : 'b tree | output === tree_model model input}
      @ immutable total = fun model f input ->
  ghost_ (tree_model_def model input);
  match input with
  | Leaf -> let output : 'b tree = Leaf in refine_ output
  | Node (x, left, right) ->
    let refine_ y = f x in
    let refine_ left = tree_map model f left in
    let refine_ right = tree_map model f right in
    let output = Node (y, left, right) in refine_ output

let rec (iterate_down @ total) :
    ((x : {n : int | n > 0}) ->
      {next : int | let refine_ n = x in 0 <= next && next < n}) @ total ->
    (initial : {n : int | n >= 0}) -> {result : int | result = 0} =
    fun step initial ->
  let refine_ n = initial in
  if n = 0 then refine_ n
  else
    let input : {i : int | i > 0} = refine_ n in
    let refine_ next = step input in
    let next : {i : int | i >= 0} = refine_ next in
    let refine_ result = iterate_down step next in
    refine_ result
  [@@decreases let refine_ n = initial in n]

let[@def] rec all_inputs
    (p : (('a : immutable_data) @ immutable total -> bool) @ total)
    (xs : 'a list @ immutable total) =
  match xs with [] -> true | x :: tail -> p x && all_inputs p tail

let rec map_pre :
    (p : (('a : immutable_data) @ immutable total -> bool)) @ total ->
    (r : ('a @ immutable total ->
      ('b : immutable_data) @ immutable total -> bool)) @ total ->
    ((x : 'a) @ immutable -> {u : unit | p x} @ ghost ->
      {y : 'b | r x y} @ immutable total) ->
    (xs : 'a list) @ immutable -> {u : unit | all_inputs p xs} @ ghost ->
    {ys : 'b list | Vox_traversal.map_rel r xs ys} @ immutable total =
    fun p r f xs premise ->
  premise;
  ghost_ (all_inputs_def p xs);
  match xs with
  | [] ->
    let ys : 'b list = [] in
    ghost_ (Vox_traversal.map_rel_def r xs ys);
    refine_ ys
  | x :: tail ->
    let u = () in
    let refine_ y = f x (refine_ u) in
    let refine_ ys = map_pre p r f tail (refine_ u) in
    let result = y :: ys in
    ghost_ (Vox_traversal.map_rel_def r xs result);
    refine_ result
