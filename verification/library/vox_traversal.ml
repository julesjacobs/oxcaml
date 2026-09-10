open Vox_sequence

let rec fold_right_ih :
    (r : (('a : immutable_data) list @ immutable total ->
      ('b : immutable_data) @ immutable total -> bool)) @ total ->
    ((x : 'a) @ immutable ->
      (xs : 'a list) @ immutable ghost ->
      (acc : 'b) @ immutable ->
      {u : unit | r xs acc} @ ghost ->
      {result : 'b | r (x :: xs) result} @ immutable total) ->
    (xs : 'a list) @ immutable -> (initial : 'b) @ immutable ->
    {u : unit | r [] initial} @ ghost ->
    {result : 'b | r xs result} @ immutable total =
    fun r f xs initial base ->
  let refine_ base = base in
  match xs with
  | [] -> refine_ initial
  | x :: tail ->
    let u = () in
    let refine_ acc = fold_right_ih r f tail initial (refine_ u) in
    let model = ghost_ tail in
    let refine_ result = f x model acc (refine_ u) in
    refine_ result

let rec map_ih :
    (r : (('a : immutable_data) list @ immutable total ->
      ('b : immutable_data) list @ immutable total -> bool)) @ total ->
    ((x : 'a) @ immutable ->
      (xs : 'a list) @ immutable ghost ->
      (ys : 'b list) @ immutable ghost ->
      {u : unit | r xs ys} @ ghost ->
      {y : 'b | r (x :: xs) (y :: ys)} @ immutable total) ->
    (xs : 'a list) @ immutable ->
    {u : unit | r [] []} @ ghost ->
    {ys : 'b list | r xs ys} @ immutable total = fun r f xs base ->
  let refine_ base = base in
  match xs with
  | [] -> let ys = [] in refine_ ys
  | x :: tail ->
    let u = () in
    let refine_ ys = map_ih r f tail (refine_ u) in
    let tail_model = ghost_ tail in
    let output_model = ghost_ ys in
    let refine_ y = f x tail_model output_model (refine_ u) in
    let result = y :: ys in
    refine_ result

let[@def] rec map_rel
    (r : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total -> bool) @ total)
    (xs : 'a list @ immutable total) (ys : 'b list @ immutable total) =
  match xs with
  | [] -> (match ys with [] -> true | _ :: _ -> false)
  | x :: xs ->
    match ys with
    | [] -> false
    | y :: ys -> r x y && map_rel r xs ys

let rec map :
    (r : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total -> bool)) @ total ->
    ((x : 'a) @ immutable -> {y : 'b | r x y} @ immutable total) ->
    (xs : 'a list) @ immutable ->
    {ys : 'b list | map_rel r xs ys} @ immutable total = fun r f xs ->
  match xs with
  | [] ->
    let ys = [] in
    ghost_ (map_rel_def r xs ys);
    refine_ ys
  | x :: tail ->
    let refine_ y = f x in
    let refine_ ys = map r f tail in
    let result = y :: ys in
    ghost_ (map_rel_def r xs result);
    refine_ result

let rec (map_length @ total) :
    (r : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total -> bool)) @ total ->
    (xs : 'a list) @ immutable -> (ys : 'b list) @ immutable ->
    {u : unit | if map_rel r xs ys then length xs === length ys else true} =
    fun r xs ys ->
  map_rel_def r xs ys;
  length_def xs;
  length_def ys;
  match xs with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    match ys with
    | [] -> let u = () in refine_ u
    | _ :: outputs ->
      map_length r tail outputs;
      let u = () in refine_ u

let rec fold_right :
    (r : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total ->
      'b @ immutable total -> bool)) @ total ->
    (inv : ('a list @ immutable total ->
      'b @ immutable total -> bool)) @ total ->
    ((x : 'a) @ immutable -> (acc : 'b) @ immutable ->
      {result : 'b | r x acc result} @ immutable total) ->
    ((x : 'a) @ immutable -> (tail : 'a list) @ immutable ->
      (acc : 'b) @ immutable -> (result : 'b) @ immutable ->
      {u : unit | inv tail acc && r x acc result} ->
      {u : unit | inv (x :: tail) result}) @ total ghost ->
    (xs : 'a list) @ immutable -> (initial : 'b) @ immutable ->
    {u : unit | inv [] initial} @ ghost ->
    {result : 'b | inv xs result} @ immutable total =
    fun r inv f preserve xs initial base ->
  let refine_ base = base in
  match xs with
  | [] -> refine_ initial
  | x :: tail ->
    let u = () in
    let refine_ acc = fold_right r inv f preserve tail initial (refine_ u) in
    let refine_ result = f x acc in
    ghost_ (preserve x tail acc result (refine_ u));
    refine_ result
