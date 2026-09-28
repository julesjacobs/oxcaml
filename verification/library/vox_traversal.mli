open Vox_sequence

val map_rel : ('a : immutable_data) ('b : immutable_data).
  ('a @ immutable total -> 'b @ immutable total -> bool) @ total ->
  'a list @ immutable total -> 'b list @ immutable total -> bool @@ total

val map_rel_def : ('a : immutable_data) ('b : immutable_data).
  (r : ('a @ immutable total -> 'b @ immutable total -> bool)) @ total ->
  (xs : 'a list) @ immutable -> (ys : 'b list) @ immutable ->
  {u : unit | map_rel r xs ys ===
    (match xs with
     | [] -> (match ys with [] -> true | _ :: _ -> false)
     | x :: tail ->
       match ys with
       | [] -> false
       | y :: outputs -> r x y && map_rel r tail outputs)} @@ total

val fold_right_ih :
    (r : (('a : immutable_data) list @ immutable total ->
      ('b : immutable_data) @ immutable total -> bool)) @ total ->
    ((x : 'a) @ immutable ->
      (xs : 'a list) @ immutable ghost ->
      (acc : 'b) @ immutable ->
      {u : unit | r xs acc} @ ghost ->
      {result : 'b | r (x :: xs) result} @ immutable total) ->
    (xs : 'a list) @ immutable -> (initial : 'b) @ immutable ->
    {u : unit | r [] initial} @ ghost ->
    {result : 'b | r xs result} @ immutable total

val map_ih :
    (r : (('a : immutable_data) list @ immutable total ->
      ('b : immutable_data) list @ immutable total -> bool)) @ total ->
    ((x : 'a) @ immutable ->
      (xs : 'a list) @ immutable ghost ->
      (ys : 'b list) @ immutable ghost ->
      {u : unit | r xs ys} @ ghost ->
      {y : 'b | r (x :: xs) (y :: ys)} @ immutable total) ->
    (xs : 'a list) @ immutable ->
    {u : unit | r [] []} @ ghost ->
    {ys : 'b list | r xs ys} @ immutable total

val map :
    (r : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total -> bool)) @ total ->
    ((x : 'a) @ immutable -> {y : 'b | r x y} @ immutable total) ->
    (xs : 'a list) @ immutable ->
    {ys : 'b list | map_rel r xs ys} @ immutable total

val map_length :
    (r : (('a : immutable_data) @ immutable total ->
      ('b : immutable_data) @ immutable total -> bool)) @ total ->
    (xs : 'a list) @ immutable -> (ys : 'b list) @ immutable ->
    {u : unit | if map_rel r xs ys then length xs === length ys else true}
    @@ total

val fold_right :
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
    {result : 'b | inv xs result} @ immutable total
