let[@def] rec size (xs : Bigint.t list) =
  match xs with [] -> 0Z | _ :: tail -> Bigint.add 1Z (size tail)
let[@def] rec mem (x : Bigint.t) (xs : Bigint.t list) =
  match xs with [] -> false | h :: tail -> Bigint.equal x h || mem x tail
let[@def] rec insert (x : Bigint.t) (xs : Bigint.t list) =
  match xs with
  | [] -> [x]
  | h :: tail ->
      if x < h then x :: xs
      else if x <= h then xs
      else h :: insert x tail
let[@def] rec range (low : Bigint.t) (high : Bigint.t) (xs : Bigint.t list) =
  match xs with [] -> true | h :: tail ->
    low <= h && h < high && range (Bigint.add h 1Z) high tail

let rec (range_absent @ total) : (low : Bigint.t) -> (high : Bigint.t) ->
    (xs : Bigint.t list) -> (x : Bigint.t) ->
    {u : unit | if range low high xs && (x < low || x >= high)
      then not (mem x xs) else true} = fun low high xs x ->
  range_def low high xs; mem_def x xs;
  (match xs with [] -> () | h :: tail ->
    range_absent (Bigint.add h 1Z) high tail x);
  let u = () in u

let rec (insert_size @ total) : (low : Bigint.t) -> (high : Bigint.t) ->
    (x : Bigint.t) -> (xs : Bigint.t list) ->
    {u : unit | if range low high xs then size (insert x xs) =
      Bigint.add (size xs) (if mem x xs then 0Z else 1Z) else true} = fun low high x xs ->
  range_def low high xs;
  insert_def x xs; size_def xs; mem_def x xs;
  (match xs with
  | [] -> size_def [x]; size_def []
  | h :: tail ->
    if x < h then (
      size_def (x :: xs);
      range_def h high xs; range_absent h high xs x)
    else if x <= h then ()
    else (
      insert_size (Bigint.add h 1Z) high x tail;
      size_def (h :: insert x tail)));
  let u = () in u

let rec (insert_range @ total) : (low : Bigint.t) -> (high : Bigint.t) ->
    (xs : Bigint.t list) -> (x : Bigint.t) ->
    {u : unit | if range low high xs && low <= x && x < high then
      range low high (insert x xs) else true} = fun low high xs x ->
  range_def low high xs; insert_def x xs;
  (match xs with
  | [] -> range_def low high [x]; range_def (Bigint.add x 1Z) high []
  | h :: tail ->
      if x < h then (
        range_def low high (x :: xs);
        range_def (Bigint.add x 1Z) high xs)
      else if x <= h then ()
      else (
        insert_range (Bigint.add h 1Z) high tail x;
        range_def low high (h :: insert x tail)));
  let u = () in u

let rec (range_size @ total) : (low : Bigint.t) -> (high : Bigint.t) ->
    (xs : Bigint.t list) ->
    {u : unit | if low <= high && range low high xs then
      0Z <= size xs && size xs <= Bigint.sub high low else true} =
    fun low high xs ->
  range_def low high xs; size_def xs;
  (match xs with [] -> () | h :: tail ->
    range_size (Bigint.add h 1Z) high tail);
  let u = () in u

let rec (insert_mem @ total) : (x : Bigint.t) -> (xs : Bigint.t list) ->
    (target : Bigint.t) ->
    {u : unit | mem target (insert x xs) =
      (Bigint.equal target x || mem target xs)} = fun x xs target ->
  insert_def x xs; mem_def target xs;
  (match xs with
  | [] -> mem_def target [x]; mem_def target []
  | h :: tail ->
      if x < h then mem_def target (x :: xs)
      else if x <= h then ()
      else (
        insert_mem x tail target;
        mem_def target (h :: insert x tail)));
  let u = () in u
