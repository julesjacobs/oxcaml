let[@def] rec height (size : Bigint.t) =
  if size <= 1Z then 0Z
  else Bigint.add 1Z (height (Bigint.div (Bigint.add size 1Z) 2Z))
[@@decreases let size : Bigint.t = size in
  if size > 0Z then size else 0Z]

let[@def] rec power (depth : Bigint.t) =
  if depth <= 0Z then 1Z
  else Bigint.mul 2Z (power (Bigint.sub depth 1Z))
[@@decreases let depth : Bigint.t = depth in
  if depth > 0Z then depth else 0Z]

let[@def] budget (size : Bigint.t) = Bigint.mul size (height size)

let rec (height_bound @ total) : (size : Bigint.t) ->
    {u : unit | 0Z <= height size && size <= power (height size)} =
    fun size ->
  height_def size;
  let depth = height size in
  power_def depth;
  if size > 1Z then (
    let smaller = Bigint.div (Bigint.add size 1Z) 2Z in
    height_bound smaller;
    ())
  else ()
[@@decreases let size : Bigint.t = size in
  if size > 0Z then size else 0Z]

let rec (bounded_int @ total) : (bound : int) ->
    (amount : {n : Bigint.t | 0Z <= n && n <= Bigint.of_int bound}) ->
    {n : int | let amount = amount in
      Bigint.of_int n = amount && 0 <= n && n <= bound} @ ghost =
    fun bound amount -> ghost_ (
  let amount = amount in
  if amount <= 0Z then (let result = 0 in result)
  else (
    let next_bound = bound - 1 in
    let smaller = Bigint.sub amount 1Z in
    let next : {n : Bigint.t | 0Z <= n &&
      n <= Bigint.of_int next_bound} = smaller in
    let previous = bounded_int next_bound next in
    let result = previous + 1 in
    result))
[@@decreases bound]

let rec (height_minimal @ total) : (size : Bigint.t) ->
    {u : unit | if size > 1Z then
      0Z < height size && power (Bigint.sub (height size) 1Z) < size
      else height size = 0Z} = fun size ->
  height_def size;
  if size > 1Z then (
    let smaller = Bigint.div (Bigint.add size 1Z) 2Z in
    height_bound smaller;
    height_minimal smaller;
    let depth = height smaller in
    power_def depth;
    if smaller <= 1Z then height_def smaller;
    ())
  else ()
[@@decreases let size : Bigint.t = size in
  if size > 0Z then size else 0Z]
