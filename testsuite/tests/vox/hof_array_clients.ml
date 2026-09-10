let modify_at :
    (model : (int @ immutable total ->
      int @ immutable total -> int @ immutable total)) @ total ->
    ((index : int) @ immutable -> (x : int) @ immutable ->
      {y : int | y === model index x} @ immutable total) ->
    (values : int iarray) @ immutable ->
    (index : {i : int | 0 <= i && i < Iarray.length values}) ->
    {result : int iarray | let refine_ i = index in
      result === Vox_iarray.updated values i
        (model i (Vox_iarray.get values index))} @ immutable total =
    fun model f values index ->
  let refine_ i = index in
  let x = Vox_iarray.get values index in
  let refine_ y = f i x in
  let result = Vox_iarray.updated values i y in
  refine_ result

let[@def] indexed_add : int @ immutable total ->
    (int @ immutable -> int @ immutable total) @ total = fun i x -> i + x

let partial_add : (i : int) @ immutable -> (x : int) @ immutable ->
    {y : int | y === indexed_add i x} @ immutable total = fun i x ->
  if x = 13 then failwith "array callback"
  else
    let y = i + x in
    ghost_ (indexed_add_def i x);
    refine_ y

let () =
  let values = [: 4; 8; 2 :] in
  let i = 1 in
  let index : {i : int | 0 <= i && i < Iarray.length values} = refine_ i in
  let f = partial_add in
  let refine_ result = modify_at indexed_add f values index in
  let (result : int iarray) = result in
  assert (Iarray.to_list result = [4; 9; 2])
