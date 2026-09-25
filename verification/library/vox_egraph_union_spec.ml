module I = Vox_iarray

let[@def] (parent @ total) (parents : int iarray @ immutable) (id : int) =
  match I.at parents id with Some p -> p | None -> id

let[@def] rec (ordered @ total) (parents : int iarray @ immutable)
    (count : int) = ghost_ (
  if count <= 0 then true
  else
    let id = count - 1 in
    0 <= parent parents id && parent parents id <= id &&
    ordered parents id)
  [@@decreases if count > 0 then count else 0]

let[@def] rec (root @ total) (parents : int iarray @ immutable)
    (id : int) =
  let next = parent parents id in
  if 0 <= next && next < id then root parents next else id
  [@@decreases if id > 0 then id else 0]

let rec (ordered_index @ total) :
    (parents : int iarray) @ immutable ->
    (count : int) -> (id : int) ->
    {u : unit | ordered parents count && 0 <= id && id < count} ->
    {u : unit | 0 <= parent parents id && parent parents id <= id}
    @ ghost = fun parents count id premise -> ghost_ (
  ordered_def parents count;
  if id < count - 1 then ordered_index parents (count - 1) id ();
  ())
  [@@decreases if count > 0 then count else 0]

let rec (root_spec @ total) :
    (parents : int iarray) @ immutable ->
    (count : int) -> (id : int) ->
    {u : unit | ordered parents count && 0 <= id && id < count} ->
    {u : unit | 0 <= root parents id && root parents id <= id &&
      parent parents (root parents id) = root parents id}
    @ ghost = fun parents count id premise -> ghost_ (
  ordered_index parents count id ();
  root_def parents id;
  let next = parent parents id in
  if next < id then root_spec parents count next ();
  ())
  [@@decreases if id > 0 then id else 0]

let rec (frame @ total) :
    (parents : int iarray) @ immutable ->
    (count : int) -> (index : int) -> (target : int) ->
    {u : unit | 0 <= count && count <= index &&
      index < Iarray.length parents && ordered parents count} ->
    {u : unit | ordered (I.updated parents index target) count}
    @ ghost = fun parents count index target premise -> ghost_ (
  ordered_def parents count;
  ordered_def (I.updated parents index target) count;
  if count > 0 then (
    I.updated_read parents index target (count - 1);
    parent_def parents (count - 1);
    parent_def (I.updated parents index target) (count - 1);
    frame parents (count - 1) index target ());
  ())
  [@@decreases if count > 0 then count else 0]

let (append_preserves @ total) :
    (parents : int iarray) @ immutable -> (count : int) ->
    {u : unit | 0 <= count && count < Iarray.length parents &&
      ordered parents count} ->
    {u : unit | ordered (I.updated parents count count) (count + 1)}
    @ ghost = fun parents count premise -> ghost_ (
  let changed = I.updated parents count count in
  frame parents count count count ();
  I.updated_read parents count count count;
  parent_def changed count;
  ordered_def changed (count + 1);
  ())

let rec (link_frame @ total) :
    (parents : int iarray) @ immutable ->
    (count : int) -> (loser : int) -> (winner : int) ->
    {u : unit | 0 <= count && count <= Iarray.length parents &&
      0 <= winner && winner < loser && loser < count &&
      ordered parents count} ->
    {u : unit | ordered (I.updated parents loser winner) count}
    @ ghost = fun parents count loser winner premise -> ghost_ (
  ordered_def parents count;
  ordered_def (I.updated parents loser winner) count;
  if count > 0 then (
    I.updated_read parents loser winner (count - 1);
    parent_def parents (count - 1);
    parent_def (I.updated parents loser winner) (count - 1);
    if count - 1 <> loser then
      link_frame parents (count - 1) loser winner ()
    else frame parents loser loser winner ());
  ())
  [@@decreases if count > 0 then count else 0]

let rec (root_after_link @ total) :
    (parents : int iarray) @ immutable ->
    (count : int) -> (loser : int) -> (winner : int) -> (query : int) ->
    {u : unit | 0 <= count && count <= Iarray.length parents &&
      ordered parents count && 0 <= winner && winner < loser &&
      loser < count && parent parents loser = loser &&
      parent parents winner = winner &&
      0 <= query && query < count} ->
    {u : unit | root (I.updated parents loser winner) query =
      (if root parents query = loser then winner else root parents query)}
    @ ghost = fun parents count loser winner query premise -> ghost_ (
  let changed = I.updated parents loser winner in
  ordered_index parents count query ();
  I.updated_read parents loser winner query;
  parent_def parents query;
  parent_def changed query;
  root_def parents query;
  root_def changed query;
  let next = parent parents query in
  if query = loser then (
    root_def changed winner;
    I.updated_read parents loser winner winner;
    parent_def changed winner;
    parent_def parents winner;
    root_def parents loser)
  else if next < query then
    root_after_link parents count loser winner next ();
  ())
  [@@decreases if query > 0 then query else 0]
