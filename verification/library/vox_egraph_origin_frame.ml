module I = Vox_iarray
module L = Vox_egraph_language_spec

let[@def] rec (preserved @ total)
    (before : L.expr iarray @ immutable)
    (after : L.expr iarray @ immutable) (count : int) = ghost_ (
  if count <= 0 then true
  else I.at before (count - 1) === I.at after (count - 1) &&
    preserved before after (count - 1))
  [@@decreases if count > 0 then count else 0]

let rec (identity @ total) : (values : L.expr iarray) @ immutable ->
    (count : int) -> {u : unit | preserved values values count} @ ghost =
  fun values count -> ghost_ (
    preserved_def values values count;
    if count > 0 then identity values (count - 1);
    ())
  [@@decreases if count > 0 then count else 0]

let rec (compose @ total) :
    (before : L.expr iarray) @ immutable ->
    (middle : L.expr iarray) @ immutable ->
    (after : L.expr iarray) @ immutable -> (count : int) ->
    {u : unit | preserved before middle count &&
      preserved middle after count} ->
    {u : unit | preserved before after count} @ ghost =
  fun before middle after count premise -> ghost_ (
    preserved_def before middle count;
    preserved_def middle after count;
    preserved_def before after count;
    if count > 0 then compose before middle after (count - 1) ();
    ())
  [@@decreases if count > 0 then count else 0]

let rec (at @ total) :
    (before : L.expr iarray) @ immutable ->
    (after : L.expr iarray) @ immutable -> (count : int) -> (id : int) ->
    {u : unit | preserved before after count && 0 <= id && id < count} ->
    {u : unit | I.at before id === I.at after id} @ ghost =
  fun before after count id premise -> ghost_ (
    preserved_def before after count;
    if id < count - 1 then at before after (count - 1) id ();
    ())
  [@@decreases if count > 0 then count else 0]

let rec (weaken @ total) :
    (before : L.expr iarray) @ immutable ->
    (after : L.expr iarray) @ immutable ->
    (count : int) -> (smaller : int) ->
    {u : unit | preserved before after count && 0 <= smaller &&
      smaller <= count} ->
    {u : unit | preserved before after smaller} @ ghost =
  fun before after count smaller premise -> ghost_ (
    preserved_def before after count;
    if smaller < count then weaken before after (count - 1) smaller ();
    ())
  [@@decreases if count > 0 then count else 0]

let rec (append @ total) :
    (before : L.expr iarray) @ immutable -> (count : int) ->
    (index : int) -> (expr : L.expr) @ immutable ->
    {u : unit | count <= index} ->
    {u : unit | preserved before (I.updated before index expr) count}
    @ ghost = fun before count index expr premise -> ghost_ (
  preserved_def before (I.updated before index expr) count;
  if count > 0 then (
    I.updated_read before index expr (count - 1);
    append before (count - 1) index expr ());
  ())
  [@@decreases if count > 0 then count else 0]
