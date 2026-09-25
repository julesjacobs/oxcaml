let[@def] rec (find @ total) : ('a : value mod separable).
    int -> (int * 'a) list @ total -> 'a option @ total =
  fun index bindings -> match bindings with
  | [] -> None
  | (key, value) :: rest ->
    if index = key then Some value else find index rest

let[@def] rec (remove @ total) : ('a : value mod separable).
    int -> (int * 'a) list @ total -> (int * 'a) list @ total =
  fun index bindings -> match bindings with
  | [] -> []
  | (key, value) :: rest ->
    if index = key then remove index rest
    else (key, value) :: remove index rest

let rec (find_remove @ total) : ('a : immutable_data).
    (bindings : (int * 'a) list) -> (index : int) -> (query : int) ->
    {u : unit | find query (remove index bindings) ===
      (if query = index then None else find query bindings)} =
  fun bindings index query ->
  remove_def index bindings;
  find_def query bindings;
  let remaining = remove index bindings in
  find_def query remaining;
  let u = () in
  match bindings with
  | [] -> u
  | (key, value) :: rest ->
    find_remove rest index query;
    u

type ('a : value mod separable) t : immutable_data with 'a =
  { base : 'a iarray; updates : (int * 'a) list }

let[@def] (base @ total) : ('a : immutable_data).
    'a t @ immutable -> 'a iarray @ immutable total =
  fun overlay -> overlay.base
let[@def] (length @ total) (overlay : 'a t @ immutable) =
  Iarray.length overlay.base
let (length_known @ total) (overlay : 'a t @ immutable) :
    {n : int | n = length overlay && n = Iarray.length overlay.base} =
  let n = Iarray.length overlay.base in
  ghost_ (length_def overlay);
  n

let[@def] (empty @ total) : ('a : value mod separable).
    'a iarray @ total -> 'a t @ total =
  fun values -> {base = values; updates = []}

let[@def] (get_raw @ total) : ('a : value mod separable).
    (overlay : 'a t) ->
    {i : int | 0 <= i && i < Iarray.length overlay.base} -> 'a @ total =
  fun overlay index ->
  let i = index in
  match find i overlay.updates with
  | Some value -> value
  | None -> Iarray.Refined.get overlay.base (i)

let[@def] (lookup @ total) : ('a : value mod separable).
    int -> 'a t @ total -> 'a option @ total =
  fun index overlay ->
  if 0 <= index && index < Iarray.length overlay.base then
    let bounded : {i : int | 0 <= i && i < Iarray.length overlay.base} =
      index in
    Some (get_raw overlay bounded)
  else None

let[@def] (get @ total) : ('a : value mod separable).
    (overlay : 'a t) ->
    (index : {i : int | 0 <= i && i < length overlay}) -> 'a @ total =
  fun overlay index ->
  let i = index in
  let size = length_known overlay in
  get_raw overlay (i)

let[@def] (set @ total) : ('a : value mod separable).
    int -> 'a @ total -> 'a t @ total -> 'a t @ total =
  fun index value overlay ->
  let rest = remove index overlay.updates in
  {overlay with updates = (index, value) :: rest}

let[@def] (clear @ total) : ('a : value mod separable).
    int -> 'a t @ total -> 'a t @ total =
  fun index overlay ->
  let updates = remove index overlay.updates in
  {overlay with updates}

module Laws (Element : sig type t : immutable_data end) = struct
  let (length_equation @ total) : (overlay : Element.t t) ->
    {u : unit | length overlay = Iarray.length (base overlay)} =
    fun overlay -> length_def overlay; base_def overlay;
    let u = () in u

  let (get_lookup @ total) : (overlay : Element.t t) ->
    (index : {i : int | 0 <= i && i < length overlay}) ->
    {u : unit | let i = index in
      lookup i overlay === Some (get overlay index)} =
    fun overlay index ->
    let i = index in
    length_def overlay; get_def overlay index; lookup_def i overlay;
    let u = () in u

  let (lookup_outside @ total) : (overlay : Element.t t) -> (index : int) ->
    {u : unit | if index < 0 || length overlay <= index then
      lookup index overlay === None else true} =
    fun overlay index ->
    length_def overlay; lookup_def index overlay;
    let u = () in u

  let (empty_base @ total) : (values : Element.t iarray) ->
    {u : unit | base (empty values) === values} =
    fun values ->
    empty_def values;
    let overlay = empty values in base_def overlay;
    let u = () in u

  let (empty_lookup @ total) : (values : Element.t iarray) -> (index : int) ->
    {u : unit | lookup index (empty values) ===
      Vox_iarray.at values index} =
    fun values index ->
    empty_def values;
    let overlay = empty values in
    lookup_def index overlay;
    Vox_iarray.at_outside values index;
    if 0 <= index && index < Iarray.length values then (
      let bounded : {i : int | 0 <= i && i < Iarray.length overlay.base} =
        index in
      get_raw_def overlay bounded;
      find_def index overlay.updates;
      Vox_iarray.at_get values bounded;
      let u = () in u)
    else let u = () in u

  let (set_base @ total) : (overlay : Element.t t) -> (index : int) ->
    (value : Element.t) ->
    {u : unit | base (set index value overlay) === base overlay} =
    fun overlay index value ->
    set_def index value overlay;
    let updated = set index value overlay in
    base_def overlay; base_def updated;
    let u = () in u

  let (set_lookup @ total) : (overlay : Element.t t) -> (index : int) ->
    (value : Element.t) -> (query : int) ->
    {u : unit | lookup query (set index value overlay) ===
      (if 0 <= query && query < length overlay && query = index then
        Some value else lookup query overlay)} =
    fun overlay index value query ->
    set_def index value overlay;
    let updated = set index value overlay in
    length_def overlay;
    lookup_def query updated; lookup_def query overlay;
    if 0 <= query && query < Iarray.length overlay.base then (
      let bounded : {i : int | 0 <= i && i < Iarray.length overlay.base} =
        query in
      let changed : {i : int | 0 <= i && i < Iarray.length updated.base} =
        query in
      get_raw_def overlay bounded; get_raw_def updated changed;
      find_def query updated.updates;
      find_remove overlay.updates index query;
      let u = () in u)
    else let u = () in u

  let (clear_base @ total) : (overlay : Element.t t) -> (index : int) ->
    {u : unit | base (clear index overlay) === base overlay} =
    fun overlay index ->
    clear_def index overlay;
    let updated = clear index overlay in
    base_def overlay; base_def updated;
    let u = () in u

  let (clear_lookup @ total) : (overlay : Element.t t) -> (index : int) ->
    (query : int) ->
    {u : unit | lookup query (clear index overlay) ===
      (if query = index then Vox_iarray.at (base overlay) query
       else lookup query overlay)} =
    fun overlay index query ->
    clear_def index overlay;
    base_def overlay;
    let values = base overlay in
    let updated = clear index overlay in
    lookup_def query updated; lookup_def query overlay;
    Vox_iarray.at_outside values query;
    if 0 <= query && query < Iarray.length overlay.base then (
      let bounded : {i : int | 0 <= i && i < Iarray.length overlay.base} =
        query in
      let changed : {i : int | 0 <= i && i < Iarray.length updated.base} =
        query in
      get_raw_def overlay bounded; get_raw_def updated changed;
      find_remove overlay.updates index query;
      Vox_iarray.at_get values bounded;
      let u = () in u)
    else let u = () in u

end
