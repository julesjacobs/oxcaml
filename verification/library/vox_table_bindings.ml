module Make (Key : Vox_table_map.Key) = struct
  type ('a : immutable_data) t = (Key.t * 'a) list

  let[@def] rec (lookup @ total) (bindings : 'a t @ immutable)
      (key : Key.t @ immutable) =
    match bindings with
    | [] -> None
    | (stored, value) :: tail ->
      if Key.equal stored key then Some value else lookup tail key

  let[@def] rec (distinct @ total) (bindings : 'a t @ immutable) = ghost_ (
    match bindings with
    | [] -> true
    | (key, _) :: tail -> lookup tail key === None && distinct tail)

  let[@def] rec (agrees @ total) (left : 'a t @ immutable)
      (right : 'a t @ immutable) = ghost_ (
    match left with
    | [] -> true
    | (key, value) :: tail ->
      lookup right key === Some value && agrees tail right)

  let[@def] (same @ total) (left : 'a t @ immutable)
      (right : 'a t @ immutable) = ghost_ (
    agrees left right && agrees right left)

  let[@def] rec (erase @ total) (bindings : 'a t @ immutable)
      (key : Key.t @ immutable) =
    match bindings with
    | [] -> []
    | (stored, value) :: tail ->
      if Key.equal stored key then erase tail key
      else (stored, value) :: erase tail key

  let[@def] (put @ total) (bindings : 'a t @ immutable)
      (key : Key.t @ immutable) (value : 'a @ immutable) =
    (key, value) :: erase bindings key

  let rec (lookup_congruent @ total) : ('a : immutable_data).
      (bindings : 'a t) @ immutable -> (left : Key.t) @ immutable ->
      (right : Key.t) @ immutable ->
      {u : unit | not (Key.equal left right) ||
        lookup bindings left === lookup bindings right} @ ghost =
    fun bindings left right -> ghost_ (
      lookup_def bindings left; lookup_def bindings right;
      match bindings with
      | [] -> ()
      | (stored, _) :: tail ->
        Key.symmetric left right;
        Key.transitive stored left right;
        Key.transitive stored right left;
        lookup_congruent tail left right)

  let rec (agrees_get @ total) : ('a : immutable_data).
      (left : 'a t) @ immutable -> (right : 'a t) @ immutable ->
      (key : Key.t) @ immutable ->
      {u : unit | not (agrees left right) || lookup left key === None ||
        lookup left key === lookup right key} @ ghost =
    fun left right key -> ghost_ (
      agrees_def left right; lookup_def left key;
      match left with
      | [] -> ()
      | (stored, _) :: tail ->
        lookup_congruent right stored key; agrees_get tail right key)

  let (same_get @ total) : ('a : immutable_data).
      (left : 'a t) @ immutable -> (right : 'a t) @ immutable ->
      (key : Key.t) @ immutable ->
      {u : unit | not (same left right) ||
        lookup left key === lookup right key} @ ghost =
    fun left right key -> ghost_ (
      same_def left right; agrees_get left right key;
      agrees_get right left key; ())

  let rec (erase_get @ total) : ('a : immutable_data).
      (bindings : 'a t) @ immutable -> (key : Key.t) @ immutable ->
      (query : Key.t) @ immutable ->
      {u : unit | lookup (erase bindings key) query ===
        (if Key.equal key query then None else lookup bindings query)} @ ghost =
    fun bindings key query -> ghost_ (
      erase_def bindings key; lookup_def bindings query;
      lookup_def (erase bindings key) query;
      match bindings with
      | [] -> ()
      | (stored, _) :: tail ->
        erase_get tail key query;
        Key.symmetric key query; Key.symmetric stored key;
        Key.transitive stored key query; Key.transitive key stored query;
        Key.transitive stored query key; ())

  let (put_get @ total) : ('a : immutable_data).
      (bindings : 'a t) @ immutable -> (key : Key.t) @ immutable ->
      (value : 'a) @ immutable -> (query : Key.t) @ immutable ->
      {u : unit | lookup (put bindings key value) query ===
        (if Key.equal key query then Some value else lookup bindings query)} @ ghost =
    fun bindings key value query -> ghost_ (
      put_def bindings key value; lookup_def (put bindings key value) query;
      erase_get bindings key query; ())
end
