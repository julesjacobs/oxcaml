module S = Vox_sequence

module type Key = sig
  type t : immutable_data
  val equal : t -> t -> bool @@ total
  val hash : t -> int @@ total
  val reflexive : (x : t) -> {u : unit | equal x x} @@ total
  val symmetric : (x : t) -> (y : t) -> {u : unit | equal x y = equal y x}
    @@ total
  val transitive : (x : t) -> (y : t) -> (z : t) ->
    {u : unit | not (equal x y && equal y z) || equal x z} @@ total
  val hash_equal : (x : t) -> (y : t) ->
    {u : unit | not (equal x y) || hash x = hash y} @@ total
end

module Make (Key : Key) = struct
  type ('a : immutable_data) slots = (Key.t * 'a) option list

  let[@def] rec (lookup @ total)
      (slots : 'a slots @ immutable) (key : Key.t @ immutable) =
    match slots with
    | [] -> None
    | None :: tail -> lookup tail key
    | Some (k, value) :: tail ->
      if Key.equal k key then Some value else lookup tail key

  let[@def] rec (absent @ total)
      (slots : 'a slots @ immutable) (key : Key.t @ immutable) =
    match slots with
    | [] -> true
    | None :: tail -> absent tail key
    | Some (k, _) :: tail -> not (Key.equal k key) && absent tail key

  let[@def] rec (distinct @ total) (slots : 'a slots @ immutable) =
    match slots with
    | [] -> true
    | None :: tail -> distinct tail
    | Some (k, _) :: tail -> absent tail k && distinct tail

  let rec (absent_lookup @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (key : Key.t) @ immutable ->
      {u : unit | absent slots key = (lookup slots key === None)} @ ghost =
    fun slots key -> ghost_ (
      lookup_def slots key; absent_def slots key;
      match slots with
      | [] -> ()
      | None :: tail -> absent_lookup tail key; ()
      | Some (_, _) :: tail -> absent_lookup tail key; ())

  let rec (empty_lookup @ total) : ('a : immutable_data).
      (capacity : int) -> (entry : (Key.t * 'a) option) @ immutable ->
      (key : Key.t) @ immutable ->
      {u : unit | not (entry === None) ||
        lookup (Vox_table_model.repeat capacity entry) key === None}
      @ ghost = fun capacity entry key -> ghost_ (
    Vox_table_model.repeat_def capacity entry;
    lookup_def (Vox_table_model.repeat capacity entry) key;
    if capacity > 0 then empty_lookup (capacity - 1) entry key;
    ())
    [@@decreases if capacity > 0 then capacity else 0]
  let rec (lookup_congruent @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (left : Key.t) @ immutable ->
      (right : Key.t) @ immutable ->
      {u : unit | not (Key.equal left right) ||
        lookup slots left === lookup slots right} @ ghost =
    fun slots left right -> ghost_ (
      lookup_def slots left; lookup_def slots right;
      match slots with
      | [] -> ()
      | None :: tail -> lookup_congruent tail left right; ()
      | Some (key, _) :: tail ->
        Key.symmetric left right;
        Key.transitive key left right;
        Key.transitive key right left;
        lookup_congruent tail left right;
        ())

  let rec (insert_at @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (index : Bigint.t) ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      (query : Key.t) @ immutable ->
      {u : unit | not (absent slots key &&
        Vox_sequence.at slots index === Some None) ||
        lookup (Vox_sequence.set slots index (Some (key, value))) query ===
          (if Key.equal key query then Some value else lookup slots query)}
      @ ghost = fun slots index key value query -> ghost_ (
    Vox_sequence.set_def slots index (Some (key, value));
    Vox_sequence.at_def slots index;
    lookup_def slots query;
    absent_def slots key;
    lookup_def (Vox_sequence.set slots index (Some (key, value))) query;
    match slots with
    | [] -> ()
    | head :: tail ->
      (match head with
       | None -> ()
       | Some (stored, _) ->
         Key.symmetric key query;
         Key.transitive stored query key);
      if index <> 0Z then insert_at tail (Bigint.sub index 1Z)
        key value query;
      ())

  let[@def] rec (agrees @ total)
      (left : 'a slots @ immutable) (right : 'a slots @ immutable) = ghost_ (
    match left with
    | [] -> true
    | None :: tail -> agrees tail right
    | Some (key, value) :: tail ->
      lookup right key === Some value && agrees tail right)

  let[@def] (same @ total)
      (left : 'a slots @ immutable) (right : 'a slots @ immutable) = ghost_ (
    agrees left right && agrees right left)

  let rec (agrees_get @ total) : ('a : immutable_data).
      (left : 'a slots) @ immutable -> (right : 'a slots) @ immutable ->
      (key : Key.t) @ immutable ->
      {u : unit | not (agrees left right) || lookup left key === None ||
        lookup left key === lookup right key} @ ghost =
    fun left right key -> ghost_ (
      agrees_def left right;
      lookup_def left key;
      match left with
      | [] -> ()
      | None :: tail -> agrees_get tail right key; ()
      | Some (stored, _) :: tail ->
        lookup_congruent right stored key;
        agrees_get tail right key;
        ())

  let (same_get @ total) : ('a : immutable_data).
      (left : 'a slots) @ immutable -> (right : 'a slots) @ immutable ->
      (key : Key.t) @ immutable ->
      {u : unit | not (same left right) ||
        lookup left key === lookup right key} @ ghost =
    fun left right key -> ghost_ (
      same_def left right;
      agrees_get left right key;
      agrees_get right left key;
      ())

  let rec (absent_at @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (index : Bigint.t) ->
      (query : Key.t) @ immutable ->
      {u : unit | not (absent slots query) ||
        (match Vox_sequence.at slots index with
         | Some (Some (stored, _)) -> not (Key.equal stored query)
         | _ -> true)} @ ghost = fun slots index query -> ghost_ (
    absent_def slots query;
    Vox_sequence.at_def slots index;
    match slots with
    | [] -> ()
    | _ :: tail ->
      if index <> 0Z then absent_at tail (Bigint.sub index 1Z) query;
      ())

  let rec (replace_at @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (index : Bigint.t) ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      (query : Key.t) @ immutable ->
      {u : unit | not (distinct slots &&
        (match Vox_sequence.at slots index with
         | Some (Some (stored, _)) -> Key.equal stored key
         | _ -> false)) ||
        lookup (Vox_sequence.set slots index (Some (key, value))) query ===
          (if Key.equal key query then Some value else lookup slots query)}
      @ ghost = fun slots index key value query -> ghost_ (
    Vox_sequence.set_def slots index (Some (key, value));
    Vox_sequence.at_def slots index;
    lookup_def slots query;
    distinct_def slots;
    lookup_def (Vox_sequence.set slots index (Some (key, value))) query;
    match slots with
    | [] -> ()
    | head :: tail ->
      (match head with
       | None -> ()
       | Some (stored, _) ->
         Key.symmetric key query;
         Key.symmetric stored key;
         Key.transitive stored key query;
         Key.transitive key stored query;
         Key.transitive stored query key;
         lookup_congruent tail stored key;
         absent_lookup tail stored;
         absent_at tail (Bigint.sub index 1Z) stored;
         (match Vox_sequence.at tail (Bigint.sub index 1Z) with
          | Some (Some (other, _)) ->
            Key.transitive other key stored;
            Key.symmetric other stored
          | _ -> ()));
      if index <> 0Z then replace_at tail (Bigint.sub index 1Z)
        key value query;
      ())

  let rec (remove_at @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (index : Bigint.t) ->
      (key : Key.t) @ immutable ->
      (query : Key.t) @ immutable ->
      {u : unit | not (distinct slots &&
        (match Vox_sequence.at slots index with
         | Some (Some (stored, _)) -> Key.equal stored key
         | _ -> false)) ||
        lookup (Vox_sequence.set slots index None) query ===
          (if Key.equal key query then None else lookup slots query)}
      @ ghost = fun slots index key query -> ghost_ (
    Vox_sequence.set_def slots index None;
    Vox_sequence.at_def slots index;
    lookup_def slots query;
    distinct_def slots;
    lookup_def (Vox_sequence.set slots index None) query;
    match slots with
    | [] -> ()
    | head :: tail ->
      (match head with
       | None -> ()
       | Some (stored, _) ->
         Key.symmetric key query;
         Key.symmetric stored key;
         Key.transitive stored key query;
         Key.transitive key stored query;
         Key.transitive stored query key;
         lookup_congruent tail stored key;
         lookup_congruent tail stored query;
         absent_lookup tail stored;
         absent_at tail (Bigint.sub index 1Z) stored;
         (match Vox_sequence.at tail (Bigint.sub index 1Z) with
          | Some (Some (other, _)) ->
            Key.transitive other key stored;
            Key.symmetric other stored
          | _ -> ()));
      if index <> 0Z then remove_at tail (Bigint.sub index 1Z)
        key query;
      ())

  let rec (absent_intro @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (query : Key.t) @ immutable ->
      ((index : Bigint.t) -> {u : unit | if 0Z <= index then
        match Vox_sequence.at slots index with
        | Some (Some (key, _)) -> not (Key.equal key query)
        | _ -> true else true} @ ghost) @ total ->
      {u : unit | absent slots query} @ ghost =
    fun slots query proof -> ghost_ (
      absent_def slots query;
      match slots with
      | [] -> ()
      | _ :: tail ->
        proof 0Z;
        Vox_sequence.at_def slots 0Z;
        absent_intro tail query (fun index -> ghost_ (
          proof (Bigint.add index 1Z);
          Vox_sequence.at_def slots (Bigint.add index 1Z);
          ()));
        ())

  let rec (lookup_at @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (index : Bigint.t) ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      (query : Key.t) @ immutable ->
      {u : unit | not (distinct slots && Key.equal key query &&
        Vox_sequence.at slots index === Some (Some (key, value))) ||
        lookup slots query === Some value} @ ghost =
    fun slots index key value query -> ghost_ (
      distinct_def slots; lookup_def slots query;
      Vox_sequence.at_def slots index;
      match slots with
      | [] -> ()
      | head :: tail ->
        if index <> 0Z then begin
          lookup_at tail (Bigint.sub index 1Z) key value query;
          (match head with
           | None -> ()
           | Some (stored, _) ->
             absent_at tail (Bigint.sub index 1Z) stored;
             Key.symmetric stored query;
             Key.transitive key query stored);
          ()
        end else ())

  let[@def] rec (erase @ total)
      (slots : 'a slots @ immutable) (key : Key.t @ immutable) =
    match slots with
    | [] -> []
    | None :: tail -> None :: erase tail key
    | Some (stored, value) :: tail ->
      (if Key.equal stored key then None else Some (stored, value)) :: erase
        tail key

  let[@def] (put @ total) (slots : 'a slots @ immutable)
      (key : Key.t @ immutable) (value : 'a @ immutable) =
    Some (key, value) :: erase slots key

  let rec (erase_get @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (key : Key.t) @ immutable ->
      (query : Key.t) @ immutable ->
      {u : unit | lookup (erase slots key) query ===
        (if Key.equal key query then None else lookup slots query)} @ ghost =
    fun slots key query -> ghost_ (
      erase_def slots key;
      lookup_def slots query; lookup_def (erase slots key) query;
      match slots with
      | [] -> ()
      | head :: tail ->
        erase_get tail key query;
        (match head with
         | None -> ()
         | Some (stored, _) ->
           Key.symmetric key query; Key.symmetric stored key;
           Key.transitive stored key query;
           Key.transitive key stored query;
           Key.transitive stored query key);
        ())

  let (put_get @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (key : Key.t) @ immutable ->
      (value : 'a) @ immutable -> (query : Key.t) @ immutable ->
      {u : unit | lookup (put slots key value) query ===
        (if Key.equal key query then Some value else lookup slots query)} @
          ghost =
    fun slots key value query -> ghost_ (
      put_def slots key value;
      lookup_def (put slots key value) query;
      erase_get slots key query;
      ())

  let rec (erase_distinct @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (key : Key.t) @ immutable ->
      {u : unit | not (distinct slots) || distinct (erase slots key)} @ ghost =
    fun slots key -> ghost_ (
      erase_def slots key;
      distinct_def slots; distinct_def (erase slots key);
      match slots with
      | [] -> ()
      | head :: tail ->
        erase_distinct tail key;
        (match head with
         | None -> ()
         | Some (stored, _) ->
           absent_lookup tail stored;
           erase_get tail key stored;
           absent_lookup (erase tail key) stored);
        ())

  let (put_distinct @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (key : Key.t) @ immutable ->
      (value : 'a) @ immutable ->
      {u : unit | not (distinct slots) || distinct (put slots key value)} @
        ghost =
    fun slots key value -> ghost_ (
      put_def slots key value;
      distinct_def (put slots key value);
      erase_distinct slots key;
      Key.reflexive key; erase_get slots key key;
      absent_lookup (erase slots key) key;
      ())

  let rec (agrees_intro @ total) : ('a : immutable_data).
      (left : 'a slots) @ immutable -> (right : 'a slots) @ immutable ->
      ((index : Bigint.t) -> {u : unit | if 0Z <= index then
        match S.at left index with
        | Some (Some (key, value)) -> lookup right key === Some value
        | _ -> true else true} @ ghost) @ total ->
      {u : unit | agrees left right} @ ghost = fun left right proof -> ghost_ (
    agrees_def left right;
    match left with
    | [] -> ()
    | _ :: tail ->
      proof 0Z; S.at_def left 0Z;
      agrees_intro tail right (fun index -> ghost_ (
        proof (Bigint.add index 1Z);
        S.at_def left (Bigint.add index 1Z);
        ()));
      ())

  let (same_intro @ total) : ('a : immutable_data).
      (left : 'a slots) @ immutable -> (right : 'a slots) @ immutable ->
      ((query : Key.t) @ immutable ->
        {u : unit | lookup left query === lookup right query} @ ghost) @ total
          ->
      {u : unit | not (distinct left && distinct right) || same left right}
      @ ghost = fun left right proof -> ghost_ (
    same_def left right;
    if distinct left && distinct right then begin
      agrees_intro left right (fun index -> ghost_ (
        match S.at left index with
        | Some (Some (key, value)) ->
          Key.reflexive key; lookup_at left index key value key; proof key; ()
        | _ -> ()));
      agrees_intro right left (fun index -> ghost_ (
        match S.at right index with
        | Some (Some (key, value)) ->
          Key.reflexive key; lookup_at right index key value key; proof key; ()
        | _ -> ()));
      ()
    end else ())

  let rec (absent_remove @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (index : Bigint.t) ->
      (key : Key.t) @ immutable ->
      {u : unit | not (absent slots key) ||
        absent (S.set slots index None) key} @ ghost =
    fun slots index key -> ghost_ (
      absent_def slots key;
      S.set_def slots index None;
      absent_def (S.set slots index None) key;
      match slots with
      | [] -> ()
      | _ :: tail ->
        if index <> 0Z then absent_remove tail (Bigint.sub index 1Z) key;
        ())

  let rec (distinct_remove @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (index : Bigint.t) ->
      {u : unit | not (distinct slots) || distinct (S.set slots index None)}
      @ ghost = fun slots index -> ghost_ (
    distinct_def slots; S.set_def slots index None;
    distinct_def (S.set slots index None);
    match slots with
    | [] -> ()
    | head :: tail ->
      if index <> 0Z then begin
        distinct_remove tail (Bigint.sub index 1Z);
        match head with
        | None -> ()
        | Some (key, _) -> absent_remove tail (Bigint.sub index 1Z) key
      end else ())

  let (same_remove @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (index : Bigint.t) ->
      (key : Key.t) @ immutable ->
      {u : unit | not (distinct slots &&
        (match S.at slots index with
         | Some (Some (stored, _)) -> Key.equal stored key | _ -> false)) ||
        same (S.set slots index None) (erase slots key)} @ ghost =
    fun slots index key -> ghost_ (
      if distinct slots &&
         (match S.at slots index with
          | Some (Some (stored, _)) -> Key.equal stored key | _ -> false) then
            begin
        distinct_remove slots index; erase_distinct slots key;
        same_intro (S.set slots index None) (erase slots key)
          (fun query -> ghost_ (
            remove_at slots index key query;
            erase_get slots key query;
            ()));
        ()
      end else ())

  let rec (absent_set @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (index : Bigint.t) ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      (query : Key.t) @ immutable ->
      {u : unit | not (absent slots query && not (Key.equal key query)) ||
        absent (S.set slots index (Some (key, value))) query} @ ghost =
    fun slots index key value query -> ghost_ (
      absent_def slots query;
      S.set_def slots index (Some (key, value));
      absent_def (S.set slots index (Some (key, value))) query;
      match slots with
      | [] -> ()
      | _ :: tail ->
        if index <> 0Z then absent_set tail (Bigint.sub index 1Z) key value
          query;
        ())

  let rec (distinct_insert @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (index : Bigint.t) ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      {u : unit | not (distinct slots && absent slots key) ||
        distinct (S.set slots index (Some (key, value)))} @ ghost =
    fun slots index key value -> ghost_ (
      distinct_def slots; absent_def slots key;
      S.set_def slots index (Some (key, value));
      distinct_def (S.set slots index (Some (key, value)));
      match slots with
      | [] -> ()
      | head :: tail ->
        if index <> 0Z then begin
          distinct_insert tail (Bigint.sub index 1Z) key value;
          match head with
          | None -> ()
          | Some (stored, _) ->
            Key.symmetric stored key;
            absent_set tail (Bigint.sub index 1Z) key value stored
        end else ())

  let (same_insert @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (index : Bigint.t) ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      {u : unit | not (distinct slots && absent slots key &&
        S.at slots index === Some None) ||
        same (S.set slots index (Some (key, value))) (put slots key value)}
      @ ghost = fun slots index key value -> ghost_ (
    if distinct slots && absent slots key && S.at slots index === Some None
      then begin
      distinct_insert slots index key value; put_distinct slots key value;
      same_intro (S.set slots index (Some (key, value))) (put slots key value)
        (fun query -> ghost_ (
          insert_at slots index key value query;
          put_get slots key value query;
          ()));
      ()
    end else ())

  let (same_absent_remove @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (key : Key.t) @ immutable ->
      {u : unit | not (distinct slots && absent slots key) ||
        same slots (erase slots key)} @ ghost = fun slots key -> ghost_ (
    if distinct slots && absent slots key then begin
      erase_distinct slots key;
      absent_lookup slots key;
      same_intro slots (erase slots key) (fun query -> ghost_ (
        lookup_congruent slots key query;
        erase_get slots key query;
        ()));
      ()
    end else ())

  let (erase_congruent @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable ->
      (left : Key.t) @ immutable -> (right : Key.t) @ immutable ->
      {u : unit | not (distinct slots && Key.equal left right) ||
        same (erase slots left) (erase slots right)} @ ghost =
    fun slots left right -> ghost_ (
      if distinct slots && Key.equal left right then begin
        erase_distinct slots left; erase_distinct slots right;
        same_intro (erase slots left) (erase slots right) (fun query -> ghost_ (
          Key.symmetric left right;
          Key.transitive left right query;
          Key.transitive right left query;
          erase_get slots left query; erase_get slots right query;
          ()));
        ()
      end else ())

  let (same_transitive @ total) : ('a : immutable_data).
      (left : 'a slots) @ immutable -> (middle : 'a slots) @ immutable ->
      (right : 'a slots) @ immutable ->
      {u : unit | not (distinct left && distinct right &&
        same left middle && same middle right) || same left right} @ ghost =
    fun left middle right -> ghost_ (
      if distinct left && distinct right && same left middle && same middle
        right
      then begin
        same_intro left right (fun query -> ghost_ (
          same_get left middle query; same_get middle right query; ()));
        ()
      end else ())

  let (put_congruent @ total) : ('a : immutable_data).
      (slots : 'a slots) @ immutable -> (left : Key.t) @ immutable ->
      (right : Key.t) @ immutable -> (value : 'a) @ immutable ->
      {u : unit | not (distinct slots && Key.equal left right) ||
        same (put slots left value) (put slots right value)} @ ghost =
    fun slots left right value -> ghost_ (
      if distinct slots && Key.equal left right then begin
        put_distinct slots left value; put_distinct slots right value;
        same_intro (put slots left value) (put slots right value) (fun query
          -> ghost_ (
          Key.symmetric left right;
          Key.transitive left right query; Key.transitive right left query;
          put_get slots left value query; put_get slots right value query;
          ()));
        ()
      end else ())

  let (put_same @ total) : ('a : immutable_data).
      (before : 'a slots) @ immutable -> (after : 'a slots) @ immutable ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      {u : unit | not (distinct before && distinct after && same before after)
        ||
        same (put before key value) (put after key value)} @ ghost =
    fun before after key value -> ghost_ (
      if distinct before && distinct after && same before after then begin
        put_distinct before key value; put_distinct after key value;
        same_intro (put before key value) (put after key value) (fun query ->
          ghost_ (
          put_get before key value query; put_get after key value query;
          same_get before after query;
          ()));
        ()
      end else ())

end
