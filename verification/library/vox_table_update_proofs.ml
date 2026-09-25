module M = Vox_table_model
module S = Vox_sequence

module Make (Key : Vox_table_map.Key)
    (Invariant : module type of Vox_table_invariant.Make (Key)) = struct
  let rec (cells_intro @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable ->
      (slots : 'a Invariant.Map.slots) @ immutable -> (start : Bigint.t) ->
      ((index : Bigint.t) -> {u : unit | if 0Z <= index then
        match S.at slots index,
          S.at model.controls (Bigint.add start index) with
        | Some (Some (key, _)), Some byte -> byte = (Key.hash key land 127)
        | Some None, Some byte -> byte = 128 || byte = 254
        | None, _ -> true
        | _ -> false
        else true} @ ghost) @ total ->
      {u : unit | Invariant.cells_valid model slots start} @ ghost =
    fun model slots start proof -> ghost_ (
      Invariant.cells_valid_def model slots start;
      match slots with
      | [] -> ()
      | _ :: tail ->
        proof 0Z; S.at_def slots 0Z;
        cells_intro model tail (Bigint.add start 1Z) (fun index -> ghost_ (
          proof (Bigint.add index 1Z);
          S.at_def slots (Bigint.add index 1Z);
          ()));
        ())

  let rec (routes_intro @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable ->
      (slots : 'a Invariant.Map.slots) @ immutable ->
      (paths : (int * int) list) @ immutable -> (start : Bigint.t) ->
      ((index : Bigint.t) -> {u : unit | if 0Z <= index then
        match S.at slots index, S.at paths index with
        | None, None -> true
        | Some entry, Some path ->
          Invariant.route model (Bigint.add start index) entry path
        | _ -> false else true} @ ghost) @ total ->
      {u : unit | Invariant.routes_valid model slots paths start} @ ghost =
    fun model slots paths start proof -> ghost_ (
      Invariant.routes_valid_def model slots paths start;
      proof 0Z; S.at_def slots 0Z; S.at_def paths 0Z;
      match slots, paths with
      | _ :: tail, _ :: rest ->
        routes_intro model tail rest (Bigint.add start 1Z) (fun index ->
          ghost_ (
          proof (Bigint.add index 1Z);
          S.at_def slots (Bigint.add index 1Z);
          S.at_def paths (Bigint.add index 1Z);
          ()));
        ()
      | _ -> ())

  let rec (clones_intro @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable ->
      (count : {n : int | 0 <= n && n <= 15}) ->
      ((lane : int) -> {u : unit | not (0 <= lane && lane < count) ||
        M.control model (model.capacity + lane) === M.control model lane}
        @ ghost) @ total ->
      {u : unit | Invariant.clones model count} @ ghost =
    fun model count proof -> ghost_ (
      Invariant.clones_def model count;
      if count > 0 then begin
        proof (count - 1);
        clones_intro model (count - 1) (fun lane -> ghost_ (proof lane; ()));
        ()
      end else ())
    [@@decreases count]

  let[@def] (weight @ total) (entry : (Key.t * 'a) option @ immutable) =
    match entry with None -> 0Z | Some _ -> 1Z

  let rec (live_set @ total) : ('a : immutable_data).
      (slots : 'a Invariant.Map.slots) @ immutable -> (index : Bigint.t) ->
      (entry : (Key.t * 'a) option) @ immutable ->
      {u : unit | Invariant.live_count (S.set slots index entry) =
        (match S.at slots index with
         | None -> Invariant.live_count slots
         | Some old -> Bigint.add
             (Bigint.sub (Invariant.live_count slots) (weight old))
             (weight entry))} @ ghost = fun slots index entry -> ghost_ (
    S.at_def slots index; S.set_def slots index entry;
    Invariant.live_count_def slots;
    Invariant.live_count_def (S.set slots index entry);
    weight_def entry;
    match slots with
    | [] -> ()
    | head :: tail ->
      weight_def head;
      if index <> 0Z then live_set tail (Bigint.sub index 1Z) entry;
      ())
  let[@def] (deleted_weight @ total) (byte : int) =
    if byte = 254 then 1Z else 0Z

  let rec (deleted_set @ total) :
      (bytes : int list) -> (remaining : int) -> (index : Bigint.t) ->
      (byte : int) ->
      {u : unit | Invariant.deleted_count (S.set bytes index byte) remaining =
        Bigint.add (Invariant.deleted_count bytes remaining)
          (if 0Z <= index && index < Bigint.of_int remaining then
             match S.at bytes index with
             | None -> 0Z
             | Some old -> Bigint.sub (deleted_weight byte) (deleted_weight old)
           else 0Z)} @ ghost = fun bytes remaining index byte -> ghost_ (
    S.at_def bytes index;
    S.set_def bytes index byte;
    Invariant.deleted_count_def bytes remaining;
    Invariant.deleted_count_def (S.set bytes index byte) remaining;
    deleted_weight_def byte;
    match bytes with
    | [] -> ()
    | head :: tail ->
      deleted_weight_def head;
      if remaining > 0 && index <> 0Z then
        deleted_set tail (remaining - 1) (Bigint.sub index 1Z) byte;
      ())

  let[@def] rec (same_keys @ total)
      (left : 'a Invariant.Map.slots @ immutable)
      (right : 'a Invariant.Map.slots @ immutable) = ghost_ (
    match left, right with
    | [], [] -> true
    | None :: tail, None :: rest -> same_keys tail rest
    | Some (key, _) :: tail, Some (other, _) :: rest ->
      key === other && same_keys tail rest
    | _ -> false)

  let rec (same_keys_refl @ total) : ('a : immutable_data).
      (slots : 'a Invariant.Map.slots) @ immutable ->
      {u : unit | same_keys slots slots} @ ghost = fun slots -> ghost_ (
    same_keys_def slots slots;
    match slots with [] -> () | _ :: tail -> same_keys_refl tail; ())

  let rec (same_keys_set @ total) : ('a : immutable_data).
      (slots : 'a Invariant.Map.slots) @ immutable -> (index : Bigint.t) ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      {u : unit | not (match S.at slots index with
        | Some (Some (stored, _)) -> stored === key | _ -> false) ||
        same_keys slots (S.set slots index (Some (key, value)))} @ ghost =
    fun slots index key value -> ghost_ (
      S.at_def slots index; S.set_def slots index (Some (key, value));
      same_keys_def slots (S.set slots index (Some (key, value)));
      match slots with
      | [] -> ()
      | _ :: tail ->
        if index = 0Z then same_keys_refl tail
        else same_keys_set tail (Bigint.sub index 1Z) key value;
        ())

  let rec (key_counts @ total) : ('a : immutable_data).
      (left : 'a Invariant.Map.slots) @ immutable ->
      (right : 'a Invariant.Map.slots) @ immutable ->
      {u : unit | not (same_keys left right) ||
        S.length left = S.length right &&
        Invariant.live_count left = Invariant.live_count right} @ ghost =
    fun left right -> ghost_ (
      same_keys_def left right;
      S.length_def left; S.length_def right;
      Invariant.live_count_def left; Invariant.live_count_def right;
      match left, right with
      | _ :: tail, _ :: rest -> key_counts tail rest; ()
      | _ -> ())

  let rec (absent_same_keys @ total) : ('a : immutable_data).
      (left : 'a Invariant.Map.slots) @ immutable ->
      (right : 'a Invariant.Map.slots) @ immutable -> (key : Key.t) @
        immutable ->
      {u : unit | not (same_keys left right) ||
        Invariant.Map.absent left key = Invariant.Map.absent right key}
      @ ghost = fun left right key -> ghost_ (
    same_keys_def left right;
    Invariant.Map.absent_def left key; Invariant.Map.absent_def right key;
    match left, right with
    | _ :: tail, _ :: rest -> absent_same_keys tail rest key; ()
    | _ -> ())

  let rec (distinct_same_keys @ total) : ('a : immutable_data).
      (left : 'a Invariant.Map.slots) @ immutable ->
      (right : 'a Invariant.Map.slots) @ immutable ->
      {u : unit | not (same_keys left right) ||
        Invariant.Map.distinct left = Invariant.Map.distinct right} @ ghost =
    fun left right -> ghost_ (
      same_keys_def left right;
      Invariant.Map.distinct_def left; Invariant.Map.distinct_def right;
      match left, right with
      | head :: tail, _ :: rest ->
        distinct_same_keys tail rest;
        (match head with None -> () | Some (key, _) -> absent_same_keys tail
          rest key);
        ()
      | _ -> ())

  let rec (matching_equal @ total) : ('a : immutable_data).
      (before : (Key.t, 'a) M.state) @ immutable ->
      (after : (Key.t, 'a) M.state) @ immutable ->
      (offset : int) -> (needle : int) -> (count : int) ->
      {u : unit | not (before.controls === after.controls) ||
        M.matching before offset needle count = M.matching after offset needle
          count}
      @ ghost = fun before after offset needle count -> ghost_ (
    M.matching_def before offset needle count;
    M.matching_def after offset needle count;
    if count > 0 then begin
      M.control_def before (offset + count - 1);
      M.control_def after (offset + count - 1);
      matching_equal before after offset needle (count - 1);
      ()
    end else ())
    [@@decreases if count > 0 then count else 0]

  let rec (empty_free_equal @ total) : ('a : immutable_data).
      (before : (Key.t, 'a) M.state) @ immutable ->
      (after : (Key.t, 'a) M.state) @ immutable -> (hash : int) -> (count :
        int) ->
      {u : unit | not (before.controls === after.controls &&
        before.capacity = after.capacity) ||
        Invariant.empty_free before hash count = Invariant.empty_free after
          hash count}
      @ ghost = fun before after hash count -> ghost_ (
    Invariant.empty_free_def before hash count;
    Invariant.empty_free_def after hash count;
    if count > 0 then begin
      empty_free_equal before after hash (count - 1);
      matching_equal before after (Invariant.group before.capacity hash (count
        - 1))
        128 16;
      ()
    end else ())
    [@@decreases if count > 0 then count else 0]

  let rec (cells_same_keys @ total) : ('a : immutable_data).
      (before : (Key.t, 'a) M.state) @ immutable ->
      (after : (Key.t, 'a) M.state) @ immutable ->
      (left : 'a Invariant.Map.slots) @ immutable ->
      (right : 'a Invariant.Map.slots) @ immutable -> (start : Bigint.t) ->
      {u : unit | not (before.controls === after.controls && same_keys left
        right) ||
        Invariant.cells_valid before left start = Invariant.cells_valid after
          right start}
      @ ghost = fun before after left right start -> ghost_ (
    same_keys_def left right;
    Invariant.cells_valid_def before left start;
    Invariant.cells_valid_def after right start;
    match left, right with
    | _ :: tail, _ :: rest ->
      cells_same_keys before after tail rest (Bigint.add start 1Z); ()
    | _ -> ())

  let (route_same_key @ total) : ('a : immutable_data).
      (before : (Key.t, 'a) M.state) @ immutable ->
      (after : (Key.t, 'a) M.state) @ immutable -> (index : Bigint.t) ->
      (entry : (Key.t * 'a) option) @ immutable ->
      (other : (Key.t * 'a) option) @ immutable -> (path : (int * int)) ->
      {u : unit | not (before.controls === after.controls &&
        before.capacity = after.capacity && (match entry, other with
        | None, None -> true
        | Some (key, _), Some (key2, _) -> key === key2
        | _ -> false)) ||
        Invariant.route before index entry path =
          Invariant.route after index other path} @ ghost =
    fun before after index entry other path -> ghost_ (
      if before.controls === after.controls &&
         before.capacity = after.capacity then
        match entry, other with
        | None, None ->
          Invariant.route_def before index entry path;
          Invariant.route_def after index other path;
          ()
        | Some (key, _), Some (key2, _) ->
          if key === key2 then begin
            let rank, _ = path in
            empty_free_equal before after (Key.hash key) rank;
            Invariant.route_def before index entry path;
            Invariant.route_def after index other path;
            ()
          end else ()
        | _ -> ()
      else ())

  let rec (routes_same_keys @ total) : ('a : immutable_data).
      (before : (Key.t, 'a) M.state) @ immutable ->
      (after : (Key.t, 'a) M.state) @ immutable ->
      (left : 'a Invariant.Map.slots) @ immutable ->
      (right : 'a Invariant.Map.slots) @ immutable ->
      (paths : (int * int) list) @ immutable -> (start : Bigint.t) ->
      {u : unit | not (before.controls === after.controls &&
        before.capacity = after.capacity && same_keys left right) ||
        Invariant.routes_valid before left paths start =
          Invariant.routes_valid after right paths start} @ ghost =
    fun before after left right paths start -> ghost_ (
      same_keys_def left right;
      Invariant.routes_valid_def before left paths start;
      Invariant.routes_valid_def after right paths start;
      match left, right, paths with
      | entry :: tail, other :: rest, path :: next ->
        route_same_key before after start entry other path;
        routes_same_keys before after tail rest next (Bigint.add start 1Z);
        ()
      | _ -> ())

  let rec (clones_equal @ total) : ('a : immutable_data).
      (before : (Key.t, 'a) M.state) @ immutable ->
      (after : (Key.t, 'a) M.state) @ immutable -> (count : int) ->
      {u : unit | not (before.controls === after.controls &&
        before.capacity = after.capacity) ||
        Invariant.clones before count = Invariant.clones after count}
      @ ghost = fun before after count -> ghost_ (
    Invariant.clones_def before count; Invariant.clones_def after count;
    if count > 0 then begin
      M.control_def before (before.capacity + count - 1);
      M.control_def before (count - 1);
      M.control_def after (after.capacity + count - 1);
      M.control_def after (count - 1);
      clones_equal before after (count - 1);
      ()
    end else ())
    [@@decreases if count > 0 then count else 0]

  let (shape_same_keys @ total) : ('a : immutable_data).
      (before : (Key.t, 'a) M.state) @ immutable ->
      (after : (Key.t, 'a) M.state) @ immutable ->
      {u : unit | not (before.capacity = after.capacity &&
        before.size = after.size && before.deleted = after.deleted &&
        before.controls === after.controls && same_keys before.slots
          after.slots) ||
        Invariant.shape before = Invariant.shape after} @ ghost =
    fun before after -> ghost_ (
      Invariant.shape_def before; Invariant.shape_def after;
      key_counts before.slots after.slots;
      ())

  let (valid_same_keys @ total) : ('a : immutable_data).
      (before : 'a Invariant.view) @ immutable ->
      (after : 'a Invariant.view) @ immutable ->
      {u : unit | not (before.model.capacity = after.model.capacity &&
        before.model.size = after.model.size &&
        before.model.deleted = after.model.deleted &&
        before.model.controls === after.model.controls &&
        same_keys before.model.slots after.model.slots &&
        before.routes === after.routes && before.plan === after.plan) ||
        Invariant.valid before = Invariant.valid after} @ ghost =
    fun before after -> ghost_ (
      Invariant.valid_def before; Invariant.valid_def after;
      shape_same_keys before.model after.model;
      cells_same_keys before.model after.model before.model.slots
        after.model.slots 0Z;
      routes_same_keys before.model after.model before.model.slots
        after.model.slots
        before.routes 0Z;
      clones_equal before.model after.model 15;
      distinct_same_keys before.model.slots after.model.slots;
      ())
  let rec (matching_zero_preserved @ total) : ('a : immutable_data).
      (before : (Key.t, 'a) M.state) @ immutable ->
      (after : (Key.t, 'a) M.state) @ immutable ->
      (offset : int) -> (needle : int) -> (count : int) ->
      ((index : int) -> {u : unit |
        not (M.control after index === Some needle) ||
        M.control before index === Some needle} @ ghost) @ total ->
      {u : unit | M.matching before offset needle count <> 0 ||
        M.matching after offset needle count = 0} @ ghost =
    fun before after offset needle count proof -> ghost_ (
      M.matching_def before offset needle count;
      M.matching_def after offset needle count;
      if count > 0 then begin
        proof (offset + count - 1);
        matching_zero_preserved before after offset needle (count - 1) proof;
        ()
      end else ())
    [@@decreases if count > 0 then count else 0]

  let rec (empty_free_preserved @ total) : ('a : immutable_data).
      (before : (Key.t, 'a) M.state) @ immutable ->
      (after : (Key.t, 'a) M.state) @ immutable ->
      (hash : int) -> (depth : int) ->
      ((index : int) -> {u : unit |
        not (M.control after index === Some 128) ||
        M.control before index === Some 128} @ ghost) @ total ->
      {u : unit | not (before.capacity = after.capacity &&
        Invariant.empty_free before hash depth) ||
        Invariant.empty_free after hash depth} @ ghost =
    fun before after hash depth proof -> ghost_ (
      Invariant.empty_free_def before hash depth;
      Invariant.empty_free_def after hash depth;
      if depth > 0 then begin
        empty_free_preserved before after hash (depth - 1) proof;
        matching_zero_preserved before after
          (Invariant.group before.capacity hash (depth - 1)) 128 16 proof;
        ()
      end else ())
    [@@decreases if depth > 0 then depth else 0]

  let (route_preserved @ total) : ('a : immutable_data).
      (before : (Key.t, 'a) M.state) @ immutable ->
      (after : (Key.t, 'a) M.state) @ immutable ->
      (index : Bigint.t) -> (entry : (Key.t * 'a) option) @ immutable ->
      (path : (int * int)) ->
      ((index : int) -> {u : unit |
        not (M.control after index === Some 128) ||
        M.control before index === Some 128} @ ghost) @ total ->
      {u : unit | not (before.capacity = after.capacity &&
        Invariant.route before index entry path) ||
        Invariant.route after index entry path} @ ghost =
    fun before after index entry path proof -> ghost_ (
      Invariant.route_def before index entry path;
      Invariant.route_def after index entry path;
      match entry with
      | None -> ()
      | Some (key, _) ->
        let depth, _ = path in
        empty_free_preserved before after (Key.hash key) depth proof;
        ())

  let rec (routes_preserved @ total) : ('a : immutable_data).
      (before : (Key.t, 'a) M.state) @ immutable ->
      (after : (Key.t, 'a) M.state) @ immutable ->
      (slots : 'a Invariant.Map.slots) @ immutable ->
      (paths : (int * int) list) -> (start : Bigint.t) ->
      ((index : int) -> {u : unit |
        not (M.control after index === Some 128) ||
        M.control before index === Some 128} @ ghost) @ total ->
      {u : unit | not (before.capacity = after.capacity &&
        Invariant.routes_valid before slots paths start) ||
        Invariant.routes_valid after slots paths start} @ ghost =
    fun before after slots paths start proof -> ghost_ (
      Invariant.routes_valid_def before slots paths start;
      Invariant.routes_valid_def after slots paths start;
      match slots, paths with
      | entry :: tail, path :: rest ->
        route_preserved before after start entry path proof;
        routes_preserved before after tail rest (Bigint.add start 1Z) proof;
        ()
      | _ -> ())

  let rec (routes_remove @ total) : ('a : immutable_data).
      (before : (Key.t, 'a) M.state) @ immutable ->
      (after : (Key.t, 'a) M.state) @ immutable ->
      (slots : 'a Invariant.Map.slots) @ immutable ->
      (paths : (int * int) list) -> (start : Bigint.t) -> (index : Bigint.t) ->
      ((index : int) -> {u : unit |
        not (M.control after index === Some 128) ||
        M.control before index === Some 128} @ ghost) @ total ->
      {u : unit | not (before.capacity = after.capacity &&
        Invariant.routes_valid before slots paths start) ||
        Invariant.routes_valid after (S.set slots index None) paths start}
      @ ghost = fun before after slots paths start index proof -> ghost_ (
    S.set_def slots index None;
    Invariant.routes_valid_def before slots paths start;
    Invariant.routes_valid_def after (S.set slots index None) paths start;
    match slots, paths with
    | entry :: tail, path :: rest ->
      if index = 0Z then begin
        Invariant.route_def after start None path;
        routes_preserved before after tail rest (Bigint.add start 1Z) proof;
        ()
      end else begin
        route_preserved before after start entry path proof;
        routes_remove before after tail rest (Bigint.add start 1Z)
          (Bigint.sub index 1Z) proof;
        ()
      end
    | _ -> ())

  let rec (cells_remove @ total) : ('a : immutable_data).
      (before : (Key.t, 'a) M.state) @ immutable ->
      (after : (Key.t, 'a) M.state) @ immutable ->
      (slots : 'a Invariant.Map.slots) @ immutable ->
      (start : Bigint.t) -> (target : Bigint.t) ->
      ((index : Bigint.t) -> {u : unit |
        not (0Z <= index && index < Bigint.of_int before.capacity) ||
        S.at after.controls index ===
          (if index = target then Some 254 else S.at before.controls index)}
        @ ghost) @ total ->
      {u : unit | not (Invariant.cells_valid before slots start &&
        0Z <= start && Bigint.add start (S.length slots) <=
          Bigint.of_int before.capacity) ||
        Invariant.cells_valid after
          (S.set slots (Bigint.sub target start) None) start} @ ghost =
    fun before after slots start target proof -> ghost_ (
      S.length_def slots;
      S.set_def slots (Bigint.sub target start) None;
      Invariant.cells_valid_def before slots start;
      Invariant.cells_valid_def after (S.set slots (Bigint.sub target start)
        None)
        start;
      match slots with
      | [] -> ()
      | _ :: tail ->
        Vox_table_model_proofs.length_nonnegative tail;
        proof start;
        Vox_table_model_proofs.set_negative tail
          (Bigint.sub target (Bigint.add start 1Z)) None;
        cells_remove before after tail (Bigint.add start 1Z) target proof;
        ())

  let rec (cell_at @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable ->
      (slots : 'a Invariant.Map.slots) @ immutable -> (start : Bigint.t) ->
      (index : Bigint.t) -> (entry : (Key.t * 'a) option) @ immutable ->
      {u : unit | not (Invariant.cells_valid model slots start &&
        S.at slots index === Some entry) ||
        (match entry, S.at model.controls (Bigint.add start index) with
         | Some (key, _), Some byte -> byte = (Key.hash key land 127)
         | None, Some byte -> byte = 128 || byte = 254
         | _ -> false)} @ ghost = fun model slots start index entry -> ghost_ (
    Invariant.cells_valid_def model slots start;
    S.at_def slots index;
    match slots with
    | [] -> ()
    | _ :: tail ->
      if index <> 0Z then
        cell_at model tail (Bigint.add start 1Z) (Bigint.sub index 1Z) entry;
      ())

  let rec (live_nonnegative @ total) : ('a : immutable_data).
      (slots : 'a Invariant.Map.slots) @ immutable ->
      {u : unit | 0Z <= Invariant.live_count slots} @ ghost =
    fun slots -> ghost_ (
      Invariant.live_count_def slots;
      match slots with [] -> () | _ :: tail -> live_nonnegative tail)

  let rec (clones_byte @ total) : ('a : immutable_data).
      (before : (Key.t, 'a) M.state) @ immutable ->
      (after : (Key.t, 'a) M.state) @ immutable ->
      (index : int) -> (byte : int) ->
      (count : {n : int | 0 <= n && n <= 15}) ->
      {u : unit | not (16 <= before.capacity && before.capacity <= 1073741824 &&
        S.length before.controls = Bigint.of_int (before.capacity + 15) &&
        0 <= index && index < before.capacity &&
        after.capacity = before.capacity &&
        after.controls === (M.set_byte before index byte).controls &&
        Invariant.clones before count) || Invariant.clones after count} @
          ghost =
    fun before after index byte count -> ghost_ (
      Invariant.clones_def before count;
      Invariant.clones_def after count;
      if count > 0 then begin
        let lane = count - 1 in
        M.control_def after lane;
        M.control_def after (after.capacity + lane);
        M.control_def (M.set_byte before index byte) lane;
        M.control_def (M.set_byte before index byte) (before.capacity + lane);
        Vox_table_model_proofs.byte_clone before index byte lane;
        clones_byte before after index byte (count - 1);
        ()
      end else ())
    [@@decreases count]

  let (remove_reachability @ total) : ('a : immutable_data).
      (before : 'a Invariant.view) @ immutable ->
      (index : int) -> (after : 'a Invariant.view) @ immutable ->
      {u : unit | not (Invariant.valid before &&
        0 <= index && index < before.model.capacity &&
        after.model === M.remove_slot before.model index &&
        after.routes === before.routes) ||
        Invariant.cells_valid after.model after.model.slots 0Z &&
        Invariant.clones after.model 15 &&
        Invariant.routes_valid after.model after.model.slots after.routes 0Z}
      @ ghost = fun before index after -> ghost_ (
    if Invariant.valid before &&
       0 <= index && index < before.model.capacity &&
       after.model === M.remove_slot before.model index &&
       after.routes === before.routes then begin
      Invariant.valid_def before; Invariant.shape_def before.model;
      Vox_table_model_proofs.remove_fields before.model index;
      cells_remove before.model after.model before.model.slots 0Z
        (Bigint.of_int index) (fun query -> ghost_ (
          Vox_table_model_proofs.byte_at_primary before.model index 254 query;
          ()));
      clones_byte before.model after.model index 254 15;
      routes_remove before.model after.model before.model.slots before.routes 0Z
        (Bigint.of_int index) (fun query -> ghost_ (
          Vox_table_model_proofs.byte_write before.model index 254 query;
          M.control_def after.model query;
          M.control_def (M.set_byte before.model index 254) query;
          ()));
      ()
    end else ())

  let (byte_deleted_count @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable -> (index : int) ->
      {u : unit | not (16 <= model.capacity && model.capacity <= 1073741824 &&
        0 <= index && index < model.capacity &&
        (match M.control model index with Some byte -> byte <> 254 | None ->
          false))
        || Invariant.deleted_count (M.set_byte model index 254).controls
             model.capacity =
           Bigint.add (Invariant.deleted_count model.controls model.capacity)
             1Z}
      @ ghost = fun model index -> ghost_ (
    M.set_byte_def model index 254;
    M.set_control_def model index 254;
    M.control_def model index;
    deleted_weight_def 254;
    deleted_set model.controls model.capacity (Bigint.of_int index) 254;
    (match M.control model index with
     | Some byte -> deleted_weight_def byte
     | None -> ());
    if index < 15 then begin
      let first = M.set_control model index 254 in
      M.set_control_def first (model.capacity + index) 254;
      deleted_set first.controls model.capacity
        (Bigint.of_int (model.capacity + index)) 254;
      ()
    end else ())

  let (byte_length @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable -> (index : int) -> (byte :
        int) ->
      {u : unit | S.length (M.set_byte model index byte).controls =
        S.length model.controls} @ ghost = fun model index byte -> ghost_ (
    M.set_byte_def model index byte;
    M.set_control_def model index byte;
    S.set_length model.controls (Bigint.of_int index) byte;
    if index < 15 then begin
      let first = M.set_control model index byte in
      M.set_control_def first (model.capacity + index) byte;
      S.set_length first.controls (Bigint.of_int (model.capacity + index)) byte;
      ()
    end else ())

  let (empty_entry @ total) : ('a : immutable_data).
      (Key.t, 'a) M.state @ immutable ->
      {entry : (Key.t * 'a) option | entry === None} @ immutable ghost =
    fun _ -> ghost_ None

  let (remove_arithmetic @ total) (capacity : int) (size : int) (deleted : int)
      (limit : int) (live_after : Bigint.t) :
      {u : unit | not (16 <= capacity && capacity <= 1073741824 &&
        0 <= size && size <= capacity && 0 <= deleted &&
        deleted <= capacity - size &&
        size + deleted <= capacity - limit &&
        0Z <= live_after &&
        live_after = Bigint.sub (Bigint.of_int size) 1Z) ||
        0 <= size - 1 && 0 <= deleted + 1 &&
        size - 1 <= capacity && deleted + 1 <= capacity - (size - 1) &&
        size - 1 + (deleted + 1) <= capacity - limit &&
        Bigint.of_int (size - 1) = live_after &&
        Bigint.of_int (deleted + 1) = Bigint.add (Bigint.of_int deleted) 1Z}
      @ ghost = ghost_ ()

  let (remove_shape @ total) : ('a : immutable_data).
      (before : (Key.t, 'a) M.state) @ immutable -> (index : int) ->
      (after : (Key.t, 'a) M.state) @ immutable ->
      {u : unit | not (Invariant.shape before &&
        0 <= index && index < before.capacity &&
        (match M.slot before index with Some (Some _) -> true | _ -> false) &&
        (match M.control before index with Some byte -> byte <> 254 | None ->
          false)
        && after === M.remove_slot before index) || Invariant.shape after}
      @ ghost = fun before index after -> ghost_ (
    if Invariant.shape before &&
       0 <= index && index < before.capacity &&
       (match M.slot before index with Some (Some _) -> true | _ -> false) &&
       (match M.control before index with Some byte -> byte <> 254 | None ->
         false)
       && after === M.remove_slot before index then begin
    Invariant.shape_def before;
    Vox_table_model_proofs.remove_fields before index;
    M.slot_def before index;
    S.set_length before.slots (Bigint.of_int index) None;
    byte_length before index 254;
    byte_deleted_count before index;
    live_set before.slots (Bigint.of_int index) None;
    live_nonnegative after.slots;
    weight_def (empty_entry before);
    (match M.slot before index with
     | Some entry -> weight_def entry
     | _ -> ());
    remove_arithmetic before.capacity before.size before.deleted
      (Invariant.reserve before.capacity) (Invariant.live_count after.slots);
    Invariant.shape_def after;
    ()
    end else ())

  let (remove_valid @ total) : ('a : immutable_data).
      (before : 'a Invariant.view) @ immutable -> (index : int) ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      (after : 'a Invariant.view) @ immutable ->
      {u : unit | not (Invariant.valid before &&
        0 <= index && index < before.model.capacity &&
        M.slot before.model index === Some (Some (key, value)) &&
        after.model === M.remove_slot before.model index &&
        after.routes === before.routes && after.plan === before.plan) ||
        Invariant.valid after && Invariant.Map.same after.model.slots
          (Invariant.Map.erase before.model.slots key)} @ ghost =
    fun before index key value after -> ghost_ (
      if Invariant.valid before &&
         0 <= index && index < before.model.capacity &&
         M.slot before.model index === Some (Some (key, value)) &&
         after.model === M.remove_slot before.model index &&
         after.routes === before.routes && after.plan === before.plan then begin
        Invariant.valid_def before;
        M.slot_def before.model index;
        cell_at before.model before.model.slots 0Z (Bigint.of_int index)
          (Some (key, value));
        M.control_def before.model index;
        let (_ : {u : unit | (Key.hash key land 127) <> 254}) = refine_ () in
        remove_shape before.model index after.model;
        remove_reachability before index after;
        Vox_table_model_proofs.remove_fields before.model index;
        Invariant.Map.distinct_remove before.model.slots (Bigint.of_int index);
        Key.reflexive key;
        Invariant.Map.same_remove before.model.slots (Bigint.of_int index) key;
        Invariant.valid_def after;
        ()
      end else ())

  let (replace_value @ total) : ('a : immutable_data).
      (before : 'a Invariant.view) @ immutable ->
      (index : int) -> (key : Key.t) @ immutable ->
      (value : 'a) @ immutable ->
      (after : 'a Invariant.view) @ immutable ->
      {u : unit | not (Invariant.valid before &&
        (match M.slot before.model index with
         | Some (Some (stored, _)) -> stored === key | _ -> false) &&
        after.model === M.set_slot before.model index (Some (key, value)) &&
        after.routes === before.routes && after.plan === before.plan) ||
        Invariant.valid after && Invariant.Map.same after.model.slots
          (Invariant.Map.put before.model.slots key value)} @ ghost =
    fun before index key value after -> ghost_ (
      if Invariant.valid before &&
         (match M.slot before.model index with
          | Some (Some (stored, _)) -> stored === key | _ -> false) &&
         after.model === M.set_slot before.model index (Some (key, value)) &&
         after.routes === before.routes && after.plan === before.plan then begin
        M.slot_def before.model index;
        M.set_slot_def before.model index (Some (key, value));
        same_keys_set before.model.slots (Bigint.of_int index) key value;
        valid_same_keys before after;
        Invariant.valid_def before; Invariant.valid_def after;
        Invariant.Map.put_distinct before.model.slots key value;
        Invariant.Map.same_intro after.model.slots
          (Invariant.Map.put before.model.slots key value)
          (fun query -> ghost_ (
            Key.reflexive key;
            Invariant.Map.replace_at before.model.slots (Bigint.of_int index)
              key value query;
            Invariant.Map.put_get before.model.slots key value query;
            ()));
        ()
      end else ())

end
