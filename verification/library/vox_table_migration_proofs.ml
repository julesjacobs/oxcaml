module S = Vox_sequence
module M = Vox_table_model

module Make (Key : Vox_table_map.Key)
    (Map : module type of Vox_table_map.Make (Key)) = struct
  let rec (take_absent @ total) : ('a : immutable_data).
      (slots : 'a Map.slots) @ immutable -> (count : Bigint.t) ->
      (key : Key.t) @ immutable ->
      {u : unit | not (Map.absent slots key) || Map.absent (S.take count
        slots) key}
      @ ghost = fun slots count key -> ghost_ (
    Map.absent_def slots key; S.take_def count slots;
    Map.absent_def (S.take count slots) key;
    match slots with
    | [] -> ()
    | _ :: tail -> if 0Z < count then take_absent tail (Bigint.sub count 1Z)
      key; ())

  let rec (take_distinct @ total) : ('a : immutable_data).
      (slots : 'a Map.slots) @ immutable -> (count : Bigint.t) ->
      {u : unit | not (Map.distinct slots) || Map.distinct (S.take count slots)}
      @ ghost = fun slots count -> ghost_ (
    Map.distinct_def slots; S.take_def count slots;
    Map.distinct_def (S.take count slots);
    match slots with
    | [] -> ()
    | entry :: tail ->
      if 0Z < count then begin
        take_distinct tail (Bigint.sub count 1Z);
        match entry with
        | None -> ()
        | Some (key, _) -> take_absent tail (Bigint.sub count 1Z) key
      end else ())

  let rec (prefix_absent @ total) : ('a : immutable_data).
      (slots : 'a Map.slots) @ immutable -> (index : Bigint.t) ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      {u : unit | not (Map.distinct slots &&
        S.at slots index === Some (Some (key, value))) ||
        Map.absent (S.take index slots) key} @ ghost =
    fun slots index key value -> ghost_ (
      Map.distinct_def slots; S.at_def slots index;
      S.take_def index slots; Map.absent_def (S.take index slots) key;
      match slots with
      | [] -> ()
      | entry :: tail ->
        if 0Z < index then begin
          prefix_absent tail (Bigint.sub index 1Z) key value;
          match entry with
          | None -> ()
          | Some (stored, _) ->
            Map.absent_at tail (Bigint.sub index 1Z) stored;
            Key.symmetric stored key
        end else ())

  let rec (prefix_next @ total) : ('a : immutable_data).
      (slots : 'a Map.slots) @ immutable -> (index : Bigint.t) ->
      (query : Key.t) @ immutable ->
      {u : unit | not (Map.distinct slots && 0Z <= index) ||
        Map.lookup (S.take (Bigint.add index 1Z) slots) query ===
          (match S.at slots index with
           | Some (Some (key, value)) ->
             if Key.equal key query then Some value
             else Map.lookup (S.take index slots) query
           | _ -> Map.lookup (S.take index slots) query)} @ ghost =
    fun slots index query -> ghost_ (
      Map.distinct_def slots; S.at_def slots index;
      S.take_def index slots; S.take_def (Bigint.add index 1Z) slots;
      Map.lookup_def (S.take index slots) query;
      Map.lookup_def (S.take (Bigint.add index 1Z) slots) query;
      match slots with
      | [] -> ()
      | entry :: tail ->
        if 0Z < index then begin
          prefix_next tail (Bigint.sub index 1Z) query;
          match entry with
          | None -> ()
          | Some (stored, _) ->
            Map.absent_at tail (Bigint.sub index 1Z) stored;
            (match S.at tail (Bigint.sub index 1Z) with
             | Some (Some (key, _)) ->
               Key.symmetric stored query; Key.transitive key query stored
             | _ -> ())
        end else begin
          S.take_def 0Z tail;
          Map.lookup_def (S.take 0Z tail) query;
          ()
        end)
  let (destination_absent @ total) : ('a : immutable_data).
      (source : 'a Map.slots) @ immutable -> (index : Bigint.t) ->
      (destination : 'a Map.slots) @ immutable ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      {u : unit | not (Map.distinct source &&
        Map.same destination (S.take index source) &&
        S.at source index === Some (Some (key, value))) ||
        Map.absent destination key} @ ghost = fun source index destination key
          value ->
    ghost_ (
      prefix_absent source index key value;
      Map.absent_lookup (S.take index source) key;
      Map.same_get destination (S.take index source) key;
      Map.absent_lookup destination key;
      ())

  let (copy_step @ total) : ('a : immutable_data).
      (source : 'a Map.slots) @ immutable -> (index : Bigint.t) ->
      (before : 'a Map.slots) @ immutable -> (after : 'a Map.slots) @
        immutable ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      {u : unit | not (Map.distinct source && Map.distinct after && 0Z <=
        index &&
        Map.same before (S.take index source) &&
        S.at source index === Some (Some (key, value)) &&
        Map.same after (Map.put before key value)) ||
        Map.same after (S.take (Bigint.add index 1Z) source)} @ ghost =
    fun source index before after key value -> ghost_ (
      if Map.distinct source && Map.distinct after && 0Z <= index &&
         Map.same before (S.take index source) &&
         S.at source index === Some (Some (key, value)) &&
         Map.same after (Map.put before key value) then begin
        take_distinct source (Bigint.add index 1Z);
        Map.same_intro after (S.take (Bigint.add index 1Z) source)
          (fun query -> ghost_ (
            Map.same_get before (S.take index source) query;
            Map.same_get after (Map.put before key value) query;
            Map.put_get before key value query;
            prefix_next source index query;
            ()));
        ()
      end else ())

  let (skip_step @ total) : ('a : immutable_data).
      (source : 'a Map.slots) @ immutable -> (index : Bigint.t) ->
      (destination : 'a Map.slots) @ immutable ->
      {u : unit | not (Map.distinct source && Map.distinct destination &&
        0Z <= index && S.at source index === Some None &&
        Map.same destination (S.take index source)) ||
        Map.same destination (S.take (Bigint.add index 1Z) source)} @ ghost =
    fun source index destination -> ghost_ (
      if Map.distinct source && Map.distinct destination && 0Z <= index &&
         S.at source index === Some None && Map.same destination (S.take index
           source)
      then begin
        take_distinct source (Bigint.add index 1Z);
        Map.same_intro destination (S.take (Bigint.add index 1Z) source)
          (fun query -> ghost_ (
            Map.same_get destination (S.take index source) query;
            prefix_next source index query;
            ()));
        ()
      end else ())

  let (empty_start @ total) : ('a : immutable_data).
      (source : 'a Map.slots) @ immutable -> (capacity : int) ->
      (entry : (Key.t * 'a) option) @ immutable ->
      {u : unit | not (entry === None &&
        Map.distinct (M.repeat capacity entry)) ||
        Map.same (M.repeat capacity entry) (S.take 0Z source)} @ ghost =
    fun source capacity entry -> ghost_ (
      if entry === None && Map.distinct (M.repeat capacity entry) then begin
      S.take_def 0Z source;
      Map.distinct_def (S.take 0Z source);
      Map.same_intro (M.repeat capacity entry) (S.take 0Z source)
        (fun query -> ghost_ (
          Map.empty_lookup capacity entry query;
          Map.lookup_def (S.take 0Z source) query;
          ()));
      ()
      end else ())
end
