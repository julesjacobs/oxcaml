(* The finite-map model of the public hash table interface.

   [Assoc] defines association lists and proves their laws. [t] restricts
   them to lists with at most one binding per [Key.equal] class; under that
   invariant [count] is the number of bindings, so the size laws hold. *)
module Make (Key : Vox_table_map.Key) = struct
  module Assoc = struct
    type ('a : immutable_data) t = (Key.t * 'a) list

    let[@def] rec (lookup @ total) (bindings : 'a t) (key : Key.t) =
      match bindings with
      | [] -> None
      | (stored, value) :: tail ->
        if Key.equal stored key then Some value else lookup tail key

    let[@def] rec (distinct @ total) (bindings : 'a t) = ghost_ (
      match bindings with
      | [] -> true
      | (key, _) :: tail -> lookup tail key === None && distinct tail)

    let[@def] rec (agrees @ total) (left : 'a t) (right : 'a t) = ghost_ (
      match left with
      | [] -> true
      | (key, value) :: tail ->
        lookup right key === Some value && agrees tail right)

    let[@def] (same @ total) (left : 'a t) (right : 'a t) = ghost_ (
      agrees left right && agrees right left)

    let[@def] rec (erase @ total) (bindings : 'a t) (key : Key.t) =
      match bindings with
      | [] -> []
      | (stored, value) :: tail ->
        if Key.equal stored key then erase tail key
        else (stored, value) :: erase tail key

    let[@def] (put @ total) (bindings : 'a t) (key : Key.t) (value : 'a) =
      (key, value) :: erase bindings key

    let[@def] rec (count @ total) (bindings : 'a t) =
      match bindings with
      | [] -> 0Z
      | _ :: tail -> Bigint.add 1Z (count tail)

    let rec (lookup_congruent @ total) : ('a : immutable_data).
        (bindings : 'a t) -> (left : Key.t) -> (right : Key.t) ->
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
        (left : 'a t) -> (right : 'a t) -> (key : Key.t) ->
        {u : unit | not (agrees left right) || lookup left key === None ||
          lookup left key === lookup right key} @ ghost =
      fun left right key -> ghost_ (
        agrees_def left right; lookup_def left key;
        match left with
        | [] -> ()
        | (stored, _) :: tail ->
          lookup_congruent right stored key; agrees_get tail right key)

    let (same_get @ total) : ('a : immutable_data).
        (left : 'a t) -> (right : 'a t) -> (key : Key.t) ->
        {u : unit | not (same left right) ||
          lookup left key === lookup right key} @ ghost =
      fun left right key -> ghost_ (
        same_def left right; agrees_get left right key;
        agrees_get right left key; ())

    let rec (erase_get @ total) : ('a : immutable_data).
        (bindings : 'a t) -> (key : Key.t) -> (query : Key.t) ->
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
        (bindings : 'a t) -> (key : Key.t) -> (value : 'a) -> (query : Key.t) ->
        {u : unit | lookup (put bindings key value) query ===
          (if Key.equal key query then Some value else lookup bindings query)} @ ghost =
      fun bindings key value query -> ghost_ (
        put_def bindings key value; lookup_def (put bindings key value) query;
        erase_get bindings key query; ())

    let rec (erase_absent @ total) : ('a : immutable_data).
        (bindings : 'a t) -> (key : Key.t) ->
        {u : unit | not (lookup bindings key === None) ||
          erase bindings key === bindings} @ ghost =
      fun bindings key -> ghost_ (
        lookup_def bindings key; erase_def bindings key;
        match bindings with
        | [] -> ()
        | _ :: tail -> erase_absent tail key)

    let rec (erase_distinct @ total) : ('a : immutable_data).
        (bindings : 'a t) -> (key : Key.t) ->
        {u : unit | not (distinct bindings) || distinct (erase bindings key)} @ ghost =
      fun bindings key -> ghost_ (
        distinct_def bindings; erase_def bindings key;
        distinct_def (erase bindings key);
        match bindings with
        | [] -> ()
        | (stored, _) :: tail ->
          erase_get tail key stored; erase_distinct tail key)

    let (put_distinct @ total) : ('a : immutable_data).
        (bindings : 'a t) -> (key : Key.t) -> (value : 'a) ->
        {u : unit | not (distinct bindings) ||
          distinct (put bindings key value)} @ ghost =
      fun bindings key value -> ghost_ (
        put_def bindings key value; distinct_def (put bindings key value);
        Key.reflexive key; erase_get bindings key key;
        erase_distinct bindings key)

    (* Erasing a key from both sides preserves agreement. *)
    let rec (erase_agrees @ total) : ('a : immutable_data).
        (left : 'a t) -> (right : 'a t) -> (key : Key.t) ->
        {u : unit | not (agrees left right) ||
          agrees (erase left key) (erase right key)} @ ghost =
      fun left right key -> ghost_ (
        agrees_def left right; erase_def left key;
        agrees_def (erase left key) (erase right key);
        match left with
        | [] -> ()
        | (stored, _) :: tail ->
          Key.symmetric key stored; erase_get right key stored;
          erase_agrees tail right key)

    (* The bindings of [left] other than [key] survive [put right key]. *)
    let rec (erase_put_agrees @ total) : ('a : immutable_data).
        (left : 'a t) -> (right : 'a t) -> (key : Key.t) -> (value : 'a) ->
        {u : unit | not (agrees left right) ||
          agrees (erase left key) (put right key value)} @ ghost =
      fun left right key value -> ghost_ (
        agrees_def left right; erase_def left key;
        agrees_def (erase left key) (put right key value);
        match left with
        | [] -> ()
        | (stored, _) :: tail ->
          Key.symmetric key stored; put_get right key value stored;
          erase_put_agrees tail right key value)

    let rec (agrees_trans @ total) : ('a : immutable_data).
        (left : 'a t) -> (middle : 'a t) -> (right : 'a t) ->
        {u : unit | not (agrees left middle && agrees middle right) ||
          agrees left right} @ ghost =
      fun left middle right -> ghost_ (
        agrees_def left middle; agrees_def left right;
        match left with
        | [] -> ()
        | (key, _) :: tail ->
          agrees_get middle right key; agrees_trans tail middle right)

    let (same_trans @ total) : ('a : immutable_data).
        (left : 'a t) -> (middle : 'a t) -> (right : 'a t) ->
        {u : unit | not (same left middle && same middle right) ||
          same left right} @ ghost =
      fun left middle right -> ghost_ (
        same_def left middle; same_def middle right; same_def left right;
        agrees_trans left middle right; agrees_trans right middle left)

    let (same_put @ total) : ('a : immutable_data).
        (left : 'a t) -> (right : 'a t) -> (key : Key.t) -> (value : 'a) ->
        {u : unit | not (same left right) ||
          same (put left key value) (put right key value)} @ ghost =
      fun left right key value -> ghost_ (
        same_def left right; same_def (put left key value) (put right key value);
        put_def left key value; put_def right key value;
        agrees_def (put left key value) (put right key value);
        agrees_def (put right key value) (put left key value);
        Key.reflexive key;
        put_get right key value key; put_get left key value key;
        erase_put_agrees left right key value;
        erase_put_agrees right left key value)

    let (same_erase @ total) : ('a : immutable_data).
        (left : 'a t) -> (right : 'a t) -> (key : Key.t) ->
        {u : unit | not (same left right) ||
          same (erase left key) (erase right key)} @ ghost =
      fun left right key -> ghost_ (
        same_def left right; same_def (erase left key) (erase right key);
        erase_agrees left right key; erase_agrees right left key)

    let rec (count_nonnegative @ total) : ('a : immutable_data).
        (bindings : 'a t) -> {u : unit | 0Z <= count bindings} @ ghost =
      fun bindings -> ghost_ (
        count_def bindings;
        match bindings with
        | [] -> ()
        | _ :: tail -> count_nonnegative tail)

    let rec (count_erase @ total) : ('a : immutable_data).
        (bindings : 'a t) -> (key : Key.t) ->
        {u : unit | not (distinct bindings) ||
          count (erase bindings key) = (if lookup bindings key === None
            then count bindings else Bigint.sub (count bindings) 1Z)} @ ghost =
      fun bindings key -> ghost_ (
        distinct_def bindings; erase_def bindings key; lookup_def bindings key;
        count_def bindings; count_def (erase bindings key);
        match bindings with
        | [] -> ()
        | (stored, _) :: tail ->
          lookup_congruent tail stored key; erase_absent tail key;
          count_erase tail key)

    let (count_put @ total) : ('a : immutable_data).
        (bindings : 'a t) -> (key : Key.t) -> (value : 'a) ->
        {u : unit | not (distinct bindings) ||
          count (put bindings key value) = (if lookup bindings key === None
            then Bigint.add (count bindings) 1Z else count bindings)} @ ghost =
      fun bindings key value -> ghost_ (
        put_def bindings key value; count_def (put bindings key value);
        count_erase bindings key)

    (* Induct on [left]: removing its first key from both sides leaves two
       agreeing distinct lists, each one binding shorter. *)
    let rec (same_count @ total) : ('a : immutable_data).
        (left : 'a t) -> (right : 'a t) ->
        {u : unit | not (distinct left && distinct right && same left right) ||
          count left = count right} @ ghost =
      fun left right -> ghost_ (
        same_def left right; agrees_def left right; agrees_def right left;
        distinct_def left; count_def left;
        match left with
        | [] -> (
          match right with
          | [] -> ()
          | (stored, _) :: _ -> lookup_def left stored)
        | (key, _) :: tail ->
          Key.reflexive key; erase_def left key; erase_absent tail key;
          erase_agrees left right key; erase_agrees right left key;
          same_def tail (erase right key);
          erase_distinct right key; count_erase right key;
          same_count tail (erase right key))
  end

  type ('a : immutable_data) t = {entries : 'a Assoc.t | Assoc.distinct entries}

  let[@def] (lookup @ total) (map : 'a t) (key : Key.t) = Assoc.lookup map key

  let[@def] (put @ total) (map : 'a t) (key : Key.t) (value : 'a) : 'a t =
    ghost_ (Assoc.put_distinct map key value);
    Assoc.put map key value

  let[@def] (erase @ total) (map : 'a t) (key : Key.t) : 'a t =
    ghost_ (Assoc.erase_distinct map key);
    Assoc.erase map key

  let[@def] (count @ total) (map : 'a t) = Assoc.count map

  let (empty @ total) : ('a : immutable_data). {map : 'a t | count map = 0Z} =
    (* The annotation stops [entries] from being generalized, so the proof
       below and the result are about the same instance. *)
    let entries = ([] : 'b Assoc.t) in
    ghost_ (Assoc.distinct_def entries; Assoc.count_def entries;
      count_def entries);
    entries

  (* [empty] is the empty list, so it agrees with any other empty list. *)
  let (empty_same @ total) : ('a : immutable_data).
      (map : 'a t) -> (other : 'a Assoc.t) ->
      {u : unit | not (map === empty && other === []) || Assoc.same map other}
      @ ghost =
    fun map other -> ghost_ (
      count_def map; Assoc.count_def map;
      Assoc.same_def map other; Assoc.agrees_def map other;
      Assoc.agrees_def other map;
      match (map : _ Assoc.t) with
      | [] -> ()
      | _ :: tail -> Assoc.count_nonnegative tail)

  let (lookup_empty @ total) : ('a : immutable_data). (map : 'a t) ->
      (key : Key.t) ->
      {u : unit | not (map === empty) || lookup map key === None} @ ghost =
    fun map key -> ghost_ (
      count_def map; Assoc.count_def map; lookup_def map key;
      Assoc.lookup_def map key;
      match (map : _ Assoc.t) with
      | [] -> ()
      | _ :: tail -> Assoc.count_nonnegative tail)

  let (put_get @ total) : ('a : immutable_data).
      (map : 'a t) -> (key : Key.t) -> (value : 'a) -> (query : Key.t) ->
      {u : unit | lookup (put map key value) query ===
        (if Key.equal key query then Some value else lookup map query)} @ ghost =
    fun map key value query -> ghost_ (
      put_def map key value; lookup_def (put map key value) query;
      lookup_def map query; Assoc.put_get map key value query)

  let (erase_get @ total) : ('a : immutable_data).
      (map : 'a t) -> (key : Key.t) -> (query : Key.t) ->
      {u : unit | lookup (erase map key) query ===
        (if Key.equal key query then None else lookup map query)} @ ghost =
    fun map key query -> ghost_ (
      erase_def map key; lookup_def (erase map key) query;
      lookup_def map query; Assoc.erase_get map key query)

  let (count_put @ total) : ('a : immutable_data).
      (map : 'a t) -> (key : Key.t) -> (value : 'a) ->
      {u : unit | count (put map key value) = (if lookup map key === None
        then Bigint.add (count map) 1Z else count map)} @ ghost =
    fun map key value -> ghost_ (
      put_def map key value; count_def (put map key value); count_def map;
      lookup_def map key; Assoc.count_put map key value)

  let (count_erase @ total) : ('a : immutable_data).
      (map : 'a t) -> (key : Key.t) ->
      {u : unit | count (erase map key) = (if lookup map key === None
        then count map else Bigint.sub (count map) 1Z)} @ ghost =
    fun map key -> ghost_ (
      erase_def map key; count_def (erase map key); count_def map;
      lookup_def map key; Assoc.count_erase map key)

end
