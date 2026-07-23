external int_equal : int -> int -> bool @@ total = "%equal"
external int_less : int -> int -> bool @@ total = "%lessthan"
external int_leq : int -> int -> bool @@ total = "%lessequal"
external list_equal :
  int list @ logical -> int list @ logical -> bool @@ total = "%equal"

let[@vox.def] rec len (values : int list @ logical) =
  match values with
  | [] -> 0
  | _ :: rest -> 1 + len rest

let[@vox.def] rec elem (values : int list @ logical) (index : int) =
  match values with
  | [] -> 0
  | value :: rest ->
    if int_equal index 0 then value else elem rest (index - 1)

let[@vox.def] rec update (values : int list @ logical) (index : int)
    (value : int) =
  match values with
  | [] -> []
  | old :: rest ->
    if int_equal index 0
    then value :: rest
    else old :: update rest (index - 1) value

let[@vox.def] rec take (count : int) (values : int list @ logical) =
  match values with
  | [] -> []
  | value :: rest ->
    if int_leq count 0 then [] else value :: take (count - 1) rest

let[@vox.def] rec drop (count : int) (values : int list @ logical) =
  match values with
  | [] -> []
  | _ :: rest ->
    if int_leq count 0 then values else drop (count - 1) rest

let[@vox.def] rec append (left : int list @ logical)
    (right : int list @ logical) =
  match left with
  | [] -> right
  | value :: rest -> value :: append rest right

let[@vox.def] singleton (value : int) : int list = [value]

let[@vox.def] segment first last (values : int list @ logical) =
  take (last - first) (drop first values)

let[@vox.def] rec all_le (values : int list @ logical) bound =
  match values with
  | [] -> true
  | value :: rest ->
    if int_leq value bound then all_le rest bound else false

let[@vox.def] rec all_ge (values : int list @ logical) bound =
  match values with
  | [] -> true
  | value :: rest ->
    if int_leq bound value then all_ge rest bound else false

let[@vox.def] rec sorted (values : int list @ logical) =
  match values with
  | [] -> true
  | value :: rest ->
    if all_ge rest value then sorted rest else false

let[@vox.def] rec insert_bag value (values : int list @ logical) =
  match values with
  | [] -> [value]
  | head :: rest ->
    if int_leq value head
    then value :: values
    else head :: insert_bag value rest

let[@vox.def] rec bag (values : int list @ logical) =
  match values with
  | [] -> []
  | value :: rest -> insert_bag value (bag rest)

let[@vox.def] rec bag_union (left : int list @ logical)
    (right : int list @ logical) =
  match left with
  | [] -> right
  | value :: rest -> insert_bag value (bag_union rest right)

let[@vox.def] perm (left : int list @ logical)
    (right : int list @ logical) =
  list_equal (bag left) (bag right)

let rec len_update_law ~(values : int list @ logical)
    ~(index : int) ~(value : int)
    : unit{ len (update values index value) = len values } =
  match values with
  | [] ->
    update_def [] index value;
    len_def [];
    ()
  | head :: rest ->
    update_def (head :: rest) index value;
    len_def (head :: rest);
    if int_equal index 0
    then begin
      len_def (value :: rest);
      ()
    end
    else begin
      len_update_law ~values:rest ~index:(index - 1) ~value;
      len_def (head :: update rest (index - 1) value);
      ()
    end

let rec len_take_law
    ~(count : int{ 0 <= _ })
    ~(values : int list @ logical)
    ~bounded:(_bounded : unit{ count <= len values })
    : unit{ len (take count values) = count } =
  match values with
  | [] ->
    len_def [];
    take_def count [];
    ()
  | head :: rest ->
    len_def (head :: rest);
    take_def count (head :: rest);
    if int_leq count 0
    then begin
      len_def [];
      ()
    end
    else begin
      len_def rest;
      len_take_law
        ~count:(count - 1) ~values:rest ~bounded:();
      len_def (head :: take (count - 1) rest);
      ()
    end

let finish_insert_commute first second (values : int list @ logical)
    (_proof : unit{
       insert_bag first (insert_bag second values)
       = insert_bag second (insert_bag first values)
     })
    : unit{
        insert_bag first (insert_bag second values)
        = insert_bag second (insert_bag first values)
      } =
  ()

let rec insert_commute_law ~(first : int) ~(second : int)
    ~(values : int list @ logical)
    : unit{
        insert_bag first (insert_bag second values)
        = insert_bag second (insert_bag first values)
      } =
  match values with
  | [] ->
    insert_bag_def first [];
    insert_bag_def second [];
    insert_bag_def first [second];
    insert_bag_def second [first];
    if int_leq first second
    then finish_insert_commute first second values ()
    else finish_insert_commute first second values ()
  | head :: rest ->
    insert_bag_def first (head :: rest);
    insert_bag_def second (head :: rest);
    if int_leq first head
    then begin
      insert_bag_def second (first :: head :: rest);
      if int_leq second head
      then begin
        insert_bag_def first (second :: head :: rest);
        if int_leq first second
        then finish_insert_commute first second values ()
        else finish_insert_commute first second values ()
      end
      else begin
        insert_bag_def second rest;
        insert_bag_def first (head :: insert_bag second rest);
        finish_insert_commute first second values ()
      end
    end
    else begin
      insert_bag_def first rest;
      insert_bag_def second (head :: insert_bag first rest);
      if int_leq second head
      then begin
        insert_bag_def first (second :: head :: rest);
        finish_insert_commute first second values ()
      end
      else begin
        insert_bag_def second rest;
        insert_bag_def first (head :: insert_bag second rest);
        insert_commute_law ~first ~second ~values:rest;
        finish_insert_commute first second values ()
      end
    end

let finish_exchange_head head (values : int list @ logical) index
    (_proof : unit{
       bag (elem values index :: update values index head)
       = bag (head :: values)
     })
    : unit{
        bag (elem values index :: update values index head)
        = bag (head :: values)
      } =
  ()

let rec bag_exchange_head_law ~(head : int)
    ~(values : int list @ logical)
    ~(index : int{ 0 <= _ && _ < len values })
    : unit{
        bag (elem values index :: update values index head)
        = bag (head :: values)
      } =
  match values with
  | [] ->
    len_def [];
    finish_exchange_head head values index ()
  | value :: rest ->
    len_def (value :: rest);
    elem_def (value :: rest) index;
    update_def (value :: rest) index head;
    bag_def (value :: rest);
    bag_def (head :: value :: rest);
    if int_equal index 0
    then begin
      bag_def (value :: head :: rest);
      bag_def (head :: rest);
      bag_def rest;
      insert_commute_law ~first:value ~second:head ~values:(bag rest);
      finish_exchange_head head values index ()
    end
    else begin
      len_def rest;
      bag_exchange_head_law ~head ~values:rest ~index:(index - 1);
      bag_def
        (elem rest (index - 1)
         :: value :: update rest (index - 1) head);
      bag_def (value :: update rest (index - 1) head);
      bag_def (update rest (index - 1) head);
      bag_def
        (elem rest (index - 1) :: update rest (index - 1) head);
      bag_def (head :: value :: rest);
      bag_def (head :: rest);
      bag_def (value :: rest);
      bag_def rest;
      insert_commute_law
        ~first:(elem rest (index - 1)) ~second:value
        ~values:(bag (update rest (index - 1) head));
      insert_commute_law
        ~first:head ~second:value ~values:(bag rest);
      finish_exchange_head head values index ()
    end

let finish_bag_swap (values : int list @ logical) first second
    (_proof : unit{
       bag
         (update
            (update values first (elem values second))
            second (elem values first))
       = bag values
     })
    : unit{
        bag
          (update
             (update values first (elem values second))
             second (elem values first))
        = bag values
      } =
  ()

let rec bag_swap_law ~(values : int list @ logical)
    ~(first : int{ 0 <= _ && _ < len values })
    ~(second : int{ 0 <= _ && _ < len values })
    : unit{
        bag
          (update
             (update values first (elem values second))
             second (elem values first))
        = bag values
      } =
  match values with
  | [] ->
    len_def [];
    finish_bag_swap values first second ()
  | head :: rest ->
    len_def (head :: rest);
    elem_def (head :: rest) first;
    elem_def (head :: rest) second;
    update_def (head :: rest) first (elem (head :: rest) second);
    if int_equal first 0
    then begin
      if int_equal second 0
      then begin
        update_def (head :: rest) second head;
        bag_def (head :: rest);
        finish_bag_swap values first second ()
      end
      else begin
        update_def
          (elem rest (second - 1) :: rest) second head;
        update_def rest (second - 1) head;
        bag_exchange_head_law
          ~head ~values:rest ~index:(second - 1);
        finish_bag_swap values first second ()
      end
    end
    else begin
      if int_equal second 0
      then begin
        update_def
          (head :: update rest (first - 1) (elem (head :: rest) second))
          second (elem rest (first - 1));
        update_def rest (first - 1) head;
        bag_exchange_head_law
          ~head ~values:rest ~index:(first - 1);
        finish_bag_swap values first second ()
      end
      else begin
        update_def rest (first - 1) (elem rest (second - 1));
        update_def
          (head
           :: update rest (first - 1) (elem rest (second - 1)))
          second (elem rest (first - 1));
        update_def
          (update rest (first - 1) (elem rest (second - 1)))
          (second - 1) (elem rest (first - 1));
        bag_swap_law
          ~values:rest ~first:(first - 1) ~second:(second - 1);
        bag_def
          (head
           :: update
                (update rest (first - 1) (elem rest (second - 1)))
                (second - 1) (elem rest (first - 1)));
        bag_def (head :: rest);
        finish_bag_swap values first second ()
      end
    end

let perm_swap_law ~(values : int list @ logical)
    ~(first : int{ 0 <= _ && _ < len values })
    ~(second : int{ 0 <= _ && _ < len values })
    : unit{
        perm values
          (update
             (update values first (elem values second))
             second (elem values first))
        = true
      } =
  bag_swap_law ~values ~first ~second;
  perm_def values
    (update
       (update values first (elem values second))
       second (elem values first));
  ()

let finish_take_update count index value (values : int list @ logical)
    (_proof : unit{
       take count (update values index value) = take count values
     })
    : unit{
        take count (update values index value) = take count values
      } =
  ()

let rec take_update_ge_law ~(count : int) ~(index : int)
    ~(value : int) ~(values : int list @ logical)
    ~proof:(_proof : unit{ count <= index })
    : unit{
        take count (update values index value) = take count values
      } =
  match values with
  | [] ->
    update_def [] index value;
    take_def count [];
    finish_take_update count index value values ()
  | head :: rest ->
    update_def (head :: rest) index value;
    take_def count (head :: rest);
    if int_leq count 0
    then begin
      take_def count (update (head :: rest) index value);
      finish_take_update count index value values ()
    end
    else begin
      if int_equal index 0
      then finish_take_update count index value values ()
      else begin
        take_update_ge_law
          ~count:(count - 1) ~index:(index - 1) ~value ~values:rest
          ~proof:();
        take_def count (head :: update rest (index - 1) value);
        take_def (count - 1) rest;
        take_def (count - 1) (update rest (index - 1) value);
        finish_take_update count index value values ()
      end
    end

let rec take_nonpositive_law ~(count : int)
    ~(values : int list @ logical) ~proof:(_proof : unit{ count <= 0 })
    : unit{ take count values = [] } =
  match values with
  | [] ->
    take_def count [];
    ()
  | head :: rest ->
    take_def count (head :: rest);
    if int_leq count 0 then () else ()

let finish_elem_update values update_index query value
    (_proof : unit{
       elem (update values update_index value) query
       = if query = update_index then value else elem values query
     })
    : unit{
        elem (update values update_index value) query
        = if query = update_index then value else elem values query
      } =
  ()

let rec elem_update_law ~(values : int list @ logical)
    ~(update_index : int{
       0 <= _ && _ < len values
     })
    ~(query : int) ~(value : int)
    : unit{
        elem (update values update_index value) query
        = if query = update_index then value else elem values query
      } =
  match values with
  | [] ->
    len_def [];
    finish_elem_update values update_index query value ()
  | head :: rest ->
    len_def (head :: rest);
    update_def (head :: rest) update_index value;
    elem_def (head :: rest) query;
    if int_equal update_index 0
    then begin
      elem_def (value :: rest) query;
      if int_equal query 0
      then finish_elem_update values update_index query value ()
      else finish_elem_update values update_index query value ()
    end
    else begin
      len_def rest;
      elem_def (head :: update rest (update_index - 1) value) query;
      elem_update_law
        ~values:rest ~update_index:(update_index - 1)
        ~query:(query - 1) ~value;
      elem_def (update rest (update_index - 1) value) (query - 1);
      if int_equal query 0
      then finish_elem_update values update_index query value ()
      else finish_elem_update values update_index query value ()
    end

let finish_all_le_append left right bound
    (_proof : unit{
       all_le (append left right) bound
       = (all_le left bound && all_le right bound)
     })
    : unit{
        all_le (append left right) bound
        = (all_le left bound && all_le right bound)
      } =
  ()

let rec all_le_append_law ~(left : int list @ logical)
    ~(right : int list @ logical) ~(bound : int)
    : unit{
        all_le (append left right) bound
        = (all_le left bound && all_le right bound)
      } =
  match left with
  | [] ->
    append_def [] right;
    all_le_def [] bound;
    finish_all_le_append left right bound ()
  | head :: rest ->
    append_def (head :: rest) right;
    all_le_def (head :: rest) bound;
    all_le_def (head :: append rest right) bound;
    if int_leq head bound
    then begin
      all_le_append_law ~left:rest ~right ~bound;
      finish_all_le_append left right bound ()
    end
    else finish_all_le_append left right bound ()

let finish_all_ge_append left right bound
    (_proof : unit{
       all_ge (append left right) bound
       = (all_ge left bound && all_ge right bound)
     })
    : unit{
        all_ge (append left right) bound
        = (all_ge left bound && all_ge right bound)
      } =
  ()

let rec all_ge_append_law ~(left : int list @ logical)
    ~(right : int list @ logical) ~(bound : int)
    : unit{
        all_ge (append left right) bound
        = (all_ge left bound && all_ge right bound)
      } =
  match left with
  | [] ->
    append_def [] right;
    all_ge_def [] bound;
    finish_all_ge_append left right bound ()
  | head :: rest ->
    append_def (head :: rest) right;
    all_ge_def (head :: rest) bound;
    all_ge_def (head :: append rest right) bound;
    if int_leq bound head
    then begin
      all_ge_append_law ~left:rest ~right ~bound;
      finish_all_ge_append left right bound ()
    end
    else finish_all_ge_append left right bound ()

let empty_bounds_law ~(bound : int)
    : unit{ all_le [] bound = true && all_ge [] bound = true } =
  all_le_def [] bound;
  all_ge_def [] bound;
  ()

let finish_take_snoc index (values : int list @ logical)
    (_proof : unit{
       take (index + 1) values
       = append (take index values) (singleton (elem values index))
     })
    : unit{
        take (index + 1) values
        = append (take index values) (singleton (elem values index))
      } =
  ()

let rec take_snoc_law ~(values : int list @ logical)
    ~(index : int{
       0 <= _ && _ < len values
     })
    : unit{
        take (index + 1) values
        = append (take index values) (singleton (elem values index))
      } =
  match values with
  | [] ->
    len_def [];
    finish_take_snoc index values ()
  | head :: rest ->
    len_def (head :: rest);
    elem_def (head :: rest) index;
    take_def (index + 1) (head :: rest);
    take_def index (head :: rest);
    if int_equal index 0
    then begin
      take_def 0 rest;
      take_def 1 (head :: rest);
      singleton_def head;
      append_def [] [head];
      finish_take_snoc index values ()
    end
    else begin
      take_snoc_law ~values:rest ~index:(index - 1);
      append_def
        (head :: take (index - 1) rest)
        (singleton (elem rest (index - 1)));
      finish_take_snoc index values ()
    end

let rec len_nonnegative_law ~(values : int list @ logical)
    : unit{ 0 <= len values } =
  match values with
  | [] ->
    len_def [];
    ()
  | _ :: rest ->
    len_def values;
    len_nonnegative_law ~values:rest;
    ()

let rec take_all_law ~(values : int list @ logical)
    : unit{ take (len values) values = values } =
  match values with
  | [] ->
    len_def [];
    take_def 0 [];
    ()
  | head :: rest ->
    len_def (head :: rest);
    len_nonnegative_law ~values:rest;
    take_def (1 + len rest) (head :: rest);
    take_all_law ~values:rest;
    ()

let swap_le_law ~(values : int list @ logical) ~(pivot : int)
    ~(lower : int{ 0 <= _ })
    ~(scan : int{ lower <= _ && _ < len values })
    ~prefix:(_prefix : unit{
      all_le (take lower values) pivot = true
    })
    ~scanned:(_scanned : unit{ elem values scan <= pivot })
    : unit{
        all_le
          (take (lower + 1)
             (update
                (update values lower (elem values scan))
                scan (elem values lower)))
          pivot
        = true
      } =
  let first_update = update values lower (elem values scan) in
  let swapped = update first_update scan (elem values lower) in
  len_update_law ~values ~index:lower ~value:(elem values scan);
  len_update_law
    ~values:first_update ~index:scan ~value:(elem values lower);
  take_update_ge_law
    ~count:lower ~index:lower ~value:(elem values scan) ~values
    ~proof:();
  take_update_ge_law
    ~count:lower ~index:scan ~value:(elem values lower)
    ~values:first_update ~proof:();
  take_snoc_law ~values:swapped ~index:lower;
  elem_update_law
    ~values:first_update ~update_index:scan ~query:lower
    ~value:(elem values lower);
  elem_update_law
    ~values ~update_index:lower ~query:lower
    ~value:(elem values scan);
  all_le_append_law
    ~left:(take lower swapped)
    ~right:(singleton (elem swapped lower)) ~bound:pivot;
  singleton_def (elem swapped lower);
  all_le_def [elem swapped lower] pivot;
  all_le_def [] pivot;
  if int_equal lower scan
  then begin
    if int_leq (elem swapped lower) pivot then () else ()
  end
  else begin
    if int_leq (elem swapped lower) pivot then () else ()
  end

let perm_refl_law ~(values : int list @ logical)
    : unit{ perm values values = true } =
  perm_def values values;
  ()

let perm_trans_law ~(first : int list @ logical)
    ~(second : int list @ logical) ~(third : int list @ logical)
    ~left:(_left : unit{ perm first second = true })
    ~right:(_right : unit{ perm second third = true })
    : unit{ perm first third = true } =
  perm_def first second;
  perm_def second third;
  perm_def first third;
  ()

let finish_len_drop count (values : int list @ logical)
    (_proof : unit{ len (drop count values) = len values - count })
    : unit{ len (drop count values) = len values - count } =
  ()

let rec len_drop_law ~(count : int{ 0 <= _ })
    ~(values : int list @ logical)
    ~bounded:(_bounded : unit{ count <= len values })
    : unit{ len (drop count values) = len values - count } =
  match values with
  | [] ->
    len_def [];
    drop_def count [];
    finish_len_drop count values ()
  | head :: rest ->
    len_def (head :: rest);
    drop_def count (head :: rest);
    if int_leq count 0
    then finish_len_drop count values ()
    else begin
      len_drop_law ~count:(count - 1) ~values:rest ~bounded:();
      finish_len_drop count values ()
    end

let len_segment_law
    ~(values : int list @ logical)
    ~(first : int{ 0 <= _ })
    ~(last : int{ first <= _ && _ <= len values })
    : unit{ len (segment first last values) = last - first } =
  len_drop_law ~count:first ~values ~bounded:();
  len_take_law
    ~count:(last - first) ~values:(drop first values) ~bounded:();
  segment_def first last values;
  ()

let finish_elem_drop count index (values : int list @ logical)
    (_proof : unit{
       elem (drop count values) index = elem values (count + index)
     })
    : unit{
        elem (drop count values) index = elem values (count + index)
      } =
  ()

let rec elem_drop_law ~(count : int{ 0 <= _ })
    ~(index : int{ 0 <= _ }) ~(values : int list @ logical)
    : unit{
        elem (drop count values) index = elem values (count + index)
      } =
  match values with
  | [] ->
    drop_def count [];
    elem_def [] index;
    elem_def [] (count + index);
    finish_elem_drop count index values ()
  | head :: rest ->
    drop_def count (head :: rest);
    if int_leq count 0
    then finish_elem_drop count index values ()
    else begin
      elem_drop_law ~count:(count - 1) ~index ~values:rest;
      elem_def (head :: rest) (count + index);
      finish_elem_drop count index values ()
    end

let finish_drop_update_lt count index value
    (values : int list @ logical)
    (_proof : unit{
       drop count (update values index value) = drop count values
     })
    : unit{
        drop count (update values index value) = drop count values
      } =
  ()

let rec drop_update_lt_law ~(count : int)
    ~(index : int{ 0 <= _ && _ < count }) ~(value : int)
    ~(values : int list @ logical)
    : unit{
        drop count (update values index value) = drop count values
      } =
  match values with
  | [] ->
    update_def [] index value;
    drop_def count [];
    finish_drop_update_lt count index value values ()
  | head :: rest ->
    update_def (head :: rest) index value;
    drop_def count (head :: rest);
    if int_equal index 0
    then begin
      drop_def count (value :: rest);
      finish_drop_update_lt count index value values ()
    end
    else begin
      drop_def count (head :: update rest (index - 1) value);
      drop_update_lt_law
        ~count:(count - 1) ~index:(index - 1) ~value ~values:rest;
      finish_drop_update_lt count index value values ()
    end

let finish_drop_update_ge count index value
    (values : int list @ logical)
    (_proof : unit{
       drop count (update values index value)
       = update (drop count values) (index - count) value
     })
    : unit{
        drop count (update values index value)
        = update (drop count values) (index - count) value
      } =
  ()

let rec drop_update_ge_law ~(count : int{ 0 <= _ })
    ~(index : int{ count <= _ }) ~(value : int)
    ~(values : int list @ logical)
    : unit{
        drop count (update values index value)
        = update (drop count values) (index - count) value
      } =
  match values with
  | [] ->
    update_def [] index value;
    drop_def count [];
    update_def [] (index - count) value;
    finish_drop_update_ge count index value values ()
  | head :: rest ->
    update_def (head :: rest) index value;
    drop_def count (head :: rest);
    if int_leq count 0
    then begin
      drop_def count (update (head :: rest) index value);
      finish_drop_update_ge count index value values ()
    end
    else begin
      drop_def count (head :: update rest (index - 1) value);
      drop_update_ge_law
        ~count:(count - 1) ~index:(index - 1) ~value ~values:rest;
      finish_drop_update_ge count index value values ()
    end

let finish_drop_cons index (values : int list @ logical)
    (_proof : unit{
       drop index values
       = append
           (singleton (elem values index))
           (drop (index + 1) values)
     })
    : unit{
        drop index values
        = append
            (singleton (elem values index))
            (drop (index + 1) values)
      } =
  ()

let rec drop_cons_law
    ~(values : int list @ logical)
    ~(index : int{ 0 <= _ && _ < len values })
    : unit{
        drop index values
        = append
            (singleton (elem values index))
            (drop (index + 1) values)
      } =
  match values with
  | [] ->
    len_def [];
    finish_drop_cons index values ()
  | head :: rest ->
    len_def (head :: rest);
    drop_def index (head :: rest);
    elem_def (head :: rest) index;
    drop_def (index + 1) (head :: rest);
    if int_equal index 0
    then begin
      drop_def 0 (head :: rest);
      drop_def 1 (head :: rest);
      drop_def 0 rest;
      singleton_def head;
      append_def [head] rest;
      append_def [] rest;
      finish_drop_cons index values ()
    end
    else begin
      drop_cons_law ~index:(index - 1) ~values:rest;
      finish_drop_cons index values ()
    end

let segment_snoc_law ~(values : int list @ logical)
    ~(first : int{ 0 <= _ })
    ~(last : int{ first <= _ && _ < len values })
    : unit{
        segment first (last + 1) values
        = append
            (segment first last values)
            (singleton (elem values last))
      } =
  segment_def first (last + 1) values;
  segment_def first last values;
  len_drop_law ~count:first ~values ~bounded:();
  take_snoc_law
    ~values:(drop first values) ~index:(last - first);
  elem_drop_law
    ~count:first ~index:(last - first) ~values;
  ()

let segment_empty_law ~(values : int list @ logical)
    ~(first : int{ 0 <= _ })
    : unit{ segment first first values = [] } =
  segment_def first first values;
  take_nonpositive_law ~count:0 ~values:(drop first values) ~proof:();
  ()

let segment_one_law ~(values : int list @ logical)
    ~(index : int{ 0 <= _ && _ < len values })
    : unit{
        segment index (index + 1) values
        = singleton (elem values index)
      } =
  segment_snoc_law ~values ~first:index ~last:index;
  segment_empty_law ~values ~first:index;
  append_def [] (singleton (elem values index));
  ()

let segment_grow_ge_law ~(values : int list @ logical) ~(pivot : int)
    ~(first : int{ 0 <= _ })
    ~(last : int{ first <= _ && _ < len values })
    ~middle:(_middle : unit{
      all_ge (segment first last values) pivot = true
    })
    ~next:(_next : unit{ pivot <= elem values last })
    : unit{
        all_ge (segment first (last + 1) values) pivot = true
      } =
  segment_snoc_law ~values ~first ~last;
  all_ge_append_law
    ~left:(segment first last values)
    ~right:(singleton (elem values last)) ~bound:pivot;
  singleton_def (elem values last);
  all_ge_def [elem values last] pivot;
  all_ge_def [] pivot;
  if int_leq pivot (elem values last) then () else ()

let segment_update_out_law ~(values : int list @ logical)
    ~(first : int{ 0 <= _ }) ~(last : int{ first <= _ })
    ~(index : int{ 0 <= _ && _ < len values }) ~(value : int)
    ~outside:(_outside : unit{ index < first || last <= index })
    : unit{
        segment first last (update values index value)
        = segment first last values
      } =
  segment_def first last (update values index value);
  segment_def first last values;
  if int_less index first
  then begin
    drop_update_lt_law ~count:first ~index ~value ~values;
    ()
  end
  else begin
    drop_update_ge_law ~count:first ~index ~value ~values;
    take_update_ge_law
      ~count:(last - first) ~index:(index - first) ~value
      ~values:(drop first values) ~proof:();
    ()
  end

let segment_cons_law ~(values : int list @ logical)
    ~(first : int{ 0 <= _ })
    ~(last : int{ first < _ && _ <= len values })
    : unit{
        segment first last values
        = append
            (singleton (elem values first))
            (segment (first + 1) last values)
      } =
  drop_cons_law ~index:first ~values;
  segment_def first last values;
  segment_def (first + 1) last values;
  take_def (last - first)
    (append
       (singleton (elem values first))
       (drop (first + 1) values));
  singleton_def (elem values first);
  append_def [elem values first] (drop (first + 1) values);
  append_def [] (drop (first + 1) values);
  take_def (last - first)
    (elem values first :: drop (first + 1) values);
  append_def
    (singleton (elem values first))
    (segment (first + 1) last values);
  singleton_def (elem values first);
  append_def
    [elem values first] (segment (first + 1) last values);
  append_def [] (segment (first + 1) last values);
  ()

let swap_mid_law ~(values : int list @ logical) ~(pivot : int)
    ~(lower : int{ 0 <= _ })
    ~(scan : int{ lower <= _ && _ < len values })
    ~middle:(_middle : unit{
      all_ge (segment lower scan values) pivot = true
    })
    : unit{
        all_ge
          (segment (lower + 1) (scan + 1)
             (update
                (update values lower (elem values scan))
                scan (elem values lower)))
          pivot
        = true
      } =
  let first_update = update values lower (elem values scan) in
  let swapped = update first_update scan (elem values lower) in
  len_update_law ~values ~index:lower ~value:(elem values scan);
  len_update_law
    ~values:first_update ~index:scan ~value:(elem values lower);
  if int_equal lower scan
  then begin
    segment_def (lower + 1) (scan + 1) swapped;
    take_def 0 (drop (lower + 1) swapped);
    all_ge_def [] pivot;
    ()
  end
  else begin
    segment_snoc_law
      ~values:swapped ~first:(lower + 1) ~last:scan;
    segment_update_out_law
      ~values:first_update ~first:(lower + 1) ~last:scan
      ~index:scan ~value:(elem values lower) ~outside:();
    segment_update_out_law
      ~values ~first:(lower + 1) ~last:scan
      ~index:lower ~value:(elem values scan) ~outside:();
    segment_cons_law ~values ~first:lower ~last:scan;
    all_ge_append_law
      ~left:(singleton (elem values lower))
      ~right:(segment (lower + 1) scan values) ~bound:pivot;
    singleton_def (elem values lower);
    all_ge_def [elem values lower] pivot;
    all_ge_def [] pivot;
    elem_update_law
      ~values:first_update ~update_index:scan ~query:scan
      ~value:(elem values lower);
    all_ge_append_law
      ~left:(segment (lower + 1) scan swapped)
      ~right:(singleton (elem swapped scan)) ~bound:pivot;
    singleton_def (elem swapped scan);
    all_ge_def [elem swapped scan] pivot;
    all_ge_def [] pivot;
    if int_leq pivot (elem swapped scan) then () else ()
  end

let segment_to_end_law ~(values : int list @ logical)
    ~(first : int{ 0 <= _ && _ <= len values })
    : unit{ segment first (len values) values = drop first values } =
  segment_def first (len values) values;
  len_drop_law ~count:first ~values ~bounded:();
  take_all_law ~values:(drop first values);
  ()

let swap_pivot_law ~(values : int list @ logical) ~(pivot : int)
    ~(lower : int{ 0 <= _ })
    ~(scan : int{ lower <= _ && _ < len values })
    ~parked:(_parked : unit{ elem values scan = pivot })
    : unit{
        elem
          (update
             (update values lower (elem values scan))
             scan (elem values lower))
          lower
        = pivot
      } =
  let first_update = update values lower (elem values scan) in
  len_update_law ~values ~index:lower ~value:(elem values scan);
  elem_update_law
    ~values:first_update ~update_index:scan ~query:lower
    ~value:(elem values lower);
  elem_update_law
    ~values ~update_index:lower ~query:lower
    ~value:(elem values scan);
  if int_equal lower scan then () else ()

let swap_final_ge_law ~(values : int list @ logical) ~(pivot : int)
    ~(lower : int{ 0 <= _ })
    ~(scan : int{ lower <= _ && _ < len values })
    ~final_index:(_final_index : unit{ scan = len values - 1 })
    ~middle:(_middle : unit{
      all_ge (segment lower scan values) pivot = true
    })
    : unit{
        all_ge
          (drop (lower + 1)
             (update
                (update values lower (elem values scan))
                scan (elem values lower)))
          pivot
        = true
      } =
  let first_update = update values lower (elem values scan) in
  let swapped = update first_update scan (elem values lower) in
  len_update_law ~values ~index:lower ~value:(elem values scan);
  len_update_law
    ~values:first_update ~index:scan ~value:(elem values lower);
  swap_mid_law ~values ~pivot ~lower ~scan ~middle:();
  segment_to_end_law ~values:swapped ~first:(lower + 1);
  ()

let sorted_short_law ~(values : int list @ logical)
    ~proof:(_proof : unit{ len values <= 1 })
    : unit{ sorted values = true } =
  match values with
  | [] ->
    sorted_def [];
    ()
  | head :: rest ->
    len_def (head :: rest);
    (match rest with
     | [] ->
       sorted_def [head];
       all_ge_def [] head;
       sorted_def [];
       ()
     | second :: tail ->
       len_def (second :: tail);
       len_nonnegative_law ~values:tail;
       ())

let rec take_drop_append_law ~(count : int)
    ~(values : int list @ logical)
    : unit{ append (take count values) (drop count values) = values } =
  match values with
  | [] ->
    take_def count [];
    drop_def count [];
    append_def [] [];
    ()
  | head :: rest ->
    take_def count (head :: rest);
    drop_def count (head :: rest);
    if int_leq count 0
    then begin
      append_def [] (head :: rest);
      ()
    end
    else begin
      take_drop_append_law ~count:(count - 1) ~values:rest;
      append_def
        (head :: take (count - 1) rest)
        (drop (count - 1) rest);
      ()
    end

let rec drop_drop_law ~(first : int{ 0 <= _ })
    ~(second : int{ 0 <= _ }) ~(values : int list @ logical)
    : unit{
        drop first (drop second values) = drop (first + second) values
      } =
  match values with
  | [] ->
    drop_def second [];
    drop_def first [];
    drop_def (first + second) [];
    ()
  | head :: rest ->
    drop_def second (head :: rest);
    if int_leq second 0
    then begin
      drop_def first (head :: rest);
      drop_def (first + second) (head :: rest);
      ()
    end
    else begin
      drop_drop_law ~first ~second:(second - 1) ~values:rest;
      drop_def (first + second) (head :: rest);
      ()
    end

let append3_decomp_law ~(values : int list @ logical)
    ~(first : int{ 0 <= _ }) ~(last : int{ first <= _ })
    : unit{
        append
          (take first values)
          (append (segment first last values) (drop last values))
        = values
      } =
  take_drop_append_law ~count:first ~values;
  take_drop_append_law
    ~count:(last - first) ~values:(drop first values);
  drop_drop_law ~first:(last - first) ~second:first ~values;
  segment_def first last values;
  ()

let decomposition_perm_law ~(values : int list @ logical)
    ~(first : int{ 0 <= _ }) ~(last : int{ first <= _ })
    : unit{
        perm
          values
          (append
             (take first values)
             (append (segment first last values) (drop last values)))
        = true
      } =
  append3_decomp_law ~values ~first ~last;
  perm_def values
    (append
       (take first values)
       (append (segment first last values) (drop last values)));
  ()

let rec bag_union_insert_law ~(value : int)
    ~(left : int list @ logical) ~(right : int list @ logical)
    : unit{
        bag_union (insert_bag value left) right
        = insert_bag value (bag_union left right)
      } =
  match left with
  | [] ->
    insert_bag_def value [];
    bag_union_def [value] right;
    bag_union_def [] right;
    ()
  | head :: rest ->
    insert_bag_def value (head :: rest);
    bag_union_def (head :: rest) right;
    if int_leq value head
    then begin
      bag_union_def (value :: head :: rest) right;
      ()
    end
    else begin
      bag_union_def (head :: insert_bag value rest) right;
      bag_union_insert_law ~value ~left:rest ~right;
      insert_commute_law
        ~first:head ~second:value ~values:(bag_union rest right);
      ()
    end

let rec bag_append_law ~(left : int list @ logical)
    ~(right : int list @ logical)
    : unit{
        bag (append left right) = bag_union (bag left) (bag right)
      } =
  match left with
  | [] ->
    append_def [] right;
    bag_def [];
    bag_union_def [] (bag right);
    ()
  | head :: rest ->
    append_def (head :: rest) right;
    bag_def (head :: append rest right);
    bag_def (head :: rest);
    bag_append_law ~left:rest ~right;
    bag_union_insert_law
      ~value:head ~left:(bag rest) ~right:(bag right);
    ()

let perm_append_law ~(left : int list @ logical)
    ~(left_result : int list @ logical)
    ~(right : int list @ logical)
    ~(right_result : int list @ logical)
    ~left_perm:(_left_perm : unit{ perm left left_result = true })
    ~right_perm:(_right_perm : unit{ perm right right_result = true })
    : unit{
        perm
          (append left right)
          (append left_result right_result)
        = true
      } =
  perm_def left left_result;
  perm_def right right_result;
  bag_append_law ~left ~right;
  bag_append_law ~left:left_result ~right:right_result;
  perm_def
    (append left right) (append left_result right_result);
  ()

let perm_glue3_law ~(original : int list @ logical)
    ~(left : int list @ logical) ~(middle : int list @ logical)
    ~(right : int list @ logical)
    ~(left_result : int list @ logical)
    ~(middle_result : int list @ logical)
    ~(right_result : int list @ logical)
    ~decomposed:(_decomposed : unit{
      perm original (append left (append middle right)) = true
    })
    ~left_perm:(_left_perm : unit{ perm left left_result = true })
    ~middle_perm:(_middle_perm : unit{
      perm middle middle_result = true
    })
    ~right_perm:(_right_perm : unit{ perm right right_result = true })
    : unit{
        perm
          original
          (append left_result (append middle_result right_result))
        = true
      } =
  perm_append_law
    ~left:middle ~left_result:middle_result
    ~right ~right_result ~left_perm:() ~right_perm:();
  perm_append_law
    ~left ~left_result
    ~right:(append middle right)
    ~right_result:(append middle_result right_result)
    ~left_perm:() ~right_perm:();
  perm_trans_law
    ~first:original
    ~second:(append left (append middle right))
    ~third:(append left_result (append middle_result right_result))
    ~left:() ~right:();
  ()

let rec all_le_insert_bag_law ~(value : int)
    ~(values : int list @ logical) ~(bound : int)
    : unit{
        all_le (insert_bag value values) bound
        = (value <= bound && all_le values bound)
      } =
  match values with
  | [] ->
    insert_bag_def value [];
    all_le_def [value] bound;
    all_le_def [] bound;
    if int_leq value bound then () else ()
  | head :: rest ->
    insert_bag_def value (head :: rest);
    all_le_def (head :: rest) bound;
    if int_leq value head
    then begin
      all_le_def (value :: head :: rest) bound;
      if int_leq value bound then () else ()
    end
    else begin
      all_le_def (head :: insert_bag value rest) bound;
      all_le_insert_bag_law ~value ~values:rest ~bound;
      if int_leq head bound then () else ()
    end

let rec all_ge_insert_bag_law ~(value : int)
    ~(values : int list @ logical) ~(bound : int)
    : unit{
        all_ge (insert_bag value values) bound
        = (bound <= value && all_ge values bound)
      } =
  match values with
  | [] ->
    insert_bag_def value [];
    all_ge_def [value] bound;
    all_ge_def [] bound;
    if int_leq bound value then () else ()
  | head :: rest ->
    insert_bag_def value (head :: rest);
    all_ge_def (head :: rest) bound;
    if int_leq value head
    then begin
      all_ge_def (value :: head :: rest) bound;
      if int_leq bound value then () else ()
    end
    else begin
      all_ge_def (head :: insert_bag value rest) bound;
      all_ge_insert_bag_law ~value ~values:rest ~bound;
      if int_leq bound head then () else ()
    end

let rec all_le_bag_law ~(values : int list @ logical) ~(bound : int)
    : unit{ all_le (bag values) bound = all_le values bound } =
  match values with
  | [] ->
    bag_def [];
    ()
  | head :: rest ->
    bag_def (head :: rest);
    all_le_insert_bag_law ~value:head ~values:(bag rest) ~bound;
    all_le_bag_law ~values:rest ~bound;
    all_le_def (head :: rest) bound;
    if int_leq head bound then () else ()

let rec all_ge_bag_law ~(values : int list @ logical) ~(bound : int)
    : unit{ all_ge (bag values) bound = all_ge values bound } =
  match values with
  | [] ->
    bag_def [];
    ()
  | head :: rest ->
    bag_def (head :: rest);
    all_ge_insert_bag_law ~value:head ~values:(bag rest) ~bound;
    all_ge_bag_law ~values:rest ~bound;
    all_ge_def (head :: rest) bound;
    if int_leq bound head then () else ()

let all_le_perm_law ~(source : int list @ logical)
    ~(target : int list @ logical) ~(bound : int)
    ~permutation:(_permutation : unit{ perm source target = true })
    ~source_bound:(_source_bound : unit{
      all_le source bound = true
    })
    : unit{ all_le target bound = true } =
  perm_def source target;
  all_le_bag_law ~values:source ~bound;
  all_le_bag_law ~values:target ~bound;
  ()

let all_ge_perm_law ~(source : int list @ logical)
    ~(target : int list @ logical) ~(bound : int)
    ~permutation:(_permutation : unit{ perm source target = true })
    ~source_bound:(_source_bound : unit{
      all_ge source bound = true
    })
    : unit{ all_ge target bound = true } =
  perm_def source target;
  all_ge_bag_law ~values:source ~bound;
  all_ge_bag_law ~values:target ~bound;
  ()

let rec all_ge_weaken_law ~(values : int list @ logical)
    ~(strong : int) ~(weak : int)
    ~ordered:(_ordered : unit{ weak <= strong })
    ~bounded:(_bounded : unit{ all_ge values strong = true })
    : unit{ all_ge values weak = true } =
  match values with
  | [] ->
    all_ge_def [] weak;
    ()
  | head :: rest ->
    all_ge_def (head :: rest) strong;
    all_ge_def (head :: rest) weak;
    all_ge_weaken_law
      ~values:rest ~strong ~weak ~ordered:() ~bounded:();
    if int_leq weak head then () else ()

let rec sorted_pivot_glue_law ~(left : int list @ logical)
    ~(right : int list @ logical) ~(pivot : int)
    ~left_sorted:(_left_sorted : unit{ sorted left = true })
    ~right_sorted:(_right_sorted : unit{ sorted right = true })
    ~left_bound:(_left_bound : unit{ all_le left pivot = true })
    ~right_bound:(_right_bound : unit{ all_ge right pivot = true })
    : unit{
        sorted (append left (append (singleton pivot) right)) = true
      } =
  match left with
  | [] ->
    append_def [] (append (singleton pivot) right);
    singleton_def pivot;
    append_def [pivot] right;
    append_def [] right;
    sorted_def (pivot :: right);
    ()
  | head :: rest ->
    sorted_def (head :: rest);
    all_le_def (head :: rest) pivot;
    sorted_pivot_glue_law
      ~left:rest ~right ~pivot
      ~left_sorted:() ~right_sorted:()
      ~left_bound:() ~right_bound:();
    all_ge_weaken_law
      ~values:right ~strong:pivot ~weak:head
      ~ordered:() ~bounded:();
    singleton_def pivot;
    append_def [pivot] right;
    append_def [] right;
    all_ge_def (pivot :: right) head;
    all_ge_append_law
      ~left:rest ~right:(pivot :: right) ~bound:head;
    append_def (head :: rest) (append (singleton pivot) right);
    sorted_def (head :: append rest (pivot :: right));
    ()

let semantic_controls_law ()
    : unit{
        perm [2; 1] [1; 2] = true
        && perm [1; 2] [2; 1] = true
        && perm [1; 1] [1] = false
        && sorted [1] = true
        && sorted [1; 2] = true
        && sorted [2; 1] = false
      } =
  bag_def [2; 1];
  bag_def [1];
  bag_def [];
  insert_bag_def 1 [];
  insert_bag_def 2 [1];
  bag_def [1; 2];
  bag_def [2];
  insert_bag_def 2 [];
  insert_bag_def 1 [2];
  perm_def [2; 1] [1; 2];
  perm_def [1; 2] [2; 1];
  bag_def [1; 1];
  insert_bag_def 1 [1];
  perm_def [1; 1] [1];
  sorted_def [1; 2];
  sorted_def [1];
  all_ge_def [2] 1;
  all_ge_def [] 1;
  sorted_def [2];
  all_ge_def [] 2;
  sorted_def [];
  sorted_def [2; 1];
  all_ge_def [1] 2;
  if int_leq 2 1 then () else ()
