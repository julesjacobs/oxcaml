open Vox_sequence

external ( + ) : Bigint.t -> Bigint.t -> Bigint.t @@ total = "caml_bigint_add"
external ( - ) : Bigint.t -> Bigint.t -> Bigint.t @@ total = "caml_bigint_sub"

let[@def] element (values : int list) (index : Bigint.t) =
  match at values index with Some value -> value | None -> 0

let[@def] accepts (value : int) (bound : int) (lower : bool) =
  if lower then value <= bound else bound <= value

let[@def] rec all (values : int list) (bound : int) (lower : bool) =
  match values with
  | [] -> true
  | head :: tail -> accepts head bound lower && all tail bound lower

let[@def] all_le (values : int list) (bound : int) = all values bound true
let[@def] all_ge (values : int list) (bound : int) = all values bound false

let[@def] rec range (values : int list) (bound : int) (lower : bool)
    (first : Bigint.t) (past : Bigint.t) =
  if past <= 0Z then true
  else match values with
  | [] -> true
  | head :: tail ->
    (first > 0Z || accepts head bound lower)
    && range tail bound lower (if first > 0Z then first - 1Z else 0Z) (past - 1Z)

let[@def] rec sorted (values : int list) =
  match values with [] -> true | head :: tail -> all tail head false && sorted tail

let[@def] rec insert (value : int) (values : int list) =
  match values with
  | [] -> [value]
  | head :: tail ->
    if value <= head then value :: values else head :: insert value tail

let[@def] rec bag (values : int list) =
  match values with [] -> [] | head :: tail -> insert head (bag tail)

let[@def] permutation (left : int list) (right : int list) =
  ghost_ (bag left === bag right)

let rec (element_at @ total) : (values : int list) -> (index : Bigint.t) ->
    {u : unit | if 0Z <= index && index < length values then
      at values index === Some (element values index) else true} = fun values index ->
  at_def values index;
  element_def values index;
  length_def values;
  match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    if index = 0Z then let u = () in refine_ u
    else
      let next = index - 1Z in
      element_at tail next;
      let u = () in refine_ u

let (swap_equation @ total) : (values : int list) ->
    (first : Bigint.t) -> (second : Bigint.t) ->
    {u : unit | if 0Z <= first && first < length values
      && 0Z <= second && second < length values then
      swap values first second ===
        set (set values first (element values second)) second (element values first)
      else true} = fun values first second ->
  element_at values first;
  element_at values second;
  swap_def values first second;
  let u = () in refine_ u

let rec (element_set @ total) : (values : int list) ->
    (index : Bigint.t) -> (query : Bigint.t) -> (value : int) ->
    {u : unit | if 0Z <= index && index < length values
      && 0Z <= query && query < length values then
      element (set values index value) query ===
        (if index = query then value else element values query)
      else true} = fun values index query value ->
  let changed = set values index value in
  set_def values index value;
  at_def values query;
  at_def changed query;
  element_def values query;
  element_def changed query;
  length_def values;
  match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    if query = 0Z then let u = () in refine_ u
    else
      let next_index = index - 1Z in
      let next_query = query - 1Z in
      if index = 0Z then let u = () in refine_ u
      else
        (element_set tail next_index next_query value;
        let changed_tail = set tail next_index value in
        element_def tail next_query;
        element_def changed_tail next_query;
        element_at tail next_query;
        set_length tail next_index value;
        element_at changed_tail next_query;
        let u = () in refine_ u)

let rec (insert_commute @ total) : (first : int) -> (second : int) ->
    (values : int list) ->
    {u : unit | insert first (insert second values) ===
      insert second (insert first values)} = fun first second values ->
  let left = insert first values in
  let right = insert second values in
  insert_def first values;
  insert_def second values;
  insert_def first right;
  insert_def second left;
  match values with
  | [] -> let u = () in refine_ u
  | head :: tail ->
    if first <= head && second <= head then let u = () in refine_ u
    else if first <= head then
      let part = insert second tail in
      let cons = head :: part in
      insert_def first cons;
      let u = () in refine_ u
    else if second <= head then
      let part = insert first tail in
      let cons = head :: part in
      insert_def second cons;
      let u = () in refine_ u
    else
      (insert_commute first second tail;
      let u = () in refine_ u)

let rec (range_empty @ total) : (values : int list) -> (bound : int) ->
    (lower : bool) -> (first : Bigint.t) -> (past : Bigint.t) ->
    {u : unit | if 0Z <= first && past <= first then
      range values bound lower first past else true} =
    fun values bound lower first past ->
  range_def values bound lower first past;
  match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    if past <= 0Z then let u = () in refine_ u
    else
      let next_first = if first > 0Z then first - 1Z else 0Z in
      let next_past = past - 1Z in
      range_empty tail bound lower next_first next_past;
      let u = () in refine_ u

let rec (range_shrink @ total) : (values : int list) -> (bound : int) ->
    (lower : bool) -> (first : Bigint.t) -> (past : Bigint.t) ->
    (new_first : Bigint.t) -> (new_past : Bigint.t) ->
    {u : unit | if 0Z <= first && first <= new_first && new_past <= past
      && range values bound lower first past then
      range values bound lower new_first new_past else true} =
    fun values bound lower first past new_first new_past ->
  range_def values bound lower first past;
  range_def values bound lower new_first new_past;
  match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    let next_first = if first > 0Z then first - 1Z else 0Z in
    let next_past = past - 1Z in
    let next_new_first = if new_first > 0Z then new_first - 1Z else 0Z in
    let next_new_past = new_past - 1Z in
    range_shrink tail bound lower next_first next_past
      next_new_first next_new_past;
    let u = () in refine_ u

let rec (range_get @ total) : (values : int list) -> (bound : int) ->
    (lower : bool) -> (first : Bigint.t) -> (past : Bigint.t) -> (index : Bigint.t) ->
    {u : unit | if 0Z <= first && first <= index && index < past
      && index < length values && range values bound lower first past then
      accepts (element values index) bound lower else true} =
    fun values bound lower first past index ->
  range_def values bound lower first past;
  length_def values;
  at_def values index;
  element_def values index;
  match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    if index = 0Z then let u = () in refine_ u
    else
      let next_first = if first > 0Z then first - 1Z else 0Z in
      let next_past = past - 1Z in
      let next_index = index - 1Z in
      range_get tail bound lower next_first next_past next_index;
      element_at tail next_index;
      element_def tail next_index;
      let u = () in refine_ u

let rec (range_grow @ total) : (values : int list) -> (bound : int) ->
    (lower : bool) -> (first : Bigint.t) -> (past : Bigint.t) ->
    {u : unit | if 0Z <= first && first <= past && past < length values
      && range values bound lower first past
      && accepts (element values past) bound lower then
      range values bound lower first (past + 1Z) else true} =
    fun values bound lower first past ->
  let next = past + 1Z in
  range_def values bound lower first past;
  range_def values bound lower first next;
  length_def values;
  at_def values past;
  element_def values past;
  match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    let next_first = if first > 0Z then first - 1Z else 0Z in
    let next_past = past - 1Z in
    if past = 0Z then
      let zero = 0Z in
      range_def tail bound lower zero zero;
      let u = () in refine_ u
    else
      (element_at tail next_past;
      element_def tail next_past;
      range_grow tail bound lower next_first next_past;
      let u = () in refine_ u)

let rec (range_set @ total) : (values : int list) -> (bound : int) ->
    (lower : bool) -> (first : Bigint.t) -> (past : Bigint.t) ->
    (index : Bigint.t) -> (value : int) ->
    {u : unit | if 0Z <= first && 0Z <= index
      && range values bound lower first past
      && (index < first || past <= index || accepts value bound lower) then
      range (set values index value) bound lower first past else true} =
    fun values bound lower first past index value ->
  let changed = set values index value in
  set_def values index value;
  range_def values bound lower first past;
  range_def changed bound lower first past;
  match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    if index = 0Z then let u = () in refine_ u
    else
      let next_first = if first > 0Z then first - 1Z else 0Z in
      let next_past = past - 1Z in
      let next_index = index - 1Z in
      range_set tail bound lower next_first next_past next_index value;
      let u = () in refine_ u

let (element_equation @ total) : (values : int list) -> (index : Bigint.t) ->
    {u : unit | element values index ===
      (match values with [] -> 0 | head :: tail ->
        if index = 0Z then head else element tail (index - 1Z))} = fun values index ->
  at_def values index;
  element_def values index;
  match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    let next = index - 1Z in
    element_def tail next;
    let u = () in refine_ u

let (bag_cons @ total) : (head : int) -> (tail : int list) ->
    {u : unit | bag (head :: tail) === insert head (bag tail)} = fun head tail ->
  let values = head :: tail in
  bag_def values;
  let u = () in refine_ u

let rec (bag_exchange @ total) : (head : int) -> (values : int list) -> (index : Bigint.t) ->
    {u : unit | if 0Z <= index && index < length values then
      bag (element values index :: set values index head) === bag (head :: values)
      else true} = fun head values index ->
  length_def values;
  element_equation values index;
  set_def values index head;
  bag_cons head values;
  match values with
  | [] -> let u = () in refine_ u
  | value :: tail ->
    let tail_bag = bag tail in
    bag_cons value tail;
    let refine_ head_tail = bag_cons head tail in
    insert_commute head value tail_bag;
    if index = 0Z then
      let head_tail = head :: tail in
      bag_cons value head_tail;
      let u = () in refine_ u
    else
      let next = index - 1Z in
      let chosen = element tail next in
      let changed_tail = set tail next head in
      let changed = value :: changed_tail in
      let changed_bag = bag changed_tail in
      bag_exchange head tail next;
      bag_cons chosen changed;
      bag_cons value changed_tail;
      bag_cons chosen changed_tail;
      insert_commute chosen value changed_bag;
      let u = () in refine_ u

let rec (bag_swap @ total) : (values : int list) ->
    (first : Bigint.t) -> (second : Bigint.t) ->
    {u : unit | if 0Z <= first && first < length values
      && 0Z <= second && second < length values then
      bag (set (set values first (element values second)) second (element values first))
        === bag values else true} = fun values first second ->
  let x = element values first in
  let y = element values second in
  let intermediate = set values first y in
  length_def values;
  element_equation values first;
  element_equation values second;
  set_def values first y;
  set_def intermediate second x;
  match values with
  | [] -> let u = () in refine_ u
  | head :: tail ->
    let i = first - 1Z in
    let j = second - 1Z in
    if first = 0Z then
      if second = 0Z then let u = () in refine_ u
      else
        (bag_exchange head tail j;
        let u = () in refine_ u)
    else if second = 0Z then
      (bag_exchange head tail i;
      let u = () in refine_ u)
    else
      let tail_first = set tail i y in
      let tail_changed = set tail_first j x in
      set_def tail_first j x;
      bag_swap tail i j;
      bag_cons head tail;
      bag_cons head tail_changed;
      let u = () in refine_ u

let (permutation_swap @ total) : (values : int list) ->
    (first : Bigint.t) -> (second : Bigint.t) ->
    {u : unit | if 0Z <= first && first < length values
      && 0Z <= second && second < length values then
      permutation values (swap values first second) else true} = fun values first second ->
  let swapped = swap values first second in
  swap_equation values first second;
  bag_swap values first second;
  permutation_def values swapped;
  let u = () in refine_ u

let (permutation_refl @ total) : (values : int list) ->
    {u : unit | permutation values values} = fun values ->
  permutation_def values values;
  let u = () in refine_ u

let (permutation_trans @ total) : (first : int list) -> (second : int list) ->
    (third : int list) ->
    {u : unit | if permutation first second && permutation second third then
      permutation first third else true} = fun first second third ->
  permutation_def first second;
  permutation_def second third;
  permutation_def first third;
  let u = () in refine_ u

let (element_swap @ total) : (values : int list) -> (first : Bigint.t) ->
    (second : Bigint.t) -> (query : Bigint.t) ->
    {u : unit | if 0Z <= first && first < length values
      && 0Z <= second && second < length values
      && 0Z <= query && query < length values then
      element (swap values first second) query ===
        (if query = first then element values second
         else if query = second then element values first else element values query)
      else true} = fun values first second query ->
  let x = element values first in
  let y = element values second in
  let intermediate = set values first y in
  swap_equation values first second;
  set_length values first y;
  element_set values first query y;
  element_set intermediate second query x;
  let u = () in refine_ u

let (swap_partition @ total) : (values : int list) -> (pivot : int) ->
    (lower : Bigint.t) -> (scan : Bigint.t) ->
    {u : unit | if 0Z <= lower && lower <= scan && scan < length values
      && range values pivot true 0Z lower
      && range values pivot false lower scan
      && element values scan <= pivot then
      range (swap values lower scan) pivot true 0Z (lower + 1Z)
      && range (swap values lower scan) pivot false (lower + 1Z) (scan + 1Z)
      else true} = fun values pivot lower scan ->
  let zero = 0Z in
  let low_side = true in
  let high_side = false in
  let next_lower = lower + 1Z in
  let next_scan = scan + 1Z in
  let x = element values lower in
  let y = element values scan in
  let intermediate = set values lower y in
  let swapped = swap values lower scan in
  swap_equation values lower scan;
  set_length values lower y;
  set_length intermediate scan x;
  element_swap values lower scan lower;
  element_swap values lower scan scan;
  range_set values pivot low_side zero lower lower y;
  range_set intermediate pivot low_side zero lower scan x;
  let final_low = element swapped lower in
  accepts_def final_low pivot low_side;
  range_grow swapped pivot low_side zero lower;
  if lower = scan then
    (range_empty swapped pivot high_side next_lower next_scan;
    let u = () in refine_ u)
  else
    (range_get values pivot high_side lower scan lower;
    range_shrink values pivot high_side lower scan next_lower scan;
    range_set values pivot high_side next_lower scan lower y;
    range_set intermediate pivot high_side next_lower scan scan x;
    range_grow swapped pivot high_side next_lower scan;
    let u = () in refine_ u)

let rec (range_sub @ total) : (values : int list) -> (bound : int) -> (lower : bool) ->
    (first : Bigint.t) -> (past : Bigint.t) ->
    {u : unit | if 0Z <= first && first <= past && past <= length values then
      range values bound lower first past === all (sub values first past) bound lower
      else true} = fun values bound lower first past ->
  let width = past - first in
  let rest = drop first values in
  let selected = sub values first past in
  length_def values;
  range_def values bound lower first past;
  sub_def values first past;
  drop_def first values;
  take_def width rest;
  all_def selected bound lower;
  match values with
  | [] -> let u = () in refine_ u
  | head :: tail ->
    if past <= 0Z then let u = () in refine_ u
    else
      let next_first = if first > 0Z then first - 1Z else 0Z in
      let next_past = past - 1Z in
      let next_width = next_past - next_first in
      let next_rest = drop next_first tail in
      range_sub tail bound lower next_first next_past;
      sub_def tail next_first next_past;
      drop_def next_first tail;
      take_def next_width next_rest;
      let u = () in refine_ u

let rec (all_append @ total) : (left : int list) -> (right : int list) ->
    (bound : int) -> (lower : bool) ->
    {u : unit | all (append left right) bound lower ===
      (all left bound lower && all right bound lower)} = fun left right bound lower ->
  let joined = append left right in
  append_def left right;
  all_def left bound lower;
  all_def joined bound lower;
  match left with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    all_append tail right bound lower;
    let u = () in refine_ u

let rec (all_insert @ total) : (value : int) -> (values : int list) ->
    (bound : int) -> (lower : bool) ->
    {u : unit | all (insert value values) bound lower ===
      (accepts value bound lower && all values bound lower)} = fun value values bound lower ->
  let inserted = insert value values in
  insert_def value values;
  all_def values bound lower;
  all_def inserted bound lower;
  match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    all_insert value tail bound lower;
    let u = () in refine_ u

let rec (all_bag @ total) : (values : int list) -> (bound : int) -> (lower : bool) ->
    {u : unit | all (bag values) bound lower === all values bound lower} =
    fun values bound lower ->
  bag_def values;
  all_def values bound lower;
  match values with
  | [] -> let u = () in refine_ u
  | head :: tail ->
    let tail_bag = bag tail in
    all_insert head tail_bag bound lower;
    all_bag tail bound lower;
    let u = () in refine_ u

let (all_permutation @ total) : (before : int list) -> (after : int list) ->
    (bound : int) -> (lower : bool) ->
    {u : unit | if permutation before after then
      all before bound lower === all after bound lower else true} =
    fun before after bound lower ->
  permutation_def before after;
  all_bag before bound lower;
  all_bag after bound lower;
  let u = () in refine_ u

let rec (all_weaken @ total) : (values : int list) -> (old_bound : int) ->
    (new_bound : int) -> (lower : bool) ->
    {u : unit | if (if lower then old_bound <= new_bound else new_bound <= old_bound)
      && all values old_bound lower then all values new_bound lower else true} =
    fun values old_bound new_bound lower ->
  all_def values old_bound lower;
  all_def values new_bound lower;
  match values with
  | [] -> let u = () in refine_ u
  | head :: tail ->
    accepts_def head old_bound lower;
    accepts_def head new_bound lower;
    all_weaken tail old_bound new_bound lower;
    let u = () in refine_ u

let (sorted_short @ total) : (values : int list) ->
    {u : unit | if length values <= 1Z then sorted values else true} = fun values ->
  length_def values;
  sorted_def values;
  match values with
  | [] -> let u = () in refine_ u
  | head :: tail ->
    let side = false in
    length_def tail;
    sorted_def tail;
    all_def tail head side;
    let u = () in refine_ u

let rec (sorted_glue @ total) : (left : int list) -> (pivot : int) -> (right : int list) ->
    {u : unit | if sorted left && sorted right
      && all left pivot true && all right pivot false then
      sorted (append left (pivot :: right)) else true} = fun left pivot right ->
  let pivot_right = pivot :: right in
  let joined = append left pivot_right in
  let low_side = true in
  let high_side = false in
  append_def left pivot_right;
  sorted_def left;
  sorted_def pivot_right;
  all_def left pivot low_side;
  sorted_def joined;
  match left with
  | [] -> let u = () in refine_ u
  | head :: tail ->
    accepts_def head pivot low_side;
    accepts_def pivot head high_side;
    all_weaken right pivot head high_side;
    all_def pivot_right head high_side;
    all_append tail pivot_right head high_side;
    sorted_glue tail pivot right;
    let u = () in refine_ u

let[@def] rec insert_all (left : int list) (right : int list) =
  match left with [] -> right | head :: tail -> insert head (insert_all tail right)

let rec (insert_all_insert @ total) : (value : int) -> (left : int list) -> (right : int list) ->
    {u : unit | insert_all (insert value left) right ===
      insert value (insert_all left right)} = fun value left right ->
  let inserted = insert value left in
  insert_def value left;
  insert_all_def left right;
  insert_all_def inserted right;
  match left with
  | [] -> let u = () in refine_ u
  | head :: tail ->
    let combined = insert_all tail right in
    insert_all_insert value tail right;
    insert_commute value head combined;
    let u = () in refine_ u

let rec (insert_all_bag @ total) : (left : int list) -> (right : int list) ->
    {u : unit | insert_all (bag left) right === insert_all left right} = fun left right ->
  bag_def left;
  insert_all_def left right;
  match left with
  | [] -> let u = () in refine_ u
  | head :: tail ->
    let tail_bag = bag tail in
    insert_all_insert head tail_bag right;
    insert_all_bag tail right;
    let u = () in refine_ u

let rec (bag_append @ total) : (left : int list) -> (right : int list) ->
    {u : unit | bag (append left right) === insert_all left (bag right)} = fun left right ->
  let joined = append left right in
  let right_bag = bag right in
  append_def left right;
  bag_def joined;
  insert_all_def left right_bag;
  match left with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    bag_append tail right;
    let u = () in refine_ u

let (permutation_append @ total) : (left : int list) -> (right : int list) ->
    (new_left : int list) -> (new_right : int list) ->
    {u : unit | if permutation left new_left && permutation right new_right then
      permutation (append left right) (append new_left new_right) else true} =
    fun left right new_left new_right ->
  let before = append left right in
  let after = append new_left new_right in
  let right_bag = bag right in
  let new_right_bag = bag new_right in
  permutation_def left new_left;
  permutation_def right new_right;
  permutation_def before after;
  bag_append left right;
  bag_append new_left new_right;
  insert_all_bag left right_bag;
  insert_all_bag new_left new_right_bag;
  let u = () in refine_ u

let rec (take_all @ total) : (values : int list) ->
    {u : unit | take (length values) values === values} = fun values ->
  let size = length values in
  length_def values;
  take_def size values;
  match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    take_all tail;
    let u = () in refine_ u

let (sub_prefix @ total) : (values : int list) -> (past : Bigint.t) ->
    {u : unit | sub values 0Z past === take past values} = fun values past ->
  let zero = 0Z in
  sub_def values zero past;
  drop_def zero values;
  let u = () in refine_ u

let (sub_suffix @ total) : (values : int list) -> (first : Bigint.t) ->
    {u : unit | if 0Z <= first && first <= length values then
      sub values first (length values) === drop first values else true} = fun values first ->
  let size = length values in
  let rest = drop first values in
  sub_def values first size;
  let refine_ size = cut values first in
  take_all rest;
  let u = () in refine_ u

let rec (sub_one @ total) : (values : int list) -> (index : Bigint.t) ->
    {u : unit | if 0Z <= index && index < length values then
      sub values index (index + 1Z) === [element values index] else true} = fun values index ->
  let past = index + 1Z in
  let one = 1Z in
  let zero = 0Z in
  let rest = drop index values in
  length_def values;
  element_equation values index;
  sub_def values index past;
  drop_def index values;
  take_def one rest;
  match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    if index = 0Z then
      (take_def zero tail;
      let u = () in refine_ u)
    else
      let next = index - 1Z in
      sub_one tail next;
      sub_def tail next index;
      let u = () in refine_ u

let (decompose3 @ total) : (values : int list) -> (first : Bigint.t) -> (past : Bigint.t) ->
    {u : unit | if 0Z <= first && first <= past && past <= length values then
      values === append (take first values) (append (sub values first past) (drop past values))
      else true} = fun values first past ->
  let rest = drop first values in
  let width = past - first in
  let refine_ front = cut values first in
  let refine_ back = cut rest width in
  sub_def values first past;
  drop_add values first width;
  let u = () in refine_ u

let (partition_bounds @ total) : (values : int list) -> (pivot : int) -> (index : Bigint.t) ->
    {u : unit | if 0Z <= index && index < length values
      && range values pivot true 0Z index
      && range values pivot false (index + 1Z) (length values) then
      all (take index values) pivot true && all (drop (index + 1Z) values) pivot false
      else true} = fun values pivot index ->
  let zero = 0Z in
  let next = index + 1Z in
  let size = length values in
  let low_side = true in
  let high_side = false in
  range_sub values pivot low_side zero index;
  sub_prefix values index;
  range_sub values pivot high_side next size;
  sub_suffix values next;
  let u = () in refine_ u

let (glue_partition @ total) : (before : int list) -> (pivot : int) -> (index : Bigint.t) ->
    (left : int list) -> (middle : int list) -> (right : int list) ->
    {u : unit | if 0Z <= index && index < length before
      && element before index = pivot
      && range before pivot true 0Z index
      && range before pivot false (index + 1Z) (length before)
      && sorted left && sorted right
      && permutation (take index before) left
      && middle === sub before index (index + 1Z)
      && permutation (drop (index + 1Z) before) right then
      sorted (append left (append middle right))
      && permutation before (append left (append middle right)) else true} =
    fun before pivot index left middle right ->
  let next = index + 1Z in
  let old_left = take index before in
  let old_middle = sub before index next in
  let old_right = drop next before in
  let old_rest = append old_middle old_right in
  let new_rest = append middle right in
  let new_values = append left new_rest in
  let nil = [] in
  let low_side = true in
  let high_side = false in
  partition_bounds before pivot index;
  sub_one before index;
  decompose3 before index next;
  permutation_refl old_middle;
  permutation_append old_middle old_right middle right;
  permutation_append old_left old_rest left new_rest;
  permutation_def before new_values;
  all_permutation old_left left pivot low_side;
  all_permutation old_right right pivot high_side;
  append_def middle right;
  append_def nil right;
  sorted_glue left pivot right;
  let u = () in refine_ u

let[@def] rec count (values : int list) (target : int) =
  match values with
  | [] -> 0Z
  | head :: tail -> (if head = target then 1Z else 0Z) + count tail target

let rec (insert_count @ total) : (value : int) -> (values : int list) -> (target : int) ->
    {u : unit | count (insert value values) target ===
      (if value = target then 1Z else 0Z) + count values target} = fun value values target ->
  let inserted = insert value values in
  insert_def value values;
  count_def values target;
  count_def inserted target;
  match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    insert_count value tail target;
    let u = () in refine_ u

let rec (bag_count @ total) : (values : int list) -> (target : int) ->
    {u : unit | count (bag values) target === count values target} = fun values target ->
  bag_def values;
  count_def values target;
  match values with
  | [] -> let u = () in refine_ u
  | head :: tail ->
    let tail_bag = bag tail in
    insert_count head tail_bag target;
    bag_count tail target;
    let u = () in refine_ u

let (permutation_count @ total) : (before : int list) -> (after : int list) -> (target : int) ->
    {u : unit | if permutation before after then count before target === count after target
      else true} = fun before after target ->
  permutation_def before after;
  bag_count before target;
  bag_count after target;
  let u = () in refine_ u
