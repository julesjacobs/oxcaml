open! Stdlib

external get : ('a : immutable_data).
  (values : 'a iarray) @ immutable ->
  {i : int | 0 <= i && i < Iarray.length values} ->
  'a @ immutable total @@ total = "%array_safe_get"

let[@def] at (values : ('a : immutable_data) iarray @ immutable total)
    (index : int) : 'a option @ immutable total =
  if 0 <= index && index < Iarray.length values then
    let bounded : {i : int | 0 <= i && i < Iarray.length values} =
      refine_ index in
    Some (get values bounded)
  else None

let (at_get @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable ->
    (index : {i : int | 0 <= i && i < Iarray.length values}) ->
    {u : unit | let refine_ i = index in
      at values i === Some (get values index)} = fun values index ->
  let refine_ i = index in
  at_def values i;
  let u = () in refine_ u

let (at_outside @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (index : int) ->
    {u : unit | if index < 0 || Iarray.length values <= index then
      at values index === None else true} = fun values index ->
  at_def values index;
  let u = () in refine_ u

external extensional : ('a : immutable_data).
  (left : 'a iarray) @ immutable total ->
  (right : 'a iarray) @ immutable total ->
  ((index : int) ->
    {u : unit | Iarray.length left = Iarray.length right &&
      at left index === at right index}) @ total ghost ->
  {u : unit | left === right} @@ total = "caml_vox_iarray_extensional"

external set : ('a : immutable_data).
  (values : 'a iarray) @ immutable ->
  {i : int | 0 <= i && i < Iarray.length values} ->
  'a @ immutable ->
  {result : 'a iarray | Iarray.length result = Iarray.length values}
    @ immutable total @@ total = "caml_vox_iarray_set"

external sub : ('a : immutable_data).
  (values : 'a iarray) @ immutable ->
  (position : {i : int | 0 <= i && i <= Iarray.length values}) ->
  (size : {n : int | let refine_ p = position in
    0 <= n && n <= Iarray.length values - p}) ->
  {result : 'a iarray | let refine_ n = size in Iarray.length result = n}
    @ immutable total @@ total = "caml_vox_iarray_sub"

let[@def] updated (values : ('a : immutable_data) iarray @ immutable total)
    (index : int) (value : 'a @ immutable total) : 'a iarray @ immutable total =
  if 0 <= index && index < Iarray.length values then
    let bounded : {i : int | 0 <= i && i < Iarray.length values} =
      refine_ index in
    let refine_ result = set values bounded value in
    result
  else values

let (set_read @ total) : ('a : immutable_data).
  (values : 'a iarray) @ immutable ->
  (index : {i : int | 0 <= i && i < Iarray.length values}) ->
  (value : 'a) @ immutable -> (query : int) ->
  {u : unit | let refine_ i = index in
    let refine_ changed = set values index value in
    at changed query === (if i = query then Some value else at values query)} =
  fun values index value query ->
    let refine_ changed = set values index value in
    at_def changed query;
    at_def values query;
    let u = () in refine_ u

let (updated_length @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (index : int) ->
    (value : 'a) @ immutable ->
    {u : unit | Iarray.length (updated values index value) =
      Iarray.length values} = fun values index value ->
  updated_def values index value;
  let u = () in refine_ u

let (updated_read @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (index : int) ->
    (value : 'a) @ immutable -> (query : int) ->
    {u : unit | at (updated values index value) query ===
      (if 0 <= index && index < Iarray.length values && index = query then
        Some value else at values query)} = fun values index value query ->
  updated_def values index value;
  let changed = updated values index value in
  at_def changed query;
  at_def values query;
  let u = () in refine_ u

let[@def] slice (values : ('a : immutable_data) iarray @ immutable total)
    (first : int) (past : int) : 'a iarray @ immutable total =
  let size = Iarray.length values in
  let first = if first < 0 then 0 else if first > size then size else first in
  let past = if past < first then first else if past > size then size else past
    in
  let position : {i : int | 0 <= i && i <= Iarray.length values} =
    refine_ first in
  let length = past - first in
  let length : {n : int | let refine_ p = position in
    0 <= n && n <= Iarray.length values - p} = refine_ length in
  let refine_ result = sub values position length in
  result

let (slice_length @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (first : int) -> (past : int) ->
    {u : unit | if 0 <= first && first <= past && past <= Iarray.length values
      then Iarray.length (slice values first past) = past - first else true} =
  fun values first past ->
    slice_def values first past;
    let u = () in refine_ u

let (slice_read @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (first : int) -> (past : int) ->
    (index : int) ->
    {u : unit | if 0 <= first && first <= past && past <= Iarray.length values
      then at (slice values first past) index ===
        (if 0 <= index && index < past - first then
          at values (first + index) else None) else true} =
  fun values first past index ->
    slice_def values first past;
    let result = slice values first past in
    let position = first + index in
    at_def result index;
    at_def values position;
    let u = () in refine_ u

let (slice_all @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable ->
    {u : unit | slice values 0 (Iarray.length values) === values} = fun values
      ->
  let zero = 0 in
  let size = Iarray.length values in
  let result = slice values zero size in
  slice_length values zero size;
  extensional result values
    (ghost_ (fun index ->
      slice_read values zero size index;
      at_def values index;
      let u = () in refine_ u));
  let u = () in refine_ u

let (slice_slice @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (first : int) -> (past : int) ->
    (lower : int) -> (upper : int) ->
    {u : unit | if 0 <= first && first <= past && past <= Iarray.length values
      && 0 <= lower && lower <= upper && upper <= past - first then
      slice (slice values first past) lower upper ===
        slice values (first + lower) (first + upper) else true} =
  fun values first past lower upper ->
    if 0 <= first && first <= past && past <= Iarray.length values
      && 0 <= lower && lower <= upper && upper <= past - first then
      let middle = slice values first past in
      let result = slice middle lower upper in
      let start = first + lower in
      let stop = first + upper in
      let expected = slice values start stop in
      slice_length values first past;
      slice_length middle lower upper;
      slice_length values start stop;
      extensional result expected
        (ghost_ (fun index ->
          let shifted = lower + index in
          slice_read middle lower upper index;
          slice_read values first past shifted;
          slice_read values start stop index;
          let u = () in refine_ u));
      let u = () in refine_ u
    else let u = () in refine_ u

let[@def] swap (values : ('a : immutable_data) iarray @ immutable total)
    (first : int) (second : int) : 'a iarray @ immutable total =
  match at values first, at values second with
  | Some x, Some y -> updated (updated values first y) second x
  | _ -> values

let (swap_length @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (first : int) -> (second : int) ->
    {u : unit | Iarray.length (swap values first second) = Iarray.length values}
      =
  fun values first second ->
    swap_def values first second;
    match at values first, at values second with
    | Some x, Some y ->
      let middle = updated values first y in
      updated_length values first y;
      updated_length middle second x;
      let u = () in refine_ u
    | _ -> let u = () in refine_ u

let (swap_read @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (first : int) -> (second : int) ->
    (query : int) ->
    {u : unit | if 0 <= first && first < Iarray.length values && 0 <= second
      && second < Iarray.length values then
      at (swap values first second) query ===
        (if query = first then at values second else
          if query = second then at values first else at values query)
      else true} = fun values first second query ->
    swap_def values first second;
    at_def values first;
    at_def values second;
    match at values first, at values second with
    | Some x, Some y ->
      let middle = updated values first y in
      updated_length values first y;
      updated_read values first y query;
      updated_read middle second x query;
      let u = () in refine_ u
    | _ -> let u = () in refine_ u

let[@def] to_list (values : ('a : immutable_data) iarray @ immutable total) :
    'a list @ immutable total =
  Vox_sequence.of_iarray values

let (to_list_length @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable ->
    {u : unit | Vox_sequence.length (to_list values) ===
      Bigint.of_int (Iarray.length values)} = fun values ->
  to_list_def values;
  Vox_sequence.of_iarray_length values;
  let u = () in refine_ u

let (to_list_get @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable ->
    (index : {i : int | 0 <= i && i < Iarray.length values}) ->
    {u : unit | let refine_ i = index in
      Vox_sequence.at (to_list values) (Bigint.of_int i) === at values i} =
    fun values index ->
  to_list_def values;
  Vox_sequence.of_iarray_at values index;
  at_get values index;
  let u = () in refine_ u

let (to_list_at @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (index : int) ->
    {u : unit | Vox_sequence.at (to_list values) (Bigint.of_int index)
      === at values index} = fun values index ->
  if 0 <= index && index < Iarray.length values then
    let bounded : {i : int | 0 <= i && i < Iarray.length values} =
      refine_ index in
    to_list_get values bounded;
    let u = () in refine_ u
  else
    let converted = to_list values in
    let query = Bigint.of_int index in
    to_list_length values;
    Vox_sequence.at_outside converted query;
    at_outside values index;
    let u = () in refine_ u

let (to_list_updated_at @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (index : int) ->
    (value : 'a) @ immutable -> (query : int) ->
    {u : unit | Vox_sequence.at (to_list (updated values index value))
      (Bigint.of_int query) ===
      (if 0 <= index && index < Iarray.length values && index = query then
        Some value else Vox_sequence.at (to_list values) (Bigint.of_int query))}
    = fun values index value query ->
  let changed = updated values index value in
  to_list_at changed query;
  to_list_at values query;
  updated_read values index value query;
  let u = () in refine_ u

let (to_list_slice_at @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (first : int) -> (past : int) ->
    (query : int) ->
    {u : unit | if 0 <= first && first <= past && past <= Iarray.length values
      then Vox_sequence.at (to_list (slice values first past))
        (Bigint.of_int query) ===
        (if 0 <= query && query < past - first then
          Vox_sequence.at (to_list values) (Bigint.of_int (first + query))
        else None) else true} = fun values first past query ->
  let part = slice values first past in
  let shifted = first + query in
  to_list_at part query;
  to_list_at values shifted;
  slice_read values first past query;
  let u = () in refine_ u

module For_all (P : Vox_sequence.Predicate) = struct
  let[@def] rec range (values : P.element iarray @ immutable total)
      (first : int) (past : int) =
    if past <= 0 then true
    else let index = past - 1 in
      (index < first ||
        (match at values index with None -> true | Some value -> P.test value))
      && range values first index
  [@@decreases let past : int = past in if past > 0 then past else 0]

  let[@def] holds (values : P.element iarray @ immutable total) =
    range values 0 (Iarray.length values)

  let rec (range_get @ total) :
      (values : P.element iarray) @ immutable ->
      (first : int) -> (past : int) -> (index : int) ->
      {u : unit | if range values first past && 0 <= index
        && first <= index && index < past then
        match at values index with None -> true | Some value -> P.test value
        else true} = fun values first past index ->
    range_def values first past;
    if past > 0 then
      let previous = past - 1 in
      if index = previous then let u = () in refine_ u
      else
        (range_get values first previous index;
        let u = () in refine_ u)
    else let u = () in refine_ u
  [@@decreases let past : int = past in if past > 0 then past else 0]

  let rec (range_intro @ total) :
      (values : P.element iarray) @ immutable ->
      (first : int) -> (past : int) ->
      ((index : int) -> {u : unit | if 0 <= index
        && first <= index && index < past then
        match at values index with None -> true | Some value -> P.test value
        else true}) @ total ->
      {u : unit | range values first past} = fun values first past proof ->
    range_def values first past;
    if past > 0 then
      let previous = past - 1 in
      let refine_ last = proof previous in
      range_intro values first previous (fun index ->
        let refine_ known = proof index in
        let u = () in refine_ u);
      let u = () in refine_ u
    else let u = () in refine_ u
  [@@decreases let past : int = past in if past > 0 then past else 0]

  let (get @ total) : (values : P.element iarray) @ immutable ->
      (index : int) ->
      {u : unit | if holds values then match at values index with
        None -> true | Some value -> P.test value else true} =
      fun values index ->
    let zero = 0 in
    let size = Iarray.length values in
    holds_def values;
    range_get values zero size index;
    at_outside values index;
    let u = () in refine_ u

  let (intro @ total) : (values : P.element iarray) @ immutable ->
      ((index : int) -> {u : unit | if 0 <= index
        && index < Iarray.length values then match at values index with
        None -> true | Some value -> P.test value else true}) @ total ->
      {u : unit | holds values} = fun values proof ->
    let zero = 0 in
    let size = Iarray.length values in
    holds_def values;
    range_intro values zero size (fun index ->
      let refine_ point = proof index in
      let u = () in refine_ u);
    let u = () in refine_ u

  let (updated_holds @ total) : (values : P.element iarray) @ immutable ->
      (index : int) -> (value : P.element) @ immutable ->
      {u : unit | if holds values && P.test value then
        holds (updated values index value) else true} = fun values index value
          ->
    if holds values && P.test value then
      let changed = updated values index value in
      intro changed (fun query ->
        let refine_ old = get values query in
        updated_read values index value query;
        let u = () in refine_ u);
      let u = () in refine_ u
    else let u = () in refine_ u

  let (slice_holds @ total) : (values : P.element iarray) @ immutable ->
      (first : int) -> (past : int) ->
      {u : unit | if holds values && 0 <= first && first <= past
        && past <= Iarray.length values then holds (slice values first past)
        else true} = fun values first past ->
    if holds values && 0 <= first && first <= past
      && past <= Iarray.length values then
      let part = slice values first past in
      slice_length values first past;
      intro part (fun index ->
        let shifted = first + index in
        slice_read values first past index;
        let refine_ known = get values shifted in
        let u = () in refine_ u);
      let u = () in refine_ u
    else let u = () in refine_ u

  let (split_holds @ total) : (values : P.element iarray) @ immutable ->
      (middle : int) ->
      {u : unit | if 0 <= middle && middle <= Iarray.length values then
        holds values = (holds (slice values 0 middle)
          && holds (slice values middle (Iarray.length values))) else true} =
      fun values middle ->
    let zero = 0 in
    let size = Iarray.length values in
    let left = slice values zero middle in
    let right = slice values middle size in
    slice_holds values zero middle;
    slice_holds values middle size;
    if 0 <= middle && middle <= size && holds left && holds right then
      (intro values (fun index ->
        let relative = index - middle in
        let refine_ first = get left index in
        let refine_ second = get right relative in
        slice_read values zero middle index;
        slice_read values middle size relative;
        let u = () in refine_ u);
      let u = () in refine_ u)
    else let u = () in refine_ u
end

module Int = struct
  let[@def] element (values : int iarray) (index : int) =
    match at values index with Some value -> value | None -> 0

  let[@def] accepts (value : int) (bound : int) (lower : bool) =
    if lower then value <= bound else bound <= value

  let[@def] rec range (values : int iarray) (bound : int) (lower : bool)
      (first : int) (past : int) =
    if past <= 0 then true
    else
      let index = past - 1 in
      (index < first || Iarray.length values <= index ||
        accepts (element values index) bound lower)
      && range values bound lower first index
  [@@decreases let past : int = past in if past > 0 then past else 0]

  let[@def] all (values : int iarray) (bound : int) (lower : bool) =
    range values bound lower 0 (Iarray.length values)

  let[@def] all_le (values : int iarray) (bound : int) = all values bound true
  let[@def] all_ge (values : int iarray) (bound : int) = all values bound false

  let rec (range_get @ total) :
      (values : int iarray) -> (bound : int) -> (lower : bool) ->
      (first : int) -> (past : int) -> (index : int) ->
      {u : unit | if 0 <= index && first <= index && index < past
        && index < Iarray.length values && range values bound lower first past
        then accepts (element values index) bound lower else true} =
    fun values bound lower first past index ->
      range_def values bound lower first past;
      if past > 0 then
        let previous = past - 1 in
        if index = previous then let u = () in refine_ u
        else
          (range_get values bound lower first previous index;
          let u = () in refine_ u)
      else let u = () in refine_ u
  [@@decreases let past : int = past in if past > 0 then past else 0]

  let rec (range_intro @ total) :
      (values : int iarray) -> (bound : int) -> (lower : bool) ->
      (first : int) -> (past : int) ->
      ((index : int) ->
        {u : unit | if 0 <= index && first <= index && index < past
          && index < Iarray.length values then
          accepts (element values index) bound lower else true}) @ total ->
      {u : unit | range values bound lower first past} =
    fun values bound lower first past proof ->
      range_def values bound lower first past;
      if past > 0 then
        let previous = past - 1 in
        let refine_ last = proof previous in
        range_intro values bound lower first previous
          (fun index ->
            let refine_ known = proof index in
            let u = () in refine_ u);
        let u = () in refine_ u
      else let u = () in refine_ u
  [@@decreases let past : int = past in if past > 0 then past else 0]
  let (element_updated @ total) : (values : int iarray) -> (index : int) ->
      (value : int) -> (query : int) ->
      {u : unit | element (updated values index value) query =
        (if 0 <= index && index < Iarray.length values && index = query
          then value else element values query)} = fun values index value query
            ->
    let changed = updated values index value in
    updated_read values index value query;
    element_def values query;
    element_def changed query;
    let u = () in refine_ u

  let (element_slice @ total) : (values : int iarray) ->
      (first : int) -> (past : int) -> (index : int) ->
      {u : unit | if 0 <= first && first <= past && past <= Iarray.length values
        && 0 <= index && index < past - first then
        element (slice values first past) index = element values (first + index)
        else true} = fun values first past index ->
    let result = slice values first past in
    let shifted = first + index in
    slice_read values first past index;
    element_def values shifted;
    element_def result index;
    let u = () in refine_ u

  let (element_swap @ total) : (values : int iarray) ->
      (first : int) -> (second : int) -> (index : int) ->
      {u : unit | if 0 <= first && first < Iarray.length values
        && 0 <= second && second < Iarray.length values then
        element (swap values first second) index =
          (if index = first then element values second else
            if index = second then element values first else element values
              index)
        else true} = fun values first second index ->
    let result = swap values first second in
    swap_read values first second index;
    element_def values index;
    element_def values first;
    element_def values second;
    element_def result index;
    let u = () in refine_ u

  let (range_empty @ total) : (values : int iarray) -> (bound : int) ->
      (lower : bool) -> (first : int) -> (past : int) ->
      {u : unit | if past <= first then range values bound lower first past
        else true} = fun values bound lower first past ->
    if past <= first then
      (range_intro values bound lower first past
        (fun index -> let u = () in refine_ u);
      let u = () in refine_ u)
    else let u = () in refine_ u

  let (range_shrink @ total) : (values : int iarray) -> (bound : int) ->
      (lower : bool) -> (first : int) -> (past : int) ->
      (new_first : int) -> (new_past : int) ->
      {u : unit | if first <= new_first && new_past <= past
        && range values bound lower first past then
        range values bound lower new_first new_past else true} =
    fun values bound lower first past new_first new_past ->
      if first <= new_first && new_past <= past
        && range values bound lower first past then
        (range_intro values bound lower new_first new_past
          (fun index ->
            range_get values bound lower first past index;
            let u = () in refine_ u);
        let u = () in refine_ u)
      else let u = () in refine_ u

  let (range_grow @ total) : (values : int iarray) -> (bound : int) ->
      (lower : bool) -> (first : int) -> (past : int) ->
      {u : unit | if 0 <= first && first <= past && past < Iarray.length values
        && range values bound lower first past
        && accepts (element values past) bound lower then
        range values bound lower first (past + 1) else true} =
    fun values bound lower first past ->
      let next = past + 1 in
      range_def values bound lower first next;
      let u = () in refine_ u

  let (range_set @ total) : (values : int iarray) -> (bound : int) ->
      (lower : bool) -> (first : int) -> (past : int) ->
      (index : int) -> (value : int) ->
      {u : unit | if range values bound lower first past &&
        (index < first || past <= index || accepts value bound lower) then
        range (updated values index value) bound lower first past else true} =
    fun values bound lower first past index value ->
      if range values bound lower first past &&
        (index < first || past <= index || accepts value bound lower) then
        let changed = updated values index value in
        updated_length values index value;
        range_intro changed bound lower first past
          (fun query ->
            range_get values bound lower first past query;
            element_updated values index value query;
            let u = () in refine_ u);
        let u = () in refine_ u
      else let u = () in refine_ u

  let[@def] rec sorted_prefix (values : int iarray) (past : int) =
    if past <= 1 then true
    else element values (past - 2) <= element values (past - 1)
      && sorted_prefix values (past - 1)
  [@@decreases let past : int = past in if past > 0 then past else 0]

  let[@def] sorted (values : int iarray) =
    sorted_prefix values (Iarray.length values)

  let rec (sorted_prefix_intro @ total) : (values : int iarray) -> (past : int)
    ->
      ((index : int) -> {u : unit | if 0 <= index
        && index < Iarray.length values - 1 then
        element values index <= element values (index + 1) else true}) @ total
          ->
      {u : unit | if past <= Iarray.length values then sorted_prefix values past
        else true} = fun values past proof ->
    sorted_prefix_def values past;
    if past > 1 then
      let previous = past - 1 in
      let index = past - 2 in
      let refine_ pair = proof index in
      sorted_prefix_intro values previous proof;
      let u = () in refine_ u
    else let u = () in refine_ u
  [@@decreases let past : int = past in if past > 0 then past else 0]

  let (sorted_intro @ total) : (values : int iarray) ->
      ((index : int) -> {u : unit | if 0 <= index
        && index < Iarray.length values - 1 then
        element values index <= element values (index + 1) else true}) @ total
          ->
      {u : unit | sorted values} = fun values proof ->
    let size = Iarray.length values in
    sorted_def values;
    sorted_prefix_intro values size proof;
    let u = () in refine_ u

  let rec (sorted_prefix_get @ total) : (values : int iarray) -> (past : int) ->
      (index : int) ->
      {u : unit | if 0 <= index && index < past - 1 && 0 <= past
        && sorted_prefix values past then
        element values index <= element values (index + 1) else true} =
    fun values past index ->
      sorted_prefix_def values past;
      if past > 1 then
        let last = past - 2 in
        let previous = past - 1 in
        if index = last then let u = () in refine_ u
        else
          (sorted_prefix_get values previous index;
          let u = () in refine_ u)
      else let u = () in refine_ u
  [@@decreases let past : int = past in if past > 0 then past else 0]

  let (sorted_get @ total) : (values : int iarray) -> (index : int) ->
      {u : unit | if sorted values && 0 <= index
        && index < Iarray.length values - 1 then
        element values index <= element values (index + 1) else true} =
    fun values index ->
      let size = Iarray.length values in
      sorted_def values;
      sorted_prefix_get values size index;
      let u = () in refine_ u

  let rec (ordered @ total) : (values : int iarray) ->
      (first : int) -> (last : int) ->
      {u : unit | if sorted values && 0 <= first && first <= last
        && last < Iarray.length values then
        element values first <= element values last else true} =
      fun values first last ->
    if sorted values && 0 <= first && first < last
      && last < Iarray.length values then
      let previous = last - 1 in
      ordered values first previous;
      sorted_get values previous;
      let u = () in refine_ u
    else let u = () in refine_ u
  [@@decreases
    let first : int = first in let last : int = last in
    if 0 <= first && first <= last then last - first else 0]

  let (sorted_slice @ total) : (values : int iarray) ->
      (first : int) -> (past : int) ->
      {u : unit | if sorted values && 0 <= first && first <= past
        && past <= Iarray.length values then sorted (slice values first past)
        else true} = fun values first past ->
    if sorted values && 0 <= first && first <= past
      && past <= Iarray.length values then
      let part = slice values first past in
      slice_length values first past;
      sorted_intro part (fun index ->
        let following = index + 1 in
        let shifted = first + index in
        element_slice values first past index;
        element_slice values first past following;
        sorted_get values shifted;
        let u = () in refine_ u);
      let u = () in refine_ u
    else let u = () in refine_ u

  let (sorted_updated @ total) : (values : int iarray) -> (index : int) ->
      (value : int) ->
      {u : unit | if sorted values && 0 <= index && index < Iarray.length values
        && (index = 0 || element values (index - 1) <= value)
        && (index = Iarray.length values - 1
          || value <= element values (index + 1)) then
        sorted (updated values index value) else true} = fun values index value
          ->
    if sorted values && 0 <= index && index < Iarray.length values
      && (index = 0 || element values (index - 1) <= value)
      && (index = Iarray.length values - 1
        || value <= element values (index + 1)) then
      let changed = updated values index value in
      updated_length values index value;
      sorted_intro changed (fun query ->
        let next = query + 1 in
        element_updated values index value query;
        element_updated values index value next;
        sorted_get values query;
        let u = () in refine_ u);
      let u = () in refine_ u
    else let u = () in refine_ u

  let (sorted_glue @ total) : (values : int iarray) -> (pivot : int) ->
      (index : int) ->
      {u : unit | if 0 <= index && index < Iarray.length values
        && element values index = pivot
        && sorted (slice values 0 index)
        && sorted (slice values (index + 1) (Iarray.length values))
        && all (slice values 0 index) pivot true
        && all (slice values (index + 1) (Iarray.length values)) pivot false
      then
        sorted values else true} = fun values pivot index ->
    let zero = 0 in
    let next = index + 1 in
    let size = Iarray.length values in
    let left = slice values zero index in
    let right = slice values next size in
    let low = true in
    let high = false in
    if 0 <= index && index < size && element values index = pivot
      && sorted left && sorted right && all left pivot low
      && all right pivot high then (
      slice_length values zero index;
      slice_length values next size;
      sorted_intro values (fun query ->
        if 0 <= query && query < size - 1 then (
          let following = query + 1 in
          if following < index then (
            element_slice values zero index query;
            element_slice values zero index following;
            sorted_get left query;
            let u = () in refine_ u)
          else if query < index then (
            element_slice values zero index query;
            all_def left pivot low;
            range_get left pivot low zero index query;
            let value = element left query in
            accepts_def value pivot low;
            let u = () in refine_ u)
          else if query = index then (
            element_slice values next size zero;
            let length = size - next in
            all_def right pivot high;
            range_get right pivot high zero length zero;
            let value = element right zero in
            accepts_def value pivot high;
            let u = () in refine_ u)
          else (
            let relative = query - next in
            let following = relative + 1 in
            element_slice values next size relative;
            element_slice values next size following;
            sorted_get right relative;
            let u = () in refine_ u))
        else let u = () in refine_ u);
      let u = () in refine_ u)
    else let u = () in refine_ u

  let[@def] rec count_prefix (values : int iarray) (target : int) (past : int) =
    if past <= 0 then 0Z
    else let index = past - 1 in
      Bigint.add (count_prefix values target index)
        (if element values index = target then 1Z else 0Z)
  [@@decreases let past : int = past in if past > 0 then past else 0]

  let[@def] count (values : int iarray) (target : int) =
    count_prefix values target (Iarray.length values)

  let[@def] bag (values : int iarray) =
    Vox_int_sequence.bag (to_list values)

  let[@def] permutation (left : int iarray) (right : int iarray) =
    ghost_ (bag left === bag right)

  let rec (to_list_prefix_count @ total) : (values : int iarray) ->
      (past : int) -> (suffix : int list) -> (target : int) ->
      {u : unit | if 0 <= past && past <= Iarray.length values then
        Vox_int_sequence.count (Vox_sequence.from_iarray values past suffix)
          target ===
          Bigint.add (count_prefix values target past)
            (Vox_int_sequence.count suffix target) else true} =
    fun values past suffix target ->
      Vox_sequence.from_iarray_unfold values past suffix;
      count_prefix_def values target past;
      if 0 < past && past <= Iarray.length values then
        let index = past - 1 in
        let bounded : {i : int | 0 <= i && i < Iarray.length values} =
          refine_ index in
        let head = get values bounded in
        Vox_sequence.iarray_at_get values bounded;
        let next = head :: suffix in
        Vox_int_sequence.count_def next target;
        at_def values index;
        element_def values index;
        to_list_prefix_count values index next target;
        let u = () in refine_ u
      else let u = () in refine_ u
  [@@decreases let past : int = past in if past > 0 then past else 0]

  let (to_list_count @ total) : (values : int iarray) -> (target : int) ->
      {u : unit | Vox_int_sequence.count (to_list values) target ===
        count values target} = fun values target ->
    let size = Iarray.length values in
    let nil = [] in
    to_list_def values;
    Vox_sequence.of_iarray_def values;
    count_def values target;
    Vox_int_sequence.count_def nil target;
    to_list_prefix_count values size nil target;
    let u = () in refine_ u

  let (permutation_count @ total) : (before : int iarray) ->
      (after : int iarray) -> (target : int) ->
      {u : unit | if permutation before after then
        count before target === count after target else true} =
    fun before after target ->
      let left = to_list before in
      let right = to_list after in
      permutation_def before after;
      bag_def before;
      bag_def after;
      Vox_int_sequence.permutation_def left right;
      Vox_int_sequence.permutation_count left right target;
      to_list_count before target;
      to_list_count after target;
      let u = () in refine_ u

  let (count_extensional @ total) : (left : int iarray) -> (right : int iarray)
    ->
      ((target : int) -> {u : unit | count left target === count right target})
        @ total ->
      {u : unit | permutation left right} = fun left right proof ->
    let before = to_list left in
    let after = to_list right in
    Vox_int_sequence.count_extensional before after
      (fun target ->
        let refine_ known = proof target in
        to_list_count left target;
        to_list_count right target;
        let u = () in refine_ u);
    Vox_int_sequence.permutation_def before after;
    bag_def left;
    bag_def right;
    permutation_def left right;
    let u = () in refine_ u

  let rec (count_prefix_updated @ total) : (values : int iarray) ->
      (index : int) -> (value : int) -> (target : int) -> (past : int) ->
      {u : unit | if 0 <= past && past <= Iarray.length values then
        count_prefix (updated values index value) target past ===
          Bigint.add (count_prefix values target past)
            (if 0 <= index && index < past then
              Bigint.sub (if value = target then 1Z else 0Z)
                (if element values index = target then 1Z else 0Z)
             else 0Z) else true} = fun values index value target past ->
    let changed = updated values index value in
    count_prefix_def values target past;
    count_prefix_def changed target past;
    if past > 0 then
      let previous = past - 1 in
      element_updated values index value previous;
      count_prefix_updated values index value target previous;
      let u = () in refine_ u
    else let u = () in refine_ u
  [@@decreases let past : int = past in if past > 0 then past else 0]

  let (count_updated @ total) : (values : int iarray) -> (index : int) ->
      (value : int) -> (target : int) ->
      {u : unit | count (updated values index value) target ===
        Bigint.add (count values target)
          (if 0 <= index && index < Iarray.length values then
            Bigint.sub (if value = target then 1Z else 0Z)
              (if element values index = target then 1Z else 0Z)
           else 0Z)} = fun values index value target ->
    let changed = updated values index value in
    let size = Iarray.length values in
    updated_length values index value;
    count_def values target;
    count_def changed target;
    count_prefix_updated values index value target size;
    let u = () in refine_ u

  let (count_swap @ total) : (values : int iarray) -> (first : int) ->
      (second : int) -> (target : int) ->
      {u : unit | count (swap values first second) target === count values
        target} =
    fun values first second target ->
      swap_def values first second;
      at_def values first;
      at_def values second;
      let refine_ x = element_def values first in
      let refine_ y = element_def values second in
      match at values first, at values second with
      | Some x, Some y ->
        let middle = updated values first y in
        updated_length values first y;
        element_updated values first y second;
        count_updated values first y target;
        count_updated middle second x target;
        let u = () in refine_ u
      | _ -> let u = () in refine_ u

  let (permutation_swap @ total) : (values : int iarray) ->
      (first : int) -> (second : int) ->
      {u : unit | permutation values (swap values first second)} =
    fun values first second ->
      let after = swap values first second in
      count_extensional values after (fun target ->
        count_swap values first second target;
        let u = () in refine_ u);
      let u = () in refine_ u

  let (permutation_refl @ total) : (values : int iarray) ->
      {u : unit | permutation values values} = fun values ->
    permutation_def values values;
    let u = () in refine_ u

  let (permutation_trans @ total) : (first : int iarray) ->
      (second : int iarray) -> (third : int iarray) ->
      {u : unit | if permutation first second && permutation second third
        then permutation first third else true} = fun first second third ->
    permutation_def first second;
    permutation_def second third;
    permutation_def first third;
    let u = () in refine_ u

  let rec (count_prefix_slice @ total) : (values : int iarray) ->
      (first : int) -> (past : int) -> (target : int) -> (size : int) ->
      {u : unit | if 0 <= first && first <= past && past <= Iarray.length values
        && 0 <= size && size <= past - first then
        count_prefix (slice values first past) target size ===
          Bigint.sub (count_prefix values target (first + size))
            (count_prefix values target first) else true} =
    fun values first past target size ->
      let part = slice values first past in
      let finish = first + size in
      count_prefix_def part target size;
      if size > 0 then
        let previous = size - 1 in
        count_prefix_def values target finish;
        element_slice values first past previous;
        count_prefix_slice values first past target previous;
        let u = () in refine_ u
      else let u = () in refine_ u
  [@@decreases let size : int = size in if size > 0 then size else 0]

  let (count_slice @ total) : (values : int iarray) ->
      (first : int) -> (past : int) -> (target : int) ->
      {u : unit | if 0 <= first && first <= past && past <= Iarray.length values
        then count (slice values first past) target ===
          Bigint.sub (count_prefix values target past)
            (count_prefix values target first) else true} =
    fun values first past target ->
      let part = slice values first past in
      let size = past - first in
      slice_length values first past;
      count_def part target;
      count_prefix_slice values first past target size;
      let u = () in refine_ u

  let (count_decompose3 @ total) : (values : int iarray) ->
      (first : int) -> (past : int) -> (target : int) ->
      {u : unit | if 0 <= first && first <= past && past <= Iarray.length values
        then count values target ===
          Bigint.add (count (slice values 0 first) target)
            (Bigint.add (count (slice values first past) target)
              (count (slice values past (Iarray.length values)) target))
        else true} = fun values first past target ->
    let zero = 0 in
    let size = Iarray.length values in
    count_slice values zero first target;
    count_slice values first past target;
    count_slice values past size target;
    count_def values target;
    count_prefix_def values target zero;
    let u = () in refine_ u

  let rec (count_prefix_nonnegative @ total) : (values : int iarray) ->
      (target : int) -> (past : int) ->
      {u : unit | 0Z <= count_prefix values target past} = fun values target
        past ->
    count_prefix_def values target past;
    if past > 0 then
      let previous = past - 1 in
      count_prefix_nonnegative values target previous;
      let u = () in refine_ u
    else let u = () in refine_ u
  [@@decreases let past : int = past in if past > 0 then past else 0]

  let rec (count_prefix_present @ total) : (values : int iarray) ->
      (target : int) -> (past : int) -> (index : int) ->
      {u : unit | if 0 <= index && index < past && element values index = target
        then 0Z < count_prefix values target past else true} =
    fun values target past index ->
      count_prefix_def values target past;
      if past > 0 then
        let previous = past - 1 in
        count_prefix_nonnegative values target previous;
        if index = previous then let u = () in refine_ u
        else
          (count_prefix_present values target previous index;
          let u = () in refine_ u)
      else let u = () in refine_ u
  [@@decreases let past : int = past in if past > 0 then past else 0]

  let rec (count_prefix_bound @ total) : (values : int iarray) ->
      (target : int) -> (bound : int) -> (lower : bool) -> (past : int) ->
      {u : unit | if 0 <= past && past <= Iarray.length values
        && all values bound lower && not (accepts target bound lower)
        then count_prefix values target past === 0Z else true} =
    fun values target bound lower past ->
      let zero = 0 in
      let size = Iarray.length values in
      count_prefix_def values target past;
      all_def values bound lower;
      if past > 0 then
        let previous = past - 1 in
        range_get values bound lower zero size previous;
        count_prefix_bound values target bound lower previous;
        let u = () in refine_ u
      else let u = () in refine_ u
  [@@decreases let past : int = past in if past > 0 then past else 0]

  let (all_permutation @ total) : (before : int iarray) -> (after : int iarray)
    ->
      (bound : int) -> (lower : bool) ->
      {u : unit | permutation before after && all before bound lower} @ ghost ->
      {u : unit | all after bound lower} = fun before after bound lower premise
        ->
    premise;
    let zero = 0 in
    let old_size = Iarray.length before in
    let size = Iarray.length after in
    range_intro after bound lower zero size (fun index ->
      let target = element after index in
      count_def before target;
      count_def after target;
      permutation_count before after target;
      count_prefix_present after target size index;
      count_prefix_bound before target bound lower old_size;
      let u = () in refine_ u);
    all_def after bound lower;
    let u = () in refine_ u

  let (range_slice @ total) : (values : int iarray) -> (bound : int) ->
      (lower : bool) -> (first : int) -> (past : int) ->
      {u : unit | if 0 <= first && first <= past
        && past <= Iarray.length values
        && range values bound lower first past then
        all (slice values first past) bound lower else true} =
    fun values bound lower first past ->
      if 0 <= first && first <= past && past <= Iarray.length values
        && range values bound lower first past then
        let part = slice values first past in
        let zero = 0 in
        let size = Iarray.length part in
        slice_length values first past;
        range_intro part bound lower zero size (fun index ->
          let shifted = first + index in
          element_slice values first past index;
          range_get values bound lower first past shifted;
          let u = () in refine_ u);
        all_def part bound lower;
        let u = () in refine_ u
      else let u = () in refine_ u

  let (sorted_short @ total) : (values : int iarray) ->
      {u : unit | if Iarray.length values <= 1 then sorted values else true} =
    fun values ->
      let size = Iarray.length values in
      sorted_def values;
      sorted_prefix_def values size;
      let u = () in refine_ u

end
