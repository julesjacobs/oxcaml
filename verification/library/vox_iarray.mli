external get : ('a : immutable_data).
  (values : 'a iarray) @ immutable total ->
  {i : int | 0 <= i && i < Iarray.length values} ->
  'a @ immutable total @@ total = "%array_safe_get"

val at : ('a : immutable_data).
  'a iarray @ immutable total -> int -> 'a option @ immutable total @@ total
val at_get : ('a : immutable_data).
  (values : 'a iarray) @ immutable ->
  (index : {i : int | 0 <= i && i < Iarray.length values}) ->
  {u : unit | let refine_ i = index in
    at values i === Some (get values index)} @@ total
val at_outside : ('a : immutable_data).
  (values : 'a iarray) @ immutable -> (index : int) ->
  {u : unit | if index < 0 || Iarray.length values <= index then
    at values index === None else true} @@ total

val updated : ('a : immutable_data).
  'a iarray @ immutable total -> int -> 'a @ immutable total ->
  'a iarray @ immutable total @@ total
val slice : ('a : immutable_data).
  'a iarray @ immutable total -> int -> int ->
  'a iarray @ immutable total @@ total
val swap : ('a : immutable_data).
  'a iarray @ immutable total -> int -> int ->
  'a iarray @ immutable total @@ total
val swap_def : ('a : immutable_data).
  (values : 'a iarray) @ immutable -> (first : int) -> (second : int) ->
  {u : unit | swap values first second ===
    (match at values first, at values second with
     | Some x, Some y -> updated (updated values first y) second x
     | _ -> values)} @@ total
val to_list : ('a : immutable_data).
  'a iarray @ immutable total -> 'a list @ immutable total @@ total

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

val set_read : ('a : immutable_data).
  (values : 'a iarray) @ immutable ->
  (index : {i : int | 0 <= i && i < Iarray.length values}) ->
  (value : 'a) @ immutable -> (query : int) ->
  {u : unit | let refine_ i = index in
    let refine_ changed = set values index value in
    at changed query === (if i = query then Some value else at values query)} @@
      total

val updated_length : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (index : int) ->
    (value : 'a) @ immutable ->
    {u : unit | Iarray.length (updated values index value) =
      Iarray.length values} @@ total

val updated_read : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (index : int) ->
    (value : 'a) @ immutable -> (query : int) ->
    {u : unit | at (updated values index value) query ===
      (if 0 <= index && index < Iarray.length values && index = query then
        Some value else at values query)} @@ total

val slice_length : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (first : int) -> (past : int) ->
    {u : unit | if 0 <= first && first <= past && past <= Iarray.length values
      then Iarray.length (slice values first past) = past - first else true} @@
        total

val slice_read : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (first : int) -> (past : int) ->
    (index : int) ->
    {u : unit | if 0 <= first && first <= past && past <= Iarray.length values
      then at (slice values first past) index ===
        (if 0 <= index && index < past - first then
          at values (first + index) else None) else true} @@ total

val slice_all : ('a : immutable_data).
    (values : 'a iarray) @ immutable ->
    {u : unit | slice values 0 (Iarray.length values) === values} @@ total

val slice_slice : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (first : int) -> (past : int) ->
    (lower : int) -> (upper : int) ->
    {u : unit | if 0 <= first && first <= past && past <= Iarray.length values
      && 0 <= lower && lower <= upper && upper <= past - first then
      slice (slice values first past) lower upper ===
        slice values (first + lower) (first + upper) else true} @@ total

val swap_length : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (first : int) -> (second : int) ->
    {u : unit | Iarray.length (swap values first second) = Iarray.length values}
      @@ total

val swap_read : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (first : int) -> (second : int) ->
    (query : int) ->
    {u : unit | if 0 <= first && first < Iarray.length values && 0 <= second
      && second < Iarray.length values then
      at (swap values first second) query ===
        (if query = first then at values second else
          if query = second then at values first else at values query)
      else true} @@ total

val to_list_length : ('a : immutable_data).
    (values : 'a iarray) @ immutable ->
    {u : unit | Vox_sequence.length (to_list values) ===
      Bigint.of_int (Iarray.length values)} @@ total

val to_list_at : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (index : int) ->
    {u : unit | Vox_sequence.at (to_list values) (Bigint.of_int index)
      === at values index} @@ total

val to_list_updated_at : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (index : int) ->
    (value : 'a) @ immutable -> (query : int) ->
    {u : unit | Vox_sequence.at (to_list (updated values index value))
      (Bigint.of_int query) ===
      (if 0 <= index && index < Iarray.length values && index = query then
        Some value else Vox_sequence.at (to_list values) (Bigint.of_int query))}
          @@ total

val to_list_slice_at : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (first : int) -> (past : int) ->
    (query : int) ->
    {u : unit | if 0 <= first && first <= past && past <= Iarray.length values
      then Vox_sequence.at (to_list (slice values first past))
        (Bigint.of_int query) ===
        (if 0 <= query && query < past - first then
          Vox_sequence.at (to_list values) (Bigint.of_int (first + query))
        else None) else true} @@ total

module For_all (P : Vox_sequence.Predicate) : sig
  val range : P.element iarray @ immutable total -> int -> int -> bool
    @@ total
  val holds : P.element iarray @ immutable total -> bool @@ total
  val holds_def : (values : P.element iarray) @ immutable ->
    {u : unit | holds values === range values 0 (Iarray.length values)} @@ total
  val range_get : (values : P.element iarray) @ immutable ->
      (first : int) -> (past : int) -> (index : int) ->
      {u : unit | if range values first past && 0 <= index
        && first <= index && index < past then
        match at values index with None -> true | Some value -> P.test value
        else true} @@ total

  val range_intro : (values : P.element iarray) @ immutable ->
      (first : int) -> (past : int) ->
      ((index : int) -> {u : unit | if 0 <= index
        && first <= index && index < past then
        match at values index with None -> true | Some value -> P.test value
        else true}) @ total ->
      {u : unit | range values first past} @@ total

  val get : (values : P.element iarray) @ immutable ->
      (index : int) ->
      {u : unit | if holds values then match at values index with
        None -> true | Some value -> P.test value else true} @@ total

  val intro : (values : P.element iarray) @ immutable ->
      ((index : int) -> {u : unit | if 0 <= index
        && index < Iarray.length values then match at values index with
        None -> true | Some value -> P.test value else true}) @ total ->
      {u : unit | holds values} @@ total

  val updated_holds : (values : P.element iarray) @ immutable ->
      (index : int) -> (value : P.element) @ immutable ->
      {u : unit | if holds values && P.test value then
        holds (updated values index value) else true} @@ total

  val slice_holds : (values : P.element iarray) @ immutable ->
      (first : int) -> (past : int) ->
      {u : unit | if holds values && 0 <= first && first <= past
        && past <= Iarray.length values then holds (slice values first past)
        else true} @@ total

  val split_holds : (values : P.element iarray) @ immutable ->
      (middle : int) ->
      {u : unit | if 0 <= middle && middle <= Iarray.length values then
        holds values = (holds (slice values 0 middle)
          && holds (slice values middle (Iarray.length values))) else true} @@
            total
end

module Int : sig
  val element : int iarray -> int -> int @@ total
  val element_def : (values : int iarray) -> (index : int) ->
    {u : unit | element values index ===
      (match at values index with Some value -> value | None -> 0)} @@ total
  val accepts : int -> int -> bool -> bool @@ total
  val accepts_def : (value : int) -> (bound : int) -> (lower : bool) ->
    {u : unit | accepts value bound lower ===
      (if lower then value <= bound else bound <= value)} @@ total
  val range : int iarray -> int -> bool -> int -> int -> bool @@ total
  val all : int iarray -> int -> bool -> bool @@ total
  val all_def : (values : int iarray) -> (bound : int) -> (lower : bool) ->
    {u : unit | all values bound lower ===
      range values bound lower 0 (Iarray.length values)} @@ total
  val all_le : int iarray -> int -> bool @@ total
  val all_ge : int iarray -> int -> bool @@ total
  val sorted : int iarray -> bool @@ total
  val count : int iarray -> int -> Bigint.t @@ total
  val bag : int iarray -> Vox_int_sequence.multiset @@ total
  val permutation : int iarray -> int iarray -> bool @ ghost @@ total

  val range_get :
        (values : int iarray) -> (bound : int) -> (lower : bool) ->
        (first : int) -> (past : int) -> (index : int) ->
        {u : unit | if 0 <= index && first <= index && index < past
          && index < Iarray.length values && range values bound lower first past
          then accepts (element values index) bound lower else true} @@ total

  val range_intro :
        (values : int iarray) -> (bound : int) -> (lower : bool) ->
        (first : int) -> (past : int) ->
        ((index : int) ->
          {u : unit | if 0 <= index && first <= index && index < past
            && index < Iarray.length values then
            accepts (element values index) bound lower else true}) @ total ->
        {u : unit | range values bound lower first past} @@ total

  val element_updated : (values : int iarray) -> (index : int) ->
        (value : int) -> (query : int) ->
        {u : unit | element (updated values index value) query =
          (if 0 <= index && index < Iarray.length values && index = query
            then value else element values query)} @@ total

  val element_slice : (values : int iarray) ->
        (first : int) -> (past : int) -> (index : int) ->
        {u : unit | if 0 <= first && first <= past && past <= Iarray.length
          values
          && 0 <= index && index < past - first then
          element (slice values first past) index = element values (first +
            index)
          else true} @@ total

  val element_swap : (values : int iarray) ->
        (first : int) -> (second : int) -> (index : int) ->
        {u : unit | if 0 <= first && first < Iarray.length values
          && 0 <= second && second < Iarray.length values then
          element (swap values first second) index =
            (if index = first then element values second else
              if index = second then element values first else element values
                index)
          else true} @@ total

  val range_empty : (values : int iarray) -> (bound : int) ->
        (lower : bool) -> (first : int) -> (past : int) ->
        {u : unit | if past <= first then range values bound lower first past
          else true} @@ total

  val range_shrink : (values : int iarray) -> (bound : int) ->
        (lower : bool) -> (first : int) -> (past : int) ->
        (new_first : int) -> (new_past : int) ->
        {u : unit | if first <= new_first && new_past <= past
          && range values bound lower first past then
          range values bound lower new_first new_past else true} @@ total

  val range_grow : (values : int iarray) -> (bound : int) ->
        (lower : bool) -> (first : int) -> (past : int) ->
        {u : unit | if 0 <= first && first <= past && past < Iarray.length
          values
          && range values bound lower first past
          && accepts (element values past) bound lower then
          range values bound lower first (past + 1) else true} @@ total

  val range_set : (values : int iarray) -> (bound : int) ->
        (lower : bool) -> (first : int) -> (past : int) ->
        (index : int) -> (value : int) ->
        {u : unit | if range values bound lower first past &&
          (index < first || past <= index || accepts value bound lower) then
          range (updated values index value) bound lower first past else true}
            @@ total

  val sorted_intro : (values : int iarray) ->
        ((index : int) -> {u : unit | if 0 <= index
          && index < Iarray.length values - 1 then
          element values index <= element values (index + 1) else true}) @ total
            ->
        {u : unit | sorted values} @@ total

  val sorted_get : (values : int iarray) -> (index : int) ->
        {u : unit | if sorted values && 0 <= index
          && index < Iarray.length values - 1 then
          element values index <= element values (index + 1) else true} @@ total

  val ordered : (values : int iarray) ->
      (first : int) -> (last : int) ->
      {u : unit | if sorted values && 0 <= first && first <= last
        && last < Iarray.length values then
        element values first <= element values last else true} @@ total

  val sorted_slice : (values : int iarray) ->
      (first : int) -> (past : int) ->
      {u : unit | if sorted values && 0 <= first && first <= past
        && past <= Iarray.length values then sorted (slice values first past)
        else true} @@ total

  val sorted_updated : (values : int iarray) -> (index : int) ->
      (value : int) ->
      {u : unit | if sorted values && 0 <= index && index < Iarray.length values
        && (index = 0 || element values (index - 1) <= value)
        && (index = Iarray.length values - 1
          || value <= element values (index + 1)) then
        sorted (updated values index value) else true} @@ total

  val sorted_glue : (values : int iarray) -> (pivot : int) ->
    (index : int) ->
    {u : unit | if 0 <= index && index < Iarray.length values
      && element values index = pivot
      && sorted (slice values 0 index)
      && sorted (slice values (index + 1) (Iarray.length values))
      && all (slice values 0 index) pivot true
      && all (slice values (index + 1) (Iarray.length values)) pivot false then
      sorted values else true} @@ total

  val to_list_count : (values : int iarray) -> (target : int) ->
        {u : unit | Vox_int_sequence.count (to_list values) target ===
          count values target} @@ total

  val permutation_count : (before : int iarray) ->
        (after : int iarray) -> (target : int) ->
        {u : unit | if permutation before after then
          count before target === count after target else true} @@ total

  val count_extensional : (left : int iarray) -> (right : int iarray) ->
        ((target : int) -> {u : unit | count left target === count right
          target})
          @ total ->
        {u : unit | permutation left right} @@ total

  val count_updated : (values : int iarray) -> (index : int) ->
        (value : int) -> (target : int) ->
        {u : unit | count (updated values index value) target ===
          Bigint.add (count values target)
            (if 0 <= index && index < Iarray.length values then
              Bigint.sub (if value = target then 1Z else 0Z)
                (if element values index = target then 1Z else 0Z)
             else 0Z)} @@ total

  val count_swap : (values : int iarray) -> (first : int) ->
        (second : int) -> (target : int) ->
        {u : unit | count (swap values first second) target === count values
          target} @@ total

  val permutation_swap : (values : int iarray) ->
        (first : int) -> (second : int) ->
        {u : unit | permutation values (swap values first second)} @@ total

  val permutation_refl : (values : int iarray) ->
        {u : unit | permutation values values} @@ total

  val permutation_trans : (first : int iarray) ->
        (second : int iarray) -> (third : int iarray) ->
        {u : unit | if permutation first second && permutation second third
          then permutation first third else true} @@ total

  val count_decompose3 : (values : int iarray) ->
        (first : int) -> (past : int) -> (target : int) ->
        {u : unit | if 0 <= first && first <= past && past <= Iarray.length
          values
          then count values target ===
            Bigint.add (count (slice values 0 first) target)
              (Bigint.add (count (slice values first past) target)
                (count (slice values past (Iarray.length values)) target))
          else true} @@ total

  val all_permutation : (before : int iarray) -> (after : int iarray) ->
        (bound : int) -> (lower : bool) ->
        {u : unit | permutation before after && all before bound lower} @ ghost
          ->
        {u : unit | all after bound lower} @@ total

  val range_slice : (values : int iarray) -> (bound : int) ->
        (lower : bool) -> (first : int) -> (past : int) ->
        {u : unit | if 0 <= first && first <= past
          && past <= Iarray.length values
          && range values bound lower first past then
          all (slice values first past) bound lower else true} @@ total

  val sorted_short : (values : int iarray) ->
        {u : unit | if Iarray.length values <= 1 then sorted values else true}
          @@ total

end
