@@ portable

module Model = Vox_sequence

type ('value, 'state) step =
  { value : 'value @@ global;
    state : 'state }

(* The maximum length of an OCaml array, [Sys.max_array_length]. No owned
   array or slice is longer. *)
val max_length : unit -> int @@ total

(* Slices and owned arrays are updated by consuming a handle and returning
   its successor, so the operations on them can be functions of their
   arguments and be [total]. The operations that lend a slice
   ([Slice.split_at], [Slice.split3], [Slice.with_range],
   [Owned_array.with_mut]) and the one that ends a loan ([Slice.finish]) are
   not [total]. A new loan's [final] contents are chosen when it is created,
   and [finish] assumes that they equal its [current] contents; neither step
   is a function of the arguments. *)
module Slice : sig @@ portable
  type ('a : immutable_data) t : value mod total contended

  external current : ('a : immutable_data).
    'a t @ local immutable -> 'a Model.t @ immutable total ghost
    @@ total = "caml_borrow_current"

  external final : ('a : immutable_data).
    'a t @ local immutable -> 'a Model.t @ immutable total ghost
    @@ total = "caml_borrow_final"

  val length : ('a : immutable_data).
    (s : 'a t) @ local immutable ->
    {n : int | 0 <= n
      && Bigint.of_int n === Model.length (current s)} @@ total

  val get : ('a : immutable_data).
    (s : 'a t) @ local immutable ->
    (index : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (current s)) < 0}) ->
    {value : 'a | let index = index in
      Some value === Model.at (current s) (Bigint.of_int index)} @@ total

  val set : ('a : immutable_data).
    (s : 'a t) @ local unique ->
    (index : {i : int |
      0 <= i && Bigint.compare (Bigint.of_int i) (Model.length (current s)) <
        0}) ->
    (value : 'a) @ immutable ->
    {r : 'a t | let index = index in
      current r === Model.set (current s) (Bigint.of_int index) value
      && final r === final s} @ local unique @@ total

  val snapshot : ('a : immutable_data).
    (s : 'a t) @ local immutable ->
    {values : 'a iarray | Model.of_iarray values === current s} @@ total

  val swap : ('a : immutable_data).
      (s : 'a t) @ local unique ->
      (first : {i : int | 0 <= i
        && Bigint.compare (Bigint.of_int i) (Model.length (current s)) < 0}) ->
      (second : {i : int | 0 <= i
        && Bigint.compare (Bigint.of_int i) (Model.length (current s)) < 0}) ->
      {r : 'a t | let i = first in let j = second in
        current r === Model.swap (current s) (Bigint.of_int i) (Bigint.of_int j)
        && final r === final s
        && Model.length (current r) === Model.length (current s)}
      @ local unique @@ total

  val split_at : ('a : immutable_data) ('r : immutable_data).
      (s : 'a t) @ local unique ->
      (index : {k : int | 0 <= k
        && Bigint.compare (Bigint.of_int k) (Model.length (current s)) <= 0}) ->
      (post : ('r @ immutable total -> 'a Model.t @ immutable ->
        'a Model.t @ immutable -> bool @ ghost)) @ ghost ->
      ((left : {left : 'a t | let k = index in
          current left === Model.take (Bigint.of_int k) (current s)})
          @ local unique ->
        (right : {right : 'a t | let k = index in
          current right === Model.drop (Bigint.of_int k) (current s)})
          @ local unique ->
        {r : 'r | let left = left in let right = right in
          post r (final left) (final right)}) @ local once ->
      {r : ('r, 'a t) step | let k = index in
        post r.value (Model.take (Bigint.of_int k) (current r.state))
          (Model.drop (Bigint.of_int k) (current r.state))
        && final r.state === final s
        && Model.length (current r.state) === Model.length (current s)}
      @ local unique @@ stateless

  val finish : ('a : immutable_data).
      (s : 'a t) @ local unique ->
      {u : unit | final s === current s} @@ stateless


  val split3 : ('a : immutable_data) ('r : immutable_data).
      (s : 'a t) @ local unique ->
      (first : {i : int | 0 <= i
        && Bigint.compare (Bigint.of_int i) (Model.length (current s)) <= 0}) ->
      (past : {j : int | let i = first in i <= j
        && Bigint.compare (Bigint.of_int j) (Model.length (current s)) <= 0}) ->
      (post : ('r @ immutable total -> 'a Model.t @ immutable ->
        'a Model.t @ immutable -> 'a Model.t @ immutable -> bool @ ghost)) @
          ghost ->
      ((left : {left : 'a t | let i = first in
          current left === Model.take (Bigint.of_int i) (current s)}) @ local
            unique ->
        (middle : {middle : 'a t | let i = first in let j = past
          in
          current middle === Model.sub (current s) (Bigint.of_int i)
            (Bigint.of_int j)})
          @ local unique ->
        (right : {right : 'a t | let j = past in
          current right === Model.drop (Bigint.of_int j) (current s)}) @ local
            unique ->
        {r : 'r | let left = left in let middle = middle in
          let right = right in post r (final left) (final middle) (final
            right)})
        @ local once ->
      {r : ('r, 'a t) step | let i = first in let j = past in
        post r.value (Model.take (Bigint.of_int i) (current r.state))
          (Model.sub (current r.state) (Bigint.of_int i) (Bigint.of_int j))
          (Model.drop (Bigint.of_int j) (current r.state))
        && final r.state === final s
        && Model.length (current r.state) === Model.length (current s)}
      @ local unique @@ stateless


  val with_range : ('a : immutable_data) ('r : immutable_data).
      (s : 'a t) @ local unique ->
      (first : {i : int | 0 <= i
        && Bigint.compare (Bigint.of_int i) (Model.length (current s)) <= 0}) ->
      (past : {j : int | let i = first in i <= j
        && Bigint.compare (Bigint.of_int j) (Model.length (current s)) <= 0}) ->
      (post : ('r @ immutable total -> 'a Model.t @ immutable -> bool @ ghost))
        @ ghost ->
      ((middle : {middle : 'a t | let i = first in let j = past
        in
          current middle === Model.sub (current s) (Bigint.of_int i)
            (Bigint.of_int j)})
          @ local unique ->
        {r : 'r | let middle = middle in post r (final middle)}) @ local
          once ->
      {r : ('r, 'a t) step | let i = first in let j = past in
        post r.value (Model.sub (current r.state) (Bigint.of_int i)
          (Bigint.of_int j))
        && current r.state === Model.append (Model.take (Bigint.of_int i)
          (current s))
          (Model.append (Model.sub (current r.state) (Bigint.of_int i)
            (Bigint.of_int j))
            (Model.drop (Bigint.of_int j) (current s)))
        && final r.state === final s
        && Model.length (current r.state) === Model.length (current s)}
      @ local unique @@ stateless

  val parallel : ('a : immutable_data).
      (spawn : bool) ->
      (left : 'a t) @ local unique -> (right : 'a t) @ local unique ->
      (lp : ('a Model.t @ immutable -> bool @ ghost)) @ ghost ->
      (rp : ('a Model.t @ immutable -> bool @ ghost)) @ ghost ->
      ((s : {s : 'a t | current s === current left
          && final s === final left}) @ local unique ->
        {u : unit | let s = s in lp (final s)}) @ portable once ->
      ((s : {s : 'a t | current s === current right
          && final s === final right}) @ local unique ->
        {u : unit | let s = s in rp (final s)}) @ portable once ->
      {u : unit | lp (final left) && rp (final right)}

end

module Owned_array : sig @@ portable
  type ('a : immutable_data) t : value mod total contended

  external contents : ('a : immutable_data).
    'a t @ local immutable -> 'a Model.t @ immutable total ghost
    @@ total = "caml_borrow_contents"

  val of_iarray : ('a : immutable_data).
    (values : 'a iarray) @ immutable ->
    {a : 'a t | contents a === Model.of_iarray values} @ unique @@ total

  val into_iarray : ('a : immutable_data).
    (a : 'a t) @ unique ->
    {values : 'a iarray | Model.of_iarray values === contents a} @@ total

  val length : ('a : immutable_data).
    (a : 'a t) @ local immutable ->
    {n : int | 0 <= n && n <= max_length ()
      && Bigint.of_int n === Model.length (contents a)} @@ total

  val get : ('a : immutable_data).
    (a : 'a t) @ local immutable ->
    (index : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (contents a)) < 0}) ->
    {value : 'a | let index = index in
      Some value === Model.at (contents a) (Bigint.of_int index)} @@ total

  val set : ('a : immutable_data).
    (a : 'a t) @ unique ->
    (index : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (contents a)) < 0}) ->
    (value : 'a) @ immutable ->
    {r : 'a t | let index = index in
      contents r === Model.set (contents a) (Bigint.of_int index) value}
    @ unique @@ total

  val swap : ('a : immutable_data).
    (a : 'a t) @ unique ->
    (first : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (contents a)) < 0}) ->
    (second : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (contents a)) < 0}) ->
    {r : 'a t | let i = first in let j = second in
      contents r === Model.swap (contents a) (Bigint.of_int i) (Bigint.of_int j)
      && Model.length (contents r) === Model.length (contents a)}
    @ unique @@ total

  (* The two owners share [a]'s storage; nothing is copied. *)
  val split_at : ('a : immutable_data).
    (a : 'a t) @ unique ->
    (index : {k : int | 0 <= k
      && Bigint.compare (Bigint.of_int k) (Model.length (contents a)) <= 0}) ->
    {r : 'a t * 'a t | let k = index in
      match r with left, right ->
        contents left === Model.take (Bigint.of_int k) (contents a)
        && contents right === Model.drop (Bigint.of_int k) (contents a)}
    @ unique @@ total

  (* Adjacent pieces of one array (as produced by [split_at]) are joined in
     place; any other pair is copied into a new array. The copy would raise
     [Invalid_argument] above [max_length ()] elements, which the
     precondition excludes. *)
  val append : ('a : immutable_data).
    (left : 'a t) @ unique ->
    (right : {right : 'a t | Bigint.compare
      (Bigint.add (Model.length (contents left))
        (Model.length (contents right)))
      (Bigint.of_int (max_length ())) <= 0}) @ unique ->
    {r : 'a t | contents r === Model.append (contents left) (contents right)}
    @ unique @@ total

  val with_mut : ('a : immutable_data) ('r : immutable_data).
      (a : 'a t) @ unique ->
      (post : ('r @ immutable total -> 'a Model.t @ immutable -> bool @ ghost))
        @ ghost ->
      ((s : {s : 'a Slice.t | Slice.current s === contents a})
          @ local unique ->
        {r : 'r | let s = s in post r (Slice.final s)}) @ local once ->
      {r : ('r, 'a t) step | post r.value (contents r.state)
        && Model.length (contents r.state) === Model.length (contents a)}
      @ unique @@ stateless
end
