@@ portable

module Model = Borrow_model

type ('value, 'state) step =
  { value : 'value @@ global;
    state : 'state }

module Slice : sig @@ portable
  type ('a : immutable_data) t : value mod total contended

  external current : ('a : immutable_data).
    'a t @ local immutable -> 'a Model.t @ immutable total ghost
    @@ total = "caml_borrow_current"

  external final : ('a : immutable_data).
    'a t @ local immutable -> 'a Model.t @ immutable total ghost
    @@ total = "caml_borrow_final"

  val length : ('a : immutable_data).
      (s : 'a t) @ local unique ->
      {r : (int, 'a t) step |
        0 <= r.value
        && Bigint.of_int r.value === Model.length (current s)
        && current r.state === current s && final r.state === final s}
      @ local unique

  val get : ('a : immutable_data).
    (s : 'a t) @ local unique ->
    (index : {i : int |
      0 <= i && Bigint.compare (Bigint.of_int i) (Model.length (current s)) < 0}) ->
    {r : ('a, 'a t) step |
      let refine_ index = index in
      Some r.value === Model.at (current s) (Bigint.of_int index)
      && current r.state === current s && final r.state === final s}
    @ local unique

  val set : ('a : immutable_data).
    (s : 'a t) @ local unique ->
    (index : {i : int |
      0 <= i && Bigint.compare (Bigint.of_int i) (Model.length (current s)) < 0}) ->
    (value : 'a) @ immutable ->
    {r : 'a t | let refine_ index = index in
      current r === Model.set (current s) (Bigint.of_int index) value
      && final r === final s} @ local unique

  val snapshot : ('a : immutable_data).
    (s : 'a t) @ local unique ->
    {r : ('a iarray, 'a t) step |
      Model.of_iarray r.value === current s
      && current r.state === current s && final r.state === final s}
    @ local unique

  val swap : ('a : immutable_data).
      (s : 'a t) @ local unique ->
      (first : {i : int | 0 <= i
        && Bigint.compare (Bigint.of_int i) (Model.length (current s)) < 0}) ->
      (second : {i : int | 0 <= i
        && Bigint.compare (Bigint.of_int i) (Model.length (current s)) < 0}) ->
      {r : 'a t | let refine_ i = first in let refine_ j = second in
        current r === Model.swap (current s) (Bigint.of_int i) (Bigint.of_int j)
        && final r === final s
        && Model.length (current r) === Model.length (current s)}
      @ local unique

  val split_at : ('a : immutable_data) ('r : immutable_data).
      (s : 'a t) @ local unique ->
      (index : {k : int | 0 <= k
        && Bigint.compare (Bigint.of_int k) (Model.length (current s)) <= 0}) ->
      (post : ('r @ immutable total -> 'a Model.t @ immutable ->
        'a Model.t @ immutable -> bool @ ghost)) @ ghost ->
      ((left : {left : 'a t | let refine_ k = index in
          current left === Model.take (Bigint.of_int k) (current s)})
          @ local unique ->
        (right : {right : 'a t | let refine_ k = index in
          current right === Model.drop (Bigint.of_int k) (current s)})
          @ local unique ->
        {r : 'r | let refine_ left = left in let refine_ right = right in
          post r (final left) (final right)}) @ local once ->
      {r : ('r, 'a t) step | let refine_ k = index in
        post r.value (Model.take (Bigint.of_int k) (current r.state))
          (Model.drop (Bigint.of_int k) (current r.state))
        && final r.state === final s
        && Model.length (current r.state) === Model.length (current s)}
      @ local unique

  val finish : ('a : immutable_data).
      (s : 'a t) @ local unique ->
      {u : unit | final s === current s}


  val split3 : ('a : immutable_data) ('r : immutable_data).
      (s : 'a t) @ local unique ->
      (first : {i : int | 0 <= i
        && Bigint.compare (Bigint.of_int i) (Model.length (current s)) <= 0}) ->
      (past : {j : int | let refine_ i = first in i <= j
        && Bigint.compare (Bigint.of_int j) (Model.length (current s)) <= 0}) ->
      (post : ('r @ immutable total -> 'a Model.t @ immutable ->
        'a Model.t @ immutable -> 'a Model.t @ immutable -> bool @ ghost)) @ ghost ->
      ((left : {left : 'a t | let refine_ i = first in
          current left === Model.take (Bigint.of_int i) (current s)}) @ local unique ->
        (middle : {middle : 'a t | let refine_ i = first in let refine_ j = past in
          current middle === Model.sub (current s) (Bigint.of_int i) (Bigint.of_int j)})
          @ local unique ->
        (right : {right : 'a t | let refine_ j = past in
          current right === Model.drop (Bigint.of_int j) (current s)}) @ local unique ->
        {r : 'r | let refine_ left = left in let refine_ middle = middle in
          let refine_ right = right in post r (final left) (final middle) (final right)})
        @ local once ->
      {r : ('r, 'a t) step | let refine_ i = first in let refine_ j = past in
        post r.value (Model.take (Bigint.of_int i) (current r.state))
          (Model.sub (current r.state) (Bigint.of_int i) (Bigint.of_int j))
          (Model.drop (Bigint.of_int j) (current r.state))
        && final r.state === final s
        && Model.length (current r.state) === Model.length (current s)}
      @ local unique


  val with_range : ('a : immutable_data) ('r : immutable_data).
      (s : 'a t) @ local unique ->
      (first : {i : int | 0 <= i
        && Bigint.compare (Bigint.of_int i) (Model.length (current s)) <= 0}) ->
      (past : {j : int | let refine_ i = first in i <= j
        && Bigint.compare (Bigint.of_int j) (Model.length (current s)) <= 0}) ->
      (post : ('r @ immutable total -> 'a Model.t @ immutable -> bool @ ghost)) @ ghost ->
      ((middle : {middle : 'a t | let refine_ i = first in let refine_ j = past in
          current middle === Model.sub (current s) (Bigint.of_int i) (Bigint.of_int j)})
          @ local unique ->
        {r : 'r | let refine_ middle = middle in post r (final middle)}) @ local once ->
      {r : ('r, 'a t) step | let refine_ i = first in let refine_ j = past in
        post r.value (Model.sub (current r.state) (Bigint.of_int i) (Bigint.of_int j))
        && current r.state === Model.append (Model.take (Bigint.of_int i) (current s))
          (Model.append (Model.sub (current r.state) (Bigint.of_int i) (Bigint.of_int j))
            (Model.drop (Bigint.of_int j) (current s)))
        && final r.state === final s
        && Model.length (current r.state) === Model.length (current s)}
      @ local unique

  val parallel : ('a : immutable_data).
      (spawn : bool) ->
      (left : 'a t) @ local unique -> (right : 'a t) @ local unique ->
      (lp : ('a Model.t @ immutable -> bool @ ghost)) @ ghost ->
      (rp : ('a Model.t @ immutable -> bool @ ghost)) @ ghost ->
      ((s : {s : 'a t | current s === current left
          && final s === final left}) @ local unique ->
        {u : unit | let refine_ s = s in lp (final s)}) @ portable once ->
      ((s : {s : 'a t | current s === current right
          && final s === final right}) @ local unique ->
        {u : unit | let refine_ s = s in rp (final s)}) @ portable once ->
      {u : unit | lp (final left) && rp (final right)}

end

module Owned_array : sig @@ portable
  type ('a : immutable_data) t : value mod total contended

  external contents : ('a : immutable_data).
    'a t @ local immutable -> 'a Model.t @ immutable total ghost
    @@ total = "caml_borrow_contents"

  val of_iarray : ('a : immutable_data).
    (values : 'a iarray) @ immutable ->
    {a : 'a t | contents a === Model.of_iarray values} @ unique

  val into_iarray : ('a : immutable_data).
    (a : 'a t) @ unique ->
    {values : 'a iarray | Model.of_iarray values === contents a}

  val with_mut : ('a : immutable_data) ('r : immutable_data).
      (a : 'a t) @ unique ->
      (post : ('r @ immutable total -> 'a Model.t @ immutable -> bool @ ghost))
        @ ghost ->
      ((s : {s : 'a Slice.t | Slice.current s === contents a})
          @ local unique ->
        {r : 'r | let refine_ s = s in post r (Slice.final s)}) @ local once ->
      {r : ('r, 'a t) step | post r.value (contents r.state)
        && Model.length (contents r.state) === Model.length (contents a)}
      @ unique
end
