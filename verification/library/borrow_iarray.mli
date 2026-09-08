@@ portable

type ('value, 'state) step =
  { value : 'value @@ global; state : 'state }

module Slice : sig @@ portable
  type ('a : immutable_data) t : value mod total contended

  external current : ('a : immutable_data).
    'a t @ local immutable -> 'a iarray @ total immutable ghost
    @@ total = "caml_borrow_current"

  external final : ('a : immutable_data).
    'a t @ local immutable -> 'a iarray @ total immutable ghost
    @@ total = "caml_borrow_final"

  val length : ('a : immutable_data).
    (s : 'a t) @ local immutable ->
    {n : int | n = Iarray.length (current s)}
    @@ total

  val get : ('a : immutable_data).
    (s : 'a t) @ local immutable ->
    (index : {i : int | 0 <= i && i < Iarray.length (current s)}) ->
    {value : 'a | let refine_ i = index in
      Some value === Vox_iarray.at (current s) i}
    @@ total

  val set : ('a : immutable_data).
    (s : 'a t) @ local unique ->
    (index : {i : int | 0 <= i && i < Iarray.length (current s)}) ->
    (value : 'a) @ immutable ->
    {r : 'a t | let refine_ i = index in
      current r === Vox_iarray.updated (current s) i value
      && final r === final s}
    @ local unique @@ total

  val snapshot : ('a : immutable_data).
    (s : 'a t) @ local immutable ->
    {values : 'a iarray | values === current s}
    @@ total

  val swap : ('a : immutable_data).
      (s : 'a t) @ local unique ->
      (first : {i : int | 0 <= i && i < Iarray.length (current s)}) ->
      (second : {i : int | 0 <= i && i < Iarray.length (current s)}) ->
      {r : 'a t | let refine_ i = first in let refine_ j = second in
        current r === Vox_iarray.swap (current s) i j && final r === final s}
      @ local unique @@ total

  val split_at : ('a : immutable_data) ('r : immutable_data).
      (s : 'a t) @ local unique ->
      (index : {k : int | 0 <= k && k <= Iarray.length (current s)}) ->
      (post : ('r @ immutable total -> 'a iarray @ total immutable ->
        'a iarray @ total immutable -> bool @ ghost)) @ ghost ->
      ((left : {left : 'a t | let refine_ k = index in
          current left === Vox_iarray.slice (current s) 0 k}) @ local unique ->
        (right : {right : 'a t | let refine_ k = index in
          current right === Vox_iarray.slice (current s) k
            (Iarray.length (current s))}) @ local unique ->
        {r : 'r | let refine_ l = left in let refine_ r_ = right in
          post r (final l) (final r_)}) @ local once ->
      {r : ('r, 'a t) step | let refine_ k = index in
        post r.value (Vox_iarray.slice (current r.state) 0 k)
          (Vox_iarray.slice (current r.state) k (Iarray.length (current
            r.state)))
        && final r.state === final s
        && Iarray.length (current r.state) = Iarray.length (current s)}
      @ local unique @@ total

  val finish : ('a : immutable_data).
      (s : 'a t) @ local unique -> {u : unit | final s === current s} @@ total

  val split3 : ('a : immutable_data) ('r : immutable_data).
      (s : 'a t) @ local unique ->
      (first : {i : int | 0 <= i && i <= Iarray.length (current s)}) ->
      (past : {j : int | let refine_ i = first in
        i <= j && j <= Iarray.length (current s)}) ->
      (post : ('r @ immutable total -> 'a iarray @ total immutable ->
        'a iarray @ total immutable -> 'a iarray @ total immutable -> bool @
          ghost)) @ ghost ->
      ((left : {left : 'a t | let refine_ i = first in
          current left === Vox_iarray.slice (current s) 0 i}) @ local unique ->
        (middle : {middle : 'a t | let refine_ i =
          first in let refine_ j = past in
          current middle === Vox_iarray.slice (current s) i j}) @ local unique
            ->
        (right : {right : 'a t | let refine_ j = past in
          current right === Vox_iarray.slice (current s) j
            (Iarray.length (current s))}) @ local unique ->
        {r : 'r | let refine_ l = left in let refine_ m = middle in
          let refine_ r_ = right in post r (final l) (final m) (final r_)})
        @ local once ->
      {r : ('r, 'a t) step | let refine_ i = first in let refine_ j = past in
        post r.value (Vox_iarray.slice (current r.state) 0 i)
          (Vox_iarray.slice (current r.state) i j)
          (Vox_iarray.slice (current r.state) j (Iarray.length (current
            r.state)))
        && final r.state === final s
        && Iarray.length (current r.state) = Iarray.length (current s)}
      @ local unique @@ total

  val parallel : ('a : immutable_data).
      (spawn : bool) ->
      (left : 'a t) @ local unique -> (right : 'a t) @ local unique ->
      (lp : ('a iarray @ total immutable -> bool @ ghost)) @ ghost ->
      (rp : ('a iarray @ total immutable -> bool @ ghost)) @ ghost ->
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
    'a t @ local immutable -> 'a iarray @ total immutable ghost
    @@ total = "caml_borrow_contents"

  val of_iarray : ('a : immutable_data).
    (values : 'a iarray) @ immutable ->
    {a : 'a t | contents a === values} @ unique
    @@ total

  val into_iarray : ('a : immutable_data).
    (a : 'a t) @ unique -> {values : 'a iarray | values === contents a}
    @@ total

  val with_mut : ('a : immutable_data) ('r : immutable_data).
      (a : 'a t) @ unique ->
      (post : ('r @ immutable total -> 'a iarray @ total immutable -> bool @
        ghost))
        @ ghost ->
      ((s : {s : 'a Slice.t | Slice.current s === contents a})
          @ local unique ->
        {r : 'r | let refine_ s = s in post r (Slice.final s)}) @ local once ->
      {r : ('r, 'a t) step | post r.value (contents r.state)
        && Iarray.length (contents r.state) = Iarray.length (contents a)}
      @ unique @@ total

end
