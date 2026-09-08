open! Stdlib

module Model = Vox_sequence

type ('value, 'state) step =
  { value : 'value @@ global;
    state : 'state }

type ('a : immutable_data) owned :
  value mod total contended
type ('a : immutable_data) loan :
  value mod total contended
type ('a : immutable_data) root_frame :
  value mod total contended
type ('a : immutable_data) split_frame :
  value mod total contended

module Raw = struct
  external contents : ('a : immutable_data).
    'a owned @ local immutable -> 'a Model.t @ immutable total ghost
    @@ total = "caml_borrow_contents"
  external current : ('a : immutable_data).
    'a loan @ local immutable -> 'a Model.t @ immutable total ghost
    @@ total = "caml_borrow_current"
  external final : ('a : immutable_data).
    'a loan @ local immutable -> 'a Model.t @ immutable total ghost
    @@ total = "caml_borrow_final"
  external root_final : ('a : immutable_data).
    'a root_frame @ local immutable -> 'a Model.t @ immutable total ghost
    @@ total = "caml_borrow_frame_final"
  external parent_final : ('a : immutable_data).
    'a split_frame @ local immutable -> 'a Model.t @ immutable total ghost
    @@ total = "caml_borrow_frame_final"
  external left_final : ('a : immutable_data).
    'a split_frame @ local immutable -> 'a Model.t @ immutable total ghost
    @@ total = "caml_borrow_frame_left"
  external right_final : ('a : immutable_data).
    'a split_frame @ local immutable -> 'a Model.t @ immutable total ghost
    @@ total = "caml_borrow_frame_right"
  external open_ : ('a : immutable_data).
    'a owned @ unique -> ('a root_frame * 'a loan) @ unique @@ portable =
      "caml_borrow_open"
  external restore : ('a : immutable_data).
    'a root_frame @ unique -> 'a owned @ unique @@ portable =
      "caml_borrow_restore"
  external transfer : ('a : immutable_data).
    'a loan @ local unique -> 'a loan @ unique
    @@ portable = "caml_borrow_transfer"
  external length : ('a : immutable_data).
    'a loan @ local immutable -> int @@ portable =
      "caml_borrow_length"
  external finish : ('a : immutable_data).
    'a loan @ local unique -> unit @@ portable = "caml_borrow_finish"
  external split : ('a : immutable_data).
    (s : 'a loan) @ local unique ->
    (index : {k : int |
      0 <= k && Bigint.compare (Bigint.of_int k) (Model.length (current s)) <=
        0}) ->
    {r : 'a split_frame * 'a loan * 'a loan |
      let refine_ index = index in
      match r with _, left, right ->
        current left === Model.take (Bigint.of_int index) (current s)
        && current right === Model.drop (Bigint.of_int index) (current s)}
    @ unique @@ portable = "caml_borrow_split"
  external recombine : ('a : immutable_data).
    (frame : 'a split_frame) @ unique ->
    {s : 'a loan |
      current s === Model.append (left_final frame) (right_final frame)}
    @ unique @@ portable = "caml_borrow_recombine"
end

let (await_both @ portable) left (right : (unit -> 'b) @ local once) =
  let right_result =
    try right () with exn ->
      let trace = Printexc.get_raw_backtrace () in
      (try ignore (Domain.join left) with _ -> ());
      Printexc.raise_with_backtrace exn trace in
  let left_result = Domain.join left in
  left_result, right_result


module Slice = struct
  type ('a : immutable_data) t = 'a loan
  external current : ('a : immutable_data).
    'a t @ local immutable -> 'a Model.t @ immutable total ghost
    @@ total = "caml_borrow_current"
  external final : ('a : immutable_data).
    'a t @ local immutable -> 'a Model.t @ immutable total ghost
    @@ total = "caml_borrow_final"
  let length : ('a : immutable_data).
    (s : 'a t) @ local immutable ->
    {n : int | 0 <= n
      && Bigint.of_int n === Model.length (current s)} = fun s ->
    let n = Raw.length s in
    refine_ n
  external get : ('a : immutable_data).
    (s : 'a t) @ local immutable ->
    (index : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (current s)) < 0}) ->
    {value : 'a | let refine_ index = index in
      Some value === Model.at (current s) (Bigint.of_int index)} @@
        portable = "caml_borrow_get"
  external set : ('a : immutable_data).
    (s : 'a t) @ local unique ->
    (index : {i : int |
      0 <= i && Bigint.compare (Bigint.of_int i) (Model.length (current s)) <
        0}) ->
    (value : 'a) @ immutable ->
    {r : 'a t | let refine_ index = index in
      current r === Model.set (current s) (Bigint.of_int index) value
      && final r === final s} @ local unique @@ portable = "caml_borrow_set"
  external snapshot : ('a : immutable_data).
    (s : 'a t) @ local immutable ->
    {values : 'a iarray | Model.of_iarray values === current s} @@
      portable = "caml_borrow_snapshot"
  let swap : ('a : immutable_data).
      (s : 'a t) @ local unique ->
      (first : {i : int | 0 <= i
        && Bigint.compare (Bigint.of_int i) (Model.length (current s)) < 0}) ->
      (second : {i : int | 0 <= i
        && Bigint.compare (Bigint.of_int i) (Model.length (current s)) < 0}) ->
      {r : 'a t | let refine_ i = first in let refine_ j = second in
        current r === Model.swap (current s) (Bigint.of_int i) (Bigint.of_int j)
        && final r === final s
        && Model.length (current r) === Model.length (current s)}
      @ local unique = fun s first second ->
    exclave_ (
      let before = ghost_ (current (borrow_ s)) in
      let refine_ i = first in
      let refine_ j = second in
      let bi = ghost_ (Bigint.of_int i) in
      let bj = ghost_ (Bigint.of_int j) in
      let refine_ x = get (borrow_ s) first in
      let refine_ y = get (borrow_ s) second in
      let refine_ s1 = set s first y in
      ghost_ (Model.set_length before bi y);
      let second : {i : int | 0 <= i
        && Bigint.compare (Bigint.of_int i) (Model.length (current s1)) < 0} =
        refine_ j in
      let intermediate = ghost_ (current (borrow_ s1)) in
      let refine_ s2 = set s1 second x in
      ghost_ (Model.set_length intermediate bj x);
      ghost_ (Model.swap_def before bi bj);
      refine_ s2)
  let split_at : ('a : immutable_data) ('r : immutable_data).
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
      @ local unique = fun s index post body ->
    exclave_ (
      let refine_ k = index in
      let raw_index : {k : int | 0 <= k
        && Bigint.compare (Bigint.of_int k) (Model.length (Raw.current s)) <= 0}
          =
        refine_ k in
      let refine_ pieces = Raw.split s raw_index in
      let frame, left, right = pieces in
      let left_end = ghost_ (Raw.final (borrow_ left)) in
      let right_end = ghost_ (Raw.final (borrow_ right)) in
      let left : {left : 'a t | let refine_ k = index in
        current left === Model.take (Bigint.of_int k) (current s)} =
        refine_ left in
      let right : {right : 'a t | let refine_ k = index in
        current right === Model.drop (Bigint.of_int k) (current s)} =
        refine_ right in
      let refine_ value = body left right in
      let refine_ state = Raw.recombine frame in
      ghost_ (Model.append_split left_end right_end);
      ghost_ (Model.append_length left_end right_end);
      let result = {value; state} in
      refine_ result)
  let finish : ('a : immutable_data).
      (s : 'a t) @ local unique ->
      {u : unit | final s === current s} = fun s ->
    let u = Raw.finish s in
    refine_ u
  let split3 : ('a : immutable_data) ('r : immutable_data).
      (s : 'a t) @ local unique ->
      (first : {i : int | 0 <= i
        && Bigint.compare (Bigint.of_int i) (Model.length (current s)) <= 0}) ->
      (past : {j : int | let refine_ i = first in i <= j
        && Bigint.compare (Bigint.of_int j) (Model.length (current s)) <= 0}) ->
      (post : ('r @ immutable total -> 'a Model.t @ immutable ->
        'a Model.t @ immutable -> 'a Model.t @ immutable -> bool @ ghost)) @
          ghost ->
      ((left : {left : 'a t | let refine_ i = first in
          current left === Model.take (Bigint.of_int i) (current s)}) @ local
            unique ->
        (middle : {middle : 'a t | let refine_ i = first in let refine_ j = past
          in
          current middle === Model.sub (current s) (Bigint.of_int i)
            (Bigint.of_int j)})
          @ local unique ->
        (right : {right : 'a t | let refine_ j = past in
          current right === Model.drop (Bigint.of_int j) (current s)}) @ local
            unique ->
        {r : 'r | let refine_ left = left in let refine_ middle = middle in
          let refine_ right = right in post r (final left) (final middle) (final
            right)})
        @ local once ->
      {r : ('r, 'a t) step | let refine_ i = first in let refine_ j = past in
        post r.value (Model.take (Bigint.of_int i) (current r.state))
          (Model.sub (current r.state) (Bigint.of_int i) (Bigint.of_int j))
          (Model.drop (Bigint.of_int j) (current r.state))
        && final r.state === final s
        && Model.length (current r.state) === Model.length (current s)}
      @ local unique = fun s first past post body ->
    exclave_ (
      let before = ghost_ (current (borrow_ s)) in
      let refine_ i = first in
      let refine_ j = past in
      let width = j - i in
      let bi = ghost_ (Bigint.of_int i) in
      let bj = ghost_ (Bigint.of_int j) in
      let bw = ghost_ (Bigint.of_int width) in
      let outer_post = ghost_ (fun (r : 'r @ immutable)
          (left : 'a Model.t @ immutable) (rest : 'a Model.t @ immutable) ->
        post r left (Model.take bw rest) (Model.drop bw rest)) in
      let refine_ result = split_at s first outer_post (fun left_arg rest_arg ->
        let refine_ left = left_arg in
        let refine_ rest = rest_arg in
        let left_end = ghost_ (final (borrow_ left)) in
        ghost_ (Model.cut before bi);
        let bounded : {k : int | 0 <= k
          && Bigint.compare (Bigint.of_int k) (Model.length (current rest)) <=
            0} =
          refine_ width in
        let inner_post = ghost_ (fun (r : 'r @ immutable)
            (middle : 'a Model.t @ immutable) (right : 'a Model.t @ immutable)
              ->
        post r left_end middle right) in
        let refine_ inner = split_at rest bounded inner_post (fun middle_arg
          right_arg ->
          let refine_ middle = middle_arg in
          let refine_ right = right_arg in
          ghost_ (Model.sub_def before bi bj);
          ghost_ (Model.drop_add before bi bw);
          let left : {left : 'a t | let refine_ i = first in
            current left === Model.take (Bigint.of_int i) (current s)} = refine_
              left in
          let middle : {middle : 'a t | let refine_ i = first in let refine_ j =
            past in
            current middle === Model.sub (current s) (Bigint.of_int i)
              (Bigint.of_int j)} =
            refine_ middle in
          let right : {right : 'a t | let refine_ j = past in
            current right === Model.drop (Bigint.of_int j) (current s)} =
            refine_ right in
          let refine_ value = body left middle right in
          refine_ value) in
        let {value; state = rest} = inner in
        finish rest;
        refine_ value) in
      let {value; state} = result in
      let after = ghost_ (current (borrow_ state)) in
      ghost_ (Model.sub_def after bi bj);
      ghost_ (Model.drop_add after bi bw);
      let result = {value; state} in
      refine_ result)

  let with_range : ('a : immutable_data) ('r : immutable_data).
      (s : 'a t) @ local unique ->
      (first : {i : int | 0 <= i
        && Bigint.compare (Bigint.of_int i) (Model.length (current s)) <= 0}) ->
      (past : {j : int | let refine_ i = first in i <= j
        && Bigint.compare (Bigint.of_int j) (Model.length (current s)) <= 0}) ->
      (post : ('r @ immutable total -> 'a Model.t @ immutable -> bool @ ghost))
        @ ghost ->
      ((middle : {middle : 'a t | let refine_ i = first in let refine_ j = past
        in
          current middle === Model.sub (current s) (Bigint.of_int i)
            (Bigint.of_int j)})
          @ local unique ->
        {r : 'r | let refine_ middle = middle in post r (final middle)}) @ local
          once ->
      {r : ('r, 'a t) step | let refine_ i = first in let refine_ j = past in
        post r.value (Model.sub (current r.state) (Bigint.of_int i)
          (Bigint.of_int j))
        && current r.state === Model.append (Model.take (Bigint.of_int i)
          (current s))
          (Model.append (Model.sub (current r.state) (Bigint.of_int i)
            (Bigint.of_int j))
            (Model.drop (Bigint.of_int j) (current s)))
        && final r.state === final s
        && Model.length (current r.state) === Model.length (current s)}
      @ local unique = fun s first past post body ->
    exclave_ (
      let before = ghost_ (current (borrow_ s)) in
      let refine_ i = first in
      let refine_ j = past in
      let bi = ghost_ (Bigint.of_int i) in
      let bj = ghost_ (Bigint.of_int j) in
      let bw = ghost_ (Bigint.sub bj bi) in
      let framed_post = ghost_ (fun (r : 'r @ immutable)
          (left : 'a Model.t @ immutable) (middle : 'a Model.t @ immutable)
          (right : 'a Model.t @ immutable) ->
        post r middle && left === Model.take bi before
          && right === Model.drop bj before) in
      let refine_ result = split3 s first past framed_post (fun left_arg middle
        right_arg ->
        let refine_ left = left_arg in
        let refine_ right = right_arg in
        let refine_ middle_loan = middle in
        let middle : {middle : 'a t | let refine_ i = first in let refine_ j =
          past in
          current middle === Model.sub (current s) (Bigint.of_int i)
            (Bigint.of_int j)} =
          refine_ middle_loan in
        let refine_ value = body middle in
        finish left;
        finish right;
        refine_ value) in
      let {value; state} = result in
      let after = ghost_ (current (borrow_ state)) in
      let rest_after = ghost_ (Model.drop bi after) in
      ghost_ (Model.cut after bi);
      ghost_ (Model.cut rest_after bw);
      ghost_ (Model.drop_add after bi bw);
      ghost_ (Model.sub_def after bi bj);
      let result = {value; state} in
      refine_ result)
  let parallel : ('a : immutable_data).
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
      {u : unit | lp (final left) && rp (final right)} =
      fun spawn left right lp rp lf rf ->
    let left1 = Raw.transfer left in
    let left1 : {s : 'a t | current s === current left
        && final s === final left} = refine_ left1 in
    let right1 : {s : 'a t | current s === current right
        && final s === final right} = refine_ right in
    let l, r =
      if spawn then
        let domain = (Domain.Safe.spawn [@alert "-do_not_spawn_domains"]) (fun
          () -> lf left1) in
        await_both domain (fun () -> rf right1)
      else
        let l = lf left1 in
        let r = rf right1 in
        l, r in
    let refine_ l = l in
    let refine_ r = r in
    let u = () in refine_ u

end

module Owned_array = struct
  type ('a : immutable_data) t = 'a owned
  external contents : ('a : immutable_data).
    'a t @ local immutable -> 'a Model.t @ immutable total ghost
    @@ total = "caml_borrow_contents"
  external of_iarray : ('a : immutable_data).
    (values : 'a iarray) @ immutable ->
    {a : 'a t | contents a === Model.of_iarray values} @ unique @@ portable =
      "caml_borrow_of_iarray"
  external into_iarray : ('a : immutable_data).
    (a : 'a t) @ unique ->
    {values : 'a iarray | Model.of_iarray values === contents a} @@ portable =
      "caml_borrow_into_iarray"
  let with_mut : ('a : immutable_data) ('r : immutable_data).
      (a : 'a t) @ unique ->
      (post : ('r @ immutable total -> 'a Model.t @ immutable -> bool @ ghost))
        @ ghost ->
      ((s : {s : 'a Slice.t | Slice.current s === contents a})
          @ local unique ->
        {r : 'r | let refine_ s = s in post r (Slice.final s)}) @ local once ->
      {r : ('r, 'a t) step | post r.value (contents r.state)
        && Model.length (contents r.state) === Model.length (contents a)}
      @ unique = fun a post body ->
    let frame, loan = Raw.open_ a in
    let loan : {s : 'a Slice.t | Slice.current s === contents a} =
      refine_ loan
    in
    let refine_ value = body loan in
    let state = Raw.restore frame in
    let result = {value; state} in
    refine_ result
end
