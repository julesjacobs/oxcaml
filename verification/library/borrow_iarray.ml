open! Stdlib

type ('value, 'state) step =
  { value : 'value @@ global; state : 'state }

type ('a : immutable_data, 'model : any) owned_handle :
  value mod total contended
type ('a : immutable_data, 'model : any) loan_handle :
  value mod total contended
type ('a : immutable_data, 'model : any) frame_handle :
  value mod total contended

type ('a : immutable_data, 'model : any) split_frame_handle :
  value mod total contended

type ('a : immutable_data) split_frame =
  ('a, 'a iarray) split_frame_handle

type ('a : immutable_data) owned = ('a, 'a iarray) owned_handle
type ('a : immutable_data) loan = ('a, 'a iarray) loan_handle
type ('a : immutable_data) frame = ('a, 'a iarray) frame_handle

module Raw = struct
  external current : ('a : immutable_data).
    'a loan @ local immutable -> 'a iarray @ total immutable ghost
    @@ total = "caml_borrow_current"
  external final : ('a : immutable_data).
    'a loan @ local immutable -> 'a iarray @ total immutable ghost
    @@ total = "caml_borrow_final"
  external left_final : ('a : immutable_data).
    'a split_frame @ local immutable -> 'a iarray @ total immutable ghost
    @@ total = "caml_borrow_frame_left"
  external right_final : ('a : immutable_data).
    'a split_frame @ local immutable -> 'a iarray @ total immutable ghost
    @@ total = "caml_borrow_frame_right"
  external split : ('a : immutable_data).
    (s : 'a loan) @ local unique ->
    (index : {k : int | 0 <= k && k <= Iarray.length (current s)}) ->
    {r : 'a split_frame * 'a loan * 'a loan | let refine_ k = index in
      match r with _, left, right ->
        current left === Vox_iarray.slice (current s) 0 k
        && current right === Vox_iarray.slice (current s) k
          (Iarray.length (current s))} @ unique
    @@ portable total = "caml_borrow_split"
  external recombine : ('a : immutable_data).
    (frame : 'a split_frame) @ unique ->
    {s : 'a loan |
      Vox_iarray.slice (current s) 0 (Iarray.length (left_final frame)) ===
        left_final frame
      && Vox_iarray.slice (current s) (Iarray.length (left_final frame))
        (Iarray.length (current s)) === right_final frame}
    @ unique @@ portable total = "caml_borrow_recombine"
  external open_ : ('a : immutable_data).
    'a owned @ unique -> ('a frame * 'a loan) @ unique
    @@ portable total = "caml_borrow_open"
  external restore : ('a : immutable_data).
    'a frame @ unique -> 'a owned @ unique
    @@ portable total = "caml_borrow_restore"
  external transfer : ('a : immutable_data).
    'a loan @ local unique -> 'a loan @ unique
    @@ portable total = "caml_borrow_transfer"
  external finish : ('a : immutable_data).
    'a loan @ local unique -> unit @@ portable total = "caml_borrow_finish"
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
    'a t @ local immutable -> 'a iarray @ total immutable ghost
    @@ total = "caml_borrow_current"
  external final : ('a : immutable_data).
    'a t @ local immutable -> 'a iarray @ total immutable ghost
    @@ total = "caml_borrow_final"
  external length : ('a : immutable_data).
    (s : 'a t) @ local immutable ->
    {n : int | n = Iarray.length (current s)}
    @@ portable total = "caml_borrow_length"
  external get : ('a : immutable_data).
    (s : 'a t) @ local immutable ->
    (index : {i : int | 0 <= i && i < Iarray.length (current s)}) ->
    {value : 'a | let refine_ i = index in
      Some value === Vox_iarray.at (current s) i}
    @@ portable total = "caml_borrow_get"
  external set : ('a : immutable_data).
    (s : 'a t) @ local unique ->
    (index : {i : int | 0 <= i && i < Iarray.length (current s)}) ->
    (value : 'a) @ immutable ->
    {r : 'a t | let refine_ i = index in
      current r === Vox_iarray.updated (current s) i value
      && final r === final s}
    @ local unique @@ portable total = "caml_borrow_set"
  external snapshot : ('a : immutable_data).
    (s : 'a t) @ local immutable ->
    {values : 'a iarray | values === current s}
    @@ portable total = "caml_borrow_snapshot"
  let (swap @ total) : ('a : immutable_data).
      (s : 'a t) @ local unique ->
      (first : {i : int | 0 <= i && i < Iarray.length (current s)}) ->
      (second : {i : int | 0 <= i && i < Iarray.length (current s)}) ->
      {r : 'a t | let refine_ i = first in let refine_ j = second in
        current r === Vox_iarray.swap (current s) i j && final r === final s}
      @ local unique = fun s first second ->
    exclave_ (
      let before = ghost_ (current (borrow_ s)) in
      let refine_ i = first in
      let refine_ j = second in
      let refine_ x = get (borrow_ s) first in
      let refine_ y = get (borrow_ s) second in
      let refine_ middle = set s first y in
      ghost_ (Vox_iarray.updated_length before i y);
      let second : {k : int | 0 <= k && k < Iarray.length (current middle)} =
        refine_ j in
      let refine_ result = set middle second x in
      ghost_ (Vox_iarray.swap_def before i j);
      refine_ result)
  let (split_at @ total) : ('a : immutable_data) ('r : immutable_data).
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
      @ local unique = fun s index post body ->
    exclave_ (
      let refine_ k = index in
      let raw_index : {k : int | 0 <= k && k <= Iarray.length (Raw.current s)} =
        refine_ k in
      let refine_ pieces = Raw.split s raw_index in
      let frame, left, right = pieces in
      let _left_end = ghost_ (Raw.final (borrow_ left)) in
      let _right_end = ghost_ (Raw.final (borrow_ right)) in
      let left : {left : 'a t | let refine_ k = index in
        current left === Vox_iarray.slice (current s) 0 k} = refine_ left in
      let right : {right : 'a t | let refine_ k = index in
        current right === Vox_iarray.slice (current s) k
          (Iarray.length (current s))} = refine_ right in
      let refine_ value = body left right in
      let refine_ state = Raw.recombine frame in
      let result = {value; state} in
      refine_ result)
  let (finish @ total) : ('a : immutable_data).
      (s : 'a t) @ local unique -> {u : unit | final s === current s} =
    fun s ->
      let u = Raw.finish s in
      refine_ u
  let (split3 @ total) : ('a : immutable_data) ('r : immutable_data).
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
      @ local unique = fun s first past post body ->
    exclave_ (
      let before = ghost_ (current (borrow_ s)) in
      let refine_ size = length (borrow_ s) in
      let refine_ i = first in
      let refine_ j = past in
      let width = j - i in
      let rest_size = size - i in
      let zero = 0 in
      let outer_post = ghost_ (fun (value : 'r @ immutable)
          (left : 'a iarray @ total immutable) (rest : 'a iarray @ total
            immutable) ->
        post value left (Vox_iarray.slice rest zero width)
          (Vox_iarray.slice rest width (Iarray.length rest))) in
      let refine_ result = split_at s first outer_post (fun left_arg rest_arg ->
        let refine_ left = left_arg in
        let refine_ rest = rest_arg in
        let left_end = ghost_ (final (borrow_ left)) in
        ghost_ (Vox_iarray.slice_length before i size);
        let bounded : {k : int | 0 <= k && k <= Iarray.length (current rest)} =
          refine_ width in
        let inner_post = ghost_ (fun (value : 'r @ immutable)
            (middle : 'a iarray @ total immutable) (right : 'a iarray @ total
              immutable) ->
          post value left_end middle right) in
        let refine_ inner =
          split_at rest bounded inner_post (fun mid_arg right_arg ->
          let refine_ middle = mid_arg in
          let refine_ right = right_arg in
          ghost_ (Vox_iarray.slice_slice before i size zero width);
          ghost_ (Vox_iarray.slice_slice before i size width rest_size);
          let left : {left : 'a t | let refine_ i = first in
            current left === Vox_iarray.slice (current s) 0 i} = refine_ left in
          let middle : {middle : 'a t | let refine_ i =
            first in let refine_ j = past in
            current middle === Vox_iarray.slice (current s) i j} = refine_
              middle in
          let right : {right : 'a t | let refine_ j = past in
            current right === Vox_iarray.slice (current s) j
              (Iarray.length (current s))} = refine_ right in
          let refine_ value = body left middle right in
          refine_ value) in
        let {value; state = rest} = inner in
        finish rest;
        refine_ value) in
      let {value; state} = result in
      let after = ghost_ (current (borrow_ state)) in
      ghost_ (Vox_iarray.slice_length after i size);
      ghost_ (Vox_iarray.slice_slice after i size zero width);
      ghost_ (Vox_iarray.slice_slice after i size width rest_size);
      let result = {value; state} in
      refine_ result)
  let parallel : ('a : immutable_data).
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
    'a t @ local immutable -> 'a iarray @ total immutable ghost
    @@ total = "caml_borrow_contents"
  external of_iarray : ('a : immutable_data).
    (values : 'a iarray) @ immutable ->
    {a : 'a t | contents a === values} @ unique
    @@ portable total = "caml_borrow_of_iarray"
  external into_iarray : ('a : immutable_data).
    (a : 'a t) @ unique -> {values : 'a iarray | values === contents a}
    @@ portable total = "caml_borrow_into_iarray"
  let (with_mut @ total) : ('a : immutable_data) ('r : immutable_data).
      (a : 'a t) @ unique ->
      (post : ('r @ immutable total -> 'a iarray @ total immutable -> bool @
        ghost))
        @ ghost ->
      ((s : {s : 'a Slice.t | Slice.current s === contents a})
          @ local unique ->
        {r : 'r | let refine_ s = s in post r (Slice.final s)}) @ local once ->
      {r : ('r, 'a t) step | post r.value (contents r.state)
        && Iarray.length (contents r.state) = Iarray.length (contents a)}
      @ unique = fun a post body ->
    let frame, loan = Raw.open_ a in
    let loan : {s : 'a Slice.t | Slice.current s === contents a} =
      refine_ loan in
    let refine_ value = body loan in
    let state = Raw.restore frame in
    let result = {value; state} in
    refine_ result
end
