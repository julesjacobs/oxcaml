open Pref_ring

let[@def] rec (append @ total) (xs : node list @ immutable)
    (ys : node list @ immutable) =
  ghost_ (match xs with [] -> ys | x :: rest -> x :: append rest ys)

let[@def] rec (reversed @ total) (xs : node list @ immutable) =
  ghost_ (match xs with [] -> [] | x :: rest -> append (reversed rest) [x])

let[@def] (same_view @ total) (before : node option Pref.heap @ immutable)
    (after : node option Pref.heap @ immutable) (n : node @ immutable) =
  ghost_ (present after n = present before n &&
    H.at after n.prev === H.at before n.prev &&
    H.at after n.next === H.at before n.next)

let[@def] (swapped_view @ total) (before : node option Pref.heap @ immutable)
    (after : node option Pref.heap @ immutable) (n : node @ immutable) =
  ghost_ (present after n &&
    H.at after n.prev === Some (value before n.next) &&
    H.at after n.next === Some (value before n.prev))

let[@def] rec (same_views @ total) (before : node option Pref.heap @ immutable)
    (after : node option Pref.heap @ immutable) (ns : node list @ immutable) =
  ghost_ (match ns with [] -> true | n :: rest ->
    same_view before after n && same_views before after rest)

let[@def] rec (swapped_views @ total) (before : node option Pref.heap @ immutable)
    (after : node option Pref.heap @ immutable) (ns : node list @ immutable) =
  ghost_ (match ns with [] -> true | n :: rest ->
    swapped_view before after n && swapped_views before after rest)

let (flip_self @ total) (h : node option Pref.heap @ immutable)
    (n : {n : node | present h n} @ immutable) :
    {u : unit | swapped_view h (flipped h n) n} @ ghost = ghost_ (
  present_def h n;
  value_def h n.prev; value_def h n.next;
  Pref_ring_proofs.flip_observations h n n;
  present_def (flipped h n) n;
  swapped_view_def h (flipped h n) n; ())

let (flip_other @ total) (h : node option Pref.heap @ immutable) (n : node @ immutable)
    (other : {n' : node | apart n n'} @ immutable) :
    {u : unit | same_view h (flipped h n) other} @ ghost = ghost_ (
  apart_def n other;
  Pref_ring_proofs.flip_observations h n other;
  present_def h other; present_def (flipped h n) other;
  same_view_def h (flipped h n) other; ())

let rec (flip_others @ total) : (h : node option Pref.heap) @ immutable ->
    (n : node) @ immutable ->
    (ns : {ns : node list | apart_all n ns}) @ immutable ->
    {u : unit | same_views h (flipped h n) ns} @ ghost =
  fun h n ns -> ghost_ (
    apart_all_def n ns; same_views_def h (flipped h n) ns;
    match ns with [] -> () | other :: rest ->
      flip_other h n other; flip_others h n rest; ())

let rec (flips_frame @ total) : (h : node option Pref.heap) @ immutable ->
    (n : node) @ immutable ->
    (ns : {ns : node list | apart_all n ns}) @ immutable ->
    {u : unit | same_view h (flipped_all h ns) n} @ ghost =
  fun h n ns -> ghost_ (
    apart_all_def n ns; flipped_all_def h ns;
    same_view_def h (flipped_all h ns) n;
    match ns with [] -> () | other :: rest ->
      apart_def n other; apart_def other n;
      flip_other h other n;
      flips_frame (flipped h other) n rest;
      same_view_def h (flipped h other) n;
      same_view_def (flipped h other) (flipped_all (flipped h other) rest) n; ())

let rec (same_owns @ total) : (before : node option Pref.heap) @ immutable ->
    (after : node option Pref.heap) @ immutable -> (ns : node list) @ immutable ->
    {u : unit | if owns before ns && same_views before after ns
      then owns after ns else true} @ ghost =
  fun before after ns -> ghost_ (
    owns_def before ns; owns_def after ns; same_views_def before after ns;
    match ns with [] -> () | n :: rest ->
      same_view_def before after n; same_owns before after rest; ())

let rec (swapped_transport @ total) : (before : node option Pref.heap) @ immutable ->
    (middle : node option Pref.heap) @ immutable -> (after : node option Pref.heap) @ immutable ->
    (ns : node list) @ immutable ->
    {u : unit | if same_views before middle ns && swapped_views middle after ns
      then swapped_views before after ns else true} @ ghost =
  fun before middle after ns -> ghost_ (
    same_views_def before middle ns;
    swapped_views_def middle after ns; swapped_views_def before after ns;
    match ns with [] -> () | n :: rest ->
      same_view_def before middle n;
      value_def before n.prev; value_def middle n.prev;
      value_def before n.next; value_def middle n.next;
      swapped_view_def middle after n; swapped_view_def before after n;
      swapped_transport before middle after rest; ())

let rec (flips_swap @ total) : (h : node option Pref.heap) @ immutable ->
    (ns : {ns : node list | owns h ns && separated ns}) @ immutable ->
    {u : unit | swapped_views h (flipped_all h ns) ns} @ ghost =
  fun h ns -> ghost_ (
    owns_def h ns; separated_def ns; flipped_all_def h ns;
    swapped_views_def h (flipped_all h ns) ns;
    match ns with [] -> () | n :: rest ->
      let middle = flipped h n in
      let after = flipped_all middle rest in
      flip_self h n;
      flip_others h n rest; same_owns h middle rest;
      flips_swap middle rest;
      swapped_transport h middle after rest;
      flips_frame middle n rest;
      swapped_view_def h middle n;
      same_view_def middle after n;
      swapped_view_def h after n; ())

let (append_head @ total) (xs : node list @ immutable)
    (middle : node @ immutable) (stop : node @ immutable) :
    {u : unit | head (append xs [middle]) stop === head xs middle} @ ghost =
  ghost_ (append_def xs [middle]; head_def (append xs [middle]) stop;
    head_def xs middle; head_def [middle] stop; ())

let rec (linked_extend @ total) : (h : node option Pref.heap) @ immutable ->
    (previous : node) @ immutable -> (xs : node list) @ immutable ->
    (middle : node) @ immutable -> (stop : node) @ immutable ->
    {u : unit | if linked h previous xs middle && present h middle &&
      not middle.sentinel && H.at h middle.next === Some (Some stop) &&
      H.at h stop.prev === Some (Some middle)
      then linked h previous (append xs [middle]) stop else true} @ ghost =
  fun h previous xs middle stop -> ghost_ (
    linked_def h previous xs middle; append_def xs [middle];
    linked_def h previous (append xs [middle]) stop;
    match xs with
    | [] -> head_def [] stop; linked_def h middle [] stop; ()
    | n :: rest ->
      append_head rest middle stop;
      linked_extend h n rest middle stop; ())

let rec (linked_reverse @ total) : (before : node option Pref.heap) @ immutable ->
    (after : node option Pref.heap) @ immutable -> (previous : node) @ immutable ->
    (ns : node list) @ immutable -> (stop : node) @ immutable ->
    {u : unit | if linked before previous ns stop &&
      H.at before previous.next === Some (Some (head ns stop)) &&
      swapped_view before after previous && swapped_view before after stop &&
      swapped_views before after ns
      then linked after stop (reversed ns) previous &&
        H.at after stop.next === Some (Some (head (reversed ns) previous))
      else true} @ ghost =
  fun before after previous ns stop -> ghost_ (
    linked_def before previous ns stop; reversed_def ns;
    swapped_view_def before after previous;
    swapped_view_def before after stop;
    swapped_views_def before after ns;
    value_def before previous.next; value_def before stop.prev;
    head_def ns stop;
    match ns with
    | [] ->
      linked_def after stop [] previous; head_def [] previous; ()
    | n :: rest ->
      swapped_view_def before after n;
      value_def before n.prev; value_def before n.next;
      linked_reverse before after n rest stop;
      linked_extend after stop (reversed rest) n previous;
      append_head (reversed rest) n previous; ())

let rec (linked_access @ total) : (h : node option Pref.heap) @ immutable ->
    (previous : node) @ immutable -> (ns : node list) @ immutable ->
    (stop : node) @ immutable ->
    {u : unit | if linked h previous ns stop then owns h ns && path h false ns stop
      else true} @ ghost =
  fun h previous ns stop -> ghost_ (
    linked_def h previous ns stop; owns_def h ns; path_def h false ns stop;
    match ns with [] -> () | n :: rest ->
      field_def false n; linked_access h n rest stop; ())

let rec (apart_append @ total) : (n : node) @ immutable ->
    (xs : node list) @ immutable -> (ys : node list) @ immutable ->
    {u : unit | apart_all n (append xs ys) =
      (apart_all n xs && apart_all n ys)} @ ghost =
  fun n xs ys -> ghost_ (
    append_def xs ys; apart_all_def n xs; apart_all_def n (append xs ys);
    match xs with [] -> () | _ :: rest -> apart_append n rest ys; ())

let rec (apart_reverse @ total) : (n : node) @ immutable ->
    (xs : node list) @ immutable ->
    {u : unit | apart_all n (reversed xs) = apart_all n xs} @ ghost =
  fun n xs -> ghost_ (
    reversed_def xs; apart_all_def n xs;
    match xs with [] -> () | x :: rest ->
      apart_append n (reversed rest) [x]; apart_reverse n rest;
      apart_all_def n [x]; apart_all_def n []; ())

let rec (separated_snoc @ total) : (xs : node list) @ immutable ->
    (n : node) @ immutable ->
    {u : unit | if separated xs && apart_all n xs && not (n.prev === n.next)
      then separated (append xs [n]) else true} @ ghost =
  fun xs n -> ghost_ (
    separated_def xs; apart_all_def n xs; append_def xs [n];
    separated_def (append xs [n]);
    match xs with
    | [] -> apart_all_def n []; separated_def []; ()
    | x :: rest ->
      apart_def n x; apart_def x n;
      apart_append x rest [n]; apart_all_def x [n]; apart_all_def x [];
      separated_snoc rest n; ())

let rec (separated_reverse @ total) : (ns : node list) @ immutable ->
    {u : unit | not (separated ns) || separated (reversed ns)} @ ghost =
  fun ns -> ghost_ (
    reversed_def ns; separated_def ns;
    match ns with [] -> () | n :: rest ->
      separated_reverse rest; apart_reverse n rest;
      separated_snoc (reversed rest) n; ())

let (reverse_law @ total) (h : node option Pref.heap @ immutable)
    (sentinel : node @ immutable)
    (ns : {ns : node list | ring h sentinel ns && separated (sentinel :: ns)} @ immutable) :
    {u : unit | let after = flipped_all h (sentinel :: ns) in
      ring after sentinel (reversed ns) && separated (sentinel :: reversed ns)
      && owns h (sentinel :: ns) && path h false ns sentinel} @ ghost = ghost_ (
  ring_def h sentinel ns; linked_access h sentinel ns sentinel;
  owns_def h (sentinel :: ns);
  flips_swap h (sentinel :: ns);
  let after = flipped_all h (sentinel :: ns) in
  swapped_views_def h after (sentinel :: ns);
  swapped_view_def h after sentinel;
  linked_reverse h after sentinel ns sentinel;
  value_def h sentinel.prev;
  ring_def after sentinel (reversed ns);
  separated_def (sentinel :: ns);
  separated_reverse ns; apart_reverse sentinel ns;
  separated_def (sentinel :: reversed ns); ())

let reverse : (sentinel : node) @ immutable ->
    (ns : node list) @ immutable ghost ->
    (t : {t : node option Pref.token | ring (Pref.own t) sentinel ns &&
      separated (sentinel :: ns)}) @ unique ->
    {r : node option Pref.token | Pref.own r === flipped_all (Pref.own t) (sentinel :: ns) &&
      ring (Pref.own r) sentinel (reversed ns) && separated (sentinel :: reversed ns)}
      @ unique = fun sentinel ns t ->
  let before = ghost_ (Pref.own (borrow_ t)) in
  ghost_ (reverse_law before sentinel ns; ring_def before sentinel ns;
    field_def false sentinel);
  let nodes = traverse false sentinel ns (borrow_ t) in
  reverse_nodes (sentinel :: nodes) t

type built = { sentinel : node @@ aliased; nodes : node list @@ aliased ghost;
  state : node option Pref.token }

module Owned = struct
  type payload = #{sentinel : node @@ global; nodes : node list @@ global ghost;
    state : node option Pref.token}
  type t = #{owned : {b : payload | ring (Pref.own b.#state) b.#sentinel b.#nodes &&
    separated (b.#sentinel :: b.#nodes)}}

  let[@def] model (state : t @ local immutable total ghost) = ghost_ state.#owned.#nodes
  let[@def] sentinel (state : t @ local immutable total ghost) = ghost_ state.#owned.#sentinel
  let[@def] heap (state : t @ local immutable total ghost) = ghost_ (Pref.own state.#owned.#state)

  let adopt : (b : {b : built | ring (Pref.own b.state) b.sentinel b.nodes &&
      separated (b.sentinel :: b.nodes)}) @ unique ->
      {state : t | model state === b.nodes && sentinel state === b.sentinel &&
        heap state === Pref.own b.state} @ unique = fun b ->
    let owned = #{sentinel = b.sentinel; nodes = b.nodes; state = b.state} in
    let state : t = #{owned} in
    ghost_ (model_def (borrow_ state); sentinel_def (borrow_ state); heap_def (borrow_ state));
    state

  let release : (state : t) @ unique ->
      {b : built | ring (Pref.own b.state) b.sentinel b.nodes &&
        separated (b.sentinel :: b.nodes) && b.nodes === model state &&
        b.sentinel === sentinel state && Pref.own b.state === heap state} @ unique =
    fun state ->
    ghost_ (model_def (borrow_ state); sentinel_def (borrow_ state); heap_def (borrow_ state));
    let b = state.#owned in
    {sentinel = b.#sentinel; nodes = b.#nodes; state = b.#state}

  let empty () : {state : t | model state === []} @ unique =
    let made = make_node true 0 (Pref.empty ()) in
    let sentinel = made.node in
    let h = ghost_ (Pref.own (borrow_ made.state)) in
    ghost_ (ring_def h sentinel []; linked_def h sentinel [] sentinel;
      head_def [] sentinel; separated_def [sentinel];
      present_def h sentinel; apart_all_def sentinel []; separated_def []);
    adopt {sentinel; nodes = ghost_ []; state = made.state}

  let reverse : (state : t) @ unique ->
      {next : t | model next === reversed (model state) &&
        sentinel next === sentinel state &&
        heap next === flipped_all (heap state) (sentinel state :: model state)} @ unique =
    fun state ->
    ghost_ (model_def (borrow_ state); sentinel_def (borrow_ state); heap_def (borrow_ state));
    let b = state.#owned in
    let nodes = ghost_ (reversed b.#nodes) in
    let after = reverse b.#sentinel b.#nodes b.#state in
    let owned = #{sentinel = b.#sentinel; nodes; state = after} in
    let next : t = #{owned} in
    ghost_ (model_def (borrow_ next); sentinel_def (borrow_ next); heap_def (borrow_ next));
    next

  let observe : (state : t) @ local read total forkable unyielding ->
      {nodes : node list | nodes === model state} @ immutable = fun state ->
    ghost_ (model_def (borrow_ state));
    let b = state.#owned in
    let before = ghost_ (Pref.own (borrow_ b.#state)) in
    ghost_ (ring_def before b.#sentinel b.#nodes;
      linked_access before b.#sentinel b.#nodes b.#sentinel;
      field_def false b.#sentinel);
    traverse false b.#sentinel b.#nodes (borrow_ b.#state)
end
