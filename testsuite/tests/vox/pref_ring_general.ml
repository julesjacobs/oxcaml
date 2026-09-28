open Pref_ring

let[@def] rec (append @ total) (xs : node list @ immutable)
    (ys : node list @ immutable) =
  ghost_ (match xs with [] -> ys | x :: rest -> x :: append rest ys)

let[@def] rec (reversed @ total) (xs : node list @ immutable) =
  ghost_ (match xs with [] -> [] | x :: rest -> append (reversed rest) [x])

let[@def] rec (last @ total) (fallback : node @ immutable) (ns : node list @ immutable) =
  ghost_ (match ns with [] -> fallback | n :: rest -> last n rest)

module Proofs = struct
  let rec (apart_append @ total) : (n : node) @ immutable ->
      (xs : node list) @ immutable -> (ys : node list) @ immutable ->
      {u : unit | apart_all n (append xs ys) =
        (apart_all n xs && apart_all n ys)} @ ghost =
    fun n xs ys -> ghost_ (
      append_def xs ys; apart_all_def n xs; apart_all_def n (append xs ys);
      match xs with [] -> () | _ :: rest -> apart_append n rest ys; ())

  let[@def] rec (member @ total) (n : node @ immutable) (ns : node list @ immutable) =
    ghost_ (match ns with [] -> false | x :: rest -> n === x || member n rest)

  let[@def] rec (apart_lists @ total) (xs : node list @ immutable) (ys : node list @ immutable) =
    ghost_ (match xs with [] -> true | x :: rest -> apart_all x ys && apart_lists rest ys)

  let rec (last_append @ total) : (fallback : node) @ immutable ->
      (xs : node list) @ immutable -> (ys : node list) @ immutable ->
      {u : unit | last fallback (append xs ys) === last (last fallback xs) ys} @ ghost =
    fun fallback xs ys -> ghost_ (
      append_def xs ys; last_def fallback xs; last_def fallback (append xs ys);
      match xs with [] -> () | n :: rest -> last_append n rest ys; ())

  let rec (last_member @ total) : (fallback : node) @ immutable ->
      (ns : node list) @ immutable ->
      {u : unit | match ns with [] -> last fallback ns === fallback
        | _ :: _ -> member (last fallback ns) ns} @ ghost =
    fun fallback ns -> ghost_ (
      last_def fallback ns;
      match ns with [] -> () | n :: rest ->
        last_member n rest; member_def (last fallback ns) ns; ())

  let rec (apart_member @ total) : (n : node) @ immutable ->
      (xs : node list) @ immutable -> (x : node) @ immutable ->
      {u : unit | if apart_all n xs && member x xs then apart n x else true} @ ghost =
    fun n xs x -> ghost_ (
      apart_all_def n xs; member_def x xs;
      match xs with [] -> () | _ :: rest -> apart_member n rest x; ())

  let rec (apart_lists_member @ total) : (xs : node list) @ immutable ->
      (ys : node list) @ immutable -> (x : node) @ immutable ->
      {u : unit | if apart_lists xs ys && member x xs then apart_all x ys else true} @ ghost =
    fun xs ys x -> ghost_ (
      apart_lists_def xs ys; member_def x xs;
      match xs with [] -> () | _ :: rest -> apart_lists_member rest ys x; ())

  let rec (separated_append @ total) : (xs : node list) @ immutable ->
      (ys : node list) @ immutable ->
      {u : unit | separated (append xs ys) =
        (separated xs && separated ys && apart_lists xs ys)} @ ghost =
    fun xs ys -> ghost_ (
      append_def xs ys; separated_def xs; separated_def (append xs ys);
      apart_lists_def xs ys;
      match xs with [] -> () | n :: rest ->
        apart_append n rest ys;
        separated_append rest ys; ())

  let[@def] rec (chain @ total) (h : node option Pref.heap @ immutable) (ns : node list @ immutable) =
    ghost_ (match ns with [] -> true | n :: rest -> present h n &&
      (match rest with [] -> true | next :: _ ->
        H.at h n.next === Some (Some next) && H.at h next.prev === Some (Some n)) &&
      chain h rest)

  let[@def] rec (ordinary @ total) (ns : node list @ immutable) =
    ghost_ (match ns with [] -> true | n :: rest -> not n.sentinel && ordinary rest)

  let rec (chain_split @ total) : (h : node option Pref.heap) @ immutable ->
      (xs : node list) @ immutable -> (ys : node list) @ immutable ->
      {u : unit | not (chain h (append xs ys)) || (chain h xs && chain h ys)} @ ghost =
    fun h xs ys -> ghost_ (
      append_def xs ys; chain_def h xs; chain_def h (append xs ys);
      match xs with [] -> () | _ :: rest ->
        append_def rest ys; chain_split h rest ys; ())

  let rec (chain_join @ total) : (h : node option Pref.heap) @ immutable ->
      (fallback : node) @ immutable -> (xs : node list) @ immutable ->
      (ys : node list) @ immutable ->
      {u : unit | if chain h xs && chain h ys &&
        (match xs, ys with [], _ | _, [] -> true | _, y :: _ ->
          H.at h (last fallback xs).next === Some (Some y) &&
          H.at h y.prev === Some (Some (last fallback xs)))
        then chain h (append xs ys) else true} @ ghost =
    fun h fallback xs ys -> ghost_ (
      append_def xs ys; chain_def h xs; chain_def h (append xs ys);
      last_def fallback xs;
      match xs with [] -> () | n :: rest ->
        append_def rest ys; last_def n rest;
        chain_def h ys;
        chain_join h n rest ys; ())

  let rec (linked_chain @ total) : (h : node option Pref.heap) @ immutable ->
      (previous : node) @ immutable -> (ns : node list) @ immutable ->
      (stop : node) @ immutable ->
      {u : unit | if linked h previous ns stop && present h previous &&
        H.at h previous.next === Some (Some (head ns stop)) then
        chain h (previous :: ns) && ordinary ns &&
        H.at h (last previous ns).next === Some (Some stop) &&
        H.at h stop.prev === Some (Some (last previous ns)) else true} @ ghost =
    fun h previous ns stop -> ghost_ (
      linked_def h previous ns stop; head_def ns stop; last_def previous ns;
      chain_def h (previous :: ns); ordinary_def ns;
      match ns with [] -> chain_def h []; () | n :: rest ->
        linked_chain h n rest stop; ())

  let rec (chain_linked @ total) : (h : node option Pref.heap) @ immutable ->
      (previous : node) @ immutable -> (ns : node list) @ immutable ->
      (stop : node) @ immutable ->
      {u : unit | if chain h (previous :: ns) && ordinary ns &&
        H.at h (last previous ns).next === Some (Some stop) &&
        H.at h stop.prev === Some (Some (last previous ns)) then
        linked h previous ns stop && H.at h previous.next === Some (Some (head ns stop))
        else true} @ ghost =
    fun h previous ns stop -> ghost_ (
      chain_def h (previous :: ns); ordinary_def ns;
      linked_def h previous ns stop; head_def ns stop; last_def previous ns;
      match ns with [] -> () | n :: rest ->
        chain_def h (n :: rest); chain_linked h n rest stop; ())

  let rec (separated_pair @ total) : (ns : node list) @ immutable ->
      (x : node) @ immutable -> (y : node) @ immutable ->
      {u : unit | if separated ns && member x ns && member y ns then
        x === y || apart x y else true} @ ghost =
    fun ns x y -> ghost_ (
      separated_def ns; member_def x ns; member_def y ns;
      match ns with [] -> () | n :: rest ->
        apart_member n rest x; apart_member n rest y;
        apart_def n x; apart_def x n;
        separated_pair rest x y; ())

  let rec (member_present @ total) : (h : node option Pref.heap) @ immutable ->
      (ns : node list) @ immutable -> (x : node) @ immutable ->
      {u : unit | if chain h ns && member x ns then present h x else true} @ ghost =
    fun h ns x -> ghost_ (
      chain_def h ns; member_def x ns;
      match ns with [] -> () | _ :: rest -> member_present h rest x; ())

  let[@def] rec (safe_cell @ total) (ns : node list @ immutable)
      (p : node option Pref.t @ immutable) =
    ghost_ (match ns with [] -> true | n :: rest ->
      (match rest with [] -> true | next :: _ ->
        not (p === n.next) && not (p === next.prev)) && safe_cell rest p)

  let rec (safe_external @ total) : (ns : node list) @ immutable ->
      (n : node) @ immutable ->
      {u : unit | if apart_all n ns then safe_cell ns n.prev && safe_cell ns n.next
        else true} @ ghost =
    fun ns n -> ghost_ (
      apart_all_def n ns; safe_cell_def ns n.prev; safe_cell_def ns n.next;
      match ns with [] -> () | x :: rest ->
        apart_def n x; apart_all_def n rest;
        (match rest with [] -> () | next :: _ -> apart_def n next; ());
        safe_external rest n; ())

  let (safe_head @ total) (n : node @ immutable) (rest : node list @ immutable) :
      {u : unit | if separated (n :: rest) then safe_cell (n :: rest) n.prev else true}
        @ ghost = ghost_ (
    separated_def (n :: rest); safe_cell_def (n :: rest) n.prev;
    apart_all_def n rest;
    (match rest with [] -> () | next :: _ -> apart_def n next; ());
    safe_external rest n; ())

  let rec (safe_last @ total) : (fallback : node) @ immutable ->
      (ns : node list) @ immutable ->
      {u : unit | not (separated ns) || safe_cell ns (last fallback ns).next} @ ghost =
    fun fallback ns -> ghost_ (
      separated_def ns; last_def fallback ns; safe_cell_def ns (last fallback ns).next;
      match ns with [] -> () | n :: rest ->
        safe_last n rest; last_member n rest;
        apart_member n rest (last n rest); apart_def n (last n rest);
        (match rest with [] -> () | next :: _ ->
          member_def next rest; separated_pair rest next (last n rest);
          separated_def rest; apart_def next (last n rest); ()); ())

  let rec (chain_put @ total) : (h : node option Pref.heap) @ immutable ->
      (ns : node list) @ immutable -> (p : node option Pref.t) @ immutable ->
      (v : node option) @ immutable ->
      {u : unit | if chain h ns && safe_cell ns p then chain (H.put h p v) ns
        else true} @ ghost =
    fun h ns p v -> ghost_ (
      chain_def h ns; safe_cell_def ns p; chain_def (H.put h p v) ns;
      match ns with [] -> () | n :: rest ->
        Pref_ring_proofs.put_observations h p v n;
        present_def h n; present_def (H.put h p v) n;
        (match rest with [] -> () | next :: _ ->
          Pref_ring_proofs.put_observations h p v next; ());
        chain_put h rest p v; ())

  let rec (apart_lists_right @ total) : (xs : node list) @ immutable ->
      (ys : node list) @ immutable -> (n : node) @ immutable ->
      {u : unit | if apart_lists xs ys && member n ys then apart_all n xs else true}
        @ ghost = fun xs ys n -> ghost_ (
      apart_lists_def xs ys; apart_all_def n xs;
      match xs with [] -> () | x :: rest ->
        apart_member x ys n; apart_def x n; apart_def n x;
        apart_lists_right rest ys n; ())

  let rec (apart_lists_append_right @ total) : (xs : node list) @ immutable ->
      (ys : node list) @ immutable -> (zs : node list) @ immutable ->
      {u : unit | apart_lists xs (append ys zs) =
        (apart_lists xs ys && apart_lists xs zs)} @ ghost =
    fun xs ys zs -> ghost_ (
      apart_lists_def xs (append ys zs); apart_lists_def xs ys; apart_lists_def xs zs;
      match xs with [] -> () | x :: rest ->
        apart_append x ys zs; apart_lists_append_right rest ys zs; ())

  let rec (apart_lists_append_left @ total) : (xs : node list) @ immutable ->
      (ys : node list) @ immutable -> (zs : node list) @ immutable ->
      {u : unit | apart_lists (append xs ys) zs =
        (apart_lists xs zs && apart_lists ys zs)} @ ghost =
    fun xs ys zs -> ghost_ (
      append_def xs ys; apart_lists_def (append xs ys) zs; apart_lists_def xs zs;
      match xs with [] -> () | _ :: rest -> apart_lists_append_left rest ys zs; ())

  let rec (apart_lists_symmetric @ total) : (xs : node list) @ immutable ->
      (ys : node list) @ immutable ->
      {u : unit | not (apart_lists xs ys) || apart_lists ys xs} @ ghost =
    fun xs ys -> ghost_ (
      apart_lists_def ys xs;
      match ys with [] -> () | y :: rest ->
        member_def y ys; apart_lists_right xs ys y;
        append_def [y] rest; append_def [] rest; apart_lists_append_right xs [y] rest;
        apart_lists_symmetric xs rest; ())

  let rec (ordinary_append @ total) : (xs : node list) @ immutable ->
      (ys : node list) @ immutable ->
      {u : unit | ordinary (append xs ys) = (ordinary xs && ordinary ys)} @ ghost =
    fun xs ys -> ghost_ (
      append_def xs ys; ordinary_def xs; ordinary_def (append xs ys);
      match xs with [] -> () | _ :: rest -> ordinary_append rest ys; ())

  let rec (last_nonempty @ total) : (a : node) @ immutable ->
      (b : node) @ immutable -> (ns : node list) @ immutable ->
      {u : unit | match ns with [] -> true | _ :: _ -> last a ns === last b ns}
        @ ghost = fun a b ns -> ghost_ (last_def a ns; last_def b ns; ())
end
open Proofs

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

let rec (owns_fresh @ total) : (h : node option Pref.heap) @ immutable ->
    (n : node) @ immutable -> (ns : node list) @ immutable ->
    {u : unit | if owns h ns && not (H.mem h n.prev) && not (H.mem h n.next)
      then apart_all n ns else true} @ ghost =
  fun h n ns -> ghost_ (
    owns_def h ns; apart_all_def n ns;
    match ns with [] -> () | x :: rest ->
      present_def h x; apart_def n x; owns_fresh h n rest; ())

let rec (chain_junction @ total) : (h : node option Pref.heap) @ immutable ->
    (fallback : node) @ immutable -> (xs : node list) @ immutable ->
    (y : node) @ immutable -> (ys : node list) @ immutable ->
    {u : unit | match xs with [] -> true | _ :: _ ->
      if chain h (append xs (y :: ys)) then
        H.at h (last fallback xs).next === Some (Some y)
        && H.at h y.prev === Some (Some (last fallback xs))
      else true} @ ghost =
  fun h fallback xs y ys -> ghost_ (
    append_def xs (y :: ys); last_def fallback xs;
    match xs with [] -> () | x :: rest ->
      chain_def h (append xs (y :: ys));
      append_def rest (y :: ys); last_def x rest;
      chain_junction h x rest y ys; ())

let (inserted_view @ total) (h : node option Pref.heap @ immutable)
    (left : node @ immutable) (n : node @ immutable) (right : node @ immutable)
    (m : {m : node | present h m && not (H.mem h n.prev) && not (H.mem h n.next)
      && (m === left || apart m left) && (m === right || apart m right)} @ immutable) :
    {u : unit | let after = inserted h left n right in
      present after m &&
      H.at after m.prev === (if m === right then Some (Some n) else H.at h m.prev) &&
      H.at after m.next === (if m === left then Some (Some n) else H.at h m.next)}
    @ ghost = ghost_ (
  let after = inserted h left n right in
  present_def h m; present_def after m; apart_def m left; apart_def m right;
  inserted_def h left n right; connected_def h left n;
  connected_def (connected h left n) n right;
  Pref_ring_proofs.put_observations h left.next (Some n) m;
  let h = H.put h left.next (Some n) in
  let h = H.put h n.prev (Some left) in
  let h = H.put h n.next (Some right) in
  Pref_ring_proofs.put_observations h right.prev (Some n) m; ())

let (removed_view @ total) (h : node option Pref.heap @ immutable)
    (left : node @ immutable) (n : node @ immutable) (right : node @ immutable)
    (m : {m : node | present h m && apart m n
      && (m === left || apart m left) && (m === right || apart m right)} @ immutable) :
    {u : unit | let after = removed h left n right in
      present after m &&
      H.at after m.prev === (if m === right then Some (Some left) else H.at h m.prev) &&
      H.at after m.next === (if m === left then Some (Some right) else H.at h m.next)}
    @ ghost = ghost_ (
  let after = removed h left n right in
  present_def h m; present_def after m;
  apart_def m left; apart_def m right; apart_def m n;
  removed_def h left n right; connected_def h left right;
  connected_def (connected h left right) n n;
  Pref_ring_proofs.put_observations h left.next (Some right) m;
  let h = H.put h left.next (Some right) in
  Pref_ring_proofs.put_observations h right.prev (Some left) m;
  let h = H.put h right.prev (Some left) in
  Pref_ring_proofs.put_observations h n.next (Some n) m;
  let h = H.put h n.next (Some n) in
  Pref_ring_proofs.put_observations h n.prev (Some n) m; ())

let (insert_position @ total) (h : node option Pref.heap @ immutable)
    (s : node @ immutable) (prefix : node list @ immutable)
    (suffix : node list @ immutable) :
    {u : unit | if ring h s (append prefix suffix)
        && separated (s :: append prefix suffix) then
      let left = last s prefix in
      let right = head suffix s in
      present h left && present h right
      && H.at h left.next === Some (Some right)
      && H.at h right.prev === Some (Some left)
      else true} @ ghost = ghost_ (
  let ns = append prefix suffix in
  let a = s :: prefix in
  let left = last s prefix in
  if ring h s ns && separated (s :: ns) then (
    ring_def h s ns; linked_chain h s ns s;
    append_def a suffix; chain_split h a suffix;
    last_def s a; last_member s a; member_present h a left;
    head_def suffix s;
    last_append s prefix suffix; last_def left [];
    (match suffix with
     | [] -> ()
     | r :: rest ->
       chain_junction h s a r rest; member_def r suffix;
       member_present h suffix r; ());
    ())
  else ())

let (insert_law @ total) (h : node option Pref.heap @ immutable)
    (s : node @ immutable) (prefix : node list @ immutable)
    (suffix : node list @ immutable) (n : node @ immutable) :
    {u : unit | if ring h s (append prefix suffix)
        && separated (s :: append prefix suffix)
        && not n.sentinel && not (n.prev === n.next)
        && not (H.mem h n.prev) && not (H.mem h n.next) then
      let left = last s prefix in
      let right = head suffix s in
      present h left && present h right
      && H.at h left.next === Some (Some right)
      && H.at h right.prev === Some (Some left)
      && ring (inserted h left n right) s (append prefix (n :: suffix))
      && separated (s :: append prefix (n :: suffix))
      else true} @ ghost = ghost_ (
  let ns = append prefix suffix in
  let a = s :: prefix in
  let left = last s prefix in
  let right = head suffix s in
  if ring h s ns && separated (s :: ns) && not n.sentinel
      && not (n.prev === n.next)
      && not (H.mem h n.prev) && not (H.mem h n.next) then (
    insert_position h s prefix suffix;
    ring_def h s ns; linked_chain h s ns s;
    append_def a suffix; chain_split h a suffix; separated_append a suffix;
    linked_access h s ns s; owns_def h (s :: ns); owns_fresh h n (s :: ns);
    apart_append n a suffix;
    last_def s a; last_member s a; member_present h a left;
    member_def s a; separated_pair a s left; apart_def s left; apart_def left s;
    apart_lists_member a suffix left; safe_external suffix left;
    safe_last s a; safe_external a n; safe_external suffix n;
    head_def suffix s;
    last_append s prefix suffix; last_def left [];
    (match suffix with
     | [] -> safe_head s prefix; safe_cell_def suffix right.prev; ()
     | r :: rest ->
       chain_junction h s a r rest;
       member_def r suffix; apart_lists_right a suffix r;
       apart_member r a left; apart_member r a s;
       apart_def r left; apart_def left r; apart_def r s; apart_def s r;
       safe_external a r; member_present h suffix r; safe_head r rest; ());
    let after = inserted h left n right in
    inserted_def h left n right; connected_def h left n;
    connected_def (connected h left n) n right;
    let h1 = H.put h left.next (Some n) in
    let h2 = H.put h1 n.prev (Some left) in
    let h3 = H.put h2 n.next (Some right) in
    chain_put h a left.next (Some n); chain_put h1 a n.prev (Some left);
    chain_put h2 a n.next (Some right); chain_put h3 a right.prev (Some n);
    chain_put h suffix left.next (Some n); chain_put h1 suffix n.prev (Some left);
    chain_put h2 suffix n.next (Some right);
    chain_put h3 suffix right.prev (Some n);
    Pref_ring_proofs.put_observations h left.next (Some n) n;
    Pref_ring_proofs.put_observations h1 n.prev (Some left) n;
    Pref_ring_proofs.put_observations h2 n.next (Some right) n;
    Pref_ring_proofs.put_observations h3 right.prev (Some n) n;
    present_def after n; present_def h left; present_def h right;
    inserted_view h left n right left;
    inserted_view h left n right s;
    let ns' = append prefix (n :: suffix) in
    chain_def after (n :: suffix);
    chain_join after s a (n :: suffix);
    append_def a (n :: suffix);
    ordinary_append prefix suffix; ordinary_append prefix (n :: suffix);
    ordinary_def (n :: suffix);
    last_append s prefix (n :: suffix); last_def left (n :: suffix);
    (match suffix with
     | [] -> last_def n []; ()
     | _ :: _ ->
       let z = last left suffix in
       last_nonempty n left suffix; last_member left suffix;
       member_present h suffix z;
       apart_lists_right a suffix z; apart_member z a left;
       apart_member n suffix z; apart_def n z; apart_def z n;
       member_def right suffix; separated_pair suffix right z;
       apart_def right z; apart_def z right; apart_def z left; apart_def left z;
       inserted_view h left n right z; ());
    chain_linked after s ns' s; ring_def after s ns';
    separated_def (n :: suffix); separated_append a (n :: suffix);
    append_def [n] suffix; append_def [] suffix;
    apart_lists_append_right a [n] suffix;
    apart_lists_def [n] a; apart_lists_def [] a; apart_lists_symmetric [n] a;
    ())
  else ())

let (remove_law @ total) (h : node option Pref.heap @ immutable)
    (s : node @ immutable) (prefix : node list @ immutable) (n : node @ immutable)
    (suffix : node list @ immutable) :
    {u : unit | if ring h s (append prefix (n :: suffix))
        && separated (s :: append prefix (n :: suffix)) then
      let left = last s prefix in
      let right = head suffix s in
      present h left && present h n && present h right && not (n === s)
      && H.at h left.next === Some (Some n) && H.at h n.prev === Some (Some left)
      && H.at h n.next === Some (Some right) && H.at h right.prev === Some (Some n)
      && ring (removed h left n right) s (append prefix suffix)
      && separated (s :: append prefix suffix)
      else true} @ ghost = ghost_ (
  let ns = append prefix (n :: suffix) in
  let a = s :: prefix in
  let b = n :: suffix in
  let left = last s prefix in
  let right = head suffix s in
  if ring h s ns && separated (s :: ns) then (
    ring_def h s ns; linked_chain h s ns s;
    append_def a b; chain_split h a b; chain_def h b;
    separated_append a b; separated_def b;
    last_def s a; last_member s a; member_present h a left;
    member_def s a; separated_pair a s left; apart_def s left; apart_def left s;
    chain_junction h s a n suffix;
    ordinary_append prefix b; ordinary_def b;
    member_def n b; apart_lists_right a b n; apart_member n a s;
    apart_def n s; apart_def s n;
    apart_lists_member a b left; apart_all_def left b;
    safe_last s a; safe_external a n; safe_external suffix n;
    safe_external suffix left;
    head_def suffix s;
    last_append s prefix b; last_def left b;
    (match suffix with
     | [] -> last_def n []; safe_head s prefix; safe_cell_def suffix right.prev; ()
     | r :: rest ->
       member_def r suffix; member_def r b; apart_lists_right a b r;
       apart_member r a left; apart_member r a s;
       apart_def r left; apart_def left r; apart_def r s; apart_def s r;
       apart_member n suffix r; apart_def n r; apart_def r n;
       safe_external a r; member_present h suffix r; safe_head r rest; ());
    let after = removed h left n right in
    removed_def h left n right; connected_def h left right;
    connected_def (connected h left right) n n;
    let h1 = H.put h left.next (Some right) in
    let h2 = H.put h1 right.prev (Some left) in
    let h3 = H.put h2 n.next (Some n) in
    chain_put h a left.next (Some right); chain_put h1 a right.prev (Some left);
    chain_put h2 a n.next (Some n); chain_put h3 a n.prev (Some n);
    chain_put h suffix left.next (Some right);
    chain_put h1 suffix right.prev (Some left);
    chain_put h2 suffix n.next (Some n); chain_put h3 suffix n.prev (Some n);
    removed_view h left n right left;
    removed_view h left n right s;
    (match suffix with [] -> () | _ :: _ -> removed_view h left n right right; ());
    let ns' = append prefix suffix in
    chain_join after s a suffix;
    append_def a suffix;
    ordinary_append prefix suffix;
    last_append s prefix suffix;
    (match suffix with
     | [] -> last_def left []; ()
     | _ :: _ ->
       let z = last left suffix in
       last_nonempty left n suffix; last_member left suffix;
       member_present h suffix z; member_def z b;
       apart_member left b z; apart_def left z; apart_def z left;
       apart_member n suffix z; apart_def n z; apart_def z n;
       member_def right suffix; separated_pair suffix right z;
       apart_def right z; apart_def z right;
       removed_view h left n right z; ());
    chain_linked after s ns' s; ring_def after s ns';
    separated_append a suffix;
    append_def [n] suffix; append_def [] suffix;
    apart_lists_append_right a [n] suffix;
    ())
  else ())

let (allocation_overwrite @ total) (h : node option Pref.heap @ immutable)
    (p : node option Pref.t @ immutable) (q : node option Pref.t @ immutable)
    (c : node option Pref.t @ immutable) (a : node option @ immutable)
    (b : node option @ immutable) (v : node option @ immutable)
    (x : node option @ immutable) (y : node option @ immutable) :
    {u : unit | if not (p === q) && not (c === p) && not (c === q) then
      H.put (H.put (H.put (H.put (H.put h p a) q b) c v) p x) q y ===
        H.put (H.put (H.put h c v) p x) q y
      else true} @ ghost = ghost_ (
  if not (p === q) && not (c === p) && not (c === q) then (
    let pa = H.put h p a in
    let pq = H.put pa q b in
    H.commute_law pq c v p x;
    H.commute_law pa q b p x;
    H.put_law h p a x;
    let px = H.put h p x in
    H.commute_law (H.put px q b) c v q y;
    H.put_law px q b y;
    H.commute_law h c v p x;
    H.commute_law px c v q y;
    ())
  else ())

let (allocation_inserted @ total) (h : node option Pref.heap @ immutable)
    (left : node @ immutable) (n : node @ immutable) (right : node @ immutable) :
    {u : unit | if not (n.prev === n.next) && H.mem h left.next
        && not (H.mem h n.prev) && not (H.mem h n.next) then
      inserted (H.put (H.put (H.put (H.put h n.prev None) n.next None)
        n.prev (Some n)) n.next (Some n)) left n right ===
      inserted h left n right
      else true} @ ghost = ghost_ (
  let p = n.prev in
  let q = n.next in
  let c = left.next in
  let fresh = H.put (H.put h p None) q None in
  let allocated = H.put (H.put fresh p (Some n)) q (Some n) in
  inserted_def allocated left n right; inserted_def h left n right;
  connected_def allocated left n; connected_def h left n;
  connected_def (connected allocated left n) n right;
  connected_def (connected h left n) n right;
  allocation_overwrite fresh p q c (Some n) (Some n) (Some n) (Some left) (Some right);
  allocation_overwrite h p q c None None (Some n) (Some left) (Some right);
  ())

let read_node : (p : node option Pref.t) @ immutable ->
    (expected : node) @ immutable ghost ->
    (state : {state : node option Pref.token | H.mem (Pref.own state) p &&
      H.at (Pref.own state) p === Some (Some expected)}) @ local read ->
    {n : node | n === expected} @ immutable = fun p expected state ->
  match Pref.read p state with
  | None -> unreachable_ ()
  | Some n -> n

let insert : (sentinel : node) @ immutable ghost ->
    (prefix : node list) @ immutable ghost -> (left : node) @ immutable ->
    (suffix : node list) @ immutable ghost -> (value : int) ->
    (t : {t : node option Pref.token | ring (Pref.own t) sentinel (append prefix suffix)
      && separated (sentinel :: append prefix suffix)
      && left === last sentinel prefix}) @ unique ->
    {r : created | r.node.value = value && not r.node.sentinel
      && not (H.mem (Pref.own t) r.node.prev) && not (H.mem (Pref.own t) r.node.next)
      && Pref.own r.state === inserted (Pref.own t) left r.node (head suffix sentinel)
      && ring (Pref.own r.state) sentinel (append prefix (r.node :: suffix))
      && separated (sentinel :: append prefix (r.node :: suffix))} @ unique =
  fun sentinel prefix left suffix value t ->
  let before = ghost_ (Pref.own (borrow_ t)) in
  ghost_ (insert_position before sentinel prefix suffix;
    present_def before left);
  let right = read_node left.next (ghost_ (head suffix sentinel)) (borrow_ t) in
  let made = make_node false value t in
  let n = made.node in
  let t = made.state in
  ghost_ (
    insert_law before sentinel prefix suffix n;
    present_def before right;
    Pref_ring_proofs.allocation_frame n left before;
    Pref_ring_proofs.allocation_frame n right before;
    present_def (Pref.own (borrow_ t)) left; present_def (Pref.own (borrow_ t)) right;
    present_def (Pref.own (borrow_ t)) n;
    allocation_inserted before left n right);
  let t = insert_between left n right t in
  {node = n; state = t}

let remove : (sentinel : node) @ immutable ->
    (prefix : node list) @ immutable ghost -> (n : node) @ immutable ->
    (suffix : node list) @ immutable ghost ->
    (t : {t : node option Pref.token | ring (Pref.own t) sentinel (append prefix (n :: suffix))
      && separated (sentinel :: append prefix (n :: suffix))}) @ unique ->
    {r : node option Pref.token |
      Pref.own r === removed (Pref.own t) (last sentinel prefix) n (head suffix sentinel)
      && ring (Pref.own r) sentinel (append prefix suffix)
      && separated (sentinel :: append prefix suffix)} @ unique =
  fun sentinel prefix n suffix t ->
  let before = ghost_ (Pref.own (borrow_ t)) in
  ghost_ (remove_law before sentinel prefix n suffix;
    present_def before n);
  let left = read_node n.prev (ghost_ (last sentinel prefix)) (borrow_ t) in
  let right = read_node n.next (ghost_ (head suffix sentinel)) (borrow_ t) in
  Pref_ring.remove sentinel left n right t

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

  let sentinel_node : (state : t) @ local read total forkable unyielding ->
      {s : node | s === sentinel state} @ immutable = fun state ->
    ghost_ (sentinel_def (borrow_ state));
    state.#owned.#sentinel

  type insertion = #{ node : node @@ aliased; ring : t }

  let insert : (prefix : node list) @ immutable ghost -> (left : node) @ immutable ->
      (suffix : node list) @ immutable ghost -> (value : int) ->
      (state : {state : t | model state === append prefix suffix &&
        left === last (sentinel state) prefix}) @ unique ->
      {r : insertion | r.#node.value = value && not r.#node.sentinel &&
        model r.#ring === append prefix (r.#node :: suffix) &&
        sentinel r.#ring === sentinel state &&
        not (H.mem (heap state) r.#node.prev) && not (H.mem (heap state) r.#node.next) &&
        heap r.#ring === inserted (heap state) left r.#node
          (head suffix (sentinel state))} @ unique =
    fun prefix left suffix value state ->
    ghost_ (model_def (borrow_ state); sentinel_def (borrow_ state); heap_def (borrow_ state));
    let b = state.#owned in
    let made = insert b.#sentinel prefix left suffix value b.#state in
    let node = made.node in
    let nodes = ghost_ (append prefix (node :: suffix)) in
    let owned = #{sentinel = b.#sentinel; nodes; state = made.state} in
    let ring : t = #{owned} in
    ghost_ (model_def (borrow_ ring); sentinel_def (borrow_ ring); heap_def (borrow_ ring));
    #{node; ring}

  let remove : (prefix : node list) @ immutable ghost -> (n : node) @ immutable ->
      (suffix : node list) @ immutable ghost ->
      (state : {state : t | model state === append prefix (n :: suffix)}) @ unique ->
      {next : t | model next === append prefix suffix &&
        sentinel next === sentinel state &&
        heap next === removed (heap state) (last (sentinel state) prefix) n
          (head suffix (sentinel state))} @ unique =
    fun prefix n suffix state ->
    ghost_ (model_def (borrow_ state); sentinel_def (borrow_ state); heap_def (borrow_ state));
    let b = state.#owned in
    let after = remove b.#sentinel prefix n suffix b.#state in
    let nodes = ghost_ (append prefix suffix) in
    let owned = #{sentinel = b.#sentinel; nodes; state = after} in
    let next : t = #{owned} in
    ghost_ (model_def (borrow_ next); sentinel_def (borrow_ next); heap_def (borrow_ next));
    next
end
