open Pref_ring
open Pref_ring_general

let[@def] rec (member @ total) (n : node @ immutable) (ns : node list @ immutable) =
  ghost_ (match ns with [] -> false | x :: rest -> n === x || member n rest)

let[@def] rec (last @ total) (fallback : node @ immutable) (ns : node list @ immutable) =
  ghost_ (match ns with [] -> fallback | n :: rest -> last n rest)

let[@def] rec (apart_lists @ total) (xs : node list @ immutable) (ys : node list @ immutable) =
  ghost_ (match xs with [] -> true | x :: rest -> apart_all x ys && apart_lists rest ys)

let rec (member_append @ total) : (n : node) @ immutable ->
    (xs : node list) @ immutable -> (ys : node list) @ immutable ->
    {u : unit | member n (append xs ys) = (member n xs || member n ys)} @ ghost =
  fun n xs ys -> ghost_ (
    append_def xs ys; member_def n xs; member_def n (append xs ys);
    match xs with [] -> () | _ :: rest -> member_append n rest ys; ())

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

let rec (apart_append @ total) : (n : node) @ immutable ->
    (xs : node list) @ immutable -> (ys : node list) @ immutable ->
    {u : unit | apart_all n (append xs ys) =
      (apart_all n xs && apart_all n ys)} @ ghost =
  fun n xs ys -> ghost_ (
    append_def xs ys; apart_all_def n xs; apart_all_def n (append xs ys);
    match xs with [] -> () | _ :: rest -> apart_append n rest ys; ())

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

type cuts : immutable_data = { left : node; first : node; final : node;
  right : node; destination_left : node; destination_right : node }

let[@def] (updated @ total) (h : node option Pref.heap @ immutable) (c : cuts @ immutable) =
  ghost_ (connected (connected (connected h c.left c.right)
    c.destination_left c.first) c.final c.destination_right)

let (chain_updated @ total) (h : node option Pref.heap @ immutable) (c : cuts @ immutable)
    (ns : node list @ immutable) :
    {u : unit | if chain h ns && safe_cell ns c.left.next && safe_cell ns c.right.prev &&
      safe_cell ns c.destination_left.next && safe_cell ns c.first.prev &&
      safe_cell ns c.final.next && safe_cell ns c.destination_right.prev
      then chain (updated h c) ns else true} @ ghost = ghost_ (
  updated_def h c;
  connected_def h c.left c.right;
  connected_def (connected h c.left c.right) c.destination_left c.first;
  connected_def (connected (connected h c.left c.right) c.destination_left c.first)
    c.final c.destination_right;
  chain_put h ns c.left.next (Some c.right);
  let h = H.put h c.left.next (Some c.right) in
  chain_put h ns c.right.prev (Some c.left);
  let h = H.put h c.right.prev (Some c.left) in
  chain_put h ns c.destination_left.next (Some c.first);
  let h = H.put h c.destination_left.next (Some c.first) in
  chain_put h ns c.first.prev (Some c.destination_left);
  let h = H.put h c.first.prev (Some c.destination_left) in
  chain_put h ns c.final.next (Some c.destination_right);
  let h = H.put h c.final.next (Some c.destination_right) in
  chain_put h ns c.destination_right.prev (Some c.final); ())

let (updated_view @ total) (h : node option Pref.heap @ immutable) (c : cuts @ immutable)
    (n : {n : node | present h n &&
      (n === c.left || apart n c.left) && (n === c.first || apart n c.first) &&
      (n === c.final || apart n c.final) && (n === c.right || apart n c.right) &&
      (n === c.destination_left || apart n c.destination_left) &&
      (n === c.destination_right || apart n c.destination_right)} @ immutable) :
    {u : unit | present (updated h c) n &&
      H.at (updated h c) n.prev ===
        (if n === c.destination_right then Some (Some c.final)
         else if n === c.first then Some (Some c.destination_left)
         else if n === c.right then Some (Some c.left) else H.at h n.prev) &&
      H.at (updated h c) n.next ===
        (if n === c.final then Some (Some c.destination_right)
         else if n === c.destination_left then Some (Some c.first)
         else if n === c.left then Some (Some c.right) else H.at h n.next)} @ ghost = ghost_ (
  present_def h n; present_def (updated h c) n;
  apart_def n c.left; apart_def n c.first; apart_def n c.final;
  apart_def n c.right; apart_def n c.destination_left; apart_def n c.destination_right;
  updated_def h c;
  connected_def h c.left c.right;
  connected_def (connected h c.left c.right) c.destination_left c.first;
  connected_def (connected (connected h c.left c.right) c.destination_left c.first)
    c.final c.destination_right;
  Pref_ring_proofs.put_observations h c.left.next (Some c.right) n;
  let h = H.put h c.left.next (Some c.right) in
  Pref_ring_proofs.put_observations h c.right.prev (Some c.left) n;
  let h = H.put h c.right.prev (Some c.left) in
  Pref_ring_proofs.put_observations h c.destination_left.next (Some c.first) n;
  let h = H.put h c.destination_left.next (Some c.first) in
  Pref_ring_proofs.put_observations h c.first.prev (Some c.destination_left) n;
  let h = H.put h c.first.prev (Some c.destination_left) in
  Pref_ring_proofs.put_observations h c.final.next (Some c.destination_right) n;
  let h = H.put h c.final.next (Some c.destination_right) in
  Pref_ring_proofs.put_observations h c.destination_right.prev (Some c.final) n; ())

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

let (partition_separated @ total) (a : node list @ immutable)
    (b : node list @ immutable) (c : node list @ immutable)
    (d : node list @ immutable) (e : node list @ immutable) :
    {u : unit | if separated (append a (append b (append c (append d e)))) then
      separated a && separated b && separated c && separated d && separated e &&
      apart_lists a b && apart_lists a c && apart_lists a d && apart_lists a e &&
      apart_lists b c && apart_lists b d && apart_lists b e &&
      apart_lists c d && apart_lists c e && apart_lists d e else true} @ ghost = ghost_ (
  separated_append a (append b (append c (append d e)));
  separated_append b (append c (append d e));
  separated_append c (append d e); separated_append d e;
  apart_lists_append_right a b (append c (append d e));
  apart_lists_append_right a c (append d e); apart_lists_append_right a d e;
  apart_lists_append_right b c (append d e); apart_lists_append_right b d e;
  apart_lists_append_right c d e; ())

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

let[@def] (boundaries @ total) (s : node @ immutable) (prefix : node list @ immutable)
    (first : node @ immutable) (rest : node list @ immutable) (suffix : node list @ immutable)
    (t : node @ immutable) (destination_prefix : node list @ immutable)
    (destination_suffix : node list @ immutable) = ghost_ (
  {left = last s prefix; first; final = last first rest; right = head suffix s;
   destination_left = last t destination_prefix;
   destination_right = head destination_suffix t})

let (partition_safety @ total) (s : node @ immutable) (prefix : node list @ immutable)
    (first : node @ immutable) (rest : node list @ immutable) (suffix : node list @ immutable)
    (t : node @ immutable) (destination_prefix : node list @ immutable)
    (destination_suffix : node list @ immutable) :
    {u : unit | let a = s :: prefix in let b = first :: rest in let c = suffix in
      let d = t :: destination_prefix in let e = destination_suffix in
      let cuts = boundaries s prefix first rest suffix t destination_prefix destination_suffix in
      if separated (append a (append b (append c (append d e)))) then
      safe_cell a cuts.left.next && safe_cell a cuts.right.prev && safe_cell a cuts.destination_left.next && safe_cell a cuts.first.prev && safe_cell a cuts.final.next && safe_cell a cuts.destination_right.prev &&
      safe_cell b cuts.left.next && safe_cell b cuts.right.prev && safe_cell b cuts.destination_left.next && safe_cell b cuts.first.prev && safe_cell b cuts.final.next && safe_cell b cuts.destination_right.prev &&
      safe_cell c cuts.left.next && safe_cell c cuts.right.prev && safe_cell c cuts.destination_left.next && safe_cell c cuts.first.prev && safe_cell c cuts.final.next && safe_cell c cuts.destination_right.prev &&
      safe_cell d cuts.left.next && safe_cell d cuts.right.prev && safe_cell d cuts.destination_left.next && safe_cell d cuts.first.prev && safe_cell d cuts.final.next && safe_cell d cuts.destination_right.prev &&
      safe_cell e cuts.left.next && safe_cell e cuts.right.prev && safe_cell e cuts.destination_left.next && safe_cell e cuts.first.prev && safe_cell e cuts.final.next && safe_cell e cuts.destination_right.prev else true} @ ghost = ghost_ (
  let a = s :: prefix in let b = first :: rest in let c = suffix in
  let d = t :: destination_prefix in let e = destination_suffix in
  let cuts = boundaries s prefix first rest suffix t destination_prefix destination_suffix in
  boundaries_def s prefix first rest suffix t destination_prefix destination_suffix;
  partition_separated a b c d e;
  last_def s a; last_def first b; last_def t d;
  last_member s a; last_member first b; last_member t d;
  member_def s a; member_def first b; member_def t d;
  safe_head s prefix; safe_head first rest; safe_head t destination_prefix;
  safe_last s a; safe_last first b; safe_last t d;
  apart_lists_member a b cuts.left;
  safe_external b cuts.left;
  apart_lists_member a c cuts.left;
  safe_external c cuts.left;
  apart_lists_member a d cuts.left;
  safe_external d cuts.left;
  apart_lists_member a e cuts.left;
  safe_external e cuts.left;
  apart_lists_right a b cuts.first;
  safe_external a cuts.first;
  apart_lists_member b c cuts.first;
  safe_external c cuts.first;
  apart_lists_member b d cuts.first;
  safe_external d cuts.first;
  apart_lists_member b e cuts.first;
  safe_external e cuts.first;
  apart_lists_right a b cuts.final;
  safe_external a cuts.final;
  apart_lists_member b c cuts.final;
  safe_external c cuts.final;
  apart_lists_member b d cuts.final;
  safe_external d cuts.final;
  apart_lists_member b e cuts.final;
  safe_external e cuts.final;
  apart_lists_right a d cuts.destination_left;
  safe_external a cuts.destination_left;
  apart_lists_right b d cuts.destination_left;
  safe_external b cuts.destination_left;
  apart_lists_right c d cuts.destination_left;
  safe_external c cuts.destination_left;
  apart_lists_member d e cuts.destination_left;
  safe_external e cuts.destination_left;
  head_def c s; head_def e t;
  (match c with
  | [] ->
  apart_lists_member a b s;
  safe_external b s;
  apart_lists_member a c s;
  safe_external c s;
  apart_lists_member a d s;
  safe_external d s;
  apart_lists_member a e s;
  safe_external e s;
    safe_cell_def [] cuts.right.prev; ()
  | n :: tail ->
    member_def n c; safe_head n tail;
  apart_lists_right a c n;
  safe_external a n;
  apart_lists_right b c n;
  safe_external b n;
  apart_lists_member c d n;
  safe_external d n;
  apart_lists_member c e n;
  safe_external e n;
    ());
  (match e with
  | [] ->
  apart_lists_right a d t;
  safe_external a t;
  apart_lists_right b d t;
  safe_external b t;
  apart_lists_right c d t;
  safe_external c t;
  apart_lists_member d e t;
  safe_external e t;
    safe_cell_def [] cuts.destination_right.prev; ()
  | n :: tail ->
    member_def n e; safe_head n tail;
  apart_lists_right a e n;
  safe_external a n;
  apart_lists_right b e n;
  safe_external b n;
  apart_lists_right c e n;
  safe_external c n;
  apart_lists_right d e n;
  safe_external d n;
    ());
  ())

let (partition_member @ total) (n : node @ immutable)
    (a : node list @ immutable) (b : node list @ immutable) (c : node list @ immutable)
    (d : node list @ immutable) (e : node list @ immutable) :
    {u : unit | member n (append a (append b (append c (append d e)))) =
      (member n a || member n b || member n c || member n d || member n e)} @ ghost = ghost_ (
  member_append n a (append b (append c (append d e)));
  member_append n b (append c (append d e));
  member_append n c (append d e); member_append n d e; ())

let (different_chunks @ total) (xs : node list @ immutable) (ys : node list @ immutable)
    (x : node @ immutable) (y : node @ immutable) :
    {u : unit | if apart_lists xs ys && member x xs && member y ys
      then apart x y && not (x === y) else true} @ ghost = ghost_ (
  apart_lists_member xs ys x; apart_member x ys y; apart_def x y; ())

let rec (chain_bridge @ total) : (h : node option Pref.heap) @ immutable ->
    (fallback : node) @ immutable -> (xs : node list) @ immutable ->
    (ys : node list) @ immutable ->
    {u : unit | if chain h (append xs ys) then
      (match xs, ys with [], _ | _, [] -> true | _, y :: _ ->
        H.at h (last fallback xs).next === Some (Some y) &&
        H.at h y.prev === Some (Some (last fallback xs))) else true} @ ghost =
  fun h fallback xs ys -> ghost_ (
    append_def xs ys; last_def fallback xs; chain_def h (append xs ys);
    match xs with [] -> () | n :: rest ->
      append_def rest ys; last_def n rest;
      chain_bridge h n rest ys; ())

let (partition_view @ total) (h : node option Pref.heap @ immutable)
    (s : node @ immutable) (prefix : node list @ immutable)
    (first : node @ immutable) (rest : node list @ immutable) (suffix : node list @ immutable)
    (t : node @ immutable) (destination_prefix : node list @ immutable)
    (destination_suffix : node list @ immutable)
    (n : {n : node | present h n &&
      let all = append (s :: prefix) (append (first :: rest)
        (append suffix (append (t :: destination_prefix) destination_suffix))) in
      separated all && member n all} @ immutable) :
    {u : unit | let c = boundaries s prefix first rest suffix t destination_prefix destination_suffix in present (updated h c) n &&
      H.at (updated h c) n.prev ===
        (if n === c.destination_right then Some (Some c.final)
         else if n === c.first then Some (Some c.destination_left)
         else if n === c.right then Some (Some c.left) else H.at h n.prev) &&
      H.at (updated h c) n.next ===
        (if n === c.final then Some (Some c.destination_right)
         else if n === c.destination_left then Some (Some c.first)
         else if n === c.left then Some (Some c.right) else H.at h n.next)} @ ghost = ghost_ (
  let a = s :: prefix in let b = first :: rest in let c = suffix in
  let d = t :: destination_prefix in let e = destination_suffix in
  let all = append a (append b (append c (append d e))) in
  let cuts = boundaries s prefix first rest suffix t destination_prefix destination_suffix in
  boundaries_def s prefix first rest suffix t destination_prefix destination_suffix;
  last_def s a; last_def first b; last_def t d;
  last_member s a; last_member first b; last_member t d;
  member_def s a; member_def first b; member_def t d;
  head_def c s; head_def e t;
  (match c with [] -> () | x :: _ -> member_def x c; ());
  (match e with [] -> () | x :: _ -> member_def x e; ());
  partition_member cuts.left a b c d e;
  separated_pair all n cuts.left;
  partition_member cuts.first a b c d e;
  separated_pair all n cuts.first;
  partition_member cuts.final a b c d e;
  separated_pair all n cuts.final;
  partition_member cuts.right a b c d e;
  separated_pair all n cuts.right;
  partition_member cuts.destination_left a b c d e;
  separated_pair all n cuts.destination_left;
  partition_member cuts.destination_right a b c d e;
  separated_pair all n cuts.destination_right;
  updated_view h cuts n; ())

let (partition_old @ total) (h : node option Pref.heap @ immutable)
    (s : node @ immutable) (prefix : node list @ immutable)
    (first : node @ immutable) (rest : node list @ immutable) (suffix : node list @ immutable)
    (t : node @ immutable) (destination_prefix : node list @ immutable)
    (destination_suffix : node list @ immutable) :
    {u : unit | let a = s :: prefix in let b = first :: rest in let c = suffix in
      let d = t :: destination_prefix in let e = destination_suffix in
      let cuts = boundaries s prefix first rest suffix t destination_prefix destination_suffix in
      if ring h s (append prefix (append b c)) &&
        ring h t (append destination_prefix e) then
      chain h a && chain h b && chain h c && chain h d && chain h e &&
      ordinary prefix && ordinary b && ordinary c && ordinary destination_prefix && ordinary e &&
      s.sentinel && t.sentinel &&
      present h s && present h t && present h cuts.left && present h first &&
      present h cuts.final && present h cuts.right &&
      present h cuts.destination_left && present h cuts.destination_right &&
      H.at h cuts.left.next === Some (Some first) &&
      H.at h first.prev === Some (Some cuts.left) &&
      H.at h cuts.final.next === Some (Some cuts.right) &&
      H.at h cuts.right.prev === Some (Some cuts.final) &&
      H.at h cuts.destination_left.next === Some (Some cuts.destination_right) &&
      H.at h cuts.destination_right.prev === Some (Some cuts.destination_left) &&
      H.at h (last cuts.final c).next === Some (Some s) &&
      H.at h s.prev === Some (Some (last cuts.final c)) &&
      H.at h (last cuts.destination_left e).next === Some (Some t) &&
      H.at h t.prev === Some (Some (last cuts.destination_left e)) else true} @ ghost = ghost_ (
  let a = s :: prefix in let b = first :: rest in let c = suffix in
  let d = t :: destination_prefix in let e = destination_suffix in
  let cuts = boundaries s prefix first rest suffix t destination_prefix destination_suffix in
  boundaries_def s prefix first rest suffix t destination_prefix destination_suffix;
  ring_def h s (append prefix (append b c));
  ring_def h t (append destination_prefix e);
  linked_chain h s (append prefix (append b c)) s;
  linked_chain h t (append destination_prefix e) t;
  append_def a (append b c); append_def d e;
  chain_split h a (append b c); chain_split h b c; chain_split h d e;
  chain_bridge h s a (append b c); chain_bridge h first b c; chain_bridge h t d e;
  append_def b c;
  ordinary_append prefix (append b c); ordinary_append b c;
  ordinary_append destination_prefix e;
  last_append s prefix (append b c); last_append (last s prefix) b c;
  last_append t destination_prefix e;
  last_def s a; last_def (last s prefix) b; last_def first b; last_def t d;
  last_member s a; last_member first b; last_member t d;
  member_def first b;
  member_present h a cuts.left; member_present h b first;
  member_present h b cuts.final; member_present h d cuts.destination_left;
  head_def c s; head_def e t;
  (match c with [] -> last_def cuts.final []; () | n :: _ ->
    member_def n c; member_present h c n; ());
  (match e with [] -> last_def cuts.destination_left []; () | n :: _ ->
    member_def n e; member_present h e n; ()); ())

let (partition_new_edges @ total) (h : node option Pref.heap @ immutable)
    (s : node @ immutable) (prefix : node list @ immutable)
    (first : node @ immutable) (rest : node list @ immutable) (suffix : node list @ immutable)
    (t : node @ immutable) (destination_prefix : node list @ immutable)
    (destination_suffix : node list @ immutable) :
    {u : unit | let a = s :: prefix in let b = first :: rest in let c = suffix in
      let d = t :: destination_prefix in let e = destination_suffix in
      let cuts = boundaries s prefix first rest suffix t destination_prefix destination_suffix in
      let after = updated h cuts in
      if ring h s (append prefix (append b c)) && ring h t (append destination_prefix e) &&
        separated (append a (append b (append c (append d e)))) then
      present after s && present after t &&
      H.at after cuts.left.next === Some (Some cuts.right) &&
      H.at after cuts.right.prev === Some (Some cuts.left) &&
      H.at after cuts.destination_left.next === Some (Some first) &&
      H.at after first.prev === Some (Some cuts.destination_left) &&
      H.at after cuts.final.next === Some (Some cuts.destination_right) &&
      H.at after cuts.destination_right.prev === Some (Some cuts.final) &&
      H.at after (last cuts.left c).next === Some (Some s) &&
      H.at after s.prev === Some (Some (last cuts.left c)) &&
      H.at after (last cuts.final e).next === Some (Some t) &&
      H.at after t.prev === Some (Some (last cuts.final e)) else true} @ ghost = ghost_ (
  let a = s :: prefix in let b = first :: rest in let c = suffix in
  let d = t :: destination_prefix in let e = destination_suffix in
  let all = append a (append b (append c (append d e))) in
  let cuts = boundaries s prefix first rest suffix t destination_prefix destination_suffix in
  if ring h s (append prefix (append b c)) && ring h t (append destination_prefix e) &&
      separated all then (
    boundaries_def s prefix first rest suffix t destination_prefix destination_suffix;
    partition_old h s prefix first rest suffix t destination_prefix destination_suffix;
    partition_separated a b c d e;
    last_def s a; last_def first b; last_def t d;
    last_member s a; last_member first b; last_member t d;
    member_def s a; member_def first b; member_def t d;
    head_def c s; head_def e t;
    let source_end = last s c in let destination_end = last t e in
    last_member s c; last_member t e;
    (match c with [] -> last_def s []; () | n :: _ ->
      member_def n c; member_present h c source_end; ());
    (match e with [] -> last_def t []; () | n :: _ ->
      member_def n e; member_present h e destination_end; ());
    partition_member s a b c d e;
    partition_view h s prefix first rest suffix t destination_prefix destination_suffix s;
    partition_member t a b c d e;
    partition_view h s prefix first rest suffix t destination_prefix destination_suffix t;
    partition_member cuts.left a b c d e;
    partition_view h s prefix first rest suffix t destination_prefix destination_suffix cuts.left;
    partition_member first a b c d e;
    partition_view h s prefix first rest suffix t destination_prefix destination_suffix first;
    partition_member cuts.final a b c d e;
    partition_view h s prefix first rest suffix t destination_prefix destination_suffix cuts.final;
    partition_member cuts.right a b c d e;
    partition_view h s prefix first rest suffix t destination_prefix destination_suffix cuts.right;
    partition_member cuts.destination_left a b c d e;
    partition_view h s prefix first rest suffix t destination_prefix destination_suffix cuts.destination_left;
    partition_member cuts.destination_right a b c d e;
    partition_view h s prefix first rest suffix t destination_prefix destination_suffix cuts.destination_right;
    partition_member source_end a b c d e;
    partition_view h s prefix first rest suffix t destination_prefix destination_suffix source_end;
    partition_member destination_end a b c d e;
    partition_view h s prefix first rest suffix t destination_prefix destination_suffix destination_end;
    different_chunks a b s first;
    different_chunks a b s cuts.final;
    different_chunks a b cuts.left first;
    different_chunks a b cuts.left cuts.final;
    different_chunks a d s t;
    different_chunks a d s cuts.destination_left;
    different_chunks a d cuts.left t;
    different_chunks a d cuts.left cuts.destination_left;
    different_chunks b d first t;
    different_chunks b d first cuts.destination_left;
    different_chunks b d cuts.final t;
    different_chunks b d cuts.final cuts.destination_left;
    (match c with [] -> () | _ :: _ ->
    different_chunks a c s cuts.right;
    different_chunks a c s source_end;
    different_chunks a c cuts.left cuts.right;
    different_chunks a c cuts.left source_end;
    different_chunks b c first cuts.right;
    different_chunks b c first source_end;
    different_chunks b c cuts.final cuts.right;
    different_chunks b c cuts.final source_end;
    different_chunks c d cuts.right t;
    different_chunks c d cuts.right cuts.destination_left;
    different_chunks c d source_end t;
    different_chunks c d source_end cuts.destination_left;
      ());
    (match e with [] -> () | _ :: _ ->
    different_chunks a e s cuts.destination_right;
    different_chunks a e s destination_end;
    different_chunks a e cuts.left cuts.destination_right;
    different_chunks a e cuts.left destination_end;
    different_chunks b e first cuts.destination_right;
    different_chunks b e first destination_end;
    different_chunks b e cuts.final cuts.destination_right;
    different_chunks b e cuts.final destination_end;
    different_chunks d e t cuts.destination_right;
    different_chunks d e t destination_end;
    different_chunks d e cuts.destination_left cuts.destination_right;
    different_chunks d e cuts.destination_left destination_end;
      (match c with [] -> () | _ :: _ ->
    different_chunks c e cuts.right cuts.destination_right;
    different_chunks c e cuts.right destination_end;
    different_chunks c e source_end cuts.destination_right;
    different_chunks c e source_end destination_end;
        ()); ());
    last_nonempty s cuts.left c; last_nonempty s cuts.final c;
    last_nonempty t cuts.final e; last_nonempty t cuts.destination_left e;
    (match c with [] -> last_def cuts.left []; last_def cuts.final []; () | _ :: _ -> ());
    (match e with [] -> last_def cuts.final []; last_def cuts.destination_left []; () | _ :: _ -> ());
    ()) else ())

let rec (append_associative @ total) : (xs : node list) @ immutable ->
    (ys : node list) @ immutable -> (zs : node list) @ immutable ->
    {u : unit | append (append xs ys) zs === append xs (append ys zs)} @ ghost =
  fun xs ys zs -> ghost_ (
    append_def xs ys; append_def (append xs ys) zs; append_def xs (append ys zs);
    match xs with [] -> () | _ :: rest -> append_associative rest ys zs; ())

let (partition_reorder @ total) (a : node list @ immutable)
    (b : node list @ immutable) (c : node list @ immutable)
    (d : node list @ immutable) (e : node list @ immutable) :
    {u : unit | if separated (append a (append b (append c (append d e)))) then
      separated (append (append a c) (append d (append b e))) else true} @ ghost = ghost_ (
  partition_separated a b c d e;
  apart_lists_symmetric b c; apart_lists_symmetric b d;
  separated_append a c; separated_append b e; separated_append d (append b e);
  apart_lists_append_right d b e;
  apart_lists_append_left a c (append d (append b e));
  apart_lists_append_right a d (append b e); apart_lists_append_right a b e;
  apart_lists_append_right c d (append b e); apart_lists_append_right c b e;
  separated_append (append a c) (append d (append b e)); ())

let (splice_law @ total) (h : node option Pref.heap @ immutable)
    (s : node @ immutable) (prefix : node list @ immutable)
    (first : node @ immutable) (rest : node list @ immutable) (suffix : node list @ immutable)
    (t : node @ immutable) (destination_prefix : node list @ immutable)
    (destination_suffix : node list @ immutable) :
    {u : unit | let before_source = append prefix (append (first :: rest) suffix) in
      let before_destination = append destination_prefix destination_suffix in
      let after_source = append prefix suffix in
      let after_destination = append destination_prefix (append (first :: rest) destination_suffix) in
      let cuts = boundaries s prefix first rest suffix t destination_prefix destination_suffix in
      let after = updated h cuts in
      if ring h s before_source && ring h t before_destination &&
        separated (append (s :: before_source) (t :: before_destination)) then
      ring after s after_source && ring after t after_destination &&
      separated (append (s :: after_source) (t :: after_destination)) else true} @ ghost = ghost_ (
  let a = s :: prefix in let b = first :: rest in let c = suffix in
  let d = t :: destination_prefix in let e = destination_suffix in
  let cuts = boundaries s prefix first rest suffix t destination_prefix destination_suffix in
  let after = updated h cuts in
  boundaries_def s prefix first rest suffix t destination_prefix destination_suffix;
  append_def a (append b c); append_def d e;
  append_associative a (append b c) (append d e);
  append_associative b c (append d e);
  partition_old h s prefix first rest suffix t destination_prefix destination_suffix;
  partition_safety s prefix first rest suffix t destination_prefix destination_suffix;
  partition_new_edges h s prefix first rest suffix t destination_prefix destination_suffix;
  chain_updated h cuts a; chain_updated h cuts b; chain_updated h cuts c;
  chain_updated h cuts d; chain_updated h cuts e;
  last_def s a; last_def first b; last_def t d;
  head_def c s; head_def e t;
  chain_join after s a c;
  chain_join after first b e;
  append_def b e;
  chain_join after t d (append b e);
  append_def a c; append_def d (append b e);
  ordinary_append prefix c; ordinary_append b e;
  ordinary_append destination_prefix (append b e);
  last_append s prefix c;
  last_append t destination_prefix (append b e);
  last_append (last t destination_prefix) b e;
  last_def (last t destination_prefix) b;
  chain_linked after s (append prefix c) s;
  chain_linked after t (append destination_prefix (append b e)) t;
  ring_def after s (append prefix c);
  ring_def after t (append destination_prefix (append b e));
  partition_reorder a b c d e; ())

let[@def] (spliced @ total) (h : node option Pref.heap @ immutable) (left : node @ immutable)
    (first : node @ immutable) (final : node @ immutable) (right : node @ immutable)
    (destination_left : node @ immutable) (destination_right : node @ immutable) =
  ghost_ (connected (connected (connected h left right) destination_left first)
    final destination_right)

let (splice_preparation @ total) (h : node option Pref.heap @ immutable)
    (s : node @ immutable) (prefix : node list @ immutable)
    (first : node @ immutable) (rest : node list @ immutable) (suffix : node list @ immutable)
    (t : node @ immutable) (destination_prefix : node list @ immutable)
    (destination_suffix : node list @ immutable) :
    {u : unit | let cuts = boundaries s prefix first rest suffix t destination_prefix destination_suffix in
      if ring h s (append prefix (append (first :: rest) suffix)) &&
        ring h t (append destination_prefix destination_suffix) then
      present h cuts.left && present h first && present h cuts.final && present h cuts.right &&
      present h cuts.destination_left && present h cuts.destination_right &&
      H.at h cuts.left.next === Some (Some first) && H.at h first.prev === Some (Some cuts.left) &&
      H.at h cuts.final.next === Some (Some cuts.right) && H.at h cuts.right.prev === Some (Some cuts.final) &&
      H.at h cuts.destination_left.next === Some (Some cuts.destination_right) &&
      H.at h cuts.destination_right.prev === Some (Some cuts.destination_left)
      else true} @ ghost = ghost_ (
  partition_old h s prefix first rest suffix t destination_prefix destination_suffix; ())

let read_node : (p : node option Pref.t) @ immutable ->
    (expected : node) @ immutable ghost ->
    (state : {state : node option Pref.token | H.mem (Pref.own state) p &&
      H.at (Pref.own state) p === Some (Some expected)}) @ local read ->
    {n : node | n === expected} @ immutable = fun p expected state ->
  match Pref.read p state with
  | None -> failwith "unlinked node"
  | Some n -> n

let splice : (s : node) @ immutable ghost -> (t : node) @ immutable ghost ->
    (prefix : node list) @ immutable ghost -> (first : node) @ immutable ->
    (rest : node list) @ immutable ghost ->
    (final : {n : node | n === last first rest}) @ immutable ->
    (suffix : node list) @ immutable ghost ->
    (destination_prefix : node list) @ immutable ghost ->
    (destination_left : {n : node | n === last t destination_prefix}) @ immutable ->
    (destination_suffix : node list) @ immutable ghost ->
    (state : {state : node option Pref.token |
      ring (Pref.own state) s (append prefix (append (first :: rest) suffix)) &&
      ring (Pref.own state) t (append destination_prefix destination_suffix) &&
      separated (append (s :: append prefix (append (first :: rest) suffix))
        (t :: append destination_prefix destination_suffix))}) @ unique ->
    {result : node option Pref.token |
      Pref.own result === spliced (Pref.own state) (last s prefix) first final
        (head suffix s) destination_left (head destination_suffix t) &&
      ring (Pref.own result) s (append prefix suffix) &&
      ring (Pref.own result) t (append destination_prefix (append (first :: rest) destination_suffix)) &&
      separated (append (s :: append prefix suffix)
        (t :: append destination_prefix (append (first :: rest) destination_suffix)))} @ unique =
  fun s t prefix first rest final suffix destination_prefix destination_left destination_suffix state ->
  let before = ghost_ (Pref.own (borrow_ state)) in
  ghost_ (
    splice_preparation before s prefix first rest suffix t destination_prefix destination_suffix;
    boundaries_def s prefix first rest suffix t destination_prefix destination_suffix;
    present_def before first; present_def before final; present_def before destination_left;
    present_def before (last s prefix); present_def before (head suffix s);
    present_def before (head destination_suffix t));
  let left = read_node first.prev (ghost_ (last s prefix)) (borrow_ state) in
  let right = read_node final.next (ghost_ (head suffix s)) (borrow_ state) in
  let destination_right = read_node destination_left.next
    (ghost_ (head destination_suffix t)) (borrow_ state) in
  ghost_ (
    splice_law before s prefix first rest suffix t destination_prefix destination_suffix;
    let cuts = boundaries s prefix first rest suffix t destination_prefix destination_suffix in
    updated_def before cuts;
    spliced_def before left first final right destination_left destination_right;
    connected_def before left right;
    connected_def (connected before left right) destination_left first;
    connected_def (connected (connected before left right) destination_left first) final destination_right);
  Pref_ring.splice_range left first final right destination_left destination_right state

type paired = #{source : node @@ aliased; destination : node @@ aliased;
  source_nodes : node list @@ aliased ghost; destination_nodes : node list @@ aliased ghost;
  state : node option Pref.token}

let rec (linked_path @ total) : (h : node option Pref.heap) @ immutable ->
    (previous : node) @ immutable -> (ns : node list) @ immutable -> (stop : node) @ immutable ->
    {u : unit | not (linked h previous ns stop) || path h false ns stop} @ ghost =
  fun h previous ns stop -> ghost_ (
    linked_def h previous ns stop; path_def h false ns stop;
    match ns with [] -> () | n :: rest -> field_def false n; linked_path h n rest stop; ())

module Owned = struct
  type payload = #{source : node @@ global; destination : node @@ global;
    source_nodes : node list @@ global ghost; destination_nodes : node list @@ global ghost;
    state : node option Pref.token}
  type t = #{owned : {b : payload |
    ring (Pref.own b.#state) b.#source b.#source_nodes &&
    ring (Pref.own b.#state) b.#destination b.#destination_nodes &&
    separated (append (b.#source :: b.#source_nodes) (b.#destination :: b.#destination_nodes))}}

  let[@def] source (state : t @ local immutable total ghost) = ghost_ state.#owned.#source
  let[@def] destination (state : t @ local immutable total ghost) = ghost_ state.#owned.#destination
  let[@def] source_model (state : t @ local immutable total ghost) = ghost_ state.#owned.#source_nodes
  let[@def] destination_model (state : t @ local immutable total ghost) = ghost_ state.#owned.#destination_nodes
  let[@def] heap (state : t @ local immutable total ghost) = ghost_ (Pref.own state.#owned.#state)

  let (observations @ total) (state : t @ local immutable total ghost forkable unyielding) :
      {u : unit | source state === state.#owned.#source && destination state === state.#owned.#destination &&
        source_model state === state.#owned.#source_nodes &&
        destination_model state === state.#owned.#destination_nodes && heap state === Pref.own state.#owned.#state}
      @ ghost = ghost_ (source_def state; destination_def state;
    source_model_def state; destination_model_def state; heap_def state; ())

  let adopt : (b : {b : paired | ring (Pref.own b.#state) b.#source b.#source_nodes &&
      ring (Pref.own b.#state) b.#destination b.#destination_nodes &&
      separated (append (b.#source :: b.#source_nodes) (b.#destination :: b.#destination_nodes))}) @ unique ->
      {state : t | source state === b.#source && destination state === b.#destination &&
        source_model state === b.#source_nodes && destination_model state === b.#destination_nodes &&
        heap state === Pref.own b.#state} @ unique = fun b ->
    let owned = #{source = b.#source; destination = b.#destination;
      source_nodes = b.#source_nodes; destination_nodes = b.#destination_nodes; state = b.#state} in
    let state : t = #{owned} in
    ghost_ (observations (borrow_ state)); state

  let release : (state : t) @ unique ->
      {b : paired | ring (Pref.own b.#state) b.#source b.#source_nodes &&
        ring (Pref.own b.#state) b.#destination b.#destination_nodes &&
        separated (append (b.#source :: b.#source_nodes) (b.#destination :: b.#destination_nodes)) &&
        b.#source === source state && b.#destination === destination state &&
        b.#source_nodes === source_model state && b.#destination_nodes === destination_model state &&
        Pref.own b.#state === heap state} @ unique = fun state ->
    ghost_ (observations (borrow_ state));
    let b = state.#owned in
    #{source = b.#source; destination = b.#destination;
      source_nodes = b.#source_nodes; destination_nodes = b.#destination_nodes; state = b.#state}

  let splice : (prefix : node list) @ immutable ghost -> (first : node) @ immutable ->
      (rest : node list) @ immutable ghost ->
      (final : {n : node | n === last first rest}) @ immutable ->
      (suffix : node list) @ immutable ghost ->
      (destination_prefix : node list) @ immutable ghost -> (destination_left : node) @ immutable ->
      (destination_suffix : node list) @ immutable ghost ->
      (state : {state : t | source_model state === append prefix (append (first :: rest) suffix) &&
        destination_model state === append destination_prefix destination_suffix &&
        destination_left === last (destination state) destination_prefix}) @ unique ->
      {next : t | source next === source state && destination next === destination state &&
        source_model next === append prefix suffix &&
        destination_model next === append destination_prefix (append (first :: rest) destination_suffix) &&
        heap next === spliced (heap state) (last (source state) prefix) first final
          (head suffix (source state)) destination_left (head destination_suffix (destination state))} @ unique =
    fun prefix first rest final suffix destination_prefix destination_left destination_suffix state ->
    ghost_ (observations (borrow_ state));
    let b = state.#owned in
    let after = splice b.#source b.#destination prefix first rest final suffix
      destination_prefix destination_left destination_suffix b.#state in
    let owned = #{source = b.#source; destination = b.#destination;
      source_nodes = ghost_ (append prefix suffix);
      destination_nodes = ghost_ (append destination_prefix (append (first :: rest) destination_suffix));
      state = after} in
    let next : t = #{owned} in
    ghost_ (observations (borrow_ next)); next

  let observe_source : (state : t) @ local read total forkable unyielding ->
      {nodes : node list | nodes === source_model state} @ immutable = fun state ->
    ghost_ (observations (borrow_ state));
    let b = state.#owned in
    let h = ghost_ (Pref.own (borrow_ b.#state)) in
    ghost_ (ring_def h b.#source b.#source_nodes;
      linked_path h b.#source b.#source_nodes b.#source; field_def false b.#source);
    traverse false b.#source b.#source_nodes (borrow_ b.#state)

  let observe_destination : (state : t) @ local read total forkable unyielding ->
      {nodes : node list | nodes === destination_model state} @ immutable = fun state ->
    ghost_ (observations (borrow_ state));
    let b = state.#owned in
    let h = ghost_ (Pref.own (borrow_ b.#state)) in
    ghost_ (ring_def h b.#destination b.#destination_nodes;
      linked_path h b.#destination b.#destination_nodes b.#destination; field_def false b.#destination);
    traverse false b.#destination b.#destination_nodes (borrow_ b.#state)
  let swap : (state : t) @ unique ->
      {next : t | source next === destination state && destination next === source state &&
        source_model next === destination_model state && destination_model next === source_model state &&
        heap next === heap state} @ unique = fun state ->
    ghost_ (observations (borrow_ state));
    let b = state.#owned in
    ghost_ (
      let xs = b.#source :: b.#source_nodes in let ys = b.#destination :: b.#destination_nodes in
      separated_append xs ys; apart_lists_symmetric xs ys; separated_append ys xs);
    let owned = #{source = b.#destination; destination = b.#source;
      source_nodes = b.#destination_nodes; destination_nodes = b.#source_nodes; state = b.#state} in
    let next : t = #{owned} in
    ghost_ (observations (borrow_ next)); next
end
