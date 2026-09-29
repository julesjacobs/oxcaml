open Pref_ring

val append : node list @ immutable -> node list @ immutable -> node list @ ghost @@ total
val append_def : (xs : node list) @ immutable -> (ys : node list) @ immutable ->
  {u : unit | append xs ys === (ghost_ (match xs with
    | [] -> ys | x :: rest -> x :: append rest ys))} @@ total
val reversed : node list @ immutable -> node list @ ghost @@ total
val reversed_def : (xs : node list) @ immutable ->
  {u : unit | reversed xs === (ghost_ (match xs with
    | [] -> [] | x :: rest -> append (reversed rest) [x]))} @@ total

val last : node @ immutable -> node list @ immutable -> node @ ghost @@ total
val last_def : (fallback : node) @ immutable -> (ns : node list) @ immutable ->
  {u : unit | last fallback ns === (ghost_ (match ns with
    | [] -> fallback | n :: rest -> last n rest))} @@ total

val reverse : (sentinel : node) @ immutable ->
    (ns : node list) @ immutable ghost ->
    (t : {t : node option Pref.token | ring (Pref.own t) sentinel ns &&
      separated (sentinel :: ns)}) @ unique ->
    {r : node option Pref.token | Pref.own r === flipped_all (Pref.own t) (sentinel :: ns) &&
      ring (Pref.own r) sentinel (reversed ns) && separated (sentinel :: reversed ns)}
      @ unique

val insert : (sentinel : node) @ immutable ghost ->
    (prefix : node list) @ immutable ghost -> (left : node) @ immutable ->
    (suffix : node list) @ immutable ghost -> (value : int) ->
    (t : {t : node option Pref.token | ring (Pref.own t) sentinel (append prefix suffix)
      && separated (sentinel :: append prefix suffix)
      && left === last sentinel prefix}) @ unique ->
    {r : created | r.node.value = value && not r.node.sentinel
      && not (H.mem (Pref.own t) r.node.prev) && not (H.mem (Pref.own t) r.node.next)
      && Pref.own r.state === inserted (Pref.own t) left r.node (head suffix sentinel)
      && ring (Pref.own r.state) sentinel (append prefix (r.node :: suffix))
      && separated (sentinel :: append prefix (r.node :: suffix))} @ unique

val remove : (sentinel : node) @ immutable ->
    (prefix : node list) @ immutable ghost -> (n : node) @ immutable ->
    (suffix : node list) @ immutable ghost ->
    (t : {t : node option Pref.token | ring (Pref.own t) sentinel (append prefix (n :: suffix))
      && separated (sentinel :: append prefix (n :: suffix))}) @ unique ->
    {r : node option Pref.token |
      Pref.own r === removed (Pref.own t) (last sentinel prefix) n (head suffix sentinel)
      && ring (Pref.own r) sentinel (append prefix suffix)
      && separated (sentinel :: append prefix suffix)} @ unique

type built = { sentinel : node @@ aliased; nodes : node list @@ aliased ghost;
  state : node option Pref.token }

module Owned : sig
  type t : value & void & void [@@total_matchable]
  val model : t @ local immutable total ghost -> node list @ ghost @@ total
  val sentinel : t @ local immutable total ghost -> node @ ghost @@ total
  val heap : t @ local immutable total ghost -> node option Pref.heap @ ghost @@ total

  val adopt : (b : {b : built | ring (Pref.own b.state) b.sentinel b.nodes &&
      separated (b.sentinel :: b.nodes)}) @ unique ->
      {state : t | model state === b.nodes && sentinel state === b.sentinel &&
        heap state === Pref.own b.state} @ unique

  val release : (state : t) @ unique ->
      {b : built | ring (Pref.own b.state) b.sentinel b.nodes &&
        separated (b.sentinel :: b.nodes) && b.nodes === model state &&
        b.sentinel === sentinel state && Pref.own b.state === heap state} @ unique

  val empty : unit -> {state : t | model state === []} @ unique

  val reverse : (state : t) @ unique ->
      {next : t | model next === reversed (model state) &&
        sentinel next === sentinel state &&
        heap next === flipped_all (heap state) (sentinel state :: model state)} @ unique

  val observe : (state : t) @ local read total forkable unyielding ->
      {nodes : node list | nodes === model state} @ immutable

  val sentinel_node : (state : t) @ local read total forkable unyielding ->
      {s : node | s === sentinel state} @ immutable

  type insertion = #{ node : node @@ aliased; ring : t }

  val insert : (prefix : node list) @ immutable ghost -> (left : node) @ immutable ->
      (suffix : node list) @ immutable ghost -> (value : int) ->
      (state : {state : t | model state === append prefix suffix &&
        left === last (sentinel state) prefix}) @ unique ->
      {r : insertion | r.#node.value = value && not r.#node.sentinel &&
        model r.#ring === append prefix (r.#node :: suffix) &&
        sentinel r.#ring === sentinel state &&
        not (H.mem (heap state) r.#node.prev) && not (H.mem (heap state) r.#node.next) &&
        heap r.#ring === inserted (heap state) left r.#node
          (head suffix (sentinel state))} @ unique

  val remove : (prefix : node list) @ immutable ghost -> (n : node) @ immutable ->
      (suffix : node list) @ immutable ghost ->
      (state : {state : t | model state === append prefix (n :: suffix)}) @ unique ->
      {next : t | model next === append prefix suffix &&
        sentinel next === sentinel state &&
        heap next === removed (heap state) (last (sentinel state) prefix) n
          (head suffix (sentinel state))} @ unique

end

(** Facts about the ring predicates, shared with [Pref_ring_splice_general].
    [chain h ns] states that consecutive nodes of [ns] are linked both ways. *)
module Proofs : sig
  val member : node @ immutable -> node list @ immutable -> bool @ ghost @@ total
  val member_def : (n : node) @ immutable -> (ns : node list) @ immutable ->
    {u : unit | member n ns === (ghost_ (match ns with [] -> false
      | x :: rest -> n === x || member n rest))} @@ total
  val apart_lists : node list @ immutable -> node list @ immutable -> bool @ ghost @@ total
  val apart_lists_def : (xs : node list) @ immutable -> (ys : node list) @ immutable ->
    {u : unit | apart_lists xs ys === (ghost_ (match xs with [] -> true
      | x :: rest -> apart_all x ys && apart_lists rest ys))} @@ total
  val chain : node option Pref.heap @ immutable -> node list @ immutable -> bool @ ghost @@ total
  val chain_def : (h : node option Pref.heap) @ immutable -> (ns : node list) @ immutable ->
    {u : unit | chain h ns === (ghost_ (match ns with [] -> true | n :: rest ->
      present h n &&
      (match rest with [] -> true | next :: _ ->
        H.at h n.next === Some (Some next) && H.at h next.prev === Some (Some n)) &&
      chain h rest))} @@ total
  val ordinary : node list @ immutable -> bool @ ghost @@ total
  val ordinary_def : (ns : node list) @ immutable ->
    {u : unit | ordinary ns === (ghost_ (match ns with [] -> true
      | n :: rest -> not n.sentinel && ordinary rest))} @@ total
  val safe_cell : node list @ immutable -> node option Pref.t @ immutable -> bool @ ghost @@ total
  val safe_cell_def : (ns : node list) @ immutable -> (p : node option Pref.t) @ immutable ->
    {u : unit | safe_cell ns p === (ghost_ (match ns with [] -> true | n :: rest ->
      (match rest with [] -> true | next :: _ ->
        not (p === n.next) && not (p === next.prev)) && safe_cell rest p))} @@ total
  val apart_append : (n : node) @ immutable ->
    (xs : node list) @ immutable -> (ys : node list) @ immutable ->
    {u : unit | apart_all n (append xs ys) =
      (apart_all n xs && apart_all n ys)} @ ghost @@ total
  val last_append : (fallback : node) @ immutable ->
    (xs : node list) @ immutable -> (ys : node list) @ immutable ->
    {u : unit | last fallback (append xs ys) === last (last fallback xs) ys} @ ghost @@ total
  val last_member : (fallback : node) @ immutable ->
    (ns : node list) @ immutable ->
    {u : unit | match ns with [] -> last fallback ns === fallback
      | _ :: _ -> member (last fallback ns) ns} @ ghost @@ total
  val apart_member : (n : node) @ immutable ->
    (xs : node list) @ immutable -> (x : node) @ immutable ->
    {u : unit | if apart_all n xs && member x xs then apart n x else true} @ ghost @@ total
  val apart_lists_member : (xs : node list) @ immutable ->
    (ys : node list) @ immutable -> (x : node) @ immutable ->
    {u : unit | if apart_lists xs ys && member x xs then apart_all x ys else true} @ ghost @@ total
  val separated_append : (xs : node list) @ immutable ->
    (ys : node list) @ immutable ->
    {u : unit | separated (append xs ys) =
      (separated xs && separated ys && apart_lists xs ys)} @ ghost @@ total
  val chain_split : (h : node option Pref.heap) @ immutable ->
    (xs : node list) @ immutable -> (ys : node list) @ immutable ->
    {u : unit | not (chain h (append xs ys)) || (chain h xs && chain h ys)} @ ghost @@ total
  val chain_join : (h : node option Pref.heap) @ immutable ->
    (fallback : node) @ immutable -> (xs : node list) @ immutable ->
    (ys : node list) @ immutable ->
    {u : unit | if chain h xs && chain h ys &&
      (match xs, ys with [], _ | _, [] -> true | _, y :: _ ->
        H.at h (last fallback xs).next === Some (Some y) &&
        H.at h y.prev === Some (Some (last fallback xs)))
      then chain h (append xs ys) else true} @ ghost @@ total
  val linked_chain : (h : node option Pref.heap) @ immutable ->
    (previous : node) @ immutable -> (ns : node list) @ immutable ->
    (stop : node) @ immutable ->
    {u : unit | if linked h previous ns stop && present h previous &&
      H.at h previous.next === Some (Some (head ns stop)) then
      chain h (previous :: ns) && ordinary ns &&
      H.at h (last previous ns).next === Some (Some stop) &&
      H.at h stop.prev === Some (Some (last previous ns)) else true} @ ghost @@ total
  val chain_linked : (h : node option Pref.heap) @ immutable ->
    (previous : node) @ immutable -> (ns : node list) @ immutable ->
    (stop : node) @ immutable ->
    {u : unit | if chain h (previous :: ns) && ordinary ns &&
      H.at h (last previous ns).next === Some (Some stop) &&
      H.at h stop.prev === Some (Some (last previous ns)) then
      linked h previous ns stop && H.at h previous.next === Some (Some (head ns stop))
      else true} @ ghost @@ total
  val separated_pair : (ns : node list) @ immutable ->
    (x : node) @ immutable -> (y : node) @ immutable ->
    {u : unit | if separated ns && member x ns && member y ns then
      x === y || apart x y else true} @ ghost @@ total
  val member_present : (h : node option Pref.heap) @ immutable ->
    (ns : node list) @ immutable -> (x : node) @ immutable ->
    {u : unit | if chain h ns && member x ns then present h x else true} @ ghost @@ total
  val safe_external : (ns : node list) @ immutable ->
    (n : node) @ immutable ->
    {u : unit | if apart_all n ns then safe_cell ns n.prev && safe_cell ns n.next
      else true} @ ghost @@ total
  val safe_head : (n : node) @ immutable -> (rest : node list) @ immutable ->
    {u : unit | if separated (n :: rest) then safe_cell (n :: rest) n.prev else true}
    @ ghost @@ total
  val safe_last : (fallback : node) @ immutable ->
    (ns : node list) @ immutable ->
    {u : unit | not (separated ns) || safe_cell ns (last fallback ns).next} @ ghost @@ total
  val chain_put : (h : node option Pref.heap) @ immutable ->
    (ns : node list) @ immutable -> (p : node option Pref.t) @ immutable ->
    (v : node option) @ immutable ->
    {u : unit | if chain h ns && safe_cell ns p then chain (H.put h p v) ns
      else true} @ ghost @@ total
  val apart_lists_right : (xs : node list) @ immutable ->
    (ys : node list) @ immutable -> (n : node) @ immutable ->
    {u : unit | if apart_lists xs ys && member n ys then apart_all n xs else true} @ ghost @@ total
  val apart_lists_append_right : (xs : node list) @ immutable ->
    (ys : node list) @ immutable -> (zs : node list) @ immutable ->
    {u : unit | apart_lists xs (append ys zs) =
      (apart_lists xs ys && apart_lists xs zs)} @ ghost @@ total
  val apart_lists_append_left : (xs : node list) @ immutable ->
    (ys : node list) @ immutable -> (zs : node list) @ immutable ->
    {u : unit | apart_lists (append xs ys) zs =
      (apart_lists xs zs && apart_lists ys zs)} @ ghost @@ total
  val apart_lists_symmetric : (xs : node list) @ immutable ->
    (ys : node list) @ immutable ->
    {u : unit | not (apart_lists xs ys) || apart_lists ys xs} @ ghost @@ total
  val ordinary_append : (xs : node list) @ immutable ->
    (ys : node list) @ immutable ->
    {u : unit | ordinary (append xs ys) = (ordinary xs && ordinary ys)} @ ghost @@ total
  val last_nonempty : (a : node) @ immutable ->
    (b : node) @ immutable -> (ns : node list) @ immutable ->
    {u : unit | match ns with [] -> true | _ :: _ -> last a ns === last b ns}
    @ ghost @@ total
end
