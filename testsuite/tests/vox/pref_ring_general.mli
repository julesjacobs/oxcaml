open Pref_ring

val append : node list @ immutable -> node list @ immutable -> node list @ ghost @@ total
val append_def : (xs : node list) @ immutable -> (ys : node list) @ immutable ->
  {u : unit | append xs ys === (ghost_ (match xs with
    | [] -> ys | x :: rest -> x :: append rest ys))} @@ total
val reversed : node list @ immutable -> node list @ ghost @@ total
val reversed_def : (xs : node list) @ immutable ->
  {u : unit | reversed xs === (ghost_ (match xs with
    | [] -> [] | x :: rest -> append (reversed rest) [x]))} @@ total

val reverse : (sentinel : node) @ immutable ->
    (ns : node list) @ immutable ghost ->
    (t : {t : node option Pref.token | ring (Pref.own t) sentinel ns &&
      separated (sentinel :: ns)}) @ unique ->
    {r : node option Pref.token | Pref.own r === flipped_all (Pref.own t) (sentinel :: ns) &&
      ring (Pref.own r) sentinel (reversed ns) && separated (sentinel :: reversed ns)}
      @ unique

type built = { sentinel : node @@ aliased; nodes : node list @@ aliased ghost;
  state : node option Pref.token }

module Owned : sig
  type t : value & void & void
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

end
