open Pref_ring

val splice_demo : (s : node) @ immutable -> (d : node) @ immutable -> (a : node)
    @ immutable -> (b : node) @ immutable -> (t : {t : node option Pref.token | s.sentinel && d.sentinel && not a.sentinel && not b.sentinel &&
      isolated (Pref.own t) [s; d; a; b]}) @ unique ->
    {r : node option Pref.token | ring (Pref.own r) s [] && ring (Pref.own r) d [a; b] &&
      path (Pref.own r) false [a; b] d && path (Pref.own r) true [b; a] d} @ unique
