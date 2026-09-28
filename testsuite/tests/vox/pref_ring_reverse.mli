open Pref_ring

val reverse_demo : (s : node) @ immutable -> (a : node) @ immutable -> (b :
    node) @ immutable -> (c : node) @ immutable -> (t : {t : node option Pref.token | s.sentinel && not a.sentinel && not b.sentinel && not c.sentinel &&
      isolated (Pref.own t) [s; a; b; c]}) @ unique ->
    {r : node option Pref.token | ring (Pref.own r) s [c; b; a] &&
      path (Pref.own r) false [c; b; a] s && path (Pref.own r) true [a; b; c] s} @ unique
