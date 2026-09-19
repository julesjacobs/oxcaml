open Copy_spec
open Generalize_spec
open Copy_cleanup_spec

val clear :
  (h : Pref.heap Ghost.t) @ immutable ->
    (trail : pool) @ immutable ->
    (members : (((x : node Pref.t) @ immutable ->
      {u : unit | not (listed trail x) || H.mem h.Ghost.ghost x})) Ghost.t) @ total ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost}) @ unique ->
    {t : Pref.token | Pref.own t === swept h.Ghost.ghost trail} @ unique
