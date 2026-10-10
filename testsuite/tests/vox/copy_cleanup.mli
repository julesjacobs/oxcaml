open Copy_spec
open Generalize_spec
open Copy_cleanup_spec

val clear :
  (h : node Pref.heap Ghost.t) @ immutable ->
    (trail : pool) @ immutable ->
    (members : (((x : node Pref.t) @ immutable ->
      {u : unit | not (listed trail x) || H.mem h.Ghost.ghost x})) Ghost.t) @ total ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost}) @ unique ->
    {t : node Pref.token | Pref.own t === swept h.Ghost.ghost trail} @ unique
