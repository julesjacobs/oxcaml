open Copy_spec
open Level_spec
open Level_unifier_spec
module E = Effective_level

let (below @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ->
    (x : node Pref.t) @ immutable -> (bound : int) ->
    {u : unit | E.valid_head h heads x && Level_spec.below h x bound} ->
    {u : unit | E.effective_below h heads x bound} @ ghost = fun h heads order x bound premise -> ghost_ (
      let refine_ premise = premise in E.valid_head_def h heads x;
      Level_spec.below_def h x bound; E.effective_below_def h heads x bound; E.level_def h heads x;
      let r = heads x in let u = () in Compression_proofs.resolution_below h order x r.root r.path bound (refine_ u);
      Level_spec.below_def h r.root bound; refine_ u)

let (order @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (valid : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x})) @ total ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered h heads x} @ ghost =
  fun h heads valid order x -> ghost_ (
    order x; ordered_def h x; E.effective_ordered_def h heads x;
    (match H.at h x with Some {desc = Arrow (a, b); level = Finite n; _} ->
      let desc = Arrow (a, b) in children_below_def h desc n;
      valid a; valid b; let u = () in below h heads order a n (refine_ u); below h heads order b n (refine_ u); ()
    | _ -> ()); let u = () in refine_ u)

let (active @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x && Level_spec.active h x} ->
    {u : unit | E.effective_active h heads x} @ ghost = fun h heads scope x premise -> ghost_ (
      let refine_ premise = premise in E.valid_head_def h heads x; active_def h x;
      E.effective_active_def h heads x; E.level_def h heads x;
      let r = heads x in let u = () in Level_unifier_metadata.resolution_active h scope x r.root r.path (refine_ u);
      active_def h r.root; refine_ u)
