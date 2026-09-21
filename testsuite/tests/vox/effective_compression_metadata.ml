open Copy_spec
open Level_spec
open Level_unifier_spec
open Compression_spec
open Effective_compression_spec
module E = Effective_level
module R = Representative_level
module P = Effective_compression_proofs

let (head @ total) : (h : node Pref.heap) @ immutable ->
    (after : node Pref.heap) @ immutable -> (edits : edits) @ immutable ->
    (heads : E.heads) @ total -> (p : node Pref.t) @ immutable ->
    {u : unit | effective_rewritten h after edits && E.valid_head h heads p} ->
    {r : R.representative | r.root === (heads p).root
      && (not (H.mem after p) || resolves after p r.root r.path)} @ immutable ghost =
  fun h after edits heads p premise -> ghost_ (
    P.frame h after edits p ();
    E.valid_head_def h heads p; let old = heads p in
    if H.mem h p then (
      let path = P.resolution h after edits p old.root old.path () in
      let out = {R.root = old.root; path} in out)
    else old)

let (level @ total) : (h : node Pref.heap) @ immutable ->
    (after : node Pref.heap) @ immutable -> (edits : edits) @ immutable ->
    (before_heads : E.heads) @ total -> (after_heads : E.heads) @ total ->
    (p : node Pref.t) @ immutable ->
    {u : unit | effective_rewritten h after edits && E.valid_head h before_heads p
      && E.valid_head after after_heads p} ->
    {u : unit | E.level h before_heads p === E.level after after_heads p} @ ghost =
  fun h after edits before_heads after_heads p premise -> ghost_ (
    P.frame h after edits p ();
    E.level_def h before_heads p; E.level_def after after_heads p;
    E.valid_head_def after after_heads p;
    if H.mem h p then (
      let transported = head h after edits before_heads p () in
      let current = after_heads p in
      R.unique after p transported.root transported.path current.root current.path ();
      let old = before_heads p in P.frame h after edits old.root (); ()) else ();
    ())

let (ordered @ total) : (h : node Pref.heap) @ immutable ->
    (after : node Pref.heap) @ immutable -> (edits : edits) @ immutable ->
    (before_heads : E.heads) @ total -> (after_heads : E.heads) @ total ->
    (before_valid : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h before_heads x})) @ total ->
    (after_valid : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head after after_heads x})) @ total ->
    (p : node Pref.t) @ immutable ->
    {u : unit | effective_rewritten h after edits && E.effective_ordered h before_heads p} ->
    {u : unit | E.effective_ordered after after_heads p} @ ghost =
  fun h after edits before_heads after_heads before_valid after_valid p premise -> ghost_ (
    P.frame h after edits p ();
    E.effective_ordered_def h before_heads p; E.effective_ordered_def after after_heads p;
    (match H.at h p with
    | None -> ()
    | Some old -> match old.desc with
      | Var | Bool | Link _ -> ()
      | Arrow (a, b) -> match old.level with
        | Generic -> ()
        | Finite n ->
          before_valid a; before_valid b; after_valid a; after_valid b;
          level h after edits before_heads after_heads a ();
          level h after edits before_heads after_heads b ();
          P.frame h after edits a (); P.frame h after edits b ();
          E.effective_below_def h before_heads a n; E.effective_below_def h before_heads b n;
          E.effective_below_def after after_heads a n; E.effective_below_def after after_heads b n; ());
    ())
