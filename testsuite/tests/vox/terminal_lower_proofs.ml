open Copy_spec
open Level_spec
open Effective_lower_spec
module T = Terminal_lower_spec
module E = Effective_level
module U = Level_unifier_spec

let rec (valid @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (bound : int) -> (d : lowering) @ immutable ->
    {u : unit | effective_lower_valid h heads bound d} -> {u : unit | T.terminal_valid h bound d} @ ghost =
  fun h heads bound d premise -> ghost_ (
    effective_lower_valid_def h heads bound d;
    T.terminal_valid_def h bound d; match d with
    | Keep -> ()
    | Lower (_, _, rest) -> valid h heads bound rest (); ()
    | Sequence (a, b) -> valid h heads bound a ();
      let mid = lower_heap h bound a in valid mid heads bound b (); ())

let rec (bounded @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (witness : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x})) @ total ->
    (bound : int) -> (tree : Level_spec.bounded) @ immutable ->
    {u : unit | effective_bounded h heads bound tree} -> {u : unit | T.terminal_bounded h bound tree} @ ghost =
  fun h heads witness bound tree premise -> ghost_ (
    effective_bounded_def h heads bound tree;
    T.terminal_bounded_def h bound tree; bound_root_def tree; let p = bound_root tree in
    E.effective_below_def h heads p bound; below_def h p bound;
    match tree with
    | Tip _ -> witness p; U.terminal_def h p; U.observe_def h p;
      E.terminal_level h heads p (); ()
    | Through (_, child) -> bounded h heads witness bound child ();
      (match H.at h p with Some {desc = List _; _} -> witness p;
        U.terminal_def h p; U.observe_def h p; E.terminal_level h heads p (); () | _ -> ()); ()
    | Fork (_, a, b) -> witness p; U.terminal_def h p; U.observe_def h p;
      E.terminal_level h heads p ();
      bounded h heads witness bound a (); bounded h heads witness bound b (); ())

let rec (effective_bounded @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (witness : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x})) @ total ->
    (bound : int) -> (tree : Level_spec.bounded) @ immutable ->
    {u : unit | T.terminal_bounded h bound tree} -> {u : unit | Effective_lower_spec.effective_bounded h heads bound tree} @ ghost =
  fun h heads witness bound tree premise -> ghost_ (
    effective_bounded_def h heads bound tree;
    T.terminal_bounded_def h bound tree; bound_root_def tree; let p = bound_root tree in
    witness p; E.effective_below_def h heads p bound; below_def h p bound;
    match tree with
    | Tip _ -> U.terminal_def h p; U.observe_def h p;
      E.terminal_level h heads p (); ()
    | Through (_, child) -> effective_bounded h heads witness bound child ();
      let q = bound_root child in witness q;
      Effective_lower_spec.effective_bounded_def h heads bound child; E.effective_below_def h heads q bound;
      U.observe_def h p;
      (match H.at h p with Some {desc = Link _; _} -> E.link_level h heads p q (); ()
      | _ -> U.terminal_def h p; E.terminal_level h heads p (); ()); ()
    | Fork (_, a, b) -> U.terminal_def h p; U.observe_def h p;
      E.terminal_level h heads p ();
      effective_bounded h heads witness bound a (); effective_bounded h heads witness bound b (); ())

open Level_proofs
let rec (lowering_at @ total) : (h : node Pref.heap) @ immutable -> (bound : int) ->
    (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | T.terminal_valid h bound d} -> {u : unit | lower_frame h (lower_heap h bound d) x} @ ghost =
  fun h bound d x premise -> ghost_ (
    T.terminal_valid_def h bound d; lower_heap_def h bound d;
    match d with Keep -> frame_refl h x; ()
    | Lower (p, old, rest) ->
      lowering_at h bound rest x (); let mid = lower_heap h bound rest in
      write_frame mid p old bound x (); let after = lower_heap h bound d in
      frame_trans h mid after x (); ()
    | Sequence (a, b) ->
      lowering_at h bound a x (); let mid = lower_heap h bound a in
      lowering_at mid bound b x (); let after = lower_heap h bound d in
      frame_trans h mid after x (); ())
let rec (lower_fixed @ total) : (h : node Pref.heap) @ immutable -> (bound : int) ->
    (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | T.terminal_valid h bound d && (below h x bound || at_level h x === Generic)} ->
    {u : unit | H.at (lower_heap h bound d) x === H.at h x} @ ghost = fun h bound d x premise -> ghost_ (
  T.terminal_valid_def h bound d; lower_heap_def h bound d;
  below_def h x bound; at_level_def h x;
  match d with Keep -> ()
  | Lower (p, old, rest) ->
    lower_fixed h bound rest x (); let mid = lower_heap h bound rest in
    lower_cell_def old bound; let v = lower_cell old bound in Copy_heap_proofs.put_frame mid p v x; ()
  | Sequence (a, b) -> lower_fixed h bound a x (); let mid = lower_heap h bound a in
    lowering_at h bound a x (); lower_frame_def h mid x; below_def mid x bound; at_level_def mid x;
    lower_fixed mid bound b x (); ())

let (lower_idempotent @ total) : (old : node) @ immutable -> (bound : int) ->
    {u : unit | lower_cell (lower_cell old bound) bound === lower_cell old bound} @ ghost =
  fun old bound -> ghost_ (
    lower_cell_def old bound; let v = lower_cell old bound in lower_cell_def v bound;
    ())

let rec (lower_exact @ total) : (h : node Pref.heap) @ immutable -> (bound : int) ->
    (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | T.terminal_valid h bound d} ->
    {u : unit | let after = lower_heap h bound d in
      match H.at h x with None -> H.at after x === None | Some v ->
        H.at after x === Some v || H.at after x === Some (lower_cell v bound)} @ ghost =
  fun h bound d x premise -> ghost_ (
    T.terminal_valid_def h bound d; lower_heap_def h bound d;
    match d with
    | Keep -> ()
    | Lower (p, old, rest) -> lower_exact h bound rest x ();
      let mid = lower_heap h bound rest in let v = lower_cell old bound in Copy_heap_proofs.put_frame mid p v x;
      (match H.at h x with None -> () | Some original -> lower_idempotent original bound; ()); ()
    | Sequence (a, b) -> lower_exact h bound a x ();
      let mid = lower_heap h bound a in lower_exact mid bound b x ();
      (match H.at h x with None -> () | Some original -> lower_idempotent original bound; ()); ())

let rec (bounded_node @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (bound : int) -> (tree : Level_spec.bounded) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | Effective_lower_spec.effective_bounded h heads bound tree && Lower_locality_spec.contains tree x} ->
    {u : unit | E.effective_below h heads x bound && match H.at h x with None -> false | Some v ->
      Effective_lower_spec.effective_children_below h heads v.desc bound} @ ghost =
  fun h heads bound tree x premise -> ghost_ (
    Effective_lower_spec.effective_bounded_def h heads bound tree;
    Lower_locality_spec.contains_def tree x; bound_root_def tree;
    let p = bound_root tree in if p === x then (
      (match tree with
      | Tip _ -> (match H.at h x with None -> () | Some v -> Effective_lower_spec.effective_children_below_def h heads v.desc bound; ())
      | Through (_, child) ->
        Effective_lower_spec.effective_bounded_def h heads bound child;
        let desc = Link (bound_root child) in Effective_lower_spec.effective_children_below_def h heads desc bound;
        let desc = List (bound_root child) in Effective_lower_spec.effective_children_below_def h heads desc bound; ()
      | Fork (_, a, b) ->
        Effective_lower_spec.effective_bounded_def h heads bound a;
        Effective_lower_spec.effective_bounded_def h heads bound b;
        let desc = Arrow (bound_root a, bound_root b) in Effective_lower_spec.effective_children_below_def h heads desc bound; ());
      ())
    else (match tree with Tip _ -> ()
      | Through (_, child) -> bounded_node h heads bound child x (); ()
      | Fork (_, a, b) -> if Lower_locality_spec.contains a x then (bounded_node h heads bound a x (); ())
        else (bounded_node h heads bound b x (); ())))

let (completed_ordered @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (witness : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x})) @ total ->
    (bound : int) -> (root : node Pref.t) @ immutable -> (after : node Pref.heap) @ immutable ->
    (d : lowering) @ immutable -> (tree : Level_spec.bounded) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | T.completed h bound root after d tree && E.effective_ordered h heads x} ->
    {u : unit | E.effective_ordered after heads x} @ ghost =
  fun h heads witness bound root after d tree x premise -> ghost_ (
    T.completed_def h bound root after d tree; T.terminal_valid_def h bound d;
    let frame : ((y : node Pref.t) @ immutable -> {u : unit | lower_frame h after y}) @ total = fun y ->
      lowering_at h bound d y (); () in
    let next : ((y : node Pref.t) @ immutable -> {u : unit | E.valid_head after heads y}) @ total = fun y ->
      witness y; Effective_lower_proofs.frame_head h after heads frame y (); () in
    effective_bounded after heads next bound tree ();
    lower_exact h bound d x (); frame x; lower_frame_def h after x;
    E.effective_ordered_def h heads x; E.effective_ordered_def after heads x;
    if not (H.at h x === H.at after x) then (
      if not (Lower_locality_spec.contains tree x) then (
        Lower_locality_proofs.confined_frame h bound d tree x (); ()) else ();
      bounded_node after heads bound tree x (); ()) else ();
    match H.at h x with None -> () | Some old ->
      lower_cell_def old bound;
      match old.desc, old.level with
      | (Arrow _ | List _), Finite n ->
        Effective_lower_spec.effective_children_below_def h heads old.desc n;
        Effective_lower_proofs.children_frame h after heads frame old.desc n ();
        Effective_lower_spec.effective_children_below_def after heads old.desc n;
        Effective_lower_spec.effective_children_below_def after heads old.desc bound;
        ()
      | _ -> ())

let rec (links_unchanged @ total) : (h : node Pref.heap) @ immutable -> (bound : int) -> (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | T.terminal_valid h bound d && (match H.at h x with Some {desc = Link _; _} -> true | _ -> false)} ->
    {u : unit | H.at (lower_heap h bound d) x === H.at h x} @ ghost =
  fun h bound d x premise -> ghost_ (
    T.terminal_valid_def h bound d;
    lower_heap_def h bound d; match d with
    | Keep -> ()
    | Lower (p, old, rest) -> links_unchanged h bound rest x ();
      let mid = lower_heap h bound rest in let v = lower_cell old bound in
      Copy_heap_proofs.put_frame mid p v x; ()
    | Sequence (a, b) -> links_unchanged h bound a x ();
      let mid = lower_heap h bound a in links_unchanged mid bound b x (); ())


let (effective_fixed @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (bound : int) -> (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | T.terminal_valid h bound d && H.mem h x && E.valid_head h heads x
      && (E.effective_below h heads x bound || E.level h heads x === Generic)} ->
    {u : unit | H.at (lower_heap h bound d) x === H.at h x} @ ghost =
  fun h heads bound d x premise -> ghost_ (
    if U.terminal h x then (
      E.terminal_level h heads x ();
      E.effective_below_def h heads x bound; below_def h x bound;
      lower_fixed h bound d x (); ())
    else (
      U.terminal_def h x; U.observe_def h x;
      match H.at h x with None -> at_level_def h x; lower_fixed h bound d x (); ()
      | Some v -> links_unchanged h bound d x (); ()))
