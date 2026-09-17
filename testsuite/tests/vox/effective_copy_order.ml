open Copy_spec
open Copy_heap_proofs
open Effective_copy_spec
open Effective_copy_heap_proofs
open Effective_copy_metadata
module E = Effective_level
module U = Level_unifier_spec
module R = Representative_level

let (saved_level @ total) : (saved : Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (next : E.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && H.mem saved x
      && E.valid_head saved heads x && E.valid_head (heap saved epoch depth d) next x} ->
    {u : unit | E.level (heap saved epoch depth d) next x === E.level saved heads x} @ ghost =
  fun saved heads next epoch depth d x premise -> ghost_ (
    let refine_ premise = premise in let u = () in let after = heap saved epoch depth d in
    let frame : ((y : node Pref.t) @ immutable -> {u : unit | not (H.mem saved y)
      || (H.mem after y && U.observe saved y === U.observe after y)} ) @ total = fun y ->
      let u = () in history_at saved heads epoch depth d y (refine_ u);
      U.observe_def saved y; U.observe_def after y; refine_ u in
    E.valid_head_def saved heads x; E.valid_head_def after next x;
    E.level_def saved heads x; E.level_def after next x;
    let before_root = heads x in let after_root = next x in
    E.head_terminal saved heads x (refine_ u);
    resolution_grows saved after frame x before_root.root before_root.path (refine_ u);
    history_at saved heads epoch depth d x (refine_ u);
    R.unique after x before_root.root before_root.path after_root.root after_root.path (refine_ u);
    history_at saved heads epoch depth d before_root.root (refine_ u);
    Level_spec.at_level_def saved before_root.root; Level_spec.at_level_def after before_root.root;
    refine_ u)

let (target_below @ total) : (saved : Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (next : E.heads) @ total ->
    (witness : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head saved heads x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (final : history) @ immutable ->
    (valid_next : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head (heap saved epoch depth final) next x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && effective_valid saved heads epoch depth final
      && extends d final && effective_target_for saved heads d p q && depth >= 0
      && (E.level saved heads p === Generic || E.effective_below saved heads p depth)} ->
    {u : unit | E.effective_below (heap saved epoch depth final) next q depth} @ ghost =
  fun saved heads next witness epoch depth d final valid_next p q premise -> ghost_ (
    let refine_ premise = premise in let u = () in let after = heap saved epoch depth final in
    target_preserved saved heads epoch depth d final p q (refine_ u);
    target_allocated saved heads epoch depth final p q (refine_ u);
    effective_target_for_def saved heads d p q; valid_next q;
    E.effective_below_def saved heads p depth; E.effective_below_def after next q depth;
    match E.level saved heads p with
    | Finite _ -> witness p; saved_level saved heads next epoch depth final p (refine_ u); refine_ u
    | Generic -> mapped_fresh saved heads witness epoch depth d p q (refine_ u);
      fresh_terminal saved heads epoch depth final q (refine_ u);
      E.terminal_level after next q (refine_ u); refine_ u)

let (saved_below @ total) : (saved : Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (next : E.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> (bound : int) ->
    {u : unit | effective_valid saved heads epoch depth d && E.effective_below saved heads x bound
      && E.valid_head saved heads x && E.valid_head (heap saved epoch depth d) next x} ->
    {u : unit | E.effective_below (heap saved epoch depth d) next x bound} @ ghost =
  fun saved heads next epoch depth d x bound premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    E.effective_below_def saved heads x bound;
    saved_level saved heads next epoch depth d x (refine_ u);
    history_grows saved heads epoch depth d x (refine_ u);
    let after = heap saved epoch depth d in E.effective_below_def after next x bound; refine_ u)

let (saved_ordered @ total) : (saved : Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (next : E.heads) @ total ->
    (witness : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head saved heads x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (valid_next : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head (heap saved epoch depth d) next x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && H.mem saved x && E.effective_ordered saved heads x} ->
    {u : unit | E.effective_ordered (heap saved epoch depth d) next x} @ ghost =
  fun saved heads next witness epoch depth d valid_next x premise -> ghost_ (
    let refine_ premise = premise in let u = () in let after = heap saved epoch depth d in
    history_at saved heads epoch depth d x (refine_ u);
    E.effective_ordered_def saved heads x; E.effective_ordered_def after next x;
    match H.at saved x with None -> refine_ u | Some v ->
      match v.desc, v.level with
      | Arrow (a, b), Finite n -> witness a; witness b; valid_next a; valid_next b;
        saved_below saved heads next epoch depth d a n (refine_ u);
        saved_below saved heads next epoch depth d b n (refine_ u); refine_ u
      | _ -> refine_ u)

let rec (fresh_ordered @ total) : (saved : Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (next : E.heads) @ total ->
    (witness : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head saved heads x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (bounds : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x)
      || E.level saved heads x === Generic || E.effective_below saved heads x depth})) @ total ->
    (d : history) @ immutable -> (final : history) @ immutable ->
    (valid_next : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head (heap saved epoch depth final) next x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && effective_valid saved heads epoch depth final
      && extends d final && H.mem (heap saved epoch depth d) x && not (H.mem saved x) && depth >= 0} ->
    {u : unit | E.effective_ordered (heap saved epoch depth final) next x} @ ghost =
  fun saved heads next witness epoch depth bounds d final valid_next x premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    effective_valid_def saved heads epoch depth d; heap_def saved epoch depth d;
    let after = heap saved epoch depth final in
    fresh_frame saved heads epoch depth d final x (refine_ u);
    E.effective_ordered_def after next x;
    match d with
    | Clean -> refine_ u
    | Start -> let desc = Bool in let v = cell desc depth in cell_def desc depth;
      put_frame saved epoch v x; refine_ u
    | Fresh (rest, p, q, old, desc) ->
      let mid = heap saved epoch depth rest in let v = cell desc depth in cell_def desc depth;
      put_frame mid q v x; let h1 = H.put mid q v in let w = session_mark rest old epoch q in
      put_frame h1 p w x;
      extends_def rest d; extends_def rest rest; extension_trans rest d final (refine_ u);
      if x === q then (
        effective_ready_def saved heads rest old.desc desc;
        match old.desc, desc with
        | Arrow (a, b), Arrow (c, e) -> bounds a; bounds b;
          effective_target_for_def saved heads rest a c; effective_target_for_def saved heads rest b e;
          target_below saved heads next witness epoch depth rest final valid_next a c (refine_ u);
          target_below saved heads next witness epoch depth rest final valid_next b e (refine_ u); refine_ u
        | _ -> refine_ u)
      else (fresh_ordered saved heads next witness epoch depth bounds rest final valid_next x (refine_ u); refine_ u)
    | Alias (rest, p, q, old) ->
      let mid = heap saved epoch depth rest in let w = session_mark rest old epoch q in
      put_frame mid p w x;
      extends_def rest d; extends_def rest rest; extension_trans rest d final (refine_ u);
      fresh_ordered saved heads next witness epoch depth bounds rest final valid_next x (refine_ u); refine_ u)

let (copy_ordered @ total) : (saved : Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem saved x then source_ok saved x else H.at saved x === None})) @ total ->
    (heads : E.heads) @ total -> (next : E.heads) @ total ->
    (witness : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head saved heads x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (bounds : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x)
      || E.level saved heads x === Generic || E.effective_below saved heads x depth})) @ total ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered saved heads x})) @ total ->
    (d : history) @ immutable ->
    (valid_next : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head (heap saved epoch depth d) next x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && depth >= 0} ->
    {u : unit | E.effective_ordered (heap saved epoch depth d) next x} @ ghost =
  fun saved scope heads next witness epoch depth bounds order d valid_next x premise -> ghost_ (
    let refine_ premise = premise in let u = () in let after = heap saved epoch depth d in
    if H.mem saved x then (
      order x; saved_ordered saved heads next witness epoch depth d valid_next x (refine_ u); refine_ u)
    else if H.mem after x then (
      extends_def d d;
      fresh_ordered saved heads next witness epoch depth bounds d d valid_next x (refine_ u); refine_ u)
    else (history_scope saved heads scope epoch depth d x (refine_ u); E.effective_ordered_def after next x; refine_ u))

let (copy_bounds @ total) : (saved : Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (next : E.heads) @ total ->
    (witness : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head saved heads x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (bounds : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x)
      || E.level saved heads x === Generic || E.effective_below saved heads x depth})) @ total ->
    (d : history) @ immutable ->
    (valid_next : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head (heap saved epoch depth d) next x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && depth >= 0} ->
    {u : unit | let after = heap saved epoch depth d in not (H.mem after x)
      || E.level after next x === Generic || E.effective_below after next x depth} @ ghost =
  fun saved heads next witness epoch depth bounds d valid_next x premise -> ghost_ (
    let refine_ premise = premise in let u = () in let after = heap saved epoch depth d in
    E.effective_below_def after next x depth;
    if H.mem saved x then (
      witness x; valid_next x; bounds x;
      saved_level saved heads next epoch depth d x (refine_ u);
      E.effective_below_def saved heads x depth; refine_ u)
    else if H.mem after x then (
      valid_next x; fresh_terminal saved heads epoch depth d x (refine_ u);
      E.terminal_level after next x (refine_ u); refine_ u)
    else refine_ u)

let (sweep_level @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (trail : Generalize_spec.pool) @ immutable ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | Copy_cleanup_spec.swept_at h after trail x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | E.level h heads x === E.level after heads x
      && H.mem h x === H.mem after x} @ ghost =
  fun h after heads trail frame x -> ghost_ (
    frame x; Copy_cleanup_spec.swept_at_def h after trail x;
    E.level_def h heads x; E.level_def after heads x;
    let r = heads x in frame r.root; Copy_cleanup_spec.swept_at_def h after trail r.root;
    Level_spec.at_level_def h r.root; Level_spec.at_level_def after r.root;
    let u = () in refine_ u)

let (sweep_below @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (trail : Generalize_spec.pool) @ immutable ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | Copy_cleanup_spec.swept_at h after trail x})) @ total ->
    (x : node Pref.t) @ immutable -> (bound : int) ->
    {u : unit | E.effective_below h heads x bound === E.effective_below after heads x bound} @ ghost =
  fun h after heads trail frame x bound -> ghost_ (
    sweep_level h after heads trail frame x;
    E.effective_below_def h heads x bound; E.effective_below_def after heads x bound;
    let u = () in refine_ u)

let (sweep_ordered @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (trail : Generalize_spec.pool) @ immutable ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | Copy_cleanup_spec.swept_at h after trail x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | E.effective_ordered h heads x === E.effective_ordered after heads x} @ ghost =
  fun h after heads trail frame x -> ghost_ (
    frame x; Copy_cleanup_spec.swept_at_def h after trail x;
    E.effective_ordered_def h heads x; E.effective_ordered_def after heads x;
    let u = () in match H.at h x with
    | None -> refine_ u
    | Some v -> match v.desc, v.level with
      | Arrow (a, b), Finite n -> sweep_below h after heads trail frame a n;
        sweep_below h after heads trail frame b n; refine_ u
      | _ -> refine_ u)
