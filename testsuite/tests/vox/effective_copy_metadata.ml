open Generalize_spec

open Copy_spec
open Copy_heap_proofs
open Copy_model_proofs
open Effective_copy_spec
open Effective_copy_heap_proofs

let rec (epoch_allocated @ total) : (saved : node Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total -> (epoch : node Pref.t) @ immutable ->
    (depth : int) -> (d : history) @ immutable -> {u : unit | effective_valid saved heads epoch depth d} ->
    {u : unit | H.mem (heap saved epoch depth d) epoch} @ ghost = fun saved heads epoch depth d premise -> ghost_ (
  effective_valid_def saved heads epoch depth d; heap_def saved epoch depth d;
  match d with
  | Clean -> ()
  | Start -> let v = cell Bool depth in put_frame saved epoch v epoch; ()
  | Fresh (rest, p, q, old, desc) ->
    epoch_allocated saved heads epoch depth rest ();
    let h = heap saved epoch depth rest in let v = cell desc depth in let h1 = H.put h q v in let w = session_mark rest old epoch q in session_mark_def rest old epoch q; put_frame h1 p w epoch; ()
  | Alias (rest, p, q, old) -> epoch_allocated saved heads epoch depth rest ();
    let h = heap saved epoch depth rest in let w = session_mark rest old epoch q in session_mark_def rest old epoch q; put_frame h p w epoch; ())

let (ready_scoped @ total) : (saved : node Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total -> (epoch : node Pref.t) @ immutable ->
    (depth : int) -> (d : history) @ immutable -> (source : desc) @ immutable -> (dest : desc) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && effective_ready saved heads d source dest} ->
    {u : unit | payload_scoped (heap saved epoch depth d) (cell dest depth)} @ ghost =
  fun saved heads epoch depth d source dest premise -> ghost_ (
    effective_ready_def saved heads d source dest;
    let h = heap saved epoch depth d in let v = cell dest depth in cell_def dest depth; payload_scoped_def h v;
    match source, dest with
    | Arrow (a, b), Arrow (x, y) -> target_allocated saved heads epoch depth d a x ();
      target_allocated saved heads epoch depth d b y (); ()
    | _ -> ())

let rec (history_scope @ total) : (saved : node Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit |
      if H.mem saved x then source_ok saved x else H.at saved x === None})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | effective_valid saved heads epoch depth d} ->
    {u : unit | if H.mem (heap saved epoch depth d) x then source_ok (heap saved epoch depth d) x
      else H.at (heap saved epoch depth d) x === None} @ ghost = fun saved heads scope epoch depth d x premise -> ghost_ (
  effective_valid_def saved heads epoch depth d; heap_def saved epoch depth d;
  match d with
  | Clean -> scope x; ()
  | Start -> let desc = Bool in let v = cell desc depth in cell_def desc depth; payload_scoped_def saved v;
    let () = put_scope saved scope epoch v x () in ()
  | Fresh (rest, p, q, old, desc) ->
    let h = heap saved epoch depth rest in
    let prior : (x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None}
        @ total = fun x -> let () = history_scope saved heads scope epoch depth rest x () in () in
    ready_scoped saved heads epoch depth rest old.desc desc ();
    let v = cell desc depth in let h1 = H.put h q v in
    let next : (x : node Pref.t) @ immutable -> {u : unit | if H.mem h1 x then source_ok h1 x else H.at h1 x === None}
        @ total = fun x -> let () = put_scope h prior q v x () in () in
    history_grows saved heads epoch depth rest p (); prior p; source_ok_def h p;
    epoch_allocated saved heads epoch depth rest ();
    let w = session_mark rest old epoch q in session_mark_def rest old epoch q; mark_def old epoch q; payload_scoped_def h1 w;
    let () = put_scope h1 next p w x () in ()
  | Alias (rest, p, q, old) ->
    let h = heap saved epoch depth rest in
    let prior : (x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None}
        @ total = fun x -> let () = history_scope saved heads scope epoch depth rest x () in () in
    history_grows saved heads epoch depth rest p (); prior p; source_ok_def h p;
    epoch_allocated saved heads epoch depth rest ();
    let w = session_mark rest old epoch q in session_mark_def rest old epoch q; mark_def old epoch q; payload_scoped_def h w;
    let () = put_scope h prior p w x () in ())


open Copy_cleanup_spec
open Copy_cleanup_proofs
open Pooled_spec
open Effective_copy_heap_proofs

let (result_at @ total) : (saved : node Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d} ->
    {u : unit | swept_at (heap saved epoch depth d)
      (swept (heap saved epoch depth d) (touched d)) (touched d) x} @ ghost =
  fun saved heads epoch depth d x premise -> ghost_ (
    let h = heap saved epoch depth d in let trail = touched d in
    let members : ((y : node Pref.t) @ immutable ->
      {u : unit | not (listed trail y) || H.mem h y}) @ total = fun y ->
      touched_saved saved heads epoch depth d y ();
      history_grows saved heads epoch depth d y (); () in
    let () = sweep_at h trail members x in ())

let (memo_released @ total) : (saved : node Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d} ->
    {u : unit | match mapping d x with None -> true | Some _ ->
      match H.at (swept (heap saved epoch depth d) (touched d)) x with
      | None -> false | Some v -> v.memo === Empty_memo} @ ghost =
  fun saved heads epoch depth d x premise -> ghost_ (
    result_at saved heads epoch depth d x ();
    touched_mapping d x; touched_saved saved heads epoch depth d x ();
    history_at saved heads epoch depth d x ();
    let h = heap saved epoch depth d in let trail = touched d in
    let after = swept h trail in swept_at_def h after trail x;
    ())

let (model_equivalence @ total) : (saved : node Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d} ->
    {u : unit | equation (heap saved epoch depth d) rho x ===
      equation (swept (heap saved epoch depth d) (touched d)) rho x} @ ghost =
  fun saved heads epoch depth d rho x premise -> ghost_ (
    result_at saved heads epoch depth d x ();
    let h = heap saved epoch depth d in let trail = touched d in
    let after = swept h trail in
    let () = sweep_model h after trail rho x () in
    ())

let (clean_result @ total) : (saved : node Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d &&
      match H.at saved x with None -> true | Some v -> v.memo === Empty_memo} ->
    {u : unit | match H.at (swept (heap saved epoch depth d) (touched d)) x with
      None -> true | Some v -> v.memo === Empty_memo} @ ghost =
  fun saved heads epoch depth d x premise -> ghost_ (
    history_clean saved heads epoch depth d x ();
    result_at saved heads epoch depth d x ();
    let h = heap saved epoch depth d in let trail = touched d in
    let after = swept h trail in swept_at_def h after trail x;
    ())

module U = Level_unifier_spec
module E = Effective_level
module R = Representative_level

let (saved_observe @ total) : (saved : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (epoch : node Pref.t) @ immutable ->
    (depth : int) -> (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && H.mem saved x} ->
    {u : unit | let after = swept (heap saved epoch depth d) (touched d) in
      H.mem after x && U.observe saved x === U.observe after x} @ ghost =
  fun saved heads epoch depth d x premise -> ghost_ (
    history_at saved heads epoch depth d x ();
    result_at saved heads epoch depth d x ();
    let h = heap saved epoch depth d in let trail = touched d in
    let after = swept h trail in swept_at_def h after trail x;
    U.observe_def saved x; U.observe_def after x; ())

let rec (resolution_grows @ total) : (before : node Pref.heap) @ immutable ->
    (after : node Pref.heap) @ immutable ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem before x)
      || (H.mem after x && U.observe before x === U.observe after x)})) @ total ->
    (p : node Pref.t) @ immutable -> (root : node Pref.t) @ immutable ->
    (path : U.resolution) @ immutable -> {u : unit | U.resolves before p root path} ->
    {u : unit | U.resolves after p root path} @ ghost =
  fun before after frame p root path premise -> ghost_ (
    frame p;
    U.resolves_def before p root path; U.resolves_def after p root path;
    match path with
    | U.Here -> U.terminal_def before p; U.terminal_def after p; ()
    | U.Via (q, rest) -> resolution_grows before after frame q root rest (); ())

let rec (fresh_terminal @ total) : (saved : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (epoch : node Pref.t) @ immutable ->
    (depth : int) -> (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && not (H.mem saved x)} ->
    {u : unit | not (H.mem (heap saved epoch depth d) x)
      || (U.terminal (heap saved epoch depth d) x && Level_spec.at_level (heap saved epoch depth d) x === Finite depth)} @ ghost =
  fun saved heads epoch depth d x premise -> ghost_ (
    effective_valid_def saved heads epoch depth d;
    heap_def saved epoch depth d; let after = heap saved epoch depth d in U.terminal_def after x; U.observe_def after x; Level_spec.at_level_def after x;
    match d with
    | Clean -> ()
    | Start -> let desc = Bool in let v = cell desc depth in
      cell_def desc depth; put_frame saved epoch v x; ()
    | Fresh (rest, p, q, old, desc) ->
      fresh_terminal saved heads epoch depth rest x ();
      let h = heap saved epoch depth rest in U.terminal_def h x; U.observe_def h x; Level_spec.at_level_def h x;
      let v = cell desc depth in cell_def desc depth; let h1 = H.put h q v in let w = session_mark rest old epoch q in
      put_frame h1 p w x; effective_ready_def saved heads rest old.desc desc; ()
    | Alias (rest, p, q, old) ->
      fresh_terminal saved heads epoch depth rest x ();
      let h = heap saved epoch depth rest in U.terminal_def h x; U.observe_def h x; Level_spec.at_level_def h x;
      let w = session_mark rest old epoch q in put_frame h p w x; ())

let (result_head @ total) : (saved : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (next : E.heads) @ total ->
    (witness : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head saved heads x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | effective_valid saved heads epoch depth d
      && (if H.mem saved x then next x === heads x else (next x).R.root === x && (next x).R.path === U.Here)} ->
    {u : unit | E.valid_head (swept (heap saved epoch depth d) (touched d)) next x} @ ghost =
  fun saved heads next witness epoch depth d x premise -> ghost_ (
    let h = heap saved epoch depth d in let trail = touched d in let after = swept h trail in
    E.valid_head_def after next x;
    if H.mem saved x then (
      witness x; E.valid_head_def saved heads x;
      let frame : ((y : node Pref.t) @ immutable -> {u : unit | not (H.mem saved y)
        || (H.mem after y && U.observe saved y === U.observe after y)} ) @ total = fun y ->
        if H.mem saved y then (saved_observe saved heads epoch depth d y (); ()) else (); () in
      let r = heads x in resolution_grows saved after frame x r.root r.path (); ())
    else (
      fresh_terminal saved heads epoch depth d x ();
      result_at saved heads epoch depth d x (); swept_at_def h after trail x;
      U.terminal_def h x; U.observe_def h x; U.terminal_def after x; U.observe_def after x;
      let here = U.Here in U.resolves_def after x x here; ()))

let (history_head @ total) : (saved : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (next : E.heads) @ total ->
    (witness : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head saved heads x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | effective_valid saved heads epoch depth d
      && (if H.mem saved x then next x === heads x else (next x).R.root === x && (next x).R.path === U.Here)} ->
    {u : unit | E.valid_head (heap saved epoch depth d) next x} @ ghost =
  fun saved heads next witness epoch depth d x premise -> ghost_ (
    let after = heap saved epoch depth d in
    E.valid_head_def after next x;
    if H.mem saved x then (
      witness x; E.valid_head_def saved heads x;
      let frame : ((y : node Pref.t) @ immutable -> {u : unit | not (H.mem saved y)
        || (H.mem after y && U.observe saved y === U.observe after y)} ) @ total = fun y ->
        history_at saved heads epoch depth d y (); U.observe_def saved y; U.observe_def after y; () in
      let r = heads x in resolution_grows saved after frame x r.root r.path (); ())
    else (
      fresh_terminal saved heads epoch depth d x ();
      U.terminal_def after x; U.observe_def after x;
      let here = U.Here in U.resolves_def after x x here; ()))
