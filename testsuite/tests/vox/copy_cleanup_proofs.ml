open Copy_spec
open Copy_heap_proofs
open Generalize_spec
open Pooled_spec
open Copy_cleanup_spec

let rec (sweep_at @ total) : (h : Pref.heap) @ immutable ->
    (trail : pool) @ immutable ->
    (members : ((x : node Pref.t) @ immutable ->
      {u : unit | not (listed trail x) || H.mem h x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | swept_at h (swept h trail) trail x} @ ghost =
  fun h trail members x -> ghost_ (
    swept_def h trail; listed_def trail x;
    match trail with
    | Empty -> swept_at_def h h trail x; ()
    | Entry (p, rest) ->
      listed_def trail p; members p;
      (match H.at h p with
      | None ->
        let tail : ((y : node Pref.t) @ immutable ->
          {u : unit | not (listed rest y) || H.mem h y}) @ total = fun y ->
          listed_def trail y; members y; () in
        sweep_at h rest tail x;
        let after = swept h rest in swept_at_def h after rest x;
        let after = swept h trail in swept_at_def h after trail x;
        members x; ()
      | Some old ->
        let v = clear_memo old in clear_memo_def old;
        let mid = H.put h p v in let tail : ((y : node Pref.t) @ immutable ->
          {u : unit | not (listed rest y) || H.mem mid y}) @ total = fun y ->
          listed_def trail y; members y; () in
        sweep_at mid rest tail x;
        let after = swept mid rest in swept_at_def mid after rest x;
        let after = swept h trail in swept_at_def h after trail x;
        ()))

let rec (touched_mapping @ total) : (d : history) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | listed (touched d) x ===
      (match mapping d x with None -> false | Some _ -> true)} @ ghost =
  fun d x -> ghost_ (
    touched_def d; mapping_def d x; let trail = touched d in listed_def trail x;
    match d with Start | Clean -> ()
    | Fresh (rest, _, _, _, _) | Alias (rest, _, _, _) ->
      touched_mapping rest x; ())

let rec (touched_saved @ total) : (saved : Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth d} ->
    {u : unit | not (listed (touched d) x) || (H.mem saved x
      && match H.at saved x with None -> false | Some _ -> true)} @ ghost =
  fun saved epoch depth d x premise -> ghost_ (
    valid_def saved epoch depth d;
    touched_def d; let trail = touched d in listed_def trail x;
    match d with Start | Clean -> ()
    | Fresh (rest, _, _, _, _) | Alias (rest, _, _, _) ->
      touched_saved saved epoch depth rest x ();
      history_at saved epoch depth rest x (); ())

let (sweep_model @ total) : (h : Pref.heap) @ immutable ->
    (after : Pref.heap) @ immutable -> (trail : pool) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | swept_at h after trail x} ->
    {u : unit | equation h rho x === equation after rho x} @ ghost =
  fun h after trail rho x premise -> ghost_ (
    swept_at_def h after trail x;
    equation_def h rho x; equation_def after rho x;
    ())

let (sweep_scope @ total) : (h : Pref.heap) @ immutable ->
    (after : Pref.heap) @ immutable -> (trail : pool) @ immutable ->
    (frame : ((x : node Pref.t) @ immutable ->
      {u : unit | swept_at h after trail x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | if H.mem h x then source_ok h x else H.at h x === None} ->
    {u : unit | if H.mem after x then source_ok after x else H.at after x === None} @ ghost =
  fun h after trail frame x premise -> ghost_ (
    frame x; swept_at_def h after trail x;
    source_ok_def h x; source_ok_def after x;
    (match H.at h x with None -> () | Some v ->
      (match v.memo with Empty_memo | Forward _ -> () | Memo (stamp, _) ->
        frame stamp; swept_at_def h after trail stamp; ());
      match v.desc with Var | Bool -> ()
      | Link p -> frame p; swept_at_def h after trail p; ()
      | Arrow (a, b) -> frame a; frame b;
        swept_at_def h after trail a; swept_at_def h after trail b; ());
    ())

let (sweep_below @ total) : (h : Pref.heap) @ immutable ->
    (after : Pref.heap) @ immutable -> (trail : pool) @ immutable ->
    (x : node Pref.t) @ immutable -> (depth : int) ->
    {u : unit | swept_at h after trail x} ->
    {u : unit | Level_spec.below h x depth === Level_spec.below after x depth} @ ghost =
  fun h after trail x depth premise -> ghost_ (
    swept_at_def h after trail x;
    Level_spec.below_def h x depth; Level_spec.below_def after x depth;
    Level_spec.at_level_def h x; Level_spec.at_level_def after x;
    ())

let (sweep_order @ total) : (h : Pref.heap) @ immutable ->
    (after : Pref.heap) @ immutable -> (trail : pool) @ immutable ->
    (frame : ((x : node Pref.t) @ immutable ->
      {u : unit | swept_at h after trail x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | Level_spec.ordered h x} ->
    {u : unit | Level_spec.ordered after x} @ ghost =
  fun h after trail frame x premise -> ghost_ (
    frame x; swept_at_def h after trail x;
    Level_spec.ordered_def h x; Level_spec.ordered_def after x;
    (match H.at h x with None -> () | Some v ->
      match v.level with Generic -> () | Finite depth ->
        Level_spec.children_below_def h v.desc depth;
        Level_spec.children_below_def after v.desc depth;
        match v.desc with Var | Bool -> ()
        | Link p -> frame p; sweep_below h after trail p depth (); ()
        | Arrow (a, b) -> frame a; frame b;
          sweep_below h after trail a depth ();
          sweep_below h after trail b depth (); ());
    ())

let[@def] rec (distinct @ total) (trail : pool @ immutable) = ghost_ (
  match trail with Empty -> true
  | Entry (p, rest) -> not (listed rest p) && distinct rest)

let rec (touched_distinct @ total) : (saved : Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> {u : unit | valid saved epoch depth d} ->
    {u : unit | distinct (touched d)} @ ghost =
  fun saved epoch depth d premise -> ghost_ (
    valid_def saved epoch depth d;
    touched_def d; let trail = touched d in distinct_def trail;
    match d with Start | Clean -> ()
    | Fresh (rest, p, _, _, _) | Alias (rest, p, _, _) ->
      touched_mapping rest p;
      touched_distinct saved epoch depth rest (); ())

let rec (history_clean @ total) : (saved : Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth d &&
      match H.at saved x with None -> true | Some v -> v.memo === Empty_memo} ->
    {u : unit | match H.at (heap saved epoch depth d) x with
      | None -> true | Some v -> v.memo === Empty_memo || listed (touched d) x} @ ghost =
  fun saved epoch depth d x premise -> ghost_ (
    valid_def saved epoch depth d;
    heap_def saved epoch depth d; touched_def d;
    let trail = touched d in listed_def trail x;
    match d with
    | Clean -> ()
    | Start ->
      let desc : desc = Bool in let v = cell desc depth in cell_def desc depth;
      put_frame saved epoch v x; ()
    | Fresh (rest, p, q, old, desc) ->
      history_clean saved epoch depth rest x ();
      let h = heap saved epoch depth rest in let v = cell desc depth in
      cell_def desc depth; let mid = H.put h q v in let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
      put_frame mid p w x; ()
    | Alias (rest, p, q, old) ->
      history_clean saved epoch depth rest x ();
      let h = heap saved epoch depth rest in let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
      put_frame h p w x; ())
