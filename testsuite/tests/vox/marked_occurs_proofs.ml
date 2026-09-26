open Copy_spec
open Copy_heap_proofs
open Level_unifier_spec

let rec (marks_at @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | marks_valid h needle d} ->
    {u : unit | marked_at h (marked_heap h d) d x} @ ghost =
  fun h needle d x premise -> ghost_ (
    marks_valid_def h needle d;
    marked_heap_def h d; marked_def d x;
    let after = marked_heap h d in marked_at_def h after d x;
    match d with No_marks -> ()
    | Marked (rest, p, old, _) ->
      marks_at h needle rest x ();
      let mid = marked_heap h rest in marked_at_def h mid rest x;
      let v = set_visited old true in set_visited_def old true;
      put_frame mid p v x; ())

let rec (cached_search @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | marks_valid h needle d && marked d x} ->
    {s : search | searched h needle x false s} @ immutable ghost =
  fun h needle d x premise -> ghost_ (
    marks_valid_def h needle d;
    marked_def d x; let s = cached d x in cached_def d x;
    match d with No_marks -> s
    | Marked (rest, p, _, _) ->
      if x === p then s else
      let s = cached_search h needle rest x () in s)

let rec (trail_members @ total) : (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | on_trail (mark_trail d) x === marked d x} @ ghost =
  fun d x -> ghost_ (
    mark_trail_def d; marked_def d x;
    let trail = mark_trail d in on_trail_def trail x;
    match d with No_marks -> ()
    | Marked (rest, _, _, _) -> trail_members rest x; ())

let rec (initially_unmarked @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | marks_valid h needle d} ->
    {u : unit | not (marked d x) || (H.mem h x && match H.at h x with
      None -> false | Some v -> not v.visited)} @ ghost =
  fun h needle d x premise -> ghost_ (
    marks_valid_def h needle d;
    marked_def d x; match d with No_marks -> ()
    | Marked (rest, p, _, proof) ->
      initially_unmarked h needle rest x ();
      marks_at h needle rest x ();
      let mid = marked_heap h rest in marked_at_def h mid rest x;
      searched_def h needle p false proof; ())

let rec (resets_at @ total) : (h : node Pref.heap) @ immutable ->
    (trail : trail) @ immutable ->
    (members : ((x : node Pref.t) @ immutable ->
      {u : unit | not (on_trail trail x) || H.mem h x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | reset_at h (reset_heap h trail) trail x} @ ghost =
  fun h trail members x -> ghost_ (
    reset_heap_def h trail; on_trail_def trail x;
    match trail with
    | End -> reset_at_def h h trail x; ()
    | Trail (p, rest) ->
      on_trail_def trail p; members p;
      (match H.at h p with
      | None ->
        let tail : ((y : node Pref.t) @ immutable ->
          {u : unit | not (on_trail rest y) || H.mem h y}) @ total = fun y ->
          on_trail_def trail y; members y; () in
        resets_at h rest tail x;
        let after = reset_heap h rest in reset_at_def h after rest x;
        let after = reset_heap h trail in reset_at_def h after trail x;
        members x; ()
      | Some old ->
        let v = set_visited old false in set_visited_def old false;
        let mid = H.put h p v in let tail : ((y : node Pref.t) @ immutable ->
          {u : unit | not (on_trail rest y) || H.mem mid y}) @ total = fun y ->
          on_trail_def trail y; members y; () in
        resets_at mid rest tail x;
        let after = reset_heap mid rest in reset_at_def mid after rest x;
        let after = reset_heap h trail in reset_at_def h after trail x;
        ()))


let (restored_at @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | marks_valid h needle d} ->
    {u : unit | let after = reset_heap (marked_heap h d) (mark_trail d) in
      H.mem h x === H.mem after x && H.at h x === H.at after x} @ ghost =
  fun h needle d x premise -> ghost_ (
    let mid = marked_heap h d in let trail = mark_trail d in
    let members : ((y : node Pref.t) @ immutable ->
      {u : unit | not (on_trail trail y) || H.mem mid y}) @ total = fun y ->
      trail_members d y; initially_unmarked h needle d y (); marks_at h needle d y ();
      marked_at_def h mid d y; () in
    resets_at mid trail members x;
    let after = reset_heap mid trail in reset_at_def mid after trail x;
    marks_at h needle d x ();
    marked_at_def h mid d x; initially_unmarked h needle d x ();
    trail_members d x; ())

let (scan_at @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | marks_valid h needle d} ->
    {u : unit | H.mem h x === H.mem (scan_heap h d) x
      && H.at h x === H.at (scan_heap h d) x} @ ghost =
  fun h needle d x premise -> ghost_ (
    scan_heap_def h d; let () = restored_at h needle d x () in ())

let (scan_frame @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | marks_valid h needle d} ->
    {u : unit | Level_spec.lower_frame h (scan_heap h d) x} @ ghost =
  fun h needle d x premise -> ghost_ (
    scan_at h needle d x ();
    let after = scan_heap h d in Level_spec.lower_frame_def h after x;
    (match H.at h x with None -> () | Some v -> Level_spec.decreases_def v.level v.level; ());
    ())

let (scan_observe @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | marks_valid h needle d} ->
    {u : unit | H.mem h x === H.mem (scan_heap h d) x
      && observe h x === observe (scan_heap h d) x
      && Level_spec.active h x === Level_spec.active (scan_heap h d) x} @ ghost =
  fun h needle d x premise -> ghost_ (
    scan_at h needle d x ();
    let after = scan_heap h d in observe_def h x; observe_def after x;
    Level_spec.active_def h x; Level_spec.active_def after x;
    Level_spec.at_level_def h x; Level_spec.at_level_def after x; ())

let (scan_equation @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | marks_valid h needle d} ->
    {u : unit | node_equation h rho x === node_equation (scan_heap h d) rho x} @ ghost =
  fun h needle d rho x premise -> ghost_ (
    scan_observe h needle d x ();
    let after = scan_heap h d in node_equation_def h rho x; node_equation_def after rho x;
    ())

let (scan_scope @ total) : (h : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h x) || Level_spec.finite_scope h x})) @ total ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | marks_valid h needle d} ->
    {u : unit | not (H.mem (scan_heap h d) x) || Level_spec.finite_scope (scan_heap h d) x} @ ghost =
  fun h scope needle d x premise -> ghost_ (
    let after = scan_heap h d in
    let frame : ((y : node Pref.t) @ immutable ->
      {u : unit | Level_spec.lower_frame h after y}) @ total = fun y ->
      let () = scan_frame h needle d y () in () in
    scope x; let () = Level_proofs.frame_scope h after frame x () in
    ())

let rec (scan_searched @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (a : node Pref.t) @ immutable -> (b : node Pref.t) @ immutable ->
    (found : bool) -> (trace : search) @ immutable ->
    {u : unit | marks_valid h needle d && searched h a b found trace} ->
    {u : unit | searched (scan_heap h d) a b found trace} @ ghost =
  fun h needle d a b found trace premise -> ghost_ (
    let after = scan_heap h d in
    searched_def h a b found trace; searched_def after a b found trace;
    scan_observe h needle d b ();
    match trace with Hit | Leaf -> ()
    | Follow (q, rest) -> scan_searched h needle d a q found rest (); ()
    | Left (x, _, left) -> scan_searched h needle d a x true left (); ()
    | Both (x, y, left, right) ->
      scan_searched h needle d a x false left ();
      scan_searched h needle d a y found right (); ())

let (scan_scoped @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | marks_valid h needle d} ->
    {u : unit | scoped h x === scoped (scan_heap h d) x} @ ghost =
  fun h needle d x premise -> ghost_ (
    scan_observe h needle d x ();
    let after = scan_heap h d in scoped_def h x; scoped_def after x;
    (match observe h x with Some (Link q | List q) -> scan_at h needle d q (); ()
    | Some (Arrow (a, b)) -> scan_at h needle d a ();
      scan_at h needle d b (); () | _ -> ()); ())

let (scan_below @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable -> (depth : int) ->
    {u : unit | marks_valid h needle d} ->
    {u : unit | Level_spec.below h x depth === Level_spec.below (scan_heap h d) x depth} @ ghost =
  fun h needle d x depth premise -> ghost_ (
    scan_at h needle d x ();
    let after = scan_heap h d in Level_spec.below_def h x depth;
    Level_spec.below_def after x depth;
    Level_spec.at_level_def h x; Level_spec.at_level_def after x; ())

let (scan_ordered @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | marks_valid h needle d && Level_spec.ordered h x} ->
    {u : unit | Level_spec.ordered (scan_heap h d) x} @ ghost =
  fun h needle d x premise -> ghost_ (
    scan_at h needle d x ();
    let after = scan_heap h d in Level_spec.ordered_def h x; Level_spec.ordered_def after x;
    (match H.at h x with None -> () | Some v -> match v.level with Generic -> ()
    | Finite depth -> Level_spec.children_below_def h v.desc depth;
      Level_spec.children_below_def after v.desc depth;
      match v.desc with Var | Bool | Word -> ()
      | Link q | List q -> scan_below h needle d q depth (); ()
      | Arrow (a, b) -> scan_below h needle d a depth ();
        scan_below h needle d b depth (); ()); ())
