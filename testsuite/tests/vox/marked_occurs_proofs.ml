open Copy_spec
open Copy_heap_proofs
open Level_unifier_spec

let rec (marks_at @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | marks_valid h needle d} ->
    {u : unit | marked_at h (marked_heap h d) d x} @ ghost =
  fun h needle d x premise -> ghost_ (
    let refine_ premise = premise in marks_valid_def h needle d;
    marked_heap_def h d; marked_def d x;
    let after = marked_heap h d in marked_at_def h after d x;
    let u = () in match d with No_marks -> refine_ u
    | Marked (rest, p, old, _) ->
      marks_at h needle rest x (refine_ u);
      let mid = marked_heap h rest in marked_at_def h mid rest x;
      let v = set_visited old true in set_visited_def old true;
      put_frame mid p v x; refine_ u)

let rec (cached_search @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | marks_valid h needle d && marked d x} ->
    {s : search | searched h needle x false s} @ immutable ghost =
  fun h needle d x premise -> ghost_ (
    let refine_ premise = premise in marks_valid_def h needle d;
    marked_def d x; let s = cached d x in cached_def d x;
    let u = () in match d with No_marks -> refine_ s
    | Marked (rest, p, _, _) ->
      if x === p then refine_ s else
      let refine_ s = cached_search h needle rest x (refine_ u) in refine_ s)

let rec (trail_members @ total) : (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | on_trail (mark_trail d) x === marked d x} @ ghost =
  fun d x -> ghost_ (
    mark_trail_def d; marked_def d x;
    let trail = mark_trail d in on_trail_def trail x;
    let u = () in match d with No_marks -> refine_ u
    | Marked (rest, _, _, _) -> trail_members rest x; refine_ u)

let rec (initially_unmarked @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | marks_valid h needle d} ->
    {u : unit | not (marked d x) || (H.mem h x && match H.at h x with
      None -> false | Some v -> not v.visited)} @ ghost =
  fun h needle d x premise -> ghost_ (
    let refine_ premise = premise in marks_valid_def h needle d;
    marked_def d x; let u = () in match d with No_marks -> refine_ u
    | Marked (rest, p, _, proof) ->
      initially_unmarked h needle rest x (refine_ u);
      marks_at h needle rest x (refine_ u);
      let mid = marked_heap h rest in marked_at_def h mid rest x;
      searched_def h needle p false proof; refine_ u)

let rec (resets_at @ total) : (h : node Pref.heap) @ immutable ->
    (trail : trail) @ immutable ->
    (members : ((x : node Pref.t) @ immutable ->
      {u : unit | not (on_trail trail x) || H.mem h x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | reset_at h (reset_heap h trail) trail x} @ ghost =
  fun h trail members x -> ghost_ (
    reset_heap_def h trail; on_trail_def trail x;
    let u = () in match trail with
    | End -> reset_at_def h h trail x; refine_ u
    | Trail (p, rest) ->
      on_trail_def trail p; members p;
      (match H.at h p with
      | None ->
        let tail : ((y : node Pref.t) @ immutable ->
          {u : unit | not (on_trail rest y) || H.mem h y}) @ total = fun y ->
          on_trail_def trail y; members y; let u = () in refine_ u in
        resets_at h rest tail x;
        let after = reset_heap h rest in reset_at_def h after rest x;
        let after = reset_heap h trail in reset_at_def h after trail x;
        members x; refine_ u
      | Some old ->
        let v = set_visited old false in set_visited_def old false;
        let mid = H.put h p v in put_frame h p v x;
        let tail : ((y : node Pref.t) @ immutable ->
          {u : unit | not (on_trail rest y) || H.mem mid y}) @ total = fun y ->
          on_trail_def trail y; members y; put_frame h p v y;
          let u = () in refine_ u in
        resets_at mid rest tail x;
        let after = reset_heap mid rest in reset_at_def mid after rest x;
        let after = reset_heap h trail in reset_at_def h after trail x;
        refine_ u))


let (restored_at @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | marks_valid h needle d} ->
    {u : unit | let after = reset_heap (marked_heap h d) (mark_trail d) in
      H.mem h x === H.mem after x && H.at h x === H.at after x} @ ghost =
  fun h needle d x premise -> ghost_ (
    let refine_ premise = premise in
    let mid = marked_heap h d in let trail = mark_trail d in
    let members : ((y : node Pref.t) @ immutable ->
      {u : unit | not (on_trail trail y) || H.mem mid y}) @ total = fun y ->
      trail_members d y; let u = () in
      initially_unmarked h needle d y (refine_ u); marks_at h needle d y (refine_ u);
      marked_at_def h mid d y; refine_ u in
    resets_at mid trail members x;
    let after = reset_heap mid trail in reset_at_def mid after trail x;
    let u = () in marks_at h needle d x (refine_ u);
    marked_at_def h mid d x; initially_unmarked h needle d x (refine_ u);
    trail_members d x; refine_ u)

let (scan_at @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | marks_valid h needle d} ->
    {u : unit | H.mem h x === H.mem (scan_heap h d) x
      && H.at h x === H.at (scan_heap h d) x} @ ghost =
  fun h needle d x premise -> ghost_ (
    let refine_ premise = premise in scan_heap_def h d; let u = () in
    let refine_ u = restored_at h needle d x (refine_ u) in refine_ u)

let (scan_frame @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | marks_valid h needle d} ->
    {u : unit | Level_spec.lower_frame h (scan_heap h d) x} @ ghost =
  fun h needle d x premise -> ghost_ (
    let refine_ premise = premise in let u = () in scan_at h needle d x (refine_ u);
    let after = scan_heap h d in Level_spec.lower_frame_def h after x;
    (match H.at h x with None -> () | Some v -> Level_spec.decreases_def v.level v.level; ());
    refine_ u)

let (scan_observe @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | marks_valid h needle d} ->
    {u : unit | H.mem h x === H.mem (scan_heap h d) x
      && observe h x === observe (scan_heap h d) x
      && Level_spec.active h x === Level_spec.active (scan_heap h d) x} @ ghost =
  fun h needle d x premise -> ghost_ (
    let refine_ premise = premise in let u = () in scan_at h needle d x (refine_ u);
    let after = scan_heap h d in observe_def h x; observe_def after x;
    Level_spec.active_def h x; Level_spec.active_def after x;
    Level_spec.at_level_def h x; Level_spec.at_level_def after x; refine_ u)

let (scan_equation @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | marks_valid h needle d} ->
    {u : unit | node_equation h rho x === node_equation (scan_heap h d) rho x} @ ghost =
  fun h needle d rho x premise -> ghost_ (
    let refine_ premise = premise in let u = () in scan_observe h needle d x (refine_ u);
    let after = scan_heap h d in node_equation_def h rho x; node_equation_def after rho x;
    refine_ u)

let (scan_scope @ total) : (h : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h x) || Level_spec.finite_scope h x})) @ total ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | marks_valid h needle d} ->
    {u : unit | not (H.mem (scan_heap h d) x) || Level_spec.finite_scope (scan_heap h d) x} @ ghost =
  fun h scope needle d x premise -> ghost_ (
    let refine_ premise = premise in let after = scan_heap h d in
    let frame : ((y : node Pref.t) @ immutable ->
      {u : unit | Level_spec.lower_frame h after y}) @ total = fun y ->
      let u = () in let refine_ u = scan_frame h needle d y (refine_ u) in refine_ u in
    scope x; let u = () in let refine_ u = Level_proofs.frame_scope h after frame x (refine_ u) in
    refine_ u)

let rec (scan_searched @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (a : node Pref.t) @ immutable -> (b : node Pref.t) @ immutable ->
    (found : bool) -> (trace : search) @ immutable ->
    {u : unit | marks_valid h needle d && searched h a b found trace} ->
    {u : unit | searched (scan_heap h d) a b found trace} @ ghost =
  fun h needle d a b found trace premise -> ghost_ (
    let refine_ premise = premise in let after = scan_heap h d in
    searched_def h a b found trace; searched_def after a b found trace;
    let u = () in scan_observe h needle d b (refine_ u);
    match trace with Hit | Leaf -> refine_ u
    | Follow (q, rest) -> scan_searched h needle d a q found rest (refine_ u); refine_ u
    | Left (x, _, left) -> scan_searched h needle d a x true left (refine_ u); refine_ u
    | Both (x, y, left, right) ->
      scan_searched h needle d a x false left (refine_ u);
      scan_searched h needle d a y found right (refine_ u); refine_ u)

let (scan_scoped @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | marks_valid h needle d} ->
    {u : unit | scoped h x === scoped (scan_heap h d) x} @ ghost =
  fun h needle d x premise -> ghost_ (
    let refine_ premise = premise in let u = () in scan_observe h needle d x (refine_ u);
    let after = scan_heap h d in scoped_def h x; scoped_def after x;
    (match observe h x with Some (Link q) -> scan_at h needle d q (refine_ u); ()
    | Some (Arrow (a, b)) -> scan_at h needle d a (refine_ u);
      scan_at h needle d b (refine_ u); () | _ -> ()); refine_ u)

let (scan_below @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable -> (depth : int) ->
    {u : unit | marks_valid h needle d} ->
    {u : unit | Level_spec.below h x depth === Level_spec.below (scan_heap h d) x depth} @ ghost =
  fun h needle d x depth premise -> ghost_ (
    let refine_ premise = premise in let u = () in scan_at h needle d x (refine_ u);
    let after = scan_heap h d in Level_spec.below_def h x depth;
    Level_spec.below_def after x depth;
    Level_spec.at_level_def h x; Level_spec.at_level_def after x; refine_ u)

let (scan_ordered @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | marks_valid h needle d && Level_spec.ordered h x} ->
    {u : unit | Level_spec.ordered (scan_heap h d) x} @ ghost =
  fun h needle d x premise -> ghost_ (
    let refine_ premise = premise in let u = () in scan_at h needle d x (refine_ u);
    let after = scan_heap h d in Level_spec.ordered_def h x; Level_spec.ordered_def after x;
    (match H.at h x with None -> () | Some v -> match v.level with Generic -> ()
    | Finite depth -> Level_spec.children_below_def h v.desc depth;
      Level_spec.children_below_def after v.desc depth;
      match v.desc with Var | Bool -> ()
      | Link q -> scan_below h needle d q depth (refine_ u); ()
      | Arrow (a, b) -> scan_below h needle d a depth (refine_ u);
        scan_below h needle d b depth (refine_ u); ()); refine_ u)
