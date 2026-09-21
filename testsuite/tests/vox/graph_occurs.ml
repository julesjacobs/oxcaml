open Copy_spec
open Copy_heap_proofs
open Level_spec
open Level_unifier_spec
open Marked_occurs_proofs

let remember : (h : (Pref.heap) Ghost.t) @ immutable  ->(needle : (node Pref.t) Ghost.t) @ immutable  ->(d : (marks) Ghost.t) @ immutable  -> (trail : trail) @ immutable  ->
    (p : node Pref.t) @ immutable  ->(search : (search) Ghost.t) @ immutable  ->
    (state : {t : Pref.token | marks_valid h.Ghost.ghost needle.Ghost.ghost d.Ghost.ghost
      && trail === mark_trail d.Ghost.ghost && Pref.own t === marked_heap h.Ghost.ghost d.Ghost.ghost
      && searched h.Ghost.ghost needle.Ghost.ghost p false search.Ghost.ghost}) @ unique  ->
    {r : scanning | marks_valid h.Ghost.ghost needle.Ghost.ghost r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap h.Ghost.ghost r.#marks
      && not r.#found && searched h.Ghost.ghost needle.Ghost.ghost p r.#found r.#search} @ unique = fun h needle d trail p search state  ->
    ghost_ (marks_at h.Ghost.ghost needle.Ghost.ghost d.Ghost.ghost p ();
      let current = marked_heap h.Ghost.ghost d.Ghost.ghost in marked_at_def h.Ghost.ghost current d.Ghost.ghost p;
      searched_def h.Ghost.ghost needle.Ghost.ghost p false search.Ghost.ghost);
    let old = Pref.read p (borrow_ state) in if old.visited then
      let r = #{state; found = false; trail; marks = d.Ghost.ghost; search = search.Ghost.ghost} in r
    else
      let v = set_visited old true in
      let state = Pref.write p v state in
      let marks = ghost_ (Marked (d.Ghost.ghost, p, old, search.Ghost.ghost)) in
      let trail = Trail (p, trail) in
      ghost_ (marks_valid_def h.Ghost.ghost needle.Ghost.ghost marks; marked_heap_def h.Ghost.ghost marks;
        mark_trail_def marks);
      let r = #{state; found = false; trail; marks; search = search.Ghost.ghost} in r

type scan_goal = { heap : Pref.heap @@ ghost; needle : node Pref.t @@ ghost;
  root : node Pref.t @@ ghost }

let rec scan_work : (goal : scan_goal) @ immutable -> (h : (Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost x) || source_ok h.Ghost.ghost x})) Ghost.t) @ total  ->(unmarked : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h.Ghost.ghost x with
        None -> true | Some v -> not v.visited})) Ghost.t) @ total  ->
    (needle : node Pref.t) @ immutable  -> (p : node Pref.t) @ immutable  ->(d : (marks) Ghost.t) @ immutable  -> (trail : trail) @ immutable  ->
    (state : {t : Pref.token | marks_valid h.Ghost.ghost needle d.Ghost.ghost
      && trail === mark_trail d.Ghost.ghost && Pref.own t === marked_heap h.Ghost.ghost d.Ghost.ghost
      && H.mem h.Ghost.ghost p}) @ unique  ->
    (use : ((r : {r : scanning | marks_valid h.Ghost.ghost needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap h.Ghost.ghost r.#marks
      && searched h.Ghost.ghost needle p r.#found r.#search}) @ unique -> {r : scanning | marks_valid goal.heap goal.needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap goal.heap r.#marks
      && searched goal.heap goal.needle goal.root r.#found r.#search} @ unique)) ->
    {r : scanning | marks_valid goal.heap goal.needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap goal.heap r.#marks
      && searched goal.heap goal.needle goal.root r.#found r.#search} @ unique = fun goal h scope unmarked needle p d trail state use ->
    let same = Pref.equal needle p in
    if same then
      let search = ghost_ Hit in
      ghost_ (searched_def h.Ghost.ghost needle p true search);
      let r = #{state; found = true; trail; marks = d.Ghost.ghost; search} in use (r)
    else begin
      ghost_ (marks_at h.Ghost.ghost needle d.Ghost.ghost p ();
        let current = marked_heap h.Ghost.ghost d.Ghost.ghost in marked_at_def h.Ghost.ghost current d.Ghost.ghost p;
        scope.Ghost.ghost p; unmarked.Ghost.ghost p; source_ok_def h.Ghost.ghost p;
        observe_def h.Ghost.ghost p);
      let old = Pref.read p (borrow_ state) in if old.visited then
        let search = ghost_ (cached_search h.Ghost.ghost needle d.Ghost.ghost p ()) in
        let r = #{state; found = false; trail; marks = d.Ghost.ghost; search} in use (r)
      else
        match old.desc with
        | Var | Bool ->
          let search = ghost_ Leaf in
          ghost_ (searched_def h.Ghost.ghost needle p false search);
          let state : {t : Pref.token | marks_valid h.Ghost.ghost needle d.Ghost.ghost
            && trail === mark_trail d.Ghost.ghost && Pref.own t === marked_heap h.Ghost.ghost d.Ghost.ghost
            && searched h.Ghost.ghost needle p false search} = state in
          let h_witness27 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
          let needle_witness28 : (node Pref.t) Ghost.t = {Ghost.ghost = ghost_ (needle)} in
          let d_witness29 : (marks) Ghost.t = {Ghost.ghost = ghost_ (d.Ghost.ghost)} in
          let search_witness30 : (search) Ghost.t = {Ghost.ghost = ghost_ (search)} in
          let state_argument31 = state in
          let r = remember h_witness27 needle_witness28 d_witness29 trail p search_witness30 (state_argument31) in use (r)
        | Link q ->
          let state : {t : Pref.token | marks_valid h.Ghost.ghost needle d.Ghost.ghost
            && trail === mark_trail d.Ghost.ghost && Pref.own t === marked_heap h.Ghost.ghost d.Ghost.ghost
            && H.mem h.Ghost.ghost q} = state in
          let h_witness7 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
          let scope_witness8 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness7.Ghost.ghost x) || source_ok h_witness7.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
          let unmarked_witness9 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h_witness7.Ghost.ghost x with
        None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked.Ghost.ghost)} in
          let d_witness10 : (marks) Ghost.t = {Ghost.ghost = ghost_ (d.Ghost.ghost)} in
          let state_argument11 = state in
          let resume_child : (child : {r : scanning | marks_valid h_witness7.Ghost.ghost needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap h_witness7.Ghost.ghost r.#marks
      && searched h_witness7.Ghost.ghost needle q r.#found r.#search}) @ unique ->
            {r : scanning | marks_valid goal.heap goal.needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap goal.heap r.#marks
      && searched goal.heap goal.needle goal.root r.#found r.#search} @ unique = fun child ->
            let search = ghost_ (Follow (q, child.#search)) in
          let found = child.#found in
          ghost_ (searched_def h.Ghost.ghost needle p found search);
          let state = child.#state in let trail = child.#trail in
          let marks = ghost_ child.#marks in
          if found then
            let r = #{state; found; trail; marks; search} in use (r)
          else
            let state : {t : Pref.token | marks_valid h.Ghost.ghost needle marks
              && trail === mark_trail marks && Pref.own t === marked_heap h.Ghost.ghost marks
              && searched h.Ghost.ghost needle p false search} = state in
            let h_witness32 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
            let needle_witness33 : (node Pref.t) Ghost.t = {Ghost.ghost = ghost_ (needle)} in
            let d_witness34 : (marks) Ghost.t = {Ghost.ghost = ghost_ (marks)} in
            let search_witness35 : (search) Ghost.t = {Ghost.ghost = ghost_ (search)} in
            let state_argument36 = state in
            let r = remember h_witness32 needle_witness33 d_witness34 trail p search_witness35 (state_argument36) in use (r) in
          scan_work goal h_witness7 scope_witness8 unmarked_witness9 needle q d_witness10 trail (state_argument11) resume_child
        | Arrow (a, b) ->
          let state : {t : Pref.token | marks_valid h.Ghost.ghost needle d.Ghost.ghost
            && trail === mark_trail d.Ghost.ghost && Pref.own t === marked_heap h.Ghost.ghost d.Ghost.ghost
            && H.mem h.Ghost.ghost a} = state in
          let h_witness12 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
          let scope_witness13 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness12.Ghost.ghost x) || source_ok h_witness12.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
          let unmarked_witness14 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h_witness12.Ghost.ghost x with
        None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked.Ghost.ghost)} in
          let d_witness15 : (marks) Ghost.t = {Ghost.ghost = ghost_ (d.Ghost.ghost)} in
          let state_argument16 = state in
          let resume_left : (left : {r : scanning | marks_valid h_witness12.Ghost.ghost needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap h_witness12.Ghost.ghost r.#marks
      && searched h_witness12.Ghost.ghost needle a r.#found r.#search}) @ unique ->
            {r : scanning | marks_valid goal.heap goal.needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap goal.heap r.#marks
      && searched goal.heap goal.needle goal.root r.#found r.#search} @ unique = fun left ->
            let state = left.#state in let trail = left.#trail in
          let marks = ghost_ left.#marks in
          if left.#found then
            let search = ghost_ (Left (a, b, left.#search)) in
            ghost_ (searched_def h.Ghost.ghost needle p true search);
            let r = #{state; found = true; trail; marks; search} in use (r)
          else
            let state : {t : Pref.token | marks_valid h.Ghost.ghost needle marks
              && trail === mark_trail marks && Pref.own t === marked_heap h.Ghost.ghost marks
              && H.mem h.Ghost.ghost b} = state in
            let h_witness17 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
            let scope_witness18 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness17.Ghost.ghost x) || source_ok h_witness17.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
            let unmarked_witness19 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h_witness17.Ghost.ghost x with
        None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked.Ghost.ghost)} in
            let d_witness20 : (marks) Ghost.t = {Ghost.ghost = ghost_ (marks)} in
            let state_argument21 = state in
            let resume_right : (right : {r : scanning | marks_valid h_witness17.Ghost.ghost needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap h_witness17.Ghost.ghost r.#marks
      && searched h_witness17.Ghost.ghost needle b r.#found r.#search}) @ unique ->
              {r : scanning | marks_valid goal.heap goal.needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap goal.heap r.#marks
      && searched goal.heap goal.needle goal.root r.#found r.#search} @ unique = fun right ->
              let search = ghost_ (Both (a, b, left.#search, right.#search)) in
            let found = right.#found in
            ghost_ (searched_def h.Ghost.ghost needle p found search);
            let state = right.#state in let trail = right.#trail in
            let marks = ghost_ right.#marks in
            if found then
              let r = #{state; found; trail; marks; search} in use (r)
            else
              let state : {t : Pref.token | marks_valid h.Ghost.ghost needle marks
                && trail === mark_trail marks && Pref.own t === marked_heap h.Ghost.ghost marks
                && searched h.Ghost.ghost needle p false search} = state in
              let h_witness37 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
              let needle_witness38 : (node Pref.t) Ghost.t = {Ghost.ghost = ghost_ (needle)} in
              let d_witness39 : (marks) Ghost.t = {Ghost.ghost = ghost_ (marks)} in
              let search_witness40 : (search) Ghost.t = {Ghost.ghost = ghost_ (search)} in
              let state_argument41 = state in
              let r = remember h_witness37 needle_witness38 d_witness39 trail p search_witness40 (state_argument41) in use (r)
 in
            scan_work goal h_witness17 scope_witness18 unmarked_witness19 needle b d_witness20 trail (state_argument21) resume_right in
          scan_work goal h_witness12 scope_witness13 unmarked_witness14 needle a d_witness15 trail (state_argument16) resume_left
    end

let scan : (h : (Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost x) || source_ok h.Ghost.ghost x})) Ghost.t) @ total  ->(unmarked : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h.Ghost.ghost x with
        None -> true | Some v -> not v.visited})) Ghost.t) @ total  ->
    (needle : node Pref.t) @ immutable  -> (p : node Pref.t) @ immutable  ->(d : (marks) Ghost.t) @ immutable  -> (trail : trail) @ immutable  ->
    (state : {t : Pref.token | marks_valid h.Ghost.ghost needle d.Ghost.ghost
      && trail === mark_trail d.Ghost.ghost && Pref.own t === marked_heap h.Ghost.ghost d.Ghost.ghost
      && H.mem h.Ghost.ghost p}) @ unique  ->
    {r : scanning | marks_valid h.Ghost.ghost needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap h.Ghost.ghost r.#marks
      && searched h.Ghost.ghost needle p r.#found r.#search} @ unique  = fun h scope unmarked needle p d trail state ->
    let goal = {heap = h.Ghost.ghost; needle; root = p} in
    let use : (r : {r : scanning | marks_valid h.Ghost.ghost needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap h.Ghost.ghost r.#marks
      && searched h.Ghost.ghost needle p r.#found r.#search}) @ unique -> {r : scanning | marks_valid goal.heap goal.needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap goal.heap r.#marks
      && searched goal.heap goal.needle goal.root r.#found r.#search} @ unique = fun r ->
      r in
    let out = scan_work goal h scope unmarked needle p d trail state use in out

let rec reset : (h : (Pref.heap) Ghost.t) @ immutable  ->
    (trail : trail) @ immutable  ->(members : (((x : node Pref.t) @ immutable ->
      {u : unit | not (on_trail trail x) || H.mem h.Ghost.ghost x})) Ghost.t) @ total  ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost}) @ unique  ->
    {t : Pref.token | Pref.own t === reset_heap h.Ghost.ghost trail} @ unique = fun h trail members state  ->
    ghost_ (reset_heap_def h.Ghost.ghost trail);
    match trail with
    | End -> state
    | Trail (p, rest) ->
      ghost_ (on_trail_def trail p; members.Ghost.ghost p);
      let old = Pref.read p (borrow_ state) in
      let v = set_visited old false in
      let state = Pref.write p v state in
      let mid = ghost_ (H.put h.Ghost.ghost p v) in
      let tail : ((x : node Pref.t) @ immutable ->
        {u : unit | not (on_trail rest x) || H.mem mid x}) @ total ghost = ghost_ (fun x ->
        on_trail_def trail x; members.Ghost.ghost x; ()) in
      let state : {t : Pref.token | Pref.own t === mid} = state in
      let h_witness1 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (mid)} in
      let members_witness2 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (on_trail rest x) || H.mem h_witness1.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (tail)} in
      let state_argument3 = state in
      let state = reset h_witness1 rest members_witness2 (state_argument3) in state

let occurs : (h : (Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost x) || source_ok h.Ghost.ghost x})) Ghost.t) @ total  ->(unmarked : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h.Ghost.ghost x with
        None -> true | Some v -> not v.visited})) Ghost.t) @ total  ->
    (needle : node Pref.t) @ immutable  -> (p : node Pref.t) @ immutable  ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p}) @ unique  ->
    {r : checked | marks_valid h.Ghost.ghost needle r.#marks
      && searched h.Ghost.ghost needle p r.#found r.#search
      && Pref.own r.#state ===
        reset_heap (marked_heap h.Ghost.ghost r.#marks) (mark_trail r.#marks)} @ unique = fun h scope unmarked needle p state  ->
    let d = ghost_ No_marks in let trail = End in
    ghost_ (marks_valid_def h.Ghost.ghost needle d; marked_heap_def h.Ghost.ghost d; mark_trail_def d);
    let state : {t : Pref.token | marks_valid h.Ghost.ghost needle d
      && trail === mark_trail d && Pref.own t === marked_heap h.Ghost.ghost d
      && H.mem h.Ghost.ghost p} = state in
    let h_witness22 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
    let scope_witness23 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness22.Ghost.ghost x) || source_ok h_witness22.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
    let unmarked_witness24 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h_witness22.Ghost.ghost x with
        None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked.Ghost.ghost)} in
    let d_witness25 : (marks) Ghost.t = {Ghost.ghost = ghost_ (d)} in
    let state_argument26 = state in
    let found = scan h_witness22 scope_witness23 unmarked_witness24 needle p d_witness25 trail (state_argument26) in
    let d = ghost_ found.#marks in let trail = found.#trail in
    let mid = ghost_ (marked_heap h.Ghost.ghost d) in
    let members : ((x : node Pref.t) @ immutable ->
      {u : unit | not (on_trail trail x) || H.mem mid x}) @ total ghost = ghost_ (fun x ->
      trail_members d x; initially_unmarked h.Ghost.ghost needle d x ();
      marks_at h.Ghost.ghost needle d x (); marked_at_def h.Ghost.ghost mid d x; ()) in
    let state = found.#state in
    let state : {t : Pref.token | Pref.own t === mid} = state in
    let h_witness4 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (mid)} in
    let members_witness5 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (on_trail trail x) || H.mem h_witness4.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (members)} in
    let state_argument6 = state in
    let state = reset h_witness4 trail members_witness5 (state_argument6) in
    let r = #{state; found = found.#found; marks = d; search = found.#search} in r
