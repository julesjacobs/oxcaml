open Copy_spec
open Copy_heap_proofs
open Level_spec
open Level_unifier_spec
open Marked_occurs_proofs

let remember : (h : (node Pref.heap) Ghost.t) @ immutable  ->(needle : (node Pref.t) Ghost.t) @ immutable  ->(d : (marks) Ghost.t) @ immutable  -> (trail : trail) @ immutable  ->
    (p : node Pref.t) @ immutable  ->(search : (search) Ghost.t) @ immutable  ->
    (state : {t : node Pref.token | marks_valid h.Ghost.ghost needle.Ghost.ghost d.Ghost.ghost
      && trail === mark_trail d.Ghost.ghost && Pref.own t === marked_heap h.Ghost.ghost d.Ghost.ghost
      && searched h.Ghost.ghost needle.Ghost.ghost p false search.Ghost.ghost}) @ unique  ->
    {r : scanning | marks_valid h.Ghost.ghost needle.Ghost.ghost r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap h.Ghost.ghost r.#marks
      && not r.#found && searched h.Ghost.ghost needle.Ghost.ghost p r.#found r.#search} @ unique = fun h needle d trail p search state  ->
    let refine_ state = state in
    ghost_ (let u = () in marks_at h.Ghost.ghost needle.Ghost.ghost d.Ghost.ghost p (refine_ u);
      let current = marked_heap h.Ghost.ghost d.Ghost.ghost in marked_at_def h.Ghost.ghost current d.Ghost.ghost p;
      searched_def h.Ghost.ghost needle.Ghost.ghost p false search.Ghost.ghost);
    let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
    let refine_ old = Pref.read p (borrow_ state) in let refine_ state = state in
    if old.visited then
      let r = #{state; found = false; trail; marks = d.Ghost.ghost; search = search.Ghost.ghost} in refine_ r
    else
      let v = set_visited old true in
      let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
      let refine_ state = Pref.write p v state in
      let marks = ghost_ (Marked (d.Ghost.ghost, p, old, search.Ghost.ghost)) in
      let trail = Trail (p, trail) in
      ghost_ (marks_valid_def h.Ghost.ghost needle.Ghost.ghost marks; marked_heap_def h.Ghost.ghost marks;
        mark_trail_def marks);
      let r = #{state; found = false; trail; marks; search = search.Ghost.ghost} in refine_ r

type scan_goal = { heap : node Pref.heap @@ ghost; needle : node Pref.t @@ ghost;
  root : node Pref.t @@ ghost }

let rec scan_work : (goal : scan_goal) @ immutable -> (h : (node Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost x) || finite_scope h.Ghost.ghost x})) Ghost.t) @ total  ->(unmarked : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h.Ghost.ghost x with
        None -> true | Some v -> not v.visited})) Ghost.t) @ total  ->
    (needle : node Pref.t) @ immutable  -> (p : node Pref.t) @ immutable  ->(d : (marks) Ghost.t) @ immutable  -> (trail : trail) @ immutable  ->
    (state : {t : node Pref.token | marks_valid h.Ghost.ghost needle d.Ghost.ghost
      && trail === mark_trail d.Ghost.ghost && Pref.own t === marked_heap h.Ghost.ghost d.Ghost.ghost
      && H.mem h.Ghost.ghost p && active h.Ghost.ghost p}) @ unique  ->
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
    let refine_ state = state in
    let refine_ same = Pref.equal needle p in
    if same then
      let search = ghost_ Hit in
      ghost_ (searched_def h.Ghost.ghost needle p true search);
      let r = #{state; found = true; trail; marks = d.Ghost.ghost; search} in use (refine_ r)
    else begin
      ghost_ (let u = () in marks_at h.Ghost.ghost needle d.Ghost.ghost p (refine_ u);
        let current = marked_heap h.Ghost.ghost d.Ghost.ghost in marked_at_def h.Ghost.ghost current d.Ghost.ghost p;
        scope.Ghost.ghost p; unmarked.Ghost.ghost p; finite_scope_def h.Ghost.ghost p; source_ok_def h.Ghost.ghost p;
        observe_def h.Ghost.ghost p);
      let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
      let refine_ old = Pref.read p (borrow_ state) in let refine_ state = state in
      if old.visited then
        let refine_ search = ghost_ (let u = () in
          cached_search h.Ghost.ghost needle d.Ghost.ghost p (refine_ u)) in
        let r = #{state; found = false; trail; marks = d.Ghost.ghost; search} in use (refine_ r)
      else
        match old.desc with
        | Var | Bool ->
          let search = ghost_ Leaf in
          ghost_ (searched_def h.Ghost.ghost needle p false search);
          let state : {t : node Pref.token | marks_valid h.Ghost.ghost needle d.Ghost.ghost
            && trail === mark_trail d.Ghost.ghost && Pref.own t === marked_heap h.Ghost.ghost d.Ghost.ghost
            && searched h.Ghost.ghost needle p false search} = refine_ state in
          let h_witness27 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
          let needle_witness28 : (node Pref.t) Ghost.t = {Ghost.ghost = ghost_ (needle)} in
          let d_witness29 : (marks) Ghost.t = {Ghost.ghost = ghost_ (d.Ghost.ghost)} in
          let search_witness30 : (search) Ghost.t = {Ghost.ghost = ghost_ (search)} in
          let refine_ state_argument31 = state in
          let refine_ r = remember h_witness27 needle_witness28 d_witness29 trail p search_witness30 (refine_ state_argument31) in use (refine_ r)
        | Link q ->
          let state : {t : node Pref.token | marks_valid h.Ghost.ghost needle d.Ghost.ghost
            && trail === mark_trail d.Ghost.ghost && Pref.own t === marked_heap h.Ghost.ghost d.Ghost.ghost
            && H.mem h.Ghost.ghost q && active h.Ghost.ghost q} = refine_ state in
          let h_witness7 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
          let scope_witness8 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness7.Ghost.ghost x) || finite_scope h_witness7.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
          let unmarked_witness9 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h_witness7.Ghost.ghost x with
        None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked.Ghost.ghost)} in
          let d_witness10 : (marks) Ghost.t = {Ghost.ghost = ghost_ (d.Ghost.ghost)} in
          let refine_ state_argument11 = state in
          let resume_child : (child : {r : scanning | marks_valid h_witness7.Ghost.ghost needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap h_witness7.Ghost.ghost r.#marks
      && searched h_witness7.Ghost.ghost needle q r.#found r.#search}) @ unique ->
            {r : scanning | marks_valid goal.heap goal.needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap goal.heap r.#marks
      && searched goal.heap goal.needle goal.root r.#found r.#search} @ unique = fun child ->
            let refine_ child = child in
          let search = ghost_ (Follow (q, child.#search)) in
          let found = child.#found in
          ghost_ (searched_def h.Ghost.ghost needle p found search);
          let state = child.#state in let trail = child.#trail in
          let marks = ghost_ child.#marks in
          if found then
            let r = #{state; found; trail; marks; search} in use (refine_ r)
          else
            let state : {t : node Pref.token | marks_valid h.Ghost.ghost needle marks
              && trail === mark_trail marks && Pref.own t === marked_heap h.Ghost.ghost marks
              && searched h.Ghost.ghost needle p false search} = refine_ state in
            let h_witness32 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
            let needle_witness33 : (node Pref.t) Ghost.t = {Ghost.ghost = ghost_ (needle)} in
            let d_witness34 : (marks) Ghost.t = {Ghost.ghost = ghost_ (marks)} in
            let search_witness35 : (search) Ghost.t = {Ghost.ghost = ghost_ (search)} in
            let refine_ state_argument36 = state in
            let refine_ r = remember h_witness32 needle_witness33 d_witness34 trail p search_witness35 (refine_ state_argument36) in use (refine_ r) in
          scan_work goal h_witness7 scope_witness8 unmarked_witness9 needle q d_witness10 trail (refine_ state_argument11) resume_child
        | Arrow (a, b) ->
          let state : {t : node Pref.token | marks_valid h.Ghost.ghost needle d.Ghost.ghost
            && trail === mark_trail d.Ghost.ghost && Pref.own t === marked_heap h.Ghost.ghost d.Ghost.ghost
            && H.mem h.Ghost.ghost a && active h.Ghost.ghost a} = refine_ state in
          let h_witness12 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
          let scope_witness13 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness12.Ghost.ghost x) || finite_scope h_witness12.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
          let unmarked_witness14 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h_witness12.Ghost.ghost x with
        None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked.Ghost.ghost)} in
          let d_witness15 : (marks) Ghost.t = {Ghost.ghost = ghost_ (d.Ghost.ghost)} in
          let refine_ state_argument16 = state in
          let resume_left : (left : {r : scanning | marks_valid h_witness12.Ghost.ghost needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap h_witness12.Ghost.ghost r.#marks
      && searched h_witness12.Ghost.ghost needle a r.#found r.#search}) @ unique ->
            {r : scanning | marks_valid goal.heap goal.needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap goal.heap r.#marks
      && searched goal.heap goal.needle goal.root r.#found r.#search} @ unique = fun left ->
            let refine_ left = left in
          let state = left.#state in let trail = left.#trail in
          let marks = ghost_ left.#marks in
          if left.#found then
            let search = ghost_ (Left (a, b, left.#search)) in
            ghost_ (searched_def h.Ghost.ghost needle p true search);
            let r = #{state; found = true; trail; marks; search} in use (refine_ r)
          else
            let state : {t : node Pref.token | marks_valid h.Ghost.ghost needle marks
              && trail === mark_trail marks && Pref.own t === marked_heap h.Ghost.ghost marks
              && H.mem h.Ghost.ghost b && active h.Ghost.ghost b} = refine_ state in
            let h_witness17 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
            let scope_witness18 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness17.Ghost.ghost x) || finite_scope h_witness17.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
            let unmarked_witness19 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h_witness17.Ghost.ghost x with
        None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked.Ghost.ghost)} in
            let d_witness20 : (marks) Ghost.t = {Ghost.ghost = ghost_ (marks)} in
            let refine_ state_argument21 = state in
            let resume_right : (right : {r : scanning | marks_valid h_witness17.Ghost.ghost needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap h_witness17.Ghost.ghost r.#marks
      && searched h_witness17.Ghost.ghost needle b r.#found r.#search}) @ unique ->
              {r : scanning | marks_valid goal.heap goal.needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap goal.heap r.#marks
      && searched goal.heap goal.needle goal.root r.#found r.#search} @ unique = fun right ->
              let refine_ right = right in
            let search = ghost_ (Both (a, b, left.#search, right.#search)) in
            let found = right.#found in
            ghost_ (searched_def h.Ghost.ghost needle p found search);
            let state = right.#state in let trail = right.#trail in
            let marks = ghost_ right.#marks in
            if found then
              let r = #{state; found; trail; marks; search} in use (refine_ r)
            else
              let state : {t : node Pref.token | marks_valid h.Ghost.ghost needle marks
                && trail === mark_trail marks && Pref.own t === marked_heap h.Ghost.ghost marks
                && searched h.Ghost.ghost needle p false search} = refine_ state in
              let h_witness37 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
              let needle_witness38 : (node Pref.t) Ghost.t = {Ghost.ghost = ghost_ (needle)} in
              let d_witness39 : (marks) Ghost.t = {Ghost.ghost = ghost_ (marks)} in
              let search_witness40 : (search) Ghost.t = {Ghost.ghost = ghost_ (search)} in
              let refine_ state_argument41 = state in
              let refine_ r = remember h_witness37 needle_witness38 d_witness39 trail p search_witness40 (refine_ state_argument41) in use (refine_ r)
 in
            scan_work goal h_witness17 scope_witness18 unmarked_witness19 needle b d_witness20 trail (refine_ state_argument21) resume_right in
          scan_work goal h_witness12 scope_witness13 unmarked_witness14 needle a d_witness15 trail (refine_ state_argument16) resume_left
    end

let scan : (h : (node Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost x) || finite_scope h.Ghost.ghost x})) Ghost.t) @ total  ->(unmarked : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h.Ghost.ghost x with
        None -> true | Some v -> not v.visited})) Ghost.t) @ total  ->
    (needle : node Pref.t) @ immutable  -> (p : node Pref.t) @ immutable  ->(d : (marks) Ghost.t) @ immutable  -> (trail : trail) @ immutable  ->
    (state : {t : node Pref.token | marks_valid h.Ghost.ghost needle d.Ghost.ghost
      && trail === mark_trail d.Ghost.ghost && Pref.own t === marked_heap h.Ghost.ghost d.Ghost.ghost
      && H.mem h.Ghost.ghost p && active h.Ghost.ghost p}) @ unique  ->
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
      let refine_ r = r in refine_ r in
    let refine_ out = scan_work goal h scope unmarked needle p d trail state use in refine_ out

let rec reset : (h : (node Pref.heap) Ghost.t) @ immutable  ->
    (trail : trail) @ immutable  ->(members : (((x : node Pref.t) @ immutable ->
      {u : unit | not (on_trail trail x) || H.mem h.Ghost.ghost x})) Ghost.t) @ total  ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost}) @ unique  ->
    {t : node Pref.token | Pref.own t === reset_heap h.Ghost.ghost trail} @ unique = fun h trail members state  ->
    let refine_ state = state in
    ghost_ (reset_heap_def h.Ghost.ghost trail);
    match trail with
    | End -> refine_ state
    | Trail (p, rest) ->
      ghost_ (on_trail_def trail p; members.Ghost.ghost p);
      let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
      let refine_ old = Pref.read p (borrow_ state) in
      let refine_ state = state in
      let v = set_visited old false in
      let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
      let refine_ state = Pref.write p v state in
      let mid = ghost_ (H.put h.Ghost.ghost p v) in
      let tail : ((x : node Pref.t) @ immutable ->
        {u : unit | not (on_trail rest x) || H.mem mid x}) @ total ghost = ghost_ (fun x ->
        on_trail_def trail x; members.Ghost.ghost x; put_frame h.Ghost.ghost p v x;
        let u = () in refine_ u) in
      let state : {t : node Pref.token | Pref.own t === mid} = refine_ state in
      let h_witness1 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (mid)} in
      let members_witness2 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (on_trail rest x) || H.mem h_witness1.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ tail)} in
      let refine_ state_argument3 = state in
      let refine_ state = reset h_witness1 rest members_witness2 (refine_ state_argument3) in refine_ state

let occurs : (h : (node Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost x) || finite_scope h.Ghost.ghost x})) Ghost.t) @ total  ->(unmarked : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h.Ghost.ghost x with
        None -> true | Some v -> not v.visited})) Ghost.t) @ total  ->
    (needle : node Pref.t) @ immutable  -> (p : node Pref.t) @ immutable  ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && active h.Ghost.ghost p}) @ unique  ->
    {r : checked | marks_valid h.Ghost.ghost needle r.#marks
      && searched h.Ghost.ghost needle p r.#found r.#search
      && Pref.own r.#state ===
        reset_heap (marked_heap h.Ghost.ghost r.#marks) (mark_trail r.#marks)} @ unique = fun h scope unmarked needle p state  ->
    let refine_ state = state in
    let d = ghost_ No_marks in let trail = End in
    ghost_ (marks_valid_def h.Ghost.ghost needle d; marked_heap_def h.Ghost.ghost d; mark_trail_def d);
    let state : {t : node Pref.token | marks_valid h.Ghost.ghost needle d
      && trail === mark_trail d && Pref.own t === marked_heap h.Ghost.ghost d
      && H.mem h.Ghost.ghost p && active h.Ghost.ghost p} = refine_ state in
    let h_witness22 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
    let scope_witness23 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness22.Ghost.ghost x) || finite_scope h_witness22.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
    let unmarked_witness24 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h_witness22.Ghost.ghost x with
        None -> true | Some v -> not v.visited})) Ghost.t = {Ghost.ghost = ghost_ (refine_ unmarked.Ghost.ghost)} in
    let d_witness25 : (marks) Ghost.t = {Ghost.ghost = ghost_ (d)} in
    let refine_ state_argument26 = state in
    let refine_ found = scan h_witness22 scope_witness23 unmarked_witness24 needle p d_witness25 trail (refine_ state_argument26) in
    let d = ghost_ found.#marks in let trail = found.#trail in
    let mid = ghost_ (marked_heap h.Ghost.ghost d) in
    let members : ((x : node Pref.t) @ immutable ->
      {u : unit | not (on_trail trail x) || H.mem mid x}) @ total ghost = ghost_ (fun x ->
      let u = () in trail_members d x; initially_unmarked h.Ghost.ghost needle d x (refine_ u);
      marks_at h.Ghost.ghost needle d x (refine_ u); marked_at_def h.Ghost.ghost mid d x; refine_ u) in
    let state = found.#state in
    let state : {t : node Pref.token | Pref.own t === mid} = refine_ state in
    let h_witness4 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (mid)} in
    let members_witness5 : (((x : node Pref.t) @ immutable ->
      {u : unit | not (on_trail trail x) || H.mem h_witness4.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ members)} in
    let refine_ state_argument6 = state in
    let refine_ state = reset h_witness4 trail members_witness5 (refine_ state_argument6) in
    let r = #{state; found = found.#found; marks = d; search = found.#search} in refine_ r
