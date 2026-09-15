open Copy_spec
open Copy_heap_proofs
open Level_spec
open Level_unifier_spec
open Marked_occurs_proofs

let remember : (h : node Pref.heap) @ immutable ghost ->
    (needle : node Pref.t) @ immutable ghost ->
    (d : marks) @ immutable ghost -> (trail : trail) @ immutable ->
    (p : node Pref.t) @ immutable -> (search : search) @ immutable ghost ->
    (state : {t : node Pref.token | marks_valid h needle d
      && trail === mark_trail d && Pref.own t === marked_heap h d
      && searched h needle p false search}) @ unique ->
    {r : scanning | marks_valid h needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap h r.#marks
      && not r.#found && searched h needle p r.#found r.#search} @ unique =
  fun h needle d trail p search state ->
    let refine_ state = state in
    ghost_ (let u = () in marks_at h needle d p (refine_ u);
      let current = marked_heap h d in marked_at_def h current d p;
      searched_def h needle p false search);
    let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
    let refine_ old = Pref.read p (borrow_ state) in let refine_ state = state in
    if old.visited then
      let r = #{state; found = false; trail; marks = d; search} in refine_ r
    else
      let v = set_visited old true in
      let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
      let refine_ state = Pref.write p v state in
      let marks = ghost_ (Marked (d, p, old, search)) in
      let trail = Trail (p, trail) in
      ghost_ (marks_valid_def h needle marks; marked_heap_def h marks;
        mark_trail_def marks);
      let r = #{state; found = false; trail; marks; search} in refine_ r

let rec scan : (h : node Pref.heap) @ immutable ghost ->
    (scope : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h x) || finite_scope h x})) @ total ghost ->
    (unmarked : ((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h x with
        None -> true | Some v -> not v.visited})) @ total ghost ->
    (needle : node Pref.t) @ immutable -> (p : node Pref.t) @ immutable ->
    (d : marks) @ immutable ghost -> (trail : trail) @ immutable ->
    (state : {t : node Pref.token | marks_valid h needle d
      && trail === mark_trail d && Pref.own t === marked_heap h d
      && H.mem h p && active h p}) @ unique ->
    {r : scanning | marks_valid h needle r.#marks
      && r.#trail === mark_trail r.#marks
      && Pref.own r.#state === marked_heap h r.#marks
      && searched h needle p r.#found r.#search} @ unique =
  fun h scope unmarked needle p d trail state ->
    let refine_ state = state in
    let refine_ same = Pref.equal needle p in
    if same then
      let search = ghost_ Hit in
      ghost_ (searched_def h needle p true search);
      let r = #{state; found = true; trail; marks = d; search} in refine_ r
    else begin
      ghost_ (let u = () in marks_at h needle d p (refine_ u);
        let current = marked_heap h d in marked_at_def h current d p;
        scope p; unmarked p; finite_scope_def h p; source_ok_def h p;
        observe_def h p);
      let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
      let refine_ old = Pref.read p (borrow_ state) in let refine_ state = state in
      if old.visited then
        let refine_ search = ghost_ (let u = () in
          cached_search h needle d p (refine_ u)) in
        let r = #{state; found = false; trail; marks = d; search} in refine_ r
      else
        match old.desc with
        | Var | Bool ->
          let search = ghost_ Leaf in
          ghost_ (searched_def h needle p false search);
          let state : {t : node Pref.token | marks_valid h needle d
            && trail === mark_trail d && Pref.own t === marked_heap h d
            && searched h needle p false search} = refine_ state in
          let refine_ r = remember h needle d trail p search state in refine_ r
        | Link q ->
          let state : {t : node Pref.token | marks_valid h needle d
            && trail === mark_trail d && Pref.own t === marked_heap h d
            && H.mem h q && active h q} = refine_ state in
          let refine_ child = scan h scope unmarked needle q d trail state in
          let search = ghost_ (Follow (q, child.#search)) in
          let found = child.#found in
          ghost_ (searched_def h needle p found search);
          let state = child.#state in let trail = child.#trail in
          let marks = ghost_ child.#marks in
          if found then
            let r = #{state; found; trail; marks; search} in refine_ r
          else
            let state : {t : node Pref.token | marks_valid h needle marks
              && trail === mark_trail marks && Pref.own t === marked_heap h marks
              && searched h needle p false search} = refine_ state in
            let refine_ r = remember h needle marks trail p search state in refine_ r
        | Arrow (a, b) ->
          let state : {t : node Pref.token | marks_valid h needle d
            && trail === mark_trail d && Pref.own t === marked_heap h d
            && H.mem h a && active h a} = refine_ state in
          let refine_ left = scan h scope unmarked needle a d trail state in
          let state = left.#state in let trail = left.#trail in
          let marks = ghost_ left.#marks in
          if left.#found then
            let search = ghost_ (Left (a, b, left.#search)) in
            ghost_ (searched_def h needle p true search);
            let r = #{state; found = true; trail; marks; search} in refine_ r
          else
            let state : {t : node Pref.token | marks_valid h needle marks
              && trail === mark_trail marks && Pref.own t === marked_heap h marks
              && H.mem h b && active h b} = refine_ state in
            let refine_ right = scan h scope unmarked needle b marks trail state in
            let search = ghost_ (Both (a, b, left.#search, right.#search)) in
            let found = right.#found in
            ghost_ (searched_def h needle p found search);
            let state = right.#state in let trail = right.#trail in
            let marks = ghost_ right.#marks in
            if found then
              let r = #{state; found; trail; marks; search} in refine_ r
            else
              let state : {t : node Pref.token | marks_valid h needle marks
                && trail === mark_trail marks && Pref.own t === marked_heap h marks
                && searched h needle p false search} = refine_ state in
              let refine_ r = remember h needle marks trail p search state in refine_ r

    end

let rec reset : (h : node Pref.heap) @ immutable ghost ->
    (trail : trail) @ immutable ->
    (members : ((x : node Pref.t) @ immutable ->
      {u : unit | not (on_trail trail x) || H.mem h x})) @ total ghost ->
    (state : {t : node Pref.token | Pref.own t === h}) @ unique ->
    {t : node Pref.token | Pref.own t === reset_heap h trail} @ unique =
  fun h trail members state ->
    let refine_ state = state in
    ghost_ (reset_heap_def h trail);
    match trail with
    | End -> refine_ state
    | Trail (p, rest) ->
      ghost_ (on_trail_def trail p; members p);
      let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
      let refine_ old = Pref.read p (borrow_ state) in
      let refine_ state = state in
      let v = set_visited old false in
      let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
      let refine_ state = Pref.write p v state in
      let mid = ghost_ (H.put h p v) in
      let tail : ((x : node Pref.t) @ immutable ->
        {u : unit | not (on_trail rest x) || H.mem mid x}) @ total ghost = ghost_ (fun x ->
        on_trail_def trail x; members x; put_frame h p v x;
        let u = () in refine_ u) in
      let state : {t : node Pref.token | Pref.own t === mid} = refine_ state in
      let refine_ state = reset mid rest tail state in refine_ state

let occurs : (h : node Pref.heap) @ immutable ghost ->
    (scope : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h x) || finite_scope h x})) @ total ghost ->
    (unmarked : ((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h x with
        None -> true | Some v -> not v.visited})) @ total ghost ->
    (needle : node Pref.t) @ immutable -> (p : node Pref.t) @ immutable ->
    (state : {t : node Pref.token | Pref.own t === h && H.mem h p && active h p}) @ unique ->
    {r : checked | marks_valid h needle r.#marks
      && searched h needle p r.#found r.#search
      && Pref.own r.#state ===
        reset_heap (marked_heap h r.#marks) (mark_trail r.#marks)} @ unique =
  fun h scope unmarked needle p state ->
    let refine_ state = state in
    let d = ghost_ No_marks in let trail = End in
    ghost_ (marks_valid_def h needle d; marked_heap_def h d; mark_trail_def d);
    let state : {t : node Pref.token | marks_valid h needle d
      && trail === mark_trail d && Pref.own t === marked_heap h d
      && H.mem h p && active h p} = refine_ state in
    let refine_ found = scan h scope unmarked needle p d trail state in
    let d = ghost_ found.#marks in let trail = found.#trail in
    let mid = ghost_ (marked_heap h d) in
    let members : ((x : node Pref.t) @ immutable ->
      {u : unit | not (on_trail trail x) || H.mem mid x}) @ total ghost = ghost_ (fun x ->
      let u = () in trail_members d x; initially_unmarked h needle d x (refine_ u);
      marks_at h needle d x (refine_ u); marked_at_def h mid d x; refine_ u) in
    let state = found.#state in
    let state : {t : node Pref.token | Pref.own t === mid} = refine_ state in
    let refine_ state = reset mid trail members state in
    let r = #{state; found = found.#found; marks = d; search = found.#search} in refine_ r
