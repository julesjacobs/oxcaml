open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Effective_unifier_spec
module E = Effective_level
module R = Representative_level
module M = Effective_unifier_metadata

type goal = {heap : Pref.heap @@ ghost; left : node Pref.t @@ ghost; right : node Pref.t @@ ghost}

let rec raw_work : (goal : goal) @ immutable -> (h : Pref.heap Ghost.t) @ immutable -> (heads : E.heads Ghost.t) @ total ->
    (valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || E.effective_scope h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (unmarked : (((x : node Pref.t) @ immutable -> {u : unit | match H.at h.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t) @ total ->
    (order : (((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (trees : (((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (rp : {r : resolved | H.mem h.Ghost.ghost r.#value && active h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value && resolves h.Ghost.ghost p r.#value r.#path}) ->
    (sq : {r : resolved | H.mem h.Ghost.ghost r.#value && active h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value && resolves h.Ghost.ghost q r.#value r.#path}) ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost
      && E.effective_active h.Ghost.ghost heads.Ghost.ghost p && E.effective_active h.Ghost.ghost heads.Ghost.ghost q}) @ unique ->
    (use : ((answer : {out : result | unified h.Ghost.ghost p q out.#ok (Pref.own out.#state) out.#derivation}) @ unique -> {out : result | unified goal.heap goal.left goal.right out.#ok (Pref.own out.#state) out.#derivation} @ unique)) ->
    {out : result | unified goal.heap goal.left goal.right out.#ok (Pref.own out.#state) out.#derivation} @ unique = fun goal h heads valid scope unmarked order trees p q rp sq state use ->
  ghost_ (E.effective_active_def h.Ghost.ghost heads.Ghost.ghost p; E.effective_active_def h.Ghost.ghost heads.Ghost.ghost q);
  let r = rp.#value in let s = sq.#value in
  let finish : (answer : {out : result | unified h.Ghost.ghost r s out.#ok (Pref.own out.#state) out.#derivation}) @ unique -> {out : result | unified goal.heap goal.left goal.right out.#ok (Pref.own out.#state) out.#derivation} @ unique = fun answer ->
    let ok = answer.#ok in let after = ghost_ (Pref.own (borrow_ answer.#state)) in
    let derivation = ghost_ (Resolve (r, s, rp.#path, sq.#path, answer.#derivation)) in
    ghost_ (unified_def h.Ghost.ghost p q ok after derivation);
    let out = #{ok; state = answer.#state; derivation} in use (out) in
  let equal = Pref.equal r s in
  if equal then (
    let old = ghost_ U.Same in let d = ghost_ (Base old) in let ok = true in
    ghost_ (U.unified_def h.Ghost.ghost r s ok h.Ghost.ghost old; unified_def h.Ghost.ghost r s ok h.Ghost.ghost d);
    finish (#{ok; state; derivation = d}))
  else (
    let left : {v : node | H.at h.Ghost.ghost r === Some v} =
      let borrowed = borrow_ state in let v = Pref.read r borrowed in v in let right : {v : node | H.at h.Ghost.ghost s === Some v} =
      let borrowed = borrow_ state in let v = Pref.read s borrowed in v in ghost_ (observe_def h.Ghost.ghost r; observe_def h.Ghost.ghost s);
    match left.desc, right.desc with
    | Var, _ ->
      let state : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost r && H.mem h.Ghost.ghost s
        && active h.Ghost.ghost r && active h.Ghost.ghost s && observe h.Ghost.ghost r === Some Var && terminal h.Ghost.ghost s && not (r === s)} = state in
      let out = Effective_bind.bind h heads valid scope unmarked order trees r s (state) in finish (out)
    | _, Var ->
      let state : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost s && H.mem h.Ghost.ghost r
        && active h.Ghost.ghost s && active h.Ghost.ghost r && observe h.Ghost.ghost s === Some Var && terminal h.Ghost.ghost r && not (s === r)} = state in
      let out = Effective_bind.bind h heads valid scope unmarked order trees s r (state) in
      let ok = out.#ok in let after = ghost_ (Pref.own (borrow_ out.#state)) in let derivation = ghost_ (Swap out.#derivation) in
      ghost_ (unified_def h.Ghost.ghost r s ok after derivation);
      finish (#{ok; state = out.#state; derivation})
    | Bool, Bool ->
      let old = ghost_ U.Constants in let d = ghost_ (Base old) in let ok = true in
      ghost_ (U.unified_def h.Ghost.ghost r s ok h.Ghost.ghost old; unified_def h.Ghost.ghost r s ok h.Ghost.ghost d);
      finish (#{ok; state; derivation = d})
    | Bool, Arrow _ | Arrow _, Bool ->
      let old = ghost_ U.Clash in let d = ghost_ (Base old) in let ok = false in
      ghost_ (U.unified_def h.Ghost.ghost r s ok h.Ghost.ghost old; unified_def h.Ghost.ghost r s ok h.Ghost.ghost d);
      finish (#{ok; state; derivation = d})
    | Link _, _ | _, Link _ ->
      ghost_ (terminal_def h.Ghost.ghost r; terminal_def h.Ghost.ghost s; let _ : {u : unit | false} = () in ()); assert false
    | Arrow (a, b), Arrow (c, e) ->
      ghost_ (
        valid.Ghost.ghost r; valid.Ghost.ghost s;
        scope.Ghost.ghost r; scope.Ghost.ghost s;
        E.terminal_children h.Ghost.ghost heads.Ghost.ghost r a b ();
        E.terminal_children h.Ghost.ghost heads.Ghost.ghost s c e ());
      let state : {t : Pref.token | Pref.own t === h.Ghost.ghost && E.effective_active h.Ghost.ghost heads.Ghost.ghost a && E.effective_active h.Ghost.ghost heads.Ghost.ghost c} = state in
      let resume_left : (answer : {out : result | unified h.Ghost.ghost a c out.#ok (Pref.own out.#state) out.#derivation}) @ unique -> {out : result | unified goal.heap goal.left goal.right out.#ok (Pref.own out.#state) out.#derivation} @ unique = fun answer ->
        let left_ok = answer.#ok in let ld = ghost_ answer.#derivation in
        let middle = ghost_ (Pref.own (borrow_ answer.#state)) in
        if not left_ok then (
          let ok = false in let d = ghost_ (Children (a, b, c, e, middle, left_ok, ld, Base U.Same)) in
          ghost_ (unified_def h.Ghost.ghost r s ok middle d);
          finish (#{ok; state = answer.#state; derivation = d}))
        else (
          let mh : Pref.heap Ghost.t = {Ghost.ghost = ghost_ middle} in
          let mid_raw : (((x : node Pref.t) @ immutable total -> {v : R.representative | not (H.mem mh.Ghost.ghost x) || resolves mh.Ghost.ghost x v.root v.path} @ immutable total)) @ ghost =
            ghost_ (fun x -> valid.Ghost.ghost x; let out = M.head h.Ghost.ghost a c left_ok mh.Ghost.ghost ld heads.Ghost.ghost x () in out) in
          let[@def] mid_fn : E.heads @ ghost = ghost_ (fun x -> let out = mid_raw x in out) in
          let mid_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ mid_fn} in
          let mid_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head mh.Ghost.ghost mid_heads.Ghost.ghost x})) Ghost.t =
            {Ghost.ghost = ghost_ (fun x -> mid_fn_def x; let _ = mid_raw x in
              E.valid_head_def mh.Ghost.ghost mid_heads.Ghost.ghost x; ())} in
          let mid_order : (((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered mh.Ghost.ghost mid_heads.Ghost.ghost x})) Ghost.t =
            {Ghost.ghost = ghost_ (fun x -> Effective_unifier_order.ordered h.Ghost.ghost a c left_ok mh.Ghost.ghost ld heads.Ghost.ghost mid_heads.Ghost.ghost valid.Ghost.ghost mid_valid.Ghost.ghost order.Ghost.ghost x (); ())} in
          let mid_scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem mh.Ghost.ghost x) || E.effective_scope mh.Ghost.ghost mid_heads.Ghost.ghost x})) Ghost.t =
            {Ghost.ghost = ghost_ (fun x -> scope.Ghost.ghost x; E.effective_scope_def h.Ghost.ghost heads.Ghost.ghost x;
              M.source h.Ghost.ghost a c left_ok mh.Ghost.ghost ld trees.Ghost.ghost x ();
              mid_order.Ghost.ghost x; if H.mem mh.Ghost.ghost x then (E.ordered_scope mh.Ghost.ghost mid_heads.Ghost.ghost mid_valid.Ghost.ghost x (); ()) else (); ())} in
          let mid_trees : (((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x && (if H.mem mh.Ghost.ghost x then finite mh.Ghost.ghost t else observe mh.Ghost.ghost x === None)} @ immutable)) Ghost.t =
            {Ghost.ghost = ghost_ (fun x -> let out = Effective_unifier_finite.unified_finite_at h.Ghost.ghost trees.Ghost.ghost a c left_ok mh.Ghost.ghost ld x () in out)} in
          let mid_unmarked : (((x : node Pref.t) @ immutable -> {u : unit | match H.at mh.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t =
            {Ghost.ghost = ghost_ (fun x -> unmarked.Ghost.ghost x; M.cells h.Ghost.ghost a c left_ok mh.Ghost.ghost ld x (); M.cell_frame_def h.Ghost.ghost mh.Ghost.ghost x; ())} in
          ghost_ (valid.Ghost.ghost b; valid.Ghost.ghost e; mid_valid.Ghost.ghost b; mid_valid.Ghost.ghost e; M.active h.Ghost.ghost a c left_ok mh.Ghost.ghost ld heads.Ghost.ghost mid_heads.Ghost.ghost b ();
            M.active h.Ghost.ghost a c left_ok mh.Ghost.ghost ld heads.Ghost.ghost mid_heads.Ghost.ghost e ());
          let state : {t : Pref.token | Pref.own t === mh.Ghost.ghost && E.effective_active mh.Ghost.ghost mid_heads.Ghost.ghost b && E.effective_active mh.Ghost.ghost mid_heads.Ghost.ghost e} = answer.#state in
          let resume_right : (right : {out : result | unified mh.Ghost.ghost b e out.#ok (Pref.own out.#state) out.#derivation}) @ unique -> {out : result | unified goal.heap goal.left goal.right out.#ok (Pref.own out.#state) out.#derivation} @ unique = fun right ->
            let ok = right.#ok in let after = ghost_ (Pref.own (borrow_ right.#state)) in
            let d = ghost_ (Children (a, b, c, e, middle, left_ok, ld, right.#derivation)) in
            ghost_ (unified_def h.Ghost.ghost r s ok after d);
            if not ok then finish (#{ok; state = right.#state; derivation = d})
            else (
          let ah : Pref.heap Ghost.t = {Ghost.ghost = ghost_ after} in
          let final_raw : (((x : node Pref.t) @ immutable total -> {v : R.representative | not (H.mem ah.Ghost.ghost x) || resolves ah.Ghost.ghost x v.root v.path} @ immutable total)) @ ghost =
            ghost_ (fun x -> valid.Ghost.ghost x; let out = M.head h.Ghost.ghost r s ok ah.Ghost.ghost d heads.Ghost.ghost x () in out) in
          let[@def] final_fn : E.heads @ ghost = ghost_ (fun x -> let out = final_raw x in out) in
          let final_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ final_fn} in
          let final_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head ah.Ghost.ghost final_heads.Ghost.ghost x})) Ghost.t =
            {Ghost.ghost = ghost_ (fun x -> final_fn_def x; let _ = final_raw x in
              E.valid_head_def ah.Ghost.ghost final_heads.Ghost.ghost x; ())} in
          let final_order : (((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered ah.Ghost.ghost final_heads.Ghost.ghost x})) Ghost.t =
            {Ghost.ghost = ghost_ (fun x -> Effective_unifier_order.ordered h.Ghost.ghost r s ok ah.Ghost.ghost d heads.Ghost.ghost final_heads.Ghost.ghost valid.Ghost.ghost final_valid.Ghost.ghost order.Ghost.ghost x (); ())} in
          let final_scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem ah.Ghost.ghost x) || E.effective_scope ah.Ghost.ghost final_heads.Ghost.ghost x})) Ghost.t =
            {Ghost.ghost = ghost_ (fun x -> scope.Ghost.ghost x; E.effective_scope_def h.Ghost.ghost heads.Ghost.ghost x;
              M.source h.Ghost.ghost r s ok ah.Ghost.ghost d trees.Ghost.ghost x ();
              final_order.Ghost.ghost x; if H.mem ah.Ghost.ghost x then (E.ordered_scope ah.Ghost.ghost final_heads.Ghost.ghost final_valid.Ghost.ghost x (); ()) else (); ())} in
          let final_trees : (((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x && (if H.mem ah.Ghost.ghost x then finite ah.Ghost.ghost t else observe ah.Ghost.ghost x === None)} @ immutable)) Ghost.t =
            {Ghost.ghost = ghost_ (fun x -> let out = Effective_unifier_finite.unified_finite_at h.Ghost.ghost trees.Ghost.ghost r s ok ah.Ghost.ghost d x () in out)} in
              ghost_ (valid.Ghost.ghost r; valid.Ghost.ghost s; final_valid.Ghost.ghost r; final_valid.Ghost.ghost s; M.active h.Ghost.ghost r s ok ah.Ghost.ghost d heads.Ghost.ghost final_heads.Ghost.ghost r ();
                M.active h.Ghost.ghost r s ok ah.Ghost.ghost d heads.Ghost.ghost final_heads.Ghost.ghost s ());
              let source : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem ah.Ghost.ghost x) || source_ok ah.Ghost.ghost x})) Ghost.t =
                {Ghost.ghost = ghost_ (fun x -> final_scope.Ghost.ghost x; E.effective_scope_def ah.Ghost.ghost final_heads.Ghost.ghost x; ())} in
              ghost_ (E.effective_active_def ah.Ghost.ghost final_heads.Ghost.ghost r;
                E.effective_active_def ah.Ghost.ghost final_heads.Ghost.ghost s);
              let state : {t : Pref.token | Pref.own t === ah.Ghost.ghost && unified h.Ghost.ghost r s true ah.Ghost.ghost d
                && H.mem ah.Ghost.ghost r && H.mem ah.Ghost.ghost s && E.effective_active ah.Ghost.ghost final_heads.Ghost.ghost r && E.effective_active ah.Ghost.ghost final_heads.Ghost.ghost s} = right.#state in
              let dh : derivation Ghost.t = {Ghost.ghost = ghost_ d} in
              let linked = Effective_link.finish h r s ah final_heads final_valid dh final_trees source (state) in finish (linked)) in
          work goal mh mid_heads mid_valid mid_scope mid_unmarked mid_order mid_trees b e (state) resume_right) in
      work goal h heads valid scope unmarked order trees a c (state) resume_left)

and work : (goal : goal) @ immutable -> (h : Pref.heap Ghost.t) @ immutable -> (heads : E.heads Ghost.t) @ total ->
    (valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || E.effective_scope h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (unmarked : (((x : node Pref.t) @ immutable -> {u : unit | match H.at h.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t) @ total ->
    (order : (((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (trees : (((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost
      && E.effective_active h.Ghost.ghost heads.Ghost.ghost p && E.effective_active h.Ghost.ghost heads.Ghost.ghost q}) @ unique ->
    (use : ((answer : {out : result | unified h.Ghost.ghost p q out.#ok (Pref.own out.#state) out.#derivation}) @ unique -> {out : result | unified goal.heap goal.left goal.right out.#ok (Pref.own out.#state) out.#derivation} @ unique)) ->
    {out : result | unified goal.heap goal.left goal.right out.#ok (Pref.own out.#state) out.#derivation} @ unique = fun goal h heads valid scope unmarked order trees p q state use ->
  ghost_ (E.effective_active_def h.Ghost.ghost heads.Ghost.ghost p; E.effective_active_def h.Ghost.ghost heads.Ghost.ghost q);
  let source : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || source_ok h.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> scope.Ghost.ghost x; E.effective_scope_def h.Ghost.ghost heads.Ghost.ghost x; ())} in
  let state : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p
    && (match E.level h.Ghost.ghost heads.Ghost.ghost p with Generic -> false | Finite n -> n >= 0)} = state in
  let first = Effective_compressed_representative.representative h heads valid source p (state) in
  let edits1 = ghost_ first.#edits in
  let h1 : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ first.#state))} in
  let one_raw : ((x : node Pref.t) @ immutable total -> {r : R.representative | not (H.mem h1.Ghost.ghost x) || resolves h1.Ghost.ghost x r.root r.path} @ immutable total) @ ghost =
    ghost_ (fun x -> valid.Ghost.ghost x; let r = Effective_compression_metadata.head h.Ghost.ghost h1.Ghost.ghost edits1 heads.Ghost.ghost x () in r) in
  let[@def] one_fn : E.heads @ ghost = ghost_ (fun x -> let r = one_raw x in r) in
  let one_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ one_fn} in
  let one_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h1.Ghost.ghost one_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> one_fn_def x; let _ = one_raw x in E.valid_head_def h1.Ghost.ghost one_heads.Ghost.ghost x; ())} in
  let one_order : (((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered h1.Ghost.ghost one_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> order.Ghost.ghost x; Effective_compression_metadata.ordered h.Ghost.ghost h1.Ghost.ghost edits1 heads.Ghost.ghost one_heads.Ghost.ghost valid.Ghost.ghost one_valid.Ghost.ghost x (); ())} in
  let one_source : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1.Ghost.ghost x) || source_ok h1.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> Effective_compression_proofs.source h.Ghost.ghost h1.Ghost.ghost edits1 source.Ghost.ghost x (); ())} in
  let one_unmarked : (((x : node Pref.t) @ immutable -> {u : unit | match H.at h1.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> unmarked.Ghost.ghost x; Effective_compression_proofs.frame h.Ghost.ghost h1.Ghost.ghost edits1 x (); ())} in
  let one_trees : (((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x && (if H.mem h1.Ghost.ghost x then finite h1.Ghost.ghost t else observe h1.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let t = trees.Ghost.ghost x in Effective_compression_proofs.frame h.Ghost.ghost h1.Ghost.ghost edits1 x ();
      if H.mem h.Ghost.ghost x then (let out = Effective_compression_proofs.finite h.Ghost.ghost h1.Ghost.ghost edits1 t () in out)
      else (observe_def h.Ghost.ghost x; observe_def h1.Ghost.ghost x; t))} in
  ghost_ (valid.Ghost.ghost q; one_valid.Ghost.ghost q; Effective_compression_metadata.level h.Ghost.ghost h1.Ghost.ghost edits1 heads.Ghost.ghost one_heads.Ghost.ghost q ();
    Effective_compression_proofs.frame h.Ghost.ghost h1.Ghost.ghost edits1 q ());
  let state : {t : Pref.token | Pref.own t === h1.Ghost.ghost && H.mem h1.Ghost.ghost q
    && (match E.level h1.Ghost.ghost one_heads.Ghost.ghost q with Generic -> false | Finite n -> n >= 0)} = first.#state in
  let second = Effective_compressed_representative.representative h1 one_heads one_valid one_source q (state) in
  let edits2 = ghost_ second.#edits in
  let h2 : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ second.#state))} in
  let two_raw : ((x : node Pref.t) @ immutable total -> {r : R.representative | not (H.mem h2.Ghost.ghost x) || resolves h2.Ghost.ghost x r.root r.path} @ immutable total) @ ghost =
    ghost_ (fun x -> one_valid.Ghost.ghost x; let r = Effective_compression_metadata.head h1.Ghost.ghost h2.Ghost.ghost edits2 one_heads.Ghost.ghost x () in r) in
  let[@def] two_fn : E.heads @ ghost = ghost_ (fun x -> let r = two_raw x in r) in
  let two_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ two_fn} in
  let two_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h2.Ghost.ghost two_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> two_fn_def x; let _ = two_raw x in E.valid_head_def h2.Ghost.ghost two_heads.Ghost.ghost x; ())} in
  let two_order : (((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered h2.Ghost.ghost two_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> one_order.Ghost.ghost x; Effective_compression_metadata.ordered h1.Ghost.ghost h2.Ghost.ghost edits2 one_heads.Ghost.ghost two_heads.Ghost.ghost one_valid.Ghost.ghost two_valid.Ghost.ghost x (); ())} in
  let two_source : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2.Ghost.ghost x) || source_ok h2.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> Effective_compression_proofs.source h1.Ghost.ghost h2.Ghost.ghost edits2 one_source.Ghost.ghost x (); ())} in
  let two_scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2.Ghost.ghost x) || E.effective_scope h2.Ghost.ghost two_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> two_source.Ghost.ghost x; two_order.Ghost.ghost x; if H.mem h2.Ghost.ghost x then (E.ordered_scope h2.Ghost.ghost two_heads.Ghost.ghost two_valid.Ghost.ghost x (); ()) else (); ())} in
  let two_unmarked : (((x : node Pref.t) @ immutable -> {u : unit | match H.at h2.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> one_unmarked.Ghost.ghost x; Effective_compression_proofs.frame h1.Ghost.ghost h2.Ghost.ghost edits2 x (); ())} in
  let two_trees : (((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x && (if H.mem h2.Ghost.ghost x then finite h2.Ghost.ghost t else observe h2.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let t = one_trees.Ghost.ghost x in Effective_compression_proofs.frame h1.Ghost.ghost h2.Ghost.ghost edits2 x ();
      if H.mem h1.Ghost.ghost x then (let out = Effective_compression_proofs.finite h1.Ghost.ghost h2.Ghost.ghost edits2 t () in out)
      else (observe_def h1.Ghost.ghost x; observe_def h2.Ghost.ghost x; t))} in
  ghost_ (valid.Ghost.ghost p; valid.Ghost.ghost q; one_valid.Ghost.ghost p; one_valid.Ghost.ghost q; two_valid.Ghost.ghost p; two_valid.Ghost.ghost q; Effective_compression_metadata.level h.Ghost.ghost h1.Ghost.ghost edits1 heads.Ghost.ghost one_heads.Ghost.ghost p ();
    Effective_compression_metadata.level h.Ghost.ghost h1.Ghost.ghost edits1 heads.Ghost.ghost one_heads.Ghost.ghost q ();
    Effective_compression_metadata.level h1.Ghost.ghost h2.Ghost.ghost edits2 one_heads.Ghost.ghost two_heads.Ghost.ghost p ();
    Effective_compression_metadata.level h1.Ghost.ghost h2.Ghost.ghost edits2 one_heads.Ghost.ghost two_heads.Ghost.ghost q ();
    Effective_compression_proofs.frame h.Ghost.ghost h1.Ghost.ghost edits1 p ();
    Effective_compression_proofs.frame h1.Ghost.ghost h2.Ghost.ghost edits2 p ();
    Effective_compression_proofs.frame h1.Ghost.ghost h2.Ghost.ghost edits2 q ();
    E.effective_active_def h2.Ghost.ghost two_heads.Ghost.ghost p; E.effective_active_def h2.Ghost.ghost two_heads.Ghost.ghost q);
  let state : {t : Pref.token | Pref.own t === h2.Ghost.ghost && E.effective_active h2.Ghost.ghost two_heads.Ghost.ghost p && E.effective_active h2.Ghost.ghost two_heads.Ghost.ghost q} = second.#state in
  let resume : (answer : {out : result | unified h2.Ghost.ghost p q out.#ok (Pref.own out.#state) out.#derivation}) @ unique ->
    {out : result | unified goal.heap goal.left goal.right out.#ok (Pref.own out.#state) out.#derivation} @ unique = fun answer ->
      let after = ghost_ (Pref.own (borrow_ answer.#state)) in let ok = answer.#ok in
      let d2 = ghost_ (Pre_compress (h2.Ghost.ghost, edits2, answer.#derivation)) in
      let d1 = ghost_ (Pre_compress (h1.Ghost.ghost, edits1, d2)) in
      ghost_ (unified_def h1.Ghost.ghost p q ok after d2; unified_def h.Ghost.ghost p q ok after d1);
      let out = #{ok; state = answer.#state; derivation = d1} in use (out) in
  let rp : {r : resolved | H.mem h2.Ghost.ghost r.#value && active h2.Ghost.ghost r.#value && terminal h2.Ghost.ghost r.#value && resolves h2.Ghost.ghost p r.#value r.#path} =
    let root = first.#value in
    let path : {d : resolution | resolves h2.Ghost.ghost p root d} @ immutable ghost = ghost_ (
      let mid_path = Effective_compression_proofs.resolution h.Ghost.ghost h1.Ghost.ghost edits1 p root first.#path () in
      let path = Effective_compression_proofs.resolution h1.Ghost.ghost h2.Ghost.ghost edits2 p root mid_path () in path) in
    ghost_ (Effective_compression_proofs.frame h1.Ghost.ghost h2.Ghost.ghost edits2 root ();
      active_def h1.Ghost.ghost root; active_def h2.Ghost.ghost root;
      Compression_path_proofs.resolution_terminal h2.Ghost.ghost p root path ());
    let out = #{value = root; path} in out in
  let sq : {r : resolved | H.mem h2.Ghost.ghost r.#value && active h2.Ghost.ghost r.#value && terminal h2.Ghost.ghost r.#value && resolves h2.Ghost.ghost q r.#value r.#path} =
    let root = second.#value in
    let path : {d : resolution | resolves h2.Ghost.ghost q root d} @ immutable ghost = ghost_ (
      let path = Effective_compression_proofs.resolution h1.Ghost.ghost h2.Ghost.ghost edits2 q root second.#path () in path) in
    ghost_ (Effective_compression_proofs.frame h1.Ghost.ghost h2.Ghost.ghost edits2 root ();
      active_def h1.Ghost.ghost root; active_def h2.Ghost.ghost root;
      Compression_path_proofs.resolution_terminal h2.Ghost.ghost q root path ());
    let out = #{value = root; path} in out in
  raw_work goal h2 two_heads two_valid two_scope two_unmarked two_order two_trees p q rp sq (state) resume

let unify : (h : Pref.heap Ghost.t) @ immutable -> (heads : E.heads Ghost.t) @ total ->
    (valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || E.effective_scope h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (unmarked : (((x : node Pref.t) @ immutable -> {u : unit | match H.at h.Ghost.ghost x with None -> true | Some v -> not v.visited})) Ghost.t) @ total ->
    (order : (((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (trees : (((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost
      && E.effective_active h.Ghost.ghost heads.Ghost.ghost p && E.effective_active h.Ghost.ghost heads.Ghost.ghost q}) @ unique ->
    {out : result | unified h.Ghost.ghost p q out.#ok (Pref.own out.#state) out.#derivation} @ unique = fun h heads valid scope unmarked order trees p q state ->
  let goal = {heap = h.Ghost.ghost; left = p; right = q} in
  let use : (answer : {out : result | unified h.Ghost.ghost p q out.#ok (Pref.own out.#state) out.#derivation}) @ unique -> {out : result | unified goal.heap goal.left goal.right out.#ok (Pref.own out.#state) out.#derivation} @ unique = fun answer -> answer in
  let out = work goal h heads valid scope unmarked order trees p q state use in out
