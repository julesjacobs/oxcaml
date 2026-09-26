(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml copy_demo.ml";
 { bytecode; }
*)
open Copy_spec
open Copy_heap_proofs
open Copy_model_proofs
open Copy_template_proofs

let read : (h : node Pref.heap) @ immutable ghost ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None})) @ total ghost ->
    (p : {p : node Pref.t | H.mem h p}) @ immutable ->
    (t : {t : node Pref.token | Pref.own t === h}) @ local read ->
    {v : node | let refine_ p = p in Some v === H.at h p && payload_scoped h v} @ immutable =
  fun h scope p t -> let refine_ p = p in let refine_ t = t in
    ghost_ (scope p; source_ok_def h p);
    let t : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ t in
    let refine_ v = Pref.read p t in ghost_ (payload_scoped_def h v); refine_ v

let () =
  let refine_ state = Pref.empty () in
  let generic = {desc = Var; level = Generic; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc generic state in let a = step.value in let state = step.state in
  let finite = cell Var 0 in let refine_ step = Pref.alloc finite state in
  let boundary = step.value in let state = step.state in
  let inner = {desc = Arrow (a, a); level = Generic; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc inner state in let pair = step.value in let state = step.state in
  let outer = {desc = Arrow (pair, boundary); level = Generic; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc outer state in let root = step.value in let state = step.state in
  let alias = {desc = Link root; level = Generic; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc alias state in let link = step.value in let state = step.state in
  let saved = ghost_ (Pref.own (borrow_ state)) in
  let scope : ((x : node Pref.t) @ immutable -> {u : unit |
      if H.mem saved x then source_ok saved x else H.at saved x === None}) @ total ghost = ghost_ (fun x ->
    let desc = Var in cell_def desc 0; source_ok_def saved x;
    let u = () in refine_ u) in
  let trees : ((x : node Pref.t) @ immutable -> {t : template | not (H.mem saved x) ||
      (Copy_spec.root t === x && template saved t)} @ immutable) @ total ghost = ghost_ (fun x ->
    let ta = Parameter a in let tb = Boundary boundary in let ti = Product (pair, ta, ta) in
    let to_ = Product (root, ti, tb) in let tl = Indirect (link, to_) in
    let desc = Var in cell_def desc 0;
    generic_desc_def saved a desc; finite_node_def saved boundary;
    generic_desc_def saved pair inner.desc; generic_desc_def saved root outer.desc; generic_desc_def saved link alias.desc;
    root_def ta; root_def tb; root_def ti; root_def to_; root_def tl;
    template_def saved ta; template_def saved tb; template_def saved ti; template_def saved to_; template_def saved tl;
    let t = if x === a then ta else if x === boundary then tb else if x === pair then ti
      else if x === root then to_ else if x === link then tl else Boundary x in
    root_def t; refine_ t) in
  let link : {p : node Pref.t | H.mem saved p} = refine_ link in
  let depth = 3 in
  let depth : {depth : int | depth >= 0} = refine_ depth in
  let state : {t : node Pref.token | Pref.own t === saved} = refine_ state in
  let refine_ first = Copy_algorithm.instantiate saved scope depth link state in
  let refine_ link = link in let refine_ depth = depth in
  let h1 = ghost_ (Pref.own (borrow_ first.#state)) in
  let scope1 : ((x : node Pref.t) @ immutable -> {u : unit |
      if H.mem h1 x then source_ok h1 x else H.at h1 x === None}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = history_scope saved scope first.#epoch depth first.#history x (refine_ u) in refine_ u) in
  ghost_ (let u = () in target_allocated saved first.#epoch depth first.#history link first.#value (refine_ u));
  let first_root = first.#value in
  let _proof = ghost_ (
    let[@def] rho : node Pref.t @ immutable total -> ty @ immutable total = fun x ->
      if x === a then Boolean else if x === pair then Function (Boolean, Boolean)
      else if x === root || x === link then Function (Function (Boolean, Boolean), Variable boundary)
      else Variable x in
    let model : (x : node Pref.t) @ immutable -> {u : unit | equation saved rho x}
        @ total = fun x -> rho_def x; rho_def a; rho_def pair; rho_def root; rho_def link; rho_def boundary;
      let desc = Var in cell_def desc 0; equation_def saved rho x; let u = () in refine_ u in
    let choices : node Pref.t @ immutable total -> ty @ immutable total = fun _ -> Function (Boolean, Boolean) in
    let refine_ actual = trees link in let u = () in
    let schema = actual in
    let claim = true in
    let use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved first.#epoch depth first.#history) tau x})) @ total ->
        (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || tau x === rho x})) @ total ->
        {u : unit | tau first_root === interpret rho choices schema} -> {u : unit | claim}) @ total =
      fun tau next equal fit -> let refine_ fit = fit in equal boundary; next first_root;
        let use_choices : ((eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
            {u : unit | tau first_root === interpret tau eta schema} -> {u : unit | claim}) @ total =
          fun _eta fit -> let refine_ fit = fit in let u = () in refine_ u in
        let u = () in let refine_ u = with_instance_choices saved first.#epoch depth first.#history tau next schema first_root
          (refine_ u) claim use_choices in refine_ u in
    let refine_ u = with_scheme_instance saved scope trees rho model choices first.#epoch depth first.#history schema first_root
      (refine_ u) claim use in ()) in
  let link : {p : node Pref.t | H.mem h1 p} =
    ghost_ (let u = () in history_grows saved first.#epoch depth first.#history link (refine_ u)); refine_ link in
  let depth2 = 5 in
  let depth2 : {depth : int | depth >= 0} = refine_ depth2 in
  let state = first.#state in let state : {t : node Pref.token | Pref.own t === h1} = refine_ state in
  let refine_ second = Copy_algorithm.instantiate h1 scope1 depth2 link state in
  let refine_ link = link in let refine_ depth2 = depth2 in
  let h2 = ghost_ (Pref.own (borrow_ second.#state)) in
  let scope2 : ((x : node Pref.t) @ immutable -> {u : unit |
      if H.mem h2 x then source_ok h2 x else H.at h2 x === None}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = history_scope h1 scope1 second.#epoch depth2 second.#history x (refine_ u) in refine_ u) in
  ghost_ (let u = () in history_grows h1 second.#epoch depth2 second.#history first_root (refine_ u);
    target_allocated h1 second.#epoch depth2 second.#history link second.#value (refine_ u));
  let first_root : {p : node Pref.t | H.mem h2 p} = refine_ first_root in
  let second_root = second.#value in
  let second_root : {p : node Pref.t | H.mem h2 p} = refine_ second_root in
  let state = second.#state in let state : {t : node Pref.token | Pref.own t === h2} = refine_ state in
  let refine_ left = read h2 scope2 first_root (borrow_ state) in
  let refine_ right = read h2 scope2 second_root (borrow_ state) in
  let refine_ first_root = first_root in let refine_ second_root = second_root in
  let refine_ same = Pref.equal first_root second_root in assert (not same);
  assert (left.level = Finite 3 && right.level = Finite 5);
  ghost_ (payload_scoped_def h2 left; payload_scoped_def h2 right);
  match left.desc, right.desc with
  | Arrow (p, b), Arrow (q, c) ->
    let refine_ same_b = Pref.equal b boundary in let refine_ same_c = Pref.equal c boundary in
    assert (same_b && same_c);
    let p : {p : node Pref.t | H.mem h2 p} = refine_ p in
    let q : {p : node Pref.t | H.mem h2 p} = refine_ q in
    let refine_ l = read h2 scope2 p (borrow_ state) in let refine_ r = read h2 scope2 q (borrow_ state) in
    (match l.desc, r.desc with
    | Arrow (a, b), Arrow (c, d) ->
      let refine_ ab = Pref.equal a b in let refine_ cd = Pref.equal c d in let refine_ ac = Pref.equal a c in
      assert (ab && cd && not ac)
    | _ -> assert false)
  | _ -> assert false

let constant level =
  let refine_ state = Pref.empty () in
  let original = {desc = Bool; level; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc original state in
  let p = step.value in let state = step.state in
  let saved = ghost_ (Pref.own (borrow_ state)) in
  let scope : ((x : node Pref.t) @ immutable -> {u : unit |
      if H.mem saved x then source_ok saved x else H.at saved x === None}) @ total ghost = ghost_ (fun x ->
    source_ok_def saved x; let u = () in refine_ u) in
  let p : {p : node Pref.t | H.mem saved p} = refine_ p in
  let depth = 7 in let depth : {depth : int | depth >= 0} = refine_ depth in
  let state : {t : node Pref.token | Pref.own t === saved} = refine_ state in
  let refine_ out = Copy_algorithm.instantiate saved scope depth p state in
  let refine_ p = p in let refine_ depth = depth in
  let h = ghost_ (Pref.own (borrow_ out.#state)) in
  let next_scope : ((x : node Pref.t) @ immutable -> {u : unit |
      if H.mem h x then source_ok h x else H.at h x === None}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = history_scope saved scope out.#epoch depth out.#history x (refine_ u) in refine_ u) in
  ghost_ (let u = () in target_allocated saved out.#epoch depth out.#history p out.#value (refine_ u);
    history_grows saved out.#epoch depth out.#history p (refine_ u));
  let q = out.#value in let q : {q : node Pref.t | H.mem h q} = refine_ q in
  let p : {p : node Pref.t | H.mem h p} = refine_ p in
  let state = out.#state in let state : {t : node Pref.token | Pref.own t === h} = refine_ state in
  let refine_ copied = read h next_scope q (borrow_ state) in
  let refine_ source = read h next_scope p (borrow_ state) in
  let refine_ p = p in let refine_ q = q in let refine_ same = Pref.equal p q in
  assert (copied.desc = Bool && source.desc = Bool && source.level = level);
  match level with
  | Finite _ -> assert (same && copied.level = level && source.memo = Empty_memo)
  | Generic -> assert (not same && copied.level = Finite 7 && copied.memo = Empty_memo);
    (match source.memo with Memo (_, target) ->
      let refine_ equal = Pref.equal target q in assert equal
    | Empty_memo | Forward _ -> assert false)

let () = constant Generic; constant (Finite 2)
