(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml copy_datatype_demo.ml";
 { bytecode; }
 { native; }
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
  let state = Pref.empty () in
  let variable = {desc = Var; level = Generic; memo = Empty_memo; visited = false} in
  let step = Pref.alloc variable state in
  let a = step.value in let state = step.state in
  let word = {desc = Word; level = Generic; memo = Empty_memo; visited = false} in
  let step = Pref.alloc word state in
  let w = step.value in let state = step.state in
  let list = {desc = List a; level = Generic; memo = Empty_memo; visited = false} in
  let step = Pref.alloc list state in
  let l = step.value in let state = step.state in
  let nested = {desc = List l; level = Generic; memo = Empty_memo; visited = false} in
  let step = Pref.alloc nested state in
  let ll = step.value in let state = step.state in
  let shared = {desc = Arrow (ll, ll); level = Generic; memo = Empty_memo; visited = false} in
  let step = Pref.alloc shared state in
  let pair = step.value in let state = step.state in
  let outer = {desc = Arrow (pair, w); level = Generic; memo = Empty_memo; visited = false} in
  let step = Pref.alloc outer state in
  let root = step.value in let state = step.state in
  let saved = ghost_ (Pref.own (borrow_ state)) in
  let scope : ((x : node Pref.t) @ immutable -> {u : unit |
      if H.mem saved x then source_ok saved x else H.at saved x === None})
      @ total ghost = ghost_ (fun x -> source_ok_def saved x; ()) in
  let out = Copy_algorithm.instantiate saved scope 2 root state in
  let after = ghost_ (Pref.own (borrow_ out.#state)) in
  let final_scope : ((x : node Pref.t) @ immutable -> {u : unit |
      if H.mem after x then source_ok after x else H.at after x === None})
      @ total ghost = ghost_ (fun x ->
        history_scope saved scope out.#epoch 2 out.#history x (); ()) in
  ghost_ (target_allocated saved out.#epoch 2 out.#history root out.#value ());
  let refine_ copied = read after final_scope out.#value (borrow_ out.#state) in
  ghost_ (payload_scoped_def after copied);
  match copied.desc with
  | Arrow (pair, word) ->
    let refine_ word = read after final_scope word (borrow_ out.#state) in
    (match word.desc with Word -> () | _ -> failwith "word type changed");
    let refine_ pair = read after final_scope pair (borrow_ out.#state) in
    ghost_ (payload_scoped_def after pair);
    (match pair.desc with
     | Arrow (left, right) ->
       if not (Pref.equal left right) then failwith "lost list sharing";
       let refine_ nested = read after final_scope left (borrow_ out.#state) in
       ghost_ (payload_scoped_def after nested);
       (match nested.desc with
        | List inner ->
          let refine_ inner = read after final_scope inner (borrow_ out.#state) in
          ghost_ (payload_scoped_def after inner);
          (match inner.desc with
           | List element ->
             if Pref.equal element a then failwith "generic variable was shared";
             let refine_ element = read after final_scope element (borrow_ out.#state) in
             (match element.desc, element.level with
              | Var, Finite 2 -> () | _ -> failwith "wrong list element instance")
           | _ -> failwith "missing inner list")
        | _ -> failwith "missing outer list")
     | _ -> failwith "missing shared arrows")
  | _ -> failwith "missing result arrow"
