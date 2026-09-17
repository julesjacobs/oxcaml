open Copy_spec
open Copy_heap_proofs
open Effective_copy_spec
open Effective_copy_heap_proofs
open Generalize_spec
open Pooled_spec
module U = Level_unifier_spec
module E = Effective_level

type context = { saved : Pref.heap @@ ghost;
  epoch : node Pref.t @@ ghost; depth : int @@ ghost; base : pool @@ ghost }

let[@def] (result @ total) (c : context @ immutable) (heads : E.heads @ total) (before : history @ immutable)
    (p : node Pref.t @ immutable) (value : node Pref.t @ immutable)
    (after : Pref.heap @ immutable) (d : history @ immutable)
    (pool : pool @ immutable) (trail : pool @ immutable) = ghost_ (
  effective_valid c.saved heads c.epoch c.depth d && clean_session d
  && after === heap c.saved c.epoch c.depth d
  && pool === registered c.base c.epoch d && trail === touched d
  && extends before d && effective_target_for c.saved heads d p value)

let finish : (c : context) @ immutable -> (heads : E.heads Ghost.t) @ total -> (depth : {n : int | n === c.depth}) ->
    (d : history Ghost.t) @ immutable -> (pool : pool) @ immutable -> (trail : pool) @ immutable -> (p : node Pref.t) @ immutable ->
    (dest : destination) @ immutable ->
    (clean : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at c.saved x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t) @ total ->
    (state : {t : Pref.token | effective_valid c.saved heads.Ghost.ghost c.epoch c.depth d.Ghost.ghost
      && pool === registered c.base c.epoch d.Ghost.ghost && trail === touched d.Ghost.ghost
      && clean_session d.Ghost.ghost && Pref.own t === heap c.saved c.epoch c.depth d.Ghost.ghost
      && H.mem c.saved p && E.level c.saved heads.Ghost.ghost p === Generic
      && match H.at c.saved p with None -> false | Some v ->
        match dest with Allocate desc -> effective_ready c.saved heads.Ghost.ghost d.Ghost.ghost v.desc desc
        | Share value -> match v.desc with Link child -> effective_target_for c.saved heads.Ghost.ghost d.Ghost.ghost child value
          | _ -> false}) @ unique ->
    {r : copied | result c heads.Ghost.ghost d.Ghost.ghost p r.#value (Pref.own r.#state) r.#history r.#pool r.#trail} @ unique =
  fun c heads depth d pool trail p dest clean state ->
    let refine_ depth = depth in let refine_ state = state in
    ghost_ (let u = () in history_at c.saved heads.Ghost.ghost c.epoch c.depth d.Ghost.ghost p (refine_ u));
    let state : {t : Pref.token | H.mem (Pref.own t) p} = refine_ state in
    let refine_ old = Pref.read p (borrow_ state) in let refine_ state = state in
    ghost_ (clean.Ghost.ghost p; let u = () in
      clean_memo_lookup c.saved heads.Ghost.ghost c.epoch c.depth d.Ghost.ghost p old (refine_ u));
    match old.memo with
    | Memo _ -> assert false
    | Forward value ->
      ghost_ (let h = Pref.own (borrow_ state) in result_def c heads.Ghost.ghost d.Ghost.ghost p value h d.Ghost.ghost pool trail;
        effective_target_for_def c.saved heads.Ghost.ghost d.Ghost.ghost p value;
        extends_def d.Ghost.ghost d.Ghost.ghost);
      let r = #{value; state; pool; trail; history = d.Ghost.ghost} in refine_ r
    | Empty_memo ->
      match dest with
      | Share value ->
        let state : {t : Pref.token | H.mem (Pref.own t) p} = refine_ state in
        let marked = {old with memo = Forward value} in
        let refine_ state = Pref.write p marked state in
        let next = ghost_ (Alias (d.Ghost.ghost, p, value, old)) in
        let trail = Entry (p, trail) in
        ghost_ (registered_def c.base c.epoch next; touched_def next);
        ghost_ (effective_valid_def c.saved heads.Ghost.ghost c.epoch c.depth next;
          heap_def c.saved c.epoch c.depth next;
          session_mark_def d.Ghost.ghost old c.epoch value;
          clean_session_def next; mapping_def next p;
          effective_target_for_def c.saved heads.Ghost.ghost next p value;
          extends_def d.Ghost.ghost next; extends_def d.Ghost.ghost d.Ghost.ghost;
          let h = Pref.own (borrow_ state) in result_def c heads.Ghost.ghost d.Ghost.ghost p value h next pool trail; ());
        let r = #{value; state; pool; trail; history = next} in refine_ r
      | Allocate desc ->
        let node = cell desc depth in let refine_ step = Pref.alloc node state in
        let value = step.value in let state = step.state in
        ghost_ (let h = heap c.saved c.epoch c.depth d.Ghost.ghost in put_frame h value node p; ());
        let state : {t : Pref.token | H.mem (Pref.own t) p} = refine_ state in
        let marked = {old with memo = Forward value} in
        let refine_ state = Pref.write p marked state in
        let next = ghost_ (Fresh (d.Ghost.ghost, p, value, old, desc)) in
        let pool = Entry (value, pool) in let trail = Entry (p, trail) in
        ghost_ (registered_def c.base c.epoch next; touched_def next);
        ghost_ (effective_valid_def c.saved heads.Ghost.ghost c.epoch c.depth next;
          heap_def c.saved c.epoch c.depth next;
          session_mark_def d.Ghost.ghost old c.epoch value;
          clean_session_def next; mapping_def next p;
          effective_target_for_def c.saved heads.Ghost.ghost next p value;
          extends_def d.Ghost.ghost next; extends_def d.Ghost.ghost d.Ghost.ghost;
          let h = Pref.own (borrow_ state) in result_def c heads.Ghost.ghost d.Ghost.ghost p value h next pool trail; ());
        let r = #{value; state; pool; trail; history = next} in refine_ r

type goal = { initial : history @@ ghost; root : node Pref.t @@ ghost }

let rec copy_work : (c : context) @ immutable -> (heads : E.heads Ghost.t) @ total ->
    (scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem c.saved x) || source_ok c.saved x})) Ghost.t) @ total ->
    (clean : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at c.saved x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t) @ total ->
    (witness : (((x : node Pref.t) @ immutable ->
      {u : unit | E.valid_head c.saved heads.Ghost.ghost x})) Ghost.t) @ total ->
    (goal : goal) @ immutable -> (depth : {n : int | n === c.depth}) ->
    (d : history Ghost.t) @ immutable -> (pool : pool) @ immutable -> (trail : pool) @ immutable ->
    (p : node Pref.t) @ immutable ->
    (state : {t : Pref.token | H.mem c.saved p && effective_valid c.saved heads.Ghost.ghost c.epoch c.depth d.Ghost.ghost
      && clean_session d.Ghost.ghost && Pref.own t === heap c.saved c.epoch c.depth d.Ghost.ghost
      && pool === registered c.base c.epoch d.Ghost.ghost && trail === touched d.Ghost.ghost}) @ unique ->
    (use : ((r : {r : copied | result c heads.Ghost.ghost d.Ghost.ghost p r.#value (Pref.own r.#state) r.#history r.#pool r.#trail}) @ unique ->
      {r : copied | result c heads.Ghost.ghost goal.initial goal.root r.#value
        (Pref.own r.#state) r.#history r.#pool r.#trail} @ unique)) ->
    {r : copied | result c heads.Ghost.ghost goal.initial goal.root r.#value
      (Pref.own r.#state) r.#history r.#pool r.#trail} @ unique =
  fun c heads scope clean witness goal depth d pool trail p state use ->
    let refine_ state = state in
    ghost_ (scope.Ghost.ghost p; witness.Ghost.ghost p; clean.Ghost.ghost p;
      let u = () in history_at c.saved heads.Ghost.ghost c.epoch c.depth d.Ghost.ghost p (refine_ u));
    let state : {t : Pref.token | H.mem (Pref.own t) p} = refine_ state in
    let refine_ old = Pref.read p (borrow_ state) in let refine_ state = state in
    ghost_ (let u = () in clean_memo_lookup c.saved heads.Ghost.ghost c.epoch c.depth d.Ghost.ghost p old (refine_ u);
      source_ok_def c.saved p; U.observe_def c.saved p);
    match old.memo with
    | Memo _ -> assert false
    | Forward value ->
      ghost_ (let u = () in mapped_generic c.saved heads.Ghost.ghost c.epoch c.depth d.Ghost.ghost p value (refine_ u);
        effective_target_for_def c.saved heads.Ghost.ghost d.Ghost.ghost p value;
        extends_def d.Ghost.ghost d.Ghost.ghost;
        let h = Pref.own (borrow_ state) in result_def c heads.Ghost.ghost d.Ghost.ghost p value h d.Ghost.ghost pool trail; ());
      let r = #{value; state; pool; trail; history = d.Ghost.ghost} in use (refine_ r)
    | Empty_memo ->
      match old.desc with
      | Link child ->
        let resume : (r : {r : copied |
            result c heads.Ghost.ghost d.Ghost.ghost child r.#value (Pref.own r.#state) r.#history r.#pool r.#trail}) @ unique ->
            {r : copied | result c heads.Ghost.ghost goal.initial goal.root r.#value
              (Pref.own r.#state) r.#history r.#pool r.#trail} @ unique = fun r ->
          let refine_ r = r in let next = ghost_ r.#history in
          ghost_ (let h = Pref.own (borrow_ r.#state) in
            result_def c heads.Ghost.ghost d.Ghost.ghost child r.#value h next r.#pool r.#trail;
            witness.Ghost.ghost child; let u = () in
            E.link_level c.saved heads.Ghost.ghost p child (refine_ u);
            copy_link_decision c.saved heads.Ghost.ghost witness.Ghost.ghost c.epoch c.depth next child r.#value (refine_ u));
          let same = Pref.equal child r.#value in let refine_ same = same in
          if same then (
            ghost_ (effective_target_for_def c.saved heads.Ghost.ghost next p p;
              let h = Pref.own (borrow_ r.#state) in result_def c heads.Ghost.ghost d.Ghost.ghost p p h next r.#pool r.#trail; ());
            let out = #{value = p; state = r.#state; pool = r.#pool; trail = r.#trail; history = next} in use (refine_ out))
          else (
            let dest = Share r.#value in let current = {Ghost.ghost = next} in
            let state = r.#state in
            let refine_ out = finish c heads depth current r.#pool r.#trail p dest clean (refine_ state) in
            ghost_ (let h = Pref.own (borrow_ out.#state) in
              result_def c heads.Ghost.ghost next p out.#value h out.#history out.#pool out.#trail;
              let u = () in extension_trans d.Ghost.ghost next out.#history (refine_ u);
              result_def c heads.Ghost.ghost d.Ghost.ghost p out.#value h out.#history out.#pool out.#trail; ());
            let out = #{value = out.#value; state = out.#state; pool = out.#pool; trail = out.#trail; history = out.#history} in use (refine_ out)) in
        copy_work c heads scope clean witness goal depth d pool trail child (refine_ state) resume
      | Var | Bool | Arrow _ ->
        ghost_ (U.terminal_def c.saved p; let u = () in
          E.terminal_level c.saved heads.Ghost.ghost p (refine_ u); Level_spec.at_level_def c.saved p);
        match old.level with
        | Finite _ ->
          ghost_ (effective_target_for_def c.saved heads.Ghost.ghost d.Ghost.ghost p p;
            extends_def d.Ghost.ghost d.Ghost.ghost;
            let h = Pref.own (borrow_ state) in result_def c heads.Ghost.ghost d.Ghost.ghost p p h d.Ghost.ghost pool trail; ());
          let r = #{value = p; state; pool; trail; history = d.Ghost.ghost} in use (refine_ r)
        | Generic ->
          match old.desc with
          | Var | Bool ->
            ghost_ (effective_ready_def c.saved heads.Ghost.ghost d.Ghost.ghost old.desc old.desc);
            let dest = Allocate old.desc in
            let refine_ out = finish c heads depth d pool trail p dest clean (refine_ state) in let out = #{value = out.#value; state = out.#state; pool = out.#pool; trail = out.#trail; history = out.#history} in use (refine_ out)
          | Link _ -> assert false
          | Arrow (a, b) ->
            let left : (left : {r : copied | result c heads.Ghost.ghost d.Ghost.ghost a r.#value
                (Pref.own r.#state) r.#history r.#pool r.#trail}) @ unique ->
                {r : copied | result c heads.Ghost.ghost goal.initial goal.root r.#value
                  (Pref.own r.#state) r.#history r.#pool r.#trail} @ unique = fun left ->
              let refine_ left = left in let first = ghost_ left.#history in
              ghost_ (let h = Pref.own (borrow_ left.#state) in
                result_def c heads.Ghost.ghost d.Ghost.ghost a left.#value h first left.#pool left.#trail; ());
              let current = {Ghost.ghost = first} in
              let right : (right : {r : copied | result c heads.Ghost.ghost current.Ghost.ghost b r.#value
                  (Pref.own r.#state) r.#history r.#pool r.#trail}) @ unique ->
                  {r : copied | result c heads.Ghost.ghost goal.initial goal.root r.#value
                    (Pref.own r.#state) r.#history r.#pool r.#trail} @ unique = fun right ->
                let refine_ right = right in let second = ghost_ right.#history in
                let desc = Arrow (left.#value, right.#value) in let dest = Allocate desc in
                ghost_ (let h = Pref.own (borrow_ right.#state) in
                  result_def c heads.Ghost.ghost first b right.#value h second right.#pool right.#trail;
                  let u = () in target_preserved c.saved heads.Ghost.ghost c.epoch c.depth first second a left.#value (refine_ u);
                  effective_ready_def c.saved heads.Ghost.ghost second old.desc desc);
                let current = {Ghost.ghost = second} in let state = right.#state in
                let refine_ out = finish c heads depth current right.#pool right.#trail p dest clean (refine_ state) in
                ghost_ (let h = Pref.own (borrow_ out.#state) in
                  result_def c heads.Ghost.ghost second p out.#value h out.#history out.#pool out.#trail;
                  let u = () in extension_trans d.Ghost.ghost first second (refine_ u);
                  extension_trans d.Ghost.ghost second out.#history (refine_ u);
                  result_def c heads.Ghost.ghost d.Ghost.ghost p out.#value h out.#history out.#pool out.#trail; ());
                let out = #{value = out.#value; state = out.#state; pool = out.#pool; trail = out.#trail; history = out.#history} in use (refine_ out) in
              let state = left.#state in
              copy_work c heads scope clean witness goal depth current left.#pool left.#trail b (refine_ state) right in
            copy_work c heads scope clean witness goal depth d pool trail a (refine_ state) left

let instantiate : (c : context) @ immutable -> (heads : E.heads Ghost.t) @ total ->
    (scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem c.saved x) || source_ok c.saved x})) Ghost.t) @ total ->
    (clean : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at c.saved x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t) @ total ->
    (witness : (((x : node Pref.t) @ immutable ->
      {u : unit | E.valid_head c.saved heads.Ghost.ghost x})) Ghost.t) @ total ->
    (depth : {n : int | n === c.depth && n >= 0}) ->
    (pool : pool) @ immutable -> (p : node Pref.t) @ immutable ->
    (state : {t : Pref.token | Pref.own t === c.saved && pool === c.base
      && H.mem c.saved p && H.mem c.saved c.epoch}) @ unique ->
    {r : Clean_copy.instance | effective_valid c.saved heads.Ghost.ghost c.epoch c.depth r.#history
      && clean_session r.#history && r.#epoch === c.epoch
      && Pref.own r.#state === Copy_cleanup_spec.swept
        (heap c.saved c.epoch c.depth r.#history) (touched r.#history)
      && r.#pool === registered c.base c.epoch r.#history
      && effective_target_for c.saved heads.Ghost.ghost r.#history p r.#value} @ unique =
  fun c heads scope clean witness depth pool p state ->
    let refine_ depth = depth in let refine_ state = state in
    let d = {Ghost.ghost = ghost_ Clean} in let trail = Empty in
    let goal = {initial = d.Ghost.ghost; root = ghost_ p} in
    ghost_ (effective_valid_def c.saved heads.Ghost.ghost c.epoch c.depth d.Ghost.ghost;
      clean_session_def d.Ghost.ghost; heap_def c.saved c.epoch c.depth d.Ghost.ghost;
      registered_def c.base c.epoch d.Ghost.ghost; touched_def d.Ghost.ghost);
    let use : (r : {r : copied | result c heads.Ghost.ghost d.Ghost.ghost p r.#value
        (Pref.own r.#state) r.#history r.#pool r.#trail}) @ unique ->
        {r : copied | result c heads.Ghost.ghost goal.initial goal.root r.#value
          (Pref.own r.#state) r.#history r.#pool r.#trail} @ unique = fun r ->
      let refine_ r = r in refine_ r in
    let depth : {n : int | n === c.depth} = refine_ depth in
    let refine_ depth = depth in
    let refine_ out = copy_work c heads scope clean witness goal (refine_ depth) d pool trail p (refine_ state) use in
    let raw = ghost_ (Pref.own (borrow_ out.#state)) in
    let history = ghost_ out.#history in let trail = out.#trail in
    ghost_ (result_def c heads.Ghost.ghost goal.initial goal.root out.#value raw history out.#pool trail);
    let heap_witness = {Ghost.ghost = ghost_ raw} in
    let members : (((x : node Pref.t) @ immutable ->
        {u : unit | not (listed trail x) || H.mem heap_witness.Ghost.ghost x})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x ->
        let u = () in touched_saved c.saved heads.Ghost.ghost c.epoch c.depth history x (refine_ u);
        history_grows c.saved heads.Ghost.ghost c.epoch c.depth history x (refine_ u); refine_ u)} in
    let state = out.#state in
    let refine_ state = Copy_cleanup.clear heap_witness trail members (refine_ state) in
    let r = #{Clean_copy.value = out.#value; state; pool = out.#pool; epoch = c.epoch; history} in refine_ r
