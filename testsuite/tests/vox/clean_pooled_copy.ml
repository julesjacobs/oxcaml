open Copy_spec
open Copy_heap_proofs
open Generalize_spec
open Pooled_spec
open Pooled_proofs

let finish : (saved : (Pref.heap) Ghost.t) @ immutable  ->(clean : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at saved.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t) @ total  ->(epoch : (node Pref.t) Ghost.t) @ immutable  ->
    (depth : int)  ->(d : (history) Ghost.t) @ immutable  ->(base : (pool) Ghost.t) @ immutable  -> (pool : pool) @ immutable  ->
    (trail : pool) @ immutable  ->
    (p : {p : node Pref.t | H.mem saved.Ghost.ghost p && source_ok saved.Ghost.ghost p}) @ immutable  ->
    (dest : destination) @ immutable  ->
    (t : {t : Pref.token | let refine_ p = p in valid saved.Ghost.ghost epoch.Ghost.ghost depth d.Ghost.ghost && (clean_session d.Ghost.ghost) && Pref.own t === heap saved.Ghost.ghost epoch.Ghost.ghost depth d.Ghost.ghost && pool === registered base.Ghost.ghost epoch.Ghost.ghost d.Ghost.ghost && trail === touched d.Ghost.ghost
      && match H.at saved.Ghost.ghost p with None -> false | Some v ->
        v.level === Generic && prepared saved.Ghost.ghost d.Ghost.ghost v.desc dest}) @ unique  ->
    {r : copied | let refine_ p = p in valid saved.Ghost.ghost epoch.Ghost.ghost depth r.#history && (clean_session r.#history)
      && Pref.own r.#state === heap saved.Ghost.ghost epoch.Ghost.ghost depth r.#history && r.#pool === registered base.Ghost.ghost epoch.Ghost.ghost r.#history && r.#trail === touched r.#history
      && extends d.Ghost.ghost r.#history && target_for saved.Ghost.ghost r.#history p r.#value} @ unique = fun saved clean epoch depth d base pool trail p dest t  ->
    let refine_ p = p in let refine_ t = t in
    let h = ghost_ (heap saved.Ghost.ghost epoch.Ghost.ghost depth d.Ghost.ghost) in
    ghost_ (let u = () in history_at saved.Ghost.ghost epoch.Ghost.ghost depth d.Ghost.ghost p (refine_ u));
    let t : {t : Pref.token | H.mem (Pref.own t) p} = refine_ t in
    let refine_ old = Pref.read p (borrow_ t) in let refine_ t = t in
    ghost_ (let u = () in clean.Ghost.ghost p; clean_memo_lookup saved.Ghost.ghost epoch.Ghost.ghost depth d.Ghost.ghost p old (refine_ u));
    let hit = match old.memo with Empty_memo -> None | Forward q -> Some q | Memo _ -> assert false in
    match hit with
    | Some value ->
      ghost_ (target_for_def saved.Ghost.ghost d.Ghost.ghost p value; extends_def d.Ghost.ghost d.Ghost.ghost);
      let r = #{value; state = t; pool; trail; history = d.Ghost.ghost} in refine_ r
    | None ->
      ghost_ (prepared_def saved.Ghost.ghost d.Ghost.ghost old.desc dest);
      (match dest with
      | Allocate desc ->
        let v = cell desc depth in let refine_ step = Pref.alloc v t in
        let value = step.value in let t = step.state in
        ghost_ (put_frame h value v p);
        let t : {t : Pref.token | H.mem (Pref.own t) p} = refine_ t in
        ghost_ (session_mark_def d.Ghost.ghost old epoch.Ghost.ghost value);
        let w = {old with memo = Forward value} in let refine_ t = Pref.write p w t in
        let history = ghost_ (Fresh (d.Ghost.ghost, p, value, old, desc)) in
        ghost_ (valid_def saved.Ghost.ghost epoch.Ghost.ghost depth history; clean_session_def history; heap_def saved.Ghost.ghost epoch.Ghost.ghost depth history;
          mapping_def history p; target_for_def saved.Ghost.ghost history p value; extends_def d.Ghost.ghost history; extends_def d.Ghost.ghost d.Ghost.ghost);
        ghost_ (registered_def base.Ghost.ghost epoch.Ghost.ghost history; touched_def history);
        let trail = Entry (p, trail) in
        let pool = Entry (value, pool) in
        let r = #{value; state = t; pool; trail; history} in refine_ r
      | Share value ->
        let t : {t : Pref.token | H.mem (Pref.own t) p} = refine_ t in
        ghost_ (session_mark_def d.Ghost.ghost old epoch.Ghost.ghost value);
        let w = {old with memo = Forward value} in let refine_ t = Pref.write p w t in
        let history = ghost_ (Alias (d.Ghost.ghost, p, value, old)) in
        ghost_ (valid_def saved.Ghost.ghost epoch.Ghost.ghost depth history; clean_session_def history; heap_def saved.Ghost.ghost epoch.Ghost.ghost depth history;
          mapping_def history p; target_for_def saved.Ghost.ghost history p value; extends_def d.Ghost.ghost history; extends_def d.Ghost.ghost d.Ghost.ghost);
        ghost_ (registered_def base.Ghost.ghost epoch.Ghost.ghost history; touched_def history);
        let trail = Entry (p, trail) in
        let r = #{value; state = t; pool; trail; history} in refine_ r)

type copy_goal = { heap : Pref.heap @@ ghost; identity : node Pref.t @@ ghost;
  depth : int @@ ghost; initial : history @@ ghost; base : pool @@ ghost;
  root : node Pref.t @@ ghost }

let rec copy_work : (goal : copy_goal) @ immutable -> (saved : (Pref.heap) Ghost.t) @ immutable  ->(scope : (((p : node Pref.t) @ immutable -> {u : unit | if H.mem saved.Ghost.ghost p then source_ok saved.Ghost.ghost p else H.at saved.Ghost.ghost p === None})) Ghost.t) @ total  ->(clean : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at saved.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t) @ total  ->(epoch : (node Pref.t) Ghost.t) @ immutable  -> (depth : int)  ->(d : (history) Ghost.t) @ immutable  ->(base : (pool) Ghost.t) @ immutable  -> (pool : pool) @ immutable  ->
    (trail : pool) @ immutable  ->
    (p : {p : node Pref.t | H.mem saved.Ghost.ghost p}) @ immutable  ->
    (t : {t : Pref.token | let refine_ p = p in valid saved.Ghost.ghost epoch.Ghost.ghost depth d.Ghost.ghost && (clean_session d.Ghost.ghost) && Pref.own t === heap saved.Ghost.ghost epoch.Ghost.ghost depth d.Ghost.ghost && pool === registered base.Ghost.ghost epoch.Ghost.ghost d.Ghost.ghost && trail === touched d.Ghost.ghost}) @ unique  ->
    (use : ((r : {r : copied | let refine_ p = p in valid saved.Ghost.ghost epoch.Ghost.ghost depth r.#history && (clean_session r.#history)
      && Pref.own r.#state === heap saved.Ghost.ghost epoch.Ghost.ghost depth r.#history && r.#pool === registered base.Ghost.ghost epoch.Ghost.ghost r.#history && r.#trail === touched r.#history
      && extends d.Ghost.ghost r.#history && target_for saved.Ghost.ghost r.#history p r.#value}) @ unique -> {r : copied | valid goal.heap goal.identity goal.depth r.#history && (clean_session r.#history)
      && Pref.own r.#state === heap goal.heap goal.identity goal.depth r.#history && r.#pool === registered goal.base goal.identity r.#history && r.#trail === touched r.#history
      && extends goal.initial r.#history && target_for goal.heap r.#history goal.root r.#value} @ unique)) ->
    {r : copied | valid goal.heap goal.identity goal.depth r.#history && (clean_session r.#history)
      && Pref.own r.#state === heap goal.heap goal.identity goal.depth r.#history && r.#pool === registered goal.base goal.identity r.#history && r.#trail === touched r.#history
      && extends goal.initial r.#history && target_for goal.heap r.#history goal.root r.#value} @ unique = fun goal saved scope clean epoch depth d base pool trail p t use ->
    let refine_ p = p in let refine_ t = t in
    ghost_ (scope.Ghost.ghost p; let u = () in history_at saved.Ghost.ghost epoch.Ghost.ghost depth d.Ghost.ghost p (refine_ u));
    let t : {t : Pref.token | H.mem (Pref.own t) p} = refine_ t in
    let refine_ old = Pref.read p (borrow_ t) in let refine_ t = t in
    match old.level with
    | Finite _ ->
      ghost_ (target_for_def saved.Ghost.ghost d.Ghost.ghost p p; extends_def d.Ghost.ghost d.Ghost.ghost);
      let r = #{value = p; state = t; pool; trail; history = d.Ghost.ghost} in use (refine_ r)
    | Generic ->
      ghost_ (let u = () in clean.Ghost.ghost p; clean_memo_lookup saved.Ghost.ghost epoch.Ghost.ghost depth d.Ghost.ghost p old (refine_ u));
      let hit = match old.memo with Empty_memo -> None | Forward q -> Some q | Memo _ -> assert false in
      match hit with
      | Some value ->
        ghost_ (target_for_def saved.Ghost.ghost d.Ghost.ghost p value; extends_def d.Ghost.ghost d.Ghost.ghost);
        let r = #{value; state = t; pool; trail; history = d.Ghost.ghost} in use (refine_ r)
      | None ->
        ghost_ (source_ok_def saved.Ghost.ghost p; payload_scoped_def saved.Ghost.ghost old);
        let p : {p : node Pref.t | H.mem saved.Ghost.ghost p && source_ok saved.Ghost.ghost p} = refine_ p in
        match old.desc with
        | Var | Bool ->
          let dest = Allocate old.desc in
          ghost_ (prepared_def saved.Ghost.ghost d.Ghost.ghost old.desc dest; ready_def saved.Ghost.ghost d.Ghost.ghost old.desc old.desc);
          let t : {t : Pref.token | let refine_ p = p in valid saved.Ghost.ghost epoch.Ghost.ghost depth d.Ghost.ghost && (clean_session d.Ghost.ghost) && Pref.own t === heap saved.Ghost.ghost epoch.Ghost.ghost depth d.Ghost.ghost && pool === registered base.Ghost.ghost epoch.Ghost.ghost d.Ghost.ghost && trail === touched d.Ghost.ghost
            && match H.at saved.Ghost.ghost p with None -> false | Some v ->
              v.level === Generic && prepared saved.Ghost.ghost d.Ghost.ghost v.desc dest} = refine_ t in
          let saved_witness25 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (saved.Ghost.ghost)} in
          let clean_witness26 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at saved_witness25.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t = {Ghost.ghost = ghost_ (refine_ clean.Ghost.ghost)} in
          let epoch_witness27 : (node Pref.t) Ghost.t = {Ghost.ghost = ghost_ (epoch.Ghost.ghost)} in
          let d_witness28 : (history) Ghost.t = {Ghost.ghost = ghost_ (d.Ghost.ghost)} in
          let base_witness29 : (pool) Ghost.t = {Ghost.ghost = ghost_ (base.Ghost.ghost)} in
          let refine_ t = t in
          let copy_source1 : {p : node Pref.t | H.mem saved_witness25.Ghost.ghost p && source_ok saved_witness25.Ghost.ghost p} =
            let refine_ p = p in refine_ p in
          let refine_ r = finish saved_witness25 clean_witness26 epoch_witness27 depth d_witness28 base_witness29 pool trail copy_source1 dest (refine_ t) in use (refine_ r)
        | Link child ->
          let child : {p : node Pref.t | H.mem saved.Ghost.ghost p} = refine_ child in
          let t : {t : Pref.token | let refine_ p = child in valid saved.Ghost.ghost epoch.Ghost.ghost depth d.Ghost.ghost && (clean_session d.Ghost.ghost) && Pref.own t === heap saved.Ghost.ghost epoch.Ghost.ghost depth d.Ghost.ghost && pool === registered base.Ghost.ghost epoch.Ghost.ghost d.Ghost.ghost && trail === touched d.Ghost.ghost} = refine_ t in
          let saved_witness1 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (saved.Ghost.ghost)} in
          let scope_witness2 : (((p : node Pref.t) @ immutable -> {u : unit | if H.mem saved_witness1.Ghost.ghost p then source_ok saved_witness1.Ghost.ghost p else H.at saved_witness1.Ghost.ghost p === None})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
          let clean_witness3 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at saved_witness1.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t = {Ghost.ghost = ghost_ (refine_ clean.Ghost.ghost)} in
          let epoch_witness4 : (node Pref.t) Ghost.t = {Ghost.ghost = ghost_ (epoch.Ghost.ghost)} in
          let d_witness5 : (history) Ghost.t = {Ghost.ghost = ghost_ (d.Ghost.ghost)} in
          let base_witness6 : (pool) Ghost.t = {Ghost.ghost = ghost_ (base.Ghost.ghost)} in
          let refine_ t = t in
          let copy_source2 : {p : node Pref.t | H.mem saved_witness1.Ghost.ghost p} =
            let refine_ p = child in refine_ p in
          let resume_r : (r : {r : copied | let refine_ copy_source2 = copy_source2 in valid saved_witness1.Ghost.ghost epoch_witness4.Ghost.ghost depth r.#history && (clean_session r.#history)
      && Pref.own r.#state === heap saved_witness1.Ghost.ghost epoch_witness4.Ghost.ghost depth r.#history && r.#pool === registered base_witness6.Ghost.ghost epoch_witness4.Ghost.ghost r.#history && r.#trail === touched r.#history
      && extends d_witness5.Ghost.ghost r.#history && target_for saved_witness1.Ghost.ghost r.#history copy_source2 r.#value}) @ unique ->
            {r : copied | valid goal.heap goal.identity goal.depth r.#history && (clean_session r.#history)
      && Pref.own r.#state === heap goal.heap goal.identity goal.depth r.#history && r.#pool === registered goal.base goal.identity r.#history && r.#trail === touched r.#history
      && extends goal.initial r.#history && target_for goal.heap r.#history goal.root r.#value} @ unique = fun r ->
            let refine_ r = r in
          let pool = r.#pool in let trail = r.#trail in let history = ghost_ r.#history in let dest = Share r.#value in
          ghost_ (prepared_def saved.Ghost.ghost history old.desc dest);
          let t = r.#state in
          let t : {t : Pref.token | let refine_ p = p in valid saved.Ghost.ghost epoch.Ghost.ghost depth history && (clean_session history) && Pref.own t === heap saved.Ghost.ghost epoch.Ghost.ghost depth history && pool === registered base.Ghost.ghost epoch.Ghost.ghost history && trail === touched history
            && match H.at saved.Ghost.ghost p with None -> false | Some v ->
              v.level === Generic && prepared saved.Ghost.ghost history v.desc dest} = refine_ t in
          let saved_witness30 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (saved.Ghost.ghost)} in
          let clean_witness31 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at saved_witness30.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t = {Ghost.ghost = ghost_ (refine_ clean.Ghost.ghost)} in
          let epoch_witness32 : (node Pref.t) Ghost.t = {Ghost.ghost = ghost_ (epoch.Ghost.ghost)} in
          let d_witness33 : (history) Ghost.t = {Ghost.ghost = ghost_ (history)} in
          let base_witness34 : (pool) Ghost.t = {Ghost.ghost = ghost_ (base.Ghost.ghost)} in
          let refine_ t = t in
          let copy_source3 : {p : node Pref.t | H.mem saved_witness30.Ghost.ghost p && source_ok saved_witness30.Ghost.ghost p} =
            let refine_ p = p in refine_ p in
          let refine_ out = finish saved_witness30 clean_witness31 epoch_witness32 depth d_witness33 base_witness34 pool trail copy_source3 dest (refine_ t) in
          ghost_ (let u = () in extension_trans d.Ghost.ghost history out.#history (refine_ u));
          let r = #{value = out.#value; state = out.#state; pool = out.#pool; trail = out.#trail; history = out.#history} in use (refine_ r) in
          copy_work goal saved_witness1 scope_witness2 clean_witness3 epoch_witness4 depth d_witness5 base_witness6 pool trail copy_source2 (refine_ t) resume_r
        | Arrow (a, b) ->
          let a : {p : node Pref.t | H.mem saved.Ghost.ghost p} = refine_ a in
          let t : {t : Pref.token | let refine_ p = a in valid saved.Ghost.ghost epoch.Ghost.ghost depth d.Ghost.ghost && (clean_session d.Ghost.ghost) && Pref.own t === heap saved.Ghost.ghost epoch.Ghost.ghost depth d.Ghost.ghost && pool === registered base.Ghost.ghost epoch.Ghost.ghost d.Ghost.ghost && trail === touched d.Ghost.ghost} = refine_ t in
          let saved_witness7 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (saved.Ghost.ghost)} in
          let scope_witness8 : (((p : node Pref.t) @ immutable -> {u : unit | if H.mem saved_witness7.Ghost.ghost p then source_ok saved_witness7.Ghost.ghost p else H.at saved_witness7.Ghost.ghost p === None})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
          let clean_witness9 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at saved_witness7.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t = {Ghost.ghost = ghost_ (refine_ clean.Ghost.ghost)} in
          let epoch_witness10 : (node Pref.t) Ghost.t = {Ghost.ghost = ghost_ (epoch.Ghost.ghost)} in
          let d_witness11 : (history) Ghost.t = {Ghost.ghost = ghost_ (d.Ghost.ghost)} in
          let base_witness12 : (pool) Ghost.t = {Ghost.ghost = ghost_ (base.Ghost.ghost)} in
          let refine_ t = t in
          let copy_source4 : {p : node Pref.t | H.mem saved_witness7.Ghost.ghost p} =
            let refine_ p = a in refine_ p in
          let resume_left : (left : {r : copied | let refine_ copy_source4 = copy_source4 in valid saved_witness7.Ghost.ghost epoch_witness10.Ghost.ghost depth r.#history && (clean_session r.#history)
      && Pref.own r.#state === heap saved_witness7.Ghost.ghost epoch_witness10.Ghost.ghost depth r.#history && r.#pool === registered base_witness12.Ghost.ghost epoch_witness10.Ghost.ghost r.#history && r.#trail === touched r.#history
      && extends d_witness11.Ghost.ghost r.#history && target_for saved_witness7.Ghost.ghost r.#history copy_source4 r.#value}) @ unique ->
            {r : copied | valid goal.heap goal.identity goal.depth r.#history && (clean_session r.#history)
      && Pref.own r.#state === heap goal.heap goal.identity goal.depth r.#history && r.#pool === registered goal.base goal.identity r.#history && r.#trail === touched r.#history
      && extends goal.initial r.#history && target_for goal.heap r.#history goal.root r.#value} @ unique = fun left ->
            let refine_ left = left in
          let pool = left.#pool in let trail = left.#trail in let d1 = ghost_ left.#history in let b : {p : node Pref.t | H.mem saved.Ghost.ghost p} = refine_ b in
          let t = left.#state in
          let t : {t : Pref.token | let refine_ p = b in valid saved.Ghost.ghost epoch.Ghost.ghost depth d1 && (clean_session d1) && Pref.own t === heap saved.Ghost.ghost epoch.Ghost.ghost depth d1 && pool === registered base.Ghost.ghost epoch.Ghost.ghost d1 && trail === touched d1} = refine_ t in
          let saved_witness13 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (saved.Ghost.ghost)} in
          let scope_witness14 : (((p : node Pref.t) @ immutable -> {u : unit | if H.mem saved_witness13.Ghost.ghost p then source_ok saved_witness13.Ghost.ghost p else H.at saved_witness13.Ghost.ghost p === None})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
          let clean_witness15 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at saved_witness13.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t = {Ghost.ghost = ghost_ (refine_ clean.Ghost.ghost)} in
          let epoch_witness16 : (node Pref.t) Ghost.t = {Ghost.ghost = ghost_ (epoch.Ghost.ghost)} in
          let d_witness17 : (history) Ghost.t = {Ghost.ghost = ghost_ (d1)} in
          let base_witness18 : (pool) Ghost.t = {Ghost.ghost = ghost_ (base.Ghost.ghost)} in
          let refine_ t = t in
          let copy_source5 : {p : node Pref.t | H.mem saved_witness13.Ghost.ghost p} =
            let refine_ p = b in refine_ p in
          let resume_right : (right : {r : copied | let refine_ copy_source5 = copy_source5 in valid saved_witness13.Ghost.ghost epoch_witness16.Ghost.ghost depth r.#history && (clean_session r.#history)
      && Pref.own r.#state === heap saved_witness13.Ghost.ghost epoch_witness16.Ghost.ghost depth r.#history && r.#pool === registered base_witness18.Ghost.ghost epoch_witness16.Ghost.ghost r.#history && r.#trail === touched r.#history
      && extends d_witness17.Ghost.ghost r.#history && target_for saved_witness13.Ghost.ghost r.#history copy_source5 r.#value}) @ unique ->
            {r : copied | valid goal.heap goal.identity goal.depth r.#history && (clean_session r.#history)
      && Pref.own r.#state === heap goal.heap goal.identity goal.depth r.#history && r.#pool === registered goal.base goal.identity r.#history && r.#trail === touched r.#history
      && extends goal.initial r.#history && target_for goal.heap r.#history goal.root r.#value} @ unique = fun right ->
            let refine_ right = right in
          let pool = right.#pool in let trail = right.#trail in let d2 = ghost_ right.#history in let desc = Arrow (left.#value, right.#value) in let dest = Allocate desc in
          ghost_ (let refine_ a = a in let refine_ b = b in let u = () in
            target_preserved saved.Ghost.ghost epoch.Ghost.ghost depth d1 d2 a left.#value (refine_ u);
            ready_def saved.Ghost.ghost d2 old.desc desc; prepared_def saved.Ghost.ghost d2 old.desc dest;
            let proof : {u : unit | prepared saved.Ghost.ghost d2 old.desc dest} = refine_ u in proof);
          let t = right.#state in
          let t : {t : Pref.token | let refine_ p = p in valid saved.Ghost.ghost epoch.Ghost.ghost depth d2 && (clean_session d2) && Pref.own t === heap saved.Ghost.ghost epoch.Ghost.ghost depth d2 && pool === registered base.Ghost.ghost epoch.Ghost.ghost d2 && trail === touched d2
            && match H.at saved.Ghost.ghost p with None -> false | Some v ->
              v.level === Generic && prepared saved.Ghost.ghost d2 v.desc dest} = refine_ t in
          let saved_witness35 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (saved.Ghost.ghost)} in
          let clean_witness36 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at saved_witness35.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t = {Ghost.ghost = ghost_ (refine_ clean.Ghost.ghost)} in
          let epoch_witness37 : (node Pref.t) Ghost.t = {Ghost.ghost = ghost_ (epoch.Ghost.ghost)} in
          let d_witness38 : (history) Ghost.t = {Ghost.ghost = ghost_ (d2)} in
          let base_witness39 : (pool) Ghost.t = {Ghost.ghost = ghost_ (base.Ghost.ghost)} in
          let refine_ t = t in
          let copy_source6 : {p : node Pref.t | H.mem saved_witness35.Ghost.ghost p && source_ok saved_witness35.Ghost.ghost p} =
            let refine_ p = p in refine_ p in
          let refine_ out = finish saved_witness35 clean_witness36 epoch_witness37 depth d_witness38 base_witness39 pool trail copy_source6 dest (refine_ t) in
          ghost_ (let u = () in extension_trans d.Ghost.ghost d1 d2 (refine_ u);
            extension_trans d.Ghost.ghost d2 out.#history (refine_ u));
          let r = #{value = out.#value; state = out.#state; pool = out.#pool; trail = out.#trail; history = out.#history} in use (refine_ r)
 in
          copy_work goal saved_witness13 scope_witness14 clean_witness15 epoch_witness16 depth d_witness17 base_witness18 pool trail copy_source5 (refine_ t) resume_right in
          copy_work goal saved_witness7 scope_witness8 clean_witness9 epoch_witness10 depth d_witness11 base_witness12 pool trail copy_source4 (refine_ t) resume_left
let copy : (saved : (Pref.heap) Ghost.t) @ immutable  ->(scope : (((p : node Pref.t) @ immutable -> {u : unit | if H.mem saved.Ghost.ghost p then source_ok saved.Ghost.ghost p else H.at saved.Ghost.ghost p === None})) Ghost.t) @ total  ->(clean : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at saved.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t) @ total  ->(epoch : (node Pref.t) Ghost.t) @ immutable  -> (depth : int)  ->(d : (history) Ghost.t) @ immutable  ->(base : (pool) Ghost.t) @ immutable  -> (pool : pool) @ immutable  ->
    (trail : pool) @ immutable  ->
    (p : {p : node Pref.t | H.mem saved.Ghost.ghost p}) @ immutable  ->
    (t : {t : Pref.token | let refine_ p = p in valid saved.Ghost.ghost epoch.Ghost.ghost depth d.Ghost.ghost && (clean_session d.Ghost.ghost) && Pref.own t === heap saved.Ghost.ghost epoch.Ghost.ghost depth d.Ghost.ghost && pool === registered base.Ghost.ghost epoch.Ghost.ghost d.Ghost.ghost && trail === touched d.Ghost.ghost}) @ unique  ->
    {r : copied | let refine_ p = p in valid saved.Ghost.ghost epoch.Ghost.ghost depth r.#history && (clean_session r.#history)
      && Pref.own r.#state === heap saved.Ghost.ghost epoch.Ghost.ghost depth r.#history && r.#pool === registered base.Ghost.ghost epoch.Ghost.ghost r.#history && r.#trail === touched r.#history
      && extends d.Ghost.ghost r.#history && target_for saved.Ghost.ghost r.#history p r.#value} @ unique  = fun saved scope clean epoch depth d base pool trail p t ->
    let goal = {heap = saved.Ghost.ghost; identity = epoch.Ghost.ghost; depth;
      initial = d.Ghost.ghost; base = base.Ghost.ghost; root = ghost_ (let refine_ p = p in p)} in
    let use : (r : {r : copied | let refine_ p = p in valid saved.Ghost.ghost epoch.Ghost.ghost depth r.#history && (clean_session r.#history)
      && Pref.own r.#state === heap saved.Ghost.ghost epoch.Ghost.ghost depth r.#history && r.#pool === registered base.Ghost.ghost epoch.Ghost.ghost r.#history && r.#trail === touched r.#history
      && extends d.Ghost.ghost r.#history && target_for saved.Ghost.ghost r.#history p r.#value}) @ unique -> {r : copied | valid goal.heap goal.identity goal.depth r.#history && (clean_session r.#history)
      && Pref.own r.#state === heap goal.heap goal.identity goal.depth r.#history && r.#pool === registered goal.base goal.identity r.#history && r.#trail === touched r.#history
      && extends goal.initial r.#history && target_for goal.heap r.#history goal.root r.#value} @ unique = fun r ->
      let refine_ r = r in refine_ r in
    let refine_ out = copy_work goal saved scope clean epoch depth d base pool trail p t use in refine_ out

let instantiate : (saved : (Pref.heap) Ghost.t) @ immutable  ->(clean : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at saved.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t) @ total  ->(scope : (((p : node Pref.t) @ immutable -> {u : unit | if H.mem saved.Ghost.ghost p then source_ok saved.Ghost.ghost p else H.at saved.Ghost.ghost p === None})) Ghost.t) @ total  ->
    (base : pool) @ immutable  -> (depth : {depth : int | depth >= 0})  -> (p : {p : node Pref.t | H.mem saved.Ghost.ghost p}) @ immutable  ->
    (t : {t : Pref.token | Pref.own t === saved.Ghost.ghost && pool_scoped saved.Ghost.ghost base}) @ unique  ->
    {r : instance | let refine_ p = p in let refine_ depth = depth in valid saved.Ghost.ghost r.#epoch depth r.#history && (clean_session r.#history)
      && Pref.own r.#state === heap saved.Ghost.ghost r.#epoch depth r.#history
      && r.#pool === registered base r.#epoch r.#history && r.#trail === touched r.#history
      && target_for saved.Ghost.ghost r.#history p r.#value && pool_scoped (Pref.own r.#state) r.#pool} @ unique = fun saved clean scope base depth p t  ->
  let refine_ depth = depth in let refine_ t = t in
  let epoch = ghost_ (let refine_ p = p in p) in let d = ghost_ Clean in let trail = Empty in let pool = base in
  ghost_ (let refine_ p = p in touched_def d; registered_def base epoch d; valid_def saved.Ghost.ghost epoch depth d; clean_session_def d; heap_def saved.Ghost.ghost epoch depth d);
  let t : {t : Pref.token | let refine_ p = p in valid saved.Ghost.ghost epoch depth d && (clean_session d) && Pref.own t === heap saved.Ghost.ghost epoch depth d && pool === registered base epoch d && trail === touched d} = refine_ t in
  let saved_witness19 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (saved.Ghost.ghost)} in
  let scope_witness20 : (((p : node Pref.t) @ immutable -> {u : unit | if H.mem saved_witness19.Ghost.ghost p then source_ok saved_witness19.Ghost.ghost p else H.at saved_witness19.Ghost.ghost p === None})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
  let clean_witness21 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at saved_witness19.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t = {Ghost.ghost = ghost_ (refine_ clean.Ghost.ghost)} in
  let epoch_witness22 : (node Pref.t) Ghost.t = {Ghost.ghost = ghost_ (epoch)} in
  let d_witness23 : (history) Ghost.t = {Ghost.ghost = ghost_ (d)} in
  let base_witness24 : (pool) Ghost.t = {Ghost.ghost = ghost_ (base)} in
  let refine_ t = t in
  let copy_source7 : {p : node Pref.t | H.mem saved_witness19.Ghost.ghost p} =
    let refine_ p = p in refine_ p in
  let refine_ r = copy saved_witness19 scope_witness20 clean_witness21 epoch_witness22 depth d_witness23 base_witness24 pool trail copy_source7 (refine_ t) in let refine_ p = p in
  ghost_ (let u = () in registered_scoped saved.Ghost.ghost scope.Ghost.ghost base epoch depth r.#history (refine_ u));
  let out = #{value = r.#value; state = r.#state; pool = r.#pool; trail = r.#trail; epoch; history = r.#history} in refine_ out
