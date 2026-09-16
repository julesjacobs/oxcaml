open Copy_spec
open Copy_heap_proofs
open Copy_model_proofs
open Level_spec
open Generalize_spec
open Pooled_spec

let rec (registered_keeps @ total) : (base : pool) @ immutable -> (epoch : node Pref.t) @ immutable ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | not (listed base x) || listed (registered base epoch d) x} @ ghost = fun base epoch d x -> ghost_ (
  registered_def base epoch d; let pool = registered base epoch d in listed_def pool x;
  let u = () in match d with Start | Clean -> refine_ u
  | Fresh (rest, _, _, _, _) | Alias (rest, _, _, _) -> registered_keeps base epoch rest x; refine_ u)
let rec (registered_covers @ total) : (saved : node Pref.heap) @ immutable -> (base : pool) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable -> (cut : int) ->
    (x : node Pref.t) @ immutable -> {u : unit | valid saved epoch depth d && covered saved cut base x} ->
    {u : unit | covered (heap saved epoch depth d) cut (registered base epoch d) x} @ ghost =
  fun saved base epoch depth d cut x premise -> ghost_ (
    let refine_ premise = premise in valid_def saved epoch depth d; heap_def saved epoch depth d; registered_def base epoch d;
    let after = heap saved epoch depth d in let pool = registered base epoch d in
    covered_def saved cut base x; covered_def after cut pool x;
    at_level_def saved x; at_level_def after x; listed_def pool x; let u = () in
    if H.mem saved x then (
      history_at saved epoch depth d x (refine_ u); registered_keeps base epoch d x; refine_ u)
    else match d with
    | Clean -> refine_ u
  | Start -> let v = cell Bool depth in put_frame saved epoch v x; refine_ u
    | Fresh (rest, p, q, old, desc) ->
      registered_covers saved base epoch depth rest cut x (refine_ u);
      let mid = heap saved epoch depth rest in let prior = registered base epoch rest in
      covered_def mid cut prior x; at_level_def mid x;
      let v = cell desc depth in let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
      put_frame mid q v x; let h1 = H.put mid q v in put_frame h1 p w x;
      refine_ u
    | Alias (rest, p, q, old) ->
      registered_covers saved base epoch depth rest cut x (refine_ u);
      let mid = heap saved epoch depth rest in let prior = registered base epoch rest in
      covered_def mid cut prior x; at_level_def mid x; let v = session_mark rest old epoch q in session_mark_def rest old epoch q; put_frame mid p v x;
      refine_ u)
let rec (registered_member @ total) : (saved : node Pref.heap) @ immutable -> (base : pool) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | valid saved epoch depth d && listed (registered base epoch d) x} ->
    {u : unit | listed base x || H.mem (heap saved epoch depth d) x} @ ghost = fun saved base epoch depth d x premise -> ghost_ (
  let refine_ premise = premise in registered_def base epoch d; valid_def saved epoch depth d; heap_def saved epoch depth d;
  let pool = registered base epoch d in listed_def pool x; let u = () in match d with
  | Clean -> refine_ u
  | Start -> let v = cell Bool depth in put_frame saved epoch v x; refine_ u
  | Fresh (rest, p, q, old, desc) ->
    let mid = heap saved epoch depth rest in let v = cell desc depth in let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
    put_frame mid q v x; let h1 = H.put mid q v in put_frame h1 p w x;
    if x === q then refine_ u else (registered_member saved base epoch depth rest x (refine_ u); refine_ u)
  | Alias (rest, p, q, old) -> let mid = heap saved epoch depth rest in let v = session_mark rest old epoch q in session_mark_def rest old epoch q;
    put_frame mid p v x; registered_member saved base epoch depth rest x (refine_ u); refine_ u)
let rec (pool_member @ total) : (h : node Pref.heap) @ immutable -> (pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | pool_scoped h pool && listed pool x} ->
    {u : unit | H.mem h x} @ ghost = fun h pool x premise -> ghost_ (
  let refine_ premise = premise in pool_scoped_def h pool; listed_def pool x; let u = () in match pool with
  | Empty -> refine_ u | Entry (p, rest) -> if x === p then refine_ u else (pool_member h rest x (refine_ u); refine_ u))
let rec (pool_from_members @ total) : (h : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || source_ok h x})) @ total ->
    (pool : pool) @ immutable ->
    (members : ((x : node Pref.t) @ immutable -> {u : unit | not (listed pool x) || H.mem h x})) @ total ->
    {u : unit | pool_scoped h pool} @ ghost = fun h scope pool members -> ghost_ (
  pool_scoped_def h pool; let u = () in match pool with Empty -> refine_ u | Entry (p, rest) ->
    listed_def pool p; members p; scope p;
    let tail : ((x : node Pref.t) @ immutable -> {u : unit | not (listed rest x) || H.mem h x}) @ total =
      fun x -> listed_def pool x; members x; let u = () in refine_ u in
    pool_from_members h scope rest tail; refine_ u)
let (registered_scoped @ total) : (saved : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem saved x then source_ok saved x else H.at saved x === None})) @ total ->
    (base : pool) @ immutable -> (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    {u : unit | valid saved epoch depth d && pool_scoped saved base} ->
    {u : unit | pool_scoped (heap saved epoch depth d) (registered base epoch d)} @ ghost = fun saved scope base epoch depth d premise -> ghost_ (
  let refine_ premise = premise in let after = heap saved epoch depth d in let pool = registered base epoch d in
  let after_scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem after x) || source_ok after x}) @ total =
    fun x -> let u = () in let refine_ u = history_scope saved scope epoch depth d x (refine_ u) in refine_ u in
  let members : ((x : node Pref.t) @ immutable -> {u : unit | not (listed pool x) || H.mem after x}) @ total = fun x ->
    let u = () in if listed pool x then (
      registered_member saved base epoch depth d x (refine_ u);
      if listed base x then (pool_member saved base x (refine_ u); history_grows saved epoch depth d x (refine_ u); refine_ u) else refine_ u)
    else refine_ u in
  let refine_ u = pool_from_members after after_scope pool members in refine_ u)

let (closed_pool @ total) : (h : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None})) @ total ->
    (cut : int) -> (pool : pool) @ immutable -> {u : unit | pool_scoped h pool} ->
    {u : unit | pool_scoped (closed_heap h cut pool) pool} @ ghost = fun h scope cut pool premise -> ghost_ (
  let refine_ premise = premise in let after = closed_heap h cut pool in
  let after_scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem after x) || source_ok after x}) @ total =
    fun x -> let u = () in let refine_ u = Generalize_scheme_proofs.closed_scope h scope cut pool x (refine_ u) in refine_ u in
  let members : ((x : node Pref.t) @ immutable -> {u : unit | not (listed pool x) || H.mem after x}) @ total = fun x ->
    let u = () in if listed pool x then (
      pool_member h pool x (refine_ u); Generalize_proofs.closed_observe h cut pool x (refine_ u); closed_at_def h after cut pool x; refine_ u) else refine_ u in
  let refine_ u = pool_from_members after after_scope pool members in refine_ u)
