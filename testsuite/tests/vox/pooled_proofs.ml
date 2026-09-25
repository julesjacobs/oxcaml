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
  match d with Start | Clean -> ()
  | Fresh (rest, _, _, _, _) | Alias (rest, _, _, _) -> registered_keeps base epoch rest x; ())
let rec (registered_covers @ total) : (saved : Pref.heap) @ immutable -> (base : pool) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable -> (cut : int) ->
    (x : node Pref.t) @ immutable -> {u : unit | valid saved epoch depth d && covered saved cut base x} ->
    {u : unit | covered (heap saved epoch depth d) cut (registered base epoch d) x} @ ghost =
  fun saved base epoch depth d cut x premise -> ghost_ (
    valid_def saved epoch depth d; heap_def saved epoch depth d; registered_def base epoch d;
    let after = heap saved epoch depth d in let pool = registered base epoch d in
    covered_def saved cut base x; covered_def after cut pool x;
    at_level_def saved x; at_level_def after x; listed_def pool x; if H.mem saved x then (
      history_at saved epoch depth d x (); registered_keeps base epoch d x; ())
    else match d with
    | Clean -> ()
  | Start -> let v = cell Bool depth in put_frame saved epoch v x; ()
    | Fresh (rest, p, q, old, desc) ->
      registered_covers saved base epoch depth rest cut x ();
      let mid = heap saved epoch depth rest in let prior = registered base epoch rest in
      covered_def mid cut prior x; at_level_def mid x;
      let v = cell desc depth in let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
      let h1 = H.put mid q v in put_frame h1 p w x;
      ()
    | Alias (rest, p, q, old) ->
      registered_covers saved base epoch depth rest cut x ();
      let mid = heap saved epoch depth rest in let prior = registered base epoch rest in
      covered_def mid cut prior x; at_level_def mid x; let v = session_mark rest old epoch q in session_mark_def rest old epoch q; put_frame mid p v x;
      ())
let rec (registered_member @ total) : (saved : Pref.heap) @ immutable -> (base : pool) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | valid saved epoch depth d && listed (registered base epoch d) x} ->
    {u : unit | listed base x || H.mem (heap saved epoch depth d) x} @ ghost = fun saved base epoch depth d x premise -> ghost_ (
  registered_def base epoch d; valid_def saved epoch depth d; heap_def saved epoch depth d;
  let pool = registered base epoch d in listed_def pool x; match d with
  | Clean -> ()
  | Start -> let v = cell Bool depth in put_frame saved epoch v x; ()
  | Fresh (rest, p, q, old, desc) ->
    let mid = heap saved epoch depth rest in let v = cell desc depth in let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
    let h1 = H.put mid q v in put_frame h1 p w x;
    if x === q then () else (registered_member saved base epoch depth rest x (); ())
  | Alias (rest, p, q, old) -> let mid = heap saved epoch depth rest in let v = session_mark rest old epoch q in session_mark_def rest old epoch q;
    put_frame mid p v x; registered_member saved base epoch depth rest x (); ())
let rec (pool_member @ total) : (h : Pref.heap) @ immutable -> (pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | pool_scoped h pool && listed pool x} ->
    {u : unit | H.mem h x} @ ghost = fun h pool x premise -> ghost_ (
  pool_scoped_def h pool; listed_def pool x; match pool with
  | Empty -> () | Entry (p, rest) -> if x === p then () else (pool_member h rest x (); ()))
let rec (pool_from_members @ total) : (h : Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || source_ok h x})) @ total ->
    (pool : pool) @ immutable ->
    (members : ((x : node Pref.t) @ immutable -> {u : unit | not (listed pool x) || H.mem h x})) @ total ->
    {u : unit | pool_scoped h pool} @ ghost = fun h scope pool members -> ghost_ (
  pool_scoped_def h pool; match pool with Empty -> () | Entry (p, rest) ->
    listed_def pool p; members p; scope p;
    let tail : ((x : node Pref.t) @ immutable -> {u : unit | not (listed rest x) || H.mem h x}) @ total =
      fun x -> listed_def pool x; members x; () in
    pool_from_members h scope rest tail; ())
let (registered_scoped @ total) : (saved : Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem saved x then source_ok saved x else H.at saved x === None})) @ total ->
    (base : pool) @ immutable -> (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    {u : unit | valid saved epoch depth d && pool_scoped saved base} ->
    {u : unit | pool_scoped (heap saved epoch depth d) (registered base epoch d)} @ ghost = fun saved scope base epoch depth d premise -> ghost_ (
  let after = heap saved epoch depth d in let pool = registered base epoch d in
  let after_scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem after x) || source_ok after x}) @ total =
    fun x -> let () = history_scope saved scope epoch depth d x () in () in
  let members : ((x : node Pref.t) @ immutable -> {u : unit | not (listed pool x) || H.mem after x}) @ total = fun x ->
    if listed pool x then (
      registered_member saved base epoch depth d x ();
      if listed base x then (pool_member saved base x (); history_grows saved epoch depth d x (); ()) else ())
    else () in
  let () = pool_from_members after after_scope pool members in ())

let (closed_pool @ total) : (h : Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None})) @ total ->
    (cut : int) -> (pool : pool) @ immutable -> {u : unit | pool_scoped h pool} ->
    {u : unit | pool_scoped (closed_heap h cut pool) pool} @ ghost = fun h scope cut pool premise -> ghost_ (
  let after = closed_heap h cut pool in
  let after_scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem after x) || source_ok after x}) @ total =
    fun x -> let () = Generalize_scheme_proofs.closed_scope h scope cut pool x () in () in
  let members : ((x : node Pref.t) @ immutable -> {u : unit | not (listed pool x) || H.mem after x}) @ total = fun x ->
    if listed pool x then (
      pool_member h pool x (); Generalize_proofs.closed_observe h cut pool x (); closed_at_def h after cut pool x; ()) else () in
  let () = pool_from_members after after_scope pool members in ())
