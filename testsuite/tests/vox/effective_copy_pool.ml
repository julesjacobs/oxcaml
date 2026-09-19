open Copy_spec
open Copy_heap_proofs
open Level_spec
open Generalize_spec
open Pooled_spec
open Pooled_proofs
open Effective_copy_spec
open Effective_copy_heap_proofs
open Effective_copy_metadata
let rec (registered_covers @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total -> (base : pool) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable -> (cut : int) ->
    (x : node Pref.t) @ immutable -> {u : unit | effective_valid saved heads epoch depth d && covered saved cut base x} ->
    {u : unit | covered (heap saved epoch depth d) cut (registered base epoch d) x} @ ghost =
  fun saved heads base epoch depth d cut x premise -> ghost_ (
    effective_valid_def saved heads epoch depth d; heap_def saved epoch depth d; registered_def base epoch d;
    let after = heap saved epoch depth d in let pool = registered base epoch d in
    covered_def saved cut base x; covered_def after cut pool x;
    at_level_def saved x; at_level_def after x; listed_def pool x; if H.mem saved x then (
      history_at saved heads epoch depth d x (); registered_keeps base epoch d x; ())
    else match d with
    | Clean -> ()
  | Start -> let v = cell Bool depth in put_frame saved epoch v x; ()
    | Fresh (rest, p, q, old, desc) ->
      registered_covers saved heads base epoch depth rest cut x ();
      let mid = heap saved epoch depth rest in let prior = registered base epoch rest in
      covered_def mid cut prior x; at_level_def mid x;
      let v = cell desc depth in let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
      let h1 = H.put mid q v in put_frame h1 p w x;
      ()
    | Alias (rest, p, q, old) ->
      registered_covers saved heads base epoch depth rest cut x ();
      let mid = heap saved epoch depth rest in let prior = registered base epoch rest in
      covered_def mid cut prior x; at_level_def mid x; let v = session_mark rest old epoch q in session_mark_def rest old epoch q; put_frame mid p v x;
      ())
let rec (registered_member @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total -> (base : pool) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | effective_valid saved heads epoch depth d && listed (registered base epoch d) x} ->
    {u : unit | listed base x || H.mem (heap saved epoch depth d) x} @ ghost = fun saved heads base epoch depth d x premise -> ghost_ (
  registered_def base epoch d; effective_valid_def saved heads epoch depth d; heap_def saved epoch depth d;
  let pool = registered base epoch d in listed_def pool x; match d with
  | Clean -> ()
  | Start -> let v = cell Bool depth in put_frame saved epoch v x; ()
  | Fresh (rest, p, q, old, desc) ->
    let mid = heap saved epoch depth rest in let v = cell desc depth in let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
    let h1 = H.put mid q v in put_frame h1 p w x;
    if x === q then () else (registered_member saved heads base epoch depth rest x (); ())
  | Alias (rest, p, q, old) -> let mid = heap saved epoch depth rest in let v = session_mark rest old epoch q in session_mark_def rest old epoch q;
    put_frame mid p v x; registered_member saved heads base epoch depth rest x (); ())
let (registered_scoped @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem saved x then source_ok saved x else H.at saved x === None})) @ total ->
    (base : pool) @ immutable -> (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && pool_scoped saved base} ->
    {u : unit | pool_scoped (heap saved epoch depth d) (registered base epoch d)} @ ghost = fun saved heads scope base epoch depth d premise -> ghost_ (
  let after = heap saved epoch depth d in let pool = registered base epoch d in
  let after_scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem after x) || source_ok after x}) @ total =
    fun x -> let () = history_scope saved heads scope epoch depth d x () in () in
  let members : ((x : node Pref.t) @ immutable -> {u : unit | not (listed pool x) || H.mem after x}) @ total = fun x ->
    if listed pool x then (
      registered_member saved heads base epoch depth d x ();
      if listed base x then (pool_member saved base x (); history_grows saved heads epoch depth d x (); ()) else ())
    else () in
  let () = pool_from_members after after_scope pool members in ())


let (registered_representatives @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total -> (base : pool) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable -> (cut : int) ->
    (x : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && Representative_level.representative_covered saved cut base x} ->
    {u : unit | Representative_level.representative_covered (heap saved epoch depth d) cut (registered base epoch d) x} @ ghost =
  fun saved heads base epoch depth d cut x premise -> ghost_ (
    let after = heap saved epoch depth d in let pool = registered base epoch d in
    Representative_level.representative_covered_def saved cut base x;
    Representative_level.representative_covered_def after cut pool x;
    if Level_unifier_spec.terminal after x then (
      history_at saved heads epoch depth d x ();
      Level_unifier_spec.terminal_def saved x; Level_unifier_spec.terminal_def after x;
      Level_unifier_spec.observe_def saved x; Level_unifier_spec.observe_def after x;
      covered_def saved cut base x;
      registered_covers saved heads base epoch depth d cut x (); ())
    else ())
