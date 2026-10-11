open Hmc_source_semantics

let rec (advance_add @ total) : (a : D.index) @ immutable ->
    (b : D.index) @ immutable -> (state : state) @ immutable ->
    {u : unit | advance (D.add a b) state === advance b (advance a state)}
      @ ghost = fun a b state -> ghost_ (
  D.add_def a b; advance_def a state; advance_def (D.add a b) state;
  match a with D.Z -> () | D.S n -> advance_add n b (step state))

let rec (done_absorbing @ total) : (fuel : D.index) @ immutable ->
    (value : V.value) @ immutable ->
    {u : unit | advance fuel (Done value) === Done value} @ ghost =
  fun fuel value -> ghost_ (
    advance_def fuel (Done value); step_def (Done value);
    match fuel with D.Z -> () | D.S n -> done_absorbing n value)

let (normal_result_stable @ total) : (a : D.index) @ immutable ->
    (extra : D.index) @ immutable -> (state : state) @ immutable ->
    (v : V.value) @ immutable -> (w : V.value) @ immutable ->
    {u : unit | advance a state === Done v
      && advance (D.add a extra) state === Done w} ->
    {u : unit | v === w} @ ghost = fun a extra state v w premise -> ghost_ (
  advance_add a extra state; done_absorbing extra v; ())

type distance = Earlier of D.index | Later of D.index [@@inductive]

let rec (compare_steps @ total) : (a : D.index) @ immutable ->
    (b : D.index) @ immutable -> {r : distance | match r with
      | Earlier extra -> D.add a extra === b
      | Later extra -> D.add b extra === a} @ immutable = fun a b ->
  match a, b with
  | D.Z, _ -> ghost_ (D.add_def D.Z b); Earlier b
  | _, D.Z -> ghost_ (D.add_def D.Z a); Later a
  | D.S m, D.S n ->
    let r = compare_steps m n in
    ghost_ (match r with
      | Earlier extra -> D.add_def a extra
      | Later extra -> D.add_def b extra);
    r

let (normal_result_unique @ total) : (a : D.index) @ immutable ->
    (b : D.index) @ immutable -> (state : state) @ immutable ->
    (v : V.value) @ immutable -> (w : V.value) @ immutable ->
    {u : unit | advance a state === Done v && advance b state === Done w} ->
    {u : unit | v === w} @ ghost = fun a b state v w premise -> ghost_ (
  match compare_steps a b with
  | Earlier extra -> normal_result_stable a extra state v w ()
  | Later extra -> normal_result_stable b extra state w v ())
