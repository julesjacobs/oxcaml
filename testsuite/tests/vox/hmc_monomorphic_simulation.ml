module D = Hm_declarative
module C = Hmc_monomorphic
module M = Hmc_manifest
module T = Hmc_templates
module L = Hmc_monomorphic_links
module E = Hmc_catalog_semantics
module W = Hmc_monomorphic_values
module H = Hmc_monomorphic_states
module S = Hmc_source_semantics
module Q = Hmc_monomorphic_semantics
module V = Hm_interpreter_typing

let rec (advance @ total) : (p : C.program) @ immutable ->
    (definitions : {d : C.definitions | C.origins d}) @ immutable -> (fuel : D.index) @ immutable ->
    (state : H.state) @ immutable ->
    {u : unit | p.C.definitions === definitions && H.valid (C.manifest definitions) state} ->
    {out : H.state | H.valid (C.manifest definitions) out
      && H.source out === S.advance fuel (H.source state)
      && H.target out === Q.advance definitions fuel (H.target state)} @ immutable =
  fun p definitions fuel state premise ->
    ghost_ (S.advance_def fuel (H.source state); Q.advance_def definitions fuel (H.target state));
    match fuel with D.Z -> state | D.S n ->
      let next = Hmc_monomorphic_step.step p definitions state () in advance p definitions n next ()

let (word_agreement @ total) : (state : H.state) @ immutable -> (word : Hmc_word64.t) @ immutable ->
    {u : unit | (H.source state === S.Done (V.Word word)) = (H.target state === Q.Done (Q.V.Word word))} @ ghost =
  fun state word -> ghost_ (
    H.source_def state; H.target_def state;
    match state with H.Done v -> W.source_def v; W.target_def v | _ -> ())

let[@def] (source_start @ total) (p : C.program @ immutable) (word : Hmc_word64.t @ immutable) =
  S.initial (D.Apply (T.rebuild p.C.source.T.globals p.C.source.T.entry, D.Word word))
let[@def] (target_start @ total) (p : C.program @ immutable) (word : Hmc_word64.t @ immutable) =
  Q.initial (C.Apply (p.C.entry, C.Word word))
let[@def] (source_offset @ total) (p : C.program @ immutable) = D.S (E.startup_steps p.C.source.T.globals)

let (initial @ total) : (p : C.program) @ immutable ->
    (definitions : {d : C.definitions | C.origins d}) @ immutable -> (word : Hmc_word64.t) @ immutable ->
    {u : unit | p.C.definitions === definitions} ->
    {state : H.state | H.valid (C.manifest definitions) state
      && H.source state === S.advance (source_offset p) (source_start p word)
      && H.target state === Q.step definitions (target_start p word)} @ immutable = fun p definitions word premise ->
  let k = H.Apply_function (T.Empty, W.Empty, C.Word word, H.Halt) in
  let control = H.Evaluate (p.C.source.T.globals, W.Empty, p.C.entry) in
  let state = H.Running (control, k) in
  ghost_ (
    C.ready_def p; T.ready_def p.C.source;
    L.from_records (C.manifest definitions) p.C.source.T.globals D.Empty_context p.C.entry p.C.source.T.derivation ();
    L.depth_def D.Empty_context;
    H.valid_def (C.manifest definitions) state;
    H.control_valid_def (C.manifest definitions) control;
    H.environment_valid_def (C.manifest definitions) W.Empty;
    W.valid_def (C.manifest definitions) W.Empty; W.environment_def W.Empty; W.length_def W.Empty;
    H.continuation_valid_def (C.manifest definitions) k;
    H.continuation_valid_def (C.manifest definitions) H.Halt;
    L.linked_def (C.manifest definitions) T.Empty D.Z (C.Word word);
    H.source_def state; H.target_def state;
    H.source_control_def control; H.target_control_def control;
    H.source_continuation_def k; H.target_continuation_def k;
    H.source_continuation_def H.Halt; H.target_continuation_def H.Halt;
    H.source_environment_def p.C.source.T.globals W.Empty; H.source_environment_def T.Empty W.Empty;
    W.source_def W.Empty; W.target_def W.Empty;
    E.environment_def T.Empty; E.append_def V.Empty V.Empty;
    E.append_def V.Empty (E.environment p.C.source.T.globals); C.erase_def (C.Word word);
    source_start_def p word; target_start_def p word; source_offset_def p;
    S.initial_def (D.Apply (T.rebuild p.C.source.T.globals p.C.source.T.entry, D.Word word));
    Q.initial_def (C.Apply (p.C.entry, C.Word word));
    S.advance_def (source_offset p) (source_start p word);
    S.step_def (source_start p word); Q.step_def definitions (target_start p word);
    E.initialize p.C.source.T.globals p.C.source.T.entry (S.Apply_function (V.Empty, D.Word word, S.Halt)) ());
  state

let (run @ total) : (p : C.program) @ immutable ->
    (definitions : {d : C.definitions | C.origins d}) @ immutable ->
    (word : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | p.C.definitions === definitions} ->
    {out : H.state | H.valid (C.manifest definitions) out
      && H.source out === S.advance (D.add (source_offset p) fuel) (source_start p word)
      && H.target out === Q.advance definitions (D.S fuel) (target_start p word)} @ immutable =
  fun p definitions word fuel premise ->
    let start = initial p definitions word () in
    let out = advance p definitions fuel start () in
    ghost_ (S.advance_add (source_offset p) fuel (source_start p word);
      Q.advance_def definitions (D.S fuel) (target_start p word));
    out

let (normal_return_at_offsets @ total) : (p : C.program) @ immutable ->
    (definitions : {d : C.definitions | C.origins d}) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (output : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | p.C.definitions === definitions} ->
    {u : unit | (S.advance (D.add (source_offset p) fuel) (source_start p input) === S.Done (V.Word output))
      = (Q.advance definitions (D.S fuel) (target_start p input) === Q.Done (Q.V.Word output))} @ ghost =
  fun p definitions input output fuel premise -> ghost_ (
    let out = run p definitions input fuel () in word_agreement out output)

let rec (add_right_successor @ total) : (a : D.index) @ immutable -> (b : D.index) @ immutable ->
    {u : unit | D.add a (D.S b) === D.S (D.add a b)} @ ghost = fun a b -> ghost_ (
  D.add_def a (D.S b); D.add_def a b; match a with D.Z -> () | D.S n -> add_right_successor n b)
let rec (add_commute @ total) : (a : D.index) @ immutable -> (b : D.index) @ immutable ->
    {u : unit | D.add a b === D.add b a} @ ghost = fun a b -> ghost_ (
  D.add_def a b; match a with D.Z -> Hm_abstraction_proofs.add_zero b
  | D.S n -> add_commute n b; add_right_successor b n)
let rec (target_advance_add @ total) : (definitions : {d : C.definitions | C.origins d}) @ immutable ->
    (a : D.index) @ immutable -> (b : D.index) @ immutable -> (state : Q.state) @ immutable ->
    {u : unit | Q.advance definitions (D.add a b) state
      === Q.advance definitions b (Q.advance definitions a state)} @ ghost = fun definitions a b state -> ghost_ (
  D.add_def a b; Q.advance_def definitions a state; Q.advance_def definitions (D.add a b) state;
  match a with D.Z -> () | D.S n -> target_advance_add definitions n b (Q.step definitions state))
let rec (target_done_absorbing @ total) : (definitions : {d : C.definitions | C.origins d}) @ immutable ->
    (fuel : D.index) @ immutable -> (value : Q.V.value) @ immutable ->
    {u : unit | Q.advance definitions fuel (Q.Done value) === Q.Done value} @ ghost = fun definitions fuel value -> ghost_ (
  Q.advance_def definitions fuel (Q.Done value); Q.step_def definitions (Q.Done value);
  match fuel with D.Z -> () | D.S n -> target_done_absorbing definitions n value)

let (normal_return_preservation @ total) : (p : C.program) @ immutable ->
    (definitions : {d : C.definitions | C.origins d}) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (output : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | p.C.definitions === definitions
      && S.advance fuel (source_start p input) === S.Done (V.Word output)} ->
    {u : unit | Q.advance definitions (D.S fuel) (target_start p input) === Q.Done (Q.V.Word output)} @ ghost =
  fun p definitions input output fuel premise -> ghost_ (
    add_commute (source_offset p) fuel;
    S.advance_add fuel (source_offset p) (source_start p input);
    S.done_absorbing (source_offset p) (V.Word output);
    normal_return_at_offsets p definitions input output fuel ())

let (normal_return_reflection @ total) : (p : C.program) @ immutable ->
    (definitions : {d : C.definitions | C.origins d}) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (output : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | p.C.definitions === definitions
      && Q.advance definitions fuel (target_start p input) === Q.Done (Q.V.Word output)} ->
    {u : unit | S.advance (D.add (source_offset p) fuel) (source_start p input) === S.Done (V.Word output)} @ ghost =
  fun p definitions input output fuel premise -> ghost_ (
    let one = D.S D.Z in
    add_commute one fuel; D.add_def one fuel; D.add_def D.Z fuel;
    target_advance_add definitions fuel one (target_start p input);
    target_done_absorbing definitions one (Q.V.Word output);
    normal_return_at_offsets p definitions input output fuel ())
