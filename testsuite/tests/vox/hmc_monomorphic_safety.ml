module D = Hm_declarative
module C = Hmc_monomorphic
module T = Hmc_templates
module I = Hmc_instance
module E = Hmc_catalog_semantics
module H = Hmc_monomorphic_states
module P = Hmc_monomorphic_simulation
module S = Hmc_source_semantics
module Q = Hmc_monomorphic_semantics
module V = Hm_interpreter_typing

let rec (rebuild_typed @ total) : (catalog : T.catalog) @ immutable -> (entry : D.term) @ immutable ->
    (ty : D.mono) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | T.valid catalog && D.typed D.Z (T.context catalog) entry ty d} ->
    {u : unit | D.typed D.Z D.Empty_context (T.rebuild catalog entry) ty (T.rebuild_derivation catalog d)} @ ghost =
  fun catalog entry ty d premise -> ghost_ (
    T.valid_def catalog; T.context_def catalog;
    T.rebuild_def catalog entry; T.rebuild_derivation_def catalog d;
    match catalog with T.Empty -> () | T.Declare (definition, earlier) ->
      T.definition_valid_def earlier definition; I.scheme_valid earlier definition ();
      Hmc_parameter_closed.catalog_context earlier ();
      D.typed_def D.Z (T.context catalog) entry ty d;
      let term = D.Let (definition.T.source, entry) in
      let proof = D.Let_binding (definition.T.scheme, definition.T.derivation, d) in
      D.typed_def D.Z (T.context earlier) term ty proof;
      rebuild_typed earlier term ty proof ())

let[@def] (application @ total) (p : C.program @ immutable) (input : Hmc_word64.t @ immutable) =
  D.Apply (T.rebuild p.C.source.T.globals p.C.source.T.entry, D.Word input)
let[@def] (derivation @ total) (p : C.program @ immutable) =
  D.Application (D.Word64, T.rebuild_derivation p.C.source.T.globals p.C.source.T.derivation, D.Word_constant)
let (application_typed @ total) : (p : C.program) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    {u : unit | D.typed D.Z D.Empty_context (application p input) D.Word64 (derivation p)} @ ghost =
  fun p input -> ghost_ (
    C.ready_def p; T.ready_def p.C.source;
    rebuild_typed p.C.source.T.globals p.C.source.T.entry (D.Function (D.Word64, D.Word64)) p.C.source.T.derivation ();
    application_def p input; derivation_def p;
    D.typed_def D.Z D.Empty_context (application p input) D.Word64 (derivation p);
    D.typed_def D.Z D.Empty_context (D.Word input) D.Word64 D.Word_constant;
    D.context_wf_def D.Z D.Empty_context; D.mono_wf_def D.Z D.Word64)

let (safe @ total) : (p : C.program) @ immutable ->
    (definitions : {d : C.definitions | C.origins d}) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | p.C.definitions === definitions} ->
    {u : unit | not (Q.advance definitions fuel (P.target_start p input) === Q.Stuck)} @ ghost =
  fun p definitions input fuel premise -> ghost_ (
    match fuel with
    | D.Z -> Q.advance_def definitions D.Z (P.target_start p input);
      P.target_start_def p input; Q.initial_def (C.Apply (p.C.entry, C.Word input))
    | D.S n ->
      let out = P.run p definitions input n () in
      application_typed p input;
      Hmc_source_safety.source_safe (D.add (P.source_offset p) n) (application p input) D.Word64 (derivation p) ();
      application_def p input; P.source_start_def p input;
      H.source_def out; H.target_def out)
