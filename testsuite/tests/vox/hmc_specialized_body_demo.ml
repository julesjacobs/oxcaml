(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_elaboration_check.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_interpreter_typing.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_ground_arguments.ml hmc_instance.ml hmc_parameter_typing.ml hmc_parameter_closed.ml hmc_ground_annotations.ml hmc_specialized_body.ml hmc_specialized_body_demo.ml";
 { bytecode; }
 { native; }
*)
module D = Hm_declarative
module E = Hm_elaboration_check
module S = Hm_interpreter_substitution
module Args = Hmc_ground_arguments
module G = Hmc_ground_type
module I = Hmc_instance
module B = Hmc_specialized_body
module T = Hmc_templates
module W = Hmc_parameter_typing

let checked term ty d = match Hm_checked_elaboration.check term ty d with
  | Some x -> x | None -> failwith "invalid source derivation"
let catalog : D.term @ immutable -> D.typing @ immutable ->
    {c : T.catalog | T.valid c} @ immutable = fun term d ->
  match Hmc_grounding.ground (checked term (D.Function (D.Word64, D.Word64)) d) with
  | Hmc_grounding.Entry_type_mismatch -> raise (Failure "entry type")
  | Hmc_grounding.Grounded p -> match Hmc_admission.admit p with
    | Hmc_admission.Rejected _ -> raise (Failure "fragment")
    | Hmc_admission.Admitted p ->
      let p = T.extract p in ghost_ (T.ready_def p); refine_ p.T.globals

let body : (c : {c : T.catalog | T.valid c}) @ immutable -> D.index @ immutable ->
    Args.t @ immutable -> B.t @ immutable = fun c index args ->
  match I.request c index args with I.Instance i -> B.instantiate i
  | _ -> raise (Failure "instance request")

let validate (body : B.t @ immutable) =
  let origin = body.B.origin in
  if not (E.check D.Z (T.context origin.I.earlier) origin.I.definition.T.source
    (G.mono origin.I.ty) body.B.derivation) then failwith "specialized body not typed";
  if not (Hmc_ground_annotations.typing body.B.derivation) then failwith "nonground local annotation"

let () =
  let p = D.Parameter D.Z in let v = D.Variable D.No_arguments in
  let id = D.Lambda (D.Bound D.Z) in
  let scheme = D.Forall (D.S D.Z, D.Function (p, p)) in
  let id_d = D.Abstraction (p, v) in
  let root_d = D.Abstraction (D.Word64, v) in
  let c = catalog (D.Let (id, id)) (D.Let_binding (scheme, id_d, root_d)) in
  let list = G.List (G.Arrow (G.Word64, G.Bool)) in
  let inst = body c D.Z (Args.Argument (list, Args.Empty)) in
  validate inst;
  (match inst.B.derivation with D.Abstraction (D.List_type (D.Function (D.Word64, D.Boolean)), _) -> ()
  | _ -> failwith "higher-order list instance");
  let wrapper = D.Lambda (D.Apply (D.Bound (D.S D.Z), D.Bound D.Z)) in
  let wrapper_d = D.Abstraction (p,
    D.Application (p, D.Variable (D.Argument (p, D.No_arguments)), v)) in
  let c = catalog (D.Let (id, D.Let (wrapper, id)))
    (D.Let_binding (scheme, id_d, D.Let_binding (scheme, wrapper_d, root_d))) in
  let inst = body c D.Z (Args.Argument (G.List G.Word64, Args.Empty)) in
  validate inst;
  (match inst.B.derivation with
  | D.Abstraction (D.List_type D.Word64,
      D.Application (D.List_type D.Word64, D.Variable (D.Argument (D.List_type D.Word64, D.No_arguments)), _)) -> ()
  | _ -> failwith "earlier global use did not receive ground arguments");
  let local = D.Lambda (D.Let (id, D.Apply (D.Bound D.Z, D.Bound (D.S D.Z)))) in
  let proof = D.Abstraction (p, D.Let_binding (scheme, id_d,
    D.Application (p, D.Variable (D.Argument (p, D.No_arguments)), v))) in
  let source = D.S D.Z in let target = D.Z in
  let args = D.Argument (D.Word64, D.No_arguments) in
  let action = {S.front = args; tail = D.Z} in
  let ty = D.Function (p, p) in
  if E.check source D.Empty_context local ty proof then (
    let transformed = S.act_typing action proof in
    ghost_ (
      let mapping : ((i : D.index) @ immutable ->
        {u : unit | not (D.present source i) || D.mono_wf target (S.at action.S.front action.S.tail i)}) @ total =
        fun i -> D.present_def source i;
          (match i with D.Z -> S.at_def args D.Z i; D.mono_wf_def D.Z D.Word64
          | D.S rest -> D.present_def D.Z rest); () in
      W.typing_action action source target mapping D.Empty_context local ty proof ());
    if not (E.check D.Z D.Empty_context local (D.Function (D.Word64, D.Word64)) transformed)
    then failwith "substitution beneath a generalized local let";
    match transformed with
    | D.Abstraction (D.Word64, D.Let_binding (
        D.Forall (D.S D.Z, D.Function (D.Parameter D.Z, D.Parameter D.Z)),
        D.Abstraction (D.Parameter D.Z, _),
        D.Application (D.Word64, D.Variable (D.Argument (D.Word64, D.No_arguments)), _))) -> ()
    | _ -> failwith "bound local scheme parameter was captured")
  else failwith "invalid quantified-let fixture"
