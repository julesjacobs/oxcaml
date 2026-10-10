module D = Hm_declarative
module A = Hm_annotation_trace
module Eq = Hm_annotation_equations
module Shape = Hm_annotation_shape
module E = Hm_elaboration
module G = Hm_generalization
module P = Hm_elaboration_projection
module Ty = Copy_spec

let (member @ total) :
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ghost ->
    (all : A.trace) @ immutable ghost -> (within : A.trace) @ immutable ghost ->
    (read : ((p : {p : Ty.node Pref.t | A.contains p all}) @ immutable ->
      {ty : Ty.ty | ty === rho p} @ immutable)) @ local total ->
    (p : Ty.node Pref.t) @ immutable ->
    {u : unit | A.subtrace within all && A.contains p within} ->
    {ty : Ty.ty | ty === rho p} @ immutable =
  fun rho all within read p premise ->
    ghost_ (A.subtrace_contains p within all ()); read p

let (arguments @ total) : (left : D.term) @ immutable -> (right : D.term) @ immutable ->
    (trace : A.trace) @ immutable ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    {u : unit | Shape.matches (Hm_primitive_constraints.arguments left right) trace && Eq.equations rho trace} ->
    {u : unit | match A.root trace with None -> false | Some p -> rho p === Ty.List_type Ty.Word64} @ ghost =
  fun left right trace rho premise -> ghost_ (
    Hm_primitive_constraints.arguments_def left right;
    let word = D.Word {Hmc_word64.lo = 0; hi = 0} in
    Shape.matches_def (D.Cons (left, D.Cons (right, D.Cons (word, D.Nil)))) trace;
    Eq.equations_def rho trace; A.root_def trace;
    match trace with
    | A.List_constructor (_, _, rest) ->
      Shape.matches_def (D.Cons (right, D.Cons (word, D.Nil))) rest;
      Eq.equations_def rho rest; A.root_def rest;
      (match rest with
      | A.List_constructor (_, _, tail) ->
        Shape.matches_def (D.Cons (word, D.Nil)) tail; Eq.equations_def rho tail; A.root_def tail;
        (match tail with
        | A.List_constructor (_, literal, _) ->
          Shape.matches_def word literal; Eq.equations_def rho literal; A.root_def literal
        | _ -> ())
      | _ -> ())
    | _ -> ())

let rec (lookup_present @ total) : (context : D.context) @ immutable ->
    (index : D.index) @ immutable ->
    {u : unit | D.present (D.depth context) index} ->
    {u : unit | not (D.lookup context index === None)} @ ghost =
  fun context index premise -> ghost_ (
    D.depth_def context; D.present_def (D.depth context) index; D.lookup_def context index;
    match context, index with
    | D.Binding (_, rest), D.S index -> lookup_present rest index ()
    | _ -> ())

let (conditional_scoped @ total) : (n : D.index) @ immutable -> (context : D.context) @ immutable ->
    (condition : D.term) @ immutable -> (yes : D.term) @ immutable -> (no : D.term) @ immutable ->
    {u : unit | D.context_wf n context && D.scoped_term (D.depth context) condition
      && D.scoped_term (D.depth context) yes && D.scoped_term (D.depth context) no} ->
    {u : unit | D.scoped_term (D.depth context) (Hm_conditional_constraints.encoded condition yes no)} @ ghost =
  fun n context condition yes no premise -> ghost_ (
    D.mono_wf_def n D.Boolean;
    let derivation = Hm_conditional_constraints.selector_construct n context D.Boolean D.Boolean () in
    let ty = D.Function (D.Boolean, D.Function (D.Boolean, D.Function (D.Boolean, D.Boolean))) in
    let selector = Hm_conditional_constraints.selector () in
    Hm_type_proofs.typing_scoped n context selector ty derivation ();
    let depth = D.depth context in
    Hm_conditional_constraints.encoded_def condition yes no;
    Hm_conditional_constraints.condition_def condition;
    let nil = D.Nil in let tail = D.Cons (D.Truth, nil) in let first = D.Cons (condition, tail) in
    D.scoped_term_def depth nil; D.scoped_term_def depth D.Truth;
    D.scoped_term_def depth tail; D.scoped_term_def depth first;
    let left = D.Apply (selector, first) in let middle = D.Apply (left, yes) in
    D.scoped_term_def depth left; D.scoped_term_def depth middle;
    D.scoped_term_def depth (D.Apply (middle, no)))

let (list_case_scoped @ total) : (n : D.index) @ immutable -> (context : D.context) @ immutable ->
    (scrutinee : D.term) @ immutable -> (empty : D.term) @ immutable -> (nonempty : D.term) @ immutable ->
    {u : unit | D.context_wf n context && D.scoped_term (D.depth context) scrutinee
      && D.scoped_term (D.depth context) empty && D.scoped_term (D.S (D.S (D.depth context))) nonempty} ->
    {u : unit | D.scoped_term (D.depth context) (Hm_list_case_constraints.encoded scrutinee empty nonempty)} @ ghost =
  fun n context scrutinee empty nonempty premise -> ghost_ (
    D.mono_wf_def n D.Boolean;
    let derivation = Hm_list_case_constraints.selector_construct n context D.Boolean D.Boolean () in
    let ty = D.Function (D.List_type D.Boolean, D.Function (D.Boolean, D.Function (
      D.Function (D.List_type D.Boolean, D.Function (D.Boolean, D.Boolean)), D.Boolean))) in
    let selector = Hm_list_case_constraints.selector () in
    Hm_type_proofs.typing_scoped n context selector ty derivation ();
    let depth = D.depth context in
    Hm_list_case_constraints.encoded_def scrutinee empty nonempty;
    let body = D.Lambda nonempty in let branch = D.Lambda body in
    D.scoped_term_def (D.S depth) body; D.scoped_term_def depth branch;
    let left = D.Apply (selector, scrutinee) in let middle = D.Apply (left, empty) in
    D.scoped_term_def depth left; D.scoped_term_def depth middle;
    D.scoped_term_def depth (D.Apply (middle, branch)))

let (primitive_scoped @ total) : (depth : D.index) @ immutable ->
    (left : D.term) @ immutable -> (right : D.term) @ immutable ->
    {u : unit | D.scoped_term depth left && D.scoped_term depth right} ->
    {u : unit | D.scoped_term depth (Hm_primitive_constraints.arguments left right)} @ ghost =
  fun depth left right premise -> ghost_ (
    Hm_primitive_constraints.arguments_def left right;
    let word = D.Word {Hmc_word64.lo = 0; hi = 0} in
    let tail = D.Cons (word, D.Nil) in let middle = D.Cons (right, tail) in
    D.scoped_term_def depth D.Nil; D.scoped_term_def depth word;
    D.scoped_term_def depth tail; D.scoped_term_def depth middle;
    D.scoped_term_def depth (D.Cons (left, middle)))

module Instances = Hm_reconstruction_instances

let rec (build @ total) :
    (all : A.trace) @ immutable ghost ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ghost ->
    (scope : E.scope) @ immutable -> (context : D.context) @ immutable ->
    (source : D.term) @ immutable -> (trace : A.trace) @ immutable -> (ty : Ty.ty) @ immutable ghost ->
    (read : ((p : {p : Ty.node Pref.t | A.contains p all}) @ immutable ->
      {ty : Ty.ty | ty === rho p} @ immutable)) @ local total ->
    {u : unit | A.subtrace trace all && Eq.annotates source trace rho ty
      && D.context_wf (E.depth scope) context && D.scoped_term (D.depth context) source
      && Instances.satisfied rho scope context source trace} ->
    {d : D.typing | D.typed (E.depth scope) context source (E.interpret scope ty) d} @ immutable =
  fun all rho scope context source trace ty read premise ->
    ghost_ (Instances.satisfied_def rho scope context source trace;
      Eq.annotates_def source trace rho ty;
      Shape.matches_def source trace; Eq.equations_def rho trace; A.root_def trace;
      D.scoped_term_def (D.depth context) source;
      A.children trace all (); E.interpret_wf scope ty; E.interpret_def scope ty);
    match source, trace with
    | D.Bound i, A.Variable_use p ->
      ghost_ (A.contains_def p trace; lookup_present context i ());
      let target = E.interpret scope (member rho all trace read p ()) in
      (match D.lookup context i with
      | None -> unreachable_ ()
      | Some scheme ->
        ghost_ (Hm_instantiation.well_formed (E.depth scope) scheme target ());
        match Hm_instantiation.infer scheme target with
        | None -> unreachable_ ()
        | Some args ->
          let d = D.Variable args in
          ghost_ (D.typed_def (E.depth scope) context source (E.interpret scope ty) d); d)
    | D.Truth, A.Boolean_literal _ | D.False, A.False_literal _ ->
      let d = D.Constant in
      ghost_ (D.typed_def (E.depth scope) context source (E.interpret scope ty) d); d
    | D.Word _, A.Word_literal _ ->
      let d = D.Word_constant in
      ghost_ (D.typed_def (E.depth scope) context source (E.interpret scope ty) d); d
    | D.Nil, A.Empty_list_literal p ->
      ghost_ (A.contains_def p trace);
      (match member rho all trace read p () with
      | Ty.List_type element ->
        let a = E.interpret scope element in let d = D.Empty_list a in
        ghost_ (E.interpret_wf scope element;
          D.typed_def (E.depth scope) context source (E.interpret scope ty) d); d
      | _ -> unreachable_ ())
    | D.Lambda body, A.Abstraction (_, argument, child) ->
      ghost_ (A.contains_def argument trace; Shape.root body child ());
      let a = E.interpret scope (member rho all trace read argument ()) in
      let inner = D.Binding (D.Forall (D.Z, a), context) in
      ghost_ (D.depth_def inner; E.interpret_wf scope (rho argument); Hm_conditional_constraints.binding_wf (E.depth scope) context a ());
      (match A.root child with
      | None -> unreachable_ ()
      | Some b ->
        ghost_ (Eq.annotates_def body child rho (rho b));
        let db = build all rho scope inner body child (ghost_ (rho b)) read () in
        let d = D.Abstraction (a, db) in
        ghost_ (D.typed_def (E.depth scope) context source (E.interpret scope ty) d); d)
    | D.Apply (left, right), A.Application (_, l, r) ->
      (match A.root l, A.root r with
      | Some f, Some a ->
        ghost_ (A.root_contains r a (); Eq.annotates_def left l rho (rho f); Eq.annotates_def right r rho (rho a));
        let argument = E.interpret scope (member rho all r read a ()) in
        (let dl = build all rho scope context left l (ghost_ (rho f)) read () in
          let dr = build all rho scope context right r (ghost_ (rho a)) read () in
          let d = D.Application (argument, dl, dr) in
          ghost_ (E.interpret_def scope (rho f);
            D.typed_def (E.depth scope) context source (E.interpret scope ty) d); d)
      | _ -> unreachable_ ())
    | D.Cons (left, right), A.List_constructor (_, l, r) ->
      (match A.root l, A.root r with
      | Some head, Some tail ->
        ghost_ (A.root_contains l head (); Eq.annotates_def left l rho (rho head); Eq.annotates_def right r rho (rho tail));
        let element = E.interpret scope (member rho all l read head ()) in
        (let dl = build all rho scope context left l (ghost_ (rho head)) read () in
          let dr = build all rho scope context right r (ghost_ (rho tail)) read () in
          let d = D.List_cons (element, dl, dr) in
          ghost_ (D.typed_def (E.depth scope) context source (E.interpret scope ty) d); d)
      | _ -> unreachable_ ())
    | D.Recursive body, A.Recursion (_, argument, result, child) ->
      ghost_ (A.contains_def argument trace; A.contains_def result trace);
      let a = E.interpret scope (member rho all trace read argument ()) in
      let b = E.interpret scope (member rho all trace read result ()) in
      let self = D.Function (a, b) in
      let outer = D.Binding (D.Forall (D.Z, self), context) in
      let inner = D.Binding (D.Forall (D.Z, a), outer) in
      ghost_ (Hm_elaboration_preparation.recursive_context scope context (rho argument) (rho result) ();
        Eq.annotates_def body child rho (rho result));
      (let db = build all rho scope inner body child (ghost_ (rho result)) read () in
        let d = D.Recursion (a, b, db) in
        ghost_ (D.typed_def (E.depth scope) context source (E.interpret scope ty) d); d)
    | D.If (condition, yes, no), A.Conditional child ->
      let encoded = Hm_conditional_constraints.encoded condition yes no in
      ghost_ (conditional_scoped (E.depth scope) context condition yes no ();
        Eq.annotates_def encoded child rho ty);
      (let d = build all rho scope context encoded child (ghost_ ty) read () in
        P.conditional (E.depth scope) context condition yes no (ghost_ (E.interpret scope ty)) d ())
    | D.CaseList (scrutinee, empty, nonempty), A.List_case child ->
      let encoded = Hm_list_case_constraints.encoded scrutinee empty nonempty in
      ghost_ (list_case_scoped (E.depth scope) context scrutinee empty nonempty ();
        Eq.annotates_def encoded child rho ty);
      (let d = build all rho scope context encoded child (ghost_ ty) read () in
        P.list_case (E.depth scope) context scrutinee empty nonempty (ghost_ (E.interpret scope ty)) d ())
    | D.Primitive (op, left, right), A.Primitive (_, _, child) ->
      let encoded = Hm_primitive_constraints.arguments left right in
      ghost_ (primitive_scoped (D.depth context) left right (); arguments left right child rho (); Eq.annotates_def encoded child rho (Ty.List_type Ty.Word64));
      (let d = build all rho scope context encoded child (Ty.List_type Ty.Word64) read () in
        ghost_ (E.interpret_def scope (Ty.List_type Ty.Word64); E.interpret_def scope Ty.Word64; D.operation_type_def op);
        P.primitive (E.depth scope) context op left right d ())
    | D.Let (rhs, body), A.Let_binding (r, b) ->
      ghost_ (Shape.root rhs r ());
      (match A.root r with
      | None -> unreachable_ ()
      | Some p ->
        ghost_ (A.root_contains r p ());
        let root = member rho all r read p () in
        let value = E.interpret scope root in
        let generalized = G.generalize context value in
        let scheme = generalized.G.scheme in let arity = D.arity scheme in
        let extended = E.Quantifiers (generalized.G.variables, scope) in
        let rhs_context = D.weaken_context arity context in
        let body_context = D.Binding (scheme, context) in
        ghost_ (Hm_elaboration_preparation.generalization scope context root ();
          Eq.annotates_def rhs r rho root; Eq.annotates_def body b rho ty);
        (let dr = build all rho extended rhs_context rhs r root read () in
          let db = build all rho scope body_context body b (ghost_ ty) read () in
          let d = D.Let_binding (scheme, dr, db) in
          ghost_ (D.typed_def (E.depth scope) context source (E.interpret scope ty) d); d))
    | _ -> unreachable_ ()
