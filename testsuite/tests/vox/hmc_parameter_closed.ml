open Hm_declarative
open Hm_interpreter_substitution
module P = Hm_interpreter_substitution_proofs
module N = Hmc_no_free
module A = Hmc_admission
module T = Hmc_templates

let rec (shift_closed @ total) : (cut : index) @ immutable -> (k : index) @ immutable ->
    (ty : mono) @ immutable -> {u : unit | mono_wf cut ty} ->
    {u : unit | shift cut k ty === ty} @ ghost = fun cut k ty premise -> ghost_ (
    mono_wf_def cut ty; shift_def cut k ty;
    match ty with Parameter i -> P.shift_low cut k i ()
    | Free _ | Boolean | Word64 -> ()
    | List_type a -> shift_closed cut k a ()
    | Function (a, b) -> shift_closed cut k a (); shift_closed cut k b ())

let rec (act_closed @ total) : (cut : index) @ immutable -> (s : substitution) @ immutable ->
    (ty : mono) @ immutable -> {u : unit | mono_wf cut ty} ->
    {u : unit | act (lift cut s) ty === ty} @ ghost = fun cut s ty premise -> ghost_ (
    mono_wf_def cut ty; act_def (lift cut s) ty;
    match ty with Parameter i -> P.at_lift_low cut s i ()
    | Free _ | Boolean | Word64 -> ()
    | List_type a -> act_closed cut s a ()
    | Function (a, b) -> act_closed cut s a (); act_closed cut s b ())

let rec (closed_context @ total) : (s : substitution) @ immutable -> (k : index) @ immutable ->
    (g : context) @ immutable -> {u : unit | context_wf Z g} ->
    {u : unit | act_context s g === g && weaken_context k g === g} @ ghost =
  fun s k g premise -> ghost_ (
    context_wf_def Z g; act_context_def s g; weaken_context_def k g;
    match g with Empty_context -> () | Binding (scheme, rest) ->
      scheme_wf_def Z scheme; act_scheme_def s scheme; weaken_scheme_def k scheme;
      (match scheme with Forall (bound, ty) ->
        Hm_abstraction_proofs.add_zero bound;
        shift_closed bound k ty (); act_closed bound s ty ());
      closed_context s k rest ())

let rec (catalog_context @ total) : (catalog : T.catalog) @ immutable ->
    {u : unit | T.valid catalog} -> {u : unit | context_wf Z (T.context catalog)} @ ghost =
  fun catalog premise -> ghost_ (
    T.valid_def catalog; T.context_def catalog; context_wf_def Z (T.context catalog);
    match catalog with T.Empty -> () | T.Declare (d, earlier) ->
      Hmc_instance.scheme_valid earlier d (); catalog_context earlier ())

let rec (shift_no_free @ total) : (cut : index) @ immutable -> (k : index) @ immutable ->
    (ty : mono) @ immutable -> {u : unit | N.mono ty} ->
    {u : unit | N.mono (shift cut k ty)} @ ghost = fun cut k ty premise -> ghost_ (
    N.mono_def ty; shift_def cut k ty; N.mono_def (shift cut k ty);
    match ty with Parameter _ | Free _ | Boolean | Word64 -> ()
    | List_type a -> shift_no_free cut k a ()
    | Function (a, b) -> shift_no_free cut k a (); shift_no_free cut k b ())

let rec (shift_arguments_no_free @ total) : (k : index) @ immutable -> (args : arguments) @ immutable ->
    {u : unit | N.arguments args} -> {u : unit | N.arguments (shift_arguments k args)} @ ghost =
  fun k args premise -> ghost_ (
    N.arguments_def args; shift_arguments_def k args; N.arguments_def (shift_arguments k args);
    match args with No_arguments -> () | Argument (ty, rest) ->
      shift_no_free Z k ty (); shift_arguments_no_free k rest ())

let rec (lift_no_free @ total) : (k : index) @ immutable -> (s : substitution) @ immutable ->
    {u : unit | N.arguments s.front} -> {u : unit | N.arguments (lift k s).front} @ ghost =
  fun k s premise -> ghost_ (
    lift_def k s; match k with Z -> () | S k ->
      lift_no_free k s ();
      let base = lift k s in bump_def base;
      shift_arguments_no_free (S Z) base.front ();
      N.arguments_def (bump base).front; N.mono_def (Parameter Z); ())

let rec (at_no_free @ total) : (args : arguments) @ immutable -> (tail : index) @ immutable ->
    (i : index) @ immutable -> {u : unit | N.arguments args} ->
    {u : unit | N.mono (at args tail i)} @ ghost = fun args tail i premise -> ghost_ (
    N.arguments_def args; at_def args tail i; N.mono_def (at args tail i);
    match args, i with Argument (_, rest), S i -> at_no_free rest tail i () | _ -> ())

let rec (type_no_free @ total) : (s : substitution) @ immutable -> (ty : mono) @ immutable ->
    {u : unit | N.arguments s.front && N.mono ty} ->
    {u : unit | N.mono (act s ty)} @ ghost = fun s ty premise -> ghost_ (
    N.mono_def ty; act_def s ty; N.mono_def (act s ty);
    match ty with Parameter i -> at_no_free s.front s.tail i ()
    | Free _ | Boolean | Word64 -> ()
    | List_type a -> type_no_free s a ()
    | Function (a, b) -> type_no_free s a (); type_no_free s b ())

let rec (arguments_no_free @ total) : (s : substitution) @ immutable -> (args : arguments) @ immutable ->
    {u : unit | N.arguments s.front && N.arguments args} ->
    {u : unit | N.arguments (act_arguments s args)} @ ghost = fun s args premise -> ghost_ (
    N.arguments_def args; act_arguments_def s args; N.arguments_def (act_arguments s args);
    match args with No_arguments -> () | Argument (ty, rest) ->
      type_no_free s ty (); arguments_no_free s rest ())

let (scheme_no_free @ total) : (s : substitution) @ immutable -> (scheme : scheme) @ immutable ->
    {u : unit | N.arguments s.front && N.scheme scheme} ->
    {u : unit | N.scheme (act_scheme s scheme)} @ ghost = fun s scheme premise -> ghost_ (
    N.scheme_def scheme; act_scheme_def s scheme; N.scheme_def (act_scheme s scheme);
    match scheme with Forall (k, ty) -> lift_no_free k s (); type_no_free (lift k s) ty ())

let rec (typing_no_free @ total) : (s : substitution) @ immutable -> (d : typing) @ immutable ->
    {u : unit | N.arguments s.front && N.typing d} ->
    {u : unit | N.typing (act_typing s d)} @ ghost = fun s d premise -> ghost_ (
    N.typing_def d; act_typing_def s d; N.typing_def (act_typing s d);
    match d with
    | Variable args -> arguments_no_free s args ()
    | Constant | Word_constant -> ()
    | Empty_list a -> type_no_free s a ()
    | List_cons (a, h, t) -> type_no_free s a (); typing_no_free s h (); typing_no_free s t ()
    | List_case (a, v, l, r) -> type_no_free s a (); typing_no_free s v (); typing_no_free s l (); typing_no_free s r ()
    | Conditional (c, a, b) -> typing_no_free s c (); typing_no_free s a (); typing_no_free s b ()
    | Word_primitive (a, b) -> typing_no_free s a (); typing_no_free s b ()
    | Abstraction (a, b) -> type_no_free s a (); typing_no_free s b ()
    | Application (a, f, x) -> type_no_free s a (); typing_no_free s f (); typing_no_free s x ()
    | Recursion (a, b, d) -> type_no_free s a (); type_no_free s b (); typing_no_free s d ()
    | Let_binding (scheme, rhs, body) ->
      scheme_no_free s scheme (); typing_no_free s body ();
      (match scheme with Forall (k, _) -> lift_no_free k s (); typing_no_free (lift k s) rhs ()))

let rec (local_shape @ total) : (s : substitution) @ immutable -> (term : term) @ immutable ->
    (d : typing) @ immutable -> {u : unit | A.local term d} ->
    {u : unit | A.local term (act_typing s d)} @ ghost = fun s term d premise -> ghost_ (
    A.local_def term d; act_typing_def s d; A.local_def term (act_typing s d);
    match term, d with
    | Lambda body, Abstraction (_, db) | Recursive body, Recursion (_, _, db) -> local_shape s body db ()
    | Apply (a, b), Application (_, da, db) | Cons (a, b), List_cons (_, da, db)
    | Primitive (_, a, b), Word_primitive (da, db) -> local_shape s a da (); local_shape s b db ()
    | If (a, b, c), Conditional (da, db, dc) | CaseList (a, b, c), List_case (_, da, db, dc) ->
      local_shape s a da (); local_shape s b db (); local_shape s c dc ()
    | Let (a, b), Let_binding (Forall (Z, ty), da, db) ->
      act_scheme_def s (Forall (Z, ty)); lift_def Z s;
      local_shape s a da (); local_shape s b db ()
    | _ -> ())
