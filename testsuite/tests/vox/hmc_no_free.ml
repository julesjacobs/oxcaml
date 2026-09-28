module D = Hm_declarative
module S = Hm_substitution

let[@def] rec (mono @ total) (ty : D.mono @ immutable) = match ty with
  | D.Free _ -> false | D.Parameter _ | D.Boolean | D.Word64 -> true
  | D.List_type a -> mono a | D.Function (a, b) -> mono a && mono b
let[@def] (scheme @ total) (s : D.scheme @ immutable) = match s with D.Forall (_, t) -> mono t
let[@def] rec (arguments @ total) (args : D.arguments @ immutable) = match args with
  | D.No_arguments -> true | D.Argument (t, rest) -> mono t && arguments rest
let[@def] rec (typing @ total) (d : D.typing @ immutable) = match d with
  | D.Variable args -> arguments args
  | D.Constant | D.Word_constant -> true
  | D.Empty_list a -> mono a
  | D.List_cons (a, h, t) -> mono a && typing h && typing t
  | D.List_case (a, s, l, r) -> mono a && typing s && typing l && typing r
  | D.Conditional (c, a, b) -> typing c && typing a && typing b
  | D.Word_primitive (a, b) -> typing a && typing b
  | D.Abstraction (a, b) -> mono a && typing b
  | D.Application (a, f, x) -> mono a && typing f && typing x
  | D.Recursion (a, b, d) -> mono a && mono b && typing d
  | D.Let_binding (s, a, b) -> scheme s && typing a && typing b

let rec (ground_mono @ total) : (ty : Hmc_ground_type.t) @ immutable ->
    {u : unit | mono (D.embed (Hmc_ground_type.copy ty))} @ ghost = fun ty -> ghost_ (
    Hmc_ground_type.copy_def ty; D.embed_def (Hmc_ground_type.copy ty);
    mono_def (D.embed (Hmc_ground_type.copy ty));
    match ty with Hmc_ground_type.Bool | Hmc_ground_type.Word64 -> ()
    | Hmc_ground_type.List a -> ground_mono a
    | Hmc_ground_type.Arrow (a, b) -> ground_mono a; ground_mono b)

let rec (substitute_mono @ total) :
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (closed : ((p : Copy_spec.node Pref.t) @ immutable -> {u : unit | mono (D.embed (rho p))})) @ total ->
    (ty : D.mono) @ immutable -> {u : unit | mono (S.substitute_type rho ty)} @ ghost =
  fun rho closed ty -> ghost_ (
    S.substitute_type_def rho ty; mono_def (S.substitute_type rho ty);
    match ty with
    | D.Free p -> closed p
    | D.Parameter _ | D.Boolean | D.Word64 -> ()
    | D.List_type a -> substitute_mono rho closed a
    | D.Function (a, b) -> substitute_mono rho closed a; substitute_mono rho closed b)

let rec (substitute_arguments @ total) :
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (closed : ((p : Copy_spec.node Pref.t) @ immutable -> {u : unit | mono (D.embed (rho p))})) @ total ->
    (args : D.arguments) @ immutable -> {u : unit | arguments (S.substitute_arguments rho args)} @ ghost =
  fun rho closed args -> ghost_ (
    S.substitute_arguments_def rho args; arguments_def (S.substitute_arguments rho args);
    match args with D.No_arguments -> ()
    | D.Argument (t, rest) -> substitute_mono rho closed t; substitute_arguments rho closed rest)

let rec (substitute_typing @ total) :
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (closed : ((p : Copy_spec.node Pref.t) @ immutable -> {u : unit | mono (D.embed (rho p))})) @ total ->
    (d : D.typing) @ immutable -> {u : unit | typing (S.substitute_typing rho d)} @ ghost =
  fun rho closed d -> ghost_ (
    S.substitute_typing_def rho d; typing_def (S.substitute_typing rho d);
    match d with
    | D.Variable args -> substitute_arguments rho closed args
    | D.Constant | D.Word_constant -> ()
    | D.Empty_list a -> substitute_mono rho closed a
    | D.List_cons (a, h, t) -> substitute_mono rho closed a; substitute_typing rho closed h; substitute_typing rho closed t
    | D.List_case (a, s, l, r) -> substitute_mono rho closed a; substitute_typing rho closed s;
      substitute_typing rho closed l; substitute_typing rho closed r
    | D.Conditional (c, a, b) -> substitute_typing rho closed c; substitute_typing rho closed a; substitute_typing rho closed b
    | D.Word_primitive (a, b) -> substitute_typing rho closed a; substitute_typing rho closed b
    | D.Abstraction (a, b) -> substitute_mono rho closed a; substitute_typing rho closed b
    | D.Application (a, f, x) -> substitute_mono rho closed a; substitute_typing rho closed f; substitute_typing rho closed x
    | D.Recursion (a, b, d) -> substitute_mono rho closed a; substitute_mono rho closed b; substitute_typing rho closed d
    | D.Let_binding (s, a, b) ->
      S.substitute_scheme_def rho s; scheme_def (S.substitute_scheme rho s);
      (match s with D.Forall (_, t) -> substitute_mono rho closed t);
      substitute_typing rho closed a; substitute_typing rho closed b)
