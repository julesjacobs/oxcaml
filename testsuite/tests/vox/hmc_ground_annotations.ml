module D = Hm_declarative
module G = Hmc_ground_type
module N = Hmc_no_free
module A = Hmc_admission

let[@def] rec (arguments @ total) (args : D.arguments @ immutable) = match args with
  | D.No_arguments -> true | D.Argument (ty, rest) -> G.ground ty && arguments rest
let[@def] rec (typing @ total) (d : D.typing @ immutable) = match d with
  | D.Variable args -> arguments args
  | D.Constant | D.Word_constant -> true
  | D.Empty_list a -> G.ground a
  | D.List_cons (a, h, t) -> G.ground a && typing h && typing t
  | D.List_case (a, s, l, r) -> G.ground a && typing s && typing l && typing r
  | D.Conditional (c, a, b) -> typing c && typing a && typing b
  | D.Word_primitive (a, b) -> typing a && typing b
  | D.Abstraction (a, b) -> G.ground a && typing b
  | D.Application (a, f, x) -> G.ground a && typing f && typing x
  | D.Recursion (a, b, d) -> G.ground a && G.ground b && typing d
  | D.Let_binding (D.Forall (D.Z, ty), a, b) -> G.ground ty && typing a && typing b
  | D.Let_binding _ -> false

let rec (mono_ground @ total) : (ty : D.mono) @ immutable ->
    {u : unit | D.mono_wf D.Z ty && N.mono ty} -> {u : unit | G.ground ty} @ ghost =
  fun ty premise -> ghost_ (
    D.mono_wf_def D.Z ty; N.mono_def ty; G.ground_def ty;
    match ty with D.Parameter i -> D.present_def D.Z i
    | D.Free _ | D.Boolean | D.Word64 -> ()
    | D.List_type a -> mono_ground a ()
    | D.Function (a, b) -> mono_ground a (); mono_ground b ())

let rec (arguments_ground @ total) : (args : D.arguments) @ immutable ->
    {u : unit | D.arguments_wf D.Z args && N.arguments args} ->
    {u : unit | arguments args} @ ghost = fun args premise -> ghost_ (
    D.arguments_wf_def D.Z args; N.arguments_def args; arguments_def args;
    match args with D.No_arguments -> () | D.Argument (ty, rest) ->
      mono_ground ty (); arguments_ground rest ())

let rec (typing_ground @ total) : (g : D.context) @ immutable -> (term : D.term) @ immutable ->
    (ty : D.mono) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | D.typed D.Z g term ty d && N.typing d && A.local term d} ->
    {u : unit | typing d} @ ghost = fun g term ty d premise -> ghost_ (
    D.typed_def D.Z g term ty d; N.typing_def d; A.local_def term d; typing_def d;
    match term, d with
    | D.Bound _, D.Variable args -> arguments_ground args ()
    | (D.Truth | D.False), D.Constant | D.Word _, D.Word_constant -> ()
    | D.Nil, D.Empty_list a -> mono_ground a ()
    | D.Cons (h, t), D.List_cons (a, dh, dt) ->
      D.mono_wf_def D.Z (D.List_type a); mono_ground a ();
      typing_ground g h a dh (); typing_ground g t ty dt ()
    | D.CaseList (s, l, r), D.List_case (a, ds, dl, dr) ->
      mono_ground a (); typing_ground g s (D.List_type a) ds (); typing_ground g l ty dl ();
      let inner = D.Binding (D.Forall (D.Z, a), D.Binding (D.Forall (D.Z, D.List_type a), g)) in
      typing_ground inner r ty dr ()
    | D.If (c, a, b), D.Conditional (dc, da, db) ->
      typing_ground g c D.Boolean dc (); typing_ground g a ty da (); typing_ground g b ty db ()
    | D.Primitive (_, a, b), D.Word_primitive (da, db) ->
      typing_ground g a D.Word64 da (); typing_ground g b D.Word64 db ()
    | D.Lambda body, D.Abstraction (a, db) ->
      (match ty with D.Function (_, b) ->
        D.mono_wf_def D.Z ty; mono_ground a ();
        typing_ground (D.Binding (D.Forall (D.Z, a), g)) body b db ()
      | _ -> ())
    | D.Apply (f, x), D.Application (a, df, dx) ->
      D.typed_def D.Z g x a dx; mono_ground a ();
      typing_ground g f (D.Function (a, ty)) df (); typing_ground g x a dx ()
    | D.Recursive body, D.Recursion (a, b, db) ->
      D.mono_wf_def D.Z ty; mono_ground a (); mono_ground b ();
      typing_ground (D.Binding (D.Forall (D.Z, a), D.Binding (D.Forall (D.Z, ty), g))) body b db ()
    | D.Let (rhs, body), D.Let_binding (D.Forall (D.Z, a), dr, db) ->
      let scheme = D.Forall (D.Z, a) in
      D.add_def D.Z D.Z; D.scheme_wf_def D.Z scheme; N.scheme_def scheme; mono_ground a ();
      typing_ground (D.weaken_context D.Z g) rhs a dr ();
      typing_ground (D.Binding (scheme, g)) body ty db ()
    | _ -> ())
