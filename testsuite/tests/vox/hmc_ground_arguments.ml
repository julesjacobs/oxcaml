module D = Hm_declarative
module G = Hmc_ground_type
module N = Hmc_no_free

type t = Empty | Argument of G.t * t [@@inductive]

let[@def] rec (declarative @ total) (args : t @ immutable) = match args with
  | Empty -> D.No_arguments
  | Argument (a, rest) -> D.Argument (G.mono a, declarative rest)
let[@def] rec (length @ total) (args : t @ immutable) = match args with
  | Empty -> D.Z | Argument (_, rest) -> D.S (length rest)

let rec (read_type @ total) : (ty : D.mono) @ immutable ->
    {r : G.t option | match r with None -> not (G.ground ty)
      | Some out -> G.ground ty && G.mono out === ty} @ immutable = fun ty ->
  ghost_ (G.ground_def ty);
  match ty with
  | D.Boolean -> ghost_ (G.mono_def G.Bool); Some G.Bool
  | D.Word64 -> ghost_ (G.mono_def G.Word64); Some G.Word64
  | D.Parameter _ | D.Free _ -> None
  | D.List_type a -> (match read_type a with None -> None | Some a ->
    let out = G.List a in ghost_ (G.mono_def out); Some out)
  | D.Function (a, b) -> (match read_type a with None -> None | Some a ->
    match read_type b with None -> None | Some b ->
      let out = G.Arrow (a, b) in ghost_ (G.mono_def out); Some out)

let[@def] rec (ground @ total) (args : D.arguments @ immutable) = match args with
  | D.No_arguments -> true
  | D.Argument (a, rest) -> G.ground a && ground rest

let rec (read @ total) : (args : D.arguments) @ immutable ->
    {r : t option | match r with None -> not (ground args)
      | Some out -> ground args && declarative out === args} @ immutable = fun args ->
  ghost_ (ground_def args);
  match args with
  | D.No_arguments -> ghost_ (declarative_def Empty); Some Empty
  | D.Argument (a, rest) -> (match read_type a with None -> None | Some a ->
    match read rest with None -> None | Some rest ->
      let out = Argument (a, rest) in ghost_ (declarative_def out); Some out)

let rec (equal @ total) : (a : t) @ immutable -> (b : t) @ immutable ->
    {r : bool | r = (a === b)} = fun a b -> match a, b with
  | Empty, Empty -> true
  | Argument (a, rest), Argument (b, other) -> G.equal a b && equal rest other
  | _ -> false

let rec (represented @ total) : (args : t) @ immutable ->
    {u : unit | ground (declarative args) && D.length (declarative args) === length args} @ ghost =
  fun args -> ghost_ (
    declarative_def args; length_def args;
    ground_def (declarative args); D.length_def (declarative args);
    match args with Empty -> () | Argument (a, rest) -> G.is_ground a; represented rest)

let rec (parameter_ground @ total) : (args : D.arguments) @ immutable ->
    (i : D.index) @ immutable ->
    {u : unit | ground args && D.present (D.length args) i} ->
    {u : unit | G.ground (D.open_index args i)} @ ghost = fun args i premise -> ghost_ (
    ground_def args; D.length_def args; D.present_def (D.length args) i; D.open_index_def args i;
    match args, i with D.Argument (_, rest), D.S i -> parameter_ground rest i () | _ -> ())

let rec (open_ground @ total) : (args : D.arguments) @ immutable -> (ty : D.mono) @ immutable ->
    {u : unit | ground args && D.mono_wf (D.length args) ty && N.mono ty} ->
    {u : unit | G.ground (D.open_type args ty)} @ ghost = fun args ty premise -> ghost_ (
    D.mono_wf_def (D.length args) ty; N.mono_def ty; D.open_type_def args ty;
    G.ground_def (D.open_type args ty);
    match ty with
    | D.Parameter i -> parameter_ground args i ()
    | D.Free _ | D.Boolean | D.Word64 -> ()
    | D.List_type a -> open_ground args a ()
    | D.Function (a, b) -> open_ground args a (); open_ground args b ())

let (instantiate @ total) :
    (scheme : {s : D.scheme | D.scheme_wf D.Z s && N.scheme s}) @ immutable ->
    (args : {a : t | length a === D.arity scheme}) @ immutable ->
    {ty : G.t | G.mono ty === D.open_scheme scheme (declarative args)} @ immutable = fun scheme args ->
  let actuals = declarative args in
  ghost_ (represented args; N.scheme_def scheme; D.scheme_wf_def D.Z scheme;
    D.arity_def scheme; D.open_scheme_def scheme actuals;
    match scheme with D.Forall (k, body) ->
      Hm_abstraction_proofs.add_zero k; open_ground actuals body ());
  let opened = D.open_scheme scheme actuals in
  match read_type opened with Some ty -> ty | None -> unreachable_ ()

type key = {owner : D.index; arguments : t}

let (key_equal @ total) : (a : key) @ immutable -> (b : key) @ immutable ->
    {r : bool | r = (a === b)} = fun a b ->
  Hm_elaboration_check.index_equal a.owner b.owner && equal a.arguments b.arguments
