open Copy_spec
open Hm_environment_spec
module D = Hm_declarative
module T = Hm_type_proofs
let rec (interpret_boundary_agreement @ total) : (s : template) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (equal : ((x : node Pref.t) @ immutable ->
      {u : unit | not (boundary_member s x) || rho x === tau x})) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    {u : unit | interpret rho choices s === interpret tau choices s} @ ghost =
  fun s rho tau equal choices -> ghost_ (
    interpret_def rho choices s; interpret_def tau choices s;
    (match s with
    | Boundary p -> boundary_member_def s p; equal p; ()
    | Parameter _ | Constant _ -> ()
    | Product (_, a, b) ->
      let left : ((x : node Pref.t) @ immutable ->
        {u : unit | not (boundary_member a x) || rho x === tau x}) @ total = fun x ->
        boundary_member_def s x; equal x; () in
      let right : ((x : node Pref.t) @ immutable ->
        {u : unit | not (boundary_member b x) || rho x === tau x}) @ total = fun x ->
        boundary_member_def s x; equal x; () in
      interpret_boundary_agreement a rho tau left choices;
      interpret_boundary_agreement b rho tau right choices; ()
    | Indirect (_, child) ->
      let next : ((x : node Pref.t) @ immutable ->
        {u : unit | not (boundary_member child x) || rho x === tau x}) @ total = fun x ->
        boundary_member_def s x; equal x; () in
      interpret_boundary_agreement child rho tau next choices; ());
    ())

let rec (aligned_lookup @ total) : (g : D.context) @ immutable ->
    (ts : templates) @ immutable -> (i : D.index) @ immutable ->
    {u : unit | aligned g ts} ->
    {u : unit | match D.lookup g i, template_lookup ts i with
      None, None | Some _, Some _ -> true | _ -> false} @ ghost =
  fun g ts i premise -> ghost_ (
    aligned_def g ts;
    D.lookup_def g i; template_lookup_def ts i;
    match g with
    | D.Empty_context -> ()
    | D.Binding (_, rest) -> match ts with No_templates -> ()
      | Template_binding (_, tail) -> match i with D.Z -> ()
        | D.S i -> aligned_lookup rest tail i (); ())

let (realize_empty @ total) :
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (xi : (D.index @ immutable total -> ty @ immutable total)) @ total ->
    (i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
    (schema : template) @ immutable -> (args : T.values) @ immutable ->
    {u : unit | D.lookup D.Empty_context i === Some sigma && template_lookup No_templates i === Some schema
      && T.values_length args === D.arity sigma} -> (claim : bool) ->
    (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | interpret rho choices schema === T.meaning xi sigma args} ->
      {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun rho xi i sigma schema args premise claim use -> ghost_ (
    let g = D.Empty_context in
    D.lookup_def g i; ())

let rec (eval_empty @ total) : (args : T.values) @ immutable ->
    (xi : (D.index @ immutable total -> ty @ immutable total)) @ total -> (a : D.mono) @ immutable ->
    {u : unit | args === T.No_values} ->
    {u : unit | T.eval_prefixed args xi a === T.eval xi a} @ ghost =
  fun args xi a premise -> ghost_ (
    T.eval_prefixed_def args xi a; T.eval_def xi a;
    match a with
    | D.Parameter i -> T.prefix_def args xi i; ()
    | D.Free _ | D.Boolean -> ()
    | D.Function (a, b) -> eval_empty args xi a ();
      eval_empty args xi b (); ())

let (realize_monomorphic @ total) :
    (g : D.context) @ immutable -> (ts : templates) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (xi : (D.index @ immutable total -> ty @ immutable total)) @ total ->
    (realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
    (schema : template) @ immutable -> (args : T.values) @ immutable ->
    {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
      && T.values_length args === D.arity sigma} -> (claim : bool) ->
    (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | interpret rho choices schema === T.meaning xi sigma args} ->
      {u : unit | claim})) @ total -> {u : unit | claim})) @ total ->
    (p : node Pref.t) @ immutable -> (a : D.mono) @ immutable ->
    {u : unit | rho p === T.eval xi a} ->
    (i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
    (schema : template) @ immutable -> (args : T.values) @ immutable ->
    {u : unit | D.lookup (D.Binding (D.Forall (D.Z, a), g)) i === Some sigma && template_lookup (Template_binding (Boundary p, ts)) i === Some schema
      && T.values_length args === D.arity sigma} -> (claim : bool) ->
    (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | interpret rho choices schema === T.meaning xi sigma args} ->
      {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun g ts rho xi realize p a fit i sigma schema args premise claim use -> ghost_ (
    let z = D.Z in let mono = D.Forall (z, a) in
    let next = D.Binding (mono, g) in let bound = Boundary p in
    let next_ts = Template_binding (bound, ts) in
    D.lookup_def next i; template_lookup_def next_ts i;
    match i with
    | D.Z -> D.arity_def mono; T.values_length_def args;
      (match args with
      | T.No_values ->
        T.meaning_def xi mono args; eval_empty args xi a ();
        interpret_def rho rho bound;
        let () = use rho () in ()
      | T.Value _ -> ())
    | D.S i -> let () = realize i sigma schema args () claim use in ())

let (realize_weaken @ total) :
    (g : D.context) @ immutable -> (ts : templates) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (xi : (D.index @ immutable total -> ty @ immutable total)) @ total ->
    (realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
    (schema : template) @ immutable -> (args : T.values) @ immutable ->
    {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
      && T.values_length args === D.arity sigma} -> (claim : bool) ->
    (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | interpret rho choices schema === T.meaning xi sigma args} ->
      {u : unit | claim})) @ total -> {u : unit | claim})) @ total ->
    (ambient : T.values) @ immutable -> (zeta : (D.index @ immutable total -> ty @ immutable total)) @ total ->
    (equal : ((j : D.index) @ immutable ->
      {u : unit | zeta j === T.prefix ambient xi j})) @ total ->
    (i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
    (schema : template) @ immutable -> (args : T.values) @ immutable ->
    {u : unit | D.lookup (D.weaken_context (T.values_length ambient) g) i === Some sigma && template_lookup ts i === Some schema
      && T.values_length args === D.arity sigma} -> (claim : bool) ->
    (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | interpret rho choices schema === T.meaning zeta sigma args} ->
      {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun g ts rho xi realize ambient zeta equal i sigma schema args premise claim use -> ghost_ (
    let k = T.values_length ambient in
    T.lookup_weaken k g i; match D.lookup g i with
    | None -> ()
    | Some original ->
      D.weaken_scheme_def k original;
      D.arity_def sigma; D.arity_def original;
      let forward : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho choices schema === T.meaning xi original args} ->
        {u : unit | claim}) @ total = fun choices fit ->
        T.meaning_weaken args ambient xi zeta equal original ();
        let () = use choices () in () in
      let () = realize i original schema args () claim forward in ())

let rec (lookup_boundary @ total) : (ts : templates) @ immutable ->
    (i : D.index) @ immutable -> (s : template) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | template_lookup ts i === Some s && boundary_member s p} ->
    {u : unit | environment_boundary ts p} @ ghost = fun ts i s p premise -> ghost_ (
    template_lookup_def ts i; environment_boundary_def ts p;
    match ts with No_templates -> ()
    | Template_binding (_, rest) -> match i with D.Z -> ()
      | D.S i -> lookup_boundary rest i s p (); ())

let (realize_transport @ total) :
    (g : D.context) @ immutable -> (ts : templates) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (xi : (D.index @ immutable total -> ty @ immutable total)) @ total ->
    (realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
    (schema : template) @ immutable -> (args : T.values) @ immutable ->
    {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
      && T.values_length args === D.arity sigma} -> (claim : bool) ->
    (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | interpret rho choices schema === T.meaning xi sigma args} ->
      {u : unit | claim})) @ total -> {u : unit | claim})) @ total ->
    (equal : ((p : node Pref.t) @ immutable ->
      {u : unit | not (environment_boundary ts p) || rho p === tau p})) @ total ->
    (i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
    (schema : template) @ immutable -> (args : T.values) @ immutable ->
    {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
      && T.values_length args === D.arity sigma} -> (claim : bool) ->
    (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | interpret tau choices schema === T.meaning xi sigma args} ->
      {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun g ts rho tau xi realize equal i sigma schema args premise claim use -> ghost_ (
    let boundaries : ((p : node Pref.t) @ immutable ->
        {u : unit | not (boundary_member schema p) || rho p === tau p}) @ total = fun p ->
      if boundary_member schema p then (
        lookup_boundary ts i schema p ();
        equal p; ()) else () in
    let forward : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho choices schema === T.meaning xi sigma args} ->
        {u : unit | claim}) @ total = fun choices fit ->
      interpret_boundary_agreement schema rho tau boundaries choices;
      let () = use choices () in () in
    let () = realize i sigma schema args () claim forward in ())
