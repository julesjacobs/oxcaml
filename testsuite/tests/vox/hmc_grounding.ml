module D = Hm_declarative
module G = Hmc_ground_type
module S = Hm_substitution
module E = Hm_elaboration_check
module C = Hm_checked_elaboration

type assignments = Empty | Assign of Copy_spec.node Pref.t * G.t * assignments [@@inductive]

let[@def] rec (lookup @ total) (bindings : assignments @ immutable)
    (p : Copy_spec.node Pref.t @ immutable) = match bindings with
  | Empty -> None
  | Assign (q, ty, rest) -> if Pref.equal p q then Some ty else lookup rest p

let[@def] (resolve @ total) (bindings : assignments @ immutable)
    (p : Copy_spec.node Pref.t @ immutable) = match lookup bindings p with
  | None -> G.Bool | Some ty -> ty

let[@def] rec (collect @ total) (root : D.mono @ immutable)
    (target : G.t @ immutable) (bindings : assignments @ immutable) =
  match root, target with
  | D.Free p, _ -> Some (Assign (p, target, bindings))
  | D.Boolean, G.Bool | D.Word64, G.Word64 -> Some bindings
  | D.List_type a, G.List b -> collect a b bindings
  | D.Function (a, b), G.Arrow (x, y) ->
    (match collect a x bindings with None -> None | Some bindings -> collect b y bindings)
  | _ -> None

let[@def] (entry @ total) (u : unit) = G.Arrow (G.Word64, G.Word64)

let[@def] (word_instance @ total) (ty : D.mono @ immutable) = match ty with
  | D.Word64 | D.Free _ -> true | _ -> false
let[@def] (entry_instance @ total) (ty : D.mono @ immutable) = match ty with
  | D.Free _ -> true | D.Function (a, b) -> word_instance a && word_instance b | _ -> false

let (entry_necessary @ total) :
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (root : D.mono) @ immutable ->
    {u : unit | S.substitute_type rho root === D.Function (D.Word64, D.Word64)} ->
    {u : unit | entry_instance root} @ ghost = fun rho root premise -> ghost_ (
    entry_instance_def root; S.substitute_type_def rho root;
    match root with D.Function (a, b) ->
      word_instance_def a; word_instance_def b;
      S.substitute_type_def rho a; S.substitute_type_def rho b; ()
    | _ -> ())

let (collect_entry @ total) : (root : D.mono) @ immutable ->
    {u : unit | (collect root (entry ()) Empty === None) = not (entry_instance root)} @ ghost =
  fun root -> ghost_ (
    entry_def (); entry_instance_def root; collect_def root (entry ()) Empty;
    match root with
    | D.Function (a, b) ->
      word_instance_def a; word_instance_def b;
      collect_def a G.Word64 Empty;
      (match a with
      | D.Word64 -> collect_def b G.Word64 Empty
      | D.Free p -> collect_def b G.Word64 (Assign (p, G.Word64, Empty))
      | _ -> ())
    | _ -> ())

let (word_assignment @ total) : (bindings : assignments) @ immutable ->
    (p : Copy_spec.node Pref.t) @ immutable ->
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (agrees : ((q : Copy_spec.node Pref.t) @ immutable ->
      {u : unit | rho q === G.copy (resolve bindings q)})) @ total ->
    {u : unit | lookup bindings p === Some G.Word64} ->
    {u : unit | S.substitute_type rho (D.Free p) === D.Word64} @ ghost =
  fun bindings p rho agrees premise -> ghost_ (
    agrees p; resolve_def bindings p; G.copy_def G.Word64;
    S.substitute_type_def rho (D.Free p); D.embed_def Copy_spec.Word64; ())

let (entry_realized @ total) : (root : D.mono) @ immutable -> (bindings : assignments) @ immutable ->
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (agrees : ((q : Copy_spec.node Pref.t) @ immutable ->
      {u : unit | rho q === G.copy (resolve bindings q)})) @ total ->
    {u : unit | collect root (entry ()) Empty === Some bindings} ->
    {u : unit | S.substitute_type rho root === D.Function (D.Word64, D.Word64)} @ ghost =
  fun root bindings rho agrees premise -> ghost_ (
    entry_def (); collect_def root (entry ()) Empty; S.substitute_type_def rho root;
    match root with
    | D.Free p ->
      let _ = Pref.equal p p in lookup_def bindings p; resolve_def bindings p; agrees p;
      G.copy_def (entry ()); G.copy_def G.Word64;
      D.embed_def (G.copy (entry ())); D.embed_def Copy_spec.Word64; ()
    | D.Function (a, b) ->
      collect_def a G.Word64 Empty;
      S.substitute_type_def rho a; S.substitute_type_def rho b;
      (match a with
      | D.Word64 ->
        collect_def b G.Word64 Empty;
        (match b with D.Free p -> let _ = Pref.equal p p in lookup_def bindings p; word_assignment bindings p rho agrees ()
        | _ -> ())
      | D.Free p ->
        let middle = Assign (p, G.Word64, Empty) in
        collect_def b G.Word64 middle;
        (match b with
        | D.Word64 -> let _ = Pref.equal p p in lookup_def bindings p; word_assignment bindings p rho agrees ()
        | D.Free q ->
          let _ = Pref.equal p q in let _ = Pref.equal p p in let _ = Pref.equal q q in
          lookup_def bindings p; lookup_def middle p;
          lookup_def bindings q;
          word_assignment bindings p rho agrees (); word_assignment bindings q rho agrees ()
        | _ -> ())
      | _ -> ())
    | _ -> ())

type payload = {term : D.term; proof : D.typing; assignments : assignments}
type grounded = {p : payload | D.typed D.Z D.Empty_context p.term
  (D.Function (D.Word64, D.Word64)) p.proof && Hmc_no_free.typing p.proof}
type result = Entry_type_mismatch | Grounded of grounded [@@inductive]

let (ground @ total) : (checked : C.t) @ immutable ->
    {r : result | match r with Entry_type_mismatch -> not (entry_instance (C.root checked))
      | Grounded p -> entry_instance (C.root checked) && p.term === C.source checked} @ immutable = fun checked ->
  let term = C.source checked in let root = C.root checked in
  let proof = C.derivation checked in
  let target = entry () in
  ghost_ (collect_entry root);
  match collect root target Empty with
  | None -> Entry_type_mismatch
  | Some assignments ->
    let[@def] rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total) =
      fun p -> G.copy (resolve assignments p) in
    ghost_ (let agrees : ((q : Copy_spec.node Pref.t) @ immutable ->
      {u : unit | rho q === G.copy (resolve assignments q)}) @ total = fun q -> rho_def q; () in
      entry_realized root assignments rho agrees ());
    let ty = S.substitute_type rho root in
    let target_type = G.mono target in
    ghost_ (entry_def (); G.mono_def target; G.mono_def G.Word64);
    if E.mono_equal ty target_type then (
      ghost_ (let closed : ((p : Copy_spec.node Pref.t) @ immutable ->
        {u : unit | Hmc_no_free.mono (D.embed (rho p))}) @ total = fun p ->
          rho_def p; Hmc_no_free.ground_mono (resolve assignments p); () in
        Hmc_no_free.substitute_typing rho closed proof);
      let proof = S.substitute_typing rho proof in
      ghost_ (Hm_substitution_proofs.substitution_typed rho D.Z D.Empty_context term root (C.derivation checked) ();
        S.substitute_context_def rho D.Empty_context;
        entry_def (); G.mono_def target; G.mono_def G.Word64);
      let out : grounded = {term; proof; assignments} in Grounded out)
    else Entry_type_mismatch
