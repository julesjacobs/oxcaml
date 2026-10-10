module D = Hm_declarative
module T = Hmc_templates
module I = Hmc_instance
module A = Hmc_ground_arguments
module B = Hmc_specialized_body
module R = Hmc_reference_tree

type t = Empty | Body of B.t * t | Reference of D.index * t
  | Child of t | Pair of t * t | Triple of t * t * t [@@inductive]

let[@def] (root @ total) (expanded : t @ immutable) (instance : I.instance @ immutable) =
  ghost_ (match expanded with Body (body, _) -> body.B.origin === instance | _ -> false)

let[@def] rec (erase @ total) (expanded : t @ immutable) = match expanded with
  | Empty | Body _ -> R.Empty
  | Reference (index, Body (body, _)) -> R.Reference (index, body.B.origin)
  | Reference _ -> R.Empty
  | Child child -> R.Child (erase child)
  | Pair (a, b) -> R.Pair (erase a, erase b)
  | Triple (a, b, c) -> R.Triple (erase a, erase b, erase c)

let[@def] rec (references_only @ total) (expanded : t @ immutable) = match expanded with
  | Empty -> true | Body _ -> false
  | Reference (_, Body _) -> true | Reference _ -> false
  | Child child -> references_only child
  | Pair (a, b) -> references_only a && references_only b
  | Triple (a, b, c) -> references_only a && references_only b && references_only c

let[@def] rec (closed @ total) (expanded : t @ immutable) = ghost_ (match expanded with
  | Empty -> true
  | Body (body, dependencies) ->
    references_only dependencies && closed dependencies
    && R.records body.B.origin.I.earlier D.Empty_context body.B.origin.I.definition.T.source
      body.B.derivation (erase dependencies)
    && R.below body.B.origin.I.earlier (erase dependencies)
  | Reference (_, body) -> (match body with Body _ -> closed body | _ -> false)
  | Child child -> references_only child && closed child
  | Pair (a, b) -> references_only a && references_only b && closed a && closed b
  | Triple (a, b, c) -> references_only a && references_only b && references_only c
    && closed a && closed b && closed c)

let rec (map @ total) : (catalog : T.catalog) @ immutable -> (tree : R.tree) @ immutable ->
    (expand : ((instance : {i : I.instance | D.present (T.rank catalog) i.I.key.A.owner}) @ immutable ->
      {r : t | closed r && root r instance} @ immutable)) @ total ->
    {u : unit | R.below catalog tree} ->
    {r : t | references_only r && closed r && erase r === tree} @ immutable =
  fun catalog tree expand premise ->
    ghost_ (R.below_def catalog tree);
    let out = match tree with
    | R.Empty -> Empty
    | R.Reference (index, instance) ->
      let instantiated = expand (refine_ instance) in
      ghost_ (root_def instantiated instance);
      Reference (index, instantiated)
    | R.Child child -> Child (map catalog child expand ())
    | R.Pair (a, b) -> Pair (map catalog a expand (), map catalog b expand ())
    | R.Triple (a, b, c) -> Triple (map catalog a expand (), map catalog b expand (), map catalog c expand ()) in
    ghost_ (references_only_def out; closed_def out; erase_def out);
    out

let rec (rank_step @ total) : (bound : D.index) @ immutable -> (owner : D.index) @ immutable ->
    (next : D.index) @ immutable ->
    {u : unit | D.present (D.S bound) owner && D.present owner next} ->
    {u : unit | D.present bound next} @ ghost = fun bound owner next premise -> ghost_ (
    D.present_def (D.S bound) owner; D.present_def owner next;
    match owner with D.Z -> () | D.S owner ->
      D.present_def bound owner;
      (match bound with D.Z -> () | D.S bound ->
        D.present_def (D.S bound) next;
        match next with D.Z -> () | D.S next -> rank_step bound owner next ()))

let rec (expand @ total) : (bound : D.index) @ immutable -> (instance : I.instance) @ immutable ->
    {u : unit | D.present bound instance.I.key.A.owner} ->
    {r : t | closed r && root r instance} @ immutable = fun bound instance premise ->
  ghost_ (D.present_def bound instance.I.key.A.owner);
  match bound with
  | D.Z -> unreachable_ ()
  | D.S smaller ->
    let body = B.instantiate instance in
    let requests = R.body body in
    ghost_ (I.valid_def instance);
    let catalog = instance.I.earlier in
    let child : ((next : {i : I.instance | D.present (T.rank catalog) i.I.key.A.owner}) @ immutable ->
      {r : t | closed r && root r next} @ immutable) @ total = fun next ->
        ghost_ (rank_step smaller instance.I.key.A.owner next.I.key.A.owner ());
        expand smaller next () in
    let dependencies = map catalog requests child () in
    let out = Body (body, dependencies) in
    ghost_ (closed_def out; root_def out instance);
    out

type payload = {program : T.program; dependencies : t}
let[@def] (valid @ total) (plan : payload @ immutable) = ghost_ (
  T.ready plan.program && references_only plan.dependencies && closed plan.dependencies
  && R.records plan.program.T.globals D.Empty_context plan.program.T.entry plan.program.T.derivation
    (erase plan.dependencies)
  && R.below plan.program.T.globals (erase plan.dependencies))
type plan = {p : payload | valid p}

let (build @ total) : (program : {p : T.program | T.ready p}) @ immutable ->
    {p : plan | p.program === program} @ immutable = fun program ->
  let requests = R.entry program in
  let catalog = program.T.globals in let bound = T.rank catalog in
  let child : ((next : {i : I.instance | D.present (T.rank catalog) i.I.key.A.owner}) @ immutable ->
    {r : t | closed r && root r next} @ immutable) @ total = fun next -> expand bound next () in
  let dependencies = map catalog requests child () in
  let out = {program; dependencies} in
  ghost_ (valid_def out);
  let out : plan = refine_ out in out
