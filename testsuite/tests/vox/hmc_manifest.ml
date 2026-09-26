module D = Hm_declarative
module B = Hmc_specialized_body
module E = Hmc_expansion
module R = Hmc_reference_tree
module C = Hmc_specialization_coherence

type references = Empty | Reference of D.index * D.index
  | Child of references | Pair of references * references
  | Triple of references * references * references [@@inductive]
type entry = {body : B.t; dependencies : references}
type table = Nil | Add of entry * table [@@inductive]

let[@def] rec (bodies @ total) (table : table @ immutable) = ghost_ (match table with
  | Nil -> C.Nil | Add (entry, rest) -> C.Add (entry.body, bodies rest))

let[@def] rec (size @ total) (table : table @ immutable) = match table with
  | Nil -> D.Z | Add (_, rest) -> D.S (size rest)
let[@def] rec (lookup @ total) (table : table @ immutable) (id : D.index @ immutable) =
  match table with Nil -> None | Add (entry, rest) ->
    if Hm_elaboration_check.index_equal id (size rest) then Some entry else lookup rest id
let[@def] rec (resolve @ total) (table : table @ immutable) (refs : references @ immutable) =
  match refs with
  | Empty -> R.Empty
  | Reference (index, id) -> (match lookup table id with
    | None -> R.Empty | Some entry -> R.Reference (index, entry.body.B.origin))
  | Child child -> R.Child (resolve table child)
  | Pair (a, b) -> R.Pair (resolve table a, resolve table b)
  | Triple (a, b, c) -> R.Triple (resolve table a, resolve table b, resolve table c)
let[@def] rec (bounded @ total) (bound : D.index @ immutable) (refs : references @ immutable) =
  match refs with
  | Empty -> true | Reference (_, id) -> D.present bound id
  | Child child -> bounded bound child
  | Pair (a, b) -> bounded bound a && bounded bound b
  | Triple (a, b, c) -> bounded bound a && bounded bound b && bounded bound c
let[@def] rec (extends @ total) (larger : table @ immutable) (smaller : table @ immutable) =
  ghost_ (larger === smaller || match larger with Nil -> false | Add (_, rest) -> extends rest smaller)
let[@def] rec (valid @ total) (table : table @ immutable) = ghost_ (match table with
  | Nil -> true
  | Add (entry, rest) -> valid rest && bounded (size rest) entry.dependencies
    && R.records entry.body.B.origin.Hmc_instance.earlier D.Empty_context
      entry.body.B.origin.Hmc_instance.definition.Hmc_templates.source
      entry.body.B.derivation (resolve rest entry.dependencies))

let rec (irreflexive @ total) : (n : D.index) @ immutable ->
    {u : unit | not (D.present n n)} @ ghost = fun n -> ghost_ (
  D.present_def n n; match n with D.Z -> () | D.S n -> irreflexive n)
let rec (weaken @ total) : (bound : D.index) @ immutable -> (id : D.index) @ immutable ->
    {u : unit | D.present bound id} -> {u : unit | D.present (D.S bound) id} @ ghost =
  fun bound id premise -> ghost_ (
    D.present_def bound id; D.present_def (D.S bound) id;
    match bound, id with D.S n, D.S i -> weaken n i () | _ -> ())
let rec (self_present @ total) : (n : D.index) @ immutable ->
    {u : unit | D.present (D.S n) n} @ ghost = fun n -> ghost_ (
  D.present_def (D.S n) n; match n with D.Z -> () | D.S n -> self_present n)
let rec (last_or_before @ total) : (n : D.index) @ immutable -> (i : D.index) @ immutable ->
    {u : unit | D.present (D.S n) i} ->
    {u : unit | i === n || D.present n i} @ ghost = fun n i premise -> ghost_ (
  D.present_def (D.S n) i; D.present_def n i;
  match n, i with D.S n, D.S i -> last_or_before n i ()
  | D.Z, D.S j -> D.present_def D.Z j | _ -> ())
let rec (lookup_present @ total) : (table : table) @ immutable -> (id : D.index) @ immutable ->
    {u : unit | D.present (size table) id} ->
    {u : unit | not (lookup table id === None)} @ ghost = fun table id premise -> ghost_ (
  size_def table; lookup_def table id;
  match table with
  | Nil -> D.present_def D.Z id
  | Add (_, rest) -> last_or_before (size rest) id ();
    if Hm_elaboration_check.index_equal id (size rest) then () else lookup_present rest id ())
let rec (preserve @ total) : (larger : table) @ immutable -> (smaller : table) @ immutable ->
    (refs : references) @ immutable ->
    {u : unit | extends larger smaller && bounded (size smaller) refs} ->
    {u : unit | bounded (size larger) refs && resolve larger refs === resolve smaller refs} @ ghost =
  fun larger smaller refs premise -> ghost_ (
    extends_def larger smaller;
    if larger === smaller then () else match larger with
    | Nil -> ()
    | Add (entry, rest) ->
      preserve rest smaller refs ();
      let rec (step @ total) : (refs : references) @ immutable ->
          {u : unit | bounded (size rest) refs} ->
          {u : unit | bounded (size larger) refs && resolve larger refs === resolve rest refs} @ ghost =
        fun refs premise -> ghost_ (
          bounded_def (size rest) refs; bounded_def (size larger) refs;
          resolve_def larger refs; resolve_def rest refs; size_def larger;
          match refs with
          | Empty -> ()
          | Reference (_, id) -> irreflexive (size rest); weaken (size rest) id (); lookup_def larger id;
            let _ = Hm_elaboration_check.index_equal id (size rest) in ()
          | Child a -> step a ()
          | Pair (a, b) -> step a (); step b ()
          | Triple (a, b, c) -> step a (); step b (); step c ()) in
      step refs ())
let rec (transitive @ total) : (a : table) @ immutable -> (b : table) @ immutable ->
    (c : table) @ immutable -> {u : unit | extends a b && extends b c} ->
    {u : unit | extends a c} @ ghost = fun a b c premise -> ghost_ (
  extends_def a b; extends_def a c;
  if a === b then () else match a with Nil -> () | Add (_, rest) -> transitive rest b c ())

type result = {table : table; references : references}
let rec (flatten @ total) : (tree : E.t) @ immutable -> (initial : table) @ immutable ->
    {u : unit | E.closed tree && E.references_only tree && valid initial} ->
    {r : result | valid r.table && extends r.table initial
      && bounded (size r.table) r.references && resolve r.table r.references === E.erase tree
      && bodies r.table === C.collected tree (bodies initial)} @ immutable =
  fun tree initial premise ->
    ghost_ (E.closed_def tree; E.references_only_def tree; E.erase_def tree;
      C.collected_def tree (bodies initial));
    match tree with
    | E.Body _ -> unreachable_ ()
    | E.Empty ->
      ghost_ (extends_def initial initial; bounded_def (size initial) Empty; resolve_def initial Empty);
      {table = initial; references = Empty}
    | E.Reference (index, expanded) -> (match expanded with
      | E.Body (body, dependencies) ->
        ghost_ (E.closed_def expanded; C.collected_def expanded (bodies initial));
        let children = flatten dependencies initial () in
        let id = size children.table in
        let entry = {body; dependencies = children.references} in
        let table = Add (entry, children.table) in
        let references = Reference (index, id) in
        ghost_ (valid_def table; extends_def table initial; size_def table; bodies_def table;
          bounded_def (size table) references; resolve_def table references; lookup_def table id;
          let _ = Hm_elaboration_check.index_equal id id in self_present id);
        {table; references}
      | _ -> unreachable_ ())
    | E.Child a ->
      let a = flatten a initial () in let references = Child a.references in
      ghost_ (bounded_def (size a.table) references; resolve_def a.table references);
      {table = a.table; references}
    | E.Pair (a, b) ->
      let a = flatten a initial () in let b = flatten b a.table () in
      let references = Pair (a.references, b.references) in
      ghost_ (preserve b.table a.table a.references (); transitive b.table a.table initial ();
        bounded_def (size b.table) references; resolve_def b.table references);
      {table = b.table; references}
    | E.Triple (a, b, c) ->
      let a = flatten a initial () in let b = flatten b a.table () in let c = flatten c b.table () in
      let references = Triple (a.references, b.references, c.references) in
      ghost_ (transitive c.table b.table a.table (); transitive c.table a.table initial ();
        preserve c.table a.table a.references (); preserve c.table b.table b.references ();
        bounded_def (size c.table) references; resolve_def c.table references);
      {table = c.table; references}

type payload = {program : Hmc_templates.program; definitions : table; entry : references}
let[@def] (ready @ total) (p : payload @ immutable) = ghost_ (
  Hmc_templates.ready p.program && valid p.definitions && bounded (size p.definitions) p.entry
  && C.all p.program.Hmc_templates.globals (bodies p.definitions)
  && R.records p.program.Hmc_templates.globals D.Empty_context p.program.Hmc_templates.entry
    p.program.Hmc_templates.derivation (resolve p.definitions p.entry))
type program = {p : payload | ready p}
let (build @ total) : (plan : E.plan) @ immutable ->
    {r : program | r.program === plan.E.program} @ immutable = fun plan ->
  ghost_ (E.valid_def plan; valid_def Nil);
  let flat = flatten plan.E.dependencies Nil () in
  let out = {program = plan.E.program; definitions = flat.table; entry = flat.references} in
  ghost_ (C.plan plan; bodies_def Nil; C.all_def plan.E.program.Hmc_templates.globals C.Nil;
    C.collect plan.E.program.Hmc_templates.globals plan.E.dependencies C.Nil (); ready_def out);
  let out : program = refine_ out in out

let (lookup_last @ total) : (entry : entry) @ immutable -> (rest : table) @ immutable ->
    {u : unit | lookup (Add (entry, rest)) (size rest) === Some entry} @ ghost = fun entry rest -> ghost_ (
  lookup_def (Add (entry, rest)) (size rest);
  let _ = Hm_elaboration_check.index_equal (size rest) (size rest) in ())

let rec (lookup_closed @ total) : (table : table) @ immutable -> (id : D.index) @ immutable ->
    (entry : entry) @ immutable -> {u : unit | valid table && lookup table id === Some entry} ->
    {u : unit | bounded (size table) entry.dependencies
      && R.records entry.body.B.origin.Hmc_instance.earlier D.Empty_context
        entry.body.B.origin.Hmc_instance.definition.Hmc_templates.source entry.body.B.derivation
        (resolve table entry.dependencies)} @ ghost = fun table id entry premise -> ghost_ (
  valid_def table; lookup_def table id;
  match table with Nil -> () | Add (last, rest) ->
    if Hm_elaboration_check.index_equal id (size rest) then (
      extends_def rest rest; extends_def table rest;
      preserve table rest entry.dependencies ()) else (
      lookup_closed rest id entry ();
      extends_def rest rest; extends_def table rest;
      preserve table rest entry.dependencies ()))

let rec (lookup_anchored @ total) : (catalog : Hmc_templates.catalog) @ immutable ->
    (table : table) @ immutable -> (id : D.index) @ immutable -> (entry : entry) @ immutable ->
    {u : unit | C.all catalog (bodies table) && lookup table id === Some entry} ->
    {u : unit | C.anchored catalog entry.body.B.origin} @ ghost = fun catalog table id entry premise -> ghost_ (
      bodies_def table; C.all_def catalog (bodies table); lookup_def table id;
      match table with Nil -> () | Add (_, rest) ->
        if Hm_elaboration_check.index_equal id (size rest) then () else lookup_anchored catalog rest id entry ())

let (duplicate_key @ total) : (program : program) @ immutable ->
    (i : D.index) @ immutable -> (j : D.index) @ immutable ->
    (a : entry) @ immutable -> (b : entry) @ immutable ->
    {u : unit | lookup program.definitions i === Some a && lookup program.definitions j === Some b
      && a.body.B.origin.Hmc_instance.key === b.body.B.origin.Hmc_instance.key} ->
    {u : unit | a.body === b.body} @ ghost = fun program i j a b premise -> ghost_ (
      ready_def program;
      lookup_anchored program.program.Hmc_templates.globals program.definitions i a ();
      lookup_anchored program.program.Hmc_templates.globals program.definitions j b ();
      C.same_body program.program.Hmc_templates.globals a.body b.body ())
