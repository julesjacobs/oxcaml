module D = Hm_declarative
module T = Hmc_templates
module I = Hmc_instance
module A = Hmc_ground_arguments
module G = Hmc_ground_type
module B = Hmc_specialized_body
module R = Hmc_reference_tree
module E = Hmc_expansion

let[@def] rec (owner @ total) (catalog : T.catalog @ immutable) (rank : D.index @ immutable) =
  ghost_ (match catalog with
  | T.Empty -> None
  | T.Declare (definition, earlier) ->
    if Hm_elaboration_check.index_equal rank (T.rank earlier)
    then Some {T.definition; earlier} else owner earlier rank)

let[@def] (anchored @ total) (catalog : T.catalog @ immutable) (instance : I.instance @ immutable) =
  ghost_ (owner catalog instance.I.key.A.owner ===
    Some {T.definition = instance.I.definition; earlier = instance.I.earlier})

let rec (irreflexive @ total) : (n : D.index) @ immutable ->
    {u : unit | not (D.present n n)} @ ghost = fun n -> ghost_ (
    D.present_def n n; match n with D.Z -> () | D.S n -> irreflexive n)

let rec (selection_owner @ total) : (catalog : T.catalog) @ immutable ->
    (index : D.index) @ immutable -> (selected : T.selected) @ immutable ->
    {u : unit | T.valid catalog && T.selection catalog index === Some selected} ->
    {u : unit | owner catalog (T.rank selected.T.earlier) === Some selected} @ ghost =
  fun catalog index selected premise -> ghost_ (
    T.selection_def catalog index; owner_def catalog (T.rank selected.T.earlier);
    T.valid_def catalog;
    match catalog with T.Empty -> () | T.Declare (_, earlier) ->
      match index with D.Z ->
        let _ = Hm_elaboration_check.index_equal (T.rank earlier) (T.rank earlier) in ()
      | D.S index ->
        let _ = T.select earlier index () in
        irreflexive (T.rank earlier);
        let _ = Hm_elaboration_check.index_equal (T.rank selected.T.earlier) (T.rank earlier) in
        selection_owner earlier index selected ())

let rec (mono_injective @ total) : (a : G.t) @ immutable -> (b : G.t) @ immutable ->
    {u : unit | G.mono a === G.mono b} -> {u : unit | a === b} @ ghost =
  fun a b premise -> ghost_ (
    G.mono_def a; G.mono_def b;
    match a, b with
    | G.List a, G.List b -> mono_injective a b ()
    | G.Arrow (a, b), G.Arrow (x, y) -> mono_injective a x (); mono_injective b y ()
    | _ -> ())

let (same_instance @ total) : (catalog : T.catalog) @ immutable ->
    (a : I.instance) @ immutable -> (b : I.instance) @ immutable ->
    {u : unit | anchored catalog a && anchored catalog b && a.I.key === b.I.key} ->
    {u : unit | a === b} @ ghost = fun catalog a b premise -> ghost_ (
    anchored_def catalog a; anchored_def catalog b;
    I.valid_def a; I.valid_def b; mono_injective a.I.ty b.I.ty ())

let (same_body @ total) : (catalog : T.catalog) @ immutable ->
    (a : B.t) @ immutable -> (b : B.t) @ immutable ->
    {u : unit | anchored catalog a.B.origin && anchored catalog b.B.origin
      && a.B.origin.I.key === b.B.origin.I.key} ->
    {u : unit | a === b} @ ghost = fun catalog a b premise -> ghost_ (
    same_instance catalog a.B.origin b.B.origin ();
    B.valid_def a; B.valid_def b)

let[@def] rec (suffix @ total) (whole : T.catalog @ immutable) (part : T.catalog @ immutable) =
  ghost_ (whole === part || match whole with
    | T.Empty -> false | T.Declare (_, earlier) -> suffix earlier part)

let rec (owner_below @ total) : (catalog : T.catalog) @ immutable ->
    (rank : D.index) @ immutable -> (selected : T.selected) @ immutable ->
    {u : unit | owner catalog rank === Some selected} ->
    {u : unit | D.present (T.rank catalog) rank && suffix catalog selected.T.earlier} @ ghost =
  fun catalog rank selected premise -> ghost_ (
    owner_def catalog rank; T.rank_def catalog; suffix_def catalog selected.T.earlier;
    match catalog with T.Empty -> () | T.Declare (_, earlier) ->
      if Hm_elaboration_check.index_equal rank (T.rank earlier) then (
        T.rank_self rank; suffix_def earlier earlier)
      else (owner_below earlier rank selected (); T.rank_weaken (T.rank earlier) rank ()))

let rec (owner_lift @ total) : (whole : T.catalog) @ immutable ->
    (part : T.catalog) @ immutable -> (rank : D.index) @ immutable ->
    (selected : T.selected) @ immutable ->
    {u : unit | suffix whole part && owner part rank === Some selected} ->
    {u : unit | owner whole rank === Some selected} @ ghost =
  fun whole part rank selected premise -> ghost_ (
    suffix_def whole part;
    if whole === part then () else match whole with
    | T.Empty -> ()
    | T.Declare (_, earlier) ->
      owner_lift earlier part rank selected ();
      owner_below earlier rank selected (); irreflexive (T.rank earlier);
      owner_def whole rank;
      let _ = Hm_elaboration_check.index_equal rank (T.rank earlier) in ())

let[@def] rec (references @ total) (catalog : T.catalog @ immutable) (tree : R.tree @ immutable) =
  ghost_ (match tree with
    | R.Empty -> true | R.Reference (_, instance) -> anchored catalog instance
    | R.Child a -> references catalog a
    | R.Pair (a, b) -> references catalog a && references catalog b
    | R.Triple (a, b, c) -> references catalog a && references catalog b && references catalog c)

let rec (records @ total) : (whole : T.catalog) @ immutable -> (catalog : T.catalog) @ immutable ->
    (locals : D.context) @ immutable -> (term : D.term) @ immutable ->
    (d : D.typing) @ immutable -> (tree : R.tree) @ immutable ->
    {u : unit | T.valid catalog && suffix whole catalog && R.records catalog locals term d tree} ->
    {u : unit | references whole tree} @ ghost = fun whole catalog locals term d tree premise -> ghost_ (
      R.records_def catalog locals term d tree; references_def whole tree;
      match term, d, tree with
      | D.Bound _, D.Variable _, R.Reference (index, instance) ->
        let selected = {T.definition = instance.I.definition; earlier = instance.I.earlier} in
        selection_owner catalog index selected ();
        I.valid_def instance;
        owner_lift whole catalog instance.I.key.A.owner selected ();
        anchored_def whole instance
      | D.Lambda body, D.Abstraction (a, db), R.Child child ->
        records whole catalog (D.Binding (D.Forall (D.Z, a), locals)) body db child ()
      | D.Recursive body, D.Recursion (a, b, db), R.Child child ->
        records whole catalog (D.Binding (D.Forall (D.Z, a),
          D.Binding (D.Forall (D.Z, D.Function (a, b)), locals))) body db child ()
      | D.Apply (a, b), D.Application (_, da, db), R.Pair (ra, rb)
      | D.Cons (a, b), D.List_cons (_, da, db), R.Pair (ra, rb)
      | D.Primitive (_, a, b), D.Word_primitive (da, db), R.Pair (ra, rb) ->
        records whole catalog locals a da ra (); records whole catalog locals b db rb ()
      | D.If (a, b, c), D.Conditional (da, db, dc), R.Triple (ra, rb, rc) ->
        records whole catalog locals a da ra (); records whole catalog locals b db rb ();
        records whole catalog locals c dc rc ()
      | D.CaseList (s, a, b), D.List_case (element, ds, da, db), R.Triple (rs, ra, rb) ->
        records whole catalog locals s ds rs (); records whole catalog locals a da ra ();
        records whole catalog (D.Binding (D.Forall (D.Z, element),
          D.Binding (D.Forall (D.Z, D.List_type element), locals))) b db rb ()
      | D.Let (a, b), D.Let_binding (D.Forall (D.Z, ty), da, db), R.Pair (ra, rb) ->
        records whole catalog locals a da ra ();
        records whole catalog (D.Binding (D.Forall (D.Z, ty), locals)) b db rb ()
      | _ -> ())

let[@def] rec (bodies @ total) (catalog : T.catalog @ immutable) (tree : E.t @ immutable) =
  ghost_ (match tree with
    | E.Empty -> true
    | E.Body (body, dependencies) -> anchored catalog body.B.origin && bodies catalog dependencies
    | E.Reference (_, a) | E.Child a -> bodies catalog a
    | E.Pair (a, b) -> bodies catalog a && bodies catalog b
    | E.Triple (a, b, c) -> bodies catalog a && bodies catalog b && bodies catalog c)

let rec (expansion @ total) : (catalog : T.catalog) @ immutable -> (tree : E.t) @ immutable ->
    {u : unit | E.closed tree && E.references_only tree && references catalog (E.erase tree)} ->
    {u : unit | bodies catalog tree} @ ghost = fun catalog tree premise -> ghost_ (
      E.closed_def tree; E.references_only_def tree; E.erase_def tree;
      references_def catalog (E.erase tree); bodies_def catalog tree;
      match tree with
      | E.Empty | E.Body _ -> ()
      | E.Reference (_, expanded) -> (match expanded with
        | E.Body (body, dependencies) ->
          E.closed_def expanded; bodies_def catalog expanded;
          let origin = body.B.origin in
          anchored_def catalog origin; I.valid_def origin;
          owner_below catalog origin.I.key.A.owner
            {T.definition = origin.I.definition; earlier = origin.I.earlier} ();
          records catalog origin.I.earlier D.Empty_context origin.I.definition.T.source
            body.B.derivation (E.erase dependencies) ();
          expansion catalog dependencies ()
        | _ -> ())
      | E.Child a -> expansion catalog a ()
      | E.Pair (a, b) -> expansion catalog a (); expansion catalog b ()
      | E.Triple (a, b, c) -> expansion catalog a (); expansion catalog b (); expansion catalog c ())

let (plan @ total) : (plan : E.plan) @ immutable ->
    {u : unit | bodies plan.E.program.T.globals plan.E.dependencies} @ ghost = fun plan -> ghost_ (
      E.valid_def plan; T.ready_def plan.E.program;
      suffix_def plan.E.program.T.globals plan.E.program.T.globals;
      records plan.E.program.T.globals plan.E.program.T.globals D.Empty_context
        plan.E.program.T.entry plan.E.program.T.derivation (E.erase plan.E.dependencies) ();
      expansion plan.E.program.T.globals plan.E.dependencies ())

type body_list = Nil | Add of B.t * body_list [@@inductive]

let[@def] rec (collected @ total) (tree : E.t @ immutable) (initial : body_list @ immutable) =
  ghost_ (match tree with
    | E.Empty -> initial
    | E.Body (body, dependencies) -> Add (body, collected dependencies initial)
    | E.Reference (_, a) | E.Child a -> collected a initial
    | E.Pair (a, b) -> collected b (collected a initial)
    | E.Triple (a, b, c) -> collected c (collected b (collected a initial)))

let[@def] rec (all @ total) (catalog : T.catalog @ immutable) (list : body_list @ immutable) =
  ghost_ (match list with Nil -> true | Add (body, rest) -> anchored catalog body.B.origin && all catalog rest)

let rec (collect @ total) : (catalog : T.catalog) @ immutable -> (tree : E.t) @ immutable ->
    (initial : body_list) @ immutable -> {u : unit | bodies catalog tree && all catalog initial} ->
    {u : unit | all catalog (collected tree initial)} @ ghost = fun catalog tree initial premise -> ghost_ (
      bodies_def catalog tree; collected_def tree initial;
      match tree with
      | E.Empty -> ()
      | E.Body (body, dependencies) ->
        collect catalog dependencies initial (); all_def catalog (collected tree initial)
      | E.Reference (_, a) | E.Child a -> collect catalog a initial ()
      | E.Pair (a, b) -> collect catalog a initial (); collect catalog b (collected a initial) ()
      | E.Triple (a, b, c) -> collect catalog a initial (); collect catalog b (collected a initial) ();
        collect catalog c (collected b (collected a initial)) ())
