module Ty = Copy_spec
module H = Pref.Heap
module U = Level_unifier_spec
module F = Level_finite_spec
module R = Representative_level
module Pool = Generalize_spec
module Env = Hm_environment_spec
module Run = Hm_effective_execution_spec
module Fresh = Hm_elaboration_freshness
module Generic = Hm_effective_generic
module Forest = Hm_effective_forest
module Close = Representative_pool_spec

let (let_suffix @ total) :
    (heap : Ty.node Pref.heap) @ immutable ->
    (trees : ((p : Ty.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem heap p then F.finite heap t else U.observe heap p === None)} @ immutable)) @ total ->
    (depth : int) -> (child_pool : Pool.pool) @ immutable ->
    (body_pool : Pool.pool) @ immutable -> (body_env : Env.env) @ immutable ->
    (body : Run.execution) @ immutable -> (after : Ty.node Pref.heap) @ immutable ->
    (final_pool : Pool.pool) @ immutable -> (final : Ty.node Pref.heap) @ immutable ->
    (future : ((protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
      (closed : ((p : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at after protected p})) @ total ->
      (generic : ((p : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected p) || Fresh.generic_at after p r} @ immutable)) @ total ->
      (p : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at final protected p})) @ total ->
    (protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
    (closed : ((p : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at heap protected p})) @ total ->
    (generic : ((p : Ty.node Pref.t) @ immutable ->
      {r : R.representative | not (protected p) || Fresh.generic_at heap p r} @ immutable)) @ total ->
    (p : Ty.node Pref.t) @ immutable ->
    {u : unit | Pool.pool_scoped heap child_pool &&
      Run.ran (Close.close_heap heap depth child_pool) depth body_pool body_env body after final_pool} ->
    {u : unit | Fresh.closed_at final protected p} @ ghost =
  fun heap trees depth child_pool body_pool body_env body after final_pool final future protected closed generic p premise -> ghost_ (
    let start = Close.close_heap heap depth child_pool in
    let start_trees : ((q : Ty.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === q &&
        (if H.mem start q then F.finite start t else U.observe start q === None)} @ immutable) @ total = fun q ->
      Forest.representative_closed_forest heap trees depth child_pool q () in
    let start_closed : ((q : Ty.node Pref.t) @ immutable ->
      {u : unit | Fresh.closed_at start protected q}) @ total = fun q ->
      closed q; Fresh.close heap protected depth child_pool q () in
    let start_generic : ((q : Ty.node Pref.t) @ immutable ->
      {r : R.representative | not (protected q) || Fresh.generic_at start q r} @ immutable) @ total = fun q ->
      let r = generic q in
      if protected q then Fresh.generic_close heap depth child_pool q r ();
      r in
    let after_closed : ((q : Ty.node Pref.t) @ immutable ->
      {u : unit | Fresh.closed_at after protected q}) @ total = fun q ->
      Fresh.run start start_trees depth body_pool body_env body after final_pool protected start_closed start_generic q () in
    let after_generic : ((q : Ty.node Pref.t) @ immutable ->
      {r : R.representative | not (protected q) || Fresh.generic_at after q r} @ immutable) @ total = fun q ->
      let r = start_generic q in
      if protected q then Fresh.generic_run start depth body_pool body_env body after final_pool q r () else r in
    future protected after_closed after_generic p)

let (let_generic_suffix @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (depth : int) -> (child_pool : Pool.pool) @ immutable ->
    (body_pool : Pool.pool) @ immutable -> (body_env : Env.env) @ immutable ->
    (body : Run.execution) @ immutable -> (after : Ty.node Pref.heap) @ immutable ->
    (final_pool : Pool.pool) @ immutable -> (final : Ty.node Pref.heap) @ immutable ->
    (future : ((p : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
      (desc : Ty.desc option) @ immutable -> {u : unit | Generic.generic_path after p r desc} ->
      {s : R.representative | Generic.generic_path final p s desc} @ immutable)) @ total ->
    (p : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
    (desc : Ty.desc option) @ immutable ->
    {u : unit | Pool.pool_scoped heap child_pool && Generic.generic_path heap p r desc &&
      Run.ran (Close.close_heap heap depth child_pool) depth body_pool body_env body after final_pool} ->
    {s : R.representative | Generic.generic_path final p s desc} @ immutable ghost =
  fun heap depth child_pool body_pool body_env body after final_pool final future p r desc premise -> ghost_ (
    let start = Close.close_heap heap depth child_pool in
    Generic.close heap depth child_pool p r desc ();
    let next = Generic.run start depth body_pool body_env body after final_pool p r desc () in
    future p next desc ())

let (generalized_let_scope @ total) : (heap : Ty.node Pref.heap) @ immutable -> (heads : Effective_level.heads) @ total ->
    (safe : ((p : Ty.node Pref.t) @ immutable -> {u : unit | Hm_effective_runtime.safe heap heads p})) @ total ->
    (after : Ty.node Pref.heap) @ immutable ->
    (owned : ((p : Ty.node Pref.t) @ immutable -> {u : unit | not (H.mem heap p) || H.mem after p})) @ total ->
    (initial_trees : ((p : Ty.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem heap p then F.finite heap t else U.observe heap p === None)} @ immutable)) @ total ->
    (depth : int) -> (child_pool : Pool.pool) @ immutable ->
    (body_pool : Pool.pool) @ immutable -> (body_env : Env.env) @ immutable ->
    (body : Run.execution) @ immutable -> (middle : Ty.node Pref.heap) @ immutable ->
    (final_pool : Pool.pool) @ immutable ->
    (future : ((protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
      (closed : ((p : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at middle protected p})) @ total ->
      (generic : ((p : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected p) || Fresh.generic_at middle p r} @ immutable)) @ total ->
      (p : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at after protected p})) @ total ->
    (trees : ((p : Ty.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem after p then F.finite after t else U.observe after p === None)} @ immutable)) @ total ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (values : ((p : Ty.node Pref.t) @ immutable -> {u : unit | rho p === F.readback (trees p)})) @ total ->
    (scope : Hm_elaboration.scope) @ immutable -> (context : Hm_declarative.context) @ immutable ->
    (root : Ty.node Pref.t) @ immutable -> (finite : R.representative) @ immutable ->
    (p : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
    {u : unit | Pool.pool_scoped heap child_pool &&
      Run.ran (Close.close_heap heap depth child_pool) depth body_pool body_env body middle final_pool &&
      Hm_effective_result.finite_path heap root finite && Fresh.generic_at heap p r
      && Hm_elaboration.parameter p scope === None} ->
    {u : unit | Hm_elaboration.parameter p (Hm_elaboration.Quantifiers
      ((Hm_generalization.generalize context (Hm_elaboration.interpret scope (rho root))).Hm_generalization.variables, scope)) === None} @ ghost =
  fun heap heads safe after owned initial_trees depth child_pool body_pool body_env body middle final_pool future trees rho values scope context root finite p r premise -> ghost_ (
    let remaining : ((protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
      (closed : ((q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at heap protected q})) @ total ->
      (generic : ((q : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected q) || Fresh.generic_at heap q r} @ immutable)) @ total ->
      (q : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at after protected q}) @ total =
      fun protected closed generic q ->
        let_suffix heap initial_trees depth child_pool body_pool body_env body middle final_pool after
          future protected closed generic q () in
    Fresh.generalized_scope_at_result heap heads safe after owned remaining trees rho values scope context root finite p r ())

let (scope_before @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (after : Ty.node Pref.heap) @ immutable -> (scope : Hm_elaboration.scope) @ immutable ->
    (preserve : ((p : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
      (desc : Ty.desc option) @ immutable -> {u : unit | Generic.generic_path heap p r desc} ->
      {s : R.representative | Generic.generic_path after p s desc} @ immutable)) @ total ->
    (fresh : ((p : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
      {u : unit | Fresh.generic_at after p r} -> {u : unit | Hm_elaboration.parameter p scope === None})) @ total ->
    (p : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
    {u : unit | Fresh.generic_at heap p r} ->
    {u : unit | Hm_elaboration.parameter p scope === None} @ ghost =
  fun heap after scope preserve fresh p r premise -> ghost_ (
    Fresh.generic_at_def heap p r;
    Generic.generic_path_def heap p r (U.observe heap p);
    let next = preserve p r (U.observe heap p) () in
    Generic.generic_path_def after p next (U.observe heap p);
    Fresh.generic_at_def after p next;
    fresh p next ())
