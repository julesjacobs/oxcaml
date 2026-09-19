open Copy_spec
open Level_spec
open Generalize_spec
open Hm_environment_spec
open Hm_runtime_spec
open Hm_effective_execution_spec

let rec (env_lookup_owned @ total) : (h : Pref.heap) @ immutable ->
    (env : Hm_environment_spec.env) @ immutable -> (i : Hm_declarative.index) @ immutable ->
    {u : unit | env_owned h env && Hm_declarative.present (env_depth env) i} ->
    {u : unit | match Hm_environment_spec.lookup env i with None -> false | Some p -> H.mem h p} @ ghost =
  fun h env i premise -> ghost_ (
    env_owned_def h env; env_depth_def env;
    let n = env_depth env in Hm_declarative.present_def n i; Hm_environment_spec.lookup_def env i;
    match env with Hm_environment_spec.Empty -> ()
    | Hm_environment_spec.Bind (_, rest) -> match i with Hm_declarative.Z -> ()
      | Hm_declarative.S i -> env_lookup_owned h rest i (); ())

let rec (env_extend @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem after x})) @ total ->
    (env : Hm_environment_spec.env) @ immutable -> {u : unit | env_owned h env} ->
    {u : unit | env_owned after env} @ ghost = fun h after frame env premise -> ghost_ (
    env_owned_def h env; env_owned_def after env;
    match env with Hm_environment_spec.Empty -> ()
    | Hm_environment_spec.Bind (p, rest) -> frame p; env_extend h after frame rest (); ())

let (run_env_owned @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable -> (env : Hm_environment_spec.env) @ immutable -> (e : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && env_owned h env} ->
    {u : unit | env_owned after env} @ ghost = fun h depth pool env e after final_pool premise -> ghost_ (
    let frame : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem after x}) @ total = fun x ->
      let () = Hm_effective_membership.run_extends h depth pool env e after final_pool x () in () in
    env_extend h after frame env (); ())

let (allocation_env @ total) : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (v : node) @ immutable -> (env : Hm_environment_spec.env) @ immutable ->
    {u : unit | env_owned h env} -> {u : unit | env_owned (H.put h p v) env} @ ghost = fun h p v env premise -> ghost_ (
    let after = H.put h p v in
    let frame : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem after x}) @ total = fun x ->
      () in
    env_extend h after frame env (); ())

let (run_result @ total) : (h : Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x
        && (if H.mem h x then Level_finite_spec.finite h t else Level_unifier_spec.observe h x === None)} @ immutable)) @ total ->
    (depth : int) -> (pool : pool) @ immutable -> (env : env) @ immutable ->
    (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && result e === Some p} ->
    {u : unit | H.mem after p} @ ghost =
  fun h trees depth pool env e after final_pool p premise -> ghost_ (
    let r = Hm_effective_result.run h trees depth pool env e after final_pool p () in
    Hm_effective_result.finite_path_def after p r;
    Level_unifier_spec.resolves_def after p r.Representative_level.root r.Representative_level.path;
    ())

let (run_pool_scoped @ total) : (h : Pref.heap) @ immutable ->
    (depth : int) -> (pool : pool) @ immutable -> (env : env) @ immutable ->
    (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (safe : ((x : node Pref.t) @ immutable ->
      {u : unit | Hm_effective_runtime.safe after heads x})) @ total ->
    (final_pool : pool) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool} ->
    {u : unit | pool_scoped after final_pool} @ ghost =
  fun h depth pool env e after heads safe final_pool premise -> ghost_ (
    let scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem after x) || source_ok after x}) @ total = fun x ->
      safe x; Hm_effective_runtime.safe_def after heads x; () in
    let members : ((x : node Pref.t) @ immutable -> {u : unit | not (listed final_pool x) || H.mem after x}) @ total = fun x ->
      if listed final_pool x then (
        Hm_effective_membership.run_pool_member h depth pool env e after final_pool x (); ())
      else () in
    let () = Pooled_proofs.pool_from_members after scope final_pool members in ())

let (run_saved_pool @ total) : (h : Pref.heap) @ immutable ->
    (depth : int) -> (pool : pool) @ immutable -> (env : env) @ immutable ->
    (e : execution) @ immutable -> (after : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (safe : ((x : node Pref.t) @ immutable ->
      {u : unit | Hm_effective_runtime.safe after heads x})) @ total ->
    (final_pool : pool) @ immutable -> (saved : pool) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && pool_scoped h saved} ->
    {u : unit | pool_scoped after saved} @ ghost =
  fun h depth pool env e after heads safe final_pool saved premise -> ghost_ (
    let scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem after x) || source_ok after x}) @ total = fun x ->
      safe x; Hm_effective_runtime.safe_def after heads x; () in
    let members : ((x : node Pref.t) @ immutable -> {u : unit | not (listed saved x) || H.mem after x}) @ total = fun x ->
      if listed saved x then (
        Pooled_proofs.pool_member h saved x ();
        Hm_effective_membership.run_extends h depth pool env e after final_pool x (); ())
      else () in
    let () = Pooled_proofs.pool_from_members after scope saved members in ())
