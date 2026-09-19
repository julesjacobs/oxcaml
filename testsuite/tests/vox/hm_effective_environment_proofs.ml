open Copy_spec
open Level_unifier_spec
open Generalize_spec
open Hm_environment_spec
open Hm_effective_environment
open Hm_effective_execution_spec
module E = Effective_level
module T = Effective_template

let (allocation_environment @ total) : (h : Pref.heap) @ immutable ->
    (a : E.heads) @ total -> (b : E.heads) @ total ->
    (va : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h a x})) @ total ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable ->
    (vb : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head (H.put h p v) b x})) @ total ->
    (depth : int) -> (env : env) @ immutable -> (ts : templates) @ immutable ->
    {u : unit | not (H.mem h p) && effective_env h a depth env ts} ->
    {u : unit | effective_env (H.put h p v) b depth env ts} @ ghost =
  fun h a b va p v vb depth env ts premise -> ghost_ (
    let after = H.put h p v in
    let frame : ((x : node Pref.t) @ immutable ->
      {u : unit | T.protected h a after b depth x}) @ total = fun x ->
      va x; vb x; observe_def h x; observe_def after x;
      E.effective_below_def h a x depth; E.effective_below_def after b x depth;
      T.generic_def h a x; T.generic_def after b x; if H.mem h x then (Hm_effective_allocation.saved_level h a b p v x (); ());
      T.protected_def h a after b depth x; () in
    transport h a after b depth frame env ts (); ())

let (run_environment @ total) : (h : Pref.heap) @ immutable ->
    (a : E.heads) @ total -> (b : E.heads) @ total ->
    (va : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h a x})) @ total ->
    (depth : int) -> (pool : pool) @ immutable -> (env : env) @ immutable ->
    (ts : templates) @ immutable -> (e : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    (vb : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head after b x})) @ total ->
    (bound : int) ->
    {u : unit | ran h depth pool env e after final_pool && bound <= depth
      && effective_env h a bound env ts} ->
    {u : unit | effective_env after b bound env ts} @ ghost =
  fun h a b va depth pool env ts e after final_pool vb bound premise -> ghost_ (
    let frame : ((x : node Pref.t) @ immutable ->
      {u : unit | T.protected h a after b bound x}) @ total = fun x ->
      va x; vb x; Hm_effective_generic.run_protected h a b depth pool env e after final_pool x bound ();
      () in
    transport h a after b bound frame env ts (); ())

let rec (boundary_raise @ total) : (h : Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (lo : int) -> (hi : int) ->
    (s : template) @ immutable -> {u : unit | lo <= hi && T.boundary_bound h heads lo s} ->
    {u : unit | T.boundary_bound h heads hi s} @ ghost =
  fun h heads lo hi s premise -> ghost_ (
    T.boundary_bound_def h heads lo s;
    T.boundary_bound_def h heads hi s; match s with
    | Boundary p -> E.effective_below_def h heads p lo;
      E.effective_below_def h heads p hi; ()
    | Parameter _ | Constant _ -> ()
    | Indirect (_, child) -> boundary_raise h heads lo hi child (); ()
    | Product (_, a, b) -> boundary_raise h heads lo hi a ();
      boundary_raise h heads lo hi b (); ())

let rec (environment_raise @ total) : (h : Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (lo : int) -> (hi : int) ->
    (env : env) @ immutable -> (ts : templates) @ immutable ->
    {u : unit | lo <= hi && effective_env h heads lo env ts} ->
    {u : unit | effective_env h heads hi env ts} @ ghost =
  fun h heads lo hi env ts premise -> ghost_ (
    effective_env_def h heads lo env ts;
    effective_env_def h heads hi env ts; match env with
    | Empty -> ()
    | Bind (_, rest) -> match ts with No_templates -> ()
      | Template_binding (s, tail) -> boundary_raise h heads lo hi s ();
        environment_raise h heads lo hi rest tail (); ())

let rec (boundary_below @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total -> (depth : int) ->
    (s : template) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | T.boundary_bound h heads depth s && boundary_member s p} ->
    {u : unit | E.effective_below h heads p depth} @ ghost = fun h heads depth s p premise -> ghost_ (
    T.boundary_bound_def h heads depth s;
    boundary_member_def s p; match s with
    | Boundary _ | Parameter _ | Constant _ -> ()
    | Product (_, a, b) -> if boundary_member a p then
      (boundary_below h heads depth a p (); ())
      else (boundary_below h heads depth b p (); ())
    | Indirect (_, child) -> boundary_below h heads depth child p (); ())

let rec (environment_boundary_owned @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (depth : int) -> (env : env) @ immutable -> (ts : templates) @ immutable ->
    (p : node Pref.t) @ immutable ->
    {u : unit | effective_env h heads depth env ts && environment_boundary ts p} ->
    {u : unit | H.mem h p} @ ghost = fun h heads depth env ts p premise -> ghost_ (
    effective_env_def h heads depth env ts; environment_boundary_def ts p;
    match env with Empty -> ()
    | Bind (_, rest) -> match ts with No_templates -> ()
      | Template_binding (s, tail) -> if boundary_member s p then
        (boundary_below h heads depth s p (); E.effective_below_def h heads p depth; ())
        else (environment_boundary_owned h heads depth rest tail p (); ()))
