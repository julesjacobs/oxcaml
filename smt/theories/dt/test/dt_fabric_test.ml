(* Stage B soundness REDs for the DT theory on the e-graph fabric ({!Oxsmt_dt.Dt}),
   driving the DARK fabric seam directly ([assert_fabric_eq] / [check_fabric] /
   [explain_fabric] / [fabric_are_equal] / [push] / [pop]).

   (a) Γ-COMPLETENESS / edge-id-preservation. A hub-injected equality carries a
       [Fabric edge_id] premise handle. The soundness contract is that this handle
       SURVIVES into the conflict Γ [check_fabric] returns — both when the edge is a
       DIRECT premise of the violated axiom (constructor distinctness) and when it is
       BURIED inside a DT-derived equality (injectivity via [derived_premise]). A
       flattening regression that dropped the edge (the exact "buried injected fabric edge
       is LOST -> incomplete Γ -> WRONG UNSAT" hazard) would omit [Fabric E] from the
       returned premises and FAIL these checks.

   (b) cancel_until 0 / restorability. A fabric edge asserted inside a pushed frame — plus
       the propagation reason it caches — is FULLY retracted by [pop]: [fabric_are_equal]
       reverts to [false] and the cached reason is gone (a post-pop [explain_fabric] of
       the propagated literal raises rather than serving a stale, popped-frame premise).
       Mirrors smt/fabric/test/stage0_trail_test.ml's push/assert/pop restorability
       oracle. *)

open Oxsmt_core
module Dt = Oxsmt_dt.Dt
module Defs = Oxsmt_core.Datatype_defs

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

type env_bundle =
  { env : Env.t
  ; ctx : Context.t
  ; reg : Defs.t
  ; list_sort : Sort.t
  ; nil : Symbol.t
  ; cons : Symbol.t
  }

(* List = nil | cons (head : Int, tail : List). *)
let setup () =
  let env = Env.create () in
  let list_sym = Env.declare_sort env "List" in
  let list_sort = Sort.datatype_ list_sym in
  let df name dom cod = Env.declare_fun env name (Rank.create dom cod) in
  let nil = df "nil" [] list_sort in
  let cons = df "cons" [ Sort.int; list_sort ] list_sort in
  let head = df "head" [ list_sort ] Sort.int in
  let tail = df "tail" [ list_sort ] list_sort in
  let is_nil = df "is-nil" [ list_sort ] Sort.bool in
  let is_cons = df "is-cons" [ list_sort ] Sort.bool in
  let sel sym index field_sort = { Defs.sym; index; field_sort } in
  let reg =
    Defs.add
      Defs.empty
      { Defs.sort_sym = list_sym
      ; constructors =
          [ { Defs.sym = nil; selectors = []; tester = is_nil }
          ; { Defs.sym = cons
            ; selectors = [ sel head 0 Sort.int; sel tail 1 list_sort ]
            ; tester = is_cons
            }
          ]
      }
  in
  let ctx = Context.create env in
  { env; ctx; reg; list_sort; nil; cons }
;;

type drv =
  { dt : Dt.t
  ; alloc : Atom.allocator
  }

let make b = { dt = Dt.create b.ctx b.env (ref b.reg); alloc = Atom.create_allocator () }

(* register [phi] as an atom and assert it at [positive] polarity *)
let assert_atom d (phi : Term.t) ~positive =
  let atom = Atom.fresh d.alloc in
  Dt.register_atom d.dt atom phi;
  Dt.assert_lit d.dt (Lit.make atom positive)
;;

(* register [phi] as a WATCHED atom without asserting it (so a merge can propagate it) *)
let register_watched d (phi : Term.t) : Lit.t =
  let atom = Atom.fresh d.alloc in
  Dt.register_atom d.dt atom phi;
  Lit.make atom true
;;

let list_var b name =
  Context.const b.ctx (Env.declare_fun b.env name (Rank.create [] b.list_sort))
;;

let int_var b name =
  Context.const b.ctx (Env.declare_fun b.env name (Rank.create [] Sort.int))
;;

let premises_contain_edge (r : Fabric.check_result) (e : Fabric.edge_id) : bool =
  match r with
  | Fabric.Conflict expl ->
    List.exists
      (function
        | Fabric.Fabric id -> id = e
        | Fabric.Real _ -> false)
      expl.Fabric.Explanation.premises
  | _ -> false
;;

let is_fabric_conflict = function
  | Fabric.Conflict _ -> true
  | _ -> false
;;

(* -------- (a1) DIRECT edge in a constructor-distinctness conflict -------- *)
(* x = cons(a,l) [L1], y = nil [L2], and a fabric edge E : x = y. The class then holds two
   distinct constructors (cons and nil); the clash explanation routes cons(a,l) — x — y —
   nil, so it MUST cite the fabric edge E. *)
let test_direct_edge b =
  let d = make b in
  let e = 4201 in
  let x = list_var b "xd" in
  let y = list_var b "yd" in
  let a = int_var b "ad" in
  let l = list_var b "ld" in
  let cons_al = Context.app b.ctx b.cons [ a; l ] in
  let nil_t = Context.const b.ctx b.nil in
  assert_atom d (Context.eq b.ctx x cons_al) ~positive:true;
  assert_atom d (Context.eq b.ctx y nil_t) ~positive:true;
  Dt.assert_fabric_eq d.dt ~edge_id:e x y;
  let r = Dt.check_fabric d.dt Theory.Final in
  check "direct: x=cons(a,l), y=nil, E:x=y is a fabric Conflict" (is_fabric_conflict r);
  check
    "direct: conflict Γ contains Fabric E (edge preserved)"
    (premises_contain_edge r e)
;;

(* -------- (a2) BURIED edge inside a DT-derived (injectivity) premise -------- *)
(* u = cons(a,l1) [L1], v = cons(c,l2) [L2], a <> c [L3], and a fabric edge E : u = v.
   Injectivity fires on the two same-constructor terms merged by E: it asserts a = c with
   a [P_derived] premise whose chain is [{L1, E, L2}]. That derived a = c then violates
   the asserted a <> c, so the conflict Γ carries the BURIED fabric edge E. Dropping the
   edge when flattening [P_derived] (the wrong-UNSAT hazard) would omit it here. *)
let test_buried_edge b =
  let d = make b in
  let e = 4202 in
  let a = int_var b "ab" in
  let c = int_var b "cb" in
  let l1 = list_var b "l1b" in
  let l2 = list_var b "l2b" in
  let cons1 = Context.app b.ctx b.cons [ a; l1 ] in
  let cons2 = Context.app b.ctx b.cons [ c; l2 ] in
  let u = list_var b "ub" in
  let v = list_var b "vb" in
  assert_atom d (Context.eq b.ctx u cons1) ~positive:true;
  assert_atom d (Context.eq b.ctx v cons2) ~positive:true;
  assert_atom d (Context.eq b.ctx a c) ~positive:false;
  Dt.assert_fabric_eq d.dt ~edge_id:e u v;
  let r = Dt.check_fabric d.dt Theory.Final in
  check "buried: injectivity conflict via E is a fabric Conflict" (is_fabric_conflict r);
  check
    "buried: conflict Γ contains Fabric E through a P_derived chain (edge preserved)"
    (premises_contain_edge r e)
;;

(* -------- (b) push / assert_fabric_eq + derive / pop restorability -------- *)
(* A watched equality atom (x = y), unasserted, is propagated true when a pushed fabric
   edge E merges x and y — caching a reason that cites E. [pop] must retract the merge
   ([fabric_are_equal] false again) AND drop the cached reason (a post-pop
   [explain_fabric] raises). *)
let test_pop_restores b =
  let d = make b in
  let e = 4203 in
  let x = list_var b "xr" in
  let y = list_var b "yr" in
  (* base frame: register + watch (x = y) without asserting; x,y become known terms *)
  let eq_lit = register_watched d (Context.eq b.ctx x y) in
  check "restore: x,y not equal at base" (not (Dt.fabric_are_equal d.dt x y));
  Dt.push d.dt;
  Dt.assert_fabric_eq d.dt ~edge_id:e x y;
  check "restore: x,y equal after fabric merge in frame" (Dt.fabric_are_equal d.dt x y);
  let r = Dt.check_fabric d.dt Theory.Final in
  let propagated_eq =
    match r with
    | Fabric.Propagations lits -> List.exists (Lit.equal eq_lit) lits
    | _ -> false
  in
  check
    "restore: merged (x=y) atom is propagated (derive populated the cache)"
    propagated_eq;
  (* the cached reason for the propagated literal cites the fabric edge E *)
  let cached_cites_e =
    match Dt.explain_fabric d.dt eq_lit with
    | expl ->
      List.exists
        (function
          | Fabric.Fabric id -> id = e
          | Fabric.Real _ -> false)
        expl.Fabric.Explanation.premises
    | exception _ -> false
  in
  check "restore: cached reason cites Fabric E before pop" cached_cites_e;
  Dt.pop d.dt 1;
  check
    "restore: x,y NOT equal after pop (merge retracted)"
    (not (Dt.fabric_are_equal d.dt x y));
  let cache_cleared =
    match Dt.explain_fabric d.dt eq_lit with
    | _ -> false (* a served reason from a popped frame is a leak *)
    | exception _ -> true
  in
  check "restore: no leaked explain-cache entry after pop" cache_cleared
;;

let () =
  let b = setup () in
  test_direct_edge b;
  test_buried_edge b;
  test_pop_restores b;
  Printf.printf "Dt fabric tests: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
