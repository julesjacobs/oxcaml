(* Unit tests for the DT theory ({!Oxsmt_dt.Dt}), driving the THEORY surface directly
   (register_atom / assert_lit / check / model). The four datatype rules produce a
   [Conflict] on the unsat cases and [Sat] on the consistent ones; constructor case splits
   (enums) need the SAT core and are covered by the end-to-end .smt2 goldens.

   Datatypes used: List = nil | cons (head : Int, tail : List) Color = red | green | blue
   (an enum: three nullary constructors) *)

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
  ; head : Symbol.t
  }

let setup () =
  let env = Env.create () in
  let list_sym = Env.declare_sort env "List" in
  let list_sort = Sort.datatype_ list_sym in
  let color_sym = Env.declare_sort env "Color" in
  let color_sort = Sort.datatype_ color_sym in
  let df name dom cod = Env.declare_fun env name (Rank.create dom cod) in
  let nil = df "nil" [] list_sort in
  let cons = df "cons" [ Sort.int; list_sort ] list_sort in
  let head = df "head" [ list_sort ] Sort.int in
  let tail = df "tail" [ list_sort ] list_sort in
  let is_nil = df "is-nil" [ list_sort ] Sort.bool in
  let is_cons = df "is-cons" [ list_sort ] Sort.bool in
  let red = df "red" [] color_sort in
  let green = df "green" [] color_sort in
  let blue = df "blue" [] color_sort in
  let is_red = df "is-red" [ color_sort ] Sort.bool in
  let is_green = df "is-green" [ color_sort ] Sort.bool in
  let is_blue = df "is-blue" [ color_sort ] Sort.bool in
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
  let reg =
    Defs.add
      reg
      { Defs.sort_sym = color_sym
      ; constructors =
          [ { Defs.sym = red; selectors = []; tester = is_red }
          ; { Defs.sym = green; selectors = []; tester = is_green }
          ; { Defs.sym = blue; selectors = []; tester = is_blue }
          ]
      }
  in
  ignore (tail, is_nil, is_cons, color_sort);
  let ctx = Context.create env in
  { env; ctx; reg; list_sort; nil; cons; head }
;;

(* A driver bundling a Dt theory with an atom allocator. *)
type drv =
  { dt : Dt.t
  ; alloc : Atom.allocator
  }

let make b = { dt = Dt.create b.ctx b.env b.reg; alloc = Atom.create_allocator () }

(* register [phi] as an atom and assert it at [positive] polarity *)
let assert_atom d (phi : Term.t) ~positive =
  let atom = Atom.fresh d.alloc in
  Dt.register_atom d.dt atom phi;
  Dt.assert_lit d.dt (Lit.make atom positive)
;;

let settle d =
  let rec go fuel =
    if fuel = 0 then failwith "settle: did not converge";
    match Dt.check d.dt Theory.Final with
    | Theory.Propagations (_ :: _) -> go (fuel - 1)
    | other -> other
  in
  go 10_000
;;

let is_conflict = function
  | Theory.Conflict _ -> true
  | _ -> false
;;

let is_sat = function
  | Theory.Sat -> true
  | _ -> false
;;

(* ---- Rule 1: different constructors on one e-class conflict ---- *)
let test_clash b =
  let d = make b in
  let x = Context.const b.ctx (Env.declare_fun b.env "x1" (Rank.create [] b.list_sort)) in
  let a = Context.const b.ctx (Env.declare_fun b.env "a1" (Rank.create [] Sort.int)) in
  let l = Context.const b.ctx (Env.declare_fun b.env "l1" (Rank.create [] b.list_sort)) in
  let cons_al = Context.app b.ctx b.cons [ a; l ] in
  let nil_t = Context.const b.ctx b.nil in
  assert_atom d (Context.eq b.ctx x cons_al) ~positive:true;
  assert_atom d (Context.eq b.ctx x nil_t) ~positive:true;
  check "clash: x=cons(a,l) & x=nil is unsat" (is_conflict (settle d))
;;

(* ---- Rule 2: same constructor propagates field equalities ---- *)
let test_injectivity b =
  let d = make b in
  let k name =
    Context.const b.ctx (Env.declare_fun b.env name (Rank.create [] Sort.int))
  in
  let kl name =
    Context.const b.ctx (Env.declare_fun b.env name (Rank.create [] b.list_sort))
  in
  let a = k "a2"
  and c = k "c2" in
  let l1 = kl "l1b"
  and l2 = kl "l2b" in
  let cons1 = Context.app b.ctx b.cons [ a; l1 ] in
  let cons2 = Context.app b.ctx b.cons [ c; l2 ] in
  assert_atom d (Context.eq b.ctx cons1 cons2) ~positive:true;
  assert_atom d (Context.eq b.ctx a c) ~positive:false;
  check "injectivity: cons(a,l1)=cons(c,l2) & a<>c is unsat" (is_conflict (settle d))
;;

(* injectivity is sound, not just complete: same head, equal fields => consistent *)
let test_injectivity_sat b =
  let d = make b in
  let k name =
    Context.const b.ctx (Env.declare_fun b.env name (Rank.create [] Sort.int))
  in
  let kl name =
    Context.const b.ctx (Env.declare_fun b.env name (Rank.create [] b.list_sort))
  in
  let a = k "a2s"
  and c = k "c2s" in
  let l1 = kl "l1s"
  and l2 = kl "l2s" in
  let cons1 = Context.app b.ctx b.cons [ a; l1 ] in
  let cons2 = Context.app b.ctx b.cons [ c; l2 ] in
  assert_atom d (Context.eq b.ctx cons1 cons2) ~positive:true;
  assert_atom d (Context.eq b.ctx a c) ~positive:true;
  check "injectivity-sat: cons(a,l1)=cons(c,l2) & a=c is sat" (is_sat (settle d))
;;

(* ---- Rule 3: selectors evaluate once the constructor is known ---- *)
let test_selector b =
  let d = make b in
  let k name =
    Context.const b.ctx (Env.declare_fun b.env name (Rank.create [] Sort.int))
  in
  let kl name =
    Context.const b.ctx (Env.declare_fun b.env name (Rank.create [] b.list_sort))
  in
  let a = k "a3"
  and l = kl "l3" in
  let cons_al = Context.app b.ctx b.cons [ a; l ] in
  let head_cons = Context.app b.ctx b.head [ cons_al ] in
  (* head(cons(a,l)) <> a is unsat *)
  assert_atom d (Context.eq b.ctx head_cons a) ~positive:false;
  check "selector: head(cons(a,l)) <> a is unsat" (is_conflict (settle d))
;;

(* ---- Rule 4: cycles refute (occurs check) ---- *)
let test_occurs b =
  let d = make b in
  let x = Context.const b.ctx (Env.declare_fun b.env "x4" (Rank.create [] b.list_sort)) in
  let a = Context.const b.ctx (Env.declare_fun b.env "a4" (Rank.create [] Sort.int)) in
  let cons_ax = Context.app b.ctx b.cons [ a; x ] in
  assert_atom d (Context.eq b.ctx x cons_ax) ~positive:true;
  check "occurs: x = cons(a,x) is unsat" (is_conflict (settle d))
;;

(* a deeper cycle: x = cons(a, y), y = cons(b, x) *)
let test_occurs_deep b =
  let d = make b in
  let kl name =
    Context.const b.ctx (Env.declare_fun b.env name (Rank.create [] b.list_sort))
  in
  let ki name =
    Context.const b.ctx (Env.declare_fun b.env name (Rank.create [] Sort.int))
  in
  let x = kl "x5"
  and y = kl "y5" in
  let a = ki "a5"
  and bb = ki "b5" in
  assert_atom d (Context.eq b.ctx x (Context.app b.ctx b.cons [ a; y ])) ~positive:true;
  assert_atom d (Context.eq b.ctx y (Context.app b.ctx b.cons [ bb; x ])) ~positive:true;
  check "occurs-deep: x=cons(a,y) & y=cons(b,x) is unsat" (is_conflict (settle d))
;;

(* a consistent list: x = cons(a, nil) is sat, and the model is a constructor tree *)
let test_sat_model b =
  let d = make b in
  let x = Context.const b.ctx (Env.declare_fun b.env "x6" (Rank.create [] b.list_sort)) in
  let a = Context.const b.ctx (Env.declare_fun b.env "a6" (Rank.create [] Sort.int)) in
  let nil_t = Context.const b.ctx b.nil in
  let cons_a_nil = Context.app b.ctx b.cons [ a; nil_t ] in
  assert_atom d (Context.eq b.ctx x cons_a_nil) ~positive:true;
  check "sat-model: x = cons(a,nil) is sat" (is_sat (settle d));
  match Dt.constructor_model d.dt with
  | None -> check "sat-model: constructor_model present" false
  | Some trees ->
    let x_tree = List.assq_opt x trees in
    let is_cons_nil =
      match x_tree with
      | Some (Dt.Ctor ("cons", [ _; Dt.Ctor ("nil", []) ])) -> true
      | _ -> false
    in
    check "sat-model: x is cons(_, nil)" is_cons_nil
;;

let () =
  let b = setup () in
  test_clash b;
  test_injectivity b;
  test_injectivity_sat b;
  test_selector b;
  test_occurs b;
  test_occurs_deep b;
  test_sat_model b;
  Printf.printf "Dt tests: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
