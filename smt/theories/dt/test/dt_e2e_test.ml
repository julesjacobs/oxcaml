(* End-to-end tests driving the full CDCL(T) loop through the public {!Session} API with
   the DT theory installed (Session.set_datatypes flips the theory). These cover what the
   direct-THEORY unit tests cannot: the constructor case split for enums, which needs the
   real SAT core to branch on the exhaustiveness clause. Mirrors the .smt2 goldens, one
   command sequence at a time. *)

open Oxsmt_core
module Session = Oxsmt_interface.Session
module Defs = Oxsmt_core.Datatype_defs

let checks = ref 0
let failures = ref 0

let expect name got want =
  incr checks;
  if got <> want
  then (
    incr failures;
    let s = function
      | Session.Sat -> "sat"
      | Session.Unsat -> "unsat"
      | Session.Unknown -> "unknown"
    in
    Printf.printf "  FAIL %s: got %s, want %s\n" name (s got) (s want))
;;

(* Declare List + Color into a fresh session via the programmatic door (which mints
   reserved .oxsmt.is-C testers), and pull the constructor/selector symbols back out of
   the returned datatype record. This exercises the exact API->theory path the TCB cares
   about. *)
let ctor (dt : Defs.datatype) i = (List.nth dt.Defs.constructors i).Defs.sym

let sel (dt : Defs.datatype) i j =
  (List.nth (List.nth dt.Defs.constructors i).Defs.selectors j).Defs.sym
;;

let setup () =
  let s = Session.create () in
  let list_sort = Sort.datatype_ (Session.declare_sort s "List") in
  let color_sort = Sort.datatype_ (Session.declare_sort s "Color") in
  let dt_list =
    Session.declare_datatype
      s
      list_sort
      [ { Session.ctor_name = "nil"; fields = [] }
      ; { Session.ctor_name = "cons"; fields = [ "head", Sort.int; "tail", list_sort ] }
      ]
  in
  ignore
    (Session.declare_datatype
       s
       color_sort
       [ { Session.ctor_name = "red"; fields = [] }
       ; { Session.ctor_name = "green"; fields = [] }
       ; { Session.ctor_name = "blue"; fields = [] }
       ]);
  let nil = ctor dt_list 0 in
  let cons = ctor dt_list 1 in
  let head = sel dt_list 1 0 in
  s, list_sort, color_sort, nil, cons, head
;;

let k s name sort = Context.const (Session.context s) (Session.declare_const s name sort)

let () =
  (* TCB property: declare_datatype mints testers in the reserved namespace, so a user
     function cannot forge one and shadow it in the printed session. *)
  (let s = Session.create () in
   let ls = Sort.datatype_ (Session.declare_sort s "L") in
   let dt = Session.declare_datatype s ls [ { Session.ctor_name = "c"; fields = [] } ] in
   let tester = (List.nth dt.Defs.constructors 0).Defs.tester in
   incr checks;
   if not (Env.is_reserved_name (Symbol.name tester))
   then (
     incr failures;
     Printf.printf "  FAIL tester %s is not reserved-namespaced\n" (Symbol.name tester)));
  (* Rule 1: constructor clash *)
  (let s, ls, _cs, nil, cons, _head = setup () in
   let ctx = Session.context s in
   let x = k s "x1" ls
   and a = k s "a1" Sort.int
   and l = k s "l1" ls in
   Session.assert_term s (Context.eq ctx x (Context.app ctx cons [ a; l ]));
   Session.assert_term s (Context.eq ctx x (Context.const ctx nil));
   expect "clash: x=cons(a,l) & x=nil" (Session.check_sat s) Session.Unsat);
  (* Rule 2: injectivity + a disequal field *)
  (let s, ls, _cs, _nil, cons, _head = setup () in
   let ctx = Session.context s in
   let a = k s "a2" Sort.int
   and c = k s "c2" Sort.int in
   let l1 = k s "l1b" ls
   and l2 = k s "l2b" ls in
   let c1 = Context.app ctx cons [ a; l1 ]
   and c2 = Context.app ctx cons [ c; l2 ] in
   Session.assert_term s (Context.eq ctx c1 c2);
   Session.assert_term s (Context.not_ ctx (Context.eq ctx a c));
   expect "injectivity: cons(a,l1)=cons(c,l2) & a<>c" (Session.check_sat s) Session.Unsat);
  (* Rule 3: selector evaluation *)
  (let s, ls, _cs, _nil, cons, head = setup () in
   let ctx = Session.context s in
   let a = k s "a3" Sort.int
   and l = k s "l3" ls in
   let hc = Context.app ctx head [ Context.app ctx cons [ a; l ] ] in
   Session.assert_term s (Context.not_ ctx (Context.eq ctx hc a));
   expect "selector: head(cons(a,l)) <> a" (Session.check_sat s) Session.Unsat);
  (* Rule 4: occurs check *)
  (let s, ls, _cs, _nil, cons, _head = setup () in
   let ctx = Session.context s in
   let x = k s "x4" ls
   and a = k s "a4" Sort.int in
   Session.assert_term s (Context.eq ctx x (Context.app ctx cons [ a; x ]));
   expect "occurs: x = cons(a,x)" (Session.check_sat s) Session.Unsat);
  (* Enum case split: four pairwise-distinct Colors, only three constructors *)
  (let s, _ls, cs, _nil, _cons, _head = setup () in
   let ctx = Session.context s in
   let v1 = k s "v1" cs
   and v2 = k s "v2" cs
   and v3 = k s "v3" cs
   and v4 = k s "v4" cs in
   Session.assert_term s (Context.distinct ctx [ v1; v2; v3; v4 ]);
   expect "enum: 4 distinct of a 3-constructor type" (Session.check_sat s) Session.Unsat);
  (* Three distinct Colors IS satisfiable (a discrimination check: the split must not
     over-refute). *)
  (let s, _ls, cs, _nil, _cons, _head = setup () in
   let ctx = Session.context s in
   let v1 = k s "w1" cs
   and v2 = k s "w2" cs
   and v3 = k s "w3" cs in
   Session.assert_term s (Context.distinct ctx [ v1; v2; v3 ]);
   let v = Session.check_sat s in
   (* sat model may degrade to unknown (constructor-tree model is a follow-up); the
      soundness requirement is only that it is NOT unsat. *)
   incr checks;
   if v = Session.Unsat
   then (
     incr failures;
     Printf.printf "  FAIL enum-sat: 3 distinct colors reported unsat\n"));
  (* codex fix 1 — bounded-enum FIELD pigeonhole. wrap injects a 2-value enum; three
     distinct wraps force three distinct fields, impossible over two values: unsat. Before
     the field-relevance fix this returned Sat (the fields were never case-split). *)
  (let s = Session.create () in
   let bit = Sort.datatype_ (Session.declare_sort s "Bit") in
   ignore
     (Session.declare_datatype
        s
        bit
        [ { Session.ctor_name = "b0"; fields = [] }
        ; { Session.ctor_name = "b1"; fields = [] }
        ]);
   let ws = Sort.datatype_ (Session.declare_sort s "W") in
   let dtw =
     Session.declare_datatype
       s
       ws
       [ { Session.ctor_name = "wrap"; fields = [ "get", bit ] } ]
   in
   let wrap = ctor dtw 0 in
   let ctx = Session.context s in
   let w a = Context.app ctx wrap [ k s a bit ] in
   Session.assert_term s (Context.distinct ctx [ w "e1"; w "e2"; w "e3" ]);
   expect
     "enum-field pigeonhole: 3 distinct wraps of a 2-value field"
     (Session.check_sat s)
     Session.Unsat);
  (* discrimination: TWO distinct wraps of a 2-value field is satisfiable (the field split
     must not over-refute). NOT unsat is the soundness requirement (sat may degrade). *)
  (let s = Session.create () in
   let bit = Sort.datatype_ (Session.declare_sort s "Bit2") in
   ignore
     (Session.declare_datatype
        s
        bit
        [ { Session.ctor_name = "z0"; fields = [] }
        ; { Session.ctor_name = "z1"; fields = [] }
        ]);
   let ws = Sort.datatype_ (Session.declare_sort s "W2") in
   let dtw =
     Session.declare_datatype
       s
       ws
       [ { Session.ctor_name = "wrap2"; fields = [ "g2", bit ] } ]
   in
   let wrap = ctor dtw 0 in
   let ctx = Session.context s in
   let w a = Context.app ctx wrap [ k s a bit ] in
   Session.assert_term s (Context.distinct ctx [ w "f1"; w "f2" ]);
   incr checks;
   if Session.check_sat s = Session.Unsat
   then (
     incr failures;
     Printf.printf "  FAIL enum-field-sat: 2 distinct wraps reported unsat\n"));
  (* codex fix 2 — single-constructor record: fst p = fst q ∧ snd p = snd q ∧ p ≠ q is
     unsat (forcing p = mk(fst p, snd p), q = mk(fst q, snd q), then congruence equates
     them ⟹ p = q). Before the single-ctor forcing this returned Sat. *)
  (let s = Session.create () in
   let ps = Sort.datatype_ (Session.declare_sort s "Pair") in
   let dt =
     Session.declare_datatype
       s
       ps
       [ { Session.ctor_name = "mk"; fields = [ "fst", Sort.int; "snd", Sort.int ] } ]
   in
   let fst_ = sel dt 0 0
   and snd_ = sel dt 0 1 in
   let ctx = Session.context s in
   let p = k s "p" ps
   and q = k s "q" ps in
   let ap f x = Context.app ctx f [ x ] in
   Session.assert_term s (Context.eq ctx (ap fst_ p) (ap fst_ q));
   Session.assert_term s (Context.eq ctx (ap snd_ p) (ap snd_ q));
   Session.assert_term s (Context.not_ ctx (Context.eq ctx p q));
   expect "record: fst=fst & snd=snd & p<>q refutes" (Session.check_sat s) Session.Unsat);
  Printf.printf "Dt e2e tests: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
