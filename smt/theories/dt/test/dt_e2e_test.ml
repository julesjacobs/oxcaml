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

(* Declare List + Color into a fresh session and register their datatype structure. *)
let setup () =
  let s = Session.create () in
  let list_sym = Session.declare_sort s "List" in
  let list_sort = Sort.datatype_ list_sym in
  let color_sym = Session.declare_sort s "Color" in
  let color_sort = Sort.datatype_ color_sym in
  let df name dom cod = Session.declare_fun s name (Rank.create dom cod) in
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
  let defs =
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
  let defs =
    Defs.add
      defs
      { Defs.sort_sym = color_sym
      ; constructors =
          [ { Defs.sym = red; selectors = []; tester = is_red }
          ; { Defs.sym = green; selectors = []; tester = is_green }
          ; { Defs.sym = blue; selectors = []; tester = is_blue }
          ]
      }
  in
  Session.set_datatypes s defs;
  s, list_sort, color_sort, nil, cons, head
;;

let k s name sort = Context.const (Session.context s) (Session.declare_const s name sort)

let () =
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
  Printf.printf "Dt e2e tests: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
