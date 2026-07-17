(* Consumer proof for integer objective optimization.

   The main problem is small enough to enumerate completely. The test checks both
   optimization directions, negative optima, a nontrivial linear combination, the
   returned model, and a fresh-session UNSAT certificate immediately past the reported
   value. Additional cases pin the query budget, initial-model certification, hard
   infeasibility, temporary-scope cleanup, arbitrary-precision values, and fail-closed
   behavior on an unbounded objective and an underlying Unknown. *)

open Oxsmt_core
module Session = Oxsmt_interface.Session
module Optimize = Oxsmt_interface.Optimize

let checks = ref 0
let failures = ref 0

let fail name message =
  incr failures;
  Printf.printf "  FAIL %s: %s\n" name message
;;

let check name condition =
  incr checks;
  if not condition then fail name "expected true"
;;

let check_bigint name expected actual = check name (Bigint.equal expected actual)

type direction =
  | Minimize
  | Maximize

type point =
  { x : int
  ; y : int
  ; z : int
  }

let point_is_feasible point =
  point.x >= -2
  && point.x <= 3
  && point.y >= -1
  && point.y <= 2
  && point.z >= -3
  && point.z <= 2
  && point.x + point.y + point.z >= 1
;;

let point_objective point = (2 * point.x) - (3 * point.y) + point.z

let feasible_points () =
  let points = ref [] in
  for x = -2 to 3 do
    for y = -1 to 2 do
      for z = -3 to 2 do
        let point = { x; y; z } in
        if point_is_feasible point then points := point :: !points
      done
    done
  done;
  !points
;;

let brute_force direction points =
  List.fold_left
    (fun best point ->
       let value = point_objective point in
       match best, direction with
       | None, _ -> Some value
       | Some prior, Minimize when value < prior -> Some value
       | Some prior, Maximize when value > prior -> Some value
       | Some _, (Minimize | Maximize) -> best)
    None
    points
;;

type problem =
  { session : Session.t
  ; objective : Term.t
  }

let int_var session name =
  Context.const (Session.context session) (Session.declare_const session name Sort.int)
;;

let build_problem () =
  let session = Session.create () in
  let ctx = Session.context session in
  let x = int_var session "x" in
  let y = int_var session "y" in
  let z = int_var session "z" in
  let int = Context.int_const ctx in
  let sum = Context.linear_combination ctx [ 1, x; 1, y; 1, z ] 0 in
  List.iter
    (Session.assert_term session)
    [ Context.ge ctx x (int (-2))
    ; Context.le ctx x (int 3)
    ; Context.ge ctx y (int (-1))
    ; Context.le ctx y (int 2)
    ; Context.ge ctx z (int (-3))
    ; Context.le ctx z (int 2)
    ; Context.ge ctx sum (int 1)
    ];
  let objective = Context.linear_combination ctx [ 2, x; -3, y; 1, z ] 0 in
  { session; objective }
;;

let binding_name = function
  | Session.Const (name, _) | Session.Fun (name, _) -> name
;;

let has_prefix string prefix =
  String.length string >= String.length prefix
  && String.sub string 0 (String.length prefix) = prefix
;;

let helper_bindings_absent (_sorts, bindings) =
  List.for_all
    (fun binding -> not (has_prefix (binding_name binding) "@oxsmt.optimize.objective."))
    bindings
;;

let model_int name (_sorts, bindings) =
  let value = ref None in
  let valid = ref true in
  List.iter
    (function
      | Session.Const (binding, Session.VInt n) when String.equal binding name ->
        (match !value with
         | None -> value := Some n
         | Some _ -> valid := false)
      | (Session.Const (binding, _) | Session.Fun (binding, _))
        when String.equal binding name -> valid := false
      | Session.Const _ | Session.Fun _ -> ())
    bindings;
  if !valid then !value else None
;;

let model_point model =
  match model_int "x" model, model_int "y" model, model_int "z" model with
  | Some x, Some y, Some z ->
    (match Bigint.to_int_opt x, Bigint.to_int_opt y, Bigint.to_int_opt z with
     | Some x, Some y, Some z -> Some { x; y; z }
     | None, _, _ | _, None, _ | _, _, None -> None)
  | None, _, _ | _, None, _ | _, _, None -> None
;;

let optimize direction problem =
  match direction with
  | Minimize -> Optimize.Omt.minimize problem.session problem.objective
  | Maximize -> Optimize.Omt.maximize problem.session problem.objective
;;

let strict_certificate direction value =
  let problem = build_problem () in
  let ctx = Session.context problem.session in
  let value = Context.int_const_big ctx value in
  let bound =
    match direction with
    | Minimize -> Context.lt ctx problem.objective value
    | Maximize -> Context.gt ctx problem.objective value
  in
  Session.assert_term problem.session bound;
  Session.check_sat problem.session = Session.Unsat
;;

let check_problem direction expected =
  let label =
    match direction with
    | Minimize -> "linear minimize"
    | Maximize -> "linear maximize"
  in
  let problem = build_problem () in
  match optimize direction problem with
  | Optimize.Omt.Hard_unsat -> fail label "reported feasible hard constraints as unsat"
  | Optimize.Omt.Unbounded -> fail label "reported a bounded objective as unbounded"
  | Optimize.Omt.Unknown -> fail label "returned unknown for a small bounded problem"
  | Optimize.Omt.Optimal optimum ->
    check_bigint (label ^ ": brute-force optimum") (Bigint.of_int expected) optimum.value;
    check (label ^ ": helper binding stripped") (helper_bindings_absent optimum.model);
    (match model_point optimum.model with
     | None -> fail label "returned model does not bind x, y, and z as integers"
     | Some point ->
       check (label ^ ": model satisfies hard constraints") (point_is_feasible point);
       check (label ^ ": model achieves optimum") (point_objective point = expected));
    check
      (label ^ ": strict bound independently unsat")
      (strict_certificate direction optimum.value);
    (match direction with
     | Maximize -> ()
     | Minimize ->
       (* A leaked search bound would conflict with this feasible maximum. *)
       let ctx = Session.context problem.session in
       Session.assert_term
         problem.session
         (Context.eq ctx problem.objective (Context.int_const ctx 11));
       check
         "linear minimize: temporary bounds removed"
         (Session.check_sat problem.session = Session.Sat))
;;

let () =
  let points = feasible_points () in
  let minimum = Option.get (brute_force Minimize points) in
  let maximum = Option.get (brute_force Maximize points) in
  check "brute-force minimum is negative" (minimum = -9);
  check "brute-force maximum" (maximum = 11);
  let seen = Array.make (maximum - minimum + 1) false in
  List.iter (fun point -> seen.(point_objective point - minimum) <- true) points;
  check
    "every adjacent objective value is feasible"
    (Array.for_all (fun present -> present) seen);
  check_problem Minimize minimum;
  check_problem Maximize maximum
;;

let range_problem () =
  let session = Session.create () in
  let ctx = Session.context session in
  let x = int_var session "range_x" in
  Session.assert_term session (Context.ge ctx x (Context.int_const ctx (-5)));
  Session.assert_term session (Context.le ctx x (Context.int_const ctx 5));
  session, x
;;

let range_oracle direction =
  let best = ref None in
  for value = -5 to 5 do
    match !best, direction with
    | None, _ -> best := Some value
    | Some prior, Minimize when value < prior -> best := Some value
    | Some prior, Maximize when value > prior -> best := Some value
    | Some _, (Minimize | Maximize) -> ()
  done;
  Option.get !best
;;

(* Both directions use the same hard problem. Returning its first model cannot satisfy
   both endpoint oracles, so together these cases reject an optimizer that stops early. *)
let check_range direction =
  let session, x = range_problem () in
  let expected = range_oracle direction in
  let result =
    match direction with
    | Minimize -> Optimize.Omt.minimize session x
    | Maximize -> Optimize.Omt.maximize session x
  in
  match result with
  | Optimize.Omt.Optimal optimum ->
    check_bigint "early-stop endpoint optimum" (Bigint.of_int expected) optimum.value;
    check
      "early-stop endpoint model achieves optimum"
      (match model_int "range_x" optimum.model with
       | Some value -> Bigint.equal value optimum.value
       | None -> false)
  | Optimize.Omt.Hard_unsat | Optimize.Omt.Unbounded | Optimize.Omt.Unknown ->
    fail "early-stop" "did not return the brute-force optimum"
;;

let () =
  check_range Minimize;
  check_range Maximize
;;

let fixed_problem value =
  let session = Session.create () in
  let ctx = Session.context session in
  let x = int_var session "fixed_x" in
  Session.assert_term session (Context.eq ctx x (Context.int_const ctx value));
  session, x
;;

let () =
  let session, x = fixed_problem (-4) in
  check
    "initial-optimal witness alone is not a certificate"
    (match Optimize.Omt.minimize ~max_checks:1 session x with
     | Optimize.Omt.Unknown -> true
     | Optimize.Omt.Optimal _ | Optimize.Omt.Hard_unsat | Optimize.Omt.Unbounded -> false);
  let session, x = fixed_problem (-4) in
  check
    "initial-optimal needs exactly one certifying check"
    (match Optimize.Omt.minimize ~max_checks:2 session x with
     | Optimize.Omt.Optimal optimum ->
       Bigint.equal optimum.value (Bigint.of_int (-4))
       &&
         (match model_int "fixed_x" optimum.model with
         | Some value -> Bigint.equal value optimum.value
         | None -> false)
     | Optimize.Omt.Unknown | Optimize.Omt.Hard_unsat | Optimize.Omt.Unbounded -> false)
;;

let () =
  let session = Session.create () in
  let ctx = Session.context session in
  let x = int_var session "infeasible_x" in
  Session.assert_term session (Context.le ctx x (Context.int_const ctx 0));
  Session.assert_term session (Context.ge ctx x (Context.int_const ctx 1));
  check
    "brute-force hard problem has no point"
    (not (List.exists (fun value -> value <= 0 && value >= 1) [ -1; 0; 1; 2 ]));
  check
    "hard infeasibility"
    (match Optimize.Omt.minimize session x with
     | Optimize.Omt.Hard_unsat -> true
     | Optimize.Omt.Optimal _ | Optimize.Omt.Unbounded | Optimize.Omt.Unknown -> false)
;;

let () =
  let huge = Bigint.of_string "9223372036854775808" in
  let session = Session.create () in
  let objective = Context.int_const_big (Session.context session) huge in
  check
    "arbitrary-precision objective"
    (match Optimize.Omt.minimize ~max_checks:2 session objective with
     | Optimize.Omt.Optimal optimum ->
       Bigint.equal optimum.value huge && helper_bindings_absent optimum.model
     | Optimize.Omt.Hard_unsat | Optimize.Omt.Unbounded | Optimize.Omt.Unknown -> false)
;;

let () =
  let session = Session.create () in
  let x = int_var session "unbounded_x" in
  check
    "unbounded search exhausts deterministic budget"
    (match Optimize.Omt.minimize ~max_checks:4 session x with
     | Optimize.Omt.Unknown -> true
     | Optimize.Omt.Optimal _ | Optimize.Omt.Hard_unsat | Optimize.Omt.Unbounded -> false);
  let ctx = Session.context session in
  Session.assert_term session (Context.eq ctx x (Context.int_const ctx 0));
  check
    "budget exhaustion removes temporary bounds"
    (Session.check_sat session = Session.Sat)
;;

let () =
  let session = Session.create ~max_effort:0 () in
  let ctx = Session.context session in
  let x = int_var session "unknown_x" in
  let p = Context.const ctx (Session.declare_const session "unknown_p" Sort.bool) in
  let q = Context.const ctx (Session.declare_const session "unknown_q" Sort.bool) in
  Session.assert_term session (Context.eq ctx x (Context.int_const ctx 0));
  Session.assert_term session (Context.or_ ctx [ p; q ]);
  check
    "underlying unknown fails closed"
    (match Optimize.Omt.minimize session x with
     | Optimize.Omt.Unknown -> true
     | Optimize.Omt.Optimal _ | Optimize.Omt.Hard_unsat | Optimize.Omt.Unbounded -> false)
;;

let () =
  let session = Session.create () in
  let ctx = Session.context session in
  let bool_objective =
    Context.const ctx (Session.declare_const session "bool_objective" Sort.bool)
  in
  check
    "Boolean objective rejected"
    (match Optimize.Omt.minimize session bool_objective with
     | exception Invalid_argument _ -> true
     | Optimize.Omt.Optimal _
     | Optimize.Omt.Hard_unsat
     | Optimize.Omt.Unbounded
     | Optimize.Omt.Unknown -> false);
  let int_objective = int_var session "negative_budget_x" in
  check
    "negative optimizer budget rejected"
    (match Optimize.Omt.minimize ~max_checks:(-1) session int_objective with
     | exception Invalid_argument _ -> true
     | Optimize.Omt.Optimal _
     | Optimize.Omt.Hard_unsat
     | Optimize.Omt.Unbounded
     | Optimize.Omt.Unknown -> false);
  check
    "zero optimizer budget fails closed"
    (match Optimize.Omt.minimize ~max_checks:0 session int_objective with
     | Optimize.Omt.Unknown -> true
     | Optimize.Omt.Optimal _ | Optimize.Omt.Hard_unsat | Optimize.Omt.Unbounded -> false);
  let condition =
    Context.const ctx (Session.declare_const session "ite_condition" Sort.bool)
  in
  let ite_objective = Context.ite ctx condition int_objective (Context.int_const ctx 0) in
  check
    "integer Ite objective rejected"
    (match Optimize.Omt.minimize session ite_objective with
     | exception Invalid_argument _ -> true
     | Optimize.Omt.Optimal _
     | Optimize.Omt.Hard_unsat
     | Optimize.Omt.Unbounded
     | Optimize.Omt.Unknown -> false);
  let divisor = Context.int_const ctx 2 in
  check
    "div objective rejected"
    (match Optimize.Omt.minimize session (Context.div ctx int_objective divisor) with
     | exception Invalid_argument _ -> true
     | Optimize.Omt.Optimal _
     | Optimize.Omt.Hard_unsat
     | Optimize.Omt.Unbounded
     | Optimize.Omt.Unknown -> false);
  check
    "mod objective rejected"
    (match Optimize.Omt.minimize session (Context.mod_ ctx int_objective divisor) with
     | exception Invalid_argument _ -> true
     | Optimize.Omt.Optimal _
     | Optimize.Omt.Hard_unsat
     | Optimize.Omt.Unbounded
     | Optimize.Omt.Unknown -> false)
;;

let () =
  Printf.printf "omt_test: %d checks, %d failures\n" !checks !failures;
  if !failures <> 0 then exit 1
;;
