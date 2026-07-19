(* Consumer proof for {!Oxsmt_interface.Interpolation}. The equality premise is left
   intact: A = {x = s+1, x <= 1}, B = {1 <= s}. Its lower-bound contribution must
   surface as a negative equation coefficient, including the equation's nonzero constant,
   and the checked interpolant is [s <= 0]. *)

open Oxsmt_core
module Session = Oxsmt_interface.Session
module Interpolation = Oxsmt_interface.Interpolation
module Rational = Oxsmt_lia.Rational

let checks = ref 0
let failures = ref 0

let check name condition =
  incr checks;
  if not condition
  then (
    incr failures;
    Printf.printf "FAIL %s\n" name)
;;

let expect_verdict name actual expected = check name (actual = expected)

let int_var session name =
  Context.const (Session.context session) (Session.declare_const session name Sort.int)
;;

let int session value = Context.int_const (Session.context session) value
let le session left right = Context.le (Session.context session) left right

let build side session =
  let context = Session.context session in
  let shared = int_var session "s" in
  let assertions =
    match side with
    | Interpolation.A ->
      let x = int_var session "x" in
      let shared_plus_one = Context.add context shared (int session 1) in
      [ Context.eq context x shared_plus_one; le session x (int session 1) ]
    | Interpolation.B -> [ le session (int session 1) shared ]
  in
  { Interpolation.assertions
  ; resolve =
      (function
       | "s" -> shared
       | name -> failwith ("resolver: non-shared variable " ^ name))
  }
;;

let candidate_term session resolve (candidate : string Interpolation.t) =
  let context = Session.context session in
  let linear =
    Context.linear_combination_big
      context
      (List.map
         (fun (name, coefficient) -> coefficient, resolve name)
         candidate.coefficients)
      candidate.constant
  in
  Context.le context linear (int session 0)
;;

let check_a_not_i candidate =
  let session = Session.create () in
  let replay = build Interpolation.A session in
  List.iter (Session.assert_term session) replay.assertions;
  Session.assert_term
    session
    (Context.not_
       (Session.context session)
       (candidate_term session replay.resolve candidate));
  Session.check_sat session
;;

let check_i_b candidate =
  let session = Session.create () in
  let replay = build Interpolation.B session in
  Session.assert_term session (candidate_term session replay.resolve candidate);
  List.iter (Session.assert_term session) replay.assertions;
  Session.check_sat session
;;

let verify candidate =
  Interpolation.verify
    ~create:Session.create
    ~build
    ~is_shared:(String.equal "s")
    candidate
;;

let () =
  let evidence = Session.create () in
  let context = Session.context evidence in
  let x = int_var evidence "x" in
  let shared = int_var evidence "s" in
  let equality = Context.eq context x (Context.add context shared (int evidence 1)) in
  let a_terms = [ equality; le evidence x (int evidence 1) ] in
  let b_term = le evidence (int evidence 1) shared in
  List.iter (Session.assert_term evidence) (a_terms @ [ b_term ]);
  expect_verdict "evidence is unsat" (Session.check_sat evidence) Session.Unsat;
  (match Session.last_farkas evidence with
   | None -> check "equality certificate is present" false
   | Some certificate ->
     let equality_coefficients =
       List.filter_map
         (fun (coefficient, (atom, _)) ->
            if Term.equal atom equality then Some coefficient else None)
         certificate
     in
     check "equality coefficient is present" (equality_coefficients <> []);
     check
       "equality coefficient has lower-bound sign"
       (List.exists
          (fun coefficient -> Rational.sign coefficient < 0)
          equality_coefficients));
  let created = ref 0 in
  let create () =
    incr created;
    Session.create ()
  in
  let candidate =
    Interpolation.interpolate
      evidence
      ~side_of:(fun (atom, _) ->
        if Term.equal atom b_term then Some Interpolation.B else Some Interpolation.A)
      ~project_shared:(fun variable ->
        if Term.equal variable shared then Some "s" else None)
      ~create
      ~build
      ~is_shared:(String.equal "s")
  in
  match candidate with
  | None -> check "checked interpolant is returned" false
  | Some candidate ->
    check "two fresh verification sessions" (!created = 2);
    check
      "interpolant is primitive s <= 0"
      (match candidate.coefficients with
       | [ (name, coefficient) ] ->
         String.equal name "s"
         && Bigint.equal coefficient Bigint.one
         && Bigint.is_zero candidate.constant
       | _ -> false);
    expect_verdict "A and not I is unsat" (check_a_not_i candidate) Session.Unsat;
    expect_verdict "I and B is unsat" (check_i_b candidate) Session.Unsat;
    check "public verifier accepts I" (verify candidate);
    check
      "shared-vocabulary exception is rejected"
      (not
         (Interpolation.verify
            ~create:Session.create
            ~build
            ~is_shared:(fun _ -> failwith "corrupt vocabulary checker")
            candidate));
    (* Weaken to [s <= 1]: A still entails it, but it intersects B at [s = 1]. *)
    let weak = { candidate with constant = Bigint.sub candidate.constant Bigint.one } in
    expect_verdict "weak I still follows from A" (check_a_not_i weak) Session.Unsat;
    expect_verdict "weak I intersects B" (check_i_b weak) Session.Sat;
    check "weak corruption is rejected" (not (verify weak));
    (* Strengthen to [s <= -1]: it still refutes B, but A admits [x = s = 0]. *)
    let strong = { candidate with constant = Bigint.add candidate.constant Bigint.one } in
    expect_verdict "strong I does not follow from A" (check_a_not_i strong) Session.Sat;
    expect_verdict "strong I still refutes B" (check_i_b strong) Session.Unsat;
    check "strong corruption is rejected" (not (verify strong));
    let corrupt_checks = ref 0 in
    let corrupt_create () =
      incr corrupt_checks;
      Session.create ()
    in
    ignore
      (Interpolation.verify
         ~create:corrupt_create
         ~build
         ~is_shared:(String.equal "s")
         strong);
    check "both checks run after first obligation rejects" (!corrupt_checks = 2);
    let foreign =
      { candidate with coefficients = ("x", Bigint.one) :: candidate.coefficients }
    in
    check "foreign vocabulary is rejected" (not (verify foreign))
;;

let () =
  if !failures = 0
  then Printf.printf "interpolation_test: all %d checks passed\n" !checks
  else (
    Printf.printf "interpolation_test: %d failure(s) / %d checks\n" !failures !checks;
    exit 1)
;;
