(* Consumer BST acceptance gate (bugreport 03 followup 3): the flagship vox2 deployment
   workload — 49 functional-correctness VCs over a BST datatype whose defining equations
   use SMT-LIB [match] expressions. All 49 are expected [unsat] (discharged proof
   obligations, z3-verified). The frontier RED is bst-r7-008 (`member query empty =
   false`, a Boolean arity-2 application defined by a datatype [match]); it and the 38 VCs
   fail-fast behind it were blocked by the parser rejecting [match]. The fix desugars
   [match] to testers + selectors + [ite] (parser.ml read_match).

   Drives every [<id>.smt2] through the SHARED loader (the exact CLI path) and asserts the
   verdict equals the sidecar's [expected]. Corpus dir is GLOBBED; an ABSENT dir is a
   clean skip (the consumer's corpus is untracked — the tracked fixture under
   tests/dtlia-bst-corpus is the default gating copy). *)

module Session = Oxsmt_interface.Session
module Parser = Oxsmt_smtlib_parser.Parser

let failures = ref 0

let vstr = function
  | Session.Sat -> "sat"
  | Session.Unsat -> "unsat"
  | Session.Unknown -> "unknown"
;;

let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s
;;

(* Extract the sidecar's ["expected": "<verdict>"] value with a tiny stdlib-only scan:
   find the ["expected"] key, skip to the value string, read the bare verdict token. *)
let expected_of_sidecar src =
  let n = String.length src in
  let key = "\"expected\"" in
  let klen = String.length key in
  let rec find_key i =
    if i + klen > n
    then None
    else if String.sub src i klen = key
    then read_value (i + klen)
    else find_key (i + 1)
  and read_value i =
    if i >= n
    then None
    else if src.[i] = '"'
    then read_token (i + 1) (Buffer.create 8)
    else read_value (i + 1)
  and read_token i buf =
    if i >= n
    then None
    else if src.[i] = '"'
    then Some (Buffer.contents buf)
    else (
      Buffer.add_char buf src.[i];
      read_token (i + 1) buf)
  in
  find_key 0
;;

let solve smt2_path =
  let src = read_file smt2_path in
  let s = Session.create () in
  match
    Parser.parse_into
      ~internal_mint:(Session.parse_minter s)
      (Session.env s)
      (Session.context s)
      src
  with
  | exception _ -> None
  | parsed ->
    if Oxsmt_query_loader.assert_all ~presolve:true s parsed
    then Some (Session.check_sat s)
    else None
;;

let solve_src src =
  let s = Session.create () in
  match
    Parser.parse_into
      ~internal_mint:(Session.parse_minter s)
      (Session.env s)
      (Session.context s)
      src
  with
  | exception _ -> None
  | parsed ->
    if Oxsmt_query_loader.assert_all ~presolve:true s parsed
    then Some (Session.check_sat s)
    else None
;;

let expect_src name src want =
  match solve_src src with
  | Some v when v = want -> Printf.printf "  ok   %s: %s\n%!" name (vstr v)
  | Some v ->
    incr failures;
    Printf.printf "  FAIL %s: got %s, want %s\n%!" name (vstr v) (vstr want)
  | None ->
    incr failures;
    Printf.printf "  FAIL %s: load/parse failed\n%!" name
;;

(* Soundness: the desugaring must never yield a wrong [unsat]. [unsat] fails; sat/unknown
   ok. *)
let expect_not_unsat name src =
  match solve_src src with
  | Some Session.Unsat ->
    incr failures;
    Printf.printf "  FAIL %s: got unsat (wrong desugaring)\n%!" name
  | Some v -> Printf.printf "  ok   %s: %s (not unsat)\n%!" name (vstr v)
  | None ->
    incr failures;
    Printf.printf "  FAIL %s: load/parse failed\n%!" name
;;

(* Soundness: must never be a wrong [unsat]; a fail-closed parse rejection ([None]) is an
   ACCEPTABLE sound outcome here (unlike {!expect_not_unsat}). *)
let expect_no_wrong_unsat name src =
  match solve_src src with
  | Some Session.Unsat ->
    incr failures;
    Printf.printf "  FAIL %s: got unsat (wrong)\n%!" name
  | Some v -> Printf.printf "  ok   %s: %s\n%!" name (vstr v)
  | None -> Printf.printf "  ok   %s: rejected (fail-closed)\n%!" name
;;

(* Match-desugaring discrimination: the all-unsat corpus does not exercise branch
   selection or the SAT direction, so a wrong desugaring (picking the wrong arm, or a
   wrong selector binding) could still pass the corpus while producing a wrong verdict.
   These pin it. *)
let run_match_discrimination () =
  Printf.printf "match-desugaring discrimination:\n%!";
  let tree =
    "(declare-datatypes ((Tree 0)) (((Node (l Tree) (k Int) (r Tree)) (Empty))))\n"
  in
  (* Node arm: match (Node _ 5 _) selects k=5; wrong arm (Empty->0) would flip to sat. *)
  expect_src
    "match Node arm binds k"
    (tree
     ^ "(assert (not (= (match (Node Empty 5 Empty) ((Empty 0) ((Node l k r) k))) 5)))\n\
        (check-sat)\n")
    Session.Unsat;
  (* Empty arm: match Empty selects the Empty body; wrong arm would flip. *)
  expect_src
    "match Empty arm"
    (tree
     ^ "(assert (not (= (match Empty ((Empty 7) ((Node l k r) k))) 7)))\n(check-sat)\n")
    Session.Unsat;
  (* Nested-selector binding: match binds l/r and reads a nested key. *)
  expect_src
    "match binds subtree selector"
    (tree
     ^ "(assert (not (= (match (Node (Node Empty 3 Empty) 5 Empty) ((Empty 0) ((Node l k \
        r) (match l ((Empty 0) ((Node ll lk lr) lk)))))) 3)))\n\
        (check-sat)\n")
    Session.Unsat;
  (* Wildcard default arm. *)
  expect_src
    "match wildcard default"
    (tree
     ^ "(assert (not (= (match (Node Empty 9 Empty) ((Empty 0) (_ 2))) 2)))\n\
        (check-sat)\n")
    Session.Unsat;
  (* SAT direction: a free scrutinee makes the match value unconstrained; a wrong
     desugaring that collapses to one arm would wrongly refute. Must never be unsat. *)
  expect_not_unsat
    "match SAT direction (free scrutinee)"
    (tree
     ^ "(declare-const t Tree)\n\
        (assert (= (match t ((Empty 0) ((Node l k r) k))) 5))\n\
        (check-sat)\n");
  (* Non-exhaustive match (missing Node, no default): must be rejected fail-closed, never
     a wrong verdict from silently using the last arm for the uncovered constructor. *)
  expect_no_wrong_unsat
    "non-exhaustive match fail-closed"
    (tree ^ "(declare-const t Tree)\n(assert (= (match t ((Empty 0))) 0))\n(check-sat)\n")
;;

let () =
  run_match_discrimination ();
  let dir =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "tests/dtlia-bst-corpus"
  in
  if not (Sys.file_exists dir && Sys.is_directory dir)
  then (
    Printf.printf "dtlia-bst gate: SKIP (corpus dir %s absent)\n%!" dir;
    exit 0);
  let smt2 =
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".smt2")
    |> List.sort String.compare
  in
  if smt2 = []
  then (
    Printf.printf "dtlia-bst gate: SKIP (no .smt2 in %s)\n%!" dir;
    exit 0);
  Printf.printf "dtlia-bst gate: %d VC(s) in %s\n%!" (List.length smt2) dir;
  let unsat = ref 0 in
  List.iter
    (fun f ->
      let id = Filename.remove_extension f in
      let smt2_path = Filename.concat dir f in
      let json_path = Filename.concat dir (id ^ ".json") in
      let expected =
        if Sys.file_exists json_path
        then expected_of_sidecar (read_file json_path)
        else None
      in
      match expected with
      | None ->
        incr failures;
        Printf.printf "  FAIL %s: missing/unreadable sidecar expected\n%!" id
      | Some exp ->
        (match solve smt2_path with
         | None ->
           incr failures;
           Printf.printf "  FAIL %s: load/parse failed (expected %s)\n%!" id exp
         | Some v ->
           let got = vstr v in
           if String.equal got exp
           then (
             if got = "unsat" then incr unsat;
             ())
           else (
             incr failures;
             Printf.printf "  FAIL %s: got %s, want %s\n%!" id got exp)))
    smt2;
  (* The frontier RED must be present and decided. *)
  if not (List.mem "bst-r7-008.smt2" smt2)
  then Printf.printf "  WARN frontier RED bst-r7-008 absent from corpus\n%!";
  Printf.printf
    "dtlia-bst gate: %d/%d unsat, %d failure(s)\n%!"
    !unsat
    (List.length smt2)
    !failures;
  if !failures > 0
  then (
    Printf.printf "dtlia-bst gate: FAILED\n%!";
    exit 1)
  else Printf.printf "dtlia-bst gate: all VCs match expected\n%!"
;;
