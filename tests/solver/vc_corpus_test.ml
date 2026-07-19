(* Live-consumer VC-corpus regression suite (bugreport 05: vc-corpus-channel).

   The vox2 refinement-type verifier ships real verification conditions as self-contained
   .smt2 files in the [check-sat-assuming] + [get-unsat-core] form, each with a JSON
   sidecar recording the expected verdict, the selector->fact map, vox2's computed core,
   and a Z3-computed [reference_core] tagged [minimum_cardinality] or
   [subset_minimal_certified]. All shipped VCs are expected [unsat] (a discharged proof
   obligation).

   This driver, for every [<id>.smt2] in the corpus dir:
   1. parses the declarations + assertions into a fresh {!Session} (the
      [check-sat- assuming] command is stripped and its selector list captured — the
      parser's batch reader has no assumption command, so we drive the in-process
      {!Session.check_sat_assuming} API directly, which is exactly the consumer's path);
   2. asserts through the SHARED {!Oxsmt_query_loader} (the same path oxsmt_cli uses, so
      the corpus cannot diverge from the CLI on how a document is loaded);
   3. calls {!Session.check_sat_assuming} with the captured selectors as assumptions and
      FAILS LOUDLY (nonzero exit) if the verdict is anything but the sidecar's
      [expected_verdict];
   4. compares our returned unsat core against the sidecar's [reference_core] and
      classifies the delta (subset-minimality defect / cardinality gap / parse-declare
      failure), emitting a per-VC delta line.

   The corpus dir is GLOBBED (never a hardcoded file list) so append-mostly future drops
   are picked up with no code change, and an ABSENT dir is a clean skip (exit 0) — the
   consumer's [bugreports/] tree is untracked, so a fresh checkout may lack it and the
   tracked fixture copy under tests/vc-corpus is the default gating set.

   Test-only: links the test-only SMT-LIB parser + the shared loader, never shipped. *)

module Session = Oxsmt_interface.Session
module Sexp = Oxsmt_smtlib_parser.Sexp
module Parser = Oxsmt_smtlib_parser.Parser
module Term = Oxsmt_core.Term
module Context = Oxsmt_core.Context
module Sort = Oxsmt_core.Sort

(* ------------------------------------------------------------------ *)
(* Minimal JSON reader (stdlib-only; the smt/ subproject links no JSON library and this
   driver honours the same I3 stdlib-only discipline). Recursive-descent over the small,
   regular sidecar/manifest grammar: objects, arrays, strings, numbers, bool, null. We
   only navigate a handful of fields, but parse the whole value so a malformed sidecar is
   a LOUD error rather than a silent mis-read. *)

type json =
  | JNull
  | JBool of bool
  | JNum of string
  | JStr of string
  | JArr of json list
  | JObj of (string * json) list

exception Json_error of string

let parse_json (src : string) : json =
  let n = String.length src in
  let pos = ref 0 in
  let peek () = if !pos < n then src.[!pos] else '\000' in
  let advance () = incr pos in
  let error msg = raise (Json_error (Printf.sprintf "%s at offset %d" msg !pos)) in
  let rec skip_ws () =
    if !pos < n
    then (
      match src.[!pos] with
      | ' ' | '\t' | '\n' | '\r' ->
        advance ();
        skip_ws ()
      | _ -> ())
  in
  let expect c =
    if peek () = c then advance () else error (Printf.sprintf "expected %c" c)
  in
  let parse_string () =
    expect '"';
    let b = Buffer.create 32 in
    let rec loop () =
      if !pos >= n
      then error "unterminated string"
      else (
        let c = src.[!pos] in
        advance ();
        match c with
        | '"' -> ()
        | '\\' ->
          let e = peek () in
          advance ();
          (match e with
           | '"' -> Buffer.add_char b '"'
           | '\\' -> Buffer.add_char b '\\'
           | '/' -> Buffer.add_char b '/'
           | 'n' -> Buffer.add_char b '\n'
           | 't' -> Buffer.add_char b '\t'
           | 'r' -> Buffer.add_char b '\r'
           | 'b' -> Buffer.add_char b '\b'
           | 'f' -> Buffer.add_char b '\012'
           | 'u' ->
             (* keep the escape verbatim; the sidecar identifiers we read are plain ASCII *)
             Buffer.add_string b "\\u"
           | other -> Buffer.add_char b other);
          loop ()
        | _ ->
          Buffer.add_char b c;
          loop ())
    in
    loop ();
    Buffer.contents b
  in
  let rec parse_value () =
    skip_ws ();
    match peek () with
    | '"' -> JStr (parse_string ())
    | '{' -> parse_object ()
    | '[' -> parse_array ()
    | 't' ->
      expect_lit "true";
      JBool true
    | 'f' ->
      expect_lit "false";
      JBool false
    | 'n' ->
      expect_lit "null";
      JNull
    | c when c = '-' || (c >= '0' && c <= '9') -> parse_number ()
    | _ -> error "unexpected character"
  and expect_lit s = String.iter (fun c -> expect c) s
  and parse_number () =
    let start = !pos in
    let is_num_char c =
      (c >= '0' && c <= '9') || c = '-' || c = '+' || c = '.' || c = 'e' || c = 'E'
    in
    while !pos < n && is_num_char src.[!pos] do
      advance ()
    done;
    JNum (String.sub src start (!pos - start))
  and parse_object () =
    expect '{';
    skip_ws ();
    if peek () = '}'
    then (
      advance ();
      JObj [])
    else (
      let members = ref [] in
      let rec loop () =
        skip_ws ();
        let key = parse_string () in
        skip_ws ();
        expect ':';
        let v = parse_value () in
        members := (key, v) :: !members;
        skip_ws ();
        match peek () with
        | ',' ->
          advance ();
          loop ()
        | '}' -> advance ()
        | _ -> error "expected , or }"
      in
      loop ();
      JObj (List.rev !members))
  and parse_array () =
    expect '[';
    skip_ws ();
    if peek () = ']'
    then (
      advance ();
      JArr [])
    else (
      let elts = ref [] in
      let rec loop () =
        let v = parse_value () in
        elts := v :: !elts;
        skip_ws ();
        match peek () with
        | ',' ->
          advance ();
          loop ()
        | ']' -> advance ()
        | _ -> error "expected , or ]"
      in
      loop ();
      JArr (List.rev !elts))
  in
  let v = parse_value () in
  skip_ws ();
  if !pos <> n then error "trailing content after JSON value";
  v
;;

let jmember key = function
  | JObj members -> List.assoc_opt key members
  | _ -> None
;;

let jstring = function
  | JStr s -> Some s
  | _ -> None
;;

(* [reference_core.selectors] / [vox2_computed_core.selectors]: a JSON array of strings. *)
let jstring_list = function
  | JArr xs -> Some (List.filter_map jstring xs)
  | _ -> None
;;

(* ------------------------------------------------------------------ *)
(* Corpus VC: the parsed pieces of one [<id>.smt2] + its [<id>.json] sidecar. *)

let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s
;;

(* Extract the [check-sat-assuming (a b ...)] selector name list, and return the sexp
   forest with that command removed. The parser's batch reader rejects
   [check-sat-assuming] as an unsupported command, so we strip it here and drive the
   in-process assumption API with the captured names. There is exactly one such command in
   every corpus VC; we take the first and drop all check-sat-assuming commands. *)
let split_assumptions (sexps : Sexp.t list) : string list * Sexp.t list =
  let assumptions = ref None in
  let kept =
    List.filter
      (fun sx ->
        match sx with
        | Sexp.List (head :: rest) ->
          (match Sexp.simple head with
           | Some "check-sat-assuming" ->
             if !assumptions = None
             then (
               let names =
                 match rest with
                 | [ Sexp.List sels ] -> List.filter_map Sexp.symbol_name sels
                 | _ -> []
               in
               assumptions := Some names);
             false
           | _ -> true)
        | _ -> true)
      sexps
  in
  Option.value ~default:[] !assumptions, kept
;;

let verdict_string = function
  | Session.Sat -> "sat"
  | Session.Unsat -> "unsat"
  | Session.Unknown -> "unknown"
;;

(* Classification of one VC's outcome. HARD failures (fail the suite, nonzero exit):
   [Verdict_mismatch] (the primary gate), [Load_failed] (we owe the consumer a decision on
   a discharged obligation and could not produce one), and [Core_not_minimal] (protocol
   class (a): a deletion-probe found a redundant core member — a genuine subset-minimality
   defect that would poison a consumer). SOFT signals (reported, never fail): the
   reference comparison — a same-cardinality-but-different minimal core is equally valid,
   and a cardinality gap vs a minimum_cardinality reference is protocol class (b) quality
   data. *)
type outcome =
  | Verdict_mismatch of string (* got — HARD *)
  | Load_failed (* HARD (protocol class (c) surfaced as a missed obligation) *)
  | Core_not_minimal of string (* HARD — deletion-probe found a redundant member *)
  | Core_match (* our core = reference core (set-equal) *)
  | Core_alt_minimal (* subset-minimal, same cardinality as ref, different members *)
  | Cardinality_gap of int (* |ours| - |ref| > 0 vs a minimum_cardinality reference *)
  | Core_smaller_than_ref of int (* |ours| < |ref| under a min_cardinality tag (WARN) *)
  | Core_unavailable (* unsat but core declined, or verdict not unsat *)

type result =
  { id : string
  ; expected : string
  ; got : string
  ; our_core : string list
  ; ref_core : string list
  ; ref_class : string
  ; outcome : outcome
  }

(* Load a FRESH session from [smt2_path] (the check-sat-assuming command stripped) and run
   {!Session.check_sat_assuming} assuming exactly the selector names in [assume] (in
   order, all polarity [true]). Returns [None] on a parse/load failure, else the verdict
   and the returned core mapped back to selector names ([None] core unless the verdict is
   unsat). A fresh session per call keeps the deletion-probes independent — the assumption
   API is scoped to one query and we never mutate a session across checks. *)
let solve_file ~smt2_path ~assume : (Session.verdict * string list option) option =
  let src = read_file smt2_path in
  let sexps =
    match Sexp.parse_many src with
    | s -> s
    | exception _ -> []
  in
  let _sel_names, kept = split_assumptions sexps in
  let s = Session.create () in
  let parsed =
    try
      Some
        (Parser.parse_into_sexps
           ~internal_mint:(Session.parse_minter s)
           (Session.env s)
           (Session.context s)
           kept)
    with
    | _ -> None
  in
  match parsed with
  | None -> None
  | Some parsed ->
    if not (Oxsmt_query_loader.assert_all ~presolve:true s parsed)
    then None
    else (
      (* Re-declaring an already-parsed Bool const is idempotent (Env re-declare returns
         the interned symbol at the same rank); the rebuilt term is hash-cons-identical to
         the one in the parsed assertions, so it is a genuine atom in this context. *)
      let ctx = Session.context s in
      let named =
        List.map
          (fun name -> Context.const ctx (Session.declare_const s name Sort.bool), name)
          assume
      in
      let assumptions = List.map (fun (t, _) -> t, true) named in
      let { Session.verdict; unsat_core } = Session.check_sat_assuming s assumptions in
      let name_of_term t =
        match List.find_opt (fun (t', _) -> Term.equal t t') named with
        | Some (_, name) -> name
        | None -> "?"
      in
      let core = Option.map (List.map (fun (t, _pol) -> name_of_term t)) unsat_core in
      Some (verdict, core))
;;

(* Deletion-probe the returned core for subset-minimality (protocol class (a)): for each
   member [m], re-solve a FRESH session assuming [core \ {m}]. If that remains [Unsat],
   [m] was redundant, so the core is NOT subset-minimal — a defect. Returns the list of
   redundant members (empty = minimal / minimality confirmed). A probe that declines
   ([Unknown]) cannot certify redundancy, so it is treated as "necessary" (no false
   defect). The active-assertion set is identical across probes because we re-load the
   same file each time. *)
let redundant_members ~smt2_path ~core : string list =
  List.filter
    (fun m ->
      let without = List.filter (fun x -> x <> m) core in
      match solve_file ~smt2_path ~assume:without with
      | Some (Session.Unsat, _) -> true (* m not needed -> redundant *)
      | _ -> false)
    core
;;

(* Run one VC end-to-end. Raises nothing structural (a load failure is an outcome, not an
   exception, so one bad VC does not abort the sweep); [Json_error] from the sidecar
   propagates to the caller, which reports it. *)
let run_vc ~smt2_path ~json_path : result =
  let id = Filename.remove_extension (Filename.basename smt2_path) in
  let sidecar = parse_json (read_file json_path) in
  let expected =
    match jmember "expected_verdict" sidecar |> Option.map jstring with
    | Some (Some s) -> s
    | _ -> raise (Json_error (Printf.sprintf "%s: no expected_verdict" id))
  in
  let ref_core, ref_class =
    match jmember "reference_core" sidecar with
    | Some rc ->
      let sels =
        Option.value
          ~default:[]
          (jmember "selectors" rc |> Option.map jstring_list |> Option.join)
      in
      let cls =
        match jmember "classification" rc |> Option.map jstring with
        | Some (Some s) -> s
        | _ -> "unclassified"
      in
      sels, cls
    | None -> [], "unclassified"
  in
  let src = read_file smt2_path in
  let sel_names, _kept =
    split_assumptions
      (match Sexp.parse_many src with
       | s -> s
       | exception _ -> [])
  in
  let base ?(got = "unknown") ?(our_core = []) outcome =
    { id; expected; got; our_core; ref_core; ref_class; outcome }
  in
  match solve_file ~smt2_path ~assume:sel_names with
  | None -> base Load_failed
  | Some (verdict, core_opt) ->
    let got = verdict_string verdict in
    let our_core = Option.value ~default:[] core_opt in
    if got <> expected
    then base ~got ~our_core (Verdict_mismatch got)
    else (
      match verdict, core_opt with
      | Session.Unsat, Some our_core ->
        (* Protocol (a): a redundant member = subset-minimality defect (HARD). *)
        (match redundant_members ~smt2_path ~core:our_core with
         | _ :: _ as redundant ->
           base
             ~got
             ~our_core
             (Core_not_minimal
                (Printf.sprintf
                   "redundant members {%s} in core {%s}"
                   (String.concat "," redundant)
                   (String.concat "," our_core)))
         | [] ->
           (* Minimality confirmed. Compare to the reference for the quality delta. *)
           let sset = List.sort_uniq String.compare in
           let ours = sset our_core in
           let refs = sset ref_core in
           let delta = List.length ours - List.length refs in
           let outcome =
             if ours = refs
             then Core_match
             else if delta = 0
             then Core_alt_minimal
             else if delta > 0
             then Cardinality_gap delta
             else Core_smaller_than_ref (-delta)
           in
           base ~got ~our_core outcome)
      | Session.Unsat, None -> base ~got Core_unavailable
      | _ -> base ~got ~our_core Core_unavailable)
;;

(* ------------------------------------------------------------------ *)

let read_manifest_revisions dir =
  let path = Filename.concat dir "manifest.json" in
  if not (Sys.file_exists path)
  then None
  else (
    match parse_json (read_file path) with
    | m ->
      let compiler_rev =
        match jmember "compiler" m with
        | Some c ->
          (match jmember "revision" c |> Option.map jstring with
           | Some (Some s) -> s
           | _ -> "?")
        | None -> "?"
      in
      let corpus_rev =
        match jmember "corpus_revision" m |> Option.map jstring with
        | Some (Some s) -> s
        | _ -> "?"
      in
      Some (compiler_rev, corpus_rev)
    | exception Json_error msg ->
      Printf.printf "vc-corpus: WARN manifest.json unreadable: %s\n" msg;
      None)
;;

let () =
  let dir =
    match Array.to_list Sys.argv with
    | _ :: d :: _ -> d
    | _ ->
      prerr_endline "vc_corpus_test: expected a corpus directory argument";
      exit 2
  in
  (* Absence = clean skip (exit 0): the consumer's bugreports/ tree is untracked, so a
     checkout without the corpus must not fail the suite. *)
  if not (Sys.file_exists dir && Sys.is_directory dir)
  then (
    Printf.printf "vc-corpus: SKIP (corpus dir %s absent)\n" dir;
    exit 0);
  let smt2_files =
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".smt2")
    |> List.sort String.compare
  in
  if smt2_files = []
  then (
    Printf.printf "vc-corpus: SKIP (no .smt2 files in %s)\n" dir;
    exit 0);
  (match read_manifest_revisions dir with
   | Some (compiler_rev, corpus_rev) ->
     Printf.printf
       "vc-corpus: manifest compiler.revision=%s corpus_revision=%s\n"
       compiler_rev
       corpus_rev
   | None -> Printf.printf "vc-corpus: (no manifest.json; revisions unattributable)\n");
  Printf.printf "vc-corpus: %d VC(s) in %s\n" (List.length smt2_files) dir;
  let results =
    List.map
      (fun smt2 ->
        let id = Filename.remove_extension smt2 in
        let smt2_path = Filename.concat dir smt2 in
        let json_path = Filename.concat dir (id ^ ".json") in
        if not (Sys.file_exists json_path)
        then (
          Printf.printf "vc-corpus: FAIL %s: missing sidecar %s.json\n" id id;
          { id
          ; expected = "?"
          ; got = "?"
          ; our_core = []
          ; ref_core = []
          ; ref_class = "?"
          ; outcome = Load_failed
          })
        else (
          match run_vc ~smt2_path ~json_path with
          | r -> r
          | exception Json_error msg ->
            Printf.printf "vc-corpus: FAIL %s: sidecar parse error: %s\n" id msg;
            { id
            ; expected = "?"
            ; got = "?"
            ; our_core = []
            ; ref_core = []
            ; ref_class = "?"
            ; outcome = Load_failed
            }))
      smt2_files
  in
  (* Verdict gate: any expected-unsat (or any expected verdict) that we did not match is a
     LOUD hard failure. A load failure on an expected-unsat is also a failure (we owe the
     consumer a decision on a discharged obligation). *)
  let verdict_failures = ref 0 in
  Printf.printf "\n== verdict gate ==\n";
  List.iter
    (fun r ->
      match r.outcome with
      | Verdict_mismatch got ->
        incr verdict_failures;
        Printf.printf "vc-corpus: FAIL %s: expected %s, got %s\n" r.id r.expected got
      | Load_failed ->
        incr verdict_failures;
        Printf.printf
          "vc-corpus: FAIL %s: could not load/decide (expected %s)\n"
          r.id
          r.expected
      | _ -> Printf.printf "vc-corpus: OK %s -> %s\n" r.id r.got)
    results;
  (* Core-comparison delta table (quality data; does NOT fail the suite unless a returned
     core is an actual subset-minimality DEFECT — a wrong core would poison a consumer). *)
  let core_defects = ref 0 in
  Printf.printf "\n== core delta table ==\n";
  Printf.printf
    "%-18s %-8s %-22s %-12s %-12s %s\n"
    "id"
    "verdict"
    "refclass"
    "our_core"
    "ref_core"
    "delta";
  List.iter
    (fun r ->
      let braces xs = if xs = [] then "{}" else "{" ^ String.concat "," xs ^ "}" in
      let our = braces r.our_core in
      let refc = braces r.ref_core in
      let delta =
        match r.outcome with
        | Core_match -> "= reference (minimality confirmed)"
        | Core_alt_minimal -> "alt-minimal (=card, subset-minimal)"
        | Cardinality_gap d -> Printf.sprintf "CARD-GAP +%d vs min-ref" d
        | Core_smaller_than_ref d ->
          Printf.sprintf "WARN smaller than min-ref by %d (ref tag suspect)" d
        | Core_not_minimal detail ->
          incr core_defects;
          "DEFECT " ^ detail
        | Core_unavailable -> "no core (verdict not unsat / declined)"
        | Verdict_mismatch got -> Printf.sprintf "n/a (verdict %s)" got
        | Load_failed -> "n/a (load failed)"
      in
      Printf.printf
        "%-18s %-8s %-22s %-12s %-12s %s\n"
        r.id
        r.got
        r.ref_class
        our
        refc
        delta)
    results;
  Printf.printf
    "\nvc-corpus: %d VC(s); %d verdict failure(s); %d core defect(s)\n"
    (List.length results)
    !verdict_failures
    !core_defects;
  if !verdict_failures > 0 || !core_defects > 0
  then (
    Printf.printf "vc-corpus: FAILED\n";
    exit 1)
  else Printf.printf "vc-corpus: PASSED\n"
;;
