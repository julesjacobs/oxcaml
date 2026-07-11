(* The gate driver: certify .smt2 files against the Lean oracle, with the
   content-addressed cache and day-one honeypots (DESIGN.md §8, §10).

   Subcommands: gate selftest sha256 + sexp self-checks (no Lean) gate certify FILE.smt2
   certify one file, print its outcome gate run [opts] honeypots first, then the case
   corpus

   Digest to stdout; full per-query detail (.lean/.out) and a summary land under
   ../logs/gate-<timestamp>/ (AGENTS.md digest-first rule). *)

type ctx =
  { logdir : string
  ; lean_version : string
  ; timeout : float
  ; mutable counter : int (* uniquifies per-query log file names *)
  ; log : Buffer.t
  }

let logf ctx fmt = Printf.ksprintf (fun s -> Buffer.add_string ctx.log (s ^ "\n")) fmt

let uniq ctx tag =
  ctx.counter <- ctx.counter + 1;
  Printf.sprintf "%03d-%s" ctx.counter tag
;;

(* ---- Lean attempts ---- *)

type attempt =
  [ `Proved of string (* tactic that succeeded *)
  | `Gaveup of string
  | `Timeout
  | `Encode of string
  ]

let try_unsat ctx (q : Ast.query) : attempt =
  match Encoder.encode_unsat q with
  | exception Encoder.Encode_error m -> `Encode m
  | src ->
    (match
       Lean_runner.run
         ~logdir:ctx.logdir
         ~timeout:ctx.timeout
         ~tag:(uniq ctx "unsat")
         ~src
     with
     | Proved -> `Proved Encoder.grind_tactic
     | Timed_out -> `Timeout
     | Tactic_failed s -> `Gaveup s
     | Elab_error s -> `Encode s)
;;

let try_sat ctx (q : Ast.query) (m : Model.t) : attempt =
  let attempt tactic =
    match Encoder.encode_sat q m ~tactic with
    | exception Encoder.Encode_error msg -> `Encode msg
    | exception Model.Bad_model msg -> `Encode ("bad model: " ^ msg)
    | src ->
      (match
         Lean_runner.run
           ~logdir:ctx.logdir
           ~timeout:ctx.timeout
           ~tag:(uniq ctx ("sat-" ^ tactic))
           ~src
       with
       | Proved -> `Proved tactic
       | Timed_out -> `Timeout
       | Tactic_failed s -> `Gaveup s
       | Elab_error s -> `Encode s)
  in
  match attempt "decide" with
  | `Proved t -> `Proved t
  | `Gaveup _ -> attempt "native_decide" (* NOTES.md: decide can blow up; fall back *)
  | other -> other
;;

(* ---- certification per claim ---- *)

let refute_with_witness ctx q model_opt ~default : Outcome.t =
  match model_opt with
  | Some m ->
    (match try_sat ctx q m with
     | `Proved tac ->
       Outcome.Refuted
         (Printf.sprintf "assertions satisfiable, witness kernel-checked by %s" tac)
     | _ -> default)
  | None -> default
;;

let certify_unsat ctx q model_opt : Outcome.t =
  match try_unsat ctx q with
  | `Proved _ -> Certified
  | `Encode m -> Encode_error m
  | `Timeout ->
    refute_with_witness ctx q model_opt ~default:(Inconclusive "grind timed out")
  | `Gaveup s ->
    refute_with_witness ctx q model_opt ~default:(Inconclusive ("grind gave up: " ^ s))
;;

let certify_sat ctx q model_opt : Outcome.t =
  match model_opt with
  | None -> Inconclusive "sat claim but no .model sidecar supplied"
  | Some m ->
    (match try_sat ctx q m with
     | `Proved _ -> Certified
     | `Encode s -> Encode_error s
     | `Timeout -> Inconclusive "model check (decide) timed out"
     | `Gaveup s ->
       (* the supplied model did not check; is the query actually unsat? *)
       (match try_unsat ctx q with
        | `Proved _ -> Refuted "sat claim but assertions proved unsat by grind"
        | _ -> Inconclusive ("model did not check: " ^ s)))
;;

(* ---- file wrapper + cache ---- *)

let model_path smt2 = Filename.remove_extension smt2 ^ ".model"

let load_model smt2 : Model.t option * string =
  let p = model_path smt2 in
  if Sys.file_exists p
  then (
    let text = Lean_runner.read_file p in
    Some (Model.of_string text), text)
  else None, ""
;;

(* A timeout-driven Inconclusive must not be cached (DESIGN.md §8). *)
let should_cache = function
  | Outcome.Inconclusive d -> not (Lean_runner.substring_mem ~needle:"timed out" d)
  | _ -> true
;;

type disposition =
  [ `Hit
  | `Miss
  | `Uncached
  ]

(* Returns (outcome, disposition, used_divmod). [used_divmod] flags a query that contains
   [div]/[mod] (whose unsat direction goes through euclidean elimination); the run digest
   counts the div/mod cases that CERTIFY so the quarantined -> certified movement is
   visible. *)
let certify_file ctx ~cache_dir ~allow_cache smt2 : Outcome.t * disposition * bool =
  match Lean_runner.read_file smt2 with
  | exception e -> Encode_error (Printexc.to_string e), `Uncached, false
  | text ->
    (match Reader.of_string text with
     | exception Reader.Malformed m -> Malformed m, `Uncached, false
     | exception Reader.Unsupported m -> Unsupported m, `Uncached, false
     | exception Sexp.Malformed m -> Malformed ("sexp: " ^ m), `Uncached, false
     | q ->
       let dm = Elim.uses_divmod q in
       (* Preflight: reject an invalid (zero / non-literal) div/mod divisor BEFORE any
          encoding, in one place, so NEITHER direction can certify a divisor outside the
          theory the solver solves (the sat direction would otherwise let decide compute a
          variable-divisor model). Fail closed -> UNSUPPORTED quarantine. *)
       (match Elim.check_divisors q with
        | exception Reader.Unsupported m -> Unsupported m, `Uncached, dm
        | () ->
          (match q.status with
           | None -> No_status, `Uncached, dm
           | Some Ast.Unknown -> No_status, `Uncached, dm
           | Some claim ->
             (match load_model smt2 with
              | exception Model.Bad_model m ->
                Encode_error ("bad model: " ^ m), `Uncached, dm
              | model_opt, model_str ->
                let claim_str = Ast.verdict_to_string claim in
                let key =
                  Cache.compose
                    ~canonical:(Canonical.canonical_query q)
                    ~claim:claim_str
                    ~model:model_str
                    ~lean_version:ctx.lean_version
                in
                let cached =
                  if allow_cache then Cache.lookup ~dir:cache_dir key else None
                in
                (match cached with
                 | Some o -> o, `Hit, dm
                 | None ->
                   let o =
                     match claim with
                     | Ast.Unsat -> certify_unsat ctx q model_opt
                     | Ast.Sat -> certify_sat ctx q model_opt
                     | Ast.Unknown -> No_status
                   in
                   if allow_cache && should_cache o
                   then Cache.store ~dir:cache_dir key ~claim:claim_str o;
                   o, (if allow_cache then `Miss else `Uncached), dm)))))
;;

(* ---- corpus listing ---- *)

let smt2_files dir =
  if not (Sys.file_exists dir && Sys.is_directory dir)
  then []
  else
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".smt2")
    |> List.sort String.compare
    |> List.map (Filename.concat dir)
;;

(* ---- reporting ---- *)

let short p = Filename.basename p

let print_line ctx label file (o : Outcome.t) disp =
  let d =
    match disp with
    | `Hit -> "cache"
    | `Miss -> "lean"
    | `Uncached -> "lean"
  in
  let det = Outcome.detail o in
  let det = if String.length det > 80 then String.sub det 0 77 ^ "..." else det in
  logf ctx "  [%s] %-13s %-28s (%s) %s" label (Outcome.tag o) (short file) d det
;;

(* ---- run subcommand ---- *)

(* Provenance (task #133): the git HEAD of the checkout that runs the gate. All worktrees
   share ../logs, so without this a `make status` in one tree could read a gate log
   another tree produced and report a foreign/stale verdict as its own. Recording HEAD in
   the log-dir name lets status_gen match a log to the exact tree that produced it (and
   makes concurrent-worktree dirs distinct). Logging-side only — it never influences any
   certification decision. Best-effort: a missing/failed git yields "nohead", which
   status_gen treats as unmatched (loud absence), never a false match. *)
let provenance_head () =
  try
    let r, w = Unix.pipe () in
    let devnull = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
    let pid =
      Unix.create_process "git" [| "git"; "rev-parse"; "HEAD" |] Unix.stdin w devnull
    in
    Unix.close w;
    Unix.close devnull;
    let ic = Unix.in_channel_of_descr r in
    let line =
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
           try input_line ic with
           | End_of_file -> "")
    in
    ignore (Unix.waitpid [] pid);
    let h = String.trim line in
    (* a HEAD is 40 hex chars; anything else (empty, error text) -> nohead *)
    if
      String.length h = 40
      && String.for_all (fun c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) h
    then h
    else "nohead"
  with
  | _ -> "nohead"
;;

let run_dir_now logs_root =
  let t = Unix.localtime (Unix.time ()) in
  let stamp =
    Printf.sprintf
      "%04d%02d%02d-%02d%02d%02d"
      (t.tm_year + 1900)
      (t.tm_mon + 1)
      t.tm_mday
      t.tm_hour
      t.tm_min
      t.tm_sec
  in
  (* gate-<stamp>-<HEAD>: stamp keeps human/mtime ordering, trailing HEAD is the
     provenance component status_gen matches on. *)
  Filename.concat logs_root (Printf.sprintf "gate-%s-%s" stamp (provenance_head ()))
;;

(* A green gate that hasn't proven it can go red is unaudited (DESIGN.md §10), so the
   honeypot phase is not vacuously satisfiable: it enforces a hard floor and an exact
   expected outcome per honeypot. *)
let min_honeypots = 12
let expect_path smt2 = Filename.remove_extension smt2 ^ ".expect"

(* A honeypot's declared expectation must be a non-certifying outcome. CERTIFIED is
   explicitly rejected — a honeypot may never be expected to certify — as is any typo or
   outcome (ENCODE_ERROR/NO_STATUS) that would make the audit meaningless. *)
let valid_expect = [ "REFUTED"; "MALFORMED"; "UNSUPPORTED"; "INCONCLUSIVE" ]

let load_expect smt2 : string option =
  let p = expect_path smt2 in
  if Sys.file_exists p then Some (String.trim (Lean_runner.read_file p)) else None
;;

(* Returns (ok, breach messages, attestation line). Honeypots run with the cache DISABLED
   and are never cached (DESIGN.md §10). A breach is any of: too few honeypots present, a
   honeypot with no declared expected outcome, an invalid .expect value, a honeypot that
   got CERTIFIED, or a honeypot whose outcome differs from its .expect (so REFUTED
   degrading to INCONCLUSIVE turns the gate red rather than passing silently). *)
let honeypots_phase ctx ~cache_dir dir : bool * string list * string =
  let files = smt2_files dir in
  let total = List.length files in
  logf
    ctx
    "HONEYPOTS (%d; floor %d) — each must match its .expect and never CERTIFY:"
    total
    min_honeypots;
  let breaches = ref [] in
  let add f msg = breaches := Printf.sprintf "%s: %s" (short f) msg :: !breaches in
  if total < min_honeypots
  then
    breaches
    := [ Printf.sprintf
           "gate unaudited — only %d honeypot(s) present, need >= %d"
           total
           min_honeypots
       ];
  let matched = ref 0
  and certified = ref 0 in
  List.iter
    (fun f ->
       let o, disp, _dm = certify_file ctx ~cache_dir ~allow_cache:false f in
       print_line ctx "honeypot" f o disp;
       let certified_here = Outcome.is_certified o in
       if certified_here
       then (
         incr certified;
         add f "CERTIFIED (gate is broken)");
       match load_expect f with
       | None -> add f "missing .expect sidecar (expected outcome undeclared)"
       | Some exp when not (List.mem exp valid_expect) ->
         add
           f
           (Printf.sprintf
              "invalid .expect value %S (allowed: %s)"
              exp
              (String.concat "/" valid_expect))
       | Some exp ->
         let got = Outcome.tag o in
         if String.equal got exp && not certified_here
         then incr matched
         else if not (String.equal got exp)
         then add f (Printf.sprintf "expected %s but got %s" exp got))
    files;
  let breaches = List.rev !breaches in
  let attestation =
    Printf.sprintf
      "honeypots: %d/%d matched, floor %d, %s"
      !matched
      total
      min_honeypots
      (if !certified = 0
       then "none certified"
       else Printf.sprintf "%d CERTIFIED" !certified)
  in
  (match breaches with
   | [] -> logf ctx "  -> %s" attestation
   | bs ->
     logf ctx "  -> BREACH (%d): %s" (List.length bs) attestation;
     List.iter (fun b -> logf ctx "       %s" b) bs);
  breaches = [], breaches, attestation
;;

let cases_phase ctx ~cache_dir dir =
  let files = smt2_files dir in
  logf ctx "CASES (%d):" (List.length files);
  let tally = Hashtbl.create 8 in
  let bump t =
    Hashtbl.replace tally t (1 + Option.value (Hashtbl.find_opt tally t) ~default:0)
  in
  let hits = ref 0
  and misses = ref 0 in
  let ship_stoppers = ref []
  and encode_errors = ref []
  and quarantined = ref []
  and divmod_certified = ref 0 in
  List.iter
    (fun f ->
       let o, disp, dm = certify_file ctx ~cache_dir ~allow_cache:true f in
       (match disp with
        | `Hit -> incr hits
        | `Miss -> incr misses
        | `Uncached -> ());
       bump (Outcome.tag o);
       (* A div/mod case that now CERTIFIES is one the pre-elimination gate would have
         UNSUPPORTED-quarantined; count the movement so it is visible in the digest. *)
       if dm && Outcome.is_certified o then incr divmod_certified;
       if Outcome.is_ship_stopper o then ship_stoppers := (f, o) :: !ship_stoppers;
       (match o with
        | Encode_error _ -> encode_errors := (f, o) :: !encode_errors
        (* Quarantine = a query the gate could not judge (reader-rejected) or that carries
          no claim; visibly counted per-reason, never dropped. *)
        | Malformed _ | Unsupported _ | No_status -> quarantined := (f, o) :: !quarantined
        | _ -> ());
       print_line ctx "case" f o disp)
    files;
  ( tally
  , List.length files
  , !hits
  , !misses
  , List.rev !ship_stoppers
  , List.rev !encode_errors
  , List.rev !quarantined
  , !divmod_certified )
;;

let cmd_run ~cases_dir ~honeypots_dir ~cache_dir ~logs_root ~timeout ~allow_cache =
  let logdir = run_dir_now logs_root in
  Cache.mkdir_p logdir;
  let lean_version = Lean_runner.version () in
  let ctx = { logdir; lean_version; timeout; counter = 0; log = Buffer.create 4096 } in
  logf ctx "oxsmt gate run";
  logf ctx "  lean: %s" lean_version;
  logf ctx "  encoding: %s   grind: %s" Encoder.encoding_version Encoder.grind_config;
  logf ctx "  cache: %s (%s)" cache_dir (if allow_cache then "enabled" else "disabled");
  logf ctx "  logdir: %s" logdir;
  logf ctx "";
  (* Sweep any orphaned temp cache files (crashed writer) before touching the cache. *)
  if allow_cache
  then (
    let swept = Cache.sweep_orphan_temps cache_dir in
    if swept > 0 then logf ctx "  swept %d orphan temp file(s) from cache" swept);
  let honeypots_ok, hp_breaches, hp_attestation =
    honeypots_phase ctx ~cache_dir honeypots_dir
  in
  logf ctx "";
  (* If a honeypot certified, abort red BEFORE trusting any case result. *)
  let ( tally
      , n_cases
      , hits
      , misses
      , ship_stoppers
      , encode_errors
      , quarantined
      , divmod_certified )
    =
    if honeypots_ok
    then cases_phase ctx ~cache_dir cases_dir
    else Hashtbl.create 1, 0, 0, 0, [], [], [], 0
  in
  logf ctx "";
  (* Write full log. *)
  let logfile = Filename.concat logdir "gate.log" in
  Lean_runner.write_file logfile (Buffer.contents ctx.log);
  (* Digest to stdout. *)
  let get t = Option.value (Hashtbl.find_opt tally t) ~default:0 in
  (* Accounting invariant (author directive): every query exits in EXACTLY ONE class —
     certified / inconclusive / quarantined — plus the two RED terminal classes refuted
     (soundness breach) and encode_error (encoder bug). The sum of the per-class counts
     MUST equal the number of case files; a mismatch means a query was silently dropped
     and is itself RED. This makes silent bypass structurally impossible rather than fixed
     per-bug. *)
  let n_certified = get "CERTIFIED"
  and n_inconclusive = get "INCONCLUSIVE"
  and n_quarantined = get "MALFORMED" + get "UNSUPPORTED" + get "NO_STATUS"
  and n_refuted = get "REFUTED"
  and n_encode = get "ENCODE_ERROR" in
  let accounted = n_certified + n_inconclusive + n_quarantined + n_refuted + n_encode in
  let accounting_ok = accounted = n_cases in
  let green = honeypots_ok && ship_stoppers = [] && encode_errors = [] && accounting_ok in
  Printf.printf "oxsmt gate: %s\n" (if green then "GREEN" else "RED");
  (* Always attest the audit ran, green or red (DESIGN.md §10). *)
  Printf.printf "  %s\n" hp_attestation;
  if not honeypots_ok
  then (
    Printf.printf "  HONEYPOT BREACH (gate unaudited or broken):\n";
    List.iter (fun b -> Printf.printf "    %s\n" b) hp_breaches);
  Printf.printf
    "  cases: %d CERTIFIED, %d REFUTED, %d INCONCLUSIVE, %d ENCODE_ERROR, %d \
     UNSUPPORTED, %d MALFORMED, %d NO_STATUS\n"
    (get "CERTIFIED")
    (get "REFUTED")
    (get "INCONCLUSIVE")
    (get "ENCODE_ERROR")
    (get "UNSUPPORTED")
    (get "MALFORMED")
    (get "NO_STATUS");
  (* Accounting identity: inputs = certified + inconclusive + quarantined + refuted +
     encode_error. Every input query lands in exactly one class; the sum must close. *)
  Printf.printf
    "  accounting: %d inputs = %d certified + %d inconclusive + %d quarantined + %d \
     refuted + %d encode_error%s\n"
    n_cases
    n_certified
    n_inconclusive
    n_quarantined
    n_refuted
    n_encode
    (if accounting_ok
     then ""
     else Printf.sprintf "  [MISMATCH: %d unaccounted — RED]" (n_cases - accounted));
  (* Visible movement: div/mod cases that CERTIFY via euclidean elimination — these would
     have been UNSUPPORTED-quarantined before this feature. Part of the certified count in
     the accounting identity above (not a separate class). *)
  if divmod_certified > 0
  then
    Printf.printf
      "  divmod-eliminated: %d case(s) certified (were UNSUPPORTED-quarantined \
       pre-elimination)\n"
      divmod_certified;
  if quarantined <> []
  then (
    Printf.printf "  QUARANTINED (counted, not certified — no silent bypass):\n";
    List.iter
      (fun (f, o) ->
         Printf.printf "    %s [%s]: %s" (short f) (Outcome.tag o) (Outcome.detail o);
         print_newline ())
      quarantined);
  Printf.printf "  cache: %d hit, %d miss\n" hits misses;
  if ship_stoppers <> []
  then (
    Printf.printf "  SHIP-STOPPERS (REFUTED):\n";
    List.iter
      (fun (f, o) -> Printf.printf "    %s: %s\n" (short f) (Outcome.detail o))
      ship_stoppers);
  if encode_errors <> []
  then (
    Printf.printf "  ENCODE ERRORS:\n";
    List.iter
      (fun (f, o) -> Printf.printf "    %s: %s\n" (short f) (Outcome.detail o))
      encode_errors);
  Printf.printf "  full log: %s\n" logfile;
  if green then 0 else 1
;;

(* ---- certify subcommand (single file, verbose) ---- *)

let cmd_certify ~cache_dir ~logs_root ~timeout ~allow_cache file =
  let logdir = run_dir_now logs_root in
  Cache.mkdir_p logdir;
  let lean_version = Lean_runner.version () in
  let ctx = { logdir; lean_version; timeout; counter = 0; log = Buffer.create 1024 } in
  let o, disp, _dm = certify_file ctx ~cache_dir ~allow_cache file in
  Lean_runner.write_file (Filename.concat logdir "gate.log") (Buffer.contents ctx.log);
  let d =
    match disp with
    | `Hit -> "cache hit"
    | `Miss -> "lean (cached)"
    | `Uncached -> "lean"
  in
  Printf.printf "%s: %s [%s]\n" (short file) (Outcome.tag o) d;
  if Outcome.detail o <> "" then Printf.printf "  %s\n" (Outcome.detail o);
  Printf.printf "  logdir: %s\n" logdir;
  Outcome.exit_code o
;;

(* ---- selftest ---- *)

let cmd_selftest () =
  let ok = ref true in
  (match Sha256.selftest () with
   | Ok () -> print_endline "sha256: OK (FIPS 180-4 vectors)"
   | Error fs ->
     ok := false;
     print_endline "sha256: FAIL";
     List.iter (fun f -> print_endline ("  " ^ f)) fs);
  (* sexp round-trip *)
  let sample = "(a (b c) |quoted sym| 42) ; comment\n(assert (= x 1))" in
  (match Sexp.parse_many sample with
   | exception e ->
     ok := false;
     Printf.printf "sexp: FAIL (%s)\n" (Printexc.to_string e)
   | _ -> print_endline "sexp: OK");
  (* reader smoke + encoder smoke (no Lean) *)
  (match
     Reader.of_string
       "(set-logic QF_UFLIA)(declare-sort S 0)(declare-fun f (S) S)(declare-const a \
        S)(declare-const b S)(assert (= a b))(assert (distinct (f a) (f b)))(set-info \
        :status unsat)(check-sat)"
   with
   | exception e ->
     ok := false;
     Printf.printf "reader: FAIL (%s)\n" (Printexc.to_string e)
   | q ->
     (match Encoder.encode_unsat q with
      | exception e ->
        ok := false;
        Printf.printf "encoder: FAIL (%s)\n" (Printexc.to_string e)
      | src ->
        if
          Lean_runner.substring_mem ~needle:"by grind" src
          && Lean_runner.substring_mem ~needle:"open Classical" src
        then print_endline "reader+encoder: OK"
        else (
          ok := false;
          print_endline "encoder: FAIL (unexpected output)")));
  (* Canonical-key injectivity regression. The exploit a review found: a |quoted symbol|
     may contain spaces/newlines/parens, which the old space/paren-concatenated canonical
     form let forge token boundaries, so an unsat query (qA) and a satisfiable query (qB)
     collapsed to the same cache key and qB inherited qA's CERTIFIED verdict. These are
     the exact exhibits (mirrored in tests/gate/collision/); their canonical strings and
     cache keys MUST differ. See canonical.ml for the injectivity argument. *)
  let qa =
    "(set-logic QF_UFLIA)(declare-const a Bool)(declare-const |a\n\
     (not $a)| Bool)(set-info :status unsat)(assert a)(assert (not a))(check-sat)"
  in
  let qb =
    "(set-logic QF_UFLIA)(declare-const a Bool)(declare-const |a\n\
     (not $a)| Bool)(set-info :status unsat)(assert |a\n\
     (not $a)|)(check-sat)"
  in
  (match Reader.of_string qa, Reader.of_string qb with
   | exception e ->
     ok := false;
     Printf.printf "injectivity: FAIL to parse exhibits (%s)\n" (Printexc.to_string e)
   | qA, qB ->
     let ca = Canonical.canonical_query qA
     and cb = Canonical.canonical_query qB in
     let ka = Cache.compose ~canonical:ca ~claim:"unsat" ~model:"" ~lean_version:"t" in
     let kb = Cache.compose ~canonical:cb ~claim:"unsat" ~model:"" ~lean_version:"t" in
     if String.equal ca cb || String.equal ka.hash kb.hash
     then (
       ok := false;
       print_endline "injectivity: FAIL — qA and qB collide (cache-key non-injective)")
     else print_endline "injectivity: OK (qA/qB distinct canonical + keys)");
  (* ser must be self-delimiting: a symbol whose bytes mimic the framing or contain
     separators cannot collide with a different structure. *)
  let inj_pairs =
    [ Canonical.A "A1:x", Canonical.L [ Canonical.A "x" ]
    ; Canonical.A "a b", Canonical.L [ Canonical.A "a"; Canonical.A "b" ]
    ; Canonical.A "x)(y", Canonical.A "x) (y"
    ; Canonical.A "p\nq", Canonical.L [ Canonical.A "p"; Canonical.A "q" ]
    ]
  in
  if
    List.for_all
      (fun (x, y) -> not (String.equal (Canonical.ser x) (Canonical.ser y)))
      inj_pairs
  then print_endline "ser injectivity: OK (separators/framing bytes stay data)"
  else (
    ok := false;
    print_endline "ser injectivity: FAIL");
  (* Iff (Bool-sorted =) canonicalises under its own tag: distinct from a plain
     conjunction of the same atoms, and commutative (p<->q = q<->p). *)
  let canon s =
    try Some (Canonical.canonical_query (Reader.of_string s)) with
    | _ -> None
  in
  let decls = "(set-logic QF_UF)(declare-const p Bool)(declare-const q Bool)" in
  let status = "(set-info :status unsat)(check-sat)" in
  (match
     ( canon (decls ^ "(assert (= p q))" ^ status)
     , canon (decls ^ "(assert (= q p))" ^ status)
     , canon (decls ^ "(assert (and p q))" ^ status) )
   with
   | Some iff_pq, Some iff_qp, Some and_pq ->
     if String.equal iff_pq iff_qp && not (String.equal iff_pq and_pq)
     then print_endline "iff canonical: OK (own tag, commutative)"
     else (
       ok := false;
       print_endline "iff canonical: FAIL")
   | _ ->
     ok := false;
     print_endline "iff canonical: FAIL (parse)");
  (* Reader reject regressions (codex round-3). SMT-LIB (check-sat) takes no arguments and
     execution terminates at (exit); a reader that accepts (check-sat X) or folds commands
     after (exit) assembles a query the file never asked, CERTIFYING a false unsat. These
     are codex's two verbatim triggers; each MUST be rejected (Malformed), never read into
     a certifiable query. *)
  let rejects_malformed label src =
    match Reader.of_string src with
    | exception Reader.Malformed _ -> print_endline (label ^ ": OK (rejected MALFORMED)")
    | exception e ->
      ok := false;
      Printf.printf
        "%s: FAIL (raised %s, expected Malformed)\n"
        label
        (Printexc.to_string e)
    | _ ->
      ok := false;
      Printf.printf "%s: FAIL (read a query; must reject)\n" label
  in
  rejects_malformed
    "check-sat-args reject"
    "(set-logic QF_LIA) (set-info :status unsat) (assert false) (check-sat X)";
  rejects_malformed
    "post-exit reject"
    "(set-logic QF_LIA) (set-info :status unsat) (exit) (assert false) (check-sat)";
  (* div/mod elimination (Lean-free structural checks). A nonzero-literal divisor is
     eliminated: the transformed query has NO Div/Mod and DOES declare fresh .oxsmt.*
     witnesses; a zero / variable divisor fails the divisor preflight (UNSUPPORTED); a
     user name in the reserved namespace is rejected so it cannot capture a fresh witness. *)
  let dm_decls = "(set-logic QF_LIA)(declare-const x Int)(declare-const y Int)" in
  let read s =
    try Some (Reader.of_string s) with
    | _ -> None
  in
  (match
     read (dm_decls ^ "(assert (>= (mod x 4) 4))(set-info :status unsat)(check-sat)")
   with
   | Some q ->
     (match Elim.check_divisors q with
      | () ->
        let q' = Elim.eliminate q in
        let no_divmod = not (Elim.uses_divmod q') in
        let has_fresh =
          List.exists (fun (n, _, _) -> Reader.is_reserved_name n) q'.fun_decls
        in
        if Elim.uses_divmod q && no_divmod && has_fresh
        then print_endline "divmod elim: OK (mod eliminated to fresh .oxsmt witnesses)"
        else (
          ok := false;
          print_endline "divmod elim: FAIL (residual div/mod or no fresh witness)")
      | exception _ ->
        ok := false;
        print_endline "divmod elim: FAIL (literal divisor wrongly rejected)")
   | None ->
     ok := false;
     print_endline "divmod elim: FAIL (parse)");
  let rejects_unsupported label src =
    match read src with
    | Some q ->
      (match Elim.check_divisors q with
       | () ->
         ok := false;
         Printf.printf "%s: FAIL (divisor accepted; must reject)\n" label
       | exception Reader.Unsupported _ ->
         print_endline (label ^ ": OK (rejected UNSUPPORTED)")
       | exception e ->
         ok := false;
         Printf.printf "%s: FAIL (raised %s)\n" label (Printexc.to_string e))
    | None ->
      ok := false;
      Printf.printf "%s: FAIL (parse)\n" label
  in
  rejects_unsupported
    "divmod zero-divisor reject"
    (dm_decls ^ "(assert (= (div x 0) 0))(set-info :status unsat)(check-sat)");
  rejects_unsupported
    "divmod variable-divisor reject"
    (dm_decls ^ "(assert (= (mod x y) 0))(set-info :status unsat)(check-sat)");
  rejects_malformed
    "reserved-name reject"
    "(set-logic QF_LIA)(declare-const |.oxsmt.q.0| Int)(assert (>= |.oxsmt.q.0| \
     0))(set-info :status unsat)(check-sat)";
  if !ok then 0 else 1
;;

(* ---- argument parsing ---- *)

let usage () =
  prerr_endline
    "usage:\n\
    \  gate selftest\n\
    \  gate certify FILE.smt2 [--no-cache] [--timeout SECS]\n\
    \  gate run [--cases DIR] [--honeypots DIR] [--cache DIR] [--logs DIR] [--no-cache] \
     [--timeout SECS]";
  2
;;

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  let cache_dir = ref (Cache.default_dir ()) in
  let logs_root =
    ref
      (match Sys.getenv_opt "OXSMT_LOGS" with
       | Some d -> d
       | None -> "/usr/local/home/jujacobs/oxsmt/logs")
  in
  let cases_dir = ref "tests/cases" in
  let honeypots_dir = ref "tests/gate/honeypots" in
  let timeout = ref 30.0 in
  let allow_cache = ref true in
  let rec parse_opts = function
    | [] -> ()
    | "--no-cache" :: tl ->
      allow_cache := false;
      parse_opts tl
    | "--cache" :: d :: tl ->
      cache_dir := d;
      parse_opts tl
    | "--logs" :: d :: tl ->
      logs_root := d;
      parse_opts tl
    | "--cases" :: d :: tl ->
      cases_dir := d;
      parse_opts tl
    | "--honeypots" :: d :: tl ->
      honeypots_dir := d;
      parse_opts tl
    | "--timeout" :: s :: tl ->
      timeout := float_of_string s;
      parse_opts tl
    | other :: _ ->
      Printf.eprintf "unknown option: %s\n" other;
      exit (usage ())
  in
  let code =
    match args with
    | "selftest" :: _ -> cmd_selftest ()
    | "certify" :: file :: rest ->
      parse_opts rest;
      cmd_certify
        ~cache_dir:!cache_dir
        ~logs_root:!logs_root
        ~timeout:!timeout
        ~allow_cache:!allow_cache
        file
    | "run" :: rest ->
      parse_opts rest;
      cmd_run
        ~cases_dir:!cases_dir
        ~honeypots_dir:!honeypots_dir
        ~cache_dir:!cache_dir
        ~logs_root:!logs_root
        ~timeout:!timeout
        ~allow_cache:!allow_cache
    | _ -> usage ()
  in
  exit code
;;
