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
  (* [decide] reduces in the kernel; on a large concrete model it can blow up (NOTES.md:
     Gaveup) OR run out the budget (Timeout). [native_decide] compiles the SAME decision
     procedure, so escalate on either failure — recovering certifications a bare-Gaveup
     fallthrough silently dropped to Inconclusive. Bounded: exactly ONE escalation, only
     on a decide failure, each attempt under the per-query timeout — it cannot burn
     unbounded budget. native_decide is compiler-trusted (ADR-0006 Decision 3); the
     succeeding tactic name rides out in the outcome so the tier is auditable. *)
  | `Gaveup _ | `Timeout -> attempt "native_decide"
  | `Encode _ as other -> other
;;

(* ---- certification per claim ---- *)

(* Trust-honest phrasing for the tactic that discharged a goal (ADR-0006 Decision 3):
   [native_decide] trusts the compiler + [Lean.ofReduceBool] axiom, so it is NOT
   kernel-checked; [grind]/[decide]/[omega] emit kernel-checked proof terms. Used in
   REFUTED detail so a compiler-trusted witness is never mislabelled kernel-checked. *)
let trust_phrase = function
  | "native_decide" -> "compiler-checked"
  | _ -> "kernel-checked"
;;

let refute_with_witness ctx q model_opt ~default : Outcome.t =
  match model_opt with
  | Some m ->
    (match try_sat ctx q m with
     | `Proved tac ->
       Outcome.Refuted
         (Printf.sprintf
            "assertions satisfiable, witness %s by %s"
            (trust_phrase tac)
            tac)
     | _ -> default)
  | None -> default
;;

let certify_unsat ctx q model_opt : Outcome.t =
  match try_unsat ctx q with
  | `Proved tac -> Certified tac
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
     | `Proved tac -> Certified tac
     | `Encode s -> Encode_error s
     (* decide AND its native_decide escalation both timed out (try_sat). Kept out of the
        cache (should_cache), so a larger --timeout re-tries from scratch. *)
     | `Timeout -> Inconclusive "model check timed out (decide, then native_decide)"
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
  | `Read_error (* entry file present but unreadable — went to Lean; counted, fail-safe *)
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
                  if allow_cache
                  then Cache.lookup ~dir:cache_dir ~claim:claim_str key
                  else Cache.Absent
                in
                (match cached with
                 | Cache.Hit o -> o, `Hit, dm
                 | (Cache.Absent | Cache.Unreadable _) as miss ->
                   (* A present-but-unreadable entry is a broken-cache signal, not a
                      normal miss: log it and count it distinctly (digest
                      [cache-read-errors]) so a corrupted / format-incompatible cache
                      cannot hide behind a green gate. Either way we fall through to a
                      real certification — fail-safe. *)
                   let miss_disp =
                     match miss with
                     | Cache.Unreadable m ->
                       logf ctx "  cache-read-error %s: %s" (Filename.basename smt2) m;
                       `Read_error
                     | _ -> `Miss
                   in
                   let o =
                     match claim with
                     | Ast.Unsat -> certify_unsat ctx q model_opt
                     | Ast.Sat -> certify_sat ctx q model_opt
                     | Ast.Unknown -> No_status
                   in
                   if allow_cache && should_cache o
                   then Cache.store ~dir:cache_dir key ~claim:claim_str o;
                   o, (if allow_cache then miss_disp else `Uncached), dm)))))
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
    | `Miss | `Read_error | `Uncached -> "lean"
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
let is_head s =
  String.length s = 40
  && String.for_all (fun c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) s
;;

(* Read from [fd] until EOF or [deadline], whichever first, capped at [cap] bytes. Returns
   [`Eof raw] only if EOF is reached within budget (the child fully closed its stdout — a
   complete, trustworthy output); [`Timeout] if the deadline passes first. B2: the read is
   itself deadlined, so a git that exits 0 yet leaves a descendant holding the write end
   open (EOF never arrives) yields `Timeout rather than wedging the gate. Every EINTR
   (from select OR read) falls back into this loop, whose FIRST act rechecks the deadline
   — so an EINTR storm can never spin unbounded (codex round-4); it can only shorten the
   budget. A well-behaved git reaches EOF in microseconds. *)
let read_deadlined fd ~deadline ~cap =
  let buf = Buffer.create 64 in
  let chunk = Bytes.create 4096 in
  let rec loop () =
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0.0
    then `Timeout
    else (
      let ready =
        try
          let rl, _, _ = Unix.select [ fd ] [] [] remaining in
          rl <> []
        with
        | Unix.Unix_error (Unix.EINTR, _, _) -> false
      in
      if not ready
      then if Unix.gettimeofday () > deadline then `Timeout else loop ()
      else (
        match Unix.read fd chunk 0 (Bytes.length chunk) with
        | 0 -> `Eof (Buffer.contents buf)
        | n ->
          Buffer.add_subbytes buf chunk 0 n;
          (* An output that never EOFs but keeps dribbling can't grow unbounded: cap it,
             and the oversized buffer fails the exact-match validation below. *)
          if Buffer.length buf > cap then `Eof (Buffer.contents buf) else loop ()
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop () (* recheck deadline *)))
  in
  loop ()
;;

(* Exactly "<40-hex>" or "<40-hex>\n" — validated on the RAW bytes (B3: NO trim; leading
   space, interior/trailing whitespace, or any extra line must be rejected). *)
let head_of_raw raw =
  let ok h = is_head h in
  if String.length raw = 40 && ok raw
  then raw
  else if String.length raw = 41 && raw.[40] = '\n' && ok (String.sub raw 0 40)
  then String.sub raw 0 40
  else "nohead"
;;

(* [git rev-parse HEAD], hardened (codex Scope B, task #133):
   - HARD TIMEOUT on BOTH the wait-for-exit AND the read, sharing one ~5s deadline, so
     neither a hung git nor a git that exits 0 while a descendant keeps stdout open can
     wedge the gate before any honeypot/certification runs (B2). Timeout -> "nohead".
   - STRICT raw acceptance: stamp a HEAD only if git EXITED 0 AND its stdout is EOF-closed
     within budget AND the raw bytes are exactly <40-hex>(\n)? — no trim (B3).
   - All fds are opened INSIDE the protected region and closed on every path; the child is
     reaped on every path incl. timeout and EINTR (B4). Logging-side only — the result
     just names the log dir, never a verdict. *)
let provenance_head () =
  let deadline = Unix.gettimeofday () +. 5.0 in
  let r = ref None
  and w = ref None
  and dn = ref None
  and pid = ref None in
  let close_opt rf =
    match !rf with
    | Some fd ->
      (try Unix.close fd with
       | _ -> ());
      rf := None
    | None -> ()
  in
  let reap_opt () =
    match !pid with
    | Some p ->
      (try Unix.kill p Sys.sigkill with
       | _ -> ());
      (* Bounded reap (codex round-4): poll WNOHANG up to a small fresh budget rather than
         a blocking waitpid. An EINTR just re-polls (budget rechecked at the top); a
         SIGKILLed child we cannot reap within budget is left as a rare zombie — strictly
         better than a wedged gate. Never blocks unbounded. *)
      let reap_deadline = Unix.gettimeofday () +. 2.0 in
      let rec poll () =
        if Unix.gettimeofday () > reap_deadline
        then () (* give up: log-and-move-on, rare zombie *)
        else (
          match Unix.waitpid [ Unix.WNOHANG ] p with
          | 0, _ ->
            (try Unix.sleepf 0.005 with
             | _ -> ());
            poll ()
          | _, _ -> () (* reaped *)
          | exception Unix.Unix_error (Unix.ECHILD, _, _) -> () (* already gone *)
          | exception Unix.Unix_error (Unix.EINTR, _, _) -> poll ())
      in
      poll ();
      pid := None
    | None -> ()
  in
  match
    Fun.protect
      ~finally:(fun () ->
        (* Reap a still-live child (timeout/exception paths) and close every fd. A child
           already reaped on the clean path has pid := None, so this is a no-op there. *)
        reap_opt ();
        close_opt r;
        close_opt w;
        close_opt dn)
      (fun () ->
         let rr, ww = Unix.pipe () in
         r := Some rr;
         w := Some ww;
         let d = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
         dn := Some d;
         let p =
           Unix.create_process "git" [| "git"; "rev-parse"; "HEAD" |] Unix.stdin ww d
         in
         pid := Some p;
         close_opt w;
         (* parent's write end; child holds its own *)
         close_opt dn;
         (* Deadline is rechecked at the TOP of every iteration, so an EINTR on waitpid
           (which just loops back here) can never spin unbounded — it only shortens the
           budget, exactly like the select path (codex round-4). *)
         let rec wait_exit () =
           if Unix.gettimeofday () > deadline
           then `Timeout
           else (
             match Unix.waitpid [ Unix.WNOHANG ] p with
             | 0, _ ->
               (try Unix.sleepf 0.01 with
                | Unix.Unix_error (Unix.EINTR, _, _) -> ());
               wait_exit ()
             | _, status ->
               pid := None;
               (* reaped *)
               `Exited status
             | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait_exit ())
         in
         match wait_exit () with
         | `Timeout -> "nohead" (* finally kills + reaps the still-live child *)
         | `Exited (Unix.WEXITED 0) ->
           let fd =
             match !r with
             | Some fd -> fd
             | None -> assert false
           in
           (match read_deadlined fd ~deadline ~cap:4096 with
            | `Timeout -> "nohead"
            | `Eof raw -> head_of_raw raw)
         | `Exited _ -> "nohead")
  with
  | h -> h
  | exception _ -> "nohead"
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
  (* gate-<stamp>-<pid>-<HEAD>: the pid makes two runs in the SAME wall-second with the
     same HEAD (or both "nohead") land in DISTINCT dirs — otherwise they'd share a log dir
     and per-query %03d .lean files, so one run's Lean child could read the other's
     overwritten file and certify a different query (codex Scope B, CRITICAL). HEAD stays
     the FINAL '-'-component so status_gen's provenance parse is unchanged. *)
  Filename.concat
    logs_root
    (Printf.sprintf "gate-%s-%d-%s" stamp (Unix.getpid ()) (provenance_head ()))
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
  and misses = ref 0
  and read_errors = ref 0 in
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
        | `Read_error -> incr read_errors
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
  , !read_errors
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
      , read_errors
      , ship_stoppers
      , encode_errors
      , quarantined
      , divmod_certified )
    =
    if honeypots_ok
    then cases_phase ctx ~cache_dir cases_dir
    else Hashtbl.create 1, 0, 0, 0, 0, [], [], [], 0
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
  (* [read-error] counts EXISTING entries that could not be parsed (e.g. a stale format) —
     distinct from a cold [miss]. It stays visible even at 0 so a cache that silently
     stops warming (review R1) cannot hide behind a green gate; it never turns the gate
     red (lookup is fail-safe — a read error re-certifies via Lean). *)
  Printf.printf "  cache: %d hit, %d miss, %d read-error\n" hits misses read_errors;
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
    | `Read_error -> "lean (cache entry unreadable)"
    | `Uncached -> "lean"
  in
  Printf.printf "%s: %s [%s]\n" (short file) (Outcome.tag o) d;
  if Outcome.detail o <> "" then Printf.printf "  %s\n" (Outcome.detail o);
  Printf.printf "  logdir: %s\n" logdir;
  Outcome.exit_code o
;;

(* The [dump-canonical] diagnostic for one query: the canonical assertions PREFIXED with
   the claim ([:status]). The claim is load-bearing for verdict ROUTING — it picks the
   certify direction (sat vs unsat) and is folded into the cache key — yet
   {!Canonical.canonical_query} covers only the assertions, so two files differing solely
   in [:status sat] vs [unsat] would otherwise dump identically and the
   reader-preservation differential would be BLIND to a status-parsing regression (review
   HIGH gate.ml:1030). Including it makes the dump distinguish them. *)
let dump_canonical (q : Ast.query) =
  let status =
    match q.status with
    | None -> "none"
    | Some v -> Ast.verdict_to_string v
  in
  Printf.sprintf "(status %s)\n%s" status (Canonical.canonical_query q)
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
  (* Cache store -> lookup soundness. Round-trip (review R1): a key whose hash AND
     query-hash are both digit-leading-with-a-hex-letter must Hit, with an exact detail
     round-trip. Every CORRUPT / hand-crafted / mismatched entry must read as [Unreadable]
     (counted, re-certified via Lean) — NEVER a Hit of any class (review CRITICALs
     cache.ml:101 corrupt tag, cache.ml:170 crafted/trailing/mismatched). *)
  let cache_dir =
    Filename.concat (Filename.get_temp_dir_name ())
    @@ Printf.sprintf "oxsmt-gate-cache-selftest-%d" (Unix.getpid ())
  in
  Cache.mkdir_p cache_dir;
  let dummy name : Cache.key = { Cache.hash = name; query_hash = name ^ "-qh" } in
  (* A full, schema-valid entry for [k] mirroring what [Cache.store] writes (so identity
     checks pass); callers override individual fields to build each corruption shape. *)
  let full_fields (k : Cache.key) ~claim ~tag ~detail =
    [ "key", k.Cache.hash
    ; "query-hash", k.Cache.query_hash
    ; "claim", claim
    ; "outcome", tag
    ; "detail", detail
    ; "encoding-version", Encoder.encoding_version
    ; "grind-config", Encoder.grind_config
    ; "timestamp", "0"
    ]
  in
  let entry_str fields ~trailing =
    let b = Buffer.create 128 in
    Buffer.add_string b "(entry\n";
    List.iter
      (fun (n, v) -> Buffer.add_string b (Printf.sprintf "  (%s %s)\n" n (Cache.atom v)))
      fields;
    Buffer.add_string b ")\n";
    Buffer.add_string b trailing;
    Buffer.contents b
  in
  (* Append the integrity digest {!Cache.store} would write, so a crafted entry passes
     schema + integrity and ISOLATES the specific check under test (identity, tag, claim)
     rather than tripping the integrity guard first. *)
  let with_integrity content = content @ [ "integrity", Cache.content_digest content ] in
  let write_raw (k : Cache.key) s = Lean_runner.write_file (Cache.path cache_dir k) s in
  let replace_first ~needle ~repl s =
    let ls = String.length s
    and ln = String.length needle in
    let rec find i =
      if i + ln > ls
      then None
      else if String.sub s i ln = needle
      then Some i
      else find (i + 1)
    in
    match find 0 with
    | None -> s
    | Some i -> String.sub s 0 i ^ repl ^ String.sub s (i + ln) (ls - i - ln)
  in
  let expect_unreadable label ~claim (k : Cache.key) =
    match Cache.lookup ~dir:cache_dir ~claim k with
    | Cache.Unreadable _ -> print_endline (label ^ ": OK (Unreadable, never a Hit)")
    | Cache.Hit o ->
      ok := false;
      Printf.printf "%s: FAIL (FALSE HIT %s — soundness)\n" label (Outcome.tag o)
    | Cache.Absent ->
      ok := false;
      Printf.printf "%s: FAIL (Absent — test file missing)\n" label
  in
  (* (1) round-trip Hit, exact detail (incl. a tab, exercising the store/read escaping
         fix). *)
  let key = dummy "2f5d4447deadbeefcafef00d1234567890abcdef0123456789abcdef012345" in
  Cache.store ~dir:cache_dir key ~claim:"unsat" (Outcome.Refuted "boundary\twith tab");
  (match Cache.lookup ~dir:cache_dir ~claim:"unsat" key with
   | Cache.Hit (Outcome.Refuted "boundary\twith tab") ->
     print_endline "cache round-trip: OK (digit-leading-hex key hits; detail exact)"
   | other ->
     ok := false;
     Printf.printf
       "cache round-trip: FAIL (expected Hit Refuted, got %s)\n"
       (match other with
        | Cache.Hit o -> "Hit " ^ Outcome.tag o
        | Cache.Absent -> "Absent"
        | Cache.Unreadable m -> "Unreadable " ^ m));
  (* (1b) CERTIFIED carries its tactic as the trust-tier provenance (#86): a compiler-
     trusted [native_decide] must round-trip verbatim, so STATUS can separate it from the
     kernel-checked tiers. A bare [Certified] that dropped the detail would read back as
     [Certified ""] and fail this. *)
  let ckey = dummy "9c0ffee5deadbeefcafef00d1234567890abcdef0123456789abcdef01234567" in
  Cache.store ~dir:cache_dir ckey ~claim:"sat" (Outcome.Certified "native_decide");
  (match Cache.lookup ~dir:cache_dir ~claim:"sat" ckey with
   | Cache.Hit (Outcome.Certified "native_decide") ->
     print_endline "cache tier round-trip: OK (CERTIFIED tactic preserved)"
   | other ->
     ok := false;
     Printf.printf
       "cache tier round-trip: FAIL (expected Hit (Certified native_decide), got %s)\n"
       (match other with
        | Cache.Hit o ->
          Printf.sprintf "Hit %s detail=%S" (Outcome.tag o) (Outcome.detail o)
        | Cache.Absent -> "Absent"
        | Cache.Unreadable m -> "Unreadable " ^ m));
  (* (2) absent key -> clean Absent, not a read-error. *)
  (match Cache.lookup ~dir:cache_dir ~claim:"unsat" (dummy "0000nope") with
   | Cache.Absent -> print_endline "cache absent-key: OK (clean miss, not a read-error)"
   | _ ->
     ok := false;
     print_endline "cache absent-key: FAIL (expected Absent)");
  (* (3) corrupt outcome tag (one-byte flip of REFUTED), otherwise a valid entry: the
     ship-stopper must NOT decay into a benign Hit (CRITICAL cache.ml:101). *)
  let k = dummy "corrupt_tag" in
  write_raw
    k
    (entry_str
       (with_integrity (full_fields k ~claim:"unsat" ~tag:"REFUTEX" ~detail:"cex"))
       ~trailing:"");
  expect_unreadable "cache corrupt-tag" ~claim:"unsat" k;
  (* (4) trailing garbage after the first entry's ')': a truncated-then-reappended file. *)
  let k = dummy "trailing" in
  write_raw
    k
    (entry_str
       (full_fields k ~claim:"unsat" ~tag:"CERTIFIED" ~detail:"")
       ~trailing:"(entry (outcome REFUTED) (detail x))\n");
  expect_unreadable "cache trailing-garbage" ~claim:"unsat" k;
  (* (5) the exact CRITICAL cache.ml:170 trigger: a hand-crafted minimal entry that names
     only an outcome — missing every identity field. *)
  let k = dummy "crafted" in
  write_raw k "(entry (outcome CERTIFIED))\n";
  expect_unreadable "cache crafted-minimal (missing fields)" ~claim:"unsat" k;
  (* (6) identity mismatch: a full entry (schema + integrity valid) whose [key] field is
     someone else's hash — must fail the identity check, not slip through. *)
  let k = dummy "wrongkey" in
  write_raw
    k
    (entry_str
       (with_integrity
          (("key", "SOMEONE-ELSE")
           :: List.tl (full_fields k ~claim:"unsat" ~tag:"CERTIFIED" ~detail:"")))
       ~trailing:"");
  expect_unreadable "cache wrong-key identity mismatch" ~claim:"unsat" k;
  (* (7) claim mismatch: a valid entry stored for unsat, looked up for a sat claim. *)
  let k = dummy "claimx" in
  write_raw
    k
    (entry_str
       (with_integrity (full_fields k ~claim:"unsat" ~tag:"CERTIFIED" ~detail:""))
       ~trailing:"");
  expect_unreadable "cache claim mismatch" ~claim:"sat" k;
  (* (8)/(9) OUTCOME-FLIP (round-3 residual): store a valid entry, then flip ONLY the
     outcome tag on disk, leaving the (now-stale) integrity. Lookup must reject it — the
     ship-stopper survives a REFUTED→CERTIFIED flip, and a CERTIFIED→REFUTED flip cannot
     manufacture a spurious ship-stopper either. *)
  let flip_test label ~claim ~stored ~needle ~repl =
    let k = dummy label in
    Cache.store ~dir:cache_dir k ~claim stored;
    let orig = Lean_runner.read_file (Cache.path cache_dir k) in
    write_raw k (replace_first ~needle ~repl orig);
    expect_unreadable ("cache outcome-flip " ^ label) ~claim k
  in
  flip_test
    "refuted->certified"
    ~claim:"unsat"
    ~stored:(Outcome.Refuted "kernel-checked counterexample")
    ~needle:"(outcome REFUTED)"
    ~repl:"(outcome CERTIFIED)";
  flip_test
    "certified->refuted"
    ~claim:"unsat"
    ~stored:(Outcome.Certified "decide")
    ~needle:"(outcome CERTIFIED)"
    ~repl:"(outcome REFUTED)";
  (* (10) integrity-preimage injectivity (codex round-3 LOW): two content tuples that
     genuinely COLLIDE under a plain NUL-join — the detail/encoding-version pair
     [("x", "y\0z")] vs [("x\0y", "z")], both of which flatten to [x\0y\0z] between the
     surrounding separators — MUST get DIFFERENT length-prefixed digests. (A shift that
     leaves a value EMPTY does not collide: the empty value still contributes its own
     separator; the collision needs the separator byte to appear INSIDE a value.) *)
  let base = full_fields (dummy "inj") ~claim:"unsat" ~tag:"CERTIFIED" ~detail:"" in
  let set n v a = (n, v) :: List.remove_assoc n a in
  let a = base |> set "detail" "x" |> set "encoding-version" "y\000z" in
  let b = base |> set "detail" "x\000y" |> set "encoding-version" "z" in
  if not (String.equal (Cache.content_digest a) (Cache.content_digest b))
  then print_endline "integrity preimage: OK (injective across field boundary)"
  else (
    ok := false;
    print_endline "integrity preimage: FAIL (boundary-shift tuples collide)");
  (* Best-effort cleanup of the throwaway cache dir. *)
  (try
     Array.iter
       (fun f ->
          try Sys.remove (Filename.concat cache_dir f) with
          | _ -> ())
       (Sys.readdir cache_dir);
     Unix.rmdir cache_dir
   with
   | _ -> ());
  (* Model sidecar hardening. (a) An oversized Fin index (all-digits but > native int)
     must be a controlled [Bad_model] (→ ENCODE_ERROR), never a crash (HIGH model.ml:128). *)
  let big = "999999999999999999999999999999999" in
  (match Model.of_string (Printf.sprintf "(model (sort S 1) (const x %s))" big) with
   | exception Model.Bad_model _ ->
     print_endline "model oversized-Fin: OK (rejected at parse)"
   | m ->
     (match Model.coerce m (Ast.Usort "S") (List.assoc "x" m.Model.consts) with
      | exception Model.Bad_model _ ->
        print_endline "model oversized-Fin: OK (Bad_model, not a crash)"
      | exception e ->
        ok := false;
        Printf.printf "model oversized-Fin: FAIL (uncaught %s)\n" (Printexc.to_string e)
      | _ ->
        ok := false;
        print_endline "model oversized-Fin: FAIL (coerced an unrepresentable index)"));
  (* (b) A |quoted| model symbol name must be accepted (MEDIUM model.ml:107). *)
  (match Model.of_string "(model (const |1x| 0))" with
   | exception e ->
     ok := false;
     Printf.printf "model quoted-name: FAIL (raised %s)\n" (Printexc.to_string e)
   | m ->
     if List.mem_assoc "1x" m.Model.consts
     then print_endline "model quoted-name: OK (|1x| accepted as \"1x\")"
     else (
       ok := false;
       print_endline "model quoted-name: FAIL (name not decoded)"));
  (* dump-canonical must reflect the claim: two files identical but for [:status] route to
     opposite certify directions, so the reader-preservation diff MUST see them as
     different (review HIGH gate.ml:1030). *)
  let dump s =
    match Reader.of_string s with
    | q -> Some (dump_canonical q)
    | exception _ -> None
  in
  let base = "(set-logic QF_UFLIA)(declare-const x Int)(assert (= x 0))" in
  (match
     ( dump (base ^ "(set-info :status sat)(check-sat)")
     , dump (base ^ "(set-info :status unsat)(check-sat)") )
   with
   | Some a, Some b when not (String.equal a b) ->
     print_endline "dump-canonical status: OK (sat vs unsat dump differently)"
   | Some _, Some _ ->
     ok := false;
     print_endline "dump-canonical status: FAIL (status-variant files dump identically)"
   | _ ->
     ok := false;
     print_endline "dump-canonical status: FAIL (parse)");
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
    (* Print the canonical query (or MALFORMED/UNSUPPORTED) for one file — a Lean-free
       differential tool for any future reader change: dump over the corpus before and
       after, diff, and the byte-identity is the reader-preservation proof (this is
       exactly how task/tokenizer-gate proved the shared-lexer migration; see
       tests/README). *)
    | "dump-canonical" :: file :: _ ->
      let src = In_channel.with_open_bin file In_channel.input_all in
      (match Reader.of_string src with
       | q ->
         print_endline (dump_canonical q);
         0
       | exception Reader.Malformed m ->
         Printf.printf "MALFORMED %s\n" m;
         0
       | exception Reader.Unsupported m ->
         Printf.printf "UNSUPPORTED %s\n" m;
         0)
    | _ -> usage ()
  in
  exit code
;;
