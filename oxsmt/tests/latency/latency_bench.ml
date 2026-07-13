(* Small-query latency microbenchmark for the in-process session API (GOALS.md "Solve rate
   and speed" bullets 3-4).

   Two targets: b3: a small query (~a dozen QF assertions) answers in under 100
   microseconds, in-process; b4: a session of 1,000 small queries with push/pop between
   them completes in well under a second.

   This is a VISIBILITY tool, not a gate. It links {!Oxsmt_interface.Session} directly (no
   CLI, no parser, no .smt2 files) and drives it with representative refinement-VC-shaped
   queries: small EUF, LIA, and mixed EUF+LIA goals, mostly in the unsat direction a
   validity check (context => goal, discharged as unsat of context /\ ¬goal) produces.

   Timing lives ONLY here in the harness (never in solver code / goldens, I5/I6). We use
   [Unix.gettimeofday] for the fine-grained per-query wall interval (nanosecond vDSO
   resolution on Linux; the numbers are printed, never committed as a golden) and
   cross-check with [Sys.time] processor time over the whole run. Build the executable
   under the RELEASE profile for the headline numbers (release compiles -noassert, so the
   assertion overhead is out of the measured path).

   Usage: latency_bench [--iters N] [--session-queries N] [--log FILE] *)

open Oxsmt_core
module Session = Oxsmt_interface.Session

(* ----------------------------------------------------------------------------- *)
(* Query builders. Each takes a session and its context, asserts a small QF goal over
   symbols it declares (per-query mode) or over a shared pool (session mode), and the
   harness records the resulting verdict. We keep the goals in the fragment that returns a
   definite verdict (EUF unsat via congruence/transitivity, LIA sat/unsat) so we measure
   real solving, not a degrade-to-unknown short-circuit. *)

(* A builder declares over a session and asserts. [pool] carries pre-declared shared
   symbols for the push/pop session mode; per-query mode passes a freshly-declared pool
   each call. *)
type pool =
  { a : Term.t array (* uninterpreted-sort constants *)
  ; f : Symbol.t (* U -> U *)
  ; x : Term.t array (* Int constants *)
  ; h : Symbol.t (* U -> Int *)
  }

let make_pool s ~tag =
  let ctx = Session.context s in
  let usym = Session.declare_sort s (Printf.sprintf "U%s" tag) in
  let u_sort = Sort.uninterpreted usym in
  let a =
    Array.init 8 (fun i ->
      Context.const ctx (Session.declare_const s (Printf.sprintf "a%s_%d" tag i) u_sort))
  in
  let f =
    Session.declare_fun s (Printf.sprintf "f%s" tag) (Rank.create [ u_sort ] u_sort)
  in
  let x =
    Array.init 8 (fun i ->
      Context.const ctx (Session.declare_const s (Printf.sprintf "x%s_%d" tag i) Sort.int))
  in
  let h =
    Session.declare_fun s (Printf.sprintf "h%s" tag) (Rank.create [ u_sort ] Sort.int)
  in
  { a; f; x; h }
;;

(* (1) EUF transitivity+congruence chain, unsat. a0=a1=..=a5, f(a0)=f(a5) forced, assert
       a0 <> a5 -> unsat. ~a dozen atoms. *)
let euf_chain s p =
  let ctx = Session.context s in
  for i = 0 to 4 do
    Session.assert_term s (Context.eq ctx p.a.(i) p.a.(i + 1))
  done;
  Session.assert_term
    s
    (Context.eq ctx (Context.app ctx p.f [ p.a.(0) ]) (Context.app ctx p.f [ p.a.(5) ]));
  Session.assert_term s (Context.not_ ctx (Context.eq ctx p.a.(0) p.a.(5)));
  Session.Unsat
;;

(* (2) EUF congruence under nesting, unsat: x=y /\ f(f(x)) <> f(f(y)). *)
let euf_congruence s p =
  let ctx = Session.context s in
  let ff t = Context.app ctx p.f [ Context.app ctx p.f [ t ] ] in
  Session.assert_term s (Context.eq ctx p.a.(0) p.a.(1));
  Session.assert_term s (Context.eq ctx p.a.(1) p.a.(2));
  Session.assert_term s (Context.not_ ctx (Context.eq ctx (ff p.a.(0)) (ff p.a.(2))));
  Session.Unsat
;;

(* (3) LIA array-index safety VC, unsat: 0<=i, i<n, n<=len, ¬(i<len). *)
let lia_index s p =
  let ctx = Session.context s in
  let i = p.x.(0)
  and n = p.x.(1)
  and len = p.x.(2)
  and k = p.x.(3) in
  Session.assert_term s (Context.le ctx (Context.int_const ctx 0) i);
  Session.assert_term s (Context.lt ctx i n);
  Session.assert_term s (Context.le ctx n len);
  Session.assert_term s (Context.le ctx (Context.int_const ctx 0) k);
  Session.assert_term s (Context.lt ctx k i);
  Session.assert_term s (Context.not_ ctx (Context.lt ctx i len));
  Session.Unsat
;;

(* (4) LIA feasible system, sat with a self-checkable (function-free) model. *)
let lia_sat s p =
  let ctx = Session.context s in
  let x = p.x.(0)
  and y = p.x.(1)
  and z = p.x.(2) in
  Session.assert_term s (Context.le ctx (Context.int_const ctx 0) x);
  Session.assert_term s (Context.le ctx x (Context.int_const ctx 10));
  Session.assert_term s (Context.eq ctx (Context.add ctx x y) (Context.int_const ctx 7));
  Session.assert_term s (Context.ge ctx y (Context.int_const ctx 2));
  Session.assert_term s (Context.eq ctx z (Context.sub ctx x y));
  Session.Sat
;;

(* (5) Mixed EUF+LIA, unsat: h(a0)=h(a1) (via a0=a1 congruence) but h(a0)>=5 and h(a1)<=3.
   Function into Int, arithmetic on the results — exercises the Nelson-Oppen combination
   in the unsat direction. *)
let mixed_unsat s p =
  let ctx = Session.context s in
  let h0 = Context.app ctx p.h [ p.a.(0) ]
  and h1 = Context.app ctx p.h [ p.a.(1) ] in
  Session.assert_term s (Context.eq ctx p.a.(0) p.a.(1));
  Session.assert_term s (Context.ge ctx h0 (Context.int_const ctx 5));
  Session.assert_term s (Context.le ctx h1 (Context.int_const ctx 3));
  Session.Unsat
;;

let builders =
  [ "euf_chain", euf_chain, Session.Unsat
  ; "euf_congruence", euf_congruence, Session.Unsat
  ; "lia_index", lia_index, Session.Unsat
  ; "lia_sat", lia_sat, Session.Sat
  ; "mixed_unsat", mixed_unsat, Session.Unsat
  ]
;;

(* ----------------------------------------------------------------------------- *)
(* Stats over a sorted float array (seconds). *)

let percentile sorted q =
  let n = Array.length sorted in
  if n = 0
  then 0.0
  else (
    let idx = int_of_float (Float.round (q *. float_of_int (n - 1))) in
    sorted.(max 0 (min (n - 1) idx)))
;;

let mean a =
  if Array.length a = 0
  then 0.0
  else Array.fold_left ( +. ) 0.0 a /. float_of_int (Array.length a)
;;

let us s = s *. 1e6

(* ----------------------------------------------------------------------------- *)

let verdict_str = function
  | Session.Sat -> "sat"
  | Session.Unsat -> "unsat"
  | Session.Unknown -> "unknown"
;;

(* A silent wrong-answer regression must KILL the bench run, not print a warning: a bench
   that keeps reporting latency for a query the solver now decides wrongly (or degrades to
   [Unknown]) is measuring non-target work and hiding a soundness/completeness regression.
   Every phase that runs a builder checks its verdict against the builder's declared
   expectation through here; [main] exits nonzero if any check failed. *)
let verdict_failures = ref 0
let verdict_reported : (string, unit) Hashtbl.t = Hashtbl.create 16

let require_verdict ~phase ~name ~expected got =
  if got <> expected
  then (
    incr verdict_failures;
    (* Called per-iteration (thorough — catches a nondeterministic flip too), but print
       once per (phase, query) so a broken run fails loudly, not in 5000 identical lines. *)
    let key = phase ^ ":" ^ name in
    if not (Hashtbl.mem verdict_reported key)
    then (
      Hashtbl.add verdict_reported key ();
      Printf.printf
        "  FAIL [%s] %s: got %s, expected %s\n"
        phase
        name
        (verdict_str got)
        (verdict_str expected)))
;;

(* Per-query latency: fresh session each iteration, declare + assert + check_sat, timed as
   one unit (that is the "in-process small query answers in under 100us" target — the
   whole cost a caller pays). *)
let bench_per_query ~iters ~warmup out =
  Printf.printf "\n== per-query latency (fresh session, declare+assert+check_sat) ==\n";
  Printf.printf
    "%-16s  %8s  %9s  %9s  %9s  %9s\n"
    "query"
    "verdict"
    "p50_us"
    "p90_us"
    "p99_us"
    "max_us";
  List.iter
    (fun (name, build, expected) ->
       (* Warmup + correctness: verdict must match, or we're timing a degrade. *)
       let got = ref Session.Unknown in
       for _ = 1 to warmup do
         let s = Session.create () in
         let p = make_pool s ~tag:"" in
         got := build s p;
         let v = Session.check_sat s in
         got := v
       done;
       require_verdict ~phase:"per_query" ~name ~expected !got;
       let samples = Array.make iters 0.0 in
       for k = 0 to iters - 1 do
         let t0 = Unix.gettimeofday () in
         let s = Session.create () in
         let p = make_pool s ~tag:"" in
         let (_ : Session.verdict) = build s p in
         let (_ : Session.verdict) = Session.check_sat s in
         let t1 = Unix.gettimeofday () in
         samples.(k) <- t1 -. t0
       done;
       Array.sort compare samples;
       let p50 = percentile samples 0.50
       and p90 = percentile samples 0.90
       and p99 = percentile samples 0.99
       and mx = samples.(iters - 1) in
       Printf.printf
         "%-16s  %8s  %9.2f  %9.2f  %9.2f  %9.2f\n"
         name
         (verdict_str expected)
         (us p50)
         (us p90)
         (us p99)
         (us mx);
       Printf.fprintf
         out
         "per_query\t%s\tverdict=%s\tp50_us=%.2f\tp90_us=%.2f\tp99_us=%.2f\tmean_us=%.2f\tmax_us=%.2f\n"
         name
         (verdict_str expected)
         (us p50)
         (us p90)
         (us p99)
         (us (mean samples))
         (us mx))
    builders
;;

(* 1,000-query push/pop session (target b4): ONE session, symbols declared once in the
   base frame, then N iterations of push / assert small goal / check_sat / pop, rotating
   through the builders. Report total wall time and per-iteration p50/p99. *)
let bench_session_pushpop ~queries out =
  Printf.printf "\n== push/pop session (%d small queries, one session) ==\n" queries;
  let s = Session.create () in
  let p = make_pool s ~tag:"" in
  let bs = Array.of_list builders in
  let nb = Array.length bs in
  (* Warmup one cycle per builder so caches/first-touch allocation is out of the loop. *)
  for j = 0 to nb - 1 do
    let name, build, expected = bs.(j) in
    Session.push s;
    let (_ : Session.verdict) = build s p in
    let got = Session.check_sat s in
    Session.pop s;
    require_verdict ~phase:"pushpop" ~name ~expected got
  done;
  let samples = Array.make queries 0.0 in
  let cpu0 = Sys.time () in
  let wall0 = Unix.gettimeofday () in
  for k = 0 to queries - 1 do
    let name, build, expected = bs.(k mod nb) in
    let t0 = Unix.gettimeofday () in
    Session.push s;
    let (_ : Session.verdict) = build s p in
    let got = Session.check_sat s in
    Session.pop s;
    let t1 = Unix.gettimeofday () in
    require_verdict ~phase:"pushpop" ~name ~expected got;
    samples.(k) <- t1 -. t0
  done;
  let wall = Unix.gettimeofday () -. wall0 in
  let cpu = Sys.time () -. cpu0 in
  Array.sort compare samples;
  Printf.printf
    "  total wall: %.2f ms   cpu: %.2f ms   (%d queries)\n"
    (wall *. 1e3)
    (cpu *. 1e3)
    queries;
  Printf.printf
    "  per-query: p50 %.2f us   p90 %.2f us   p99 %.2f us   max %.2f us\n"
    (us (percentile samples 0.50))
    (us (percentile samples 0.90))
    (us (percentile samples 0.99))
    (us samples.(queries - 1));
  Printf.fprintf
    out
    "session_pushpop\tqueries=%d\ttotal_wall_ms=%.3f\ttotal_cpu_ms=%.3f\tp50_us=%.2f\tp99_us=%.2f\n"
    queries
    (wall *. 1e3)
    (cpu *. 1e3)
    (us (percentile samples 0.50))
    (us (percentile samples 0.99))
;;

(* Phase breakdown: split one per-query unit into create / declare-pool / assert /
   check_sat and report the median of each phase, so the ~30-55us is attributed. Clock
   overhead per timestamp (~tens of ns) is negligible against the us-scale phases. *)
let bench_breakdown ~iters out =
  Printf.printf "\n== per-query phase breakdown (median us over %d iters) ==\n" iters;
  Printf.printf
    "%-16s  %9s  %9s  %9s  %9s  %9s\n"
    "query"
    "create"
    "declare"
    "assert"
    "check_sat"
    "total";
  let median a =
    Array.sort compare a;
    percentile a 0.50
  in
  List.iter
    (fun (name, build, expected) ->
       let c = Array.make iters 0.0
       and d = Array.make iters 0.0
       and asrt = Array.make iters 0.0
       and chk = Array.make iters 0.0 in
       for k = 0 to iters - 1 do
         let t0 = Unix.gettimeofday () in
         let s = Session.create () in
         let t1 = Unix.gettimeofday () in
         let p = make_pool s ~tag:"" in
         let t2 = Unix.gettimeofday () in
         let (_ : Session.verdict) = build s p in
         let t3 = Unix.gettimeofday () in
         let got = Session.check_sat s in
         let t4 = Unix.gettimeofday () in
         require_verdict ~phase:"breakdown" ~name ~expected got;
         c.(k) <- t1 -. t0;
         d.(k) <- t2 -. t1;
         asrt.(k) <- t3 -. t2;
         chk.(k) <- t4 -. t3
       done;
       let mc = us (median c)
       and md = us (median d)
       and ma = us (median asrt)
       and mk = us (median chk) in
       Printf.printf
         "%-16s  %9.2f  %9.2f  %9.2f  %9.2f  %9.2f\n"
         name
         mc
         md
         ma
         mk
         (mc +. md +. ma +. mk);
       Printf.fprintf
         out
         "breakdown\t%s\tcreate_us=%.2f\tdeclare_us=%.2f\tassert_us=%.2f\tcheck_us=%.2f\n"
         name
         mc
         md
         ma
         mk)
    builders
;;

let () =
  let iters = ref 5000
  and warmup = ref 200
  and queries = ref 1000
  and log = ref "" in
  let rec parse = function
    | [] -> ()
    | "--iters" :: n :: r ->
      iters := int_of_string n;
      parse r
    | "--warmup" :: n :: r ->
      warmup := int_of_string n;
      parse r
    | "--session-queries" :: n :: r ->
      queries := int_of_string n;
      parse r
    | "--log" :: f :: r ->
      log := f;
      parse r
    | a :: r ->
      Printf.eprintf "ignoring arg %s\n" a;
      parse r
  in
  parse (List.tl (Array.to_list Sys.argv));
  let out =
    if !log = ""
    then open_out "/dev/null"
    else (
      (try Unix.mkdir (Filename.dirname !log) 0o755 with
       | Unix.Unix_error (Unix.EEXIST, _, _) | Unix.Unix_error (Unix.ENOENT, _, _) -> ());
      open_out !log)
  in
  Printf.printf
    "latency_bench: iters=%d warmup=%d session-queries=%d\n"
    !iters
    !warmup
    !queries;
  bench_per_query ~iters:!iters ~warmup:!warmup out;
  bench_breakdown ~iters:!iters out;
  bench_session_pushpop ~queries:!queries out;
  if !log <> "" then Printf.printf "\nlog: %s\n" !log;
  close_out out;
  (* A wrong-answer regression in ANY phase kills the run — the bench is a solver harness,
     not just a stopwatch, so a silent verdict flip must not pass as green. *)
  if !verdict_failures > 0
  then (
    Printf.printf "latency_bench: %d verdict mismatch(es) — FAILED\n" !verdict_failures;
    exit 1)
;;
