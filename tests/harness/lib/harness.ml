(* Core logic of the .smt2 golden/expect regression harness (DESIGN.md §8).

   Split from I/O on purpose: everything here except [run_solver] is pure, so the
   self-test (harness_test.ml) exercises the pass/fail classification and golden-text
   generation directly, with hand-built solver output and no subprocess. run_harness.ml is
   the thin CLI that scans directories, spawns the solver, writes the stats sidecar, and
   prints the digest. *)

type verdict =
  | Sat
  | Unsat
  | Unknown

let verdict_to_string = function
  | Sat -> "sat"
  | Unsat -> "unsat"
  | Unknown -> "unknown"
;;

let verdict_of_string = function
  | "sat" -> Some Sat
  | "unsat" -> Some Unsat
  | "unknown" -> Some Unknown
  | _ -> None
;;

type counters =
  { conflicts : int
  ; decisions : int
  ; propagations : int
  }

(* A sat model, in one of two solver-emitted shapes (R9 transport):
   - [Flat] is the LEGACY table-free body [((name token) ...)] — one nullary-symbol
     binding per pair, the value a rendered token the eval Model reader types against the
     symbol's declared sort. Const/Bool/LIA models take this path, byte-identical to the
     pre-UF harness.
   - [Table] is the §8 sidecar-grammar body: a sequence of [(sort S n)] /
     [(const name tok)] / [(fun f (default tok) (case (toks) tok) ...)] entries, carried
     verbatim as the harness [Sexp.t] the solver emitted (already in the solver's
     canonical R10 order). The entries are re-serialized on the way OUT (golden text +
     eval sidecar) with the symbol NAME slot re-quoted — the harness Sexp reader strips
     [|bars|], so a name like [a b] must be re-wrapped or it re-lexes as two tokens (see
     {!render_entry}). Both N-version readers (tests/eval, tests/gate) parse this grammar
     verbatim, so the sidecar write is near pass-through. *)
type model =
  | Flat of (string * string) list
  | Table of Sexp.t list

(* One goal = one (check-sat). *)
type goal_result =
  { verdict : verdict
  ; core_size : int option (* present when the solver reports a core (unsat) *)
  ; model : model option (* present when sat *)
  ; counters : counters
  }

type solver_output = goal_result list

(* The solver CLI contract (authoritative copy in tests/README.md).

   The harness invokes [SOLVER <file.smt2>] and reads stdout. The solver prints exactly
   one (result ...) block per (check-sat), in order:

   {v
     (result
       (verdict sat|unsat|unknown)
       (core-size N)          ; optional; unsat with a core
       (model ((x 0) (y 1)))  ; optional; present iff sat — LEGACY flat body, or
       (model (sort S 2) (const a 0) (fun f (default 0) (case (0) 0)))  ; sidecar body
       (counters (conflicts N) (decisions N) (propagations N)))
   v}

   verdict and counters are required; field order is not significant. The [(model ...)]
   body is EITHER the legacy flat [((name token) ...)] list (table-free models) OR the
   sidecar grammar (sort/const/fun entries) for a function-table model; the harness
   accepts both (R9). *)

exception Bad_output of string

(* A counter that overflows OCaml's int (>= 2^62 on a real run, i.e. astronomically large)
   is clamped to max_int rather than rejected: it still buckets to ">=10k", which is all
   the golden records, so a genuinely huge count is information, not a harness error. A
   non-numeric counter is still a contract violation. *)
let int_or_clamp k n =
  match int_of_string_opt n with
  | Some i -> i
  | None ->
    if n <> "" && String.for_all (fun c -> c >= '0' && c <= '9') n
    then max_int
    else raise (Bad_output ("non-integer counter " ^ k))
;;

let parse_counters vs =
  let t =
    List.filter_map
      (function
        | Sexp.List [ Sexp.Atom k; Sexp.Atom n ] -> Some (k, int_or_clamp k n)
        | _ -> None)
      vs
  in
  let get k =
    match List.assoc_opt k t with
    | Some v -> v
    | None -> raise (Bad_output ("missing counter " ^ k))
  in
  { conflicts = get "conflicts"
  ; decisions = get "decisions"
  ; propagations = get "propagations"
  }
;;

let parse_result sx =
  match sx with
  | Sexp.List (Sexp.Atom "result" :: fields) ->
    let tbl =
      List.filter_map
        (function
          | Sexp.List (Sexp.Atom k :: vs) -> Some (k, vs)
          | _ -> None)
        fields
    in
    let get k = List.assoc_opt k tbl in
    let verdict =
      match get "verdict" with
      | Some [ Sexp.Atom v ] ->
        (match verdict_of_string v with
         | Some v -> v
         | None -> raise (Bad_output ("bad verdict " ^ v)))
      | _ -> raise (Bad_output "result is missing (verdict ...)")
    in
    let core_size =
      match get "core-size" with
      | Some [ Sexp.Atom n ] ->
        (match int_of_string_opt n with
         | Some i -> Some i
         | None -> raise (Bad_output "non-integer core-size"))
      | None -> None
      | _ -> raise (Bad_output "bad core-size")
    in
    let model =
      (* The (model ...) body is EITHER the legacy flat pair-list — a SINGLE [Sexp.List]
         whose every element is a 2-element [(name value)] pair — OR the sidecar grammar,
         a sequence of [(sort ...)] / [(const ...)] / [(fun ...)] entries. A sidecar entry
         is a list headed by a keyword atom (arity <> 2 for fun/sort), so it never
         masquerades as a flat pair-list; the discriminator is exactly "one list of
         2-lists" => flat. *)
      let is_flat_pairs pairs =
        List.for_all
          (function
            | Sexp.List [ (Sexp.Atom _ | Sexp.Quoted _); _ ] -> true
            | _ -> false)
          pairs
      in
      match get "model" with
      | None | Some [] -> None
      | Some [ Sexp.List pairs ] when is_flat_pairs pairs ->
        Some
          (Flat
             (List.map
                (function
                  (* the name may arrive |quoted| (e.g. [|a b|]); keep the content and let
                     [render_model_body] re-derive the canonical quoting. The VALUE is
                     carried by [to_string], which preserves a [Quoted] token's bars
                     (faithful — a malformed quoted value is not laundered to a bare one). *)
                  | Sexp.List [ (Sexp.Atom k | Sexp.Quoted k); v ] -> k, Sexp.to_string v
                  | _ -> raise (Bad_output "bad model binding"))
                pairs))
      | Some entries -> Some (Table entries)
    in
    let counters =
      match get "counters" with
      | Some vs -> parse_counters vs
      | None -> raise (Bad_output "result is missing (counters ...)")
    in
    { verdict; core_size; model; counters }
  | _ -> raise (Bad_output "expected a (result ...) block")
;;

let parse_solver_output (s : string) : (solver_output, string) result =
  match Sexp.parse_all s with
  | exception Sexp.Parse_error m -> Error ("solver output parse error: " ^ m)
  | sexps ->
    (match List.map parse_result sexps with
     | out -> Ok out
     | exception Bad_output m -> Error m)
;;

(* Expected verdict for each (check-sat), taken from the most recent (set-info :status
   ...) in effect at that point. [None] = unspecified. *)
let expected_statuses (sexps : Sexp.t list) : verdict option list =
  let cur = ref None in
  let acc = ref [] in
  List.iter
    (fun sx ->
       match sx with
       | Sexp.List [ Sexp.Atom "set-info"; Sexp.Atom ":status"; Sexp.Atom v ] ->
         cur := verdict_of_string v
       | Sexp.List (Sexp.Atom ("check-sat" | "check-sat-assuming") :: _) ->
         acc := !cur :: !acc
       | _ -> ())
    sexps;
  List.rev !acc
;;

(* ------------------------------------------------------------------ *)
(* Golden text generation. *)
(* ------------------------------------------------------------------ *)

(* Re-serialize one sidecar model entry [(sort|const|fun NAME ...)]. The NAME slot is
   re-quoted with {!Sexp.quote_symbol} on its content — a name like [a b] must round-trip
   as [|a b|] or it re-lexes as two tokens and the eval reader rejects the model — so a
   name is faithful whether it arrived bare ([Atom]) or [|quoted|] ([Quoted]). The rest of
   an entry (cardinalities, value tokens: numerals, [(- n)], [true]/[false]) are NOT
   symbol names: they pass through {!Sexp.to_string} verbatim, which PRESERVES a [Quoted]
   token's bars rather than laundering it — a malformed quoted payload like
   [(case (0) |true|)] stays [|true|] so the eval reader fails it closed instead of the
   harness repairing it. A shape the CLI does not emit falls back to a plain [to_string]. *)
let render_entry (e : Sexp.t) : string =
  match e with
  | Sexp.List
      (Sexp.Atom (("sort" | "const" | "fun") as kw)
      :: (Sexp.Atom name | Sexp.Quoted name)
      :: rest) ->
    let parts = kw :: Sexp.quote_symbol name :: List.map Sexp.to_string rest in
    "(" ^ String.concat " " parts ^ ")"
  | other -> Sexp.to_string other
;;

(* Render a model body (the text spliced inside [(model ...)]) for both golden text and
   the eval sidecar. [Flat] sorts pairs by name (stability) and re-quotes each name;
   [Table] carries the solver's canonical entry order (R10) verbatim, re-quoting names per
   entry. *)
let render_model_body = function
  | Flat m ->
    let m = List.sort (fun (a, _) (c, _) -> String.compare a c) m in
    "("
    ^ String.concat
        " "
        (List.map (fun (k, v) -> Printf.sprintf "(%s %s)" (Sexp.quote_symbol k) v) m)
    ^ ")"
  | Table entries -> String.concat " " (List.map render_entry entries)
;;

(* Table-size stats for a sat model (design-author watch item), for the corpus sweep to
   aggregate: [max_card] = the largest uninterpreted-sort finite-universe cardinality (0
   when the model has no [(sort ...)] entry — a table-free / const-only model),
   [table_rows] = the total number of [(case ...)] rows across all function/predicate
   tables (0 for a flat model, which has none). Derived from the parsed model, not a
   solver-emitted counter, so it is a pure function of the accepted model
   (byte-determinism carries over). *)
let model_table_stats = function
  | None | Some (Flat _) -> 0, 0
  | Some (Table entries) ->
    List.fold_left
      (fun (max_card, rows) e ->
         match e with
         | Sexp.List [ Sexp.Atom "sort"; _; Sexp.Atom n ] ->
           (match int_of_string_opt n with
            | Some k -> max max_card k, rows
            | None -> max_card, rows)
         | Sexp.List (Sexp.Atom "fun" :: _ :: fun_entries) ->
           let cases =
             List.fold_left
               (fun c -> function
                  | Sexp.List (Sexp.Atom "case" :: _) -> c + 1
                  | _ -> c)
               0
               fun_entries
           in
           max_card, rows + cases
         | _ -> max_card, rows)
      (0, 0)
      entries
;;

let goal_block idx g =
  let b = Buffer.create 128 in
  Printf.bprintf b "(goal %d\n" idx;
  Printf.bprintf b "  (verdict %s)\n" (verdict_to_string g.verdict);
  (match g.core_size with
   | Some sz -> Printf.bprintf b "  (core-size %d)\n" sz
   | None -> ());
  (match g.model with
   | Some m -> Printf.bprintf b "  (model %s)\n" (render_model_body m)
   | None -> ());
  Printf.bprintf
    b
    "  (counters (conflicts %s) (decisions %s) (propagations %s)))\n"
    (Bucket.label g.counters.conflicts)
    (Bucket.label g.counters.decisions)
    (Bucket.label g.counters.propagations);
  Buffer.contents b
;;

let produced_text (out : solver_output) : string =
  String.concat "" (List.mapi (fun i g -> goal_block (i + 1) g) out)
;;

(* ------------------------------------------------------------------ *)
(* Pass/fail classification. *)
(* ------------------------------------------------------------------ *)

(* Result of the layer-1 eval self-check (DESIGN.md §8 layer 1): every SAT verdict's model
   is evaluated against the assertions by the independent N-version evaluator (tests/eval)
   before we accept it. One per goal. *)
type eval_outcome =
  | Eval_skipped (* verdict not sat — nothing to self-check *)
  | Eval_satisfies (* MODEL-SATISFIES (eval exit 0) *)
  | Eval_fails of string (* MODEL-FAILS (eval exit 1): SOUNDNESS breach *)
  | Eval_unusable of string (* eval exit 2 / couldn't run: our own model is unreadable *)

type outcome =
  | Pass
  | Fail_missing_golden
  | Fail_golden_mismatch
  | Fail_label_mismatch of string
  | Fail_model_unsound of
      string (* eval says our sat model fails — soundness, like a label mismatch *)
  | Fail_eval_unusable of
      string (* eval couldn't read our model — a contract bug, not a pass *)
  | Fail_error of string (* solver error, output parse error, block-count *)

type file_eval =
  { path : string
  ; produced : string (* produced golden text ("" on solver error) *)
  ; golden : string option (* committed golden, if present *)
  ; outcome : outcome
  ; output : solver_output (* parsed solver output ([] on error) *)
  }

(* A verdict contradicts its declared :status only when both are definite and they
   disagree. verdict=unknown against a sat/unsat label is a completeness gap, not a
   soundness failure, so it does not fail the label check. *)
let contradicts expected verdict =
  match expected, verdict with
  | Some Sat, Unsat | Some Unsat, Sat -> true
  | _ -> false
;;

(* First [Some] of [f] over the list, in order (used to surface the first issue). *)
let first_issue f xs =
  List.fold_left
    (fun acc x ->
       match acc with
       | Some _ -> acc
       | None -> f x)
    None
    xs
;;

(* [eval_outcomes] carries the layer-1 self-check result per goal (aligned with the
   solver's blocks; a shorter/empty list pads with [Eval_skipped], so pure callers that
   don't self-check still work). The runner fills it in by running the eval CLI on every
   sat model; the self-test supplies canned values. *)
let evaluate
      ~path
      ~(expected_statuses : verdict option list)
      ~(golden : string option)
      ~(solver_result : (solver_output, string) result)
      ?(eval_outcomes : eval_outcome list = [])
      ()
  : file_eval
  =
  match solver_result with
  | Error e -> { path; produced = ""; golden; outcome = Fail_error e; output = [] }
  | Ok out ->
    let n_goals = List.length out in
    let n_expected = List.length expected_statuses in
    if n_goals <> n_expected
    then
      { path
      ; produced = ""
      ; golden
      ; outcome =
          Fail_error
            (Printf.sprintf
               "solver produced %d result block(s) but the file has %d check-sat(s)"
               n_goals
               n_expected)
      ; output = out
      }
    else (
      (* Label check: a declared-status contradiction is a soundness signal that fails
         regardless of whether the golden matches. *)
      let label_issue =
        first_issue
          (fun (expected, g) ->
             if contradicts expected g.verdict
             then
               Some
                 (Printf.sprintf
                    "declared status %s but solver said %s"
                    (match expected with
                     | Some v -> verdict_to_string v
                     | None -> "?")
                    (verdict_to_string g.verdict))
             else None)
          (List.combine expected_statuses out)
      in
      (* Layer-1 eval self-check over each goal's model (pad missing with skipped). *)
      let evals =
        List.mapi
          (fun i _ ->
             try List.nth eval_outcomes i with
             | _ -> Eval_skipped)
          out
      in
      let model_unsound =
        first_issue
          (function
            | Eval_fails m -> Some m
            | _ -> None)
          evals
      in
      let eval_unusable =
        first_issue
          (function
            | Eval_unusable m -> Some m
            | _ -> None)
          evals
      in
      let produced = produced_text out in
      let outcome =
        (* Soundness signals dominate the golden comparison. *)
        match label_issue, model_unsound, eval_unusable with
        | Some msg, _, _ -> Fail_label_mismatch msg
        | None, Some msg, _ -> Fail_model_unsound msg
        | None, None, Some msg -> Fail_eval_unusable msg
        | None, None, None ->
          (match golden with
           | None -> Fail_missing_golden
           | Some g -> if String.equal g produced then Pass else Fail_golden_mismatch)
      in
      { path; produced; golden; outcome; output = out })
;;

let is_fail = function
  | Pass -> false
  | _ -> true
;;

(* A promote may accept produced output as the new golden only when the sole issue is a
   missing or mismatched golden. Soundness signals (label mismatch, an unsound sat model)
   and any error (solver, or eval unable to read our own model) are never masked. *)
let promotable = function
  | Fail_missing_golden | Fail_golden_mismatch -> true
  | Pass
  | Fail_label_mismatch _
  | Fail_model_unsound _
  | Fail_eval_unusable _
  | Fail_error _ -> false
;;

(* ------------------------------------------------------------------ *)
(* Solver invocation (the only impure part; uses Unix). *)
(* ------------------------------------------------------------------ *)

let read_all_fd fd =
  let ic = Unix.in_channel_of_descr fd in
  let b = Buffer.create 4096 in
  let chunk = Bytes.create 65536 in
  let rec loop () =
    let n = input ic chunk 0 (Bytes.length chunk) in
    if n > 0
    then (
      Buffer.add_subbytes b chunk 0 n;
      loop ())
  in
  loop ();
  Buffer.contents b
;;

(* Spawn [argv], returning (stdout, stderr, status). stdout is read from a live pipe
   (drained before waitpid, so a chatty child can't deadlock); stderr is captured via a
   temp file. The [finally] closes both pipe ends, the stderr fd, and removes the temp
   file on EVERY path — including a mid-function exception (e.g. create_process failing) —
   so no descriptor or temp file leaks. Double-closes are guarded. *)
let spawn_capture argv =
  let r, w = Unix.pipe () in
  let err_path = Filename.temp_file "oxsmt_proc" ".err" in
  let err_fd = Unix.openfile err_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  let close_guarded fd =
    try Unix.close fd with
    | _ -> ()
  in
  Fun.protect
    ~finally:(fun () ->
      close_guarded r;
      close_guarded w;
      close_guarded err_fd;
      try Sys.remove err_path with
      | _ -> ())
    (fun () ->
       let pid = Unix.create_process argv.(0) argv Unix.stdin w err_fd in
       close_guarded w;
       (* parent no longer writes; reader sees EOF *)
       close_guarded err_fd;
       (* child holds its own dup *)
       let out = read_all_fd r in
       let _, status = Unix.waitpid [] pid in
       let errs =
         let ic = open_in_bin err_path in
         Fun.protect
           ~finally:(fun () -> close_in_noerr ic)
           (fun () -> really_input_string ic (in_channel_length ic))
       in
       out, errs, status)
;;

let run_solver solver file : (solver_output, string) result * string =
  match spawn_capture [| solver; file |] with
  | exception e ->
    Error (Printf.sprintf "failed to run solver: %s" (Printexc.to_string e)), ""
  | out, errs, status ->
    (match status with
     | Unix.WEXITED 0 -> parse_solver_output out, errs
     | _ -> Error "solver exited with a non-zero status", errs)
;;

(* ------------------------------------------------------------------ *)
(* Layer-1 eval self-check (DESIGN.md §8 layer 1): run the independent *)
(* N-version evaluator on a sat verdict's model before accepting it. *)
(* ------------------------------------------------------------------ *)

(* Render a sat model into the eval CLI's sidecar grammar (the file the N-version
   evaluator and the Lean gate both parse verbatim). A [Flat] model becomes one
   [(const NAME VALUE)] per binding — VALUE is the solver's own rendered token
   ([true]/[false] for Bool, [5] / [(- 3)] for Int, the element index for an uninterpreted
   sort), typed by the reader against each symbol's declared sort. A [Table] model is near
   pass-through: its sidecar entries are re-serialized (names re-quoted, {!render_entry})
   and wrapped in [(model ...)] — the sort/const/fun grammar the solver already emitted. *)
let model_to_sidecar (model : (string * string) list) : string =
  let b = Buffer.create 128 in
  Buffer.add_string b "(model\n";
  List.iter
    (fun (name, value) ->
       Printf.bprintf b "  (const %s %s)\n" (Sexp.quote_symbol name) value)
    model;
  Buffer.add_string b ")\n";
  Buffer.contents b
;;

let sidecar_of_model = function
  | Flat m -> model_to_sidecar m
  | Table entries -> "(model " ^ String.concat " " (List.map render_entry entries) ^ ")\n"
;;

let truncate_detail s =
  let s = String.trim s in
  if String.length s <= 400 then s else String.sub s 0 400 ^ " [...]"
;;

(* Run [eval_bin smt2 <tmp-model>] and map its exit code to an [eval_outcome]: 0
   MODEL-SATISFIES, 1 MODEL-FAILS (soundness), 2 malformed/unsupported (contract bug),
   anything else / spawn failure -> unusable. The temp model file is always removed. *)
let run_eval ~eval_bin ~smt2 ~(model : model) : eval_outcome =
  match
    let model_path = Filename.temp_file "oxsmt_model" ".model" in
    Fun.protect
      ~finally:(fun () ->
        try Sys.remove model_path with
        | _ -> ())
      (fun () ->
         let oc = open_out_bin model_path in
         output_string oc (sidecar_of_model model);
         close_out oc;
         spawn_capture [| eval_bin; smt2; model_path |])
  with
  | exception e ->
    Eval_unusable (Printf.sprintf "failed to run eval: %s" (Printexc.to_string e))
  | out, errs, status ->
    let detail = truncate_detail (if String.trim errs <> "" then errs else out) in
    (match status with
     | Unix.WEXITED 0 -> Eval_satisfies
     | Unix.WEXITED 1 -> Eval_fails detail
     | Unix.WEXITED 2 -> Eval_unusable ("eval could not read our model: " ^ detail)
     | Unix.WEXITED n -> Eval_unusable (Printf.sprintf "eval exited %d: %s" n detail)
     | Unix.WSIGNALED n | Unix.WSTOPPED n ->
       Eval_unusable (Printf.sprintf "eval killed by signal %d" n))
;;
