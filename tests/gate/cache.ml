(* Content-addressed oracle cache (DESIGN.md §8). Lean is orders of magnitude slower than
   a solver diff, so every certification is memoised.

   Key = SHA-256 of canonical-query ‖ claim ‖ model ‖ encoding-version ‖ lean-version ‖
   grind-config. Folding the toolchain identifiers into the key (rather than checking them
   after lookup) keeps the cache monotonic: a new encoder or Lean version simply produces
   new keys; nothing is overwritten or silently re-certified (DESIGN.md §8). One
   s-expression file per entry.

   Timeouts are never cached (they may pass with a larger budget — nightly queue).
   Honeypot certifications are never cached (caller's responsibility). *)

let default_dir () =
  match Sys.getenv_opt "OXSMT_CACHE" with
  | Some d -> d
  | None -> "/usr/local/home/jujacobs/oxsmt/cache"
;;

let rec mkdir_p dir =
  if not (Sys.file_exists dir)
  then (
    let parent = Filename.dirname dir in
    if String.length parent < String.length dir then mkdir_p parent;
    try Unix.mkdir dir 0o755 with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ())
;;

(* Entry files are [<hash>.sexp]; atomic writes stage as [<hash>.sexp.tmp.<pid>] (see
   [store]). This infix identifies a staging file and nothing else. *)
let temp_infix = ".sexp.tmp."

let contains ~needle s =
  let nl = String.length needle
  and sl = String.length s in
  let rec loop i = i + nl <= sl && (String.sub s i nl = needle || loop (i + 1)) in
  nl = 0 || loop 0
;;

(* Remove staging files a crashed writer may have left behind. Scoped strictly to [dir]
   and to names carrying [temp_infix]; committed [<hash>.sexp] entries and anything else
   are never touched. *)
let sweep_orphan_temps dir : int =
  if not (Sys.file_exists dir && Sys.is_directory dir)
  then 0
  else
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> contains ~needle:temp_infix f)
    |> List.fold_left
         (fun n f ->
            try
              Sys.remove (Filename.concat dir f);
              n + 1
            with
            | _ -> n)
         0
;;

type key =
  { hash : string
  ; query_hash : string
  }

let compose ~canonical ~claim ~model ~lean_version : key =
  let composite =
    String.concat
      "\x00"
      [ canonical
      ; claim
      ; model
      ; Encoder.encoding_version
      ; lean_version
      ; Encoder.grind_config
      ]
  in
  { hash = Sha256.hex_digest composite; query_hash = Sha256.hex_digest canonical }
;;

(* Quote an atom for the entry file if it is empty or contains a byte the reader treats as
   a delimiter. The trigger covers ALL whitespace/control bytes ([c <= ' ']: space, tab,
   CR, LF, …) plus [( ) ; |] — so a value with an interior tab/CR round-trips as one
   quoted atom instead of being silently split (review MEDIUM cache.ml:84). The one
   residual lossiness is INTENTIONAL: an interior [|] is normalized to [/] because [|…|]
   has no escape grammar and [/] keeps the closing delimiter unambiguous. This only ever
   touches a [detail] string (a human diagnostic, not part of any verdict), so a proper
   escape scheme would be over-engineering; the classification (the [outcome] tag) is
   always delimiter-free and round-trips exactly. *)
let atom s =
  let needs_quote =
    String.length s = 0
    || String.exists (fun c -> c <= ' ' || c = '(' || c = ')' || c = ';' || c = '|') s
  in
  if needs_quote
  then "|" ^ String.map (fun c -> if c = '|' then '/' else c) s ^ "|"
  else s
;;

(* Decode an outcome tag. [None] on an UNRECOGNIZED tag — a corrupted/garbled tag (e.g.
   [REFUTEX], a one-byte flip of [REFUTED]) MUST NOT be silently reclassified: the old
   [_ -> Inconclusive …] default turned a corrupted ship-stopper into a benign cache Hit
   that a GREEN run permits (review CRITICAL cache.ml:101). The caller ([lookup]) maps
   [None] to [Unreadable] (counted, re-certified via Lean) — never a Hit of any class. *)
let outcome_of tag detail : Outcome.t option =
  match tag with
  | "CERTIFIED" -> Some Certified
  | "REFUTED" -> Some (Refuted detail)
  | "INCONCLUSIVE" -> Some (Inconclusive detail)
  | "ENCODE_ERROR" -> Some (Encode_error detail)
  | "MALFORMED" -> Some (Malformed detail)
  | "UNSUPPORTED" -> Some (Unsupported detail)
  | "NO_STATUS" -> Some No_status
  | _ -> None
;;

let path dir (k : key) = Filename.concat dir (k.hash ^ ".sexp")

(* Cache-entry reader — deliberately INDEPENDENT of the SMT-LIB reader ({!Sexp}, now on
   the shared §3.1 lexer). The entry file format is not SMT-LIB: a 64-hex
   [key]/[query-hash] is a digit-leading token with hex letters (e.g. [2f5d…]) that the
   shared lexer's numeral rule rejects ("malformed numeral"), which — via a swallowed
   lookup — turned ~86% of entries into perpetual cache misses (review R1). This restores
   the N-version separation the migration incidentally removed for the cache format, and
   matches the trivial grammar {!store} actually writes: whitespace separates tokens;
   [(]/[)] nest; a bare atom runs to the next whitespace/[(]/[)]/[|]; a [|…|] atom is
   verbatim up to the next [|] ({!store}'s {!atom} maps any inner [|] to [/], so the
   closing delimiter is unambiguous). No numeral rule, so a digit-leading hash is just an
   atom. *)
exception Parse_error of string

type tok =
  | LP
  | RP
  | ATOM of string

let tokenize src : tok list =
  let n = String.length src in
  let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  let is_break c = is_ws c || c = '(' || c = ')' || c = '|' in
  let out = ref [] in
  let i = ref 0 in
  while !i < n do
    let c = src.[!i] in
    if is_ws c
    then incr i
    else if c = '('
    then (
      out := LP :: !out;
      incr i)
    else if c = ')'
    then (
      out := RP :: !out;
      incr i)
    else if c = '|'
    then (
      incr i;
      let b = Buffer.create 16 in
      while !i < n && src.[!i] <> '|' do
        Buffer.add_char b src.[!i];
        incr i
      done;
      if !i >= n then raise (Parse_error "unterminated |quoted| atom");
      incr i;
      out := ATOM (Buffer.contents b) :: !out)
    else (
      let b = Buffer.create 16 in
      while !i < n && not (is_break src.[!i]) do
        Buffer.add_char b src.[!i];
        incr i
      done;
      out := ATOM (Buffer.contents b) :: !out)
  done;
  List.rev !out
;;

(* Parse the WHOLE input as exactly one [(entry (name value) …)] form and return its
   (name, value) pairs. STRICT (review CRITICAL cache.ml:170): the entry's closing [)]
   must be the LAST token — any trailing form (e.g. a second [(entry …)] appended after a
   truncated first one) is rejected, not silently ignored. Each field is exactly [(] name
   value [)] with an atom value ({!store} only ever writes atoms); anything else raises.
   Schema/field-value validation is the caller's job ({!lookup}). *)
let read_fields src : (string * string) list =
  match tokenize src with
  | LP :: ATOM "entry" :: rest ->
    let rec fields acc = function
      | [ RP ] -> List.rev acc (* the entry's closing paren AND end of input *)
      | LP :: ATOM name :: ATOM value :: RP :: tl -> fields ((name, value) :: acc) tl
      | [] -> raise (Parse_error "missing closing ')' for (entry ...)")
      | RP :: _ -> raise (Parse_error "trailing content after (entry ...)")
      | _ -> raise (Parse_error "malformed (name value) field")
    in
    fields [] rest
  | _ -> raise (Parse_error "expected a single (entry ...)")
;;

(* The content fields {!store} writes, in a FIXED order — the preimage of the [integrity]
   digest below. *)
let content_fields =
  [ "key"
  ; "query-hash"
  ; "claim"
  ; "outcome"
  ; "detail"
  ; "encoding-version"
  ; "grind-config"
  ; "timestamp"
  ]
;;

(* An entry carries the {!content_fields} PLUS an [integrity] digest over their values.
   The digest binds the OUTCOME (and detail) to the entry, closing the last false-GREEN
   hole (codex round-3): identity validation ties an entry to its KEY, but nothing tied
   the CERTIFICATION RESULT to that identity, so flipping
   [(outcome REFUTED)]→[(outcome CERTIFIED)] in an otherwise-valid entry was trusted,
   dropping a ship-stopper under GREEN. {!store} writes [integrity = SHA-256] of the
   content values ([content_fields] order); {!lookup} recomputes it from the read-back
   fields and rejects a mismatch as [Unreadable] (re-certified via Lean). This defeats
   accidental corruption AND naive tampering. RESIDUAL, stated honestly (see
   tests/gate/NOTES.md): with no secret in the TCB, a keyless digest cannot stop a
   determined same-UID adversary who edits a field and RECOMPUTES the digest — an in-file
   MAC would need an embedded "secret" (security theater). The systemic backstops are the
   documented trust assumption (the cache dir is trusted local state) and the nightly
   cache-audit intent (re-certify N random hits, alarm on mismatch). *)
let expected_fields = content_fields @ [ "integrity" ]

let content_digest fields =
  Sha256.hex_digest
    (String.concat "\x00" (List.map (fun n -> List.assoc n fields) content_fields))
;;

(* The three outcomes of a lookup, kept DISTINCT so the driver can count a corrupted /
   unreadable EXISTING entry separately from a genuine cold miss: the former is a broken
   mechanism (review's "green gate masking a broken cache"), the latter is normal. Lookup
   stays fail-safe either way — neither returns a certification the file did not contain. *)
type lookup_result =
  | Hit of Outcome.t
  | Absent (* no entry file present — a genuine cold/absent key *)
  | Unreadable of
      string (* entry file present but unparseable — corruption / format break *)

(* Look up the entry for [k] and the requested [claim], VALIDATING it before trusting its
   outcome. A file existing at [<k.hash>.sexp] is necessary but NOT sufficient: the entry
   must carry exactly {!expected_fields} (each once) and its identity fields must match
   what we are asking for — [key]=[k.hash], [query-hash]=[k.query_hash], [claim], and the
   current [encoding-version]/[grind-config]. This closes the false-GREEN hole (review
   CRITICAL cache.ml:170) where a hand-crafted [(entry (outcome CERTIFIED))] at the
   computed path was trusted. Any schema/identity mismatch, or an unrecognized outcome
   tag, → [Unreadable] (counted, re-certified via Lean) — never a [Hit]. [key]=[k.hash]
   already binds the full composite cryptographically
   (canonical‖claim‖model‖enc‖lean‖grind, cache.ml:76); the explicit per-field checks are
   defense in depth against corruption/tampering. *)
let lookup ~dir ~claim (k : key) : lookup_result =
  let file = path dir k in
  if not (Sys.file_exists file)
  then Absent
  else (
    match read_fields (Lean_runner.read_file file) with
    | exception Parse_error m -> Unreadable m
    | exception e -> Unreadable (Printexc.to_string e)
    | fields ->
      let names = List.map fst fields in
      let schema_ok =
        List.sort String.compare names = List.sort String.compare expected_fields
      in
      let field_is n v = List.assoc_opt n fields = Some v in
      if not schema_ok
      then Unreadable "cache entry schema mismatch (missing/extra/duplicate field)"
      else if not (field_is "key" k.hash)
      then Unreadable "cache entry key does not match the requested key"
      else if not (field_is "query-hash" k.query_hash)
      then Unreadable "cache entry query-hash mismatch"
      else if not (field_is "claim" claim)
      then Unreadable "cache entry claim mismatch"
      else if not (field_is "encoding-version" Encoder.encoding_version)
      then Unreadable "cache entry encoding-version mismatch"
      else if not (field_is "grind-config" Encoder.grind_config)
      then Unreadable "cache entry grind-config mismatch"
      else if not (field_is "integrity" (content_digest fields))
      then
        (* Integrity binds outcome+detail to the entry: a flipped/corrupted content field
           whose digest was not recomputed fails here → the ship-stopper survives. *)
        Unreadable "cache entry integrity mismatch (outcome/detail tampered or corrupted)"
      else (
        let tag = List.assoc "outcome" fields in
        let detail = List.assoc "detail" fields in
        match outcome_of tag detail with
        | Some o -> Hit o
        | None -> Unreadable (Printf.sprintf "unrecognized outcome tag %S" tag)))
;;

let store ~dir (k : key) ~claim (outcome : Outcome.t) : unit =
  mkdir_p dir;
  let content =
    [ "key", k.hash
    ; "query-hash", k.query_hash
    ; "claim", claim
    ; "outcome", Outcome.tag outcome
    ; "detail", Outcome.detail outcome
    ; "encoding-version", Encoder.encoding_version
    ; "grind-config", Encoder.grind_config
    ; "timestamp", Printf.sprintf "%.0f" (Unix.time ())
    ]
  in
  (* Bind outcome+detail to the entry (see {!content_digest}); written as the last field. *)
  let fields = content @ [ "integrity", content_digest content ] in
  let buf = Buffer.create 256 in
  Buffer.add_string buf "(entry\n";
  List.iter
    (fun (n, v) -> Buffer.add_string buf (Printf.sprintf "  (%s %s)\n" n (atom v)))
    fields;
  Buffer.add_string buf ")\n";
  (* Atomic publish: write a private temp file in the same dir, then rename over the final
     path. rename(2) within a directory is atomic, so a concurrent reader (or a racing
     writer for the same key) never observes a torn entry. *)
  let final = path dir k in
  let tmp = Printf.sprintf "%s.tmp.%d" final (Unix.getpid ()) in
  Lean_runner.write_file tmp (Buffer.contents buf);
  Unix.rename tmp final
;;
