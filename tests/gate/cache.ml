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

(* Quote an atom for the entry file if it is empty or contains delimiters. *)
let atom s =
  let needs_quote =
    String.length s = 0
    || String.exists
         (fun c -> c = ' ' || c = '(' || c = ')' || c = '\n' || c = ';' || c = '|')
         s
  in
  if needs_quote
  then "|" ^ String.map (fun c -> if c = '|' then '/' else c) s ^ "|"
  else s
;;

let outcome_of tag detail : Outcome.t =
  match tag with
  | "CERTIFIED" -> Certified
  | "REFUTED" -> Refuted detail
  | "INCONCLUSIVE" -> Inconclusive detail
  | "ENCODE_ERROR" -> Encode_error detail
  | "MALFORMED" -> Malformed detail
  | "UNSUPPORTED" -> Unsupported detail
  | "NO_STATUS" -> No_status
  | other -> Inconclusive ("unknown cached tag: " ^ other)
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

(* Parse the [(entry (name value) …)] shape into its (name, value) pairs. Each field is
   exactly [(] name value [)] with an atom value ({!store} only ever writes atoms). *)
let read_fields src : (string * string) list =
  match tokenize src with
  | LP :: ATOM "entry" :: rest ->
    let rec fields acc = function
      | RP :: _ -> List.rev acc (* the entry's closing paren; ignore any trailing bytes *)
      | LP :: ATOM name :: ATOM value :: RP :: tl -> fields ((name, value) :: acc) tl
      | [] -> raise (Parse_error "missing closing ')' for (entry ...)")
      | _ -> raise (Parse_error "malformed (name value) field")
    in
    fields [] rest
  | _ -> raise (Parse_error "expected (entry ...)")
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

let lookup ~dir (k : key) : lookup_result =
  let file = path dir k in
  if not (Sys.file_exists file)
  then Absent
  else (
    match
      let src = Lean_runner.read_file file in
      let fields = read_fields src in
      List.assoc_opt "outcome" fields, List.assoc_opt "detail" fields
    with
    | Some tag, detail -> Hit (outcome_of tag (Option.value detail ~default:""))
    | None, _ -> Unreadable "entry has no (outcome ...) field"
    | exception Parse_error m -> Unreadable m
    | exception e -> Unreadable (Printexc.to_string e))
;;

let store ~dir (k : key) ~claim (outcome : Outcome.t) : unit =
  mkdir_p dir;
  let fields =
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
