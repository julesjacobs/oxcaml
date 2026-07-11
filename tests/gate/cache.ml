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

let lookup ~dir (k : key) : Outcome.t option =
  let file = path dir k in
  if not (Sys.file_exists file)
  then None
  else (
    try
      let src = Lean_runner.read_file file in
      let sexps = Sexp.parse_many src in
      let field name =
        let rec find = function
          | Sexp.List [ Sexp.Atom n; Sexp.Atom v ] :: _ when String.equal n name -> Some v
          | _ :: tl -> find tl
          | [] -> None
        in
        match sexps with
        | [ Sexp.List (Sexp.Atom "entry" :: fields) ] -> find fields
        | _ -> None
      in
      match field "outcome", field "detail" with
      | Some tag, detail -> Some (outcome_of tag (Option.value detail ~default:""))
      | None, _ -> None
    with
    | _ -> None)
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
