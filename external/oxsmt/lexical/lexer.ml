(* SMT-LIB 2.6 §3.1 lexer. One pass over the input; token kind is preserved exactly (see
   lexer.mli). Section numbers below cite the SMT-LIB 2.6 standard, §3.1 "Lexicon". *)

type token =
  | Lparen
  | Rparen
  | Numeral of string
  | Decimal of string
  | Hex of string
  | Binary of string
  | String of string
  | Keyword of string
  | Symbol of
      { text : string
      ; quoted : bool
      }
  | Reserved of string

exception Error of string

(* §3.1 reserved words. Quoting escapes reserved-ness: [|let|] is an ordinary symbol. Note
   Core/Ints operators ([and], [+], [true], …) are NOT lexically reserved — they are
   ordinary symbols the grammar layer interprets. *)
let reserved_words =
  [ "_"
  ; "!"
  ; "as"
  ; "let"
  ; "exists"
  ; "forall"
  ; "match"
  ; "par"
  ; "BINARY"
  ; "DECIMAL"
  ; "HEXADECIMAL"
  ; "NUMERAL"
  ; "STRING"
  ]
;;

(* Membership in [reserved_words], but as a constant-time [match] rather than a linear
   [List.mem] (which does a polymorphic string compare against up to 13 entries for EVERY
   symbol token). The compiler lowers this to a length/byte decision tree. This MUST list
   exactly the strings in [reserved_words] above; a divergence changes the token stream
   and is caught by the lexer round-trip/fuzz tests and any counted-identity A/B. *)
let is_reserved_word = function
  | "_"
  | "!"
  | "as"
  | "let"
  | "exists"
  | "forall"
  | "match"
  | "par"
  | "BINARY"
  | "DECIMAL"
  | "HEXADECIMAL"
  | "NUMERAL"
  | "STRING" -> true
  | _ -> false
;;

(* §3.1 ⟨symbol⟩ constituent characters of a simple symbol. *)
let is_symbol_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | '~'
  | '!'
  | '@'
  | '$'
  | '%'
  | '^'
  | '&'
  | '*'
  | '_'
  | '+'
  | '='
  | '<'
  | '>'
  | '.'
  | '?'
  | '/'
  | '-' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_hex = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false
;;

let is_simple_symbol s =
  String.length s > 0 && (not (is_digit s.[0])) && String.for_all is_symbol_char s
;;

(* §3.1 ⟨numeral⟩ is [0] or [[1-9][0-9]*]. We accept any nonempty digit run (leading zeros
   included): real benchmark/tool output emits [007], and the KIND (numeral) is what
   matters for the boundary invariant, not leading-zero rejection. Documented deviation. *)

type pos =
  { mutable idx : int
  ; mutable line : int
  ; mutable col : int
  }

let errorf pos fmt =
  Printf.ksprintf
    (fun s -> raise (Error (Printf.sprintf "line %d, col %d: %s" pos.line pos.col s)))
    fmt
;;

let peek s pos = if pos.idx < String.length s then Some s.[pos.idx] else None

let advance s pos =
  let c = s.[pos.idx] in
  pos.idx <- pos.idx + 1;
  if Char.equal c '\n'
  then (
    pos.line <- pos.line + 1;
    pos.col <- 1)
  else pos.col <- pos.col + 1
;;

(* Scan a maximal run of chars satisfying [pred] from [pos.idx], advance [pos] over the
   run in one step, and return the run as a single substring (FE2 L5). This replaces the
   per-char [peek] (which boxes a [Some] per byte) → [Buffer.add_char] → [advance] loop
   the token readers used, for the token classes whose runs contain NO newline (simple
   symbols and numerals): [line] is therefore unchanged and [col] advances by the run
   length, so the result — bytes, final [pos.idx], and [pos.line]/[pos.col] — is identical
   to the old loop. Not used for strings/quoted symbols, whose content is not a verbatim
   input substring. *)
let scan_run s pos pred =
  let start = pos.idx in
  let n = String.length s in
  let j = ref start in
  while !j < n && pred s.[!j] do
    incr j
  done;
  pos.idx <- !j;
  pos.col <- pos.col + (!j - start);
  String.sub s start (!j - start)
;;

let is_ws = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let rec skip_trivia s pos =
  match peek s pos with
  | Some c when is_ws c ->
    advance s pos;
    skip_trivia s pos
  | Some ';' ->
    (* §3.1 comment: [;] to end of line. *)
    let rec eol () =
      match peek s pos with
      | Some '\n' -> advance s pos
      | Some _ ->
        advance s pos;
        eol ()
      | None -> ()
    in
    eol ();
    skip_trivia s pos
  | _ -> ()
;;

(* §3.1 ⟨symbol⟩ quoted form: [|] then any char except [|] and [\], then [|]. No escapes —
   so a [|] always closes and a [\] is illegal; this is what makes a quoted symbol's bytes
   unable to forge a token boundary (ADR-0008). *)
let read_quoted s pos =
  advance s pos;
  (* consume opening '|' *)
  let buf = Buffer.create 16 in
  let rec loop () =
    match peek s pos with
    | None -> errorf pos "end of input inside |quoted| symbol"
    | Some '\\' -> errorf pos "backslash is not permitted inside a |quoted| symbol (§3.1)"
    | Some '|' ->
      advance s pos;
      Symbol { text = Buffer.contents buf; quoted = true }
    | Some c ->
      Buffer.add_char buf c;
      advance s pos;
      loop ()
  in
  loop ()
;;

(* §3.1 ⟨string⟩: delimited by double quotes, with a doubled quote denoting one quote. *)
let read_string s pos =
  advance s pos;
  (* consume opening '"' *)
  let buf = Buffer.create 16 in
  let rec loop () =
    match peek s pos with
    | None -> errorf pos "end of input inside string literal"
    | Some '"' ->
      advance s pos;
      (match peek s pos with
       | Some '"' ->
         Buffer.add_char buf '"';
         advance s pos;
         loop ()
       | _ -> String (Buffer.contents buf))
    | Some c ->
      Buffer.add_char buf c;
      advance s pos;
      loop ()
  in
  loop ()
;;

(* A maximal run of simple-symbol chars starting at a non-digit: a simple ⟨symbol⟩, or a
   ⟨reserved word⟩ if it is one of the fixed set. *)
let read_symbolish s pos =
  let text = scan_run s pos is_symbol_char in
  if is_reserved_word text then Reserved text else Symbol { text; quoted = false }
;;

(* Digit-leading: §3.1 ⟨numeral⟩ / ⟨decimal⟩. Read digits, then optional [.] digits. A
   trailing symbol char (e.g. [1a]) is a malformed numeral. *)
let read_number s pos =
  let int_part = scan_run s pos is_digit in
  let is_decimal, text =
    match peek s pos with
    | Some '.' ->
      advance s pos;
      let frac = scan_run s pos is_digit in
      if String.length frac = 0 then errorf pos "decimal has no fractional digits (§3.1)";
      true, int_part ^ "." ^ frac
    | _ -> false, int_part
  in
  (match peek s pos with
   | Some c when is_symbol_char c ->
     errorf pos "malformed numeral: unexpected %C after digits (§3.1)" c
   | _ -> ());
  if is_decimal then Decimal text else Numeral text
;;

let read_hash s pos =
  advance s pos;
  (* consume '#' *)
  match peek s pos with
  | Some ('x' | 'X') ->
    advance s pos;
    let buf = Buffer.create 8 in
    let rec loop () =
      match peek s pos with
      | Some c when is_hex c ->
        Buffer.add_char buf c;
        advance s pos;
        loop ()
      | _ -> ()
    in
    loop ();
    if Buffer.length buf = 0 then errorf pos "#x with no hex digits (§3.1)";
    Hex (Buffer.contents buf)
  | Some ('b' | 'B') ->
    advance s pos;
    let buf = Buffer.create 8 in
    let rec loop () =
      match peek s pos with
      | Some (('0' | '1') as c) ->
        Buffer.add_char buf c;
        advance s pos;
        loop ()
      | _ -> ()
    in
    loop ();
    if Buffer.length buf = 0 then errorf pos "#b with no binary digits (§3.1)";
    Binary (Buffer.contents buf)
  | _ -> errorf pos "expected #x or #b after '#' (§3.1)"
;;

(* §3.1 ⟨keyword⟩: [:] then a simple-symbol run. *)
let read_keyword s pos =
  advance s pos;
  (* consume ':' *)
  let buf = Buffer.create 16 in
  let rec loop () =
    match peek s pos with
    | Some c when is_symbol_char c ->
      Buffer.add_char buf c;
      advance s pos;
      loop ()
    | _ -> ()
  in
  loop ();
  Keyword (Buffer.contents buf)
;;

let tokenize (s : string) : token list =
  let pos = { idx = 0; line = 1; col = 1 } in
  let rec loop acc =
    skip_trivia s pos;
    match peek s pos with
    | None -> List.rev acc
    | Some c ->
      let tok =
        match c with
        | '(' ->
          advance s pos;
          Lparen
        | ')' ->
          advance s pos;
          Rparen
        | '|' -> read_quoted s pos
        | '"' -> read_string s pos
        | ':' -> read_keyword s pos
        | '#' -> read_hash s pos
        | c when is_digit c -> read_number s pos
        | c when is_symbol_char c -> read_symbolish s pos
        | c -> errorf pos "unexpected character %C (§3.1)" c
      in
      loop (tok :: acc)
  in
  loop []
;;

let kind = function
  | Lparen -> "lparen"
  | Rparen -> "rparen"
  | Numeral _ -> "numeral"
  | Decimal _ -> "decimal"
  | Hex _ -> "hex"
  | Binary _ -> "binary"
  | String _ -> "string"
  | Keyword _ -> "keyword"
  | Symbol { quoted = false; _ } -> "symbol"
  | Symbol { quoted = true; _ } -> "quoted-symbol"
  | Reserved _ -> "reserved"
;;
