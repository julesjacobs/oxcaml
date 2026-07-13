(* Minimal s-expression reader for the test harness.

   Deliberately small and stdlib-only. It is NOT the solver's SMT-LIB parser (that lives
   under smt/smtlib and is owned by M0-smtlib); the harness must not depend on smt/ code.
   This reader handles just enough SMT-LIB2 lexical syntax to (a) locate
   [(set-info :status ...)] and [(check-sat)] commands in a .smt2 file, and (b) parse the
   solver's machine-readable output blocks.

   Double-quoted STRINGS collapse to plain atoms on the way in (harmless — strings never
   appear in a model). A pipe-|quoted symbol|, by contrast, is kept as a DISTINCT [Quoted]
   node that remembers it arrived quoted: a model payload token (a function-table case
   result, a const value) is legitimately only a bare numeral / [true]/[false] / [(- n)],
   so a QUOTED payload token is a malformed model. Preserving the [Quoted] kind lets
   [to_string] re-emit the bars verbatim rather than laundering [|true|] into a valid bare
   [true] — the eval reader (which has its own [Quoted] and rejects a quoted value token)
   then fails the model closed, instead of the harness silently repairing a solver
   regression. A [Quoted] in a NAME slot is re-derived to the canonical quoting by
   [quote_symbol] on output, so names stay faithful either way. *)

type t =
  | Atom of string
  | Quoted of string
  | List of t list

exception Parse_error of string

(* Render an s-expression back to text. Used for canonicalizing model values in golden
   output. A [Quoted] re-emits its [|bars|] verbatim (faithful carrier: a quoted token is
   never normalized to a bare atom). *)
let rec to_string = function
  | Atom a -> a
  | Quoted s -> "|" ^ s ^ "|"
  | List l -> "(" ^ String.concat " " (List.map to_string l) ^ ")"
;;

let is_delim = function
  | ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"' | '|' -> true
  | _ -> false
;;

(* Re-quote a symbol for output — the inverse of [read_pipe]. Our [t] does not record
   whether an atom arrived bare or |quoted| (read_pipe keeps the content, drops the bars),
   so on output we re-derive the need for quoting from the characters: a symbol that is
   not a valid SMT-LIB 2.6 *simple* symbol (§3.1) is wrapped in |...|, otherwise it would
   re-lex as several tokens (e.g. [a b] as two atoms, breaking the [(const NAME VALUE)]
   grammar the eval bridge emits). We hand-roll the rule rather than reuse smt/smtlib's
   printer or smt/lexical because the harness must not depend on smt/ code (see header).
   Reserved words are not specially quoted: the sidecar/golden grammars are positional, so
   only tokenization matters here. *)
let is_simple_symbol_char = function
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
  | '-'
  | '+'
  | '='
  | '<'
  | '>'
  | '.'
  | '?'
  | '/' -> true
  | _ -> false
;;

let quote_symbol s =
  let simple =
    s <> ""
    && (match s.[0] with
        | '0' .. '9' -> false (* a simple symbol may not start with a digit *)
        | _ -> true)
    && String.for_all is_simple_symbol_char s
  in
  if simple then s else "|" ^ s ^ "|"
;;

(* Parse every top-level s-expression in [s], in order. Comments (';' to end-of-line),
   double-quoted strings (with "" escaping, per SMT-LIB 2.6), and |quoted symbols| are all
   recognized so we never choke on real benchmarks. String and quoted-symbol contents
   become plain atoms; the harness only ever inspects bare atoms, so that loss is
   harmless. *)
let parse_all (s : string) : t list =
  let n = String.length s in
  let pos = ref 0 in
  let peek () = if !pos < n then Some s.[!pos] else None in
  let adv () = incr pos in
  let rec skip_ws () =
    match peek () with
    | Some (' ' | '\t' | '\n' | '\r') ->
      adv ();
      skip_ws ()
    | Some ';' ->
      adv ();
      let rec to_eol () =
        match peek () with
        | Some '\n' -> adv ()
        | Some _ ->
          adv ();
          to_eol ()
        | None -> ()
      in
      to_eol ();
      skip_ws ()
    | _ -> ()
  in
  let read_string () =
    adv ();
    (* consume opening quote *)
    let b = Buffer.create 16 in
    let rec loop () =
      match peek () with
      | None -> raise (Parse_error "unterminated string literal")
      | Some '"' ->
        adv ();
        (match peek () with
         | Some '"' ->
           Buffer.add_char b '"';
           adv ();
           loop ()
         | _ -> ())
      | Some c ->
        Buffer.add_char b c;
        adv ();
        loop ()
    in
    loop ();
    Atom (Buffer.contents b)
  in
  let read_pipe () =
    adv ();
    (* consume opening | *)
    let b = Buffer.create 16 in
    let rec loop () =
      match peek () with
      | None -> raise (Parse_error "unterminated |quoted symbol|")
      | Some '|' -> adv ()
      | Some c ->
        Buffer.add_char b c;
        adv ();
        loop ()
    in
    loop ();
    Quoted (Buffer.contents b)
  in
  let read_atom () =
    let b = Buffer.create 16 in
    let rec loop () =
      match peek () with
      | Some c when not (is_delim c) ->
        Buffer.add_char b c;
        adv ();
        loop ()
      | _ -> ()
    in
    loop ();
    Atom (Buffer.contents b)
  in
  let rec read_sexp () =
    skip_ws ();
    match peek () with
    | None -> None
    | Some '(' ->
      adv ();
      Some (read_list [])
    | Some ')' -> raise (Parse_error "unexpected ')'")
    | Some '"' -> Some (read_string ())
    | Some '|' -> Some (read_pipe ())
    | Some _ -> Some (read_atom ())
  and read_list acc =
    skip_ws ();
    match peek () with
    | None -> raise (Parse_error "unterminated list")
    | Some ')' ->
      adv ();
      List (List.rev acc)
    | Some _ ->
      (match read_sexp () with
       | Some e -> read_list (e :: acc)
       | None -> raise (Parse_error "end of input inside list"))
  in
  let rec all acc =
    match read_sexp () with
    | Some e -> all (e :: acc)
    | None -> List.rev acc
  in
  all []
;;
