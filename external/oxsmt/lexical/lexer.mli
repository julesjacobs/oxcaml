(** The one SMT-LIB 2.6 lexer (§3.1 lexicon), shared by {!Oxsmt_smtlib} (printer + parser)
    and — after task/gate3 — the gate reader. Stdlib-only.

    {b Headline invariant: token kind is never lost.} Every distinction §3.1 draws
    survives into the token type. In particular a {e quoted} symbol keeps [quoted = true]
    even when its text spells a numeral or a reserved word, so [|0|] is
    [Symbol {text="0"; quoted=true}] — {b never} confused with the numeral [0] — and
    [|let|] is a [Symbol], never the [Reserved] word [let]. Collapsing these was the
    [|0|]/cache-collision token-boundary bug family (ADR-0008). *)

(** SMT-LIB 2.6 §3.1 tokens. [Lparen]/[Rparen] are structural; the rest are lexemes. *)
type token =
  | Lparen
  | Rparen
  | Numeral of string (** §3.1 ⟨numeral⟩ (leading-zero-lenient; see .ml) *)
  | Decimal of string (** §3.1 ⟨decimal⟩ *)
  | Hex of string (** §3.1 ⟨hexadecimal⟩, digits after [#x] *)
  | Binary of string (** §3.1 ⟨binary⟩, bits after [#b] *)
  | String of string (** §3.1 ⟨string⟩, decoded (a doubled quote denotes one quote) *)
  | Keyword of string (** §3.1 ⟨keyword⟩, text after the [:] *)
  | Symbol of
      { text : string
      ; quoted : bool
      } (** §3.1 ⟨symbol⟩: simple ([quoted=false]) or [|…|] ([quoted=true]) *)
  | Reserved of string (** §3.1 reserved word ([let], [!], [as], …); only when unquoted *)

(** Lexically malformed input (unterminated quote/string, [\\] or [|] inside [|…|], a stray
    close paren is {e not} lexical — parens are tokens; bad numeral, unexpected byte).
    Carries a line/col. *)
exception Error of string

(** SMT-LIB 2.6 §3.1 reserved words. *)
val reserved_words : string list

(** [is_simple_symbol s] — §3.1 ⟨simple symbol⟩: nonempty, every char in the §3.1 set
    [[a-zA-Z0-9~!@$%^&*_+=<>.?/-]], not starting with a digit. *)
val is_simple_symbol : string -> bool

(** [tokenize s] lexes the whole string into tokens (whitespace and [;] comments dropped).
    Raises {!Error} on a lexical violation. *)
val tokenize : string -> token list

(** A short kind tag ([symbol]/[quoted-symbol]/[numeral]/…), for diagnostics and the
    differential fuzzer's kind-preservation check. *)
val kind : token -> string
