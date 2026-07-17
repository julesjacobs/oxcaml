(* SMT-LIB2 -> frozen-API terms, test-only. See parser.mli. All term construction threads
   one Context, so the smart constructors do the sort-checking and normalization for us;
   we only translate syntax and manage declarations + let-scopes. *)

open Oxsmt_core

exception Malformed of string
exception Unsupported of string

let malformedf fmt = Printf.ksprintf (fun s -> raise (Malformed s)) fmt
let unsupportedf fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

(* Dark flag for the front-end quantified pipeline (design basis: typed formula IR ->
   NNF/polarity -> Skolemization + definitional clausification -> lowering into the
   ground/lemma APIs). Read ONCE, so a process's routing is stable. Default OFF (unset or
   any non-truthy value) keeps the current hand-coded quantifier-shape classification,
   i.e. BYTE-IDENTICAL behavior; ON routes quantified assertions through {!Fol} (stage 2).
   RUNG 1 only defines the switch and the pure IR engine — no consumer yet, so ON = OFF
   for now. *)
let quant_pipeline_enabled =
  lazy
    (match Sys.getenv_opt "OXSMT_QUANT_PIPELINE" with
     | Some ("1" | "true" | "yes" | "on") -> true
     | Some _ | None -> false)
;;

(* Let-/qvar-binding scope. A persistent [String] map, NOT an association list: a deeply
   nested [let]-chain (thousands deep in the TPTP first-order model-finding families)
   makes an assoc-list scope O(references x nesting-depth) — the whole formula's term
   construction went quadratic (NEQ015_size6: 1.5MB / 32s just to build). A map keyed by
   the bound name is O(references x log depth) and returns the identical term for every
   well-formed input (innermost binding wins, exactly as first-match on the old prepended
   list), so the constructed term — hence every downstream verdict — is unchanged. *)
module Scope = Map.Make (String)

let name_of s =
  match Sexp.symbol_name s with
  | Some n -> n
  | None -> malformedf "expected a symbol name, got %s" (Sexp.to_string s)
;;

(* The internal function-symbol name minted for a constructor's tester [(_ is C)].
   Readable and deterministic; a user symbol colliding with it is caught as a
   redeclaration by [declare_fun]. *)
let tester_name_of cname = "is-" ^ cname

(* A universally-quantified assertion, parsed from [(assert (forall (binders) body))]. The
   parser cannot BUILD the lemma itself: the bound variables must be minted as cap-gated
   placeholder qvars through the {!Oxsmt_interface.Session} (ADR-0012 §1.3
   mint-before-build), which lives in a library this test-only parser must not depend on.
   So instead the parser records the binders it read and a deferred [build] closure: the
   driver mints one qvar per binder, hands their {!Term.t} images to [build] in binder
   order, and [build] reads the body (and any [:pattern] triggers) with each binder name
   bound to its qvar image. The body is read lazily inside [build] rather than eagerly,
   because it is only well-sorted once the qvar images exist. *)
(* A driver-supplied Skolem-FUNCTION minter (lemmas-climb chunk 2b). [skolem ~cod ~args]
   declares a FRESH uninterpreted function of rank [(sorts of args) -> cod] and returns it
   applied to [args]. It Skolemizes a POSITIVE-position [exists] nested in a [forall]
   body: each existential binder becomes a fresh function of the enclosing universals
   ([args] is the forall's qvar images), so the lemma stays universal and EQUISATISFIABLE
   with the original (standard Skolemization: an existential dominated only by universals
   [x] becomes a function [f x]). The parser cannot mint the fresh, collision-proof symbol
   itself (that authority is the driver's {!Oxsmt_interface.Session}), so it is threaded
   into [build] like the qvar images. *)
type skolemizer = cod:Sort.t -> args:Term.t list -> Term.t

type lemma_src =
  { qvars :
      (string * Sort.t) list (* forall binders, outer-first then inner (flattened) *)
  ; build : skolem:skolemizer -> Term.t array -> Term.t * Term.t list list
  (* [build ~skolem qvar_images] is [(body, triggers)]: [qvar_images.(k)] is the term to
     substitute for the k-th binder; [skolem] mints a fresh Skolem function for a positive
     [exists] in the body (lemmas-climb chunk 2b). May raise {!Malformed}/{!Unsupported}
     (a body op outside the subset), which the driver maps to a sound [unknown]. *)
  }

(* A top-level EXISTENTIAL assertion, [(assert (exists (binders) body))] in a POSITIVE
   position (lemmas-climb chunk 2a). Skolemized: the binders become fresh ground witnesses
   (uninterpreted constants) and the body is asserted over them. Equisatisfiable with the
   original assertion set (sound in BOTH directions), so it is a real assertion, not a
   drop. Like {!lemma_src} the parser cannot mint the witnesses itself (fresh,
   collision-proof symbols come from the driver's {!Oxsmt_interface.Session}); it records
   the binders and a deferred [ex_build] that reads the body with each binder bound to the
   witness term the driver supplies (in binder order). Only produced for an [exists] the
   parser sees at a positive assertion position (root or a top-level [(and ...)] conjunct)
   — never under a negation, where Skolemizing to a constant would be UNSOUND. *)
type exists_src =
  { ex_qvars : (string * Sort.t) list (* exists binders, flattened, outer-first *)
  ; ex_build : Term.t array -> Term.t
  (* [ex_build witnesses] is the Bool body with binder [k] -> [witnesses.(k)] (a fresh
     ground constant). May raise {!Malformed}/{!Unsupported} when the body is outside the
     subset (e.g. a nested [forall]); the driver drops it with the sat-degrade sentinel. *)
  }

(* A binder-keyed Skolem minter for the pipeline: [skolem ~key ~cod ~args] mints (or
   REUSES, memoized by [~key] = the eliminated existential's binder id) a Skolem function
   of the [args] sorts applied to [args] (0-ary => a witness constant). Keying by binder
   id is load-bearing: one existential referenced by several clauses must share ONE symbol
   (a split [exists x. (p x /\ q x)] otherwise gets two witnesses -> wrong sat). Distinct
   from {!skolemizer} (the OFF chunk-2b seam, deliberately fresh-per-call). *)
type keyed_skolemizer = key:int -> cod:Sort.t -> args:Term.t list -> Term.t

(* A lowered clause from the front-end quantified pipeline (dark: [OXSMT_QUANT_PIPELINE]).
   [cl_qvars] are the universal binders ([] = a GROUND clause, lowered via a plain assert,
   not a live lemma); [cl_build ~skolem qvar_images] is [(body, triggers)]. Skolem symbols
   come from the {!keyed_skolemizer} seam (memoized by binder id, so a split existential
   shares one witness). May raise
   {!Malformed}/{!Unsupported}/{!Term.Unsupported}/{!Term.Overflow} when a leaf is outside
   the fragment — the loader drops that clause and arms the sentinel. *)
type clause =
  { cl_qvars : (string * Sort.t) list
  ; cl_build : skolem:keyed_skolemizer -> Term.t array -> Term.t * Term.t list list
  ; cl_source : Sexp.t
      (* the source assertion body this clause was clausified from — the provenance root
         for an audit dump / certificate replay (ADR-0013 seam). *)
  ; cl_skolems : (string * int list) list
  (* Skolem provenance: each eliminated existential's source binder name paired with its
     dependency list (the dominating universal binder ids it is a function of). Together
     with {!cl_qvars} and {!cl_source} this records why the clause is equisatisfiable with
     the source (Skolemization witness), for the cert-replay seam. *)
  }

type t =
  { env : Env.t
  ; ctx : Context.t
  ; logic : string option
  ; status : Oxsmt_smtlib.Status.t option
  ; assertions : Term.t list
  ; datatypes : Datatype_defs.t
      (* the algebraic-datatype shape declared by [declare-datatype(s)], for the datatype
         theory; empty when the query declares none *)
  ; arrays : Array_defs.t
      (* the array [select]/[store] symbols used, for the arrays theory; empty when the
         query uses no arrays *)
  ; lemmas : lemma_src list (* the [(assert (forall ...))] assertions, in file order *)
  ; existentials : exists_src list
      (* top-level POSITIVE [(assert (exists ...))] assertions the loader Skolemizes into
         fresh ground witnesses (lemmas-climb chunk 2a), in file order *)
  ; clauses : clause list
      (* front-end quantified pipeline (dark: [OXSMT_QUANT_PIPELINE]) output: the clauses
         a quantifier-bearing assertion was clausified into (NNF -> Skolemize -> lower).
         Empty when the flag is OFF (byte-identical) — quantifiers then take {!lemmas}/
         {!existentials}. Ground (non-quantifier) assertions always take {!assertions}. *)
  ; dropped : int
  (* count of assertion content the reader could not represent and dropped via partial
     assertion (lemmas-climb); [> 0] means the loader must arm the sat-degrade sentinel *)
  }

type fundecl =
  { sym : Symbol.t
  ; dom : Sort.t list
  }

(* A [define-fun] macro: parameters (name + sort), declared result sort, and the body as
   an unread s-expression. The body is expanded (capture-avoidingly) at each use site,
   never at definition time — see [expand]. *)
type definition =
  { params : (string * Sort.t) list
  ; ret : Sort.t
  ; body : Sexp.t
  }

type pstate =
  { ctx : Context.t
  ; env : Env.t
  ; sorts : (string, Symbol.t) Hashtbl.t
  ; funs : (string, fundecl) Hashtbl.t
  ; defines : (string, definition) Hashtbl.t
  ; expanding :
      (string, unit) Hashtbl.t (* define names currently mid-expansion (cycle guard) *)
  ; memo : (string * int list, Term.t) Hashtbl.t
      (* expansion cache keyed by (define name, argument-term tags). A define with the
         same arguments always expands to the same hash-consed term, so this turns the
         exponential body re-read on nested chains (e.g. [f_{i+1}(x) = f_i(x) + f_i(x)])
         into linear work. Tags are the [Context] hash-cons identity, so the key is exact
         and cheap. *)
  ; dt_names : (string, unit) Hashtbl.t
      (* sort names introduced by [declare-datatype(s)]: [sort_of_sexp] resolves these to
         [Sort.datatype_] rather than [Sort.uninterpreted] *)
  ; mutable datatypes : Datatype_defs.t (* the accumulated datatype shape registry *)
  ; internal_mint : Internal_minter.t option
      (* board #58 O-MINTER: mints a theory-internal reserved symbol ([.oxsmt.<theory>.*])
         mid-parse. Some internal symbols cannot be pre-minted at a declaration site:
         arrays op symbols are per-(index sort, element sort) instantiations discovered
         only at the first [select]/[store] use. Supplied by the parser's OWNER as an
         OPAQUE {!Oxsmt_core.Internal_minter.t} — a [Session]-driven parse threads
         [Oxsmt_interface.Session.parse_minter], which wraps [Env.declare_reserved] over
         the session's private cap behind an [admit] gate, so the parser can mint a
         collision-proof sanctioned marker WITHOUT ever holding the cap or a general
         closure (ADR-0012: only [Session] holds the cap). [None] (a driver that threads
         no [~internal_mint]) means no cap-backed minter: {!internal_mint} then raises
         [Malformed] rather than silently succeeding. The bit-vector builders
         ({!Oxsmt_core.Bv}) mint their [.oxsmt.bv.*] markers through this and the arrays
         [array_op_sym] its [.oxsmt.arr.*] ones. *)
  ; array_ops : (string, Symbol.t) Hashtbl.t
      (* the monomorphic [select]/[store] symbols minted per (role, index, element)
         instantiation, keyed by a deterministic string; arrays are polymorphic so each
         instantiation gets its own symbol with a concrete rank *)
  ; mutable arrays : Array_defs.t (* the accumulated array select/store symbol registry *)
  }

module Tok = Oxsmt_lexical.Lexer

(* Get-or-mint a theory-internal reserved symbol mid-parse via the owner-supplied opaque
   {!Oxsmt_core.Internal_minter.t} (board #58 O-MINTER). Callers go through here instead
   of [Env.declare_fun st.env], which rejects the reserved [.oxsmt.*] namespace: the
   bit-vector builders ({!Oxsmt_core.Bv}) mint their [.oxsmt.bv.*] operator/literal
   symbols through [(internal_mint st)], and the arrays branch's [array_op_sym] its
   [.oxsmt.arr.*] ones. With no minter supplied, degrade to [Malformed] (a sound unknown),
   never a silent success; [Internal_minter.mint] itself raises if the name is outside the
   minter's [admit] grammar. *)
let internal_mint st name rank =
  match st.internal_mint with
  | Some m -> Internal_minter.mint m name rank
  | None ->
    malformedf
      "internal symbol %s requires a cap-backed minter (parse this through a Session)"
      name
;;

(* ---- bit-vector width cap (rider #19) ---- *)

(* Upper bound on any bit-vector width the reader will CONSTRUCT. The eager bit-blaster
   allocates ~width SAT literals per bv term, and a repeat/extend/literal/BitVec width
   comes straight from user numerals — so an adversarial [((_ repeat 500000000) x)] or
   [(_ BitVec 1099511627776)] would allocate until the process is KILLED (an uncatchable
   out-of-memory abort during GC, NOT an exception a parse-scoped handler can absorb —
   fable MED-1). Bound it UP FRONT: a width over the cap degrades to [Malformed] ->
   unknown BEFORE any allocation. Real bit-vectors are far below this (a few thousand bits
   at most); the cap only rejects pathological inputs. Overridable via
   [OXSMT_MAX_BV_WIDTH] so a test can drive the cap without allocating a crash-sized term. *)
let max_bv_width =
  match Sys.getenv_opt "OXSMT_MAX_BV_WIDTH" with
  | Some s ->
    (match int_of_string_opt s with
     | Some n when n >= 1 -> n
     | _ -> 1 lsl 20)
  | None -> 1 lsl 20
;;

let check_bv_width what w =
  if w < 1 || w > max_bv_width
  then malformedf "%s: bitvector width %d out of range [1, %d]" what w max_bv_width
;;

(* [n*w] guarded against BOTH the cap and int overflow (an [(_ repeat n)] result width). *)
let checked_product ~what ~n ~w =
  if n < 1 || w < 1 || n > max_bv_width / w
  then malformedf "%s: bitvector width %d*%d exceeds the max %d" what n w max_bv_width;
  n * w
;;

(* ---- sorts ---- *)

let rec sort_of_sexp st (s : Sexp.t) : Sort.t =
  match Sexp.symbol_name s with
  (* [Bool]/[Int] are the builtin sorts regardless of quoting (quoting is lexical). *)
  | Some "Bool" -> Sort.bool
  | Some "Int" -> Sort.int
  | Some name ->
    (match Hashtbl.find_opt st.sorts name with
     | Some sym ->
       if Hashtbl.mem st.dt_names name then Sort.datatype_ sym else Sort.uninterpreted sym
     | None -> malformedf "unknown sort: %s" name)
  | None ->
    (match s with
     (* [(Array I E)] is the one compound sort v1 models: a functional, extensional array
        from index sort [I] to element sort [E]. Both are read recursively (so a nested
        [(Array I (Array J E))] works). Any other parametric/compound sort is out of the
        fragment (fail-closed to unknown). *)
     | Sexp.List [ head; i_s; e_s ] when Sexp.simple head = Some "Array" ->
       Sort.array_ ~index:(sort_of_sexp st i_s) ~element:(sort_of_sexp st e_s)
     (* [(_ BitVec n)] — the only indexed sort in the v1 subset. *)
     | Sexp.List
         [ Sexp.Atom (Tok.Reserved "_")
         ; Sexp.Atom (Tok.Symbol { text = "BitVec"; _ })
         ; Sexp.Atom (Tok.Numeral n)
         ] ->
       (match int_of_string_opt n with
        | Some w when w >= 1 ->
          check_bv_width "(_ BitVec n)" w;
          Sort.bitvec w
        | _ -> malformedf "(_ BitVec %s): width must be a positive integer" n)
     | Sexp.List _ ->
       unsupportedf "parametric/compound sorts are not supported: %s" (Sexp.to_string s)
     | _ -> malformedf "expected a sort, got %s" (Sexp.to_string s))
;;

(* Get-or-mint the monomorphic [select]/[store] symbol for one array instantiation and
   record it in {!Array_defs} so the arrays theory can classify the [App] head. The
   canonical name comes from {!Array_defs.op_symbol_name}, so the arrays theory minting a
   fresh [select] mid-solve interns the {e same} symbol (identity is by name) and its
   terms hash-cons with these. Ranks: [select] is [(Array(i,e), i) -> e]; [store] is
   [(Array(i,e), i, e) -> Array(i,e)]. *)
let array_op_sym st (role : Array_defs.role) ~index ~element : Symbol.t =
  let name = Array_defs.op_symbol_name role ~index ~element in
  match Hashtbl.find_opt st.array_ops name with
  | Some sym -> sym
  | None ->
    let arr = Sort.array_ ~index ~element in
    let dom, cod =
      match role with
      | Array_defs.Select -> [ arr; index ], element
      | Array_defs.Store -> [ arr; index; element ], arr
    in
    (* board #58: mint through the cap-backed opaque internal minter, not
       [Env.declare_fun]. The op name is a reserved [.oxsmt.arr.*] symbol bearing [|]
       sort-key separators; the public door rejects both, so this is the only door that
       can intern it (and the only one that should — it is a theory-internal symbol, not a
       user declaration). The session's minter admits exactly this op-name grammar
       (Session.parse_minter). *)
    let sym = internal_mint st name (Rank.create dom cod) in
    Hashtbl.replace st.array_ops name sym;
    st.arrays <- Array_defs.add st.arrays sym role ~index ~element;
    sym
;;

(* ---- numerals ---- *)

let int_lit st a =
  match int_of_string_opt a with
  | Some k -> Context.int_const st.ctx k
  | None ->
    (* Exceeds int63 — build an arbitrary-precision constant (core-bignum W2). SMT-LIB
       numerals are canonical decimal (no sign, no leading zeros), which is exactly
       [Bigint.of_string]'s grammar; a genuinely malformed token is a parse error. *)
    (match Bigint.of_string a with
     | b -> Context.int_const_big st.ctx b
     | exception Invalid_argument _ -> malformedf "malformed integer literal: %s" a)
;;

(* ---- bitvector literals ---- *)

(* [#b<bits>] and [#x<hex>] (SMT-LIB §3.1). Width is the digit count times bits-per-digit;
   the value is folded arbitrary-precision ({!Bigint}), so a literal wider than native
   [int] is exact. The lexer already validated the digits, so an unexpected character here
   is an internal error. *)
let bv_literal st ~digits ~bits_per_digit ~value_of_digit =
  let base = Bigint.of_int (1 lsl bits_per_digit) in
  let value =
    String.fold_left
      (fun acc c -> Bigint.add (Bigint.mul acc base) (Bigint.of_int (value_of_digit c)))
      Bigint.zero
      digits
  in
  let width = String.length digits * bits_per_digit in
  if width < 1 then malformedf "empty bitvector literal";
  Bv.const st.ctx (internal_mint st) ~value ~width
;;

(* [(_ bvN W)] — the SMT-LIB decimal bitvector-literal indexed identifier (§3.1): the
   nonnegative decimal value [N] at width [W]. This is the pervasive constant form emitted by
   symbolic-execution generators (Sage/Sydr/Triton/…); we parse it into the same
   {!Oxsmt_core.Bv} constant a [#x]/[#b] literal produces (value reduced into [0, 2^W)). *)
let is_bv_dec_name text =
  String.length text > 2
  && Char.equal text.[0] 'b'
  && Char.equal text.[1] 'v'
  && String.for_all
       (fun c -> c >= '0' && c <= '9')
       (String.sub text 2 (String.length text - 2))
;;

let read_bv_dec_literal st ~text ~width_s =
  let digits = String.sub text 2 (String.length text - 2) in
  let value =
    match Bigint.of_string digits with
    | v -> v
    | exception Invalid_argument _ ->
      malformedf "malformed (_ bv%s W) literal value" digits
  in
  match int_of_string_opt width_s with
  | Some width when width >= 1 ->
    check_bv_width "(_ bvN W)" width;
    Bv.const st.ctx (internal_mint st) ~value ~width
  | _ -> malformedf "(_ bv%s %s): width must be a positive integer" digits width_s
;;

let hex_digit_value c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
  | _ -> malformedf "malformed hex digit %c" c
;;

let bin_digit_value c =
  match c with
  | '0' -> 0
  | '1' -> 1
  | _ -> malformedf "malformed binary digit %c" c
;;

(* The prefix-form bitvector operator names (see {!read_bv_op}); indexed operators
   ([extract], the extends) and the literals arrive by other syntactic routes. *)
let is_bv_keyword = function
  | "bvnot"
  | "bvand"
  | "bvor"
  | "bvxor"
  | "bvneg"
  | "bvadd"
  | "bvsub"
  | "bvmul"
  | "bvudiv"
  | "bvurem"
  | "bvshl"
  | "bvlshr"
  | "bvashr"
  | "bvult"
  | "bvule"
  | "bvslt"
  | "bvsle"
  | "bvugt"
  | "bvuge"
  | "bvsgt"
  | "bvsge"
  | "bvsdiv"
  | "bvsrem"
  | "bvsmod"
  | "bvcomp"
  | "bvnand"
  | "bvnor"
  | "bvxnor"
  | "concat" -> true
  | _ -> false
;;

(* ---- terms ---- *)

(* [scope] is the let-/qvar-binding map ({!Scope}), keyed by bound name; an inner binding
   overwrites (shadows) an outer one of the same name. Matching is on the shared lexer's
   token KINDS, so a quoted [|0|]/[|let|] is a symbol looked up by name — never the
   numeral [0] or the [let] keyword (the ADR-0008 boundary invariant, enforced
   end-to-end). *)
(* F2 (codex): validate a [(! body attr...)] annotation TAIL is well-formed —
   [attr = :keyword | :keyword value]. The tail is otherwise discarded, so an unvalidated
   drop can silently delete content: [(! true (forall ((x Int)) false))] parses to [true],
   dropping the [forall] (a wrong [sat]). Rejecting a non-keyword in attribute-name
   position turns that malformed input into [Malformed] -> unknown. A well-formed tail
   ([:named foo], [:pattern (...)], bare [:foo]) validates and is still dropped. *)
let is_keyword_atom = function
  | Sexp.Atom (Tok.Keyword _) -> true
  | _ -> false
;;

let rec validate_bang_attrs = function
  | [] -> ()
  | Sexp.Atom (Tok.Keyword _) :: rest ->
    (* consume the optional single attribute value (any non-keyword s-expr) *)
    let rest =
      match rest with
      | v :: more when not (is_keyword_atom v) -> more
      | _ -> rest
    in
    validate_bang_attrs rest
  | other :: _ ->
    malformedf
      "malformed (! ...) annotation: expected a :keyword attribute, got %s"
      (Sexp.to_string other)
;;

let rec read_term st scope (s : Sexp.t) : Term.t =
  match s with
  | Sexp.Atom tok -> read_atom st scope tok
  | Sexp.List (Sexp.Atom (Tok.Reserved "let") :: rest) -> read_let st scope rest
  (* [(! t :attr ...)] annotation: validate the attribute tail (F2), then keep the term
     and drop the (well-formed) attributes (e.g. :named). *)
  | Sexp.List (Sexp.Atom (Tok.Reserved "!") :: body :: attrs) ->
    validate_bang_attrs attrs;
    read_term st scope body
  (* [(_ bvN W)] decimal bitvector literal — a nullary indexed identifier (a constant
     term, never applied), so it resolves here rather than through [read_app]. *)
  | Sexp.List
      [ Sexp.Atom (Tok.Reserved "_")
      ; Sexp.Atom (Tok.Symbol { text; _ })
      ; Sexp.Atom (Tok.Numeral width_s)
      ]
    when is_bv_dec_name text -> read_bv_dec_literal st ~text ~width_s
  | Sexp.List (head :: args) -> read_app st scope head args s
  | Sexp.List [] -> malformedf "empty application ()"

and read_atom st scope (tok : Tok.token) : Term.t =
  match tok with
  | Tok.Numeral n -> int_lit st n
  | Tok.Decimal d -> unsupportedf "decimal (real) literal is not in QF_UFLIA: %s" d
  | Tok.Hex h -> bv_literal st ~digits:h ~bits_per_digit:4 ~value_of_digit:hex_digit_value
  | Tok.Binary b ->
    bv_literal st ~digits:b ~bits_per_digit:1 ~value_of_digit:bin_digit_value
  | Tok.String s -> malformedf "unexpected string literal in term position: %S" s
  | Tok.Keyword k -> malformedf "unexpected keyword :%s in term position" k
  | Tok.Reserved r -> malformedf "unexpected reserved word %s in term position" r
  | Tok.Lparen | Tok.Rparen -> malformedf "internal: paren token as atom"
  (* [true]/[false] are the booleans only UNQUOTED; [|true|] is a symbol named "true". *)
  | Tok.Symbol { text = "true"; quoted = false } -> Context.bool_const st.ctx true
  | Tok.Symbol { text = "false"; quoted = false } -> Context.bool_const st.ctx false
  | Tok.Symbol { text = name; _ } ->
    (match Scope.find_opt name scope with
     | Some t -> t
     | None ->
       (match Hashtbl.find_opt st.defines name with
        | Some def -> expand st scope name def []
        | None ->
          (match Hashtbl.find_opt st.funs name with
           | Some { sym; dom = []; _ } -> Context.const st.ctx sym
           | Some { dom = _ :: _; _ } ->
             malformedf "function %s used without arguments" name
           | None -> malformedf "undeclared symbol: %s" name)))

and read_let st scope rest =
  match rest with
  | [ Sexp.List bindings; body ] ->
    (* parallel let: definitions see the outer scope, then all extend it at once *)
    let new_scope =
      List.map
        (fun b ->
          match b with
          | Sexp.List [ name; def ] ->
            (match Sexp.symbol_name name with
             | Some n -> n, read_term st scope def
             | None -> malformedf "malformed let binding name: %s" (Sexp.to_string name))
          | _ -> malformedf "malformed let binding: %s" (Sexp.to_string b))
        bindings
    in
    (* Extend the scope map with this let's bindings, shadowing the outer scope. Folding
       right (first-listed binding added last) preserves the old assoc-list's first-match
       resolution of an intra-let duplicate name (ill-formed input either way). *)
    let scope = List.fold_right (fun (n, t) acc -> Scope.add n t acc) new_scope scope in
    read_term st scope body
  | _ -> malformedf "malformed let (expected (let (bindings) body))"

(* The application head selects interpretation. Only an UNQUOTED symbol can be a builtin
   operator; a quoted [|+|] head (or a reserved word) is never an operator. *)
and read_app st scope head args orig =
  match head with
  (* F1 (codex): a let-/qvar-bound variable in HEAD position. [scope] must be consulted
     for the head, not just [read_atom] — SMT-LIB scoping shadows a global function of the
     same name, and a scalar-bound name is not applicable, so [(x args)] is ill-sorted.
     Resolving it to the shadowed global instead (the pre-fix behaviour) mis-parses the
     body and can yield a definite verdict on ill-typed input (a wrong [unsat] when a
     refuting lemma body is mis-built). Fail closed: reject -> Malformed -> unknown.
     Binder-agnostic (any scope entry: [let] or a lemma qvar). *)
  | Sexp.Atom (Tok.Symbol { text; _ }) when Scope.mem text scope ->
    malformedf "bound variable %s cannot head an application (ill-sorted)" text
  | Sexp.Atom (Tok.Symbol { text = op; quoted = false }) -> read_op st scope op args orig
  | Sexp.Atom (Tok.Symbol { text = op; quoted = true }) ->
    apply_named st scope op args orig
  (* [(as t Sort)] sort ascription (e.g. [(as nil nat)]): read the term and CHECK the
     ascription against its actual sort, rejecting a mismatch (codex) rather than dropping
     it — a silently-ignored ascription could let a wrong-sorted term through. A
     parametric / compound ascription [sort_of_sexp] cannot model raises [Unsupported] (->
     unknown), which is sound: we abstain rather than skip the check. *)
  | Sexp.Atom (Tok.Reserved "as") ->
    (match args with
     | [ t; sort_s ] ->
       let term = read_term st scope t in
       let ascribed = sort_of_sexp st sort_s in
       if not (Sort.equal term.Term.sort ascribed)
       then
         malformedf
           "(as ...) sort ascription %s does not match the term's sort"
           (Sexp.to_string sort_s);
       term
     | _ -> malformedf "malformed (as term sort): %s" (Sexp.to_string orig))
  | Sexp.Atom (Tok.Reserved ("forall" | "exists")) ->
    (* A quantifier in TERM position (nested inside a term, not an assertion root) is out
       of the fragment. VALIDATE its structural shape FIRST so degenerate syntax (bare
       [(exists)], binder-less, wrong arity) is a hard [Malformed] — the reader's stated
       "Malformed whole-fails" contract — rather than a salvageable [Unsupported] drop.
       The check is purely structural ([(Q (binders...) body)] with each binder a
       2-element list); it does NOT resolve binder sorts, so a well-formed nested
       quantifier over an unsupported sort (e.g. [Real]) stays [Unsupported] (soundly
       dropped), not a hard fail. A well-formed-but-nested quantifier is the genuine
       out-of-fragment case. *)
    (match args with
     | [ Sexp.List binders; _body ]
       when List.for_all
              (function
                | Sexp.List [ _; _ ] -> true
                | _ -> false)
              binders -> unsupportedf "quantifiers are not supported (QF only)"
     | _ -> malformedf "malformed quantifier (expected (forall|exists (binders) body))")
  | Sexp.Atom (Tok.Reserved "match") -> unsupportedf "datatype match is not supported yet"
  | Sexp.Atom (Tok.Reserved r) ->
    malformedf "reserved word %s cannot head an application" r
  (* Tester [((_ is C) t)]: an indexed identifier heads the application. Resolve to the
     constructor [C]'s tester symbol (registered by [declare-datatype(s)]) and apply. *)
  | Sexp.List
      [ Sexp.Atom (Tok.Reserved "_"); Sexp.Atom (Tok.Symbol { text = "is"; _ }); cname_s ]
    -> read_tester st scope (name_of cname_s) args orig
  (* Indexed bitvector operators: [(_ extract i j)], [(_ zero_extend n)],
     [(_ sign_extend n)]. Each heads a single-argument application. *)
  | Sexp.List
      [ Sexp.Atom (Tok.Reserved "_")
      ; Sexp.Atom (Tok.Symbol { text = "extract"; _ })
      ; Sexp.Atom (Tok.Numeral i)
      ; Sexp.Atom (Tok.Numeral j)
      ] -> read_bv_extract st scope ~i ~j args orig
  | Sexp.List
      [ Sexp.Atom (Tok.Reserved "_")
      ; Sexp.Atom (Tok.Symbol { text = ("zero_extend" | "sign_extend") as ext; _ })
      ; Sexp.Atom (Tok.Numeral n)
      ] -> read_bv_extend st scope ~ext ~n args orig
  | Sexp.List
      [ Sexp.Atom (Tok.Reserved "_")
      ; Sexp.Atom (Tok.Symbol { text = ("rotate_left" | "rotate_right") as rot; _ })
      ; Sexp.Atom (Tok.Numeral n)
      ] -> read_bv_rotate st scope ~rot ~n args orig
  | Sexp.List
      [ Sexp.Atom (Tok.Reserved "_")
      ; Sexp.Atom (Tok.Symbol { text = "repeat"; _ })
      ; Sexp.Atom (Tok.Numeral n)
      ] -> read_bv_repeat st scope ~n args orig
  | _ ->
    unsupportedf "higher-order / non-symbol application head: %s" (Sexp.to_string head)

(* A tester application [((_ is C) t)]. The tester function symbol is minted as
   ["is-" ^ C] by [declare-datatype(s)]; look it up and apply it (it is a [(dt) -> Bool]
   predicate). *)
and read_tester st scope cname args orig =
  let tester_name = tester_name_of cname in
  match Hashtbl.find_opt st.funs tester_name with
  | None -> malformedf "tester for unknown constructor %s: %s" cname (Sexp.to_string orig)
  | Some { sym; dom; _ } ->
    if List.length args <> List.length dom
    then malformedf "tester (_ is %s) expects 1 argument" cname;
    Context.app st.ctx sym (List.map (read_term st scope) args)

and read_bv_extract st scope ~i ~j args orig =
  match args with
  | [ a ] ->
    (match int_of_string_opt i, int_of_string_opt j with
     | Some i, Some j -> Bv.extract st.ctx (internal_mint st) ~i ~j (read_term st scope a)
     | _ ->
       malformedf "(_ extract i j): indices must be numerals: %s" (Sexp.to_string orig))
  | _ -> malformedf "(_ extract i j) expects 1 argument: %s" (Sexp.to_string orig)

and read_bv_extend st scope ~ext ~n args orig =
  match args with
  | [ a ] ->
    (match int_of_string_opt n with
     | Some n when n >= 0 ->
       let x = read_term st scope a in
       let w =
         match Bv.width_of_sort x.Term.sort with
         | Some w -> w
         | None -> malformedf "(_ %s n): operand is not a bitvector" ext
       in
       (* result width is [w + n]; cap it before the blaster allocates (rider #19). The
          [n > max_bv_width] short-circuit avoids overflowing [w + n]. *)
       check_bv_width ext (if n > max_bv_width then max_bv_width + 1 else w + n);
       if String.equal ext "zero_extend"
       then Bv.zero_extend st.ctx (internal_mint st) ~n x
       else Bv.sign_extend st.ctx (internal_mint st) ~n x
     | _ ->
       malformedf "(_ %s n): n must be a nonneg numeral: %s" ext (Sexp.to_string orig))
  | _ -> malformedf "(_ %s n) expects 1 argument: %s" ext (Sexp.to_string orig)

(* [(_ rotate_left n) x] / [(_ rotate_right n) x] — SUGAR via extract + concat over the
   operand width [w]: rotate by [k = n mod w]. Both leaf ops are oracle-verified; the
   expansion's semantics are oracle-checked against a direct rotate reference. *)
and read_bv_rotate st scope ~rot ~n args orig =
  match args, int_of_string_opt n with
  | [ a ], Some n when n >= 0 ->
    let x = read_term st scope a in
    let ctx = st.ctx
    and mint = internal_mint st in
    let w =
      match Bv.width_of_sort x.Term.sort with
      | Some w -> w
      | None -> malformedf "(_ %s n): operand is not a bitvector" rot
    in
    let k = n mod w in
    if k = 0
    then x
    else (
      let ext hi lo = Bv.extract ctx mint ~i:hi ~j:lo x in
      let cat hi lo = Bv.concat ctx mint hi lo in
      if String.equal rot "rotate_left"
      then cat (ext (w - 1 - k) 0) (ext (w - 1) (w - k))
      else cat (ext (k - 1) 0) (ext (w - 1) k))
  | _ ->
    malformedf
      "(_ %s n) expects a nonneg numeral index and 1 argument: %s"
      rot
      (Sexp.to_string orig)

(* [(_ repeat n) x] — SUGAR: [x] concatenated with itself [n >= 1] times. *)
and read_bv_repeat st scope ~n args orig =
  match args, int_of_string_opt n with
  | [ a ], Some n when n >= 1 ->
    let x = read_term st scope a in
    let ctx = st.ctx
    and mint = internal_mint st in
    let w =
      match Bv.width_of_sort x.Term.sort with
      | Some w -> w
      | None -> malformedf "(_ repeat n): operand is not a bitvector"
    in
    (* Cap the result width [n*w] BEFORE the fold, so an adversarial [n] cannot build a
       crash-sized chain of concats (rider #19 / fable MED-1). *)
    let (_ : int) = checked_product ~what:"(_ repeat n)" ~n ~w in
    let rec go k acc = if k <= 1 then acc else go (k - 1) (Bv.concat ctx mint x acc) in
    go n x
  | _ -> malformedf "(_ repeat n) expects n >= 1 and 1 argument: %s" (Sexp.to_string orig)

(* The equal-width prefix bitvector operators + [concat] + the four comparisons, plus the
   "greater" sugar duals rewritten to the swapped "lesser" form. [None] for a name outside
   the v1 bitvector subset. Membership decides routing in {!read_op} (user functions take
   precedence, so a user symbol that happens to look like one of these is unaffected — the
   bitvector names are all SMT-LIB-reserved theory symbols). *)
and read_bv_op st scope op args orig =
  let rd = read_term st scope in
  let bin f =
    match args with
    | [ a; b ] -> f (rd a) (rd b)
    | _ -> malformedf "%s expects 2 arguments: %s" op (Sexp.to_string orig)
  in
  (* SMT-LIB FixedSizeBitVectors declares bvand/bvor/bvxor/bvadd/bvmul as [:left-assoc],
     so [(bvor a b c ...)] is legal and means [(bvor (bvor a b) c) ...]. The prior [bin]
     rejected any arity != 2 as malformed — the census's single largest structural-unknown
     bucket (task #78: 104 QF_BV files, all n-ary bvor/bvadd/bvxor, mostly the
     20230221-oisc-gurtner unsat family). The 2-argument arm is the LITERAL [bin] body
     [f (rd a) (rd b)] — same operand read order (OCaml evaluates the two [rd] calls
     right-to-left, exactly as [bin] did), so a 2-arg application is byte-for-byte
     identical to trunk including the printed [sat] model (review B1: a [List.map rd] fold
     would read left-to-right and swap the hash-cons / SAT-var order, perturbing the —
     still valid — model bytes). Only the >2-arg case is new behavior (no trunk baseline
     to match, since trunk rejected it). *)
  let left_assoc f =
    match args with
    | [ a; b ] -> f (rd a) (rd b)
    | a :: b :: rest ->
      (* fold left: ((a op b) op c) ... — the [rd a]/[rd b] seed keeps the trunk read
         order for the first pair, then each further operand is read in argument order. *)
      List.fold_left (fun acc x -> f acc (rd x)) (f (rd a) (rd b)) rest
    | _ -> malformedf "%s expects >= 2 arguments: %s" op (Sexp.to_string orig)
  in
  let un f =
    match args with
    | [ a ] -> f (rd a)
    | _ -> malformedf "%s expects 1 argument: %s" op (Sexp.to_string orig)
  in
  let b o x y = Bv.binop st.ctx (internal_mint st) o x y in
  (* Signed division/remainder/modulo and [bvcomp] are SMT-LIB SUGAR over the unsigned
     primitives (bvudiv/bvurem/bvneg/bvadd) plus sign-bit tests — the reference expansions
     from the QF_BV theory. Building them here (rather than as new blaster circuits) keeps
     the bit-blaster and its exhaustive oracle unchanged: every leaf op is already
     oracle-verified, and the parser-side expansions are oracle-checked against a direct
     signed reference (bv_blast_test). *)
  let ctx = st.ctx in
  let mint = internal_mint st in
  let width x =
    match Bv.width_of_sort x.Term.sort with
    | Some w -> w
    | None -> malformedf "%s: operand is not a bitvector" op
  in
  let neg x = Bv.unop ctx mint Bv.Bvneg x in
  let bit1 v = Bv.const ctx mint ~value:(Bigint.of_int v) ~width:1 in
  let msb_set x =
    let w = width x in
    Context.eq ctx (Bv.extract ctx mint ~i:(w - 1) ~j:(w - 1) x) (bit1 1)
  in
  let ite = Context.ite ctx in
  let both p q = Context.and_ ctx [ p; q ] in
  let notb = Context.not_ ctx in
  (* bvsdiv/bvsrem: unsigned op on magnitudes, result sign per the four sign combinations. *)
  let signed_divlike uop x y =
    let ms = msb_set x
    and mt = msb_set y in
    ite
      (both (notb ms) (notb mt))
      (b uop x y)
      (ite
         (both ms (notb mt))
         (neg (b uop (neg x) y))
         (ite (both (notb ms) mt) (neg (b uop x (neg y))) (b uop (neg x) (neg y))))
  in
  (* bvsrem shares bvsdiv's shape EXCEPT the both-negative case is [bvneg (urem -x -y)]. *)
  let bvsrem x y =
    let ms = msb_set x
    and mt = msb_set y in
    ite
      (both (notb ms) (notb mt))
      (b Bv.Bvurem x y)
      (ite
         (both ms (notb mt))
         (neg (b Bv.Bvurem (neg x) y))
         (ite
            (both (notb ms) mt)
            (b Bv.Bvurem x (neg y))
            (neg (b Bv.Bvurem (neg x) (neg y)))))
  in
  (* bvsmod: sign follows the DIVISOR (SMT-LIB); computed from the magnitude remainder
     [u]. *)
  let bvsmod x y =
    let w = width x in
    let ms = msb_set x
    and mt = msb_set y in
    let abs_x = ite ms (neg x) x
    and abs_y = ite mt (neg y) y in
    let u = b Bv.Bvurem abs_x abs_y in
    let zero = Bv.const ctx mint ~value:Bigint.zero ~width:w in
    ite
      (Context.eq ctx u zero)
      u
      (ite
         (both (notb ms) (notb mt))
         u
         (ite
            (both ms (notb mt))
            (b Bv.Bvadd (neg u) y)
            (ite (both (notb ms) mt) (b Bv.Bvadd u y) (neg u))))
  in
  (* bvcomp: 1-bit result, all-ones iff the operands are bitwise equal. *)
  let bvcomp x y = ite (Context.eq ctx x y) (bit1 1) (bit1 0) in
  match op with
  | "bvnot" -> un (Bv.unop st.ctx (internal_mint st) Bv.Bvnot)
  | "bvneg" -> un (Bv.unop st.ctx (internal_mint st) Bv.Bvneg)
  | "bvand" -> left_assoc (b Bv.Bvand)
  | "bvor" -> left_assoc (b Bv.Bvor)
  | "bvxor" -> left_assoc (b Bv.Bvxor)
  | "bvadd" -> left_assoc (b Bv.Bvadd)
  | "bvsub" -> bin (b Bv.Bvsub)
  | "bvmul" -> left_assoc (b Bv.Bvmul)
  | "bvudiv" -> bin (b Bv.Bvudiv)
  | "bvurem" -> bin (b Bv.Bvurem)
  | "bvshl" -> bin (b Bv.Bvshl)
  | "bvlshr" -> bin (b Bv.Bvlshr)
  | "bvashr" -> bin (b Bv.Bvashr)
  | "bvult" -> bin (b Bv.Bvult)
  | "bvule" -> bin (b Bv.Bvule)
  | "bvslt" -> bin (b Bv.Bvslt)
  | "bvsle" -> bin (b Bv.Bvsle)
  (* sugar: a "greater" op is the swapped "lesser" op *)
  | "bvugt" -> bin (fun x y -> b Bv.Bvult y x)
  | "bvuge" -> bin (fun x y -> b Bv.Bvule y x)
  | "bvsgt" -> bin (fun x y -> b Bv.Bvslt y x)
  | "bvsge" -> bin (fun x y -> b Bv.Bvsle y x)
  | "concat" -> bin (Bv.concat st.ctx (internal_mint st))
  | "bvsdiv" -> bin (signed_divlike Bv.Bvudiv)
  | "bvsrem" -> bin bvsrem
  | "bvsmod" -> bin bvsmod
  | "bvcomp" -> bin bvcomp
  (* Negated bitwise ops: SMT-LIB SUGAR = bvnot of the corresponding bitwise op. *)
  | "bvnand" -> bin (fun x y -> Bv.unop ctx mint Bv.Bvnot (b Bv.Bvand x y))
  | "bvnor" -> bin (fun x y -> Bv.unop ctx mint Bv.Bvnot (b Bv.Bvor x y))
  | "bvxnor" -> bin (fun x y -> Bv.unop ctx mint Bv.Bvnot (b Bv.Bvxor x y))
  | _ -> unsupportedf "bitvector operator %s is not in the v1 subset" op

(* Apply a user-declared function or expand a define-fun (no builtin-operator meaning). *)
and apply_named st scope op args orig =
  match Hashtbl.find_opt st.defines op with
  | Some def -> expand st scope op def args
  | None ->
    (match Hashtbl.find_opt st.funs op with
     | Some { sym; dom; _ } ->
       let n_expect = List.length dom
       and n_got = List.length args in
       if n_expect <> n_got
       then malformedf "%s applied to %d args, expected %d" op n_got n_expect;
       Context.app st.ctx sym (List.map (read_term st scope) args)
     | None ->
       malformedf "undeclared function or unknown operator: %s" (Sexp.to_string orig))

and read_op st scope op args orig =
  let rd = read_term st scope in
  let rds () = List.map rd args in
  match op, args with
  | "not", [ a ] -> Context.not_ st.ctx (rd a)
  | "not", _ -> malformedf "not expects 1 argument"
  | "and", _ -> Context.and_ st.ctx (rds ())
  | "or", _ -> Context.or_ st.ctx (rds ())
  | "=>", _ :: _ :: _ -> read_implies st scope args
  | "=>", _ -> malformedf "=> expects >= 2 arguments"
  | "ite", [ c; th; el ] -> Context.ite st.ctx (rd c) (rd th) (rd el)
  | "ite", _ -> malformedf "ite expects 3 arguments"
  | "=", _ :: _ :: _ -> chain st (fun a b -> Context.eq st.ctx a b) (rds ())
  | "=", _ -> malformedf "= expects >= 2 arguments"
  | "distinct", _ :: _ :: _ -> Context.distinct st.ctx (rds ())
  | "distinct", _ -> malformedf "distinct expects >= 2 arguments"
  | "<=", _ :: _ :: _ -> chain st (fun a b -> Context.le st.ctx a b) (rds ())
  | "<", _ :: _ :: _ -> chain st (fun a b -> Context.lt st.ctx a b) (rds ())
  | ">=", _ :: _ :: _ -> chain st (fun a b -> Context.ge st.ctx a b) (rds ())
  | ">", _ :: _ :: _ -> chain st (fun a b -> Context.gt st.ctx a b) (rds ())
  | ("<=" | "<" | ">=" | ">"), _ -> malformedf "%s expects >= 2 arguments" op
  (* Build sums in one [linear_combination] pass rather than left-folding [add]/[sub]: a
     left fold re-normalizes and hash-conses every partial sum (O(n^2) work AND O(n^2)
     interned intermediates — a real memory blowup on wide sums), while
     [linear_combination] merges once. The normalized result is identical. *)
  | "+", _ :: _ -> Context.linear_combination st.ctx (List.map (fun a -> 1, rd a) args) 0
  | "+", [] -> malformedf "+ expects >= 1 argument"
  | "-", [ a ] -> Context.neg st.ctx (rd a)
  | "-", x :: rest ->
    (* a - b - c ... = 1*a + (-1)*b + (-1)*c ... *)
    Context.linear_combination st.ctx ((1, rd x) :: List.map (fun a -> -1, rd a) rest) 0
  | "-", [] -> malformedf "- expects >= 1 argument"
  | "*", _ :: _ -> read_mul st scope args
  | "*", [] -> malformedf "* expects >= 1 argument"
  | "div", [ a; b ] -> Context.div st.ctx (rd a) (rd b)
  | "mod", [ a; b ] -> Context.mod_ st.ctx (rd a) (rd b)
  | ("div" | "mod"), _ -> malformedf "%s expects 2 arguments" op
  | "abs", [ a ] -> Context.abs st.ctx (rd a)
  | "abs", _ -> malformedf "abs expects 1 argument"
  (* Array theory operators (QF_AX). The array argument is read first so its
     [Sort.Array (index, element)] is known; the monomorphic [select]/[store] symbol for
     that instantiation is minted/recorded ([array_op_sym]) and applied. [Context.app]
     sort-checks the remaining arguments against the minted rank, so an ill-sorted index
     or value fails closed to [Malformed] -> unknown. A [select]/[store] on a non-array
     first argument is likewise rejected. *)
  | "select", [ a; i ] ->
    let arr = rd a in
    (match arr.Term.sort with
     | Sort.Array (index, element) ->
       Context.app
         st.ctx
         (array_op_sym st Array_defs.Select ~index ~element)
         [ arr; rd i ]
     | _ -> malformedf "select applied to a non-array term")
  | "select", _ -> malformedf "select expects 2 arguments"
  | "store", [ a; i; v ] ->
    let arr = rd a in
    (match arr.Term.sort with
     | Sort.Array (index, element) ->
       Context.app
         st.ctx
         (array_op_sym st Array_defs.Store ~index ~element)
         [ arr; rd i; rd v ]
     | _ -> malformedf "store applied to a non-array term")
  | "store", _ -> malformedf "store expects 3 arguments"
  (* A user-declared function / define-fun takes precedence over the bitvector-operator
     keywords (so a user symbol that happens to spell one of the reserved bitvector names
     still resolves to the user's declaration); otherwise a bitvector keyword routes to
     the bitvector builders, and anything else is an undeclared/unknown operator. *)
  | _ when Hashtbl.mem st.defines op || Hashtbl.mem st.funs op ->
    apply_named st scope op args orig
  | _ when is_bv_keyword op -> read_bv_op st scope op args orig
  | _ -> apply_named st scope op args orig

(* Expand a [define-fun] use site by capture-avoiding substitution: the argument
   s-expressions are read in the CALLER's [scope] (so they may use the caller's
   let-bindings and globals), then the body is read in a FRESH scope containing ONLY the
   parameters — the caller's locals do not leak into the body, and a nested [let] in the
   body binds tighter than a parameter (both fall out of [read_term]'s innermost-first
   scope lookup). Argument values are already-built [Term.t]s, so substituting them can
   never capture. Recursion (direct or mutual) is rejected via the [expanding] cycle
   guard; SMT-LIB non-rec [define-fun] bodies reference only earlier definitions, so this
   is the only cycle possible. *)
and expand st scope name (def : definition) arg_sexps =
  if Hashtbl.mem st.expanding name
  then unsupportedf "recursive use of define-fun %s is not supported" name;
  let n_expect = List.length def.params
  and n_got = List.length arg_sexps in
  if n_expect <> n_got
  then malformedf "define-fun %s applied to %d args, expected %d" name n_got n_expect;
  let bindings =
    List.map2
      (fun (pname, psort) arg ->
        let t = read_term st scope arg in
        if not (Sort.equal t.Term.sort psort)
        then malformedf "define-fun %s: argument for %s has the wrong sort" name pname;
        pname, t)
      def.params
      arg_sexps
  in
  let key = name, List.map (fun (_, (t : Term.t)) -> t.tag) bindings in
  match Hashtbl.find_opt st.memo key with
  | Some cached -> cached
  | None ->
    (* Cycle guard stays live across the body read: recursion re-enters [expand] with the
       same [name] before this key is cached, so it is caught here, not memoized. *)
    Hashtbl.replace st.expanding name ();
    (* Fresh scope containing ONLY the parameters (caller locals do not leak into the
       body); [bindings] stays a list for the memo key above. *)
    let param_scope =
      List.fold_right (fun (n, t) acc -> Scope.add n t acc) bindings Scope.empty
    in
    let body = read_term st param_scope def.body in
    Hashtbl.remove st.expanding name;
    if not (Sort.equal body.Term.sort def.ret)
    then malformedf "define-fun %s body sort differs from declared result sort" name;
    Hashtbl.replace st.memo key body;
    body

(* [(=> a b c)] is right-associative: [a => (b => c)]. *)
and read_implies st scope args =
  match List.rev_map (read_term st scope) args with
  | last :: rest -> List.fold_left (fun acc a -> Context.implies st.ctx a acc) last rest
  | [] -> malformedf "=> expects arguments"

(* Linear multiplication only: at most one non-constant factor (DESIGN §1). Constant
   factors fold into a coefficient via [mul_const]; two or more non-constants is nonlinear
   and unsupported. *)
and read_mul st scope args =
  let ts = List.map (read_term st scope) args in
  let consts, nonconsts =
    List.partition_map
      (fun (t : Term.t) ->
        match t.node with
        | Term.Int_const k -> Either.Left k
        | _ -> Either.Right t)
      ts
  in
  match nonconsts with
  | _ :: _ :: _ -> unsupportedf "nonlinear multiplication (>= 2 non-constant factors)"
  | _ ->
    let base =
      match nonconsts with
      | [ t ] -> t
      | _ -> Context.int_const st.ctx 1
    in
    (* [k] is a [Bigint.t] coefficient (possibly > int63); fold with the
       arbitrary-precision [mul_const_big]. *)
    List.fold_left (fun acc k -> Context.mul_const_big st.ctx k acc) base consts

(* [(rel a b c ...)] means the conjunction of consecutive pairs. *)
and chain st mk ts =
  match ts with
  | a :: (_ :: _ as rest) ->
    let rec loop prev = function
      | [] -> []
      | x :: tl -> mk prev x :: loop x tl
    in
    (match loop a rest with
     | [ one ] -> one
     | many -> Context.and_ st.ctx many)
  | _ -> malformedf "chained relation needs >= 2 arguments"
;;

(* ---- quantifiers (ADR-0012 lemma tier) ---- *)

(* A quantifier binder list [((x S) (y T) ...)] -> [(name, sort)] pairs. Sorts are read
   eagerly (so an out-of-subset binder sort — e.g. an array — fails at parse time, exactly
   as a top-level declaration would). *)
let read_binders st (binders : Sexp.t) : (string * Sort.t) list =
  match binders with
  | Sexp.List bs ->
    List.map
      (fun b ->
        match b with
        | Sexp.List [ nm; srt ] ->
          (match Sexp.symbol_name nm with
           | Some n -> n, sort_of_sexp st srt
           | None -> malformedf "malformed quantifier binder name: %s" (Sexp.to_string nm))
        | _ -> malformedf "malformed quantifier binder: %s" (Sexp.to_string b))
      bs
  | _ -> malformedf "quantifier binder list must be a list"
;;

(* Pull the [:pattern (t1 t2 ...)] annotations out of a [(! body :attr ... )] attribute
   tail. Each [:pattern] is one CONJUNCTIVE multi-trigger (its terms must all match under
   one substitution); several [:pattern]s are ALTERNATIVE triggers (any may fire). Other
   attributes (e.g. [:qid], [:named], [:weight]) are ignored. Returns the pattern groups
   as raw s-expressions, read (with the qvars in scope) only inside [build]. *)
let rec extract_patterns (attrs : Sexp.t list) : Sexp.t list list =
  match attrs with
  | Sexp.Atom (Tok.Keyword "pattern") :: Sexp.List pats :: rest ->
    pats :: extract_patterns rest
  | Sexp.Atom (Tok.Keyword _) :: _value :: rest -> extract_patterns rest
  | _ :: rest -> extract_patterns rest
  | [] -> []
;;

(* Peel nested [forall]s into one flat binder list (forall x. forall y. P == forall
   x y. P) and return the innermost body together with its trigger groups. Only universal
   quantifiers are flattened here; an [exists] in the body is NOT rejected any more
   (lemmas-climb chunk 2b): it is returned as the body and Skolemized by [read_lemma_body]
   if it sits in a positive position. [(! body :pattern ...)] on the innermost body
   supplies the triggers. *)
let rec collect_forall st acc (tail : Sexp.t list) =
  match tail with
  | [ binders; body ] ->
    let acc = acc @ read_binders st binders in
    (match body with
     | Sexp.List (Sexp.Atom (Tok.Reserved "forall") :: inner) ->
       collect_forall st acc inner
     | Sexp.List (Sexp.Atom (Tok.Reserved "!") :: core :: attrs) ->
       acc, core, extract_patterns attrs
     | _ -> acc, body, [])
  | _ -> malformedf "malformed forall (expected (forall (binders) body))"
;;

(* Peel nested [exists]s into one flat binder list (exists x. exists y. P == exists
   x y. P) and return the innermost body. All existentials reaching here are POSITIVE (the
   caller only enters on an [exists] at a positive position), so flattening is sound. A
   [(! body ...)] wrapper on the innermost body is unwrapped (its attributes — [:pattern]
   has no meaning for a Skolemized existential — are validated then dropped). A [forall]
   (or anything else) as the body is left for the caller to read. *)
let rec collect_exists st acc (tail : Sexp.t list) =
  match tail with
  | [ binders; body ] ->
    let acc = acc @ read_binders st binders in
    (match body with
     | Sexp.List (Sexp.Atom (Tok.Reserved "exists") :: inner) ->
       collect_exists st acc inner
     | Sexp.List (Sexp.Atom (Tok.Reserved "!") :: core :: attrs) ->
       validate_bang_attrs attrs;
       acc, core
     | _ -> acc, body)
  | _ -> malformedf "malformed exists (expected (exists (binders) body))"
;;

(* Read a [forall] lemma body known to sit in POSITIVE polarity, Skolemizing every
   positive-position nested [exists] into a fresh function of the enclosing universals
   (lemmas-climb chunk 2b). It descends ONLY through the polarity-preserving skeleton that
   keeps a sub-formula positive — [and], [or], the CONSEQUENT of [=>], and a [(! ...)]
   wrapper — and delegates every other node (including [not], the ANTECEDENTS of [=>],
   [ite], [=]/[distinct], and all leaves) to {!read_term}. That delegation is the
   soundness boundary: read_term REJECTS any [exists] it meets ([Unsupported] -> the whole
   lemma is dropped with the sat-degrade sentinel), so a NON-positive existential is never
   Skolemized (Skolemizing a [forall]-in-disguise to a function would be unsound). A
   positive [exists] is replaced by its body with each binder bound to
   [skolem_witness sort] — a fresh function applied to the enclosing universals — leaving
   a genuine universal lemma that is equisatisfiable with the original. A connective name
   shadowed by a binder ([Scope.mem]) is NOT treated as the core op — it falls to
   read_term, which rejects the ill-sorted application, exactly as {!read_app} does. *)
let rec read_lemma_body st scope ~skolem_witness (s : Sexp.t) : Term.t =
  let recur = read_lemma_body st scope ~skolem_witness in
  match s with
  | Sexp.List (Sexp.Atom (Tok.Reserved "!") :: body :: attrs) ->
    validate_bang_attrs attrs;
    recur body
  | Sexp.List (Sexp.Atom (Tok.Reserved "exists") :: tail) ->
    (* POSITIVE-position existential: Skolemize each (flattened) binder to a fresh
       function of the enclosing universals, bind it, and read the still-positive body.
       Reading the binders may raise [Malformed] (degenerate/bad binder — hard fail, per
       the reader's contract) or [Unsupported] (out-of-subset binder sort, e.g. [Real])
       which drops the whole lemma via the caller's handler — both sound. *)
    let binders, body = collect_exists st [] tail in
    let scope =
      List.fold_left
        (fun acc (name, sort) -> Scope.add name (skolem_witness sort) acc)
        scope
        binders
    in
    read_lemma_body st scope ~skolem_witness body
  | Sexp.List (Sexp.Atom (Tok.Symbol { text = "and"; quoted = false }) :: args)
    when not (Scope.mem "and" scope) -> Context.and_ st.ctx (List.map recur args)
  | Sexp.List (Sexp.Atom (Tok.Symbol { text = "or"; quoted = false }) :: args)
    when not (Scope.mem "or" scope) -> Context.or_ st.ctx (List.map recur args)
  | Sexp.List
      (Sexp.Atom (Tok.Symbol { text = "=>"; quoted = false }) :: (_ :: _ :: _ as args))
    when not (Scope.mem "=>" scope) ->
    (* [(=> a1 .. an c)] is right-associative [a1 => (.. => c)]: only the final CONSEQUENT
       [c] stays positive; every antecedent flips to negative, so it is read by
       [read_term] (which rejects any [exists] in it). Matches {!read_implies}'s
       associativity. *)
    (match List.rev args with
     | consequent :: rev_antecedents ->
       List.fold_left
         (fun acc a -> Context.implies st.ctx (read_term st scope a) acc)
         (recur consequent)
         rev_antecedents
     | [] -> malformedf "=> expects arguments")
  | _ -> read_term st scope s
;;

(* Parse [(assert (forall ...))] into a {!lemma_src}. The binders are read now; the body
   and triggers are read lazily by [build], with each binder name bound (innermost-first,
   so an inner binder shadows an outer one of the same name) to its minted qvar image. The
   body goes through {!read_lemma_body} so a positive nested [exists] is Skolemized to a
   fresh function of the qvar images via the driver-supplied [skolem]. *)
let read_forall st (tail : Sexp.t list) : lemma_src =
  let qvars, body_sexp, trigger_sexps = collect_forall st [] tail in
  let build ~skolem qvar_images =
    (* [qvars] is outer-to-inner; adding each in that order lets an inner binder overwrite
       (shadow) an outer one of the same name — matching the old innermost-first list. *)
    let scope =
      List.fold_left
        (fun acc (i, name) -> Scope.add name qvar_images.(i) acc)
        Scope.empty
        (List.mapi (fun i (name, _sort) -> i, name) qvars)
    in
    (* Every Skolem function of a nested existential takes ALL the forall's universals as
       arguments — a sound over-approximation of the existential's true dependencies
       (standard Skolemization only requires the DOMINATING universals, which are a subset
       of these). *)
    let skolem_witness cod = skolem ~cod ~args:(Array.to_list qvar_images) in
    let body = read_lemma_body st scope ~skolem_witness body_sexp in
    let triggers = List.map (List.map (read_term st scope)) trigger_sexps in
    body, triggers
  in
  { qvars; build }
;;

(* Parse [(assert (exists ...))] at a POSITIVE position into an {!exists_src}. Binders are
   read now; the body is read lazily by [ex_build] with each binder bound (outer-to-inner,
   inner shadows) to the fresh ground witness the driver supplies. *)
let read_exists st (tail : Sexp.t list) : exists_src =
  let ex_qvars, body_sexp = collect_exists st [] tail in
  let ex_build witnesses =
    let scope =
      List.fold_left
        (fun acc (i, name) -> Scope.add name witnesses.(i) acc)
        Scope.empty
        (List.mapi (fun i (name, _sort) -> i, name) ex_qvars)
    in
    read_term st scope body_sexp
  in
  { ex_qvars; ex_build }
;;

(* Quantifier duals at a positive assertion position: [not (exists x. p)] is
   [forall x. not p], so it must become a lemma rather than a Skolem constant (which would
   be an unsound weakening); [not (forall x. p)] is [exists x. not p], so the ordinary
   fresh-witness path is equisatisfiable. The bodies are read with [read_term], not
   [read_lemma_body]: a further nested quantifier remains outside the fragment and is
   dropped under the existing sentinel discipline. *)
let read_negated_exists st (tail : Sexp.t list) : lemma_src =
  let qvars, body_sexp = collect_exists st [] tail in
  let build ~skolem:_ qvar_images =
    let scope =
      List.fold_left
        (fun acc (i, name) -> Scope.add name qvar_images.(i) acc)
        Scope.empty
        (List.mapi (fun i (name, _sort) -> i, name) qvars)
    in
    Context.not_ st.ctx (read_term st scope body_sexp), []
  in
  { qvars; build }
;;

let read_negated_forall st (tail : Sexp.t list) : exists_src =
  let ex_qvars, body_sexp, _triggers = collect_forall st [] tail in
  let ex_build witnesses =
    let scope =
      List.fold_left
        (fun acc (i, name) -> Scope.add name witnesses.(i) acc)
        Scope.empty
        (List.mapi (fun i (name, _sort) -> i, name) ex_qvars)
    in
    Context.not_ st.ctx (read_term st scope body_sexp)
  in
  { ex_qvars; ex_build }
;;

(* ---- front-end quantified pipeline (dark: OXSMT_QUANT_PIPELINE) ---- *)

(* Does [s] syntactically contain a [forall]/[exists]? Gates pipeline routing and the
   leaf/structure split inside {!formula_of_sexp}. *)
let rec has_quantifier (s : Sexp.t) =
  match s with
  | Sexp.Atom (Tok.Reserved ("forall" | "exists")) -> true
  | Sexp.Atom _ -> false
  | Sexp.List items -> List.exists has_quantifier items
;;

(* The symbol names appearing anywhere in [s] (used to compute a leaf's referenced binder
   ids — an over-approximation is sound: it can only keep an unused qvar, never miss one). *)
let symbol_names s =
  let tbl = Hashtbl.create 16 in
  let rec go = function
    | Sexp.Atom (Tok.Symbol { text; _ }) -> Hashtbl.replace tbl text ()
    | Sexp.Atom _ -> ()
    | Sexp.List items -> List.iter go items
  in
  go s;
  tbl
;;

(* Wrap a leaf s-expression in the enclosing let-nesting ([lets] outermost-first), so a
   leaf split out of a [let] body still resolves its let-bound names. Sound because [let]
   is pure sharing; re-reading the (quantifier-free) bindings at each leaf recomputes the
   same term. A binding whose VALUE contains a quantifier makes the wrapped read raise
   [Unsupported] -> the assertion is dropped (sound). *)
let wrap_lets lets leaf =
  List.fold_right
    (fun bindings body ->
      Sexp.List [ Sexp.Atom (Tok.Reserved "let"); Sexp.List bindings; body ])
    lets
    leaf
;;

(* A deferred leaf reader: a quantifier-free Bool sub-formula, captured with its enclosing
   lets and the quantifier-binder scope active at its position. Term construction is
   deferred to lowering, when the binder images (qvar / Skolem terms) exist. *)
type qleaf =
  { q_sexp : Sexp.t (* leaf content, wrapped in enclosing lets *)
  ; q_scope : (string * int) list (* quantifier binder name -> id, OUTERMOST-first *)
  ; q_refs : int list (* binder ids the sexp mentions (subset of q_scope ids) *)
  ; q_patterns : Sexp.t list list
  (* explicit [:pattern] multi-triggers (HINTS ONLY) attached to this leaf when it is a
     whole [forall] body (the common lemma shape); raw s-expressions over the qvar NAMES —
     resolved through the leaf's read scope at lowering, so no id-rename is needed. Empty
     for a non-body leaf, or when clausification could not attach them cleanly. *)
  }

let make_leaf ~lets ~qscope (s : Sexp.t) : qleaf Fol.t =
  let q_sexp = wrap_lets lets s in
  let names = symbol_names q_sexp in
  let q_scope = List.map (fun (n, (b : Fol.binder)) -> n, b.Fol.id) qscope in
  let q_refs =
    List.filter_map (fun (n, id) -> if Hashtbl.mem names n then Some id else None) q_scope
  in
  Fol.Atom { q_sexp; q_scope; q_refs; q_patterns = [] }
;;

(* [:pattern] terms are name-based (resolved through the leaf's read scope at lowering),
   so rename-apart — which only rewrites binder IDS — leaves them untouched. *)
let rename_qleaf remap q =
  { q with
    q_scope = List.map (fun (n, i) -> n, remap i) q.q_scope
  ; q_refs = List.map remap q.q_refs
  }
;;

let qleaf_refs q = q.q_refs

(* Is [s] PROVABLY Bool-sorted? Conservative — [false] on any uncertainty. A false
   NEGATIVE only costs coverage (the [=]/[distinct]/[ite] becomes a leaf, and a quantifier
   buried in it then drops soundly); a false POSITIVE would structurally decompose a
   theory operator, which is unsound, so we never guess Bool. [lets] is the in-scope
   [let]-binding environment (name -> is-the-bound-value-provably-Bool); a [let]-bound
   name SHADOWS a same-named global, so a shadowed non-Bool value (e.g.
   [(let ((p 0)) ...)] with a global [p : Bool]) is correctly NOT provably Bool. *)
let rec definitely_bool st ~qscope ~lets (s : Sexp.t) : bool =
  let cod_is_bool name =
    match Hashtbl.find_opt st.funs name with
    | Some { sym; _ } ->
      (match Env.rank st.env sym with
       | r -> Sort.equal r.Rank.codomain Sort.bool
       | exception _ -> false)
    | None ->
      (match Hashtbl.find_opt st.defines name with
       | Some d -> Sort.equal d.ret Sort.bool
       | None -> false)
  in
  (* name resolution: an inner [let] binding shadows a quantifier binder shadows a global. *)
  let name_is_bool name =
    match List.assoc_opt name lets with
    | Some b -> b
    | None ->
      (match List.assoc_opt name qscope with
       | Some (b : Fol.binder) -> Sort.equal b.Fol.sort Sort.bool
       | None -> cod_is_bool name)
  in
  match s with
  | Sexp.Atom (Tok.Symbol { text = "true" | "false"; quoted = false }) -> true
  | Sexp.Atom (Tok.Symbol { text; _ }) -> name_is_bool text
  | Sexp.Atom _ -> false
  | Sexp.List (Sexp.Atom (Tok.Reserved ("forall" | "exists")) :: _) -> true
  | Sexp.List (Sexp.Atom (Tok.Reserved "!") :: body :: _) ->
    definitely_bool st ~qscope ~lets body
  | Sexp.List [ Sexp.Atom (Tok.Reserved "let"); Sexp.List bindings; body ] ->
    (* SMT [let] is PARALLEL: each bound value is classified in the OUTER [lets]; the body
       sees them all. A binding whose value is not provably Bool records [false], so a
       name it shadows can no longer be taken as Bool. *)
    let lets' =
      List.fold_left
        (fun acc b ->
          match b with
          | Sexp.List [ nm; v ] ->
            (match Sexp.symbol_name nm with
             | Some n -> (n, definitely_bool st ~qscope ~lets v) :: acc
             | None -> acc)
          | _ -> acc)
        lets
        bindings
    in
    definitely_bool st ~qscope ~lets:lets' body
  | Sexp.List (head :: args) ->
    (match Sexp.simple head with
     | Some ("and" | "or" | "not" | "=>" | "xor") -> true
     | Some ("=" | "distinct" | "<" | "<=" | ">" | ">=") -> true
     | Some "ite" ->
       (match args with
        | _ :: t :: _ -> definitely_bool st ~qscope ~lets t
        | _ -> false)
     | Some name ->
       (* a let-/qvar-bound scalar heading an application is ill-sorted (not a function),
          so it is never a Bool-headed operator; only a global function's codomain counts. *)
       if List.mem_assoc name lets || List.mem_assoc name qscope
       then false
       else cod_is_bool name
     | None -> false)
  | Sexp.List [] -> false
;;

(* n-ary Boolean [=] (all-equal, a chain of adjacent iffs) / [distinct]. Only entered when
   an argument is provably Bool. *)
let bool_eq_or_distinct ~op recur args =
  match op with
  | "=" ->
    let rec chain = function
      | a :: (b :: _ as rest) -> Fol.Iff (recur a, recur b) :: chain rest
      | _ -> []
    in
    (match chain args with
     | [ single ] -> single
     | cs -> Fol.And cs)
  | _ ->
    (match args with
     | [ a; b ] -> Fol.Xor (recur a, recur b)
     | _ :: _ :: _ -> Fol.False (* >2 Bool values cannot be pairwise distinct *)
     | _ -> malformedf "distinct expects >= 2 arguments")
;;

(* Build the formula IR for a quantifier-bearing assertion body. A maximal quantifier-free
   subterm becomes ONE deferred leaf; only the skeleton on the path to a quantifier is
   structural. Raises [Unsupported] for content the IR cannot faithfully represent (a
   quantifier under a theory operator, or a theory-sorted [=]/[ite]/[let]-value), so the
   caller DROPS it under the sentinel discipline — never an unsound over-approximation. A
   [Malformed] body still propagates (hard fail -> unknown). *)
let rec formula_of_sexp st ~lets ~qscope (s : Sexp.t) : qleaf Fol.t =
  if not (has_quantifier s)
  then make_leaf ~lets ~qscope s
  else (
    let recur = formula_of_sexp st ~lets ~qscope in
    let bound name = List.mem_assoc name qscope in
    match s with
    | Sexp.List (Sexp.Atom (Tok.Reserved "!") :: body :: attrs) ->
      (* :pattern hints are discarded at stage 2 (RUNG 3 threads them); the tail is still
         VALIDATED so a malformed annotation cannot silently drop content. *)
      validate_bang_attrs attrs;
      formula_of_sexp st ~lets ~qscope body
    | Sexp.List (Sexp.Atom (Tok.Reserved "forall") :: tail) ->
      let binders, body, patterns = collect_forall st [] tail in
      quantifier
        st
        ~lets
        ~qscope
        ~patterns
        ~mk:(fun bs g -> Fol.Forall (bs, g))
        binders
        body
    | Sexp.List (Sexp.Atom (Tok.Reserved "exists") :: tail) ->
      let binders, body = collect_exists st [] tail in
      quantifier
        st
        ~lets
        ~qscope
        ~patterns:[]
        ~mk:(fun bs g -> Fol.Exists (bs, g))
        binders
        body
    | Sexp.List [ Sexp.Atom (Tok.Reserved "let"); Sexp.List bindings; body ] ->
      formula_of_sexp st ~lets:(lets @ [ bindings ]) ~qscope body
    | Sexp.List (head :: args) ->
      (match Sexp.simple head with
       | Some "and" when not (bound "and") -> Fol.And (List.map recur args)
       | Some "or" when not (bound "or") -> Fol.Or (List.map recur args)
       | Some "not" when not (bound "not") ->
         (match args with
          | [ a ] -> Fol.Not (recur a)
          | _ -> malformedf "not expects exactly one argument")
       | Some "=>" when not (bound "=>") ->
         (match List.rev args with
          | consequent :: rev_ante ->
            List.fold_left
              (fun acc a -> Fol.Implies (recur a, acc))
              (recur consequent)
              rev_ante
          | [] -> malformedf "=> expects arguments")
       | Some "xor" when not (bound "xor") ->
         (match args with
          | a :: b :: rest ->
            List.fold_left
              (fun acc x -> Fol.Xor (acc, recur x))
              (Fol.Xor (recur a, recur b))
              rest
          | _ -> malformedf "xor expects >= 2 arguments")
       (* Classify [=]/[distinct]/[ite] Bool-vs-theory: wrap the arg in the ENCLOSING lets
          first so [definitely_bool] sees (and shadows through) let bindings active here —
          a shadowed non-Bool value must not be read as its same-named Bool global
          (wrong-unsat landmine). *)
       | Some (("=" | "distinct") as op)
         when (not (bound op))
              && List.exists
                   (fun a -> definitely_bool st ~qscope ~lets:[] (wrap_lets lets a))
                   args -> bool_eq_or_distinct ~op recur args
       | Some "ite"
         when (not (bound "ite"))
              &&
              match args with
              | [ _; t; _ ] -> definitely_bool st ~qscope ~lets:[] (wrap_lets lets t)
              | _ -> false ->
         (match args with
          | [ c; t; e ] -> Fol.Ite (recur c, recur t, recur e)
          | _ -> malformedf "ite expects three arguments")
       | Some _ | None ->
         unsupportedf "quantifier under an unsupported operator or theory position")
    | _ -> unsupportedf "quantifier in an unsupported position")

and quantifier st ~lets ~qscope ~patterns ~mk binders body =
  let fbs = List.map (fun (n, srt) -> n, Fol.fresh_binder ~name:n ~sort:srt) binders in
  let qscope' = qscope @ fbs in
  let g = formula_of_sexp st ~lets ~qscope:qscope' body in
  (* Attach explicit [:pattern] hints only when the body is a SINGLE leaf (the common
     [forall qvars. QF-body] lemma), so the multi-triggers map cleanly to the one clause
     this forall produces. A structured body (and/or/nested quantifier) splits into
     several clauses across which a whole-body pattern is no longer well-defined — discard
     it (clausification invalidated it); the loader then infers a trigger. Patterns are
     hints, so either choice is sound. *)
  let g =
    match patterns, g with
    | _ :: _, Fol.Atom leaf -> Fol.Atom { leaf with q_patterns = patterns }
    | _ -> g
  in
  mk (List.map snd fbs) g
;;

(* Read a deferred leaf into a [Term.t]. [lookup] maps a binder id to its image; a binder
   in the leaf's [q_scope] but NOT referenced by the leaf's s-expression has no image
   ([None]) and is simply omitted from the read scope — it can never be looked up by
   [read_term] (its name does not appear), and the clause only mints images for referenced
   binders. A referenced binder's name DOES appear, so its id is in [q_refs] hence in the
   clause's universals/Skolems, so [lookup] returns [Some]. *)
let qleaf_scope lookup (q : qleaf) : Term.t Scope.t =
  List.fold_left
    (fun acc (n, id) ->
      match lookup id with
      | Some t -> Scope.add n t acc
      | None -> acc)
    Scope.empty
    q.q_scope
;;

let read_qleaf st lookup (q : qleaf) : Term.t =
  read_term st (qleaf_scope lookup q) q.q_sexp
;;

(* Build the explicit [:pattern] multi-triggers a leaf carries (HINTS ONLY). Each pattern
   group is read over the leaf's own binder scope (qvar names -> images); a group whose
   terms all build is kept, one that references a name not in scope (a binder
   clausification moved to another clause) or is otherwise out of the reader's fragment is
   DISCARDED — a pattern never affects soundness, only which instances the matcher
   generates. *)
let leaf_triggers st lookup (q : qleaf) : Term.t list list =
  match q.q_patterns with
  | [] -> []
  | groups ->
    let scope = qleaf_scope lookup q in
    List.filter_map
      (fun grp ->
        match List.map (read_term st scope) grp with
        | terms -> Some terms
        | exception
            ( Malformed _
            | Unsupported _
            | Term.Sort_error _
            | Term.Unsupported _
            | Term.Overflow ) -> None)
      groups
;;

(* Fold a clausal matrix (True/False/Atom/Not(Atom)/And/Or over built [Term.t]s) into one
   [Term.t] through the shared [Context]. *)
let rec fold_matrix st = function
  | Fol.True -> Context.bool_const st.ctx true
  | Fol.False -> Context.bool_const st.ctx false
  | Fol.Atom t -> t
  | Fol.Not g -> Context.not_ st.ctx (fold_matrix st g)
  | Fol.And gs -> Context.and_ st.ctx (List.map (fold_matrix st) gs)
  | Fol.Or gs -> Context.or_ st.ctx (List.map (fold_matrix st) gs)
  | Fol.Implies _ | Fol.Iff _ | Fol.Xor _ | Fol.Ite _ | Fol.Forall _ | Fol.Exists _ ->
    failwith "Fol lowering: matrix is not clausal (internal invariant)"
;;

(* Clausify one assertion body into lowering clauses (NNF -> rename-apart -> Skolemize ->
   prenex -> split). Raises {!Unsupported}/{!Malformed} exactly as {!formula_of_sexp}. *)
let clauses_of_assertion st (s : Sexp.t) : clause list =
  let phi = formula_of_sexp st ~lets:[] ~qscope:[] s in
  let fol_clauses = Fol.clausify ~rename_atom:rename_qleaf ~atom_refs:qleaf_refs phi in
  List.map
    (fun (cl : qleaf Fol.clause) ->
      let cl_qvars =
        List.map (fun (b : Fol.binder) -> b.Fol.name, b.Fol.sort) cl.Fol.univ
      in
      let cl_build ~skolem qvar_images =
        let tbl = Hashtbl.create 16 in
        List.iteri
          (fun i (b : Fol.binder) -> Hashtbl.replace tbl b.Fol.id qvar_images.(i))
          cl.Fol.univ;
        let lookup id = Hashtbl.find_opt tbl id in
        (* A Skolem dependency is a dominating universal, always present in [cl.univ] (the
           clause keeps every universal a referenced Skolem depends on), so this lookup is
           total by construction. *)
        let resolve_dep id =
          match lookup id with
          | Some t -> t
          | None ->
            failwith "Fol lowering: unresolved Skolem dependency (internal invariant)"
        in
        (* Skolem functions/constants resolve AFTER universals (their deps are universals,
           never other Skolems — standard Skolemization), so one pass suffices. The binder
           id is the memo [~key]: a single existential referenced by SEVERAL clauses (e.g.
           [exists x. (p x /\ q x)] splitting into ground clauses [p k] and [q k]) MUST
           get ONE witness symbol, not a fresh one per clause — [~key] makes the driver
           reuse the symbol across those clauses (distinct binders keep distinct keys,
           hence fresh symbols). Without it the split weakens the assertion (two
           witnesses) -> wrong sat. *)
        List.iter
          (fun (d : Fol.skolem_descr) ->
            let args = List.map resolve_dep d.Fol.sk_deps in
            let t =
              skolem ~key:d.Fol.sk_binder.Fol.id ~cod:d.Fol.sk_binder.Fol.sort ~args
            in
            Hashtbl.replace tbl d.Fol.sk_binder.Fol.id t)
          cl.Fol.skolems;
        (* Explicit [:pattern] hints carried by the matrix leaves (empty for a ground
           clause or a clause whose foralls shipped no pattern). Empty triggers make the
           loader infer one; non-empty preserves the author's hint. *)
        let triggers =
          let acc = ref [] in
          Fol.iter_atoms (fun q -> acc := leaf_triggers st lookup q @ !acc) cl.Fol.matrix;
          List.rev !acc
        in
        let term_matrix = Fol.map_atoms (read_qleaf st lookup) cl.Fol.matrix in
        fold_matrix st term_matrix, triggers
      in
      let cl_skolems =
        List.map
          (fun (d : Fol.skolem_descr) -> d.Fol.sk_binder.Fol.name, d.Fol.sk_deps)
          cl.Fol.skolems
      in
      { cl_qvars; cl_build; cl_source = s; cl_skolems })
    fol_clauses
;;

(* ---- commands ---- *)

(* Reject user declarations in the reserved fresh-symbol namespace (board #48): a user
   symbol named ".oxsmt.*" would collide with a symbol preprocessing invents, which is
   unsound. Single source of truth = {!Oxsmt_core.Env} (ADR-0012 F1): this parser links
   [oxsmt_core], so it references [Env.is_reserved_name] directly rather than keeping a
   local copy of the prefix (retiring the two-copies drift the old note warned about). *)
let check_not_reserved name =
  if Env.is_reserved_name name
  then
    malformedf
      "declaration of reserved internal symbol %s (%s* is preprocessing-only)"
      name
      Env.reserved_prefix
;;

let declare_sort st name =
  check_not_reserved name;
  if Hashtbl.mem st.sorts name then malformedf "redeclaration of sort %s" name;
  match Env.declare_sort st.env name with
  | sym -> Hashtbl.replace st.sorts name sym
  | exception Env.Reserved_symbol _ -> malformedf "cannot declare reserved symbol %s" name
;;

let declare_fun st name dom cod =
  check_not_reserved name;
  if Hashtbl.mem st.funs name || Hashtbl.mem st.defines name
  then malformedf "redeclaration of symbol %s" name;
  match Env.declare_fun st.env name (Rank.create dom cod) with
  | sym -> Hashtbl.replace st.funs name { sym; dom }
  | exception Env.Reserved_symbol _ -> malformedf "cannot declare reserved symbol %s" name
;;

(* [(define-fun name ((p S)...) Ret body)]: a MACRO. We parse the parameter/result sorts
   now (so undeclared sorts fail here) but store the body unread — it is expanded at each
   use site (see [expand]). define-fun names share the function namespace, so they collide
   with declares and each other; [div]/[mod] stay reserved. *)
let define_fun st name params_sexp ret_sexp body =
  if Hashtbl.mem st.funs name || Hashtbl.mem st.defines name
  then malformedf "redeclaration of symbol %s" name;
  if String.equal name "div" || String.equal name "mod"
  then malformedf "cannot define reserved symbol %s" name;
  let params =
    List.map
      (fun p ->
        match p with
        | Sexp.List [ pn; psort ] ->
          (match Sexp.symbol_name pn with
           | Some pn -> pn, sort_of_sexp st psort
           | None ->
             malformedf "malformed define-fun parameter name: %s" (Sexp.to_string pn))
        | _ -> malformedf "malformed define-fun parameter: %s" (Sexp.to_string p))
      params_sexp
  in
  let ret = sort_of_sexp st ret_sexp in
  Hashtbl.replace st.defines name { params; ret; body }
;;

let read_signature st (params : Sexp.t) (ret : Sexp.t) =
  let dom =
    match params with
    | Sexp.List ps -> List.map (sort_of_sexp st) ps
    | _ -> malformedf "declare-fun parameter list must be a list"
  in
  dom, sort_of_sexp st ret
;;

(* ---- datatypes ---- *)

(* Declare (and store) a function symbol, returning its interned [Symbol.t]. *)
let declare_fun_sym st name dom cod =
  declare_fun st name dom cod;
  (Hashtbl.find st.funs name).sym
;;

(* Intern a datatype sort name (phase 1), marking it so [sort_of_sexp] renders it as
   [Sort.datatype_]. *)
let declare_datatype_sort st name =
  check_not_reserved name;
  if Hashtbl.mem st.sorts name then malformedf "redeclaration of sort %s" name;
  match Env.declare_sort st.env name with
  | sym ->
    Hashtbl.replace st.sorts name sym;
    Hashtbl.replace st.dt_names name ();
    sym
  | exception Env.Reserved_symbol _ -> malformedf "cannot declare reserved symbol %s" name
;;

(* Parse one constructor definition [(C (sel1 S1) ... (seln Sn))] (nullary: [(C)]).
   Declares the constructor [(S1..Sn) -> dt], each selector [(dt) -> Si], and the tester
   [(dt) -> Bool], and returns the {!Datatype_defs.constructor} shape. Field sorts resolve
   through [sort_of_sexp], so they may reference this datatype or a sibling already
   interned in phase 1 (mutual recursion). *)
let parse_constructor st dt_sort (cdef : Sexp.t) : Datatype_defs.constructor =
  match cdef with
  | Sexp.List (cname_s :: sel_sexps) ->
    let cname = name_of cname_s in
    let selectors =
      List.mapi
        (fun index sel ->
          match sel with
          | Sexp.List [ sname_s; ssort_s ] ->
            name_of sname_s, index, sort_of_sexp st ssort_s
          | _ ->
            malformedf
              "malformed selector in constructor %s: %s"
              cname
              (Sexp.to_string sel))
        sel_sexps
    in
    let dom = List.map (fun (_, _, fs) -> fs) selectors in
    let ctor_sym = declare_fun_sym st cname dom dt_sort in
    let sel_records =
      List.map
        (fun (sname, index, field_sort) ->
          let sel_sym = declare_fun_sym st sname [ dt_sort ] field_sort in
          { Datatype_defs.sym = sel_sym; index; field_sort })
        selectors
    in
    let tester_sym = declare_fun_sym st (tester_name_of cname) [ dt_sort ] Sort.bool in
    { Datatype_defs.sym = ctor_sym; selectors = sel_records; tester = tester_sym }
  | _ -> malformedf "malformed constructor definition: %s" (Sexp.to_string cdef)
;;

(* Shared core of [declare-datatype] (one datatype) and [declare-datatypes] (mutually
   recursive block). [sort_decls] are the [(name arity)] pairs, [ctor_lists] the parallel
   constructor-definition lists. Phase 1 interns every sort name first so phase 2's field
   sorts can reference any of them. *)
let process_datatypes st sort_decls ctor_lists =
  if List.length sort_decls <> List.length ctor_lists
  then
    malformedf
      "declare-datatypes: %d sort declarations but %d constructor lists"
      (List.length sort_decls)
      (List.length ctor_lists);
  let sort_syms =
    List.map
      (fun (name, arity) ->
        (match arity with
         | Sexp.Atom (Tok.Numeral "0") -> ()
         | _ ->
           unsupportedf "parametric datatype %s (nonzero arity) is not supported" name);
        name, declare_datatype_sort st name)
      sort_decls
  in
  List.iter2
    (fun (name, sort_sym) ctor_list ->
      let dt_sort = Sort.datatype_ sort_sym in
      let constructors =
        match ctor_list with
        (* A datatype with zero constructors is uninhabited and not well-formed SMT-LIB
           (SMT-LIB 2.6 requires >= 1 constructor); reject rather than register an empty,
           value-less datatype. *)
        | Sexp.List [] -> malformedf "datatype %s has no constructors" name
        | Sexp.List cs -> List.map (parse_constructor st dt_sort) cs
        | _ -> malformedf "malformed constructor list for datatype %s" name
      in
      match Datatype_defs.add st.datatypes { sort_sym; constructors } with
      | dts -> st.datatypes <- dts
      | exception Invalid_argument m -> malformedf "%s" m)
    sort_syms
    ctor_lists
;;

(* The sort-declaration list [((n0 a0) (n1 a1) ...)] of a declare-datatypes block. *)
let parse_sort_decls (s : Sexp.t) =
  match s with
  | Sexp.List decls ->
    List.map
      (fun d ->
        match d with
        | Sexp.List [ n; a ] -> name_of n, a
        | _ -> malformedf "malformed datatype sort declaration: %s" (Sexp.to_string d))
      decls
  | _ -> malformedf "declare-datatypes sort list must be a list: %s" (Sexp.to_string s)
;;

(* Logics we accept at [set-logic]. The quantifier-free family is fully modelled. The
   QUANTIFIED family (UF/LIA + the array/real/datatype supersets present in the public
   corpora) is accepted at the NAME level so the lemma pipeline (ADR-0012) is reached —
   but acceptance of the name is not a promise to model the theory: any construct outside
   the subset (an array/real/datatype sort, a decimal literal, a nonlinear product) still
   fails downstream (a parse-level [Unsupported], or a runtime degrade to [unknown] for a
   lemma body). This is fail-closed by CONSTRUCT, not by logic name — a UFLIA file using
   only UF + linear integers + universals solves; an AUFLIA file touching arrays degrades.
   Universals only: an [exists] in the body is rejected as {!Unsupported} regardless. *)
let known_logic = function
  | "QF_UFLIA"
  | "QF_UF"
  | "QF_LIA"
  | "QF_IDL"
  | "QF_RDL"
  | "QF_DT"
  | "QF_UFDT"
  | "QF_DTLIA"
  | "QF_UFDTLIA"
  (* quantifier-free arrays (select/store/extensionality). Arrays combined with arithmetic
     (QF_ALIA/QF_AUFLIA) are accepted at the name level too, but arithmetic atoms fall
     outside the standalone arrays theory and degrade to unknown per the fail-closed
     CONSTRUCT discipline. *)
  | "QF_AX"
  | "QF_ALIA"
  | "QF_AUFLIA"
  (* bitvectors (bit-blasted); BV combined with arithmetic accepted at the name level. *)
  | "QF_BV"
  | "QF_UFBV"
  | "QF_BVLIA"
  | "QF_UFBVLIA" -> true
  (* quantified UF/LIA family + array/real/datatype supersets seen in ../corpora *)
  | "UF"
  | "UFLIA"
  | "UFIDL"
  | "UFLRA"
  | "LIA"
  | "LRA"
  | "AUFLIA"
  | "AUFLIRA"
  | "ALIA"
  | "AUFDTLIA"
  | "UFDT"
  | "UFDTLIA" -> true
  | _ -> false
;;

let run st sexps =
  let logic = ref None in
  let status = ref None in
  let asserts = ref [] in
  let lemmas = ref [] in
  let existentials = ref [] in
  (* Clauses from the front-end quantified pipeline (dark: [OXSMT_QUANT_PIPELINE]). Empty
     unless the flag is ON, in which case a quantifier-bearing assertion routes here
     instead of to {!lemmas}/{!existentials}; ground (non-quantifier) assertions still
     take the ordinary {!asserts} path. *)
  let clauses = ref [] in
  (* Count of assertion content the reader could not represent and DROPPED (partial
     assertion, below). Surfaced on {!t} so the shared loader arms a sentinel lemma when
     [dropped > 0] — the live-lemma soundness rule then degrades any [Sat] to [Unknown],
     so a dropped conjunct can never yield a wrong [sat] (dropping only WEAKENS the set,
     sound for [unsat]). *)
  let dropped = ref 0 in
  (* Classify an assertion body: a top-level [forall] (optionally wrapped in a [(! ...)]
     annotation, e.g. [:named]) becomes a lemma; anything else is a ground term. A
     [forall] nested INSIDE a term (not the assertion root) stays out of the fragment —
     [read_term] rejects it — so only assertion-root universals are lifted to lemmas. *)
  let rec classify (b : Sexp.t) =
    match b with
    | Sexp.List (Sexp.Atom (Tok.Reserved "forall") :: tail) ->
      `Lemma (read_forall st tail)
    | Sexp.List (Sexp.Atom (Tok.Reserved "!") :: inner :: attrs) ->
      (* F2: validate the annotation tail before dropping it — otherwise a malformed tail
         (e.g. a bare [forall] mistaken for an attribute) is silently discarded and can
         drop a live quantifier (wrong [sat]). *)
      validate_bang_attrs attrs;
      classify inner
    | _ ->
      let t = read_term st Scope.empty b in
      if not (Sort.equal t.Term.sort Sort.bool) then malformedf "assertion is not Bool";
      `Ground t
  in
  (* Does the s-expression syntactically contain a [forall]/[exists]? Gates partial
     assertion (below) to quantifier-bearing assertions only, so a NON-quantifier reader
     rejection (recursion, arity/sort mismatch) still fails the file exactly as before. *)
  let rec contains_quantifier (s : Sexp.t) =
    match s with
    | Sexp.Atom (Tok.Reserved ("forall" | "exists")) -> true
    | Sexp.Atom _ -> false
    | Sexp.List items -> List.exists contains_quantifier items
  in
  (* Partial assertion (lemmas-climb). Only invoked when {!classify} of a
     QUANTIFIER-bearing assertion RAISED [Unsupported] (i.e. the file would otherwise
     parse-fail on an out-of-fragment quantifier): salvage the representable core instead
     of dropping the whole file. A top-level [(and c1 c2 ...)] is split so a representable
     conjunct survives a sibling outside the fragment ([and] is a core operator, never
     user-redefinable, so splitting a Boolean conjunction is always sound); a [(! ...)]
     wrapper is unwrapped (its tail still VALIDATED — a [Malformed] tail propagates, never
     silently dropped); a root [forall] becomes a lemma; anything else is read as a ground
     term. A conjunct/leaf that is [Unsupported] (out of fragment) is DROPPED and counted
     — never fatal to the rest of the file, and never silently unsound (the [dropped]
     sentinel forces [Sat] -> [Unknown]). [Malformed] (genuinely ill-formed) is NOT
     caught: it propagates and fails the file, exactly as without partial assertion. *)
  let rec take (b : Sexp.t) =
    match b with
    | Sexp.List (Sexp.Atom (Tok.Reserved "!") :: inner :: attrs) ->
      validate_bang_attrs attrs;
      take inner
    | Sexp.List (head :: (_ :: _ :: _ as conjs)) when Sexp.simple head = Some "and" ->
      List.iter take conjs
    | Sexp.List (Sexp.Atom (Tok.Reserved "exists") :: tail) ->
      (* Positive existential: record it for fresh-witness Skolemization. Reading the
         binders may raise [Malformed] (bad binder), which propagates as a hard fail;
         [Unsupported] (out-of-subset binder sort) is a drop. *)
      (match read_exists st tail with
       | ex -> existentials := ex :: !existentials
       | exception Unsupported _ -> incr dropped)
    | Sexp.List [ head; Sexp.List (Sexp.Atom (Tok.Reserved "exists") :: tail) ]
      when Sexp.simple head = Some "not" ->
      (match read_negated_exists st tail with
       | lemma -> lemmas := lemma :: !lemmas
       | exception Unsupported _ -> incr dropped)
    | Sexp.List [ head; Sexp.List (Sexp.Atom (Tok.Reserved "forall") :: tail) ]
      when Sexp.simple head = Some "not" ->
      (match read_negated_forall st tail with
       | ex -> existentials := ex :: !existentials
       | exception Unsupported _ -> incr dropped)
    | _ ->
      (match classify b with
       | `Ground t -> asserts := t :: !asserts
       | `Lemma l -> lemmas := l :: !lemmas
       | exception Unsupported _ -> incr dropped)
  in
  (* Pipeline routing (dark: [OXSMT_QUANT_PIPELINE]) for a quantifier-bearing assertion.
     Splits a top-level [(and ...)] and unwraps [(! ...)] first — same partial-assertion
     granularity as {!take}, so one out-of-fragment conjunct drops alone rather than
     sinking its representable siblings. Each piece is clausified (NNF -> Skolemize ->
     lower); an [Unsupported] piece is dropped and counted (sentinel), a [Malformed] piece
     propagates as a hard fail (-> unknown), exactly as {!take}. *)
  let rec take_ir (b : Sexp.t) =
    match b with
    | Sexp.List (Sexp.Atom (Tok.Reserved "!") :: inner :: attrs) ->
      validate_bang_attrs attrs;
      take_ir inner
    | Sexp.List (head :: (_ :: _ :: _ as conjs)) when Sexp.simple head = Some "and" ->
      List.iter take_ir conjs
    | _ ->
      (match clauses_of_assertion st b with
       | cls -> List.iter (fun c -> clauses := c :: !clauses) cls
       | exception (Unsupported _ | Term.Unsupported _ | Term.Overflow) -> incr dropped)
  in
  (* Extract a declared name (any symbol atom, quoted or not). *)
  let name_of s =
    match Sexp.symbol_name s with
    | Some n -> n
    | None -> malformedf "expected a symbol name, got %s" (Sexp.to_string s)
  in
  List.iter
    (fun (cmd : Sexp.t) ->
      (* Command keywords are UNQUOTED symbol heads; dispatch on that text. *)
      match cmd with
      | Sexp.Atom _ -> malformedf "unexpected top-level atom: %s" (Sexp.to_string cmd)
      | Sexp.List [] -> malformedf "malformed command: ()"
      | Sexp.List (head :: rest) ->
        (match Sexp.simple head, rest with
         | Some "set-logic", [ l ] ->
           (match Sexp.simple l with
            | Some l when known_logic l -> logic := Some l
            | Some l ->
              unsupportedf "unsupported logic: %s (need QF_UF/QF_LIA/QF_UFLIA)" l
            | None -> malformedf "malformed set-logic argument")
         | Some "set-info", _ ->
           (match rest with
            | [ Sexp.Atom (Tok.Keyword "status"); v ] ->
              (match Sexp.simple v with
               | Some v ->
                 (match Oxsmt_smtlib.Status.of_string v with
                  | Some s -> status := Some s
                  | None -> malformedf "unknown :status value: %s" v)
               | None -> malformedf "malformed :status value")
            | _ -> () (* ignore other :info, incl. multi-line |...| / string values *))
         | Some "declare-sort", [ n; arity ] ->
           (match arity with
            | Sexp.Atom (Tok.Numeral "0") -> declare_sort st (name_of n)
            | _ -> unsupportedf "declare-sort %s with nonzero arity" (name_of n))
         | Some "declare-const", [ n; ret ] ->
           declare_fun st (name_of n) [] (sort_of_sexp st ret)
         | Some "declare-fun", [ n; params; ret ] ->
           let dom, cod = read_signature st params ret in
           declare_fun st (name_of n) dom cod
         (* [(declare-datatypes ((T0 a0) ...) (ctor-list0 ...))] — mutually recursive. *)
         | Some "declare-datatypes", [ sort_decls; Sexp.List ctor_lists ] ->
           process_datatypes st (parse_sort_decls sort_decls) ctor_lists
         | Some "declare-datatypes", _ ->
           malformedf "malformed declare-datatypes: %s" (Sexp.to_string cmd)
         (* [(declare-datatype T (ctor ...))] — the single-datatype (arity 0) form. *)
         | Some "declare-datatype", [ n; Sexp.List ctors ] ->
           process_datatypes
             st
             [ name_of n, Sexp.Atom (Tok.Numeral "0") ]
             [ Sexp.List ctors ]
         | Some "declare-datatype", _ ->
           malformedf "malformed declare-datatype: %s" (Sexp.to_string cmd)
         | Some "define-fun", [ n; Sexp.List params; ret; body ] ->
           define_fun st (name_of n) params ret body
         | Some ("define-fun-rec" | "define-funs-rec"), _ ->
           unsupportedf "recursive define-fun-rec / define-funs-rec is not supported"
         | Some "define-fun", _ ->
           malformedf "malformed define-fun: %s" (Sexp.to_string cmd)
         | Some "assert", [ body ]
           when Lazy.force quant_pipeline_enabled && has_quantifier body ->
           (* Front-end quantified pipeline (dark): route quantifier-bearing assertions
              through the typed formula IR. Non-quantifier assertions (and the whole file
              when the flag is OFF) take the byte-identical path below. *)
           take_ir body
         | Some "assert", [ body ] ->
           (match classify body with
            | `Ground t -> asserts := t :: !asserts
            | `Lemma l -> lemmas := l :: !lemmas
            (* A QUANTIFIER-bearing assertion that classify could not represent: rather
               than fail the whole file, salvage its representable core via partial
               assertion (only [Unsupported] leaves are dropped; [Malformed] still
               propagates). A non-quantifier rejection re-raises unchanged — recursion /
               arity / sort errors fail the file exactly as before.
               Currently-representable assertions never reach this arm, so their behaviour
               is byte-identical. *)
            | exception ((Malformed _ | Unsupported _) as e) ->
              if contains_quantifier body then take body else raise e)
         | Some "check-sat", _ -> ()
         | Some "exit", _ -> ()
         | Some ("push" | "pop"), _ ->
           unsupportedf "incremental push/pop is not supported"
         | Some ("get-model" | "get-value" | "get-unsat-core"), _ -> ()
         (* Output-only / non-stateful directives: ignoring them cannot change the
            assertion set, hence cannot flip a verdict. *)
         | Some "set-option", _ -> ()
         | Some (("reset" | "reset-assertions") as c), _ ->
           (* Fail CLOSED — NOT a silent no-op. This reader folds every [assert] into ONE
              assertion set for a single [check-sat], so it cannot honour [reset] /
              [reset-assertions] clearing that set mid-script. Silently ignoring them left
              the pre-reset assertions live and FLIPPED the verdict (e.g.
              [(assert (= 0 1)) (reset-assertions) (check-sat)] is [sat] but came out
              [unsat]). Raising degrades the query to [unknown] (I8), never a wrong
              verdict; incremental support is a documented follow-up (see the CLI's
              push/pop degrade). *)
           unsupportedf "%s is not supported by the batch (single-check) reader" c
         | Some other, _ -> unsupportedf "unsupported command: %s" other
         | None, _ -> malformedf "malformed command: %s" (Sexp.to_string cmd)))
    sexps;
  ( !logic
  , !status
  , List.rev !asserts
  , List.rev !lemmas
  , List.rev !existentials
  , List.rev !clauses
  , !dropped )
;;

let parse_into_sexps ?internal_mint env ctx (sexps : Sexp.t list) =
  let st =
    { ctx
    ; env
    ; sorts = Hashtbl.create 16
    ; funs = Hashtbl.create 64
    ; defines = Hashtbl.create 16
    ; expanding = Hashtbl.create 8
    ; memo = Hashtbl.create 64
    ; dt_names = Hashtbl.create 8
    ; datatypes = Datatype_defs.empty
    ; internal_mint
    ; array_ops = Hashtbl.create 8
    ; arrays = Array_defs.empty
    }
  in
  let logic, status, assertions, lemmas, existentials, clauses, dropped =
    try run st sexps with
    | Term.Sort_error m -> raise (Malformed ("sort error: " ^ m))
    | Term.Unsupported m -> raise (Unsupported m)
    | Term.Overflow -> raise (Unsupported "arithmetic exceeds native int range")
  in
  { env
  ; ctx
  ; logic
  ; status
  ; assertions
  ; datatypes = st.datatypes
  ; arrays = st.arrays
  ; lemmas
  ; existentials
  ; clauses
  ; dropped
  }
;;

let parse_into ?internal_mint env ctx src =
  let sexps =
    try Sexp.parse_many src with
    | Sexp.Malformed m -> raise (Malformed ("s-expression: " ^ m))
    | Tok.Error m -> raise (Malformed ("lexical: " ^ m))
  in
  parse_into_sexps ?internal_mint env ctx sexps
;;

let parse src =
  (* A standalone parse owns its env, so it builds its OWN cap-backed [Internal_minter]
     (board #58 O-MINTER) and threads it — a theory that mints a reserved marker mid-parse
     (bit-vectors, arrays) resolves rather than raising [Malformed]. Sound because the cap
     and env are local to this parse and never leave it (contrast a [Session]-driven
     [parse_into], where the cap stays private to the Session and only its opaque
     [parse_minter] token is threaded). The [admit] gate is the same parse-time theory
     vocabulary the Session sanctions — the bit-vector marker grammar (one predicate per
     line so a further theory ORs in merge-friendly); it is PAIRED with the consuming-side
     rank/sort check ([Bv.view]) that keeps a mismatched mint inert. *)
  let env, cap = Env.create_with_cap () in
  let ctx = Context.create env in
  let minter =
    Internal_minter.create
      ~admit:(fun name -> Array_defs.is_op_name name || Bv.is_bv_name name)
      cap
      env
  in
  parse_into ~internal_mint:minter env ctx src
;;
