(** Fixed-width bitvector vocabulary (GOALS: bitvectors). The FRONT half of the bitvector
    lane: builds bitvector operator/literal {!Term.t}s and classifies them for the
    bit-blasting engine (the BACK half), which supplies the bit-level semantics.

    {b Representation.} A bitvector operator or literal is an ordinary [App (sym, args)]
    term whose result sort is [Sort.BitVec w] — exactly the datatypes precedent, so the
    frozen 9-node {!Term.t} set is untouched and EUF congruence-closes bitvector terms for
    free (sound: the same operator on the same arguments is the same value; the bit-level
    axioms come from the blaster, not the combinator).

    {b Symbol identity.} Each specialised instance (operator + operand widths + any
    indices, or a literal's value + width) memoises a distinct {!Symbol.t} whose {e name}
    encodes that instance. The names live in the reserved [.oxsmt.bv.*] sub-namespace
    (board #58): the public declaration doors ([Env.declare_fun]/[declare_sort] and their
    [Session] wrappers) reject any [.oxsmt.*] name and the SMT-LIB reader rejects a user
    declaration of one, so a bitvector symbol can never collide with a user-declared
    function even though interning is by name. Because the name carries the full instance,
    {!view} decodes a term with no side registry to thread (contrast {!Datatype_defs});
    the classification is a pure function of the term.

    Construction goes through {!Context}'s smart constructors (I2): each builder mints the
    instance's rank through a cap-backed {!minter} (the reserved namespace can only be
    minted via {!Env.declare_reserved}, so the builder cannot use the public door) and
    calls {!Context.app}/{!Context.const}, which sort-checks the operands against that
    rank. The builders {e also} width-check up front and raise {!Term.Sort_error} with a
    specific message (fail-closed, ADR release [-noassert] safe) so a width error is a
    clear parse-time failure. *)

(** The v1 operator set. Comparisons ([Bvult]/[Bvule]/[Bvslt]/[Bvsle]) yield [Bool]; every
    other operator yields a bitvector. The signed/unsigned "greater" duals and the strict
    forms are parser sugar rewritten to these, so the blaster sees only four predicates. *)
type op =
  | Bvnot
  | Bvand
  | Bvor
  | Bvxor
  | Bvneg
  | Bvadd
  | Bvsub
  | Bvmul
  | Bvudiv (* SMT-LIB total: [bvudiv x 0 = ~0] (all ones) *)
  | Bvurem (* SMT-LIB total: [bvurem x 0 = x] *)
  | Bvshl
  | Bvlshr
  | Bvashr
  | Bvult
  | Bvule
  | Bvslt
  | Bvsle
  | Concat
  | Extract of int * int (* (i, j): bits [i .. j] inclusive, [i >= j] *)
  | Zero_extend of int (* prepend [n] zero bits *)
  | Sign_extend of int (* prepend [n] copies of the sign bit *)

(** The classification the bit-blasting engine reads. [Const] is a bitvector literal:
    [value] is canonical in [0, 2^width). [Op] is an operator application: [args] are the
    operand terms (each carries its own [Sort.BitVec] so the blaster reads operand widths
    off them), and [result_width] is [Some w] for a bitvector result or [None] for a
    [Bool]-valued comparison. *)
type view =
  | Const of
      { value : Bigint.t
      ; width : int
      }
  | Op of
      { op : op
      ; args : Term.t list
      ; result_width : int option
      }

(** [view t] is [Some] iff [t] is a bitvector operator/literal application built by this
    module (recognised by the symbol-name prefix), else [None]. Pure — no registry. *)
val view : Term.t -> view option

(** [is_bv_sym sym] iff [sym] is a bitvector operator/literal symbol minted here. Lets the
    printer / declaration collector skip these built-in symbols (they are never emitted as
    [declare-fun], like the reserved [div]/[mod]). *)
val is_bv_sym : Symbol.t -> bool

(** [is_bv_name name] iff [name] is in the bit-vector marker namespace ([.oxsmt.bv|...]).
    This is the bit-vector [admit] grammar for the parse-time reserved minter (board #58
    O-MINTER): {!Oxsmt_interface.Session.parse_minter} and a standalone parse sanction
    exactly these names. Admitting the grammar OBLIGATES the consuming-side rank/sort
    check ({!view} verifies the decoded op's operand and result sorts against the term's
    actual sorts), which keeps a mismatched mint inert. *)
val is_bv_name : string -> bool

(** [width_of_sort s] is [Some w] iff [s] is [Sort.BitVec w]. *)
val width_of_sort : Sort.t -> int option

(** [bits_lsb value ~width] is the little-endian (index 0 = least-significant) bit array
    of [value] taken modulo [2^width], length [width]. The blaster's bit-access helper,
    since {!Bigint} exposes no bit operations. *)
val bits_lsb : Bigint.t -> width:int -> bool array

(** A cap-backed minter for the reserved bitvector namespace: [mint name rank] interns a
    [.oxsmt.bv.*] [name] with [rank]. Board #58 O-MINTER: a [Session]-driven parse applies
    {!Oxsmt_interface.Session.parse_minter} (an opaque {!Internal_minter.t}) via
    [Internal_minter.mint]; a standalone {!Oxsmt_smtlib_parser}[.parse] builds an
    [Internal_minter] over its own capped env. The builders take a minter (not an
    {!Env.t}) because the reserved namespace is unreachable through the public
    [Env.declare_fun] door. *)
type minter = string -> Rank.t -> Symbol.t

(* ---- Smart constructors. Each takes the session [ctx] and a reserved-namespace
   {!minter}; each width-checks and raises {!Term.Sort_error} on a violation, then builds
   through {!Context}. ---- *)

(** [const ctx mint ~value ~width] is the literal [value] (reduced into [0, 2^width)) of the
    given [width]. Raises [Invalid_argument] if [width < 1]. *)
val const : Context.t -> minter -> value:Bigint.t -> width:int -> Term.t

(** [unop ctx mint op x] applies a unary operator ([Bvnot] or [Bvneg]); result width
    equals [x]'s. Raises {!Term.Sort_error} if [x] is not a bitvector or [op] is not
    unary. *)
val unop : Context.t -> minter -> op -> Term.t -> Term.t

(** [binop ctx mint op x y] applies an equal-width binary operator: the bitwise ops, the
    arithmetic ops, the three shifts, and the four comparisons. [x] and [y] must be
    bitvectors of the {e same} width (shift amount shares the operand width, SMT-LIB).
    Result is that width, except a comparison which is [Bool]. Raises {!Term.Sort_error}
    on a width mismatch, a non-bitvector operand, or a non-binary [op]. *)
val binop : Context.t -> minter -> op -> Term.t -> Term.t -> Term.t

(** [concat ctx mint hi lo] is the concatenation ([hi] the high bits); result width is the
    sum. Raises {!Term.Sort_error} if either operand is not a bitvector. *)
val concat : Context.t -> minter -> Term.t -> Term.t -> Term.t

(** [extract ctx mint ~i ~j x] extracts bits [i .. j] inclusive ([i >= j >= 0],
    [i < width x]); result width is [i - j + 1]. Raises {!Term.Sort_error} on an
    out-of-range index or a non-bitvector operand. *)
val extract : Context.t -> minter -> i:int -> j:int -> Term.t -> Term.t

(** [zero_extend ctx mint ~n x] prepends [n >= 0] zero bits; result width is
    [width x + n]. Raises {!Term.Sort_error} if [n < 0] or [x] is not a bitvector. *)
val zero_extend : Context.t -> minter -> n:int -> Term.t -> Term.t

(** [sign_extend ctx mint ~n x] prepends [n >= 0] copies of [x]'s sign bit; result width
    is [width x + n]. Raises {!Term.Sort_error} if [n < 0] or [x] is not a bitvector. *)
val sign_extend : Context.t -> minter -> n:int -> Term.t -> Term.t
