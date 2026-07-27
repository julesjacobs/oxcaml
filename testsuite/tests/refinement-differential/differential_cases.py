"""The operations whose two meanings could differ, and where they would show.

Every primitive the verifier models has two meanings: the one the compiled
program executes and the one the backend reasons about.  This module names
those operations, fixes the operand values at which a wrong translation would
show, and records whether the verifier is expected to commit to a value at
all.  It computes no answers of its own: the machine supplies them, and the
driver compares them with what the backend proves.

A case is an operation applied to literal operands.  Each case is rendered
twice.  The folded form is what a constant folder may reduce at compile time;
the opaque form puts every operand behind [Sys.opaque_identity] so the
operation runs as a machine instruction.  Both forms are executed under
bytecode and under native code, and a case whose four answers do not all
agree has no single machine meaning, so the verifier must not commit to one.
"""

WIDTH = 63
MAX_INT = (1 << (WIDTH - 1)) - 1
MIN_INT = -(1 << (WIDTH - 1))

# The smallest non-negative operand whose square leaves the range: with
# [max_int] at 2^62 - 1, that is 2^31, whose square is exactly 2^62.  Its
# predecessor is the largest square that stays in range, and the two together
# are the boundary.
ROOT = 1 << ((WIDTH - 1) // 2)
ROOT_BELOW = ROOT - 1

# About the square root of 2^63, which also overflows but is not the boundary.
# Kept in the sweep because it is a different bit pattern, not because it is
# the first product to leave the range.
WIDE_ROOT = 3037000499


def wrap(value):
    """Reduce an integer to the machine's range, as the hardware does."""
    masked = value & ((1 << WIDTH) - 1)
    return masked - (1 << WIDTH) if masked > MAX_INT else masked


def literal(value):
    """OCaml source for an integer or boolean value."""
    if isinstance(value, bool):
        return "true" if value else "false"
    return str(value) if value >= 0 else "(%d)" % value


class Case:
    """One operation at one choice of operands."""

    def __init__(self, family, operator, form, sort, operands, modelled=True):
        self.family = family
        self.operator = operator
        self.form = form
        self.sort = sort
        self.operands = operands
        # Whether the verifier is expected to prove the machine's answer.
        # False says the operation is deliberately left uninterpreted, so
        # every answer must be refused rather than any one of them proved.
        self.modelled = modelled

    @property
    def key(self):
        spelled = ".".join(
            operand.replace("(", "").replace(")", "").replace("-", "m")
            for operand in self.operands
        )
        return "%s.%s.%s" % (self.family, self.operator, spelled)

    def render(self, wrapper=None):
        def operand(text):
            return text if wrapper is None else "%s (%s)" % (wrapper, text)

        if self.form == "infix":
            return "(%s) %s (%s)" % (
                operand(self.operands[0]),
                self.operator,
                operand(self.operands[1]),
            )
        return "%s (%s)" % (self.operator, operand(self.operands[0]))


ARITHMETIC = ["+", "-", "*"]
BITWISE = ["land", "lor", "lxor"]
SHIFTS = ["lsl", "lsr", "asr"]
UNARY = ["~-", "succ", "pred"]
COMPARISONS = ["<", "<=", ">", ">=", "=", "<>"]
DIVISION = ["/", "mod"]

# A shift distance outside this range is unspecified in OCaml, so the answer
# the program happens to produce is not a semantic commitment.
SPECIFIED_SHIFT = range(0, WIDTH + 1)

CORE_ARITHMETIC_PAIRS = [
    ("max_int", "max_int"),
    ("max_int", "min_int"),
    ("min_int", "min_int"),
    ("min_int", "max_int"),
    ("max_int", "1"),
    ("max_int", "(-1)"),
    ("min_int", "1"),
    ("min_int", "(-1)"),
    ("0", "max_int"),
    ("0", "min_int"),
    ("1", "(-1)"),
    ("(-1)", "(-1)"),
    ("2", "(-2)"),
    (str(ROOT), str(ROOT)),
    (str(ROOT_BELOW), str(ROOT_BELOW)),
    (str(ROOT), "(-%d)" % ROOT),
    (str(WIDE_ROOT), str(WIDE_ROOT)),
    ("1073741824", "1073741824"),
]

CORE_BITWISE_PAIRS = [
    ("max_int", "min_int"),
    ("max_int", "(-1)"),
    ("min_int", "(-1)"),
    ("0", "(-1)"),
    ("(-1)", "(-1)"),
    ("1073741824", "(-1)"),
    ("max_int", "1"),
    ("min_int", "1"),
]

CORE_SHIFT_OPERANDS = ["max_int", "min_int", "1", "(-1)"]
# Zero, one, one short of the width, exactly the width, one past it, and a
# negative distance.  The last two are outside what OCaml specifies.
CORE_SHIFT_DISTANCES = [0, 1, WIDTH - 1, WIDTH, WIDTH + 1, -1]

CORE_UNARY_OPERANDS = ["max_int", "min_int", "0", "(-1)"]

CORE_COMPARISON_PAIRS = [
    ("max_int", "min_int"),
    ("min_int", "max_int"),
    ("max_int", "max_int"),
    ("0", "(-1)"),
    ("(-1)", "0"),
    ("1", "(-1)"),
]

# Truncation towards zero against rounding towards minus infinity, the
# overflowing quotient, and every division by zero.
CORE_DIVISION_PAIRS = [
    ("6", "2"),
    ("7", "2"),
    ("(-7)", "2"),
    ("7", "(-2)"),
    ("(-7)", "(-2)"),
    ("min_int", "(-1)"),
    ("min_int", "1"),
    ("max_int", "(-1)"),
    # Where the quotient or the remainder sits at an extreme of the range.
    ("max_int", "2"),
    ("min_int", "2"),
    ("max_int", "max_int"),
    ("min_int", "min_int"),
    ("max_int", "min_int"),
    ("min_int", "max_int"),
    ("0", "min_int"),
    ("(-1)", "max_int"),
    ("1", "min_int"),
    (str(MAX_INT), "(-1)"),
    (str(MIN_INT), "(-1)"),
    ("1", "0"),
    ("0", "0"),
    ("(-1)", "0"),
    ("min_int", "0"),
]

# The same boundaries written as numerals rather than as [max_int] and
# [min_int], which reach the verifier by a different route.
CORE_NUMERAL_PAIRS = [
    (str(MAX_INT), "1"),
    (str(MIN_INT), "(-1)"),
    (str(MAX_INT), str(MAX_INT)),
    (str(MIN_INT), "1"),
]

FULL_VALUES = [
    "max_int",
    "min_int",
    "0",
    "1",
    "(-1)",
    "2",
    "(-2)",
    str(ROOT),
    str(ROOT_BELOW),
    "(-%d)" % ROOT,
    str(WIDE_ROOT),
    "1073741824",
    "(-1073741824)",
    str(MAX_INT - 1),
]

FULL_SHIFT_DISTANCES = [0, 1, 2, 31, 32, WIDTH - 2, WIDTH - 1, WIDTH, WIDTH + 1, -1]


def _binary(family, operators, pairs, modelled=True):
    return [
        Case(family, operator, "infix", "int", [left, right], modelled)
        for operator in operators
        for left, right in pairs
    ]


def _shifts(operands, distances):
    return [
        Case(
            "shift",
            operator,
            "infix",
            "int",
            [operand, literal(distance)],
            distance in SPECIFIED_SHIFT,
        )
        for operator in SHIFTS
        for operand in operands
        for distance in distances
    ]


def _unary(operands):
    return [
        Case("unary", operator, "prefix", "int", [operand])
        for operator in UNARY
        for operand in operands
    ]


def _comparisons(pairs):
    return [
        Case("compare", operator, "infix", "bool", [left, right])
        for operator in COMPARISONS
        for left, right in pairs
    ]


def _division(pairs):
    # Division and remainder are modelled, but only where the machine gives
    # an answer.  A zero divisor raises, so the verifier must prove no value
    # for it; the probes below pin the answers a bitvector theory would hand
    # out there if the guard were dropped.
    return [
        Case(
            "divmod",
            operator,
            "infix",
            "int",
            [left, right],
            _value_of(right) != 0,
        )
        for operator in DIVISION
        for left, right in pairs
    ]


def core_cases():
    return (
        _binary("arith", ARITHMETIC, CORE_ARITHMETIC_PAIRS)
        + _binary("numeral", ARITHMETIC, CORE_NUMERAL_PAIRS)
        + _binary("bitwise", BITWISE, CORE_BITWISE_PAIRS)
        + _shifts(CORE_SHIFT_OPERANDS, CORE_SHIFT_DISTANCES)
        + _unary(CORE_UNARY_OPERANDS)
        + _comparisons(CORE_COMPARISON_PAIRS)
        + _division(CORE_DIVISION_PAIRS)
    )


def full_cases():
    pairs = [(left, right) for left in FULL_VALUES for right in FULL_VALUES]
    return (
        _binary("arith", ARITHMETIC, pairs)
        # The boundaries spelled as numerals rather than as [max_int] and
        # [min_int]; the sweep's own operand list carries neither spelling of
        # the extremes as a numeral.
        + _binary("numeral", ARITHMETIC, CORE_NUMERAL_PAIRS)
        + _binary("bitwise", BITWISE, pairs)
        + _shifts(FULL_VALUES, FULL_SHIFT_DISTANCES)
        + _unary(FULL_VALUES)
        + _comparisons(pairs)
        + _division(pairs)
    )


def lean_cases():
    """A thinner table for the backend that costs the most per obligation.

    Lean takes several times what an SMT obligation takes and is held to one
    process at a time, so the ordinary gate cannot afford the whole core here.
    Every operator keeps the operands where a wrong translation would show:
    both extremes of the range, the shift distances at and past the word, the
    truncation and overflow choices in division, and division by zero.  The
    remaining core operands run against Lean in the offline sweep.
    """
    return (
        _binary(
            "arith",
            ARITHMETIC,
            [
                ("max_int", "max_int"),
                ("min_int", "min_int"),
                ("max_int", "1"),
                ("min_int", "(-1)"),
            ],
        )
        + _binary("numeral", ARITHMETIC, [(str(MAX_INT), "1")])
        + _binary(
            "bitwise",
            BITWISE,
            [("max_int", "min_int"), ("min_int", "(-1)"), ("0", "(-1)")],
        )
        + _shifts(["max_int", "min_int"], [0, WIDTH - 1, WIDTH, WIDTH + 1])
        + _unary(["max_int", "min_int"])
        + _comparisons([("max_int", "min_int"), ("min_int", "max_int")])
        + _division(
            [
                ("(-7)", "2"),
                ("7", "(-2)"),
                ("min_int", "(-1)"),
                ("1", "0"),
                ("min_int", "0"),
            ]
        )
    )


PROFILES = {"core": core_cases, "lean": lean_cases, "full": full_cases}


def cases(profile):
    return PROFILES[profile]()


def wrong_witnesses(case, observed):
    """Answers the verifier must not prove for this case.

    [observed] is the machine's answer, or None where the operation raised.
    Proving any of these would mean the backend and the machine disagree; and
    because they are refused even where the machine's own answer is proved,
    they also show that the obligation is not vacuous.
    """
    witnesses = []

    def add(value):
        if value != observed and value not in witnesses:
            witnesses.append(value)

    if case.sort == "bool":
        if observed is not None:
            add(not observed)
        return witnesses

    if case.family == "divmod":
        left, right = (_value_of(text) for text in case.operands)
        if right == 0:
            # What a bitvector theory answers for division by zero if the
            # translation simply hands the operation to the solver: the
            # SMT-LIB signed quotient, and the dividend for the remainder.
            if case.operator == "/":
                add(-1)
                add(1)
            else:
                add(wrap(left))
            add(0)
        else:
            quotient = left // right
            remainder = left - quotient * right
            # Rounding towards minus infinity rather than towards zero.
            add(wrap(quotient if case.operator == "/" else remainder))
        if observed is not None:
            add(wrap(observed + 1))
        return witnesses[:3]

    if observed is not None:
        add(wrap(observed + 1))
    return witnesses


_NAMED = {"max_int": MAX_INT, "min_int": MIN_INT}


def _value_of(text):
    stripped = text.strip()
    if stripped.startswith("(") and stripped.endswith(")"):
        stripped = stripped[1:-1]
    if stripped in _NAMED:
        return _NAMED[stripped]
    return int(stripped)


def observation_program(cases_):
    """An OCaml program that prints what the machine does for each case."""
    lines = [
        "let render_int f =",
        "  match f () with",
        "  | value -> string_of_int value",
        '  | exception exn -> "!" ^ Printexc.to_string exn',
        ";;",
        "",
        "let render_bool f =",
        "  match f () with",
        "  | value -> string_of_bool value",
        '  | exception exn -> "!" ^ Printexc.to_string exn',
        ";;",
        "",
        "let emit key folded opaque =",
        "  print_string key; print_char '\\t';",
        "  print_string folded; print_char '\\t';",
        "  print_string opaque; print_newline ()",
        ";;",
        "",
        'let () = emit "#int_size" (string_of_int Sys.int_size) '
        "(string_of_int Sys.int_size)",
        "",
    ]
    for case in cases_:
        render = "render_int" if case.sort == "int" else "render_bool"
        lines.append(
            'let () = emit "%s" (%s (fun () -> %s)) (%s (fun () -> %s))'
            % (
                case.key,
                render,
                case.render(),
                render,
                case.render(wrapper="Sys.opaque_identity"),
            )
        )
    return "\n".join(lines) + "\n"


def obligation_program(case, witness):
    """A single obligation: the operation, and the answer to prove of it."""
    return "let probe = ((%s) : %s{ _ = %s })\n" % (
        case.render(),
        case.sort,
        literal(witness),
    )
