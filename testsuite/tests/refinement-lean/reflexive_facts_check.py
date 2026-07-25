"""Fail if any verification condition carries a fact of the form [a = a].

A fact whose two sides are the same term holds at every instantiation, so it
constrains nothing while costing solver input, proof-pane lines and hydration
payload.  Compare the emitted terms rather than their rendered form: a global
reference and a local binder can print with the same name while denoting
different terms, and those facts do carry content.
"""

import json
import re
import sys

APPLY = re.compile(r"^\(app\[Stdlib!\.=\]\s+(.*)\)$", re.S)


def operands(text):
    """Split the two arguments of a top-level Stdlib.( = ) application."""
    match = APPLY.match(text.strip())
    if match is None:
        return None
    depth = 0
    body = match.group(1)
    pieces = []
    current = []
    for token in re.split(r"(\s+|\(|\))", body):
        if token == "(":
            depth += 1
        elif token == ")":
            depth -= 1
        if token.strip() == "" and depth == 0 and current:
            pieces.append("".join(current))
            current = []
            continue
        if token.strip() or depth > 0:
            current.append(token)
    if current:
        pieces.append("".join(current))
    pieces = [piece for piece in pieces if piece.strip()]
    return pieces if len(pieces) == 2 else None


def main(path):
    document = json.load(open(path))
    offenders = []
    total = 0
    for condition in document.get("verification_conditions", []):
        for fact in condition.get("facts") or []:
            total += 1
            split = operands(fact.get("text") or "")
            if split is not None and split[0] == split[1]:
                offenders.append(fact.get("display") or fact.get("text"))
    if offenders:
        print("reflexive facts emitted:")
        for offender in sorted(set(offenders)):
            print("  " + offender)
        sys.exit(1)
    print("no reflexive facts among %d" % total)


main(sys.argv[1])
