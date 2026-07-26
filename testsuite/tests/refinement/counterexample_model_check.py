"""Check that a refuted obligation reports the assignment that refutes it.

A refuted obligation is one no state satisfies, and the solver already found
the refuting assignment while failing to prove it.  Before it was asked for,
every refuted obligation reached the editor with a null counterexample.
"""

import json
import sys


def conditions(path):
    with open(path) as handle:
        document = json.load(handle)
    return document["verification_conditions"]


def discharge(condition):
    return condition.get("discharge") or {}


def main():
    z3_refuted, z3_proved, oxsmt_refuted = sys.argv[1:4]

    refuted = conditions(z3_refuted)
    assert len(refuted) == 1, len(refuted)
    result = discharge(refuted[0])
    assert result.get("status") == "disproved", result.get("status")
    reported = result.get("counterexample")
    assert reported is not None, "refuted obligation reported no counterexample"
    assert reported.startswith("counterexample:\n"), reported[:40]
    assert "define-fun" in reported, reported
    # The named fact selectors belong to the unsat core, not to the
    # assignment, and must not appear among the reported values.
    assert "define-fun h_" not in reported, reported

    proved = conditions(z3_proved)
    assert len(proved) == 1, len(proved)
    result = discharge(proved[0])
    assert result.get("status") == "proved", result.get("status")
    assert result.get("counterexample") is None, result.get("counterexample")

    # The in-process backend is not asked for an assignment, and says so by
    # reporting none rather than by inventing one.
    oxsmt = conditions(oxsmt_refuted)
    assert len(oxsmt) == 1, len(oxsmt)
    result = discharge(oxsmt[0])
    assert result.get("status") == "disproved", result.get("status")
    assert result.get("counterexample") is None, result.get("counterexample")

    print("counterexample reporting: as expected")


main()
