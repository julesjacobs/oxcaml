import json
import re
import sys


def read_vcs(path):
    with open(path, encoding="utf-8") as channel:
        return json.load(channel)["verification_conditions"]


def check_acceptance():
    cases = [
        ("regular-default", "proved", False),
        ("nonregular-default", "solver-error", True),
        ("regular-z3", "proved", False),
        ("nonregular-z3", "solver-error", True),
        ("regular-oxsmt", "proved", False),
        ("nonregular-oxsmt", "solver-error", True),
    ]
    for name, expected_status, expect_emission_error in cases:
        off = read_vcs(f"{name}-off.json")
        on = read_vcs(f"{name}-on.json")
        if len(off) != 1 or len(on) != 1:
            raise AssertionError(
                f"{name}: expected one VC with each flag setting"
            )
        if "generated_smt" in off[0]:
            raise AssertionError(f"{name}: flag-off VC has generated_smt")
        if off[0]["discharge"] != on[0]["discharge"]:
            raise AssertionError(f"{name}: discharge changed with dump flag")
        status = off[0]["discharge"]["status"]
        if status != expected_status:
            raise AssertionError(
                f"{name}: expected {expected_status!r}, got {status!r}"
            )
        generated = on[0]["generated_smt"]
        has_emission_error = generated["emission_error"] is not None
        if has_emission_error != expect_emission_error:
            raise AssertionError(f"{name}: unexpected SMT emission result")
        if expect_emission_error:
            if generated["prove"] is not None:
                raise AssertionError(f"{name}: failed emission has prove query")
            message = generated["emission_error"]["message"]
            if message != on[0]["discharge"]["detail"]:
                raise AssertionError(
                    f"{name}: dump does not record the ordinary emission error"
                )
        elif generated["prove"] is None:
            raise AssertionError(f"{name}: bounded dump query is missing")


if sys.argv[1:] == ["acceptance"]:
    check_acceptance()
    sys.exit(0)


with open(sys.argv[1], encoding="utf-8") as channel:
    document = json.load(channel)

vcs = document["verification_conditions"]
if len(vcs) != 3:
    raise AssertionError(f"expected 3 VCs, got {len(vcs)}")

for vc in vcs:
    generated = vc["generated_smt"]
    if generated["emission_error"] is not None:
        raise AssertionError(generated["emission_error"])
    prove = generated["prove"]
    facts = generated["facts"]
    if len(facts) != len(vc["facts"]):
        raise AssertionError("SMT fact count does not match JSON fact count")
    expected_selectors = []
    for index, fact in enumerate(facts):
        selector = f"h_{index}"
        expected_selectors.append(selector)
        if fact["fact_index"] != index or fact["selector"] != selector:
            raise AssertionError("SMT selector is not aligned with fact index")
        assertion = f"(assert (! {fact['term']} :named {selector}))\n"
        if prove.count(assertion) != 1:
            raise AssertionError(
                "structured SMT fact does not match prove query"
            )
    actual_selectors = re.findall(
        r":named (h_[0-9]+)\)\)\n", prove
    )
    if actual_selectors != expected_selectors:
        raise AssertionError("prove-query selectors are not in JSON fact order")
    goal_assertion = f"(assert {generated['goal']})\n"
    if prove.count(goal_assertion) != 1:
        raise AssertionError("structured SMT goal does not match prove query")

expected_terms_by_fact_text = {
    "3 >= 3": "(= (>= 3 3) true)",
    "annotation >= 3": "(= (>= v_0 3) true)",
    "annotation = 3": "(= (= v_0 3) true)",
    "1 > 0": "(= (> 1 0) true)",
    "y > 0": "(= (> v_1 0) true)",
}
for vc in vcs:
    for source_fact, smt_fact in zip(
        vc["facts"], vc["generated_smt"]["facts"]
    ):
        expected = expected_terms_by_fact_text[source_fact["display"]]
        if smt_fact["term"] != expected:
            raise AssertionError(
                f"misaligned SMT term for {source_fact['display']!r}: "
                f"expected {expected!r}, got {smt_fact['term']!r}"
            )
