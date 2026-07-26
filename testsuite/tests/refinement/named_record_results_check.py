import json


def conditions(path):
    with open(path, encoding="utf-8") as channel:
        return json.load(channel)["verification_conditions"]


positive = conditions("positive.json")
if len(positive) != 9:
    raise AssertionError(f"expected 9 positive VCs, got {len(positive)}")
if any(vc["discharge"]["status"] != "proved" for vc in positive):
    raise AssertionError("every positive record-result VC must be proved")
if any(vc["emission_error"] is not None for vc in positive):
    raise AssertionError("positive record-result VC failed during emission")

goals = [vc["goal"]["display"] for vc in positive]
required_fragments = [
    "(mk (n + 0, n + 1)).x = n",
    "(mk (n, call_result)).x = n",
    "(mk (n + 0, base.y)).y = base.y",
    "(mk (first + 0, second + 0, base.kept)).kept = base.kept",
    "(mk (0 + 0, call_result)).value = 0",
    "(mk (payload, tag + 0)).tag = tag",
]
for fragment in required_fragments:
    if not any(fragment in goal for goal in goals):
        raise AssertionError(f"missing record-result goal fragment: {fragment}")

ordered = "(mk (n + 0, n + 1)).x = n"
if sum(ordered in goal for goal in goals) != 2:
    raise AssertionError("source field order changed record declaration order")

seals = [vc for vc in positive if vc["kind"] == "seal-implication"]
if len(seals) != 1 or seals[0]["goal"]["display"] != "value = witness":
    raise AssertionError("abstract record result did not produce its seal VC")

negative = conditions("negative.json")
if len(negative) != 1:
    raise AssertionError(f"expected one false-field VC, got {len(negative)}")
if negative[0]["discharge"]["status"] != "disproved":
    raise AssertionError("the false record-field obligation must be disproved")
if "(mk (n, 0)).x = n + 1" not in negative[0]["goal"]["display"]:
    raise AssertionError("the false-field VC lost the concrete record subject")

uninhabited = conditions("uninhabited.json")
if len(uninhabited) != 1:
    raise AssertionError(
        f"expected one uninhabited-result VC, got {len(uninhabited)}"
    )
if uninhabited[0]["discharge"]["status"] != "solver-error":
    raise AssertionError(
        "an uninhabited abstract record field must fail closed"
    )
detail = uninhabited[0]["discharge"]["detail"] or ""
if "is not known to be inhabited" not in detail:
    raise AssertionError("missing abstract-inhabitance failure detail")

distinct = conditions("distinct.json")
if len(distinct) != 1:
    raise AssertionError("expected one distinct-call VC")
if distinct[0]["discharge"]["status"] != "not-proved":
    raise AssertionError("two unstable record fields must remain distinct")
call_results = [
    variable["model_name"]
    for variable in distinct[0]["witness_relevance"]["goal_variables"]
    if variable["name"] == "call_result"
]
if len(call_results) != 2 or len(set(call_results)) != 2:
    raise AssertionError("two unstable calls were assigned one logical subject")

variant = conditions("variant.json")
if [vc["discharge"]["status"] for vc in variant] != ["proved", "disproved"]:
    raise AssertionError("variant constructor index regression")
if [vc["goal"]["display"] for vc in variant] != [
    "Second value = Second value",
    "Second value = First value",
]:
    raise AssertionError("variant constructor subjects changed")

smt = conditions("smt.json")
if len(smt) != 9:
    raise AssertionError(f"expected 9 emitted SMT VCs, got {len(smt)}")
for vc in smt:
    generated = vc.get("generated_smt")
    if generated is None or generated["emission_error"] is not None:
        raise AssertionError("named-record construction failed SMT emission")

z3_positive = conditions("z3-positive.json")
if len(z3_positive) != 9:
    raise AssertionError(f"expected 9 positive Z3 VCs, got {len(z3_positive)}")
if any(vc["discharge"]["status"] != "proved" for vc in z3_positive):
    raise AssertionError("every positive named-record Z3 VC must be proved")

z3_negative = conditions("z3-negative.json")
if len(z3_negative) != 1:
    raise AssertionError("expected one false-field Z3 VC")
if z3_negative[0]["discharge"]["status"] != "disproved":
    raise AssertionError("Z3 must disprove the false record field")

z3_uninhabited = conditions("z3-uninhabited.json")
if len(z3_uninhabited) != 1:
    raise AssertionError("expected one uninhabited Z3 VC")
if z3_uninhabited[0]["discharge"]["status"] != "solver-error":
    raise AssertionError("Z3 must fail closed on abstract inhabitance")

z3_variant = conditions("z3-variant.json")
if [vc["discharge"]["status"] for vc in z3_variant] != [
    "proved",
    "disproved",
]:
    raise AssertionError("Z3 variant constructor index regression")

# Neither the positive nor the false-field source is compiled under oxsmt: the
# record beside the bitvector arithmetic costs that backend its bitvector path,
# so it discharges one of the nine obligations z3 discharges and answers
# inconclusively on the false field rather than refuting it.  Filed upstream as
# report 08, second and third cases; restore these blocks with the arms when a
# fixed revision lands.  The acceptance backend checks both sources above.

oxsmt_uninhabited = conditions("oxsmt-uninhabited.json")
if len(oxsmt_uninhabited) != 1:
    raise AssertionError("expected one uninhabited oxsmt VC")
if oxsmt_uninhabited[0]["discharge"]["status"] != "solver-error":
    raise AssertionError("oxsmt must fail closed on abstract inhabitance")

oxsmt_variant = conditions("oxsmt-variant.json")
if [vc["discharge"]["status"] for vc in oxsmt_variant] != [
    "proved",
    "disproved",
]:
    raise AssertionError("oxsmt variant constructor index regression")
