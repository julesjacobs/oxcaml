import json
import sys


with open(sys.argv[1], encoding="utf-8") as channel:
    vcs = json.load(channel)["verification_conditions"]

contracts = [vc for vc in vcs if vc["kind"] == "contract-argument"]
if len(contracts) != 6:
    raise AssertionError(f"expected six contract VCs, got {len(contracts)}")

anchor_models = {}
anchor_identities = {}
for vc in contracts:
    if vc["discharge"]["status"] != "proved":
        raise AssertionError("inferred CMI contract was not proved")
    goal = vc["goal"]["text"]
    facts = [fact["text"] for fact in vc["facts"]]
    if goal not in facts:
        raise AssertionError("contract fact and goal use different identities")
    anchors = {
        token
        for token in goal.replace("(", " ").replace(")", " ").split()
        if token.startswith("global[") and token.endswith(".anchor]")
    }
    if len(anchors) != 1:
        raise AssertionError(f"expected one exact anchor identity, got {anchors}")
    anchor_identities[vc["goal"]["display"]] = next(iter(anchors))
    for variable in vc["witness_relevance"]["goal_variables"]:
        if variable["name"] == "anchor":
            anchor_models.setdefault(goal, set()).add(variable["model_name"])

if any(len(models) != 1 for models in anchor_models.values()):
    raise AssertionError(
        f"a contract uses more than one anchor identity: {anchor_models}"
    )

provider = "Refinement_program_scope_inferred_provider"
expected_prefixes = {
    f"{provider}.witness = {provider}.anchor": f"global[{provider}!.",
    "First.witness = First.anchor": "global[First/",
    "Second.witness = Second.anchor": "global[Second/",
    "First.Nested.outer_witness = First.anchor": "global[First/",
    "Second.Nested.outer_witness = Second.anchor": "global[Second/",
}
for display, prefix in expected_prefixes.items():
    identity = anchor_identities.get(display)
    if identity is None or not identity.startswith(prefix):
        raise AssertionError(f"missing qualified identity for {display}")

if anchor_identities["First.witness = First.anchor"] == anchor_identities[
    "Second.witness = Second.anchor"
]:
    raise AssertionError("functor instances share one anchor identity")
