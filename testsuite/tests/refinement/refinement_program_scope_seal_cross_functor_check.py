import json
import sys


with open(sys.argv[1], encoding="utf-8") as channel:
    vcs = json.load(channel)["verification_conditions"]

seals = [vc for vc in vcs if vc["kind"] == "seal-implication"]
if len(seals) != 1:
    raise AssertionError(f"expected one seal VC, got {len(seals)}")

seal = seals[0]
if seal["discharge"]["status"] == "proved":
    raise AssertionError("distinct functor instances were identified")
if seal["goal"]["display"] != "value = A.f":
    raise AssertionError(f"unexpected seal goal: {seal['goal']['display']}")

facts = [fact["display"] for fact in seal["facts"]]
if facts != ["value = B.f"]:
    raise AssertionError(f"implementation identity was not preserved: {facts}")
if seal["goal"]["text"] == seal["facts"][0]["text"]:
    raise AssertionError("distinct functor-instance paths collapsed before discharge")
