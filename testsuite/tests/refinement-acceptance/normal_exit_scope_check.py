import json
import sys


with open(sys.argv[1], encoding="utf-8") as channel:
    vcs = json.load(channel)["verification_conditions"]

if len(vcs) != 1:
    raise AssertionError(f"expected one VC, got {len(vcs)}")

vc = vcs[0]
if vc["goal"]["display"] != "result = 7":
    raise AssertionError(f"unexpected goal: {vc['goal']['display']!r}")

facts = [fact["display"] for fact in vc["facts"]]
if "result = (let scope_leak_sentinel = 7 in scope_leak_sentinel)" not in facts:
    raise AssertionError(f"missing stable result summary: {facts!r}")
if "scope_leak_sentinel = 7" in facts:
    raise AssertionError(f"inner let binder escaped its scope: {facts!r}")
