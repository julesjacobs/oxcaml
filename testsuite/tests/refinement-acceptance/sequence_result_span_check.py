import json
import sys


with open(sys.argv[1], encoding="utf-8") as channel:
    vcs = json.load(channel)["verification_conditions"]

if len(vcs) != 4:
    raise AssertionError(f"expected four VCs, got {len(vcs)}")


def span_text(span):
    with open(span["file"], encoding="utf-8") as source:
        lines = source.readlines()
    start = span["start"]
    end = span["end"]
    if start["line"] != end["line"]:
        raise AssertionError(f"expected a single-line leaf: {span!r}")
    return lines[start["line"] - 1][start["column"] : end["column"]]


application = []
for vc in vcs:
    result = vc["result_span"]
    location = vc["location"]
    result_text = span_text(result)
    if result_text == "Fun.id":
        application.append(vc)
        if location == result:
            raise AssertionError(
                f"application result span was not compacted: {location!r}"
            )
    elif location != result:
        raise AssertionError(
            f"diagnostic location and result span disagree: {location!r}, {result!r}"
        )
    elif result_text != "()":
        raise AssertionError(f"result span is not the returned leaf: {result!r}")

if len(application) != 1:
    raise AssertionError(
        f"expected one compact application result, got {len(application)}"
    )

annotation = [vc for vc in vcs if vc["kind"] == "annotation"]
contracts = [vc for vc in vcs if vc["kind"] == "contract-argument"]
if len(annotation) != 2 or len(contracts) != 2:
    raise AssertionError(
        f"unexpected VC kinds: {[vc['kind'] for vc in vcs]!r}"
    )

source = annotation[0]["provenance"]["source_span"]
result = annotation[0]["result_span"]
if source["start"] == result["start"] or source["end"] != result["end"]:
    raise AssertionError(
        f"body provenance did not retain the full wrapper: {source!r}, {result!r}"
    )

for vc in contracts:
    related = {
        item["role"]: item["span"]
        for item in vc["provenance"]["related_spans"]
    }
    argument = related.get("argument")
    if argument is None:
        raise AssertionError(f"contract provenance lost the argument: {related!r}")
    if argument["start"] == vc["result_span"]["start"]:
        raise AssertionError(
            f"argument provenance unexpectedly collapsed: {argument!r}"
        )
