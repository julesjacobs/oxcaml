import json
import os
import sys


mode, dump_name, source_name = sys.argv[1:]

with open(dump_name, encoding="utf-8") as input_file:
    document = json.load(input_file)
with open(source_name, encoding="utf-8") as input_file:
    source_lines = input_file.read().splitlines()


def points(span):
    assert isinstance(span, dict), span
    assert span.get("ghost") is False, span
    assert os.path.basename(span["file"]) == os.path.basename(source_name), span
    start = span["start"]
    end = span["end"]
    result = (
        int(start["line"]),
        int(start["column"]),
        int(end["line"]),
        int(end["column"]),
    )
    assert result[0] >= 1 and result[2] >= result[0], span
    return result


def source_slice(span):
    start_line, start_column, end_line, end_column = points(span)
    encoded = [
        source_lines[line].encode("utf-8")
        for line in range(start_line - 1, end_line)
    ]
    if start_line == end_line:
        piece = encoded[0][start_column:end_column]
    else:
        piece = b"\n".join(
            [encoded[0][start_column:]]
            + encoded[1:-1]
            + [encoded[-1][:end_column]]
        )
    return piece.decode("utf-8")


def contains(outer, inner):
    outer_start = points(outer)[:2]
    outer_end = points(outer)[2:]
    inner_start = points(inner)[:2]
    inner_end = points(inner)[2:]
    return outer_start <= inner_start and inner_end <= outer_end


conditions = document.get("verification_conditions")
assert isinstance(conditions, list), document

if mode == "rebound-effect-summary":
    assert len(conditions) == 1, conditions
    condition = conditions[0]
    assert condition["kind"] == "contract-argument", condition
    assert source_slice(condition["result_span"]) == "value", condition
    assert points(condition["location"]) == points(condition["result_span"]), condition
    provenance = condition["provenance"]
    assert provenance["kind"] == "contract-argument", provenance
    related = {
        item["role"]: item["span"] for item in provenance["related_spans"]
    }
    assert points(related["argument"]) == points(condition["result_span"]), provenance
    assert points(related["application"]) == points(condition["program_point"]), provenance
    assert condition["discharge"]["status"] == "not-proved", condition
    sys.exit(0)

annotations = [condition for condition in conditions if condition["kind"] == "annotation"]
assert len(annotations) == len(conditions), conditions

observed = {}
for condition in annotations:
    location = condition["location"]
    program_point = condition["program_point"]
    result_span = condition["result_span"]
    assert points(location) == points(program_point) == points(result_span), condition
    text = source_slice(result_span)
    assert text and not text.startswith("raise"), condition

    provenance = condition["provenance"]
    assert provenance["kind"] == "annotation", provenance
    assert contains(provenance["source_span"], result_span), provenance
    subjects = [
        related["span"]
        for related in provenance["related_spans"]
        if related.get("role") == "subject"
    ]
    assert len(subjects) == 1, provenance
    assert points(subjects[0]) == points(result_span), provenance

    status = condition["discharge"]["status"]
    assert text not in observed, (text, observed)
    observed[text] = status

if mode == "positive":
    expected = {
        "body_result": "proved",
        "not_found_result": "proved",
        "exit_result": "proved",
        "normal_result": "proved",
        "fallback_result": "proved",
    }
elif mode == "negative":
    expected = {
        "body_result": "proved",
        "handler_result": "not-proved",
    }
elif mode == "effect-nonresume":
    expected = {"1": "proved"}
elif mode == "effect-resume":
    expected = {"perform Pick": "not-proved"}
elif mode == "unmatched-gate":
    expected = {"0": "disproved"}
elif mode == "unmatched-leaf":
    expected = {"perform A": "not-proved"}
elif mode == "unmatched-summary":
    expected = {"result": "not-proved"}
elif mode == "dynamic":
    expected = {"perform operation": "not-proved"}
elif mode == "nested":
    expected = {
        "inner_try_handler": "proved",
        "outer_try_handler": "proved",
        "match_value_result": "proved",
        "inner_match_handler": "proved",
        "outer_match_handler": "proved",
    }
elif mode == "nested-same-effect":
    expected = {"resumed_body_result": "disproved"}
elif mode == "nested-guarded-effect":
    expected = {"guarded_resumed_body_result": "disproved"}
elif mode == "nested-refutable-effect":
    expected = {"refutable_resumed_body_result": "disproved"}
elif mode == "nested-rebound-effect":
    expected = {"rebound_resumed_body_result": "disproved"}
elif mode == "nested-alias-effect":
    expected = {"alias_resumed_body_result": "disproved"}
elif mode == "shadowed-module":
    expected = {"Stdlib.Effect.perform A": "not-proved"}
elif mode == "shadowed-local":
    expected = {"Stdlib.Effect.perform B": "not-proved"}
elif mode == "shadowed-functor":
    expected = {"Stdlib.Effect.perform C": "not-proved"}
elif mode == "shadowed-match":
    expected = {"result": "not-proved"}
elif mode == "shadowed-continue":
    expected = {"result": "not-proved"}
elif mode == "conservative-continue":
    expected = {"result": "not-proved"}
elif mode == "false":
    expected = {"0": "disproved"}
else:
    raise AssertionError(mode)

assert observed == expected, (observed, expected)
