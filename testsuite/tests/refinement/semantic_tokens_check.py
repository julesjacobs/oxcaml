import json
import sys


def main():
    dump_path, source_path = sys.argv[1], sys.argv[2]
    with open(dump_path, encoding="utf-8") as channel:
        document = json.load(channel)
    with open(source_path, encoding="utf-8") as channel:
        lines = channel.read().split("\n")

    tokens = document.get("semantic_tokens")
    if not tokens:
        raise AssertionError("dump has no semantic_tokens")

    def fragment(token):
        start = token["location"]["start"]
        end = token["location"]["end"]
        if start["line"] != end["line"]:
            return "<multiline>"
        return lines[start["line"] - 1][start["column"] : end["column"]]

    seen = []
    for token in tokens:
        seen.append((fragment(token), token["role"], token["classification"]))

    def occurrences(fragment_text, role):
        return [
            classification
            for text, seen_role, classification in seen
            if text == fragment_text and seen_role == role
        ]

    # Lemma call heads (direct, and as the head of a partial application)
    # are proof calls, as are calls through a total alias or a total
    # partial-application binding; the argument-position imperative call
    # stays ordinary.
    expect(occurrences("Facts.lemma", "call-head"), ["proof-call"])
    expect(occurrences("local_lemma", "call-head"), ["proof-call"])
    expect(occurrences("alias", "call-head"), ["proof-call"])
    expect(occurrences("partial", "call-head"), ["proof-call"])
    expect(occurrences("imperative_fn", "call-head"), ["ordinary"])

    # A statement-position mention of an exported refined value is a proof
    # call; the alias-creating mention of the lemma is a proof use.
    expect(occurrences("Facts.evidence", "statement"), ["proof-call"])
    expect(occurrences("Facts.lemma", "use"), ["proof-use"])

    # Ordinary imperative and arithmetic heads stay ordinary, including the
    # same-spelling imperative shadow of the lemma.
    expect(occurrences("print_int", "call-head"), ["ordinary"] * 2)
    expect(occurrences("ignore", "call-head"), ["ordinary"])
    expect(occurrences("+", "call-head"), ["ordinary"])
    expect(occurrences("lemma", "call-head"), ["ordinary"])

    # Data-variable uses never classify as proof uses here.
    for text, role, classification in seen:
        if text in ("x", "y") and role == "use":
            if classification != "ordinary":
                raise AssertionError(
                    f"variable {text!r} classified {classification!r}"
                )

    # Every identifier_modes companion entry matches a non-ordinary token.
    modes = document.get("identifier_modes")
    if not modes:
        raise AssertionError("dump has no identifier_modes")
    token_spans = {
        json.dumps(token["location"], sort_keys=True): token["classification"]
        for token in tokens
    }
    for entry in modes:
        span = json.dumps(entry["location"], sort_keys=True)
        if token_spans.get(span, "ordinary") == "ordinary":
            raise AssertionError("identifier_modes entry for ordinary token")
        if not entry["mode"].startswith("@ "):
            raise AssertionError(f"malformed mode {entry['mode']!r}")
    non_ordinary = sum(
        1 for token in tokens if token["classification"] != "ordinary"
    )
    if len(modes) != non_ordinary:
        raise AssertionError(
            f"{len(modes)} identifier_modes for {non_ordinary} tokens"
        )

    print("semantic tokens: all checks passed")


def expect(actual, expected):
    if actual != expected:
        raise AssertionError(f"expected {expected!r}, got {actual!r}")


if __name__ == "__main__":
    main()
