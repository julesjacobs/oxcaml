import json
import sys


def main():
    with open(sys.argv[1], encoding="utf-8") as channel:
        document = json.load(channel)

    calls = document.get("lemma_calls")
    if not calls:
        raise AssertionError("dump has no lemma_calls")

    def site(span):
        return (span["start"]["line"], span["start"]["column"])

    sites = sorted(site(call["span"]) for call in calls)
    # The five `law` calls, and only those: the `effectful_law` call on line
    # 45 hands back a refined unit too, but it is not evidence-only, so it is
    # not a call anyone could be told to drop.
    expected = [(31, 11), (35, 11), (36, 11), (40, 21), (40, 35)]
    if sites != expected:
        raise AssertionError("lemma_calls sites %s != %s" % (sites, expected))
    for call in calls:
        if call["name"] != "law":
            raise AssertionError("unexpected callee %r" % (call["name"],))
        if call["introduced"] is not True:
            raise AssertionError("call at %s introduced nothing" % (site(call["span"]),))

    # Every fact names every site that introduced it.
    producing = {}
    for condition in document["verification_conditions"]:
        for fact in condition["facts"]:
            producers = fact.get("producers")
            if producers is None:
                raise AssertionError("fact has no producers: %r" % (fact,))
            if not producers:
                raise AssertionError("fact has an empty producer set: %r" % (fact,))
            # The displayed origin is always among them.
            origins = [(p["kind"], p.get("name"), p["span"]) for p in producers]
            origin = (
                fact["origin"]["kind"],
                fact["origin"].get("name"),
                fact["origin"]["span"],
            )
            if origin not in origins:
                raise AssertionError("origin missing from producers: %r" % (fact,))
            for producer in producers:
                if producer["kind"] != "application" or producer.get("name") != "law":
                    continue
                producing.setdefault(
                    fact["display"], set()
                ).add(site(producer["span"]))

    # The repeated pair on lines 35-36 states one proposition; both sites are
    # named on the single entry the environment kept.
    if producing.get("b + 0 = b") != {(35, 11), (36, 11)}:
        raise AssertionError("repeated pair not both recorded: %r" % (producing,))
    # Both arms of the branch on line 40 are named on the merged entry.
    if producing.get("d + 0 = d") != {(40, 21), (40, 35)}:
        raise AssertionError("branch arms not both recorded: %r" % (producing,))

    # Where the backend reported which facts it read, none of these was read:
    # every goal here follows from the binder alone.  A backend that reports
    # no accounting leaves nothing to check, and must not be read as one that
    # reported everything unread.
    for condition in document["verification_conditions"]:
        for fact in condition["facts"]:
            if "used" not in fact:
                continue
            if fact["origin"].get("name") != "law":
                continue
            if fact["used"]:
                raise AssertionError("law fact reported as read: %r" % (fact,))

    print("lemma-call channel checks passed")


main()
