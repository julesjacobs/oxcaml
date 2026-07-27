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
    # The seven `law` calls and the two calls to `needed`, and only those: the
    # `effectful_law` call on line 45 hands back a refined unit too, but it is
    # not evidence-only, so it is not a call anyone could be told to drop, and
    # the `outer` calls on lines 67 and 77 are evidence-only but each span
    # contains an argument that did work, so neither is one either.
    expected = [
        (31, 11),
        (35, 11),
        (36, 11),
        (40, 21),
        (40, 35),
        (56, 11),
        (67, 17),
        (76, 11),
        (77, 17),
    ]
    if sites != expected:
        raise AssertionError("lemma_calls sites %s != %s" % (sites, expected))
    # Said again on its own, because the list above fails for any difference
    # and this is the difference that matters: the text of the `outer` call
    # contains the call to `needed` at 67:17, whose proposition is the only
    # reason the goal on line 68 holds.  Recording the outer call invites a
    # reader to delete both.
    for outer_site, inner_site in (((67, 11), (67, 17)), ((77, 11), (77, 17))):
        if outer_site in sites:
            raise AssertionError(
                "the call at %s was recorded; its span contains the call at %s"
                % (outer_site, inner_site)
            )
    for call in calls:
        if call["name"] not in ("law", "needed"):
            raise AssertionError("unexpected callee %r" % (call["name"],))
        if call["introduced"] is not True:
            raise AssertionError(
                "call at %s introduced nothing" % (site(call["span"]),)
            )

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
                producing.setdefault(fact["display"], set()).add(site(producer["span"]))

    # The repeated pair on lines 35-36 states one proposition; both sites are
    # named on the single entry the environment kept.
    if producing.get("b + 0 = b") != {(35, 11), (36, 11)}:
        raise AssertionError("repeated pair not both recorded: %r" % (producing,))
    # Both arms of the branch on line 40 are named on the merged entry.
    if producing.get("d + 0 = d") != {(40, 21), (40, 35)}:
        raise AssertionError("branch arms not both recorded: %r" % (producing,))

    # Usage, in both directions.  Checking only that the unread facts read as
    # unread cannot fail if the reading stops being a reading: a mechanism
    # returning "unread" for everything satisfies it, and that mechanism is
    # the one that would fade every lemma call in every buffer.  So a fact a
    # goal genuinely needs is checked too.
    accounted = 0
    needed_facts = 0
    for condition in document["verification_conditions"]:
        for fact in condition["facts"]:
            if "used" not in fact:
                continue
            accounted += 1
            name = fact["origin"].get("name")
            # None of the `law` facts is read: every goal that sees one holds
            # on the binder alone.
            if name == "law" and fact["used"]:
                raise AssertionError("law fact reported as read: %r" % (fact,))
            # Every `needed` fact is read: the goals on lines 57 and 68 hold
            # for no other reason.
            if name == "needed":
                needed_facts += 1
                if not fact["used"]:
                    raise AssertionError("needed fact reported as unread: %r" % (fact,))

    # A backend that reports no accounting leaves the two checks above with
    # nothing to look at.  That is not a pass: it is this test asserting
    # nothing about the reading it exists to pin, while reporting success.
    if accounted == 0:
        raise AssertionError(
            "no fact in the dump carries a `used` key, so nothing here checked "
            "what a proof read; the backend reported no fact accounting"
        )
    if needed_facts == 0:
        raise AssertionError("no fact from `needed` was accounted for at all")

    print("lemma-call channel checks passed")


main()
