import re
import sys


mode, filename = sys.argv[1:]
with open(filename, encoding="utf-8") as channel:
    text = channel.read()


ERASED = {
    "erased_direct_site": "erase_direct",
    "erased_generated_site": "identity_nat_def",
    "erased_pipe_site": "erase_direct",
    "erased_revapply_site": "erase_direct",
    "erased_expensive_site": "erase_direct",
}

RETAINED = {
    "retained_tailcall_site": "erase_direct",
    "retained_inlined_site": "erase_direct",
    "retained_unrolled_site": "erase_direct",
    "retained_specialised_site": "erase_direct",
    "retained_zero_alloc_site": "erase_direct",
    "retained_nested_attribute_argument_site": "erase_direct",
    "retained_shadowed_unit_site": "keep_shadowed_unit",
    "retained_effectful_body_site": "keep_effectful",
    "retained_unproved_recursion_site": "keep_unproved_recursion",
    "retained_recursive_rollback_site": "rollback_safe",
    "retained_effectful_argument_site": "erase_direct",
    "retained_pipe_argument_site": "erase_direct",
    "retained_revapply_argument_site": "erase_direct",
    "retained_alias_site": "keep_alias",
    "retained_higher_order_site": "keep_higher_order",
    "retained_partial_site": "keep_partial",
    "retained_bare_site": "keep_bare",
    "retained_nonunit_site": "keep_nonunit",
    "keep_assert_via_return_parameter": "return_parameter",
    "retained_assert_site": "keep_assert",
    "retained_partial_match_site": "keep_partial_match",
    "retained_transitive_site": "keep_transitive",
    "retained_trapping_argument_site": "erase_direct",
}


def lambda_tokens(source):
    pattern = r'\(\*.*?\*\)|"(?:\\.|[^"\\])*"|[()]|[^\s()]+'
    return [
        token
        for token in re.findall(pattern, source, flags=re.DOTALL)
        if not token.startswith("(*")
    ]


def lambda_tree(source):
    root = []
    stack = [root]
    for token in lambda_tokens(source):
        if token == "(":
            child = []
            stack[-1].append(child)
            stack.append(child)
        elif token == ")":
            if len(stack) == 1:
                raise AssertionError("unbalanced Lambda close parenthesis")
            stack.pop()
        else:
            stack[-1].append(token)
    if len(stack) != 1:
        raise AssertionError("unbalanced Lambda open parenthesis")
    return root


def base_name(atom):
    return atom.split("/", 1)[0] if isinstance(atom, str) else None


def binding_rhs(node, wanted):
    if isinstance(node, list):
        for index in range(len(node) - 2):
            equals = node[index + 1]
            if (
                base_name(node[index]) == wanted
                and isinstance(equals, str)
                and equals.startswith("=")
            ):
                return node[index + 2]
        for child in node:
            result = binding_rhs(child, wanted)
            if result is not None:
                return result
    return None


def apply_heads(node):
    result = []
    if isinstance(node, list):
        if node and node[0] == "apply" and len(node) > 1:
            head = base_name(node[1])
            if head is not None:
                result.append(head)
        for child in node:
            result.extend(apply_heads(child))
    return result


def check_lambda(verified):
    if "Warning 53 [misplaced-attribute]" in text:
        raise AssertionError("retained application attribute was left unused")
    tree = lambda_tree(text)
    expected = (
        RETAINED
        if verified
        else dict(list(ERASED.items()) + list(RETAINED.items()))
    )
    absent = ERASED if verified else {}
    for binding, head in expected.items():
        rhs = binding_rhs(tree, binding)
        if rhs is None:
            raise AssertionError(f"Lambda binding {binding!r} was not found")
        heads = apply_heads(rhs)
        if head not in heads:
            raise AssertionError(
                f"{binding}: expected application of {head}, got {heads!r}"
            )
    for binding, head in absent.items():
        rhs = binding_rhs(tree, binding)
        if rhs is None:
            raise AssertionError(f"Lambda binding {binding!r} was not found")
        heads = apply_heads(rhs)
        if head in heads:
            raise AssertionError(f"{binding}: erased call {head} was retained")
    expensive = binding_rhs(tree, "erased_expensive_site")
    heads = apply_heads(expensive)
    if verified and "pure_expensive" in heads:
        raise AssertionError("erasure retained expensive argument evaluation")
    if not verified and "pure_expensive" not in heads:
        raise AssertionError("no-verify lost expensive argument evaluation")


def indentation_tree(source):
    root = (-1, "root", [])
    stack = [root]
    for raw_line in source.splitlines():
        if not raw_line.strip():
            continue
        indent = len(raw_line) - len(raw_line.lstrip(" "))
        node = (indent, raw_line.strip(), [])
        while stack[-1][0] >= indent:
            stack.pop()
        stack[-1][2].append(node)
        stack.append(node)
    return root


def descendants(node):
    yield node
    for child in node[2]:
        yield from descendants(child)


def typedtree_bindings(source):
    result = {}
    for node in descendants(indentation_tree(source)):
        if node[1] not in {"<def>", "<def_rec>"}:
            continue
        names = []
        decisions = []
        for child in descendants(node):
            match = re.search(r'Tpat_var "([^"/]+)', child[1])
            if match:
                names.append(match.group(1))
            match = re.search(r"proof_erased=(true|false)", child[1])
            if match:
                decisions.append(match.group(1) == "true")
        if names:
            result.setdefault(names[0], []).extend(decisions)
    return result


if mode == "lambda-verified":
    check_lambda(True)
elif mode == "lambda-noverify":
    check_lambda(False)
elif mode == "typedtree":
    bindings = typedtree_bindings(text)
    for binding in ERASED:
        if bindings.get(binding) != [True]:
            raise AssertionError(
                f"{binding}: expected one proof_erased=true, "
                f"got {bindings.get(binding)!r}"
            )
    for binding in RETAINED:
        if True in bindings.get(binding, []):
            raise AssertionError(f"{binding}: unexpectedly marked proof-erased")
        if binding != "retained_bare_site" and not bindings.get(binding):
            raise AssertionError(f"{binding}: refinement application not found")
elif mode == "typedtree-quotation":
    if "Texp_quotation" not in text:
        raise AssertionError("quotation fixture did not produce Texp_quotation")
    bindings = typedtree_bindings(text)
    decisions = bindings.get("retained_quotation_site")
    if not decisions:
        raise AssertionError("quoted refinement application was not found")
    if True in decisions:
        raise AssertionError("proof call inside quotation was marked erased")
else:
    raise AssertionError(f"unknown check mode: {mode!r}")
