# Vox demo catalogue

A static site with one page per verified demo: what is proved, what the demo
trusts, what it does not claim, a checked client, a rejected program, the
interface and how to reproduce the checks. Pages quote the repository at one
commit, so the site describes exactly that commit.

## Build

After `make install` (the line counts parse sources with the installed
compiler-libs):

```sh
python3 verification/catalogue/build.py
python3 -m http.server -d _build/catalogue 8766
```

`build.py` reads every quoted file with `git show` at `--revision` (default
`HEAD`), counts lines, writes the site to `_build/catalogue` and fails on any
broken link or anchor. Links to GitHub point at that commit, so build from a
pushed commit when the site is shared. For a draft, `--working-tree` reads
the checkout instead, and `--pages flat-hash-table,_trust` builds only those
pages.

## Files

- `pages/<demo>.md`: one page per demo. The format and its directives
  (`@code`, `@text`, `@performance`) are described at the top of `pages.py`.
- `pages/_trust.md`: what every demo trusts. Each demo page lists only what
  it trusts beyond it.
- `catalogue.json`: the order of demos, the language mechanisms, and for the
  line counts each demo's root modules (`census`) and the few functions with
  a refined `unit` result that run at run time (`runtime_units`).
- `line_stats.py`, `source_inventory.ml`: the line counts; the convention is
  stated on the generated line-count pages.
- `compiler-example/`: the two WebAssembly modules the HM-to-Wasm compiler
  emits for its design example, run in the browser on the presentation page.
  `original_run_smoke.ml` produces them. `run-examples.sh` builds
  `testsuite/tests/vox/hmc_compilation_examples.ml`, and
  `run-examples-node.js` runs the modules it writes in Node and compares
  them with the model.

## Writing a page

Every factual claim must be true of the commit the site is built from.
Quote code only through directives, preferring `"from" "to"` patterns to
line numbers so edits elsewhere in a file do not break a page. Keep the
order of the existing pages: opening claim and what is not proved; client
example; a rejected program with the compiler's message; interface; trusted
base; scope; reproduce. Write plainly; define Vox syntax the first time a
page uses it.

Status: `reviewed` when two independent reviews found no false claim and the
page states every gap they found; `review-pending` when a review found
something to fix or has not been redone; `in-progress` when a public
contract is weaker than the demo needs, stated on the page.

When a demo's code changes, rebuild: a quote whose pattern no longer matches
fails the build, and the counts follow the code.
