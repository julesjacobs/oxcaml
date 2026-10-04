# Vox demo catalogue

A static site with one page per verified demo: its interface and definitions,
what it trusts, what it does not claim, a checked client, a rejected program,
and how to reproduce the checks. Pages quote the repository at one
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
`--home-url URL` adds a "Vox" link to URL at the start of every page's
navigation, for a catalogue that is part of a larger site (the public site
uses `/vox/`); without it there is no such link.

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
- `compiler-example/`: the WebAssembly module the HM-to-Wasm compiler emits
  for its design example and its final memory for the inputs 4 and 8, run in
  the browser on the presentation page. `original_run_smoke.ml` produces them.
  `run-examples.sh` builds `testsuite/tests/vox/hmc_compilation_examples.ml`,
  and `run-examples-node.js` runs the modules it writes in Node and compares
  them with the model.

## Writing a page

Every factual claim must be true of the commit the site is built from.
Quote code only through directives, preferring `"from" "to"` patterns to
line numbers so edits elsewhere in a file do not break a page. Keep the
order of the existing pages: opening behavior summary; interface
and the definitions it uses; trusted base; scope; client example; a rejected
program with the compiler's message; reproduce. The build checks that
Interface, Trusted base and Scope are the first three sections. Keep proof
bodies out of the interface section and identify the files that define each
contract predicate. Write plainly; define Vox syntax the first time a page
uses it. Link complete definitions with `[label](src:PATH)`; the renderer
checks the repository path and links the source in the built catalogue.

A demo should make it plausible that engineers could write, read and maintain
specifications like these in a real codebase. Prefer a useful, understandable
promise to a stronger one that needs a thicket of definitions. Judge the
interface together with the pure definitions it depends on: an opaque name
or a link does not make a complicated meaning simpler.

Union–find illustrates the aim. Its snapshot is a list of classes with a
representative for each; its operation relations permit different list
orders and representative choices. The forest and bridge proofs are private.
More proof work is worthwhile when it makes the public specification easier
to understand. Use the model that fits each demo, keep primary contracts
apart from derived lemmas and accounting, and explain only what readers need.

Status: `owner-review` when the demo and page are ready for the owner to
read; `reviewed` once the owner has checked them; `review-pending` when a
known issue needs attention; `in-progress` while work on the demo is under way.

When a demo's code changes, rebuild: a quote whose pattern no longer matches
fails the build, and the counts follow the code.
