# Explorer content: schema

The source explorer shows two kinds of hand-written content on top of the
data that `build.py` computes from git (the tree, line counts, Vox's diff
against the upstream merge base, demo membership):

- **descriptions**: for a directory or a file, what this part of OxCaml is,
  and what Vox changed there and why, plus optional notes on line ranges;
- **tours**: named sequences of stops, each a place in the source with a
  short narrative.

Content is JSON, in this directory. Several authors can work in parallel as
long as each owns different files; no code needs to change.

```
content/
  SCHEMA.md
  descriptions/<anything>.json   any number of files, merged
  tours/<id>.json                one tour per file
```

Check your work with

```sh
python3 verification/explorer/content.py          # errors and warnings
python3 verification/explorer/content.py --show   # also where every range resolves
```

It checks against the commit `HEAD` (use `--revision REV` for another, or
`--working-tree` to check patterns against uncommitted source edits). A
build with an error fails, so a later revision that moves or deletes the
code a range points at is caught.

## Paths

Paths are relative to the repository root, exactly as at the revision the
site is built from. A directory ends in `/` (`typing/`,
`middle_end/flambda2/`); the root is `/`. A file has no trailing slash
(`typing/typecore.ml`).

## Descriptions

A descriptions file is one JSON object mapping paths to entries. A path may
be described in only one file. Keys starting with `_` are ignored anywhere,
so you can record where a claim comes from:

```json
{
  "_about": "Top-level directories. Sources: mastery INDEX Part A.",
  "typing/": {
    "what": {
      "short": "The type checker: parsetree to typedtree, with modes, kinds (jkinds) and module inclusion.",
      "long": ["First paragraph, Markdown.", "Second paragraph."]
    },
    "vox": {
      "short": "Refinement types, dependent arrows, the totality and ghostliness mode axes, and the typer's hooks for the verifier.",
      "long": "Markdown."
    }
  },
  "typing/typecore.ml": {
    "what": {"short": "Type inference for expressions and patterns."},
    "vox": {"short": "Elimination and introduction of refinements at expression boundaries; ghost_, refine_, assume_."},
    "notes": [
      {
        "title": "Introduction of an expected refinement",
        "from": "let introduce_refinement",
        "span": 12,
        "kind": "vox",
        "text": "Markdown, 1–4 sentences."
      }
    ]
  }
}
```

| Key | Required | Meaning |
|---|---|---|
| `what` | no | What this part of OxCaml (or of Vox, for Vox-only code) is, for a reader who knows the upstream compiler. |
| `vox` | no | What Vox changed here and why. Omit it where Vox changed nothing. |
| `what.short`, `vox.short` | yes, if the block is present | One plain sentence, under 200 characters. It appears in hover tooltips, so no Markdown. |
| `what.long`, `vox.long` | no | Markdown, one to four paragraphs, shown in the side panel under "More". |
| `notes` | no, files only | Notes on line ranges, shown in the side panel and as marks in the file view. |
| `notes[].from`, `to`, `span`, `lines` | `from` | The range; see *Line ranges*. |
| `notes[].title` | no | A short label. |
| `notes[].kind` | no | `"vox"` (default) for a note about Vox's change, `"what"` for one about the upstream code. |
| `notes[].text` | yes | Markdown, one to four sentences. |

Write for the Jane Street OxCaml compiler team: they know the upstream
compiler intimately, so say what is new or different, name the functions,
and do not explain OCaml. Every claim must be true of the commit the site
is built from; prefer the code over the reports when they disagree.

## Tours

A tour file `tours/<id>.json`:

```json
{
  "id": "flat-hash-table",
  "title": "The flat hash table",
  "kind": "demo",
  "demo": "flat-hash-table",
  "order": 10,
  "summary": "Markdown, one to three sentences: what the tour shows.",
  "stops": [
    {
      "title": "The public interface",
      "path": "verification/library/vox_verified_flat_hashtbl.mli",
      "from": "module type S = sig",
      "to": "end",
      "lines": "40-96",
      "text": "Markdown, two to six sentences."
    }
  ]
}
```

| Key | Required | Meaning |
|---|---|---|
| `id` | yes | The file name without `.json`; used in URLs (`#tour/<id>/<stop>`). Lower case with hyphens. |
| `title` | yes | Shown in the tour list. |
| `kind` | yes | `"vox"` for a tour of Vox's own source, `"demo"` for a demo tour. |
| `demo` | for `kind: demo` | The demo's id in `verification/catalogue/catalogue.json`; the tour links to its catalogue page. |
| `order` | no | Sort key within its kind (default 1000). |
| `summary` | no | Markdown shown before the first stop. |
| `stops[].title` | yes | Under 70 characters. |
| `stops[].path` | yes | A file, or a directory (ending in `/`) for a stop about a whole part of the tree. |
| `stops[].from`, `to`, `span`, `lines` | no | The range in the file to show; see *Line ranges*. A directory stop has none. |
| `stops[].focus` | no | The directory the treemap zooms to (default: the file's directory). Use it to show a file in a wider context, e.g. `"typing/"`. |
| `stops[].text` | yes | Markdown, **two to six sentences**. Say what the reader is looking at and why it matters; name the lines. |

Stops are numbered from 1 in URLs: `#tour/flat-hash-table/3`.

A good stop shows 5–40 lines: the reader sees about 40 at once, and the
view scrolls to the start of the range and highlights it.

## Line ranges

A range is written with patterns, like the catalogue's `@code`, so that it
survives edits elsewhere in the file:

- `from`: a string that occurs on **exactly one** line of the file; the
  range starts there. It may be a list, `["and type_expect_", "| Pexp_apply"]`:
  the first string must be unique, and each later one is the first line
  containing it *after* the previous match. Use a list to reach a line
  whose text is not unique.
- `to` (optional): the range ends at the first line, at or after the start,
  containing this string. It may also be a list, searched in order.
- `span` (optional, instead of `to`): the number of lines in the range.
- Neither `to` nor `span`: the range is the single line of `from`.
- `lines` (optional): `"N-M"` or `"N"`, what you expect the patterns to give.
  It is for human readers; the checker warns when the patterns give
  something else, and the patterns win.

Patterns are plain substrings (not regular expressions), matched against
one line at a time. Choose text that names the thing (`let rec
type_approx`, `| Texp_refinement`) rather than punctuation.

## Markdown

Text fields (`long`, `text`, `summary`) take a string or a list of strings;
a list is joined as separate paragraphs. The subset:

- paragraphs separated by a blank line, and `- ` bullet lists;
- fenced code blocks with three backticks (keep them short; prefer a stop
  or note that points at the code);
- `code`, `**bold**`, `*emphasis*`;
- links: `[text](https://...)`, and inside the explorer
  `[text](src:typing/ctype.ml)` (a file or directory; checked to exist),
  `[text](tour:vox-inside-oxcaml)` or `[text](tour:vox-inside-oxcaml/4)`,
  and `[text](demo:flat-hash-table)` (the demo's catalogue page).

Raw HTML is escaped.

## Style

Plain, serious, precise. Short declarative sentences. No marketing, no
"simply", no exclamation marks. British or American spelling, but be
consistent within a file. Refer to code by name (`Vox_vc.generate`,
`Trefine`) and to lines through ranges, not through line numbers in the
prose, which go stale.
