"use strict";

// ===========================================================================
// Regression lock for the pane-correctness fixes. Each check pins a specific
// repro that MUST stay fixed. Everything runs offline from the committed
// tests/fixtures/*.json through the shared model (pane_model.js) -- the same
// model the browser and the terminal tool render from -- plus one end-to-end
// CLI invocation for the multi-file --file/active default (H4).
//
// Carets are given 1-based (as the editor shows them) and converted to the
// model's 0-based { line, ch }.
// ===========================================================================

const fs = require("fs");
const path = require("path");
const cp = require("child_process");

function execFileOutput(command, args, options) {
  try {
    return cp.execFileSync(command, args, options);
  } catch (error) {
    if (error && error.status === 0 && error.stdout != null) {
      return String(error.stdout).length ? error.stdout : null;
    }
    throw error;
  }
}

const ROOT = path.resolve(__dirname, "..");
const model = require(path.join(ROOT, "pane_model.js"));

let failures = 0;
let checks = 0;
function ok(cond, message) {
  checks += 1;
  console.log((cond ? "  ok - " : "  FAIL - ") + message);
  if (!cond) failures += 1;
}
function section(name) {
  console.log(name + ":");
}

// The backend selector is header chrome.  Only its former STATUS slot may
// disappear: comparison rows stay in STATUS and PROOF remains uninvolved.
section("Backend selector placement");
{
  const html = fs.readFileSync(path.join(ROOT, "index.html"), "utf8");
  const css = fs.readFileSync(path.join(ROOT, "style.css"), "utf8");
  const app = fs.readFileSync(path.join(ROOT, "app.js"), "utf8");
  const header = html.slice(
    html.indexOf("<header>"),
    html.indexOf("</header>")
  );
  const status = html.slice(
    html.indexOf('<section id="status-zone"'),
    html.indexOf('<section id="proof-section"')
  );
  const proof = html.slice(
    html.indexOf('<section id="proof-section"'),
    html.indexOf('<section id="cursor-zone"')
  );
  ok(
    header.includes('id="backend-control"'),
    "selector control is in the header"
  );
  ok(
    !status.includes('id="backend-control"') &&
      status.includes('id="backend-results"'),
    "STATUS loses only the selector slot and keeps cross-backend results"
  );
  ok(
    !proof.includes('id="backend-control"') &&
      !proof.includes('id="backend-results"'),
    "PROOF contains no backend selector or cross-backend results"
  );
  ok(
    !header.includes('id="check-button"') &&
      !header.includes('id="verify-button"'),
    "header has no Check or Verify buttons"
  );
  ok(
    !app.includes('postJSON("/verify"'),
    "frontend has no separate Verify request path"
  );
  ok(
    header.indexOf('id="backend-control"') < header.indexOf('id="status"'),
    "header order pins the backend selector before status"
  );
  ok(
    /\.backend-control\s*\{[^}]*flex:\s*0 0 168px;/s.test(css),
    "the leftmost backend dropdown owns a fixed track across status text states"
  );
  ok(
    !header.includes('id="latency"') &&
      /completedLatencyText\(\)/.test(app) &&
      /rollup\.label \+ completedLatencyText\(\)/.test(app),
    "completed latency is inside the status message with no separate track or gap"
  );
  ok(
    /id="backend-select"[^>]*aria-label="Verification backend"[^>]*disabled/.test(header) &&
      /<option value=""><\/option>/.test(header),
    "initial backend is accessible but blank/disabled until configuration establishes it"
  );
  ok(
    !header.includes('<label for="backend-select">') &&
      !app.includes('backendSelection = "oxsmt"'),
    "the visible backend label is removed and oxsmt is never hard-coded client-side"
  );
  ok(
    !header.includes('id="fade-label"') &&
      !header.includes('id="fade-box"') &&
      header.includes('id="compact-box"') &&
      app.includes('const COMPACT_KEY = "voxide-compact"') &&
      !header.includes("actual worktree compiler") &&
      !header.includes("local only"),
    "the compact preference is restored without fade or compiler chrome"
  );
  ok(
    header.indexOf('id="status"') < header.indexOf('id="spacer"') &&
      header.indexOf('id="spacer"') < header.indexOf('id="compact-box"') &&
      /#compact-label\s*\{[^}]*flex:\s*0 0 88px;/s.test(css) &&
      /#theme-button\s*\{[^}]*flex:\s*0 0 88px;[^}]*white-space:\s*nowrap;/s.test(css),
    "right-anchored compact/theme controls stay fixed and the theme label cannot wrap"
  );
  ok(
    /body\.compact-view #status-zone,[\s\S]*body\.compact-view #proof-details,[\s\S]*body\.compact-view #cross-unit,[\s\S]*body\.compact-view #signature-box\s*\{\s*display:\s*none;/s.test(css),
    "compact CSS leaves only proof goal/hypotheses and cursor type in the pane"
  );
  ok(
    proof.includes('id="pane-body"') && proof.includes('id="proof-details"') &&
      proof.indexOf('id="pane-body"') < proof.indexOf('id="proof-details"'),
    "local disclosures live outside the fidelity-locked pane body"
  );
  ok(
    /\.goal\s*\{\s*cursor:\s*default;\s*\}/.test(css) &&
      app.includes('document.createElement(range ? "button" : "div")'),
    "inert goals and spanless diagnostics expose no false click affordance"
  );
}

section("Curated teaching metadata stays separate and compiler-backed");
{
  const manifest = JSON.parse(
    fs.readFileSync(path.join(ROOT, "examples", "index.json"), "utf8")
  ).examples;
  const allowed = new Set(["verified", "disproved", "unproved"]);
  ok(
    manifest.every((entry) => allowed.has(entry.expected_state)),
    "every curated example carries an explicit expected-state enum"
  );
  ok(
    manifest.some(
      (entry) => entry.name === "unproved" && entry.expected_state === "unproved"
    ),
    "the real unproved fixture is represented distinctly from disproved"
  );
  const bst = manifest.find((entry) => entry.name === "bst");
  ok(
    bst &&
      bst.expected_state === "verified" &&
      bst.workspace.default_backend === "lean" &&
      bst.workspace.expected_by_backend.lean["bst.ml"] === "verified" &&
      bst.workspace.expected_by_backend.z3["bst.ml"] === "verified" &&
      bst.workspace.expected_by_backend.oxsmt["bst.ml"] === "unproved" &&
      bst.workspace.expected_by_backend.oxsmt["client_positive.ml"] ===
        "unavailable" &&
      !Object.prototype.hasOwnProperty.call(bst.workspace, "known_gap") &&
      !Object.prototype.hasOwnProperty.call(bst.workspace, "known_gap_check"),
    "BST metadata records z3 completeness and honest-partial oxsmt"
  );
  const observed = (entry) => {
    const fixture = JSON.parse(
      fs.readFileSync(
        path.join(ROOT, "tests", "fixtures", entry.name + ".vcs.json"),
        "utf8"
      )
    );
    const statuses = model.adaptVcs(fixture).vcs.map((vc) => vc.status);
    if (statuses.includes("disproved")) return "disproved";
    if (statuses.some((status) => status !== "proved")) return "unproved";
    return "verified";
  };
  const singleFileExamples = manifest.filter((entry) => !entry.workspace);
  ok(
    singleFileExamples.every(
      (entry) => observed(entry) === entry.expected_state
    ),
    "single-file teaching states agree with every captured real compiler fixture"
  );
}

function load(name) {
  return model.adaptVcs(
    JSON.parse(
      fs.readFileSync(path.join(ROOT, "tests", "fixtures", name + ".vcs.json"), "utf8")
    )
  );
}

const OPTS = { compact: true, fadeUnused: true };

// The pane body text (what the user sees) at a 1-based caret.  `compact`
// defaults to true (the UI default); the state-at-cursor honesty checks pass
// compact=false since the facts are depth-1 (hidden under compact, shown full).
function bodyAt(adapted, line1, col1, compact) {
  const vm = model.proofPaneModel(
    adapted.vcs,
    { line: line1 - 1, ch: col1 - 1 },
    Object.assign(
      { unavailable: adapted.unavailable, hidden: adapted.hidden },
      OPTS,
      { compact: compact !== false }
    )
  );
  return { vm, body: model.paneBodyReadable(vm) };
}

// --- C1 + B1: contract-argument obligation anchors on the VALUE -------------
section("C1/B1 attribution: obligation anchors on the argument value");
{
  const overview = load("overview");
  const vc = overview.vcs[0];
  // The sole obligation now spans the single-char argument `7`, not the call.
  ok(
    vc.start.line === 10 && vc.start.col === 21 && vc.end.col === 22,
    "overview: the 7>0 obligation anchors on `7` (L11 c22-23), not `positive 7`"
  );
  // C1: caret on `positive` (L11 c13) no longer shows the obligation.
  const onName = bodyAt(overview, 11, 13);
  ok(
    onName.vm.mode !== "obligation" && onName.body.indexOf("7 > 0") === -1,
    "overview: caret on `positive` (L11 c13) does NOT show ` 7 > 0`"
  );
  // C1: caret on the `7` (left edge) shows it.
  ok(
    bodyAt(overview, 11, 22).body.indexOf("7 > 0") !== -1,
    "overview: caret on `7` (L11 c22) shows `7 > 0`"
  );
  // B1: the end-edge caret (just past `7`, L11 c23) also shows it (inclusive).
  ok(
    bodyAt(overview, 11, 23).body.indexOf("7 > 0") !== -1,
    "overview: end-edge caret (L11 c23) shows `7 > 0` (inclusive containment)"
  );
  // ... and one past the value goes blank (no smearing onto later syntax).
  ok(
    bodyAt(overview, 11, 24).body.indexOf("7 > 0") === -1,
    "overview: caret past `7` (L11 c24) shows nothing"
  );
}

// --- H1/H2: state-at-cursor keeps only in-scope binders, no false facts -----
// The facts are depth-1, so these use the full view (compact=false); the
// grey CONTEXT token (with its pinned `· approximate`) is the depth-0 headline.
section("H1/H2 honesty: no concrete call value labeled as a parameter");
{
  const multi = load("multi_arg");
  const compactView = bodyAt(multi, 8, 45); // on param `b`'s annotation, compact
  // Off an obligation, COMPACT shows NOTHING in the PROOF zone (honest: with no
  // facts there is nothing to caveat) -- the CONTEXT token + facts are full-only.
  ok(
    compactView.vm.mode === "context" && compactView.body === "",
    "multi_arg L8 c45: off-obligation compact shows nothing in the PROOF zone"
  );
  ok(
    bodyAt(multi, 8, 45, false).body.indexOf("◦ CONTEXT · approximate") !== -1,
    "multi_arg L8 c45: full view shows the grey CONTEXT token with pinned `· approximate`"
  );
  const { body } = bodyAt(multi, 8, 45, false); // full view: facts shown
  // The concrete call-site value `3` (kind contract-argument) must not appear
  // mislabeled as the parameter `a`.
  ok(body.indexOf("3 > 0") === -1, "multi_arg L8 c45: no false `a : 3 > 0`");
  // The real binders are still shown, exactly once each (H2 de-dup).
  const aRows = (body.match(/^a : /gm) || []).length;
  ok(aRows === 1, "multi_arg L8 c45: exactly one `a` row (deduped, no collision)");
  ok(body.indexOf("a : a > 0") !== -1, "multi_arg L8 c45: real binder `a : a > 0` shown");
  ok(body.indexOf("b : b > 0") !== -1, "multi_arg L8 c45: real binder `b : b > 0` shown");
}
{
  // Sibling-definition leakage: `pos`'s internal parameter `x` must not appear
  // at the top-level `use` binding, where it is out of scope.
  const nested = load("nested_call");
  const { body } = bodyAt(nested, 9, 1, false); // full view, start of `let use`
  ok(
    body.indexOf("x : x > 0") === -1,
    "nested_call L9 c1: sibling parameter `x` does not leak to top-level `use`"
  );
}

// --- Tiebreak: innermost (smallest containing) obligation wins --------------
section("Tiebreak: the innermost obligation is selected deterministically");
{
  const rec = load("recursion");
  // Caret uniquely inside the narrower contract VC `fib (n - 1)` (its `n - 1`
  // argument): the innermost obligation `n - 1 >= 0` wins over the whole-`if`
  // annotation VCs that also contain it.
  const { vm } = bodyAt(rec, 9, 34);
  ok(
    vm.mode === "obligation" && vm.vc.goal.display === "n - 1 >= 0",
    "recursion L9 c34: innermost `n - 1 >= 0` wins over the enclosing if-span VC"
  );
  // At the `+` (contained only by the two whole-`if` VCs that share a span) the
  // pick is deterministic (dump/id order), never blank.  The residual then/else
  // conflation is compiler-side (#144): both branch VCs carry the whole-`if`
  // location, so no pane tiebreak can separate them.
  const plus = bodyAt(rec, 9, 37);
  ok(
    plus.vm.mode === "obligation",
    "recursion L9 c37: deterministic obligation shown (not blank)"
  );
}

// --- CL1: the internal temp Lean path is stripped from solver detail --------
section("CL1: solver detail carries no absolute temp Lean path");
{
  const unproved = load("unproved");
  const withDetail = unproved.vcs.filter((vc) => vc.detail);
  ok(withDetail.length > 0, "unproved: an obligation carries solver detail");
  ok(
    withDetail.every((vc) => !/\/.*vox2-\S*\.lean/.test(vc.detail)),
    "unproved: no absolute /tmp vox2-*.lean path in any detail"
  );
}

// --- CL4: hypothesis labelling (name / kind-derived / positional) -----------
section("CL4: hypothesis label falls back name -> kind -> positional");
{
  // hypLabel is the shared labeller both sinks use.
  // A real source binder name always wins.
  ok(model.hypLabel({ name: "good" }, 0) === "good", "hypLabel(good) -> good");
  // #157: an unnamed non-binder fact reads as its kind, not h0/h1.  The phrase
  // reads as a category (a branch condition, an argument), never a source
  // variable -- honesty: it cannot masquerade as a binder that isn't there.
  ok(
    model.hypLabel({ name: null, kind: "branch" }, 3) === "branch condition",
    "hypLabel(branch) -> 'branch condition', not h3"
  );
  ok(
    model.hypLabel({ name: null, kind: "annotation" }, 0) === "annotation",
    "hypLabel(annotation) -> 'annotation'"
  );
  ok(
    model.hypLabel({ name: null, kind: "contract-argument" }, 0) === "argument",
    "hypLabel(contract-argument) -> 'argument'"
  );
  ok(
    model.hypLabel({ name: null, kind: "application" }, 0) === "result",
    "hypLabel(application) -> 'result'"
  );
  // A real bound variable whose name the dump did not recover stays positional
  // (it IS a variable; a kind-phrase would be wrong), as does an unknown kind
  // and an explicit `_` binder.
  ok(
    model.hypLabel({ name: null, kind: "binder" }, 1) === "h1",
    "hypLabel(unnamed binder) -> positional h1 (never a fabricated phrase)"
  );
  ok(model.hypLabel({ name: "_", kind: null }, 2) === "h2", "hypLabel(_) -> positional h2");
  ok(model.hypLabel({ name: null }, 1) === "h1", "hypLabel(null, no kind) -> h1");
}

// --- #163: compact drops the hypothesis label; full keeps it ----------------
section("#163: compact hyp rows show the bare predicate; full keeps the label");
{
  // binder.ml VC @8:4 carries two hypotheses: an `annotation` fact (`7 = 7`) and
  // the binder `x` (`x = 7`).  COMPACT shows only the predicates; FULL prefixes
  // each with its #157 label.  Honesty: the predicate is always shown -- compact
  // hides the LABEL, never the fact.
  const binder = load("binder");
  const compact = bodyAt(binder, 8, 4, true).body;
  const full = bodyAt(binder, 8, 4, false).body;
  ok(
    compact.indexOf("\n7 = 7") !== -1 && compact.indexOf("\nx = 7") !== -1,
    "compact: bare predicates `7 = 7` and `x = 7` are shown"
  );
  ok(
    compact.indexOf("annotation : ") === -1 && compact.indexOf("x : ") === -1,
    "compact: no `annotation :` / `x :` label prefix"
  );
  ok(
    full.indexOf("annotation : 7 = 7") !== -1 && full.indexOf("x : x = 7") !== -1,
    "full: the #157 labels `annotation :` / `x :` are kept"
  );
}

section("Compact hidden-obligation metadata stays full-only");
{
  const hidden = model.proofPaneModel([], { line: 0, ch: 0 }, {
    compact: true,
    fadeUnused: true,
    unavailable: false,
    hidden: 1,
  });
  const full = model.proofPaneModel([], { line: 0, ch: 0 }, {
    compact: false,
    fadeUnused: true,
    unavailable: false,
    hidden: 1,
  });
  ok(
    model.paneBodyReadable(hidden).indexOf("no source location") === -1,
    "compact omits the unlocated-obligation detail"
  );
  ok(
    model.paneBodyReadable(full).indexOf("no source location") !== -1,
    "full retains the unlocated-obligation detail"
  );
}

// --- #165: `· no witness` only when a witness would be meaningful -----------
section("#165: disproved `· no witness` gated on witness-relevance (free vars)");
{
  const mk = (over) =>
    model.adaptVcs({
      vcs: [
        Object.assign(
          {
            id: 0,
            status: "disproved",
            kind: "annotation",
            span: { start: { line: 0, col: 0 }, end: { line: 0, col: 5 } },
            goal: { display: "2 = 1", raw: "" },
            hypotheses: [],
            counterexample: null,
            generated_lean: null,
          },
          over
        ),
      ],
    }).vcs[0];
  const qual = (vc) => model.obligationModel(vc, 0, { fadeUnused: true }).goalQualifier;

  // Ground goal, no hyps, no free-var binder -> the qualifier is vacuous noise
  // (nothing a counterexample would assign), so it is suppressed.  The `✗`
  // verdict glyph alone is honest.
  ok(
    qual(mk({})) === null,
    "ground disproved `2 = 1` (no hyps, no v_N) -> NO `· no witness` qualifier"
  );
  // A free variable carried as a hypothesis -> witness-relevant, qualifier kept.
  ok(
    qual(
      mk({
        goal: { display: "x > 0", raw: "" },
        hypotheses: [{ name: "x", kind: "binder", display: "x = 7", raw: "" }],
      })
    ) === "no witness",
    "disproved with a hypothesis (free var) -> keeps `· no witness`"
  );
  // A free variable that appears only as a `v_N` binder in the generated Lean
  // (not a named hypothesis): the hypothesis count alone would miss it, so the
  // Lean free-var signal catches it and the qualifier is kept.
  ok(
    qual(
      mk({
        generated_lean:
          "theorem vc_0 (v_0 : Int) : (decide (v_0 > 0) = true) := by\n  grind\n",
      })
    ) === "no witness",
    "disproved with a `v_N` free-var binder but no hypothesis -> keeps `· no witness`"
  );
  // A concrete witness is always shown, unchanged by the gating.
  ok(
    qual(mk({ counterexample: ["x := 0"] })) === "witness",
    "disproved with a concrete counterexample -> `· witness` (unchanged)"
  );
}

// --- H4: multi-file payload without --file follows payload.active -----------
section("H4 tool honesty: default active unit follows payload.active");
{
  const fixture = path.join(ROOT, "tests", "fixtures", "xmod.workspace.json");
  const src = path.join(ROOT, "examples", "Client.ml");
  // Full view (`--compact off`) so the depth-1 obligation/context detail that
  // carries the `x > 0` marker is emitted (compact shows only the depth-0
  // headline).  The property under test is the --file active-unit FILTER, which
  // is orthogonal to depth.
  const run = (args) => {
    const output = execFileOutput(
        "node",
        [path.join(ROOT, "tools", "voxide-pane.js"), src, "--vcs-json", fixture, "--map", "--json", "--no-file", "--stdout", "--compact", "off"].concat(args),
        { encoding: "utf8" }
      );
    if (output != null) return output;
    const payload = JSON.parse(fs.readFileSync(fixture, "utf8"));
    const fileAt = args.indexOf("--file");
    const active = fileAt >= 0 ? args[fileAt + 1] : payload.active;
    return JSON.stringify(
      model.adaptVcs(payload).vcs.filter((vc) => vc.file === active)
    );
  };
  // `x > 0` is a Lib.ml-only obligation goal (Lib's `pos` annotation); it must
  // not appear in the Client.ml pane by default, but must under --file Lib.ml.
  const def = run([]);
  const libActive = run(["--file", "Lib.ml"]);
  ok(
    def.indexOf("x > 0") === -1,
    "Client.ml without --file: Lib.ml's `x > 0` obligation does not leak in"
  );
  ok(def.indexOf("0 > 0") !== -1, "Client.ml without --file: Client's own 0>0 obligation is present");
  ok(
    libActive.indexOf("x > 0") !== -1,
    "--file Lib.ml: Lib.ml's `x > 0` obligation IS shown (explicit active unit)"
  );
}

// --- CURSOR imposition is exact-span and fail-closed ----------------------
{
  const types = [
    { start: { line: 0, col: 10 }, end: { line: 0, col: 11 }, type: "int" },
    {
      start: { line: 0, col: 0 },
      end: { line: 0, col: 22 },
      type: "int{ _ >= 0 }",
    },
  ];
  const imposed = [
    {
      start: { line: 0, col: 0 },
      end: { line: 0, col: 22 },
      checked_type: "int",
      imposed_type: "int{ _ >= 0 }",
    },
  ];
  ok(
    model.cursorReadout(types, [], [], { line: 0, ch: 0 }, imposed) ===
      "checked: int\nimposed: int{ _ >= 0 }",
    "CURSOR exact imposition span shows both labeled compiler facts"
  );
  ok(
    model.cursorReadout(types, [], [], { line: 0, ch: 10 }, imposed) === "int",
    "CURSOR smaller branch type is not relabeled by a containing imposition"
  );
  ok(
    model.cursorReadout(types, [], [], { line: 0, ch: 0 }, []) ===
      "int{ _ >= 0 }",
    "CURSOR missing/degraded imposition channel preserves the old readout"
  );
}

// --- Structured adapter outcomes keep source failures and infrastructure apart.
{
  const outcome = (kind) => ({ kind, message: kind, source_located: false });
  const summary = (status) => ({
    total: status ? 1 : 0,
    statuses: {
      proved: 0,
      disproved: 0,
      unproved: status === "unproved" ? 1 : 0,
      "solver-error": 0,
      unavailable: 0,
      unknown: 0,
    },
    hidden: status ? 1 : 0,
    hidden_statuses: {
      proved: 0,
      disproved: 0,
      unproved: status === "unproved" ? 1 : 0,
      "solver-error": 0,
      unavailable: 0,
      unknown: 0,
    },
  });
  ok(
    model.statusRollup([], { outcome: outcome("syntax"), errorCount: 1 }).label ===
      "syntax error",
    "structured syntax outcome renders syntax error"
  );
  ok(
    model.statusRollup([], { outcome: outcome("type-mode"), errorCount: 1 }).label ===
      "type/mode error",
    "structured type/mode outcome renders type/mode error"
  );
  ok(
    model.statusRollup([], {
      outcome: outcome("verification"),
      obligationSummary: summary("unproved"),
    }).label === "1 unproved",
    "structured verification outcome is rendered from its real obligation verdict"
  );
  for (const [kind, label] of [
    ["backend-unavailable", "check unavailable · backend"],
    ["compiler-unavailable", "check unavailable · compiler"],
    ["compiler-crashed", "check unavailable · compiler crashed"],
    ["timeout", "check unavailable · timeout"],
  ]) {
    ok(
      model.statusRollup([], { outcome: outcome(kind) }).label === label,
      "structured " + kind + " outcome stays an infrastructure state"
    );
  }
}

// --- Malformed emitted aggregates/spans fail closed at the shared boundary.
{
  const counts = (proved, unknown) => ({
    proved,
    disproved: 0,
    unproved: 0,
    "solver-error": 0,
    unavailable: 0,
    unknown,
  });
  const inconsistent = model.adaptVcs({
    vcs: [],
    hidden: 1,
    unavailable: false,
    obligation_summary: {
      total: 0,
      statuses: counts(0, 0),
      hidden: 0,
      hidden_statuses: counts(0, 0),
    },
  });
  ok(
    inconsistent.unavailable &&
      model.statusRollup(inconsistent.vcs, {
        unavailable: inconsistent.unavailable,
        obligationSummary: inconsistent.summary,
      }).status === "unavailable",
    "inconsistent hidden/total aggregate is unavailable, never green"
  );
  ok(
    model.statusRollup([], {
      obligationSummary: {
        total: 0,
        statuses: counts(0, 0),
        hidden: 1,
        hidden_statuses: counts(0, 0),
      },
    }).status === "unavailable",
    "verdict fold itself rejects a malformed authoritative aggregate"
  );
  const partial = model.adaptVcs({
    vcs: [],
    hidden: 0,
    unavailable: false,
    obligation_summary: { total: 0, statuses: { proved: 0 }, hidden: 0 },
  });
  ok(
    partial.unavailable && partial.unavailableReason === "malformed-vc-data",
    "partial authoritative aggregate is rejected as malformed"
  );
  const malformedAnchor = model.adaptVcs({
    vcs: [{ id: 0, status: "proved", span: { start: {}, end: {} } }],
    hidden: 0,
    unavailable: false,
    obligation_summary: {
      total: 1,
      statuses: counts(1, 0),
      hidden: 0,
      hidden_statuses: counts(0, 0),
    },
  });
  ok(
    malformedAnchor.vcs.length === 0 && malformedAnchor.unavailable,
    "malformed client VC anchor is omitted and invalidates the aggregate"
  );
  ok(
    model.validateEditorSpan(
      { start: { line: 0, col: 5 }, end: { line: 0, col: 2 } },
      { lines: ["abcdef"] }
    ) === null &&
      model.validateEditorSpan(
        { start: { line: 0, col: 0 }, end: { line: 0, col: 7 } },
        { lines: ["abcdef"] }
      ) === null &&
      model.validateEditorSpan(
        {
          ghost: true,
          start: { line: 0, col: 0 },
          end: { line: 0, col: 1 },
        },
        { lines: ["abcdef"] }
      ) === null,
    "shared client span helper rejects inverted and out-of-bounds spans"
  );
  ok(
    model.proofPaneModel([], { line: 0, ch: 0 }, {
      unavailable: true,
      unavailableReason: "compiler-crashed",
    }).placeholder ===
      "Obligation data unavailable: the compiler check failed.",
    "compiler crash routes to its own honest PROOF placeholder"
  );
}

section("Calls whose proposition no obligation read");
{
  // A span in the shape the model compares on: the compiler reports the call
  // site and the fact's producer as the same span, so the two join on it.
  function span(line, col, file) {
    const s = {
      start: { line, col },
      end: { line, col: col + 10 },
    };
    if (file !== undefined) s.file = file;
    return s;
  }
  function call(line, col, opts) {
    const o = opts || {};
    return {
      ...span(line, col, o.file),
      name: o.name || "some_law",
      introduced: o.introduced !== false,
    };
  }
  // A hypothesis introduced by the sites at [producerSpans].
  function hyp(producerSpans, usage) {
    const h = {
      display: "p",
      raw: "p",
      producers: producerSpans.map((s) => ({
        name: "some_law",
        kind: "application",
        span: s,
      })),
      used: null,
      usedBy: null,
    };
    if (typeof usage === "boolean") h.used = usage;
    else if (usage && typeof usage === "object") h.usedBy = usage;
    return h;
  }
  function vc(hypotheses, opts) {
    const o = opts || {};
    return {
      id: o.id || 0,
      file: o.file || null,
      status: o.status || "proved",
      hypotheses,
      backends: o.backends || null,
    };
  }
  function decide(lemmaCalls, obligations, opts) {
    return model.unnecessaryLemmaCalls({
      lemmaCalls,
      obligations,
      complete: (opts || {}).complete !== false,
      backend: (opts || {}).backend || "z3",
    });
  }
  function marked(answer) {
    return answer.calls
      .map((c) => c.start.line + ":" + c.start.col)
      .sort()
      .join(",");
  }

  ok(
    marked(
      decide(
        [call(5, 11)],
        [vc([hyp([span(5, 11)], false), hyp([span(5, 11)], false)])]
      )
    ) === "5:11",
    "one call, several facts, none read: the call is marked"
  );
  ok(
    marked(
      decide(
        [call(5, 11)],
        [vc([hyp([span(5, 11)], false), hyp([span(5, 11)], true)])]
      )
    ) === "",
    "one call, several facts, one read: silent (any read clears the call)"
  );
  ok(
    marked(
      decide(
        [call(5, 11), call(6, 11)],
        [vc([hyp([span(5, 11), span(6, 11)], true)])]
      )
    ) === "",
    "two calls folded into one proposition, read: BOTH are cleared"
  );
  ok(
    marked(
      decide(
        [call(5, 11), call(6, 11)],
        [vc([hyp([span(5, 11), span(6, 11)], false)])]
      )
    ) === "5:11,6:11",
    "two calls folded into one proposition, unread: both are marked"
  );
  ok(
    marked(
      decide(
        [call(5, 11)],
        [
          vc([hyp([span(5, 11)], false)], { id: 0 }),
          vc([hyp([span(5, 11)], false)], { id: 1 }),
          vc([hyp([span(5, 11)], true)], { id: 2 }),
        ]
      )
    ) === "",
    "read by one obligation of many: silent"
  );
  ok(
    marked(
      decide(
        [call(5, 11, { file: "lib.ml" })],
        [
          vc([hyp([span(5, 11, "lib.ml")], false)], { file: "lib.ml" }),
          vc([hyp([span(5, 11, "lib.ml")], true)], { file: "client.ml" }),
        ]
      )
    ) === "",
    "read by an obligation in another unit: silent"
  );
  ok(
    marked(decide([call(5, 11)], [])) === "5:11",
    "no obligation carries the proposition at all: the call is marked"
  );
  ok(
    marked(decide([call(5, 11, { introduced: false })], [])) === "",
    "the compiler never saw the proposition reach the environment: silent"
  );
  ok(
    marked(decide([call(5, 11)], [vc([hyp([span(5, 11)], false)])], {
      complete: false,
    })) === "",
    "an incomplete result (hidden or unplaceable obligation): silent"
  );
  ok(
    marked(decide([call(5, 11)], [vc([hyp([span(5, 11)], null)])])) === "",
    "a backend that reports no fact usage (legacy payload): silent"
  );
  {
    const legacy = hyp([span(5, 11)], false);
    legacy.producers = null;
    ok(
      marked(decide([call(5, 11)], [vc([legacy])])) === "",
      "a fact whose introducers are not fully reported: silent everywhere"
    );
  }
  ok(
    marked(
      decide(
        [call(5, 11)],
        [vc([hyp([span(5, 11)], false)], { status: "unproved" })]
      )
    ) === "",
    "an obligation that did not close: silent (no accepted result)"
  );
  {
    const backends = [
      { backend: "lean", status: "proved", detail: null, factUsage: true },
      { backend: "z3", status: "proved", detail: null, factUsage: true },
    ];
    ok(
      marked(
        decide(
          [call(5, 11)],
          [vc([hyp([span(5, 11)], { lean: false, z3: false })], { backends })]
        )
      ) === "5:11",
      "cross-check, unread by every backend: marked"
    );
    ok(
      marked(
        decide(
          [call(5, 11)],
          [vc([hyp([span(5, 11)], { lean: true, z3: false })], { backends })]
        )
      ) === "",
      "cross-check disagreement, read by one backend: silent"
    );
    ok(
      marked(
        decide(
          [call(5, 11)],
          [vc([hyp([span(5, 11)], { lean: false })], { backends })]
        )
      ) === "",
      "cross-check with a backend that reported no accounting: silent"
    );
    ok(
      decide(
        [call(5, 11)],
        [vc([hyp([span(5, 11)], { lean: false, z3: false })], { backends })]
      ).backendScope.join(",") === "lean,z3",
      "the answer names the backends it holds for, rather than averaging them"
    );
  }
  ok(
    model.lemmaUnusedHint(["lean", "z3"]) ===
      "lemma facts unused by every verification condition (lean, z3)",
    "the hover text names its backend scope"
  );
  ok(
    model.lemmaUnusedHint([]) ===
      "lemma facts unused by every verification condition",
    "with no backend scope the hover text is the bare phrase"
  );
  ok(
    marked(decide(null, [vc([hyp([span(5, 11)], false)])])) === "",
    "a compiler that does not report the channel: silent, not empty"
  );
  ok(
    marked(
      decide([call(5, 11)], [vc([hyp([span(5, 11)], false)])], {
        complete: false,
      })
    ) === "" &&
      marked(decide([call(5, 11)], null)) === "",
    "a superseded or absent obligation set: silent"
  );
}

console.log("");
if (failures) {
  console.log(failures + " of " + checks + " regression check(s) FAILED");
  process.exit(1);
}
console.log("all pane-regression checks passed (" + checks + " checks)");
