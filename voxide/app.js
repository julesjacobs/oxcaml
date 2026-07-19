"use strict";

// Fallback buffer, used only if the curated examples fail to load.
const SAMPLE = `(* Refinements are checked and their obligations discharged by Lean
   as you edit.  This buffer verifies clean. *)
type positive = int{ _ > 0 }

let three : int{ _ >= 3 } = 5     (* obligation 5 >= 3: discharged *)
let f (x : int{ _ > 0 }) = x
let seven = f 7                   (* obligation 7 > 0: discharged *)
`;

const cm = CodeMirror.fromTextArea(document.getElementById("code"), {
  mode: "text/x-vox",
  lineNumbers: true,
  indentUnit: 2,
  tabSize: 2,
  lineWrapping: false,
});

const statusElement = document.getElementById("status");
const diagnosticsElement = document.getElementById("diagnostics");
const signatureElement = document.getElementById("signature");
const cursorTypeElement = document.getElementById("cursor-type");
const verifyElement = document.getElementById("verify-output");
const verificationDetailsElement = document.getElementById("verification-details");
const paneModeElement = document.getElementById("pane-mode");
const paneBodyElement = document.getElementById("pane-body");
const proofDetailsElement = document.getElementById("proof-details");
const legendElement = document.getElementById("legend");
const editorPaneElement = document.getElementById("editor-pane");
const docViewElement = document.getElementById("doc-view");
const tabsElement = document.getElementById("tabs");
const crossUnitElement = document.getElementById("cross-unit");
const backendControlElement = document.getElementById("backend-control");
const backendSelectElement = document.getElementById("backend-select");
const backendResultsElement = document.getElementById("backend-results");

let documentRevision = 0;
let appliedRevision = -1;
let expressionTypes = [];
// Type-at-cursor ranges for the subterms of refinement predicates (the hole
// `_`, literals, operators, the whole predicate).  These come from the /vcs
// dump, not the /check `.annot`, because a predicate is not a program
// expression -- so they are kept in their own array and folded into the cursor
// readout alongside `expressionTypes` (smallest containing span wins).
let refinementTypes = [];
// Exact annotation/imposition spans for which the compiler adapter could join
// a checked predicate-hole skeleton to the .annot-imposed type.  Empty for a
// legacy, degraded, or ambiguous dump; the cursor renderer never infers one.
let imposedTypes = [];
// Mode-at-cursor ranges for identifier binders and reads.  These ride the VC
// dump beside refinementTypes because .annot has no mode channel.
let identifierModes = [];
let diagnosticMarks = [];
let inlineDiagnosticMark = null;
let currentErrors = [];
let debounceTimer = null;
let checkQueued = false;
let retryTimer = null;
let retryAttempt = 0;
let retryGeneration = 0;
let authoritativeController = null;
let signatureController = null;
let vcsInFlight = false;
let vcsQueued = false;
let lastVerification = null;
// The last compile outcome, folded with the obligations from the same /check
// response into the STATUS roll-up token.  Fail-closed defaults: no successful
// compile seen yet.
let lastCompiles = false;
let lastErrorCount = 0;
let lastOutcome = null;
// Whether a COMPLETED check has populated the outcome for this buffer.  Until
// the first unified result lands, the STATUS header must show the pending state
// and NEVER a fail-closed verdict.  Reset on every edit or buffer switch.
let firstCheckDone = false;
// The authoritative round time is part of a completed status sentence. It is
// cleared with all results and never rendered while checking/retrying.
let lastLatencyMs = null;

// The verification conditions for the current buffer, as normalized by
// adaptVcs (fed from /vcs).  Cursor moves re-render the proof pane from this
// cache with no network round-trip.
let vcs = [];
let vcMarks = [];
// True when the last /vcs could not produce trustworthy data (the compile did
// not run, or the dump was missing/unreadable/malformed) -- shown distinctly
// from a program that genuinely has no obligations.
let vcsUnavailable = false;
let vcsUnavailableReason = "unknown";
let obligationSummary = summaryFromVcs([], 0);
// Obligations the dump reported but that have no placeable source span, so the
// pane can note a count instead of letting it silently shrink.
let vcsHidden = 0;
// The obligation currently shown in the proof pane, so the delegated
// hypothesis-click handler can resolve a clicked row to its source span.
let paneVc = null;

// The file-explorer path id of the buffer's current file
// ("examples/overview.ml"), or null for a hand-edited scratch buffer.
// Sent with checks and used to drop responses from a superseded buffer.
let currentPath = null;
// The source last loaded into the editor, so we can warn before discarding
// hand edits when a new file is picked.  Empty until an editable file (or the
// SAMPLE fallback) is loaded, so restoring a doc on reload -- which never
// loads an editable buffer -- does not later trigger a spurious guard.
let lastLoaded = "";
// A delayed column refinement for a teaching cursor that was already placed on
// its authored line. The binding records every state that must remain untouched
// before a VC response may refine column 0 to the compiler-emitted span.
let pendingCursor = null;
let cursorInteractionToken = 0;
let suppressCursorInteraction = false;
// True while a read-only .md doc is shown in the rendered doc viewer (the
// editor is hidden).  Docs are never compiled, so every compile path checks
// this and no-ops.
let docOpen = false;

// ---------------------------------------------------------------------------
// Multi-file workspace (slice 6) state.
//
// A workspace is a set of editable buffers compiled together; the single
// CodeMirror instance shows the *active* one.  When workspaceMode is off,
// every path below behaves exactly as the single-buffer editor did.
// ---------------------------------------------------------------------------
let workspaceMode = false;
// filename -> buffer text, for the whole open set (the active one also lives in
// the editor and is mirrored back here on every edit).
let workspaceBuffers = {};
// The unit currently shown in the editor / followed by the pane and marks.
let activeFile = null;
// The most recent /workspace-check payload, so switching tabs can re-derive the
// active-file view (marks, diagnostics, cross-unit list) without a round-trip
// while a fresh compile refines the active unit's types/signature.
let lastWorkspacePayload = null;
// A known implementation backend gap may prevent later units from being
// reached.  When configured by curated metadata, a second live check of the
// unaffected layer is kept separately from the full-workspace observation.
let lastWorkspaceLayer = null;
// VCs whose anchor file is not the active buffer: surfaced as jump links rather
// than painted as marks in the wrong buffer.
let crossUnitVcs = [];
// Guards the change handler while a tab switch programmatically swaps buffers.
let suppressChange = false;
let workspaceInFlight = false;
let workspaceQueued = false;
let workspaceResultUnavailable = false;
// The built-in Demo workspace remains available, while curated workspaces are
// loaded from examples/index.json.  The id/meta pair keeps explorer highlights
// and backend expectations scoped to the workspace that produced the buffers.
let activeWorkspaceId = "demo";
let activeWorkspaceMeta = null;

// Compact proof state is the default. The persisted global preference controls
// the minimal goal + hypotheses + type surface versus the complete pane.
let compact = true;
// Product policy: unused proof facts are always de-emphasized. This is the
// single decision point now that the user-facing fade toggle is gone.
const FADE_UNUSED = true;
let backendSelection = null;
let backendOptions = [];
let backendSolverConfiguration = { z3: false, oxsmt: false };
let configuredDefaultBackend = null;
let paneSelectedVcId = null;
let paneOverlappingVcs = [];

// The status taxonomy (VC_STATUSES / normalizeStatus / FAILED_STATUSES /
// BADGE_HINT) now lives in pane_model.js, the shared pure model this file and
// the terminal tool both render from; they are globals from that script.

function setStatus(kind, message) {
  statusElement.className = "status-" + kind;
  statusElement.textContent = message;
}

function completedLatencyText() {
  return lastLatencyMs === null
    ? ""
    : " (" + Math.max(0, Math.round(lastLatencyMs)) + " ms)";
}

function applyBackendMetadata(payload, preferredBackend) {
  const advertised = payload && Array.isArray(payload.backend_options)
    ? payload.backend_options.filter((name) =>
        ["lean", "z3", "oxsmt", "cross"].includes(name)
      )
    : [];
  if (!advertised.length) return false;
  backendOptions = advertised;
  const solverConfiguration = payload && payload.backend_solver_configuration;
  if (solverConfiguration && typeof solverConfiguration === "object") {
    backendSolverConfiguration = {
      z3: solverConfiguration.z3 === true,
      oxsmt: solverConfiguration.oxsmt === true,
    };
  }
  const preferredIsUsable =
    preferredBackend === "lean" ||
    (preferredBackend === "z3" && backendSolverConfiguration.z3) ||
    (preferredBackend === "oxsmt" && backendSolverConfiguration.oxsmt) ||
    (preferredBackend === "cross" &&
      backendSolverConfiguration.z3 &&
      backendSolverConfiguration.oxsmt);
  if (
    preferredBackend &&
    backendOptions.includes(preferredBackend) &&
    preferredIsUsable
  ) {
    backendSelection = preferredBackend;
  }
  if (!backendOptions.includes(backendSelection)) {
    // This fallback is selected only from server-advertised capabilities. The
    // client never assumes Lean or oxsmt before configuration establishes it.
    backendSelection = backendOptions[0];
  }
  backendSelectElement.replaceChildren();
  backendOptions.forEach((name) => {
    const option = document.createElement("option");
    option.value = name;
    const label =
      name === "oxsmt" ? "oxsmt" : name[0].toUpperCase() + name.slice(1);
    const required = name === "cross" ? ["z3", "oxsmt"] : [name];
    const missing = required.filter(
      (solver) =>
        solver in backendSolverConfiguration &&
        !backendSolverConfiguration[solver]
    );
    option.textContent = missing.length
      ? label + " (configure " + missing.join(" + ") + ")"
      : label;
    if (missing.length) {
      option.title = "Solver command not configured: " + missing.join(", ");
      option.dataset.unconfigured = "true";
    }
    backendSelectElement.appendChild(option);
  });
  backendSelectElement.value = backendSelection;
  backendSelectElement.disabled = false;
  backendControlElement.hidden = false;
  return true;
}

async function loadBackendConfiguration() {
  try {
    const response = await fetch("/config");
    if (!response.ok) throw new Error(response.statusText);
    const payload = await response.json();
    configuredDefaultBackend = payload.default_backend;
    applyBackendMetadata(payload, payload.default_backend);
  } catch (error) {
    // No configuration means no established backend fact. Keep the fixed-width
    // selector blank/disabled; a later server response may still establish it.
    backendSelectElement.disabled = true;
  }
}

function renderBackendResults() {
  backendResultsElement.replaceChildren();
  backendResultsElement.hidden = true;
  if (backendSelection !== "cross") return;
  vcs.forEach((vc) => {
    if (!Array.isArray(vc.backends) || !vc.backends.length) return;
    const row = document.createElement("div");
    row.className = "backend-result";
    const statuses = vc.backends.map((result) => result.status);
    if (statuses.includes("proved") && statuses.includes("disproved")) {
      row.classList.add("backend-divergence");
      const warning = document.createElement("strong");
      warning.className = "backend-divergence-label";
      warning.textContent = "DIVERGENCE — proved and disproved";
      row.appendChild(warning);
    }
    const anchor = document.createElement("span");
    anchor.className = "backend-vc-anchor";
    anchor.textContent = "VC " + anchorText(vc);
    row.appendChild(anchor);
    vc.backends.forEach((result) => {
      const badge = document.createElement("span");
      badge.className = "backend-badge backend-" + result.status;
      badge.textContent = result.backend + ": " + statusLabel(result.status);
      if (result.detail) badge.title = result.detail;
      row.appendChild(badge);
    });
    backendResultsElement.appendChild(row);
  });
  backendResultsElement.hidden = backendResultsElement.children.length === 0;
}

const RETRYABLE_HTTP_STATUSES = new Set([502, 503, 504]);

function requestError(message, retryable, status) {
  const error = new Error(message);
  error.retryable = retryable;
  error.status = status;
  return error;
}

async function postJSON(path, body, signal) {
  let response;
  try {
    const options = {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(body),
    };
    // The non-browser textual harness has no AbortController. Its fallback
    // token still guards stale application, but is not a native AbortSignal and
    // must never be handed to native fetch.
    if (signal && typeof signal.addEventListener === "function") {
      options.signal = signal;
    }
    response = await fetch(path, options);
  } catch (error) {
    const wrapped = requestError(
      error && error.message ? error.message : "transport failure",
      !(error && error.name === "AbortError"),
      null
    );
    wrapped.cancelled = !!(error && error.name === "AbortError");
    throw wrapped;
  }
  const status = Number(response.status) || 0;
  const retryable = RETRYABLE_HTTP_STATUSES.has(status);
  let payload;
  try {
    payload = await response.json();
  } catch (error) {
    throw requestError("invalid server response", retryable, status);
  }
  if (!response.ok) {
    throw requestError(
      payload.error || response.statusText || "request failed",
      retryable,
      status
    );
  }
  return payload;
}

function esc(text) {
  return String(text).replace(
    /[&<>]/g,
    (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;" }[c])
  );
}

// Colorize a fragment of pane text with the SAME tokenizer the editor
// uses (vox-mode.js), so a goal / hypothesis row reads like the source.
// `refine` starts the tokenizer inside a refinement (predicate text).
function tokenPairs(text, refine) {
  if (typeof CodeMirror.voxTokenize !== "function")
    return [[String(text), null]];
  return CodeMirror.voxTokenize(
    String(text),
    refine ? { refine: 1 } : null
  );
}

function tok(text, refine) {
  return tokenPairs(text, refine)
    .map(([piece, cls]) =>
      cls
        ? '<span class="' +
          cls.split(" ").map((c) => "cm-" + c).join(" ") +
          '">' +
          esc(piece) +
          "</span>"
        : esc(piece)
    )
    .join("");
}

function appendHighlighted(element, text, refine) {
  tokenPairs(text, refine).forEach(([piece, cls]) => {
    if (!cls) {
      element.appendChild(document.createTextNode(piece));
      return;
    }
    const token = document.createElement("span");
    token.className = cls
      .split(" ")
      .map((name) => "cm-" + name)
      .join(" ");
    token.textContent = piece;
    element.appendChild(token);
  });
}

function renderHighlightedText(element, text) {
  element.replaceChildren();
  appendHighlighted(element, String(text), false);
}

function clearDiagnosticMarks() {
  diagnosticMarks.forEach((mark) => mark.clear());
  diagnosticMarks = [];
}

function clearInlineDiagnostic() {
  if (inlineDiagnosticMark) inlineDiagnosticMark.clear();
  inlineDiagnosticMark = null;
}

function cmPosition(location) {
  const lastLine = Math.max(0, cm.lineCount() - 1);
  const line = Math.max(0, Math.min(lastLine, Number(location.line) || 0));
  const length = cm.getLine(line).length;
  const ch = Math.max(0, Math.min(length, Number(location.col) || 0));
  return { line, ch };
}

function emittedSpanContext() {
  if (workspaceMode) {
    const linesByFile = {};
    Object.keys(workspaceBuffers).forEach((name) => {
      const source = name === activeFile ? cm.getValue() : workspaceBuffers[name];
      linesByFile[name] = String(source).split("\n");
    });
    return { linesByFile };
  }
  return { lines: cm.getValue().split("\n") };
}

function validatedRanges(ranges, context) {
  return (Array.isArray(ranges) ? ranges : [])
    .map((range) => {
      const span = validateEditorSpan(range, context);
      return span ? { ...range, file: span.file, start: span.start, end: span.end } : null;
    })
    .filter((range) => range !== null);
}

function diagnosticRange(error) {
  const span = validateEditorSpan(
    { start: error && error.start, end: error && error.end },
    { lines: cm.getValue().split("\n") }
  );
  if (!span) return null;
  let from = cmPosition(span.start);
  const anchor = from;
  let to = cmPosition(span.end);
  if (from.line === to.line && from.ch === to.ch) {
    const length = cm.getLine(from.line).length;
    if (from.ch < length) {
      to = { line: from.line, ch: from.ch + 1 };
    } else if (from.ch > 0) {
      from = { line: from.line, ch: from.ch - 1 };
    }
  }
  const point = from.line === to.line && from.ch === to.ch;
  return { from, to, point, anchor };
}

function rangesOverlap(left, right) {
  return contains(
    { start: { line: left.from.line, col: left.from.ch }, end: { line: left.to.line, col: left.to.ch } },
    right.from
  ) || contains(
    { start: { line: right.from.line, col: right.from.ch }, end: { line: right.to.line, col: right.to.ch } },
    left.from
  );
}

function hasMatchingVcDecoration(range) {
  return vcs.some((vc) => {
    const vcRange = {
      from: { line: vc.start.line, ch: vc.start.col },
      to: { line: vc.end.line, ch: vc.end.col },
    };
    return rangesOverlap(range, vcRange);
  });
}

function renderDiagnostics(errors) {
  clearDiagnosticMarks();
  clearInlineDiagnostic();
  currentErrors = errors.slice();
  diagnosticsElement.replaceChildren();
  // No "No parse or type errors." prose (spec cut): silence plus the STATUS
  // roll-up token (`✓ verified`) is the signal.  An empty list renders nothing.
  if (!errors.length) return;
  errors.forEach((error) => {
    const isVerify = error.kind === "verification";
    const isSourceError = ["syntax", "type-mode", "type"].includes(error.kind);
    const range = diagnosticRange(error);
    const needsVerifyFallback =
      isVerify && range && !hasMatchingVcDecoration(range);
    if (range) {
      if (range.point) {
        if (isSourceError || needsVerifyFallback) {
          const widget = document.createElement("span");
          widget.className = needsVerifyFallback
            ? "diagnostic-point diagnostic-point-verify-fallback"
            : "diagnostic-point";
          widget.textContent = needsVerifyFallback ? "!" : "^";
          widget.title = error.message;
          diagnosticMarks.push(
            cm.setBookmark(range.from, { widget, insertLeft: true })
          );
        }
      } else if (isSourceError || needsVerifyFallback) {
        // Parse/type errors get the wavy squiggle.  A VERIFICATION failure does
        // NOT: its editor underline is the status-aware vox1 vc-<status> mark
        // (solid red disproved / dashed red unproved) drawn by markVcs -- adding
        // a status-blind wavy line here would be a redundant second underline
        // and the wrong (vox1: no wavy for verification) style.  The failure
        // still appears in the STATUS diagnostics list below.
        diagnosticMarks.push(
          cm.markText(range.from, range.to, {
            className: needsVerifyFallback
              ? "diagnostic-verify-fallback"
              : "diagnostic-squiggle",
            title: error.message,
          })
        );
      }
    }
    // Only a diagnostic with a source target is a button. A spanless message is
    // static: it has nowhere honest to navigate.
    const item = document.createElement(range ? "button" : "div");
    if (range) item.type = "button";
    // A type/compile error is STATUS-level. Verification diagnostics remain in
    // STATUS too because their exact located compiler text is the one copy.
    item.className = isVerify
      ? "diagnostic-verify"
      : "diagnostic-error";
    if (!range) item.classList.add("diagnostic-static");
    const badge = document.createElement("span");
    badge.className = "diagnostic-badge";
    badge.textContent = isVerify
      ? "verify"
      : error.kind === "syntax"
        ? "syntax"
        : error.kind === "type-mode" || error.kind === "type"
          ? "type/mode"
          : "check";
    item.appendChild(badge);
    const location = error.start
      ? `Line ${error.start.line + 1}, column ${error.start.col + 1}: `
      : "";
    item.appendChild(document.createTextNode(location + error.message));
    if (range) {
      item.addEventListener("click", () => {
        cm.setCursor(range.anchor);
        cm.focus();
      });
    }
    diagnosticsElement.appendChild(item);
  });
  renderInlineDiagnostic();
}

function renderInlineDiagnostic() {
  clearInlineDiagnostic();
  if (!firstCheckDone) return;
  const cursor = cm.getCursor();
  const located = currentErrors
    .map((error) => ({ error, range: diagnosticRange(error) }))
    .filter(
      (entry) =>
        entry.range &&
        contains(
          {
            start: { line: entry.range.from.line, col: entry.range.from.ch },
            end: { line: entry.range.to.line, col: entry.range.to.ch },
          },
          cursor
        )
    )
    .sort((left, right) => {
      const leftRange = {
        start: { line: left.range.from.line, col: left.range.from.ch },
        end: { line: left.range.to.line, col: left.range.to.ch },
      };
      const rightRange = {
        start: { line: right.range.from.line, col: right.range.from.ch },
        end: { line: right.range.to.line, col: right.range.to.ch },
      };
      return spanSize(leftRange) - spanSize(rightRange);
    });
  let message = located.length ? located[0].error.message : null;
  if (!message) {
    const failed = vcs
      .filter(
        (vc) =>
          vc.status !== "proved" &&
          vc.detail &&
          contains({ start: vc.start, end: vc.end }, cursor)
      )
      .sort((left, right) => spanSize(left) - spanSize(right));
    if (failed.length) message = failed[0].detail;
  }
  if (!message) return;
  const widget = document.createElement("span");
  widget.className = "caret-inline-diagnostic";
  widget.textContent = String(message);
  widget.setAttribute("aria-hidden", "true");
  const line = cursor.line;
  inlineDiagnosticMark = cm.setBookmark(
    { line, ch: cm.getLine(line).length },
    { widget, insertLeft: false }
  );
}

// `contains` and `spanSize` (cursor/span geometry) now live in pane_model.js
// and are globals from that shared script.

function renderCursorType() {
  if (!firstCheckDone) {
    cursorTypeElement.replaceChildren();
    return;
  }
  const lines = cursorReadoutLines(
    expressionTypes,
    refinementTypes,
    identifierModes,
    cm.getCursor(),
    imposedTypes
  );
  cursorTypeElement.replaceChildren();
  lines.forEach((line, index) => {
    if (index) cursorTypeElement.appendChild(document.createTextNode("\n"));
    const row = document.createElement("span");
    row.className = "cursor-line cursor-" + line.kind;
    if (line.label) {
      const label = document.createElement("span");
      label.className = "cursor-label";
      label.textContent = line.label;
      row.appendChild(label);
    }
    if (line.kind === "empty") {
      row.appendChild(document.createTextNode(line.text));
    } else {
      appendHighlighted(row, line.text, false);
    }
    cursorTypeElement.appendChild(row);
  });
}

// ---------------------------------------------------------------------------
// Proof pane (skeleton) -- fed by /vcs through the single adapter below.
// ---------------------------------------------------------------------------

// The /vcs adapter (adaptPredicate / adaptHyp / adaptVcs) now lives in
// pane_model.js and is a global from that shared script -- the one integration
// point between the compiler's per-obligation dump and both the browser UI and
// the terminal tool.

function clearVcMarks() {
  vcMarks.forEach((mark) => mark.clear());
  vcMarks = [];
}

// Does obligation span `outer` STRICTLY contain span `inner` (contains it and
// is not the identical span)?  Endpoints are {line, col}; posCmp (shared model)
// orders them.  Two obligations that share the exact same span (e.g. the two
// branch VCs on a whole `if`, #144) do NOT contain each other, so both stay at
// the same nesting depth.
function spanStrictlyContains(outer, inner) {
  const startsAtOrBefore = posCmp(outer.start, inner.start) <= 0;
  const endsAtOrAfter = posCmp(outer.end, inner.end) >= 0;
  const strict =
    posCmp(outer.start, inner.start) < 0 || posCmp(outer.end, inner.end) > 0;
  return startsAtOrBefore && endsAtOrAfter && strict;
}

// Per-VC colored source underlines, one class per status (vc-<status>),
// shared with the legend swatches so the two cannot drift, PLUS a per-depth
// wash class (vc-goal-d0..d3) whose opacity deepens with nesting so a nested
// obligation reads as more opaque than its encloser.
function markVcs() {
  clearVcMarks();
  // Each obligation's nesting depth = how many OTHER obligation spans strictly
  // contain it.  This drives the wash opacity EXPLICITLY (rather than relying on
  // CodeMirror to composite overlapping translucent marks, which it does not),
  // so depth is readable regardless of how CM splits the mark spans.  Clamp at 3
  // (d0..d3); deeper nestings all read as the most opaque.
  vcs.forEach((vc) => {
    // Clamp to buffer bounds (cmPosition): a span that outruns the current
    // buffer -- e.g. a /vcs response the edit debounce has not yet caught up
    // to -- must not throw out of markText.
    const from = cmPosition(vc.start);
    const to = cmPosition(vc.end);
    // Count DISTINCT enclosing span geometries, not obligations: two
    // obligations that share one span (e.g. the branch pair on a whole `if`,
    // #144) are one visual enclosing region = one nesting level, so the inner
    // goal reads as d1, not d2.
    const containers = new Set();
    vcs.forEach((other) => {
      if (other !== vc && spanStrictlyContains(other, vc)) {
        containers.add(
          other.start.line + ":" + other.start.col + "-" +
            other.end.line + ":" + other.end.col
        );
      }
    });
    const depthClass = "vc-goal-d" + Math.min(containers.size, 3);
    vcMarks.push(
      cm.markText(from, to, {
        className: "vc-" + vc.status + " " + depthClass,
        title: BADGE_HINT[vc.status] || vc.status,
      })
    );
  });
}

// Move the caret to a hypothesis's source span and briefly highlight it, so
// clicking a hypothesis in the pane shows where it entered scope.  Reuses the
// same coordinate machinery as the diagnostic and per-VC marks.  In a
// multi-file workspace the span may live in another unit: switch to that unit's
// tab first, then highlight there (painting a mark in the wrong buffer would be
// meaningless).  The tab switch re-runs the check for the new active file.
function jumpToSpan(span) {
  if (workspaceMode && span.file && span.file !== activeFile) {
    switchTab(span.file);
  }
  const from = cmPosition(span.start);
  const to = cmPosition(span.end);
  cm.setCursor(from);
  cm.scrollIntoView({ from, to }, 60);
  cm.focus();
  const flash = cm.markText(from, to, { className: "hyp-flash" });
  window.setTimeout(() => flash.clear(), 1200);
}

// ---------------------------------------------------------------------------
// Provenance hover correlation (vox1-style), BROWSER-ONLY interactive chrome.
// Two directions, both painting no text and never entering the shared model, so
// the anti-drift lock (tool == browser text) is untouched:
//   pane row -> editor : hovering a goal / hypothesis row paints its SOURCE SPAN
//     in the editor (a transient `.prov-hl` mark).
//   editor -> pane     : moving the pointer over a shown span in the editor
//     highlights the matching pane row (`.prov-active`), scoped to the
//     obligation currently in the pane (the only one whose rows are visible).
// This is the affordance that replaces the compact hypothesis label: the label
// is dropped from the compact row and "which variable is this?" is one hover
// away instead.
// ---------------------------------------------------------------------------

let hoverMark = null; // transient editor mark for the hovered pane row
let hoverRow = null; // the pane row the pointer is currently over
let paneRowHl = null; // the pane row highlighted from an editor hover

function clearHoverMark() {
  if (hoverMark) {
    hoverMark.clear();
    hoverMark = null;
  }
}

// Paint a source span in the editor (the pane-row -> editor direction).  A
// cross-unit span belongs to another buffer, so it is painted only when it lies
// in the active file.
function paintHoverSpan(span) {
  clearHoverMark();
  if (!span || !span.start || !span.end) return;
  if (workspaceMode && span.file && span.file !== activeFile) return;
  hoverMark = cm.markText(cmPosition(span.start), cmPosition(span.end), {
    className: "prov-hl",
  });
}

function clearPaneRowHighlight() {
  if (paneRowHl) {
    paneRowHl.classList.remove("prov-active");
    paneRowHl = null;
  }
}

function setPaneRowHighlight(el) {
  if (paneRowHl === el) return;
  clearPaneRowHighlight();
  if (el) {
    el.classList.add("prov-active");
    paneRowHl = el;
  }
}

// editor -> pane: map the pointer to a document position and highlight the
// smallest shown span containing it (a hypothesis nested in the goal wins,
// matching the pane's innermost rule).  Scoped to paneVc -- the obligation whose
// rows are on screen.
function editorHoverToPane(event) {
  if (!paneVc) return clearPaneRowHighlight();
  const pos = cm.coordsChar({ left: event.clientX, top: event.clientY });
  if (!pos) return clearPaneRowHighlight();
  const cur = { line: pos.line, ch: pos.ch };
  let best = null;
  let bestSize = Infinity;
  const consider = (span, el) => {
    if (!el || !span || !span.start || !span.end) return;
    if (workspaceMode && span.file && span.file !== activeFile) return;
    const range = { start: span.start, end: span.end };
    if (!contains(range, cur)) return;
    const size = spanSize(range);
    if (size < bestSize) {
      bestSize = size;
      best = el;
    }
  };
  if (paneVc.start && paneVc.end) {
    consider(
      { start: paneVc.start, end: paneVc.end },
      paneBodyElement.querySelector(".goal")
    );
  }
  (paneVc.hypotheses || []).forEach((h, i) => {
    if (h && h.span) {
      consider(h.span, paneBodyElement.querySelector('[data-hyp="' + i + '"]'));
    }
  });
  setPaneRowHighlight(best);
}

// The self-documenting verdict legend: shown only while some obligation
// failed (exactly when the red underlines need explaining).  Each swatch
// reuses a vc-* marker class, so legend and underlines can never diverge.
function renderLegend() {
  // The entry set (which swatches, in what order, and when to show them) is
  // decided by the shared model so the terminal tool's legend cannot drift
  // from the browser's; this builds the DOM from that decision.
  const legend = legendModel(vcs);
  if (!legend.visible) {
    legendElement.hidden = true;
    legendElement.replaceChildren();
    return;
  }
  // Collapsed into a disclosure by default: the full verdict key is a reference,
  // not something to dump above every pane.  The <summary> ("verdict key") is
  // presentation chrome -- the swatches are the shared surface, so the terminal
  // mirror (which lists them) and this pane cannot drift.
  legendElement.replaceChildren();
  const details = document.createElement("details");
  details.className = "legend-details";
  const summary = document.createElement("summary");
  summary.className = "legend-summary";
  summary.textContent = "verdict key";
  details.appendChild(summary);
  legend.entries.forEach(([status, label]) => {
    const swatch = document.createElement("span");
    swatch.className = "leg vc-" + status;
    swatch.textContent = label;
    details.appendChild(swatch);
  });
  legendElement.appendChild(details);
  legendElement.hidden = false;
}

// `hypLabel`, `STATUS_NOTE`, `posCol`, and `posCmp` now live in pane_model.js
// (the shared model builds the labels and the status notes into the view-model);
// they remain globals from that script for any residual reference.

// `stateAtCursor` -- the approximate off-obligation "known here" derivation,
// with its load-bearing honesty invariant (only NAMED facts, so branch
// conditions never leak) -- now lives in pane_model.js and feeds the view-model
// directly; the browser renders the rows it produces below.

// The off-obligation "known at this point" view.  Depth 0 is the grey CONTEXT
// token with its pinned `· approximate` qualifier welded on -- NOT a verdict,
// and its approximate nature can never be dropped (honesty).  Depth 1 (full
// only) adds the named facts and the full verbatim caveat.  `rows` are the
// context rows from the shared view-model; `full` mirrors paneBodyLines' gating.
function renderStateAtCursor(rows, full) {
  // Off an obligation, COMPACT shows NOTHING in the PROOF zone (honest: with no
  // facts shown there is nothing to caveat).  FULL shows the grey CONTEXT token
  // (not a verdict), the approximate facts, and the verbatim caveat riding with
  // them -- all full-only, matching paneBodyLines' context branch.
  if (!full) return "";
  return (
    '<div class="verdict-token verdict-token-context">' +
    '<span class="tok tok-context">' +
    esc(CONTEXT_TOKEN_TEXT) +
    "</span></div>" +
    renderContext(rows) +
    '<div class="context-note">Approximate: facts introduced textually above the ' +
    "cursor, derived from nearby obligations. Branch conditions are omitted, and " +
    "a binding introduced inside a branch or other nested scope may still appear " +
    "below that scope where it is no longer in scope. Treat this as a hint, not a " +
    "guarantee of what holds here.</div>"
  );
}

// A grind witness is a model over Lean's UNBOUNDED Int, so it is a candidate,
// not a validated runtime counterexample: a value it assigns need not fit a
// machine int, and a genuine overflow refutation would never be found under
// this model.  It is labelled as such (the design's data contract), never as an
// established fact about runtime values.
function renderWitness(counterexample) {
  return (
    '<div class="cex">goal is false when (candidate; under Lean\'s unbounded-Int ' +
    "model, may not be a valid machine int):\n" +
    tok(counterexample.join("\n"), true) +
    "</div>"
  );
}

// The context: one named hypothesis per line, above the turnstile, like a
// prover's proof state.  A hypothesis whose origin span is known is a link
// (data-hyp drives the delegated click handler) that jumps to its source.
// `rows` are the shared view-model's context rows ({ label, display, faded,
// linked }); the fade / link / label decisions are already made in the model,
// so this is pure DOM construction (the row index is the data-hyp index the
// click handler resolves against paneVc.hypotheses).
function renderContext(rows, showLabels) {
  if (!rows.length) return "";
  if (showLabels === undefined) showLabels = true;
  const html = rows
    .map((r, i) => {
      const cls =
        "hyprow" + (r.faded ? " hyp-unused" : "") + (r.linked ? " hyp-link" : "");
      const attr = r.linked ? ' data-hyp="' + i + '"' : "";
      const title = r.linked
        ? ' title="jump to source"'
        : r.faded
        ? ' title="unused in this proof"'
        : "";
      // COMPACT drops the label (the #157 binder / kind label): the bare
      // predicate only.  The row stays hover-linked to its source span, so the
      // "which variable is this?" answer is one hover away -- honest (the
      // predicate is still shown), just minimal.  FULL keeps `name : predicate`.
      const label = showLabels
        ? '<span class="hyp-name">' +
          esc(r.label) +
          "</span>" +
          '<span class="hyp-sep"> : </span>'
        : "";
      return (
        '<div class="' + cls + '"' + attr + title + ">" +
        label +
        '<span class="hyp-pred">' + tok(r.display, true) + "</span>" +
        "</div>"
      );
    })
    .join("");
  return '<div class="context">' + html + "</div>";
}

// Build the obligation view's DOM from the shared view-model's `obligation`
// sub-model (`ob`), following the depth model exactly as paneBodyLines does:
// depth 0 (token + goal + anchor) always; depth 1 (kind / hypotheses / notes /
// refutation / sibling count) only in full view; depth 2 (raw predicate /
// solver detail / generated Lean) always, as one-click disclosures.  Keeping
// the same gating here as in the model is what lets the terminal tool mirror
// this pane byte-for-byte (tests/test_pane_fidelity.js).  `full` === !compact.
function renderVc(ob, full) {
  // --- depth 0: the goal line, which CARRIES the verdict ----------------
  // The whole line is tinted by verdict (goal-<status>) and a status glyph sits
  // at the END (right) of the line; a disproved goal keeps its welded honesty
  // qualifier (`· no witness` / `· witness`) with the goal, glyph to its right.
  // No separate loud token, no underline swatch: colour + glyph + qualifier
  // convey the verdict subtly and colour-blind-safe (the glyph distinguishes
  // proved ✓ / disproved ✗ / unproved ⚠, never hue alone).  The goal predicate
  // keeps its syntax highlighting (the inner tok spans).
  let html =
    '<div class="goal goal-' +
    ob.status +
    '"><span class="goal-turn tok-' +
    ob.status +
    '">⊢ </span>' +
    tok(ob.goalDisplay, true) +
    (ob.goalQualifier
      ? '<span class="goal-qual tok-' +
        ob.status +
        '"> · ' +
        esc(ob.goalQualifier) +
        "</span>"
      : "") +
    '<span class="goal-mark tok-' +
    ob.status +
    '">  ' +
    esc(ob.goalGlyph) +
    "</span></div>";
  // Full only: the code anchor + kind tag sit between the goal and the
  // hypotheses, so compact stays goal / hypotheses (vox1's rule).
  if (full) {
    html += '<div class="pane-anchor">' + esc(ob.anchor) + "</div>";
    if (ob.kindLabel) {
      html += '<div class="vc-kind">' + esc(ob.kindLabel) + " obligation</div>";
    }
  }
  // The hypotheses: unused ones faded (vox1).  Depth 0 -- right after the goal.
  // FULL labels each row `name : predicate`; COMPACT shows the bare predicate
  // (the label is a hover away -- see the provenance-hover wiring below).
  html += renderContext(ob.context, full);
  // --- depth 1: the rest of the proof state + escape hatches (full only) --
  if (full) {
    // The verdict spelled out: disproved gets its refutation section below;
    // every other non-proved status gets a note so "unproved" is never skimmed
    // as "disproved" (an unproved goal may still hold).
    if (ob.statusNote) {
      html +=
        '<div class="status-note status-note-' +
        ob.status +
        '">' +
        esc(ob.statusNote) +
        "</div>";
    }
    // Honest refutation surfacing.  The no-witness FACT already rides the token
    // at depth 0; this is the expanded reason / concrete model shown in full.
    if (ob.counterexample) {
      html += "<h3>" + ob.counterexample.heading + "</h3>";
      if (ob.counterexample.witness) {
        html += renderWitness(ob.counterexample.witness);
      } else {
        html += '<div class="cex-none">' + ob.counterexample.noneText + "</div>";
      }
    }
    // Sibling obligations at overlapping spans are discoverable as a count.
    if (ob.overlapping > 0) {
      html +=
        '<div class="also-here">+' +
        ob.overlapping +
        " more obligation" +
        (ob.overlapping > 1 ? "s" : "") +
        " here</div>";
    }
    // The escape-hatch disclosures are FULL ONLY (reachable by unchecking
    // compact); each still renders as a collapsed <details>.
    // The instantiated app-syntax, shown only when it adds something over the
    // pretty display (schema v2).
    if (ob.rawPredicate) {
    html +=
      '<details class="raw"><summary>raw predicate</summary><pre>' +
      esc(ob.rawPredicate) +
      "</pre></details>";
  }
  // The raw solver diagnostic (verbatim), collapsed by default.  The <summary>
  // serializes to "[solver detail]" (the model's summary label).
  if (ob.solverDetail) {
    html +=
      '<details class="solver-detail"><summary>solver detail</summary>' +
      '<pre class="detail">' +
      esc(ob.solverDetail) +
      "</pre></details>";
  }
  // The positive theorem the compiler handed the solver, behind a disclosure.
  // A complete, self-contained Lean file (the honesty escape hatch): copy /
  // download / open it and run the exact check in a real Lean.  The buttons are
  // wired by the delegated handler below (they read the shown obligation).
  if (ob.lean) {
    html +=
      '<details class="lean"><summary>generated Lean</summary>' +
      '<div class="lean-actions">' +
      '<button type="button" class="lean-btn" data-lean="copy">copy</button>' +
      '<button type="button" class="lean-btn" data-lean="download">download .lean</button>' +
      '<button type="button" class="lean-btn" data-lean="open">open in new tab</button>' +
      "</div>" +
      '<p class="lean-hint">The exact, self-contained theorem the compiler ' +
      "hands Lean -- run it in any Lean to reproduce this check.</p>" +
      "<pre>" +
      esc(ob.lean) +
      "</pre></details>";
    }
  }
  return html;
}

// Full view puts proof detail in #pane-body.  This sibling container owns only
// the overlap-cycling control; compact must not grow a second detail surface
// beside its fidelity-locked goal + hypotheses body.
function renderProofDetails(vm) {
  proofDetailsElement.replaceChildren();
  if (!vm || compact || vm.mode !== "obligation") return;
  const ob = vm.obligation;
  const html = ob.overlapCount > 1
    ? '<button type="button" class="overlap-control" data-overlap-cycle ' +
      'data-overlap-label="' +
      (ob.overlapIndex + 1) +
      "/" +
      ob.overlapCount +
      '" aria-live="polite" aria-label="Obligation ' +
      (ob.overlapIndex + 1) +
      " of " +
      ob.overlapCount +
      '; show next obligation"></button>'
    : "";
  proofDetailsElement.innerHTML = html;
}

function renderProofPane() {
  // The mode, the chosen obligation, the state-at-cursor facts, the sibling
  // count, and the placeholder text are ALL decided by the shared model, so the
  // terminal tool shows precisely this pane.  This function only sets the
  // interaction state (paneVc, for the delegated hypothesis / Lean handlers)
  // and builds the DOM from the returned view-model.
  // The pane is about to be rebuilt: drop any provenance-hover state pointing at
  // the outgoing DOM (a highlighted row that no longer exists, or an editor mark
  // for it).
  clearHoverMark();
  clearPaneRowHighlight();
  hoverRow = null;
  if (!firstCheckDone) {
    paneVc = null;
    paneOverlappingVcs = [];
    paneModeElement.textContent = "";
    paneBodyElement.replaceChildren();
    proofDetailsElement.replaceChildren();
    return;
  }
  const vm = proofPaneModel(vcs, cm.getCursor(), {
    compact,
    fadeUnused: FADE_UNUSED,
    unavailable: vcsUnavailable,
    unavailableReason: vcsUnavailableReason,
    hidden: vcsHidden,
    selectedVcId: paneSelectedVcId,
  });
  // The old "mode:" header line is gone (spec cut) -- the loud verdict token
  // carries the mode now; pane-mode is always empty.
  paneModeElement.textContent = vm.paneMode;
  const full = !compact;
  paneOverlappingVcs = vm.overlappingVcs || [];
  if (vm.mode === "obligation") {
    paneVc = vm.vc;
    paneSelectedVcId = vm.vc.id;
    paneBodyElement.innerHTML = renderVc(vm.obligation, full) + hiddenNote(full);
    renderProofDetails(vm);
    return;
  }
  paneSelectedVcId = null;
  renderProofDetails(vm);
  // Off every obligation: paneVc is a synthetic holder in the context case
  // purely so the delegated hypothesis-click handler can resolve a clicked
  // context row to its source span, exactly as it does for an obligation.
  paneVc = vm.mode === "context" ? { hypotheses: vm.facts } : null;
  if (vm.mode === "context") {
    paneBodyElement.innerHTML =
      renderStateAtCursor(vm.contextFacts, full) + hiddenNote(full);
    return;
  }
  // "unavailable" (the check did not complete) and "empty" (no obligations, or
  // caret off every mark) both render a placeholder the model selected.
  paneBodyElement.innerHTML =
    '<p class="placeholder">' + esc(vm.placeholder) + "</p>" + hiddenNote(full);
}

// Obligations the dump reported but that have no placeable source span: noted
// so the visible count never silently understates the real obligation count.
function hiddenNote(full) {
  if (!full || !vcsHidden) return "";
  return (
    '<div class="also-here">' +
    vcsHidden +
    " obligation" +
    (vcsHidden > 1 ? "s" : "") +
    " with no source location (not shown)</div>"
  );
}

// The STATUS zone verdict detail (`#verify-output`, depth 1): the header pill
// (`#status`) carries the pass/fail verdict now, so the success SENTENCE is cut
// (spec) -- on success this stays hidden.  A failure keeps its located message
// here as a depth-1 detail line.
function renderVerification(verification, errors) {
  const status = verification ? verification.status : null;
  const message = (verification && verification.message) || "";
  const hasVerificationDiagnostic = (errors || []).some(
    (error) => error.kind === "verification"
  );
  if (status === "failed" && message && !hasVerificationDiagnostic) {
    verificationDetailsElement.hidden = false;
    verifyElement.className = "verify-fail";
    verifyElement.textContent = message;
  } else {
    verificationDetailsElement.hidden = true;
    verificationDetailsElement.open = false;
    verifyElement.className = "muted";
    verifyElement.textContent = "";
  }
}

// The fail-closed buffer verdict, shown in the TOP HEADER pill (`#status`): a
// pure function of the obligations plus the compile outcome, computed by the
// shared model (statusRollup).  `✓ verified · N/N` appears ONLY when the buffer
// compiles AND every obligation proved; a compile error reads `✗ type error`
// with the obligations `unavailable`, never proved.  The header label already
// carries the count (`verified · 2/2`), so the STATUS zone needs no separate
// verdict line.
function renderStatusVerdict() {
  // Honesty: never assert a fail-closed verdict before the first COMPLETED
  // check.  Until then the header keeps its pending "checking…" state (set by
  // clearResults); no partial result is allowed to replace it.
  if (!firstCheckDone) return;
  const rollup = statusRollup(vcs, {
    compiles: lastCompiles,
    errorCount: lastErrorCount,
    unavailable: vcsUnavailable,
    outcome: lastOutcome,
    obligationSummary,
  });
  statusElement.className = "status-" + rollup.status;
  statusElement.textContent =
    rollup.glyph + " " + rollup.label + completedLatencyText();
}

function applyCheck(response) {
  appliedRevision = response.revision;
  // A completed check has landed: the STATUS header may now show a verdict.
  firstCheckDone = true;
  expressionTypes = validatedRanges(response.types, {
    lines: cm.getValue().split("\n"),
  });
  lastVerification = response.verification || null;
  lastOutcome = response.outcome || null;
  // Only parse/type errors count toward the "type error" roll-up; a verification
  // failure leaves `ok` false too, but it is a disproved/unproved OBLIGATION,
  // not a compile error -- so "compiles" keys off the type-error count, never
  // `ok` directly.
  lastErrorCount = (response.errors || []).filter((e) =>
    ["syntax", "type-mode", "type"].includes(e.kind)
  ).length;
  lastCompiles =
    lastErrorCount === 0 &&
    (!lastOutcome || ["ok", "verification", "backend-unavailable"].includes(lastOutcome.kind));
  renderDiagnostics(response.errors || []);
  renderSignatureState(response.signature, response.outcome);
  renderVerification(response.verification, response.errors || []);
  // The header pill IS the fail-closed verdict now (glyph + count); there is no
  // separate generic "typechecks ✓" message to set.
  renderStatusVerdict();
  renderCursorType();
}

// Apply every result channel from one completed /check response.  The compiler
// emits its VC dump during the same authoritative -c pass as diagnostics and
// .annot types, so marks, proof rows, cursor data, and the header verdict all
// advance atomically to one revision/backend.
function applyUnifiedCheck(response, elapsed) {
  applyBackendMetadata(response, response.backend);
  // During the rollout, a legacy/custom adapter can omit the additive VC
  // fields.  Preserve an already-arrived compatibility /vcs view in that case;
  // after a normal edit clearResults leaves the proof state unavailable, so no
  // old obligation can leak through.
  if (Array.isArray(response.vcs)) {
    const spanContext = emittedSpanContext();
    const adapted = adaptVcs(response, spanContext);
    vcs = adapted.vcs;
    vcsUnavailable = adapted.unavailable;
    vcsUnavailableReason = adapted.unavailableReason;
    vcsHidden = adapted.hidden;
    obligationSummary = adapted.summary;
    refinementTypes = validatedRanges(response.refinement_types, spanContext);
    identifierModes = validatedRanges(response.identifier_modes, spanContext);
    imposedTypes = validatedRanges(response.imposed_types, spanContext);
  }
  applyCheck(response);
  markVcs();
  renderLegend();
  renderBackendResults();
  applyPendingCursor();
  renderProofPane();
  renderCursorType();
  lastLatencyMs = elapsed;
  renderStatusVerdict();
}

function renderSignatureState(signature, outcome) {
  const channel = typeof signature === "string"
    ? {
        status: signature ? "available" : "empty",
        text: signature,
        error: "",
      }
    : signature && typeof signature === "object"
      ? signature
      : null;
  if (channel && channel.status === "available") {
    renderHighlightedText(signatureElement, channel.text || "");
  } else if (channel && channel.status === "empty") {
    renderHighlightedText(signatureElement, "(empty signature)");
  } else if (channel && channel.status === "interface") {
    renderHighlightedText(signatureElement, "(interface — this file is the signature)");
  } else if (channel && channel.status === "unavailable") {
    renderHighlightedText(
      signatureElement,
      "Unavailable: " + (channel.error || "signature inference failed")
    );
  } else if (!outcome || outcome.kind === "ok") {
    renderHighlightedText(signatureElement, "loading…");
  } else {
    renderHighlightedText(signatureElement, "Unavailable for this check.");
  }
}

// A /vcs response is stale once the buffer has moved on: a later edit (the
// revision changed), a file switch (the path changed), or a switch to a
// read-only doc (which owns the pane placeholder and must not be painted
// over, and whose editor must not be focused).
function vcsSuperseded(path, revision) {
  return (
    path !== currentPath ||
    revision !== documentRevision ||
    cm.getOption("readOnly")
  );
}

// Fetch the real per-obligation dump for the current buffer, then relay it to
// the marks, the legend, and the proof pane.  Single-flighted like runCheck: a
// request arriving while one is in flight is coalesced into one trailing run,
// so out-of-order responses cannot paint stale marks.
async function refreshVcs() {
  // A read-only doc is open: it owns the pane placeholder and has no buffer to
  // check, so never even fetch /vcs for it (ux2 doc-mode guard).
  if (docOpen) return;
  // Single-flight: coalesce a request arriving mid-flight into one trailing
  // run so out-of-order responses cannot paint stale marks.
  if (vcsInFlight) {
    vcsQueued = true;
    return;
  }
  vcsInFlight = true;
  const path = currentPath;
  const revision = documentRevision;
  try {
    // A request that fails to reach the server is itself "data unavailable".
    let adapted = { vcs: [], unavailable: true, hidden: 0 };
    // Refinement-predicate subterm types ride the same /vcs dump; fail closed
    // to none on any error/supersession so stale predicate types never linger.
    let refinementRanges = [];
    let modeRanges = [];
    let imposedRanges = [];
    try {
      const payload = await postJSON("/vcs", {
        source: cm.getValue(),
        revision,
        path,
        backend: backendSelection,
      });
      // Drop a response the buffer has moved past (by revision, not path).
      if (payload.revision !== documentRevision || vcsSuperseded(path, revision)) {
        return;
      }
      const spanContext = emittedSpanContext();
      adapted = adaptVcs(payload, spanContext);
      refinementRanges = validatedRanges(payload.refinement_types, spanContext);
      modeRanges = validatedRanges(payload.identifier_modes, spanContext);
      imposedRanges = validatedRanges(payload.imposed_types, spanContext);
      applyBackendMetadata(payload, payload.backend);
    } catch (error) {
      // A failed run marks the pane "unavailable" -- but only if we are still
      // on that buffer; a run that failed across a switch leaves the new
      // buffer (or doc placeholder) untouched.
      if (vcsSuperseded(path, revision)) return;
    }
    vcs = adapted.vcs;
    vcsUnavailable = adapted.unavailable;
    vcsUnavailableReason = adapted.unavailableReason;
    vcsHidden = adapted.hidden;
    obligationSummary = adapted.summary;
    refinementTypes = refinementRanges;
    identifierModes = modeRanges;
    imposedTypes = imposedRanges;
    markVcs();
    renderLegend();
    renderBackendResults();
    applyPendingCursor();
    renderProofPane();
    // The refinement subterm types just changed; refresh the CURSOR readout so
    // a caret already inside a predicate picks them up without a further move.
    renderCursorType();
    // The obligation counts just changed; refresh the STATUS roll-up token
    // (folding in the last compile outcome from /check).
    renderStatusVerdict();
  } finally {
    vcsInFlight = false;
    if (vcsQueued) {
      vcsQueued = false;
      void refreshVcs();
    }
  }
}

function setCursorProgrammatically(position) {
  suppressCursorInteraction = true;
  try {
    cm.setCursor(position);
  } finally {
    suppressCursorInteraction = false;
  }
}

function cancelPendingCursor() {
  cursorInteractionToken += 1;
  pendingCursor = null;
}

function cancelTransportRetries() {
  retryGeneration += 1;
  retryAttempt = 0;
  if (retryTimer !== null) {
    window.clearTimeout(retryTimer);
    retryTimer = null;
  }
}

function invalidateWorkspaceResults(unavailable) {
  if (!workspaceMode) return;
  lastWorkspacePayload = null;
  workspaceResultUnavailable = unavailable;
  crossUnitVcs = [];
  crossUnitElement.hidden = true;
  crossUnitElement.replaceChildren();
  renderTabs();
}

const RETRY_DELAYS = [250, 750, 1500];

function retryTransport(generation, isCurrent, callback) {
  if (generation !== retryGeneration || !isCurrent()) return;
  clearResults();
  invalidateWorkspaceResults(false);
  if (retryAttempt >= RETRY_DELAYS.length) {
    invalidateWorkspaceResults(true);
    setStatus("unavailable", "⚠ unavailable · server");
    return;
  }
  setStatus("unavailable", "server unavailable · retrying…");
  const delay = RETRY_DELAYS[retryAttempt];
  retryAttempt += 1;
  retryTimer = window.setTimeout(() => {
    retryTimer = null;
    if (generation === retryGeneration && isCurrent()) void callback();
  }, delay);
}

function handleRequestError(error, generation, isCurrent, callback) {
  if (generation !== retryGeneration || !isCurrent()) return;
  if (error && error.cancelled) return;
  if (error && error.retryable === true) {
    retryTransport(generation, isCurrent, callback);
    return;
  }
  clearResults();
  invalidateWorkspaceResults(true);
  retryAttempt = 0;
  const rejected =
    error && Number(error.status) >= 400 && Number(error.status) < 500;
  setStatus(
    "unavailable",
    rejected ? "⚠ unavailable · request rejected" : "⚠ unavailable · server"
  );
}

function beginAuthoritativeRequest() {
  if (debounceTimer !== null) {
    window.clearTimeout(debounceTimer);
    debounceTimer = null;
  }
  checkQueued = false;
  if (authoritativeController) authoritativeController.abort();
  if (signatureController) {
    signatureController.abort();
    signatureController = null;
  }
  authoritativeController = newRequestController();
  return authoritativeController;
}

function newRequestController() {
  if (typeof AbortController !== "undefined") return new AbortController();
  const signal = { aborted: false };
  return {
    signal,
    abort() {
      signal.aborted = true;
    },
  };
}

function cancelActiveRequests() {
  if (debounceTimer !== null) {
    window.clearTimeout(debounceTimer);
    debounceTimer = null;
  }
  checkQueued = false;
  if (authoritativeController) authoritativeController.abort();
  if (signatureController) signatureController.abort();
  authoritativeController = null;
  signatureController = null;
}

function finishAuthoritativeRequest(controller) {
  if (authoritativeController !== controller) return;
  authoritativeController = null;
  if (checkQueued) {
    checkQueued = false;
    scheduleCheck(0);
  }
}

async function refreshSignature(snapshot) {
  if (snapshot.outcome !== "ok") return;
  if (signatureController) signatureController.abort();
  const controller = newRequestController();
  signatureController = controller;
  const endpoint = snapshot.workspace ? "/workspace-signature" : "/signature";
  const body = snapshot.workspace
    ? {
        revision: snapshot.revision,
        active: snapshot.active,
        files: snapshot.files,
        backend: snapshot.backend,
      }
    : {
        revision: snapshot.revision,
        source: snapshot.source,
        backend: snapshot.backend,
      };
  try {
    const payload = await postJSON(endpoint, body, controller.signal);
    const current =
      signatureController === controller &&
      !controller.signal.aborted &&
      payload.revision === documentRevision &&
      snapshot.backend === backendSelection &&
      (snapshot.workspace
        ? workspaceMode && snapshot.active === activeFile
        : !workspaceMode && snapshot.path === currentPath && snapshot.source === cm.getValue());
    if (current) renderSignatureState(payload.signature, { kind: "ok" });
  } catch (error) {
    if (
      !(error && error.cancelled) &&
      signatureController === controller &&
      snapshot.revision === documentRevision
    ) {
      renderSignatureState(
        { status: "unavailable", text: "", error: "signature request failed" },
        { kind: "ok" }
      );
    }
  } finally {
    if (signatureController === controller) signatureController = null;
  }
}

// Refine an already-placed teaching line to a compiler-emitted VC column only
// while file/source/backend/focus interaction remain exactly untouched.
function applyPendingCursor() {
  if (pendingCursor === null) return;
  const pending = pendingCursor;
  if (
    pending.path !== currentPath ||
    pending.revision !== documentRevision ||
    pending.backend !== backendSelection ||
    pending.interaction !== cursorInteractionToken
  ) {
    pendingCursor = null;
    return;
  }
  const line = pending.line;
  pendingCursor = null;
  const onLine = vcs
    .filter((vc) => vc.start.line === line)
    .sort((a, b) => spanSize(a) - spanSize(b));
  const target = onLine[0];
  setCursorProgrammatically(
    target
      ? { line: target.start.line, ch: target.start.col }
      : { line, ch: 0 }
  );
}

async function runCheck() {
  if (docOpen) return;
  const controller = beginAuthoritativeRequest();
  const revision = documentRevision;
  const source = cm.getValue();
  const path = currentPath;
  const backend = backendSelection;
  const generation = retryGeneration;
  const startedAt = Date.now();
  try {
    const response = await postJSON("/check", {
      source,
      revision,
      backend,
    }, controller.signal);
    const isCurrent =
      generation === retryGeneration &&
      response.revision === documentRevision &&
      response.revision >= appliedRevision &&
      source === cm.getValue() &&
      path === currentPath &&
      backend === backendSelection &&
      authoritativeController === controller &&
      !controller.signal.aborted;
    if (isCurrent) {
      retryAttempt = 0;
      applyUnifiedCheck(response, Date.now() - startedAt);
      void refreshSignature({
        workspace: false,
        revision,
        source,
        path,
        backend,
        outcome: response.outcome ? response.outcome.kind : response.ok ? "ok" : "unknown",
      });
    }
  } catch (error) {
    handleRequestError(
      error,
      generation,
      () =>
        !docOpen &&
        !workspaceMode &&
        revision === documentRevision &&
        path === currentPath &&
        backend === backendSelection &&
        source === cm.getValue(),
      runCheck
    );
  } finally {
    finishAuthoritativeRequest(controller);
  }
}

const TYPING_CHECK_DELAY_MS = 50;

function scheduleCheck(delay = TYPING_CHECK_DELAY_MS) {
  if (docOpen) return;
  if (debounceTimer !== null) window.clearTimeout(debounceTimer);
  if (authoritativeController) {
    debounceTimer = null;
    checkQueued = true;
    return;
  }
  debounceTimer = window.setTimeout(() => {
    debounceTimer = null;
    // Both endpoints return diagnostics and VCs from one authoritative compile;
    // the workspace form additionally routes results by file.
    if (workspaceMode) {
      void runWorkspaceCheck();
    } else {
      void runCheck();
    }
  }, delay);
}

cm.on("change", () => {
  if (cm.getOption("readOnly")) return;
  // A programmatic buffer swap during a tab switch is not a user edit: the
  // switch handler drives its own re-check, so ignore the change event it
  // triggers (otherwise the active file's edits would be checked twice).
  if (suppressChange) return;
  cancelTransportRetries();
  if (signatureController) {
    signatureController.abort();
    signatureController = null;
  }
  cancelPendingCursor();
  documentRevision += 1;
  // In workspace mode, mirror the edit back into the active buffer's cell so a
  // later tab switch preserves it.
  if (workspaceMode && activeFile) {
    workspaceBuffers[activeFile] = cm.getValue();
  }
  // A changed buffer invalidates every visible result immediately.  Keeping
  // old squiggles/VCs while merely changing the header would let stale proof
  // state masquerade as current during a multi-second solver run.
  clearResults();
  crossUnitVcs = [];
  crossUnitElement.hidden = true;
  crossUnitElement.replaceChildren();
  if (workspaceMode) {
    lastWorkspacePayload = null;
    renderTabs();
  }
  scheduleCheck();
});
// Cursor moves re-render the proof pane and type line only (no network).
cm.on("cursorActivity", () => {
  if (!suppressCursorInteraction && pendingCursor !== null) {
    cancelPendingCursor();
  }
  paneSelectedVcId = null;
  renderCursorType();
  renderProofPane();
  renderInlineDiagnostic();
});
// One delegated handler for the pane: a clicked hypothesis row (rendered with
// data-hyp) jumps to that hypothesis's source span.  Attached once; survives
// the pane's innerHTML re-renders.
paneBodyElement.addEventListener("click", (event) => {
  const target = event.target;
  const row = target && target.closest ? target.closest(".hyp-link") : null;
  if (!row || !paneVc) return;
  const hyp = paneVc.hypotheses[Number(row.dataset.hyp)];
  if (hyp && hyp.span) jumpToSpan(hyp.span);
});
proofDetailsElement.addEventListener("click", (event) => {
  const target = event.target;
  const cycle =
    target && target.closest ? target.closest(".overlap-control") : null;
  if (!cycle || paneOverlappingVcs.length < 2 || !paneVc) return;
  const current = paneOverlappingVcs.findIndex((vc) => vc.id === paneVc.id);
  const next = paneOverlappingVcs[(current + 1) % paneOverlappingVcs.length];
  paneSelectedVcId = next.id;
  renderProofPane();
});

// pane row -> editor: hovering a goal / hypothesis row paints its source span in
// the editor.  Delegated (survives the pane's innerHTML re-renders), like the
// click handler above; deduped by the row under the pointer so moving within a
// row does not repaint.
paneBodyElement.addEventListener("mouseover", (event) => {
  const target = event.target;
  const row =
    target && target.closest
      ? target.closest(".hyp-link") || target.closest(".goal")
      : null;
  if (row === hoverRow) return;
  hoverRow = row;
  if (!row || !paneVc) {
    clearHoverMark();
    return;
  }
  if (row.classList.contains("goal")) {
    if (paneVc.start && paneVc.end) {
      paintHoverSpan({
        start: paneVc.start,
        end: paneVc.end,
        file: paneVc.file,
      });
    }
  } else {
    const hyp = paneVc.hypotheses[Number(row.dataset.hyp)];
    paintHoverSpan(hyp && hyp.span);
  }
});
paneBodyElement.addEventListener("mouseout", (event) => {
  const to = event.relatedTarget;
  // Ignore moves that stay within the same row (mouseout fires on child hops).
  if (
    hoverRow &&
    to &&
    to.closest &&
    (to.closest(".hyp-link") === hoverRow || to.closest(".goal") === hoverRow)
  ) {
    return;
  }
  hoverRow = null;
  clearHoverMark();
});

// editor -> pane: pointer over a shown span highlights the matching pane row.
// Guarded: the test sandboxes' CodeMirror stubs may lack a wrapper element.
const cmWrapper = cm.getWrapperElement && cm.getWrapperElement();
if (cmWrapper && cmWrapper.addEventListener) {
  cmWrapper.addEventListener("mousemove", editorHoverToPane);
  cmWrapper.addEventListener("mouseleave", clearPaneRowHighlight);
  cmWrapper.addEventListener("focusout", () => {
    if (pendingCursor !== null) cancelPendingCursor();
  });
}

// The generated Lean is a complete, self-contained file, so it can leave the
// pane unchanged: copy it to the clipboard, download it as a .lean, or open it
// in a new tab.  All three read the obligation currently in the pane (paneVc).
// Each affordance degrades quietly if its browser API is unavailable.
function leanFilename(vc) {
  return "vc_" + (vc && vc.id != null ? vc.id : 0) + ".lean";
}

function leanBlobUrl(text) {
  return URL.createObjectURL(new Blob([text], { type: "text/plain" }));
}

async function copyLean(text) {
  try {
    if (navigator.clipboard && navigator.clipboard.writeText) {
      await navigator.clipboard.writeText(text);
      return true;
    }
  } catch (e) {}
  return false;
}

function downloadLean(text, filename) {
  try {
    const url = leanBlobUrl(text);
    const anchor = document.createElement("a");
    anchor.href = url;
    anchor.download = filename;
    document.body.appendChild(anchor);
    anchor.click();
    document.body.removeChild(anchor);
    window.setTimeout(() => URL.revokeObjectURL(url), 0);
  } catch (e) {}
}

function openLean(text) {
  try {
    const url = leanBlobUrl(text);
    window.open(url, "_blank", "noopener");
    window.setTimeout(() => URL.revokeObjectURL(url), 30000);
  } catch (e) {}
}

function handleLeanAction(event) {
  const target = event.target;
  const btn = target && target.closest ? target.closest(".lean-btn") : null;
  if (!btn || !paneVc || !paneVc.lean) return;
  const action = btn.dataset.lean;
  if (action === "copy") {
    void copyLean(paneVc.lean).then((copied) => {
      btn.textContent = copied ? "copied" : "copy failed";
      window.setTimeout(() => {
        btn.textContent = "copy";
      }, 1200);
    });
  } else if (action === "download") {
    downloadLean(paneVc.lean, leanFilename(paneVc));
  } else if (action === "open") {
    openLean(paneVc.lean);
  }
}
paneBodyElement.addEventListener("click", handleLeanAction);
proofDetailsElement.addEventListener("click", handleLeanAction);

// ---------------------------------------------------------------------------
// Theme and compact controls (persisted).
// ---------------------------------------------------------------------------

const THEME_KEY = "voxide-theme";
const themeButton = document.getElementById("theme-button");

function currentTheme() {
  return document.documentElement.dataset.theme === "light" ? "light" : "dark";
}

function applyTheme(theme) {
  if (theme === "light") document.documentElement.dataset.theme = "light";
  else delete document.documentElement.dataset.theme;
  themeButton.textContent = theme === "light" ? "☾ Dark" : "☀ Light";
}

(function initTheme() {
  let saved = "dark";
  try {
    saved = localStorage.getItem(THEME_KEY) || "dark";
  } catch (e) {}
  applyTheme(saved);
})();

themeButton.addEventListener("click", () => {
  const next = currentTheme() === "light" ? "dark" : "light";
  try {
    localStorage.setItem(THEME_KEY, next);
  } catch (e) {}
  applyTheme(next);
});

const COMPACT_KEY = "voxide-compact";
const compactBox = document.getElementById("compact-box");

function applyCompact() {
  document.body.classList.toggle("compact-view", compact);
}

function setCompact(value) {
  compact = !!value;
  compactBox.checked = compact;
  applyCompact();
  renderProofPane();
}

(function initCompact() {
  try {
    compact = localStorage.getItem(COMPACT_KEY) !== "off";
  } catch (e) {}
  compactBox.checked = compact;
  applyCompact();
})();

compactBox.addEventListener("change", () => {
  compact = compactBox.checked;
  try {
    localStorage.setItem(COMPACT_KEY, compact ? "on" : "off");
  } catch (e) {}
  applyCompact();
  renderProofPane();
});

backendSelectElement.addEventListener("change", () => {
  const selected = backendSelectElement.value;
  if (!backendOptions.includes(selected) || selected === backendSelection) return;
  backendSelection = selected;
  cancelTransportRetries();
  cancelActiveRequests();
  cancelPendingCursor();
  // Invalidate every in-flight response from the previous backend even though
  // the source text itself did not change.
  documentRevision += 1;
  clearResults();
  applyBackendMetadata({
    backend_options: backendOptions,
    backend_solver_configuration: backendSolverConfiguration,
  });
  scheduleCheck(0);
});

// ---------------------------------------------------------------------------
// Read-only documentation viewer.
//
// A curated .md doc opens read-only: it is rendered (never compiled), the
// editor is hidden, and every stale result surface from the previous buffer
// is cleared so no diagnostics / proof state linger over the doc.
// ---------------------------------------------------------------------------

// Reset every output surface to a neutral placeholder and drop the cached
// obligations and marks.  Called on a doc open (they stay cleared) and on an
// example open (the scheduled check then repopulates them), so a switch never
// shows the previous file's diagnostics, signature, type, or proof state.
function clearResults() {
  clearDiagnosticMarks();
  clearVcMarks();
  vcs = [];
  // Pending means no trustworthy VC set exists yet.  This is never rendered as
  // a completed empty set; it keeps legacy adapters fail-closed too.
  vcsUnavailable = true;
  vcsUnavailableReason = "unknown";
  vcsHidden = 0;
  obligationSummary = summaryFromVcs([], 0);
  expressionTypes = [];
  refinementTypes = [];
  identifierModes = [];
  imposedTypes = [];
  lastVerification = null;
  lastCompiles = false;
  lastErrorCount = 0;
  lastOutcome = null;
  currentErrors = [];
  clearInlineDiagnostic();
  // Back to the pre-check state: the next completed check re-enables the verdict.
  firstCheckDone = false;
  lastLatencyMs = null;
  workspaceResultUnavailable = false;
  lastWorkspaceLayer = null;
  paneSelectedVcId = null;
  paneOverlappingVcs = [];
  legendElement.hidden = true;
  legendElement.replaceChildren();
  backendResultsElement.hidden = true;
  backendResultsElement.replaceChildren();
  diagnosticsElement.replaceChildren();
  setStatus("checking", "checking…");
  signatureElement.replaceChildren();
  cursorTypeElement.replaceChildren();
  verificationDetailsElement.hidden = true;
  verificationDetailsElement.open = false;
  verifyElement.className = "muted";
  verifyElement.textContent = "";
  paneModeElement.textContent = "";
  paneBodyElement.replaceChildren();
  proofDetailsElement.replaceChildren();
}

// An href is safe to follow to the open web only if it is http(s) or mailto;
// anything else (javascript:, data:, …) is dropped to plain text.
function externalHref(url) {
  return /^(https?:\/\/|mailto:)/i.test(String(url).trim())
    ? String(url).trim()
    : null;
}

// A link to a curated file ("docs/x.md", "examples/y.ml") opens that file in
// the explorer rather than navigating the page.  No "..", so it cannot point
// outside the allowlisted roots the server already guards.
function internalPath(url) {
  const dest = String(url).trim();
  return /^(docs|examples)\/[A-Za-z0-9_.\/-]+$/.test(dest) &&
    !dest.includes("..")
    ? dest
    : null;
}

// Build the DOM node for one matched inline construct and append it to
// `parent`.  Emphasis recurses into its own content (depth bounded by nesting
// level only), so this is not the unbounded path.
function appendInlineMatch(kind, match, parent) {
  if (kind !== "link") {
    const element = document.createElement(kind === "strong" ? "strong" : "em");
    renderInlineEmphasis(match[1], element);
    parent.appendChild(element);
    return;
  }
  const label = match[1];
  const internal = internalPath(match[2]);
  const external = externalHref(match[2]);
  if (!internal && !external) {
    // An unsafe/unknown target: keep the label text (rendered), drop the link.
    renderInlineEmphasis(label, parent);
    return;
  }
  const anchor = document.createElement("a");
  anchor.className = "md-link";
  // The label may itself carry bold/italic; render it rather than dumping raw.
  renderInlineEmphasis(label, anchor);
  if (internal) {
    anchor.href = "#";
    anchor.addEventListener("click", (event) => {
      event.preventDefault();
      void openFile(
        { path: internal, kind: internal.endsWith(".md") ? "doc" : "ml" },
        false
      );
    });
  } else {
    anchor.href = external;
    anchor.target = "_blank";
    anchor.rel = "noopener noreferrer";
  }
  parent.appendChild(anchor);
}

// Render one inline run into `parent`, resolving links / bold / italic to the
// earliest match.  Iterates across the remainder (rather than recursing on the
// tail) so a doc with thousands of inline tokens cannot exhaust the JS stack.
// The `_`/`__` forms require non-word boundaries so intra-word underscores
// (snake_case) are not misread as emphasis; `*`/`**` follow CommonMark and may
// occur intra-word.  All text goes through textContent, so nothing here can
// inject markup.
function renderInlineEmphasis(text, parent) {
  const patterns = [
    { re: /\[([^\]]+)\]\(([^)\s]+)\)/, kind: "link" },
    { re: /\*\*([^*]+)\*\*/, kind: "strong" },
    { re: /(?<![A-Za-z0-9])__([^_]+)__(?![A-Za-z0-9])/, kind: "strong" },
    { re: /\*([^*]+)\*/, kind: "em" },
    { re: /(?<![A-Za-z0-9])_([^_]+)_(?![A-Za-z0-9])/, kind: "em" },
  ];
  let rest = String(text);
  while (rest.length > 0) {
    let best = null;
    patterns.forEach((p) => {
      const match = p.re.exec(rest);
      if (match && (best === null || match.index < best.match.index)) {
        best = { match, kind: p.kind };
      }
    });
    if (best === null) {
      parent.appendChild(document.createTextNode(rest));
      return;
    }
    const { match, kind } = best;
    if (match.index > 0) {
      parent.appendChild(document.createTextNode(rest.slice(0, match.index)));
    }
    appendInlineMatch(kind, match, parent);
    rest = rest.slice(match.index + match[0].length);
  }
}

// Inline: `code` spans first (they suppress emphasis inside), then emphasis.
function renderInline(text, parent) {
  String(text)
    .split(/(`[^`]*`)/)
    .forEach((piece) => {
      if (!piece) return;
      if (piece.length >= 2 && piece[0] === "`" && piece[piece.length - 1] === "`") {
        const code = document.createElement("code");
        code.className = "md-inline-code";
        code.textContent = piece.slice(1, -1);
        parent.appendChild(code);
      } else {
        renderInlineEmphasis(piece, parent);
      }
    });
}

// A compact, safe Markdown-to-DOM renderer (a curated-docs subset: headings,
// paragraphs, fenced code, lists, blockquotes, rules, and inline emphasis /
// code / links).  Everything is built with DOM APIs, never innerHTML.
function renderMarkdown(text) {
  const root = document.createElement("div");
  root.className = "md";
  const lines = String(text).replace(/\r\n?/g, "\n").split("\n");
  const isStructural = (line) =>
    /^```/.test(line) ||
    /^#{1,6}\s/.test(line) ||
    /^\s*[-*+]\s+/.test(line) ||
    /^\s*\d+\.\s+/.test(line) ||
    /^>\s?/.test(line) ||
    /^(-{3,}|\*{3,}|_{3,})\s*$/.test(line) ||
    /^\s*$/.test(line);
  let list = null;
  let listOrdered = false;
  const endList = () => {
    if (list) root.appendChild(list);
    list = null;
  };
  let i = 0;
  while (i < lines.length) {
    const line = lines[i];
    if (/^```/.test(line)) {
      endList();
      i += 1;
      const buffer = [];
      while (i < lines.length && !/^```/.test(lines[i])) {
        buffer.push(lines[i]);
        i += 1;
      }
      if (i < lines.length) i += 1; // consume closing fence
      const pre = document.createElement("pre");
      pre.className = "md-code";
      const code = document.createElement("code");
      code.textContent = buffer.join("\n");
      pre.appendChild(code);
      root.appendChild(pre);
      continue;
    }
    const heading = line.match(/^(#{1,6})\s+(.*)$/);
    if (heading) {
      endList();
      const element = document.createElement("h" + heading[1].length);
      renderInline(heading[2], element);
      root.appendChild(element);
      i += 1;
      continue;
    }
    if (/^(-{3,}|\*{3,}|_{3,})\s*$/.test(line)) {
      endList();
      root.appendChild(document.createElement("hr"));
      i += 1;
      continue;
    }
    if (/^>\s?/.test(line)) {
      endList();
      const buffer = [];
      while (i < lines.length && /^>\s?/.test(lines[i])) {
        buffer.push(lines[i].replace(/^>\s?/, ""));
        i += 1;
      }
      const quote = document.createElement("blockquote");
      renderInline(buffer.join(" "), quote);
      root.appendChild(quote);
      continue;
    }
    const bullet = line.match(/^\s*[-*+]\s+(.*)$/);
    const numbered = line.match(/^\s*\d+\.\s+(.*)$/);
    if (bullet || numbered) {
      const ordered = !!numbered;
      if (!list || listOrdered !== ordered) {
        endList();
        list = document.createElement(ordered ? "ol" : "ul");
        list.className = "md-list";
        listOrdered = ordered;
      }
      const item = document.createElement("li");
      renderInline((bullet || numbered)[1], item);
      list.appendChild(item);
      i += 1;
      continue;
    }
    if (/^\s*$/.test(line)) {
      endList();
      i += 1;
      continue;
    }
    endList();
    const buffer = [line];
    i += 1;
    while (i < lines.length && !isStructural(lines[i])) {
      buffer.push(lines[i]);
      i += 1;
    }
    const paragraph = document.createElement("p");
    renderInline(buffer.join(" "), paragraph);
    root.appendChild(paragraph);
  }
  endList();
  return root;
}

// Show a rendered doc: hide the editor, clear stale results, cancel any
// in-flight/queued check, and mark the buffer read-only so no compile path
// runs against the hidden editor buffer.
function enterDocMode(source) {
  cancelTransportRetries();
  cancelActiveRequests();
  cancelPendingCursor();
  docOpen = true;
  window.clearTimeout(debounceTimer);
  // Bump the revision so a /check already in flight for the previous buffer
  // is dropped by its own revision guard instead of painting over the doc.
  documentRevision += 1;
  cm.setOption("readOnly", true);
  clearResults();
  docViewElement.replaceChildren(renderMarkdown(source));
  docViewElement.hidden = false;
  docViewElement.scrollTop = 0;
  editorPaneElement.classList.add("doc-mode");
  document.body.classList.add("doc-mode");
}

// Restore the editor when leaving a doc for an editable file.  CodeMirror was
// display:none, so it needs a refresh to re-measure once visible again.
function exitDocMode() {
  if (!docOpen) return;
  docOpen = false;
  docViewElement.hidden = true;
  docViewElement.replaceChildren();
  editorPaneElement.classList.remove("doc-mode");
  document.body.classList.remove("doc-mode");
  cm.setOption("readOnly", false);
  cm.refresh();
}

// ---------------------------------------------------------------------------
// Multi-file workspace (slice 6): the built-in Demo.mli + Demo.ml + Client.ml
// set plus manifest-backed curated workspaces.  Each set is compiled together,
// so seal implications and cross-unit uses are verified live and each
// obligation routes to its own tab.
//
// The set is client-owned: every /workspace-check sends the full buffer set,
// so the server stays stateless.  Only routing (which VCs reach the active
// buffer's marks / pane) and the explorer/tab UI live here; the pane's
// internal rendering is unchanged.
// ---------------------------------------------------------------------------

const DEMO_WORKSPACE_ORDER = ["Demo.mli", "Demo.ml", "Client.ml"];
let WORKSPACE_ORDER = DEMO_WORKSPACE_ORDER.slice();
const WORKSPACE_DEMO = {
  "Demo.mli":
    "(* A sealed interface: the refinements the implementation must satisfy. *)\n" +
    "val positive : int{ _ > 0 }\n" +
    "val nonneg : int -> int{ _ >= 0 }\n",
  "Demo.ml":
    "(* Sealed against Demo.mli.  `positive` gets the tighter type _ = 1, so the\n" +
    "   seal obligation 1 > 0 is discharged across the .ml/.mli boundary. *)\n" +
    "let positive = (1 : int{ _ = 1 })\n" +
    "let nonneg (x : int) : int{ _ >= 0 } = if x >= 0 then x else 0\n",
  "Client.ml":
    "(* Compiled together with Demo: nonneg's return refinement (_ >= 0) flows\n" +
    "   across the unit boundary and discharges _ >= -1. *)\n" +
    "let at_least : int{ _ >= -1 } = Demo.nonneg 5\n",
};

const WORKSPACE_VERIFICATION_STATUSES = new Set([
  "verified",
  "failed",
  "blocked",
  "none",
]);

function isRecord(value) {
  return value !== null && typeof value === "object" && !Array.isArray(value);
}

function workspaceExpectedState(name, backend) {
  const workspace = activeWorkspaceMeta && activeWorkspaceMeta.workspace;
  const expectations = workspace && workspace.expected_by_backend;
  const byBackend = isRecord(expectations) && isRecord(expectations[backend])
    ? expectations[backend]
    : null;
  return byBackend && typeof byBackend[name] === "string"
    ? byBackend[name]
    : null;
}

function workspaceKnownGap(name, backend) {
  const workspace = activeWorkspaceMeta && activeWorkspaceMeta.workspace;
  return workspaceExpectedState(name, backend) === "solver-error" &&
      workspace && typeof workspace.known_gap === "string"
    ? workspace.known_gap
    : null;
}

function validWorkspaceVerification(value) {
  return (
    isRecord(value) &&
    WORKSPACE_VERIFICATION_STATUSES.has(value.status) &&
    typeof value.message === "string" &&
    typeof value.obligations === "boolean"
  );
}

function validWorkspaceError(error) {
  return (
    isRecord(error) &&
    typeof error.message === "string" &&
    typeof error.kind === "string"
  );
}

function validOutcome(value) {
  return (
    isRecord(value) &&
    typeof value.kind === "string" &&
    typeof value.message === "string" &&
    typeof value.source_located === "boolean"
  );
}

const OBLIGATION_STATUSES = [
  "proved",
  "disproved",
  "unproved",
  "solver-error",
  "unavailable",
  "unknown",
];

function validObligationSummary(value) {
  if (
    !isRecord(value) ||
    !isRecord(value.statuses) ||
    !isRecord(value.hidden_statuses) ||
    !Number.isSafeInteger(value.total) ||
    value.total < 0 ||
    !Number.isSafeInteger(value.hidden) ||
    value.hidden < 0 ||
    value.hidden > value.total
  ) {
    return false;
  }
  let total = 0;
  let hidden = 0;
  for (const status of OBLIGATION_STATUSES) {
    const count = value.statuses[status];
    const hiddenCount = value.hidden_statuses[status];
    if (
      !Number.isSafeInteger(count) ||
      count < 0 ||
      !Number.isSafeInteger(hiddenCount) ||
      hiddenCount < 0 ||
      hiddenCount > count
    ) {
      return false;
    }
    total += count;
    hidden += hiddenCount;
  }
  return total === value.total && hidden === value.hidden;
}

function workspaceExpectedUnits(backend) {
  const units = WORKSPACE_ORDER.slice();
  const workspace = activeWorkspaceMeta && activeWorkspaceMeta.workspace;
  const expectations = workspace && workspace.expected_by_backend;
  const byBackend = isRecord(expectations) ? expectations[backend] : null;
  if (isRecord(byBackend)) {
    Object.keys(byBackend).forEach((name) => {
      if (!units.includes(name)) units.push(name);
    });
  }
  return units;
}

// Validate the response channels before any status fold. A missing unit or a
// missing/malformed per-unit compile outcome is absence of evidence, never a
// type error and never permission for a green workspace header.
function auditWorkspacePayload(payload, expectedOrder) {
  const order = expectedOrder || workspaceExpectedUnits(payload && payload.backend);
  const invalidUnits = new Set();
  const files = isRecord(payload && payload.files) ? payload.files : null;
  order.forEach((name) => {
    const entry = files && files[name];
    if (
      !isRecord(entry) ||
      !Array.isArray(entry.errors) ||
      !entry.errors.every(validWorkspaceError) ||
      !validOutcome(entry.outcome) ||
      !validWorkspaceVerification(entry.verification) ||
      !validObligationSummary(entry.obligation_summary)
    ) {
      invalidUnits.add(name);
    }
  });
  const exactUnitSet =
    files !== null &&
    Object.keys(files).length === order.length &&
    Object.keys(files).every((name) => order.includes(name));
  const recognizedBackend =
    typeof (payload && payload.backend) === "string" &&
    backendOptions.includes(payload.backend) &&
    payload.backend === backendSelection;
  let everyUnitSummaryMatches = false;
  if (files !== null && Array.isArray(payload && payload.vcs)) {
    const adapted = adaptVcs(payload, emittedSpanContext());
    everyUnitSummaryMatches =
      !adapted.unavailable &&
      order.every((name) => {
        const entry = files[name];
        if (!entry || !validObligationSummary(entry.obligation_summary)) {
          return false;
        }
        const fileVcs = adapted.vcs.filter((vc) => vc.file === name);
        return adaptObligationSummary(
          entry.obligation_summary,
          fileVcs,
          entry.obligation_summary.hidden
        ).valid;
      });
  }
  const globalValid =
    exactUnitSet &&
    recognizedBackend &&
    order.length > 0 &&
    order.includes(payload.active) &&
    typeof payload.ok === "boolean" &&
    validOutcome(payload.outcome) &&
    validWorkspaceVerification(payload.workspace_verification) &&
    Array.isArray(payload.vcs) &&
    validObligationSummary(payload.obligation_summary) &&
    everyUnitSummaryMatches;
  return {
    invalidUnits,
    valid: globalValid && invalidUnits.size === 0,
  };
}

function unavailableWorkspaceRollup() {
  return {
    status: "unavailable",
    glyph: "⚠",
    label: "obligations unavailable",
  };
}

function workspaceUnitNotReached(payload, name, order, fileVcs) {
  const index = order.indexOf(name);
  const entry = payload.files[name];
  return (
    index > 0 &&
    entry.verification.status === "none" &&
    fileVcs.length === 0 &&
    order.slice(0, index).some((earlier) => {
      const prior = payload.files[earlier];
      return prior &&
        (prior.verification.status === "failed" ||
          prior.verification.status === "blocked" ||
          prior.outcome.kind !== "ok");
    })
  );
}

function workspaceFileRollup(payload, name, expectedOrder) {
  if (!payload) {
    return { status: "pending", glyph: "…", label: "pending" };
  }
  const order = expectedOrder || workspaceExpectedUnits(payload.backend);
  const audit = auditWorkspacePayload(payload, order);
  if (!audit.valid || audit.invalidUnits.has(name)) {
    return unavailableWorkspaceRollup();
  }
  const spanContext = emittedSpanContext();
  const adapted = adaptVcs(payload, spanContext);
  const entry = payload.files[name];
  const errors = entry.errors;
  const typeErrors = errors.filter((error) => error.kind !== "verification");
  const fileVcs = adapted.vcs.filter((vc) => vc.file === name);
  const verificationStatus = entry && entry.verification
    ? entry.verification.status
    : null;
  // Reachability comes only from the observed compile order and outcomes.
  // Curated expectations never participate in a live tab verdict.
  if (workspaceUnitNotReached(payload, name, order, fileVcs)) {
    return {
      status: "unavailable",
      glyph: "⚠",
      label: "not reached",
    };
  }
  const hasUnverifiedVc = fileVcs.some((vc) => vc.status !== "proved");
  const fileAggregate = adaptObligationSummary(
    entry.obligation_summary,
    fileVcs,
    entry.obligation_summary && entry.obligation_summary.hidden
  );
  return statusRollup(fileVcs, {
    compiles: typeErrors.length === 0,
    errorCount: typeErrors.length,
    // Hidden VCs have no trustworthy file attribution. Fail every tab closed
    // rather than allowing one unit to become green from an absent payload.
    unavailable:
      adapted.unavailable ||
      !fileAggregate.valid ||
      adapted.hidden !== adapted.summary.hidden ||
      (["failed", "blocked"].includes(verificationStatus) && !hasUnverifiedVc),
    outcome: entry.outcome,
    obligationSummary: fileAggregate.summary,
  });
}

function wholeWorkspaceRollup(payload, expectedOrder) {
  const order = expectedOrder || workspaceExpectedUnits(payload && payload.backend);
  const audit = auditWorkspacePayload(payload, order);
  if (!audit.valid) return unavailableWorkspaceRollup();
  const adapted = adaptVcs(payload, emittedSpanContext());
  const entries = order.map((name) => payload.files[name]);
  const errors = entries.flatMap((entry) => entry.errors || []);
  const typeErrors = errors.filter((error) => error.kind !== "verification");
  const hasFailedEntry = entries.some(
    (entry) =>
      entry.verification && ["failed", "blocked"].includes(entry.verification.status)
  );
  const hasUnverifiedVc = adapted.vcs.some((vc) => vc.status !== "proved");
  return statusRollup(adapted.vcs, {
    compiles: entries.length > 0 && typeErrors.length === 0,
    errorCount: typeErrors.length,
    unavailable:
      adapted.unavailable ||
      adapted.hidden !== adapted.summary.hidden ||
      (hasFailedEntry && !hasUnverifiedVc),
    outcome: payload.outcome,
    obligationSummary: adapted.summary,
  });
}

// One unit's tab glyph uses the exact single-buffer taxonomy and never relies
// on hue alone.
function workspaceLayerFor(name) {
  if (
    !lastWorkspaceLayer ||
    lastWorkspaceLayer.payload.backend !== backendSelection ||
    !lastWorkspaceLayer.order.includes(name) ||
    !auditWorkspacePayload(
      lastWorkspaceLayer.payload,
      lastWorkspaceLayer.order
    ).valid
  ) {
    return null;
  }
  return lastWorkspaceLayer;
}

function tabVerdict(name) {
  if (workspaceResultUnavailable) return unavailableWorkspaceRollup();
  const layer = workspaceLayerFor(name);
  return layer
    ? workspaceFileRollup(layer.payload, name, layer.order)
    : workspaceFileRollup(lastWorkspacePayload, name);
}

function renderTabs() {
  if (!workspaceMode) {
    tabsElement.hidden = true;
    tabsElement.replaceChildren();
    return;
  }
  tabsElement.replaceChildren();
  WORKSPACE_ORDER.forEach((name) => {
    const tab = document.createElement("button");
    tab.type = "button";
    tab.className = "tab" + (name === activeFile ? " tab-active" : "");
    tab.setAttribute("role", "tab");
    tab.setAttribute("aria-selected", String(name === activeFile));
    tab.dataset.file = name;
    const verdict = tabVerdict(name);
    const layer = workspaceLayerFor(name);
    const knownGap = workspaceKnownGap(name, backendSelection);
    const glyph = document.createElement("span");
    glyph.className = "tab-status tab-status-" + verdict.status;
    glyph.textContent = verdict.glyph;
    glyph.title =
      name +
      ": " +
      verdict.glyph +
      " " +
      verdict.label +
      (layer ? " — live " + layer.label + " check" : "") +
      (knownGap ? " — known backend gap: " + knownGap : "");
    glyph.setAttribute("aria-label", glyph.title);
    tab.appendChild(glyph);
    tab.appendChild(document.createTextNode(name));
    tab.addEventListener("click", () => switchTab(name));
    tabsElement.appendChild(tab);
  });
  tabsElement.hidden = false;
  highlightWorkspace(activeFile);
}

// The header verdict for the whole workspace (folded over every unit), shown
// distinctly from any one unit's per-tab glyph.
function setWorkspaceStatus(payload) {
  const rollup = wholeWorkspaceRollup(payload);
  const knownGap =
    rollup.status === "solver-error" &&
    WORKSPACE_ORDER.some((name) => workspaceKnownGap(name, backendSelection));
  const layer = lastWorkspaceLayer &&
    lastWorkspaceLayer.payload.backend === backendSelection &&
    auditWorkspacePayload(
      lastWorkspaceLayer.payload,
      lastWorkspaceLayer.order
    ).valid
      ? lastWorkspaceLayer
      : null;
  const layerRollup = layer
    ? wholeWorkspaceRollup(layer.payload, layer.order)
    : null;
  const layerText = layerRollup && layerRollup.status === "verified"
    ? " · " + layer.label + " " + layerRollup.label
    : "";
  setStatus(
    rollup.status,
    rollup.glyph +
      " " +
      rollup.label +
      " · workspace" +
      (knownGap ? " · known backend gap" : "") +
      layerText +
      completedLatencyText()
  );
}

// Obligations whose anchor is a *different* unit than the active buffer: listed
// with a per-unit jump so a seal discharged in Demo.mli (or an obligation in
// Client.ml) is discoverable while editing Demo.ml, and one click switches to
// that unit and lands on the obligation.
function renderCrossUnit() {
  crossUnitElement.replaceChildren();
  if (!workspaceMode || !crossUnitVcs.length) {
    crossUnitElement.hidden = true;
    return;
  }
  const byFile = {};
  crossUnitVcs.forEach((vc) => {
    (byFile[vc.file] || (byFile[vc.file] = [])).push(vc);
  });
  const title = document.createElement("div");
  title.className = "cross-unit-title";
  title.textContent = "Obligations in other units";
  crossUnitElement.appendChild(title);
  Object.keys(byFile).forEach((file) => {
    const list = byFile[file];
    const proved = list.filter((vc) => vc.status === "proved").length;
    const row = document.createElement("button");
    row.type = "button";
    row.className = "cross-unit-row";
    row.dataset.file = file;
    row.textContent =
      list.length +
      " in " +
      file +
      " (" +
      proved +
      "/" +
      list.length +
      " proved)";
    row.title = "jump to " + file;
    row.addEventListener("click", () => {
      const target = list[0];
      switchTab(file);
      if (target) {
        cm.setCursor(cmPosition(target.start));
        cm.focus();
      }
    });
    crossUnitElement.appendChild(row);
  });
  crossUnitElement.hidden = false;
}

// Derive the active-buffer view from a /workspace-check payload: partition the
// file-tagged VCs into active-buffer marks vs cross-unit jump links, and drive
// the diagnostics / signature / verdicts.  Reused verbatim on a tab switch
// (from the cached payload) so switching is instant.
function applyWorkspaceView(payload, elapsed) {
  if (!payload) return;
  workspaceResultUnavailable = false;
  lastWorkspacePayload = payload;
  appliedRevision = payload.revision;
  const fullAudit = auditWorkspacePayload(payload);
  const spanContext = emittedSpanContext();
  // Response metadata may refresh available options, but an observed payload
  // never gets to change which backend the user requested.
  applyBackendMetadata(payload, backendSelection);
  const active = activeFile;
  const layer = workspaceLayerFor(active);
  const viewPayload = layer ? layer.payload : payload;
  const viewOrder = layer ? layer.order : workspaceExpectedUnits(payload.backend);
  const audit = layer
    ? auditWorkspacePayload(viewPayload, viewOrder)
    : fullAudit;
  const adapted = adaptVcs(viewPayload, spanContext);
  const fullAdapted = layer ? adaptVcs(payload, spanContext) : adapted;
  const activeValid = audit.valid && !audit.invalidUnits.has(active);
  const entry = activeValid ? viewPayload.files[active] : null;
  vcs = activeValid
    ? adapted.vcs.filter((vc) => (vc.file || null) === active)
    : [];
  crossUnitVcs = fullAdapted.vcs.filter(
    (vc) =>
      (vc.file || null) !== active &&
      fullAudit.valid &&
      !fullAudit.invalidUnits.has(vc.file || "")
  );
  vcsHidden =
    activeValid && entry && entry.obligation_summary
      ? Number(entry.obligation_summary.hidden) || 0
      : adapted.hidden;
  const activeAggregate = adaptObligationSummary(
    entry && entry.obligation_summary,
    vcs,
    activeValid && entry && entry.obligation_summary
      ? entry.obligation_summary.hidden
      : adapted.hidden
  );
  vcsUnavailable = adapted.unavailable || !activeValid || !activeAggregate.valid;
  vcsUnavailableReason = !activeAggregate.valid
    ? "malformed-vc-data"
    : activeValid
      ? adapted.unavailableReason
      : "check-failed";
  identifierModes = validatedRanges(viewPayload.identifier_modes, spanContext).filter(
    (range) => (range.file || null) === active
  );
  obligationSummary = activeAggregate.summary;
  renderDiagnostics(entry ? entry.errors : []);
  // Types and signature are produced only for the unit the compile ran with as
  // active; if we are showing a different tab (a switch not yet recompiled),
  // leave them at their "waiting" state until the refresh lands.
  if (activeValid && viewPayload.active === active) {
    expressionTypes = validatedRanges(entry.types, {
      lines: cm.getValue().split("\n"),
    });
    refinementTypes = validatedRanges(
      viewPayload.refinement_types,
      spanContext
    ).filter((range) => (range.file || null) === active);
    imposedTypes = validatedRanges(entry.imposed_types, {
      lines: cm.getValue().split("\n"),
    });
    renderSignatureState(
      active.endsWith(".mli")
        ? { status: "interface", text: "", error: "" }
        : entry.signature,
      entry.outcome
    );
  } else if (!activeValid) {
    expressionTypes = [];
    refinementTypes = [];
    imposedTypes = [];
    signatureElement.replaceChildren();
  }
  lastVerification = entry ? entry.verification : null;
  lastOutcome = entry ? entry.outcome : { kind: "unknown", message: "" };
  // The STATUS roll-up folds the active unit's compile outcome with its own
  // obligation counts (vcs is already filtered to the active unit above).  Only
  // parse/type errors mark the unit as not compiling; verification failures show
  // as disproved obligations.
  lastErrorCount = (entry ? entry.errors : []).filter((e) =>
    ["syntax", "type-mode", "type"].includes(e.kind)
  ).length;
  lastCompiles =
    activeValid &&
    lastErrorCount === 0 &&
    (!lastOutcome || ["ok", "verification", "backend-unavailable"].includes(lastOutcome.kind));
  // A completed workspace check has landed for the active unit.
  firstCheckDone = true;
  renderVerification(
    entry ? entry.verification : null,
    entry ? entry.errors : []
  );
  if (activeValid) renderStatusVerdict();
  if (elapsed !== undefined) lastLatencyMs = elapsed;
  setWorkspaceStatus(payload);
  markVcs();
  renderLegend();
  renderBackendResults();
  renderCrossUnit();
  renderTabs();
  renderCursorType();
  renderProofPane();
}

function knownGapLayerConfig(backend) {
  const workspace = activeWorkspaceMeta && activeWorkspaceMeta.workspace;
  const config = workspace && workspace.known_gap_check;
  if (!isRecord(config) || !Array.isArray(config.files)) return null;
  const observedGap = WORKSPACE_ORDER.some(
    (name) =>
      workspaceKnownGap(name, backend) &&
      workspaceFileRollup(lastWorkspacePayload, name).status === "solver-error"
  );
  return observedGap ? config : null;
}

async function runKnownGapLayerCheck(options) {
  const config = knownGapLayerConfig(options.backend);
  if (!config) return null;
  const order = config.files.slice();
  const files = order.map((name) => ({
    name,
    source: workspaceBuffers[name],
  }));
  let payload;
  try {
    payload = await postJSON("/workspace-check", {
      revision: options.revision,
      active: config.active,
      files,
      backend: options.backend,
    }, options.controller.signal);
  } catch (error) {
    return null;
  }
  if (
    !workspaceMode ||
    payload.revision !== documentRevision ||
    options.backend !== backendSelection ||
    options.controller !== authoritativeController ||
    options.controller.signal.aborted ||
    !auditWorkspacePayload(payload, order).valid
  ) {
    return null;
  }
  lastWorkspaceLayer = {
    label: config.label,
    order,
    payload,
  };
  applyWorkspaceView(lastWorkspacePayload);
  return { files, payload };
}

// Fetch + apply one whole-workspace compile.  Single-flighted; a response is
// dropped if the buffer moved on (revision) or the active tab changed since the
// request (so a stale active unit's types never paint the wrong tab).
async function runWorkspaceCheck() {
  if (!workspaceMode) return;
  const controller = beginAuthoritativeRequest();
  const revision = documentRevision;
  const active = activeFile;
  const backend = backendSelection;
  const generation = retryGeneration;
  const startedAt = Date.now();
  const files = WORKSPACE_ORDER.map((name) => ({
    name,
    source: workspaceBuffers[name],
  }));
  try {
    let payload = null;
    try {
      payload = await postJSON("/workspace-check", {
        revision,
        active,
        files,
        backend,
      }, controller.signal);
    } catch (error) {
      handleRequestError(
        error,
        generation,
        () =>
          workspaceMode &&
          revision === documentRevision &&
          active === activeFile &&
          backend === backendSelection,
        runWorkspaceCheck
      );
      return;
    }
    if (
      !workspaceMode ||
      payload.revision !== documentRevision ||
      active !== activeFile ||
      authoritativeController !== controller ||
      controller.signal.aborted
    ) {
      return;
    }
    retryAttempt = 0;
    lastWorkspaceLayer = null;
    applyWorkspaceView(payload, Date.now() - startedAt);
    const layerResult = await runKnownGapLayerCheck({
      revision,
      backend,
      controller,
    });
    if (
      !workspaceMode ||
      revision !== documentRevision ||
      active !== activeFile ||
      authoritativeController !== controller ||
      controller.signal.aborted
    ) {
      return;
    }
    const signatureFiles = layerResult && layerResult.files.some(
      (file) => file.name === active
    )
      ? layerResult.files
      : files;
    const signatureOutcome = layerResult && layerResult.payload.files[active]
      ? layerResult.payload.files[active].outcome.kind
      : payload.outcome
      ? payload.outcome.kind
      : payload.ok
      ? "ok"
      : "unknown";
    void refreshSignature({
      workspace: true,
      revision,
      active,
      files: signatureFiles,
      backend,
      outcome: signatureOutcome,
    });
  } finally {
    finishAuthoritativeRequest(controller);
  }
}

// Switch the active unit: save the current buffer, load the target, re-route
// the cached view instantly, then recompile to refresh the new unit's types /
// signature.  No network is needed for the marks/pane (they come from the
// cached file-tagged payload).
function switchTab(name) {
  if (!workspaceMode || name === activeFile || !(name in workspaceBuffers)) {
    return;
  }
  cancelTransportRetries();
  cancelActiveRequests();
  cancelPendingCursor();
  workspaceBuffers[activeFile] = cm.getValue();
  activeFile = name;
  suppressChange = true;
  cm.setValue(workspaceBuffers[name]);
  suppressChange = false;
  lastLoaded = workspaceBuffers[name];
  signatureElement.replaceChildren();
  expressionTypes = [];
  refinementTypes = [];
  identifierModes = [];
  imposedTypes = [];
  if (lastWorkspacePayload) applyWorkspaceView(lastWorkspacePayload);
  else renderTabs();
  cm.setCursor({ line: 0, ch: 0 });
  cm.focus();
  scheduleCheck(0);
}

// Enter workspace mode after its order, buffers, id, and metadata have been
// selected.  Buffers persist across enter/exit so edits are not lost.
function activateWorkspace(name) {
  cancelTransportRetries();
  cancelActiveRequests();
  cancelPendingCursor();
  exitDocMode();
  workspaceMode = true;
  activeFile = name in workspaceBuffers ? name : WORKSPACE_ORDER[0];
  currentPath = null;
  highlightActive(null);
  clearResults();
  cm.setOption("readOnly", false);
  suppressChange = true;
  cm.setValue(workspaceBuffers[activeFile]);
  suppressChange = false;
  lastLoaded = workspaceBuffers[activeFile];
  pendingCursor = null;
  renderTabs();
  documentRevision += 1;
  scheduleCheck(0);
}

function enterWorkspace(name) {
  if (activeWorkspaceId !== "demo" || !Object.keys(workspaceBuffers).length) {
    WORKSPACE_ORDER = DEMO_WORKSPACE_ORDER.slice();
    workspaceBuffers = {};
    WORKSPACE_ORDER.forEach((n) => {
      workspaceBuffers[n] = WORKSPACE_DEMO[n];
    });
    activeWorkspaceId = "demo";
    activeWorkspaceMeta = null;
  }
  activateWorkspace(name);
}

function validCuratedWorkspace(example) {
  const workspace = example && example.workspace;
  if (
    !isRecord(workspace) ||
    !["verified", "disproved", "unproved"].includes(example.expected_state) ||
    !Array.isArray(workspace.files) ||
    workspace.files.length === 0
  ) {
    return false;
  }
  const names = workspace.files.map((file) => file && file.name);
  const uniqueNames = new Set(names);
  const declaredOrder = workspace.order;
  const validOrder =
    declaredOrder === undefined ||
    (Array.isArray(declaredOrder) &&
      declaredOrder.length === names.length &&
      new Set(declaredOrder).size === declaredOrder.length &&
      declaredOrder.every((name) => uniqueNames.has(name)));
  const validFiles =
    uniqueNames.size === names.length &&
    workspace.files.every((file) => {
      if (
        !isRecord(file) ||
        typeof file.name !== "string" ||
        typeof file.path !== "string" ||
        !/^[A-Za-z0-9_.-]+\.mli?$/.test(file.name) ||
        !/^examples\/[A-Za-z0-9_.-]+\/[A-Za-z0-9_.-]+\.mli?$/.test(file.path)
      ) {
        return false;
      }
      return file.path.split("/").pop() === file.name;
    });
  if (
    !validFiles ||
    !validOrder ||
    !uniqueNames.has(workspace.active) ||
    !["lean", "z3", "oxsmt", "cross"].includes(workspace.default_backend) ||
    !isRecord(workspace.expected_by_backend) ||
    !isRecord(workspace.expected_by_backend[workspace.default_backend])
  ) {
    return false;
  }
  const validExpectedStates = new Set([
    "interface",
    "verified",
    "disproved",
    "unproved",
    "solver-error",
    "unavailable",
  ]);
  const expectationsValid = Object.entries(workspace.expected_by_backend).every(
    ([backend, expected]) =>
      ["lean", "z3", "oxsmt", "cross"].includes(backend) &&
      isRecord(expected) &&
      Object.keys(expected).length === names.length &&
      Object.keys(expected).every(
        (name) => uniqueNames.has(name) && validExpectedStates.has(expected[name])
      )
  );
  const layer = workspace.known_gap_check;
  const layerValid =
    layer === undefined ||
    (isRecord(layer) &&
      typeof layer.label === "string" &&
      layer.label.length > 0 &&
      Array.isArray(layer.files) &&
      layer.files.length > 0 &&
      layer.files.length < names.length &&
      new Set(layer.files).size === layer.files.length &&
      layer.files.every((name) => uniqueNames.has(name)) &&
      layer.files.includes(layer.active));
  return expectationsValid && layerValid;
}

function curatedWorkspaceOrder(workspace) {
  return Array.isArray(workspace.order)
    ? workspace.order.slice()
    : workspace.files.map((file) => file.name);
}

// Load a curated workspace's source files through the same allowlisted /file
// endpoint as ordinary examples.  The full source set then remains client-
// owned and is sent on every stateless /workspace-check request.
async function enterCuratedWorkspace(example, name) {
  if (!validCuratedWorkspace(example)) return false;
  const workspace = example.workspace;
  const order = curatedWorkspaceOrder(workspace);
  const alreadyActive = workspaceMode && activeWorkspaceId === example.name;
  if (
    activeWorkspaceId !== example.name ||
    order.some((file) => !(file in workspaceBuffers))
  ) {
    const loaded = {};
    try {
      for (const file of workspace.files) {
        const response = await fetch(
          "/file?path=" + encodeURIComponent(file.path)
        );
        if (!response.ok) return false;
        loaded[file.name] = await response.text();
      }
    } catch (error) {
      setStatus("error", "could not load workspace");
      return false;
    }
    WORKSPACE_ORDER = order;
    workspaceBuffers = loaded;
    activeWorkspaceId = example.name;
    activeWorkspaceMeta = example;
  }
  const preferred = workspace.default_backend;
  if (!alreadyActive && typeof preferred === "string") {
    applyBackendMetadata(
      {
        backend_options: backendOptions,
        backend_solver_configuration: backendSolverConfiguration,
      },
      preferred
    );
  }
  activateWorkspace(name || workspace.active || WORKSPACE_ORDER[0]);
  return true;
}

// Leave workspace mode (opening a single-buffer example or a doc): hide the tab
// strip and cross-unit list so none of it lingers over the single buffer.
function exitWorkspace() {
  if (!workspaceMode) return;
  workspaceMode = false;
  tabsElement.hidden = true;
  tabsElement.replaceChildren();
  crossUnitElement.hidden = true;
  crossUnitElement.replaceChildren();
  crossUnitVcs = [];
  lastWorkspacePayload = null;
  lastWorkspaceLayer = null;
  workspaceResultUnavailable = false;
  highlightWorkspace(null);
  workspaceBuffers = {};
  WORKSPACE_ORDER = DEMO_WORKSPACE_ORDER.slice();
  activeWorkspaceId = "demo";
  activeWorkspaceMeta = null;
  if (configuredDefaultBackend) {
    applyBackendMetadata(
      {
        backend_options: backendOptions,
        backend_solver_configuration: backendSolverConfiguration,
      },
      configuredDefaultBackend
    );
  }
}

// ---------------------------------------------------------------------------
// File explorer sidebar + curated examples.
// ---------------------------------------------------------------------------

const treeElement = document.getElementById("tree");
let examplesList = [];
let treeData = null;

async function loadExamples() {
  try {
    const response = await fetch("/examples");
    examplesList = (await response.json()).examples || [];
  } catch (e) {
    examplesList = [];
  }
  return examplesList;
}

function highlightActive(path) {
  treeElement.querySelectorAll(".tree-file.active").forEach((el) =>
    el.classList.remove("active")
  );
  treeElement.querySelectorAll(".tree-file").forEach((el) => {
    if (el.dataset.path === path) el.classList.add("active");
  });
}

// Load an allowlisted file.  `force` skips the unsaved-edits guard (startup).
// A .md doc opens in the read-only rendered viewer; an .ml/.mli loads into the
// editor and is checked.  Either way, stale results from the previous buffer
// are cleared first.
async function openFile(node, force) {
  const isDoc = node.kind === "doc";
  // Opening a doc never discards editor edits (the buffer is left intact and
  // simply hidden behind the doc viewer), so only an editable file needs the
  // unsaved-edits guard.
  if (
    !isDoc &&
    !force &&
    cm.getValue() !== lastLoaded &&
    !window.confirm("Discard your edits and load this file?")
  ) {
    return false;
  }
  cancelTransportRetries();
  cancelActiveRequests();
  cancelPendingCursor();
  // Opening a single-buffer file leaves the multi-file workspace (its tab
  // strip and cross-unit list must not linger over the single buffer).
  exitWorkspace();
  try {
    const response = await fetch("/file?path=" + encodeURIComponent(node.path));
    if (!response.ok) return false;
    const source = await response.text();
    currentPath = node.path;
    highlightActive(node.path);
    rememberFile(node.path);
    if (isDoc) {
      enterDocMode(source);
      return true;
    }
    // Leaving a doc (if we were in one) restores the editor; clear the
    // previous buffer's diagnostics / signature / proof state so none of it
    // lingers while this file is fetched and checked.
    exitDocMode();
    clearResults();
    cm.setOption("readOnly", false);
    lastLoaded = source;
    cm.setValue(source);
    // Open an example on its suggested teaching line (1-based in the
    // manifest); other files open at the top.
    const meta = examplesList.find(
      (e) => "examples/" + e.name + ".ml" === node.path
    );
    const teachingLine =
      meta && typeof meta.cursor === "number" ? meta.cursor - 1 : 0;
    setCursorProgrammatically({ line: teachingLine, ch: 0 });
    cm.focus();
    pendingCursor = meta && typeof meta.cursor === "number"
      ? {
          line: teachingLine,
          path: currentPath,
          revision: documentRevision,
          backend: backendSelection,
          interaction: cursorInteractionToken,
        }
      : null;
    scheduleCheck(0);
    return true;
  } catch (e) {
    setStatus("error", "could not load file");
    return false;
  }
}

async function loadExample(name, force) {
  const example = examplesList.find((entry) => entry.name === name);
  if (example && validCuratedWorkspace(example)) {
    if (
      !force &&
      !workspaceMode &&
      !docOpen &&
      cm.getValue() !== lastLoaded &&
      !window.confirm("Discard your edits and open this workspace?")
    ) {
      return false;
    }
    return enterCuratedWorkspace(example, example.workspace.active);
  }
  return openFile({ path: "examples/" + name + ".ml", kind: "ml" }, force);
}

// The tree is keyboard-operable with a roving tabindex: exactly one item is in
// the tab order (tabindex 0) at a time and the rest are -1, so Tab enters the
// tree once and the arrow keys move within it.  Up/Down move focus among the
// visible items, Left/Right collapse/expand a directory, Enter/Space activate.
function allTreeItems() {
  return Array.from(treeElement.querySelectorAll('[role="treeitem"]'));
}

function visibleTreeItems() {
  return allTreeItems().filter((el) => el.offsetParent !== null);
}

// Make `target` the single tab stop; called on render and whenever an item
// takes focus (mouse or keyboard), so the tab order tracks the active item.
function setRovingItem(target) {
  allTreeItems().forEach((el) => {
    el.tabIndex = el === target ? 0 : -1;
  });
}

function focusTreeSibling(current, delta) {
  const items = visibleTreeItems();
  const index = items.indexOf(current);
  if (index === -1) return;
  const next = items[index + delta];
  if (next) {
    setRovingItem(next);
    next.focus();
  }
}

function treeKeydown(event) {
  const el = event.currentTarget;
  if (event.key === "ArrowDown") {
    event.preventDefault();
    focusTreeSibling(el, 1);
  } else if (event.key === "ArrowUp") {
    event.preventDefault();
    focusTreeSibling(el, -1);
  } else if (event.key === "Enter" || event.key === " ") {
    event.preventDefault();
    if (typeof el._activate === "function") el._activate();
  } else if (event.key === "ArrowRight" && typeof el._expand === "function") {
    event.preventDefault();
    el._expand();
  } else if (event.key === "ArrowLeft" && typeof el._collapse === "function") {
    event.preventDefault();
    el._collapse();
  }
}

function renderFileNode(node) {
  const el = document.createElement("div");
  el.className = "tree-file kind-" + (node.kind || "file");
  el.setAttribute("role", "treeitem");
  el.tabIndex = -1;
  el.dataset.path = node.path;
  const meta = examplesList.find(
    (example) => "examples/" + example.name + ".ml" === node.path
  );
  const titleText = meta && meta.title ? meta.title : node.title || node.name;
  const expected = (meta && meta.expected_state) || node.expected_state;
  el.title = expected
    ? titleText + " — teaching intent: deliberately " + expected
    : titleText;
  const title = document.createElement("span");
  title.className = "tree-file-title";
  title.textContent = titleText;
  el.appendChild(title);
  if (titleText !== node.name) {
    const filename = document.createElement("span");
    filename.className = "tree-file-name";
    filename.textContent = node.name;
    el.appendChild(filename);
  }
  el._activate = () => openFile(node, false);
  el.addEventListener("click", el._activate);
  el.addEventListener("keydown", treeKeydown);
  el.addEventListener("focus", () => setRovingItem(el));
  return el;
}

function renderDirNode(node, isRoot) {
  const wrap = document.createElement("div");
  wrap.className = "tree-dir" + (isRoot ? " tree-root" : "");
  const label = document.createElement("div");
  label.className = "tree-dir-label";
  label.setAttribute("role", "treeitem");
  label.setAttribute("aria-expanded", "true");
  label.tabIndex = -1;
  const chevron = document.createElement("span");
  chevron.className = "tree-chevron";
  chevron.textContent = "▾";
  label.appendChild(chevron);
  const name = document.createElement("span");
  name.className = "tree-name";
  name.textContent = node.name;
  label.appendChild(name);
  const kids = document.createElement("div");
  kids.className = "tree-children";
  kids.setAttribute("role", "group");
  (node.children || []).forEach((child) =>
    kids.appendChild(
      child.type === "dir" ? renderDirNode(child, false) : renderFileNode(child)
    )
  );
  const setCollapsed = (collapsed) => {
    wrap.classList.toggle("collapsed", collapsed);
    chevron.textContent = collapsed ? "▸" : "▾";
    label.setAttribute("aria-expanded", String(!collapsed));
  };
  label._activate = () => setCollapsed(!wrap.classList.contains("collapsed"));
  label._expand = () => setCollapsed(false);
  label._collapse = () => setCollapsed(true);
  label.addEventListener("click", label._activate);
  label.addEventListener("keydown", treeKeydown);
  label.addEventListener("focus", () => setRovingItem(label));
  wrap.appendChild(label);
  wrap.appendChild(kids);
  return wrap;
}

// Explorer entry for a client-owned multi-file set.  A curated unit loads its
// workspace through manifest paths; the built-in demo keeps its in-memory
// sources.  Either path enters the same live workspace UI.
function renderWorkspaceFileNode(name, example) {
  const workspaceId = example ? example.name : "demo";
  const workspace = example && example.workspace;
  const defaultBackend = workspace && workspace.default_backend;
  const expected =
    workspace && isRecord(workspace.expected_by_backend) &&
    isRecord(workspace.expected_by_backend[defaultBackend])
      ? workspace.expected_by_backend[defaultBackend][name]
      : null;
  const hasKnownGap =
    workspace && isRecord(workspace.expected_by_backend) &&
    Object.keys(workspace.expected_by_backend).some(
      (backend) =>
        isRecord(workspace.expected_by_backend[backend]) &&
        workspace.expected_by_backend[backend][name] === "solver-error"
    );
  const el = document.createElement("div");
  el.className =
    "tree-file kind-" + (name.endsWith(".mli") ? "mli" : "ml") + " workspace-file";
  el.setAttribute("role", "treeitem");
  el.tabIndex = -1;
  el.dataset.file = name;
  el.dataset.workspace = workspaceId;
  el.title =
    name +
    (expected && defaultBackend
      ? " — expected on " + defaultBackend + ": " + expected
      : " — multi-file workspace unit") +
    (hasKnownGap && typeof workspace.known_gap === "string"
      ? " — known backend gap: " + workspace.known_gap
      : "");
  el.textContent = name;
  el._activate = async () => {
    if (
      !workspaceMode &&
      !docOpen &&
      cm.getValue() !== lastLoaded &&
      !window.confirm("Discard your edits and open this workspace?")
    ) {
      return;
    }
    if (example) await enterCuratedWorkspace(example, name);
    else enterWorkspace(name);
  };
  el.addEventListener("click", el._activate);
  el.addEventListener("keydown", treeKeydown);
  el.addEventListener("focus", () => setRovingItem(el));
  return el;
}

function renderWorkspaceRoot(example) {
  const workspace = example && example.workspace;
  const workspaceId = example ? example.name : "demo";
  const wrap = document.createElement("div");
  wrap.className =
    "tree-dir tree-root" + (example ? " workspace-example" : "");
  wrap.dataset.workspace = workspaceId;
  const label = document.createElement("div");
  label.className = "tree-dir-label";
  label.setAttribute("role", "treeitem");
  label.setAttribute("aria-expanded", "true");
  label.tabIndex = -1;
  const chevron = document.createElement("span");
  chevron.className = "tree-chevron";
  chevron.textContent = "▾";
  label.appendChild(chevron);
  if (example) {
    const identity = document.createElement("span");
    identity.className = "workspace-identity";
    const title = document.createElement("span");
    title.className = "tree-file-title";
    title.textContent = example.title || example.name;
    identity.appendChild(title);
    const filename = document.createElement("span");
    filename.className = "tree-file-name";
    filename.textContent = example.filename || example.name + "/";
    identity.appendChild(filename);
    label.appendChild(identity);
    label.title =
      (example.description || example.title || example.name) +
      " — expected on " +
      (workspace.default_backend || "configured backend") +
      ": " +
      example.expected_state;
  } else {
    const name = document.createElement("span");
    name.className = "tree-name";
    name.textContent = "Workspace (multi-file)";
    label.appendChild(name);
  }
  const kids = document.createElement("div");
  kids.className = "tree-children";
  kids.setAttribute("role", "group");
  const names = example
    ? curatedWorkspaceOrder(workspace)
    : DEMO_WORKSPACE_ORDER;
  names.forEach((n) => kids.appendChild(renderWorkspaceFileNode(n, example)));
  const setCollapsed = (collapsed) => {
    wrap.classList.toggle("collapsed", collapsed);
    chevron.textContent = collapsed ? "▸" : "▾";
    label.setAttribute("aria-expanded", String(!collapsed));
  };
  label._activate = () => setCollapsed(!wrap.classList.contains("collapsed"));
  label._expand = () => setCollapsed(false);
  label._collapse = () => setCollapsed(true);
  label.addEventListener("click", label._activate);
  label.addEventListener("keydown", treeKeydown);
  label.addEventListener("focus", () => setRovingItem(label));
  wrap.appendChild(label);
  wrap.appendChild(kids);
  return wrap;
}

// Mark the active workspace unit in the explorer (kept in sync with the tab
// strip).  A null name clears it (on leaving workspace mode).
function highlightWorkspace(name) {
  treeElement
    .querySelectorAll(".workspace-file")
    .forEach((el) =>
      el.classList.toggle(
        "active",
        el.dataset.workspace === activeWorkspaceId && el.dataset.file === name
      )
    );
}

function renderTree(data) {
  treeElement.replaceChildren();
  (data.roots || []).forEach((root) =>
    treeElement.appendChild(renderDirNode(root, true))
  );
  // Client-owned workspace sets live below the server-backed roots.  Curated
  // sets come from the manifest; their sources are still served only through
  // the allowlisted /file endpoint.
  treeElement.appendChild(renderWorkspaceRoot(null));
  examplesList
    .filter(validCuratedWorkspace)
    .forEach((example) => treeElement.appendChild(renderWorkspaceRoot(example)));
  if (currentPath) highlightActive(currentPath);
  if (workspaceMode) highlightWorkspace(activeFile);
  // Seed the roving tabindex on the first item so Tab reaches the tree.
  const first = allTreeItems()[0];
  if (first) first.tabIndex = 0;
}

async function loadTree() {
  await loadExamples();
  try {
    const response = await fetch("/ls");
    treeData = await response.json();
  } catch (e) {
    treeData = { roots: [] };
  }
  renderTree(treeData);
  return treeData;
}

// Sidebar show/hide (persisted): collapsing gives the editor full width.
const sidebarButton = document.getElementById("sidebar-button");
const SIDEBAR_KEY = "voxide-sidebar";

function applySidebar(hidden) {
  document.body.classList.toggle("sidebar-hidden", hidden);
}

sidebarButton.addEventListener("click", () => {
  const hidden = !document.body.classList.contains("sidebar-hidden");
  try {
    localStorage.setItem(SIDEBAR_KEY, hidden ? "hidden" : "shown");
  } catch (e) {}
  applySidebar(hidden);
});

(function initSidebar() {
  try {
    applySidebar(localStorage.getItem(SIDEBAR_KEY) === "hidden");
  } catch (e) {}
})();

// Remember the last file opened, so a reload reopens it instead of always
// snapping back to the default example.
const FILE_KEY = "voxide-file";

function rememberFile(path) {
  try {
    localStorage.setItem(FILE_KEY, path);
  } catch (e) {}
}

function savedFile() {
  try {
    return localStorage.getItem(FILE_KEY);
  } catch (e) {
    return null;
  }
}

// Find a tree file node by its path id, so a remembered path can be reopened
// through the same openFile path (with its kind) rather than guessed at.
function findFileNode(path) {
  let found = null;
  const walk = (nodes) =>
    (nodes || []).forEach((node) => {
      if (node.type === "dir") walk(node.children);
      else if (node.path === path) found = node;
    });
  walk(treeData ? treeData.roots : []);
  return found;
}

// Startup: render the tree, reopen the last-viewed file if it still exists,
// otherwise open the default example (auto-checked); fall back to checking
// the built-in SAMPLE if the examples do not load.
async function init() {
  await loadBackendConfiguration();
  await loadTree();
  const remembered = savedFile();
  if (remembered) {
    const node = findFileNode(remembered);
    if (node && (await openFile(node, true))) return;
  }
  const def = examplesList.find((e) => e.default) || examplesList[0];
  if (def && (await loadExample(def.name, true))) return;
  cm.setValue(SAMPLE);
  lastLoaded = SAMPLE;
  cm.setCursor({ line: 5, ch: 12 });
  cm.focus();
  scheduleCheck(0);
}

window.__voxide = {
  cm,
  runCheck,
  refreshVcs,
  loadExample,
  openFile,
  renderMarkdown,
  isDocOpen: () => docOpen,
  getTypes: () => expressionTypes,
  getVcs: () => vcs,
  getCurrentPath: () => currentPath,
  getTree: () => treeData,
  // Multi-file workspace (slice 6) hooks, used by the textual harness.
  openWorkspace: (name) => enterWorkspace(name || WORKSPACE_ORDER[0]),
  openCuratedWorkspace: (id, name) => {
    const example = examplesList.find((entry) => entry.name === id);
    return enterCuratedWorkspace(example, name);
  },
  isValidCuratedWorkspace: (example) => validCuratedWorkspace(example),
  switchTab: (name) => switchTab(name),
  runWorkspaceCheck: () => runWorkspaceCheck(),
  exitWorkspace: () => exitWorkspace(),
  isWorkspace: () => workspaceMode,
  getActiveFile: () => activeFile,
  getCrossUnitVcs: () => crossUnitVcs,
  getWorkspacePayload: () => lastWorkspacePayload,
  getWorkspaceOrder: () => WORKSPACE_ORDER.slice(),
  getWorkspaceId: () => activeWorkspaceId,
  getBackend: () => backendSelection,
  getBackendOptions: () => backendOptions.slice(),
  getBackendSolverConfiguration: () => ({ ...backendSolverConfiguration }),
  getCompact: () => compact,
  setCompact,
  getRetryState: () => ({ attempt: retryAttempt, scheduled: retryTimer !== null }),
};

init();
