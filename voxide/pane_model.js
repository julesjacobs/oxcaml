"use strict";

// ===========================================================================
// pane_model.js -- the ONE pure model behind the vox2 IDE proof pane.
//
// Both sinks -- the browser (app.js, which builds the #pane-body / #pane-mode /
// #legend DOM) and the terminal (tools/voxide-pane.js) -- render from the
// SAME view-model computed here.  There is deliberately no second copy of the
// pane's decision or content logic: a reimplementation could drift and lie
// about what the user sees, which is the one thing this tool must never do.
//
// This file is a plain script: loaded with a <script> tag it defines its
// functions as globals (app.js calls them directly); required in node it also
// exposes them on module.exports.  It has NO DOM or tokenizer dependency -- it
// emits raw text spans only.  Syntax colouring is a browser-only presentation
// concern (the tokenizer stays in app.js); the pane's *textual content* is
// exactly what this model produces, which is why the terminal tool can mirror
// the pane byte-for-byte (see tests/test_pane_fidelity.js).
// ===========================================================================

// ---------------------------------------------------------------------------
// Status taxonomy (moved verbatim from app.js -- the single source of truth).
// ---------------------------------------------------------------------------

// The allowlist of statuses that have a badge, an underline class, and a
// verdict meaning.  Any status outside this set is normalized to "unknown"
// before it reaches a class-attribute sink (defense in depth) and is treated
// as a fail-closed anomaly, never as a benign "not yet checked".
const VC_STATUSES = [
  "proved",
  "disproved",
  "unproved",
  "solver-error",
  "unavailable",
  "unknown",
  "failed",
];

function normalizeStatus(status) {
  return VC_STATUSES.indexOf(status) >= 0 ? status : "unknown";
}

// The statuses a failing (or unresolved) solve attaches to an obligation.
// "unknown" is included: an unrecognized verdict fails closed to unverified.
const FAILED_STATUSES = [
  "disproved",
  "unproved",
  "failed",
  "solver-error",
  "unavailable",
  "unknown",
];

const BADGE_HINT = {
  proved: "the verifier discharged this obligation",
  disproved: "the verifier refuted this obligation (its negation was proved; a concrete witness may not be available)",
  unproved: "automation gave up; no counterexample found (may still hold)",
  failed: "the verifier rejected this obligation",
  "solver-error": "the solver itself failed (e.g. a timeout); no verdict",
  unavailable: "the selected backend is not available; no verdict",
  unknown: "the backend returned unknown; treated as unverified",
};

// The verdict word shown in the status badge -- the ONE clean status the pane
// body carries.  The mode header ("obligation") already names what is being
// shown, so the body must not repeat it: it shows only this badge (see
// paneBodyLines / renderVc).  "solver-error" reads as two words; the rest are
// their own status name.
const STATUS_LABEL = {
  proved: "proved",
  disproved: "disproved",
  unproved: "unproved",
  failed: "rejected",
  "solver-error": "solver error",
  unavailable: "unavailable",
  unknown: "unknown",
};

function statusLabel(status) {
  return STATUS_LABEL[status] || status;
}

// A verdict spelled out prominently, so a skimming user cannot conflate a
// refuted goal with one automation merely gave up on.  This is the single most
// important honesty property of the pane: an UNPROVED goal may still hold (no
// counterexample was found), whereas a DISPROVED goal is genuinely false (and
// gets its own counterexample section instead of a note).
const STATUS_NOTE = {
  unproved:
    "Unproved: automation gave up. No counterexample was found, so the goal may still hold.",
  "solver-error":
    "Solver error: the solver itself failed (e.g. a timeout). No verdict either way.",
  unavailable: "Unavailable: the selected backend could not be run. No verdict either way.",
  failed: "The verifier rejected this obligation.",
  unknown: "Unknown: the backend returned no verdict either way.",
};

// ---------------------------------------------------------------------------
// Verdict TOKEN vocabulary (the PROOF zone's loud, colored depth-0 headline).
//
// The token is the ONE thing a skimmer over-reads, so its words are strict and
// its honesty qualifiers are WELDED to it (never dropped by compact): a
// disproved token always carries `· no witness` / `· witness`, and the
// off-obligation `context` token always carries `· approximate` and is grey and
// NOT a verdict.  A compile error has no obligation verdict at all.
const TOKEN_GLYPH = {
  proved: "✓",
  disproved: "✗",
  unproved: "⚠",
  failed: "✗",
  "solver-error": "⚠",
  unavailable: "⚠",
  unknown: "✗",
};
const TOKEN_LABEL = {
  proved: "PROVED",
  disproved: "DISPROVED",
  unproved: "UNPROVED",
  failed: "REJECTED",
  "solver-error": "SOLVER ERROR",
  unavailable: "UNAVAILABLE",
  unknown: "UNKNOWN",
};

// The verdict token text for an obligation status, with any welded honesty
// qualifier already appended (e.g. "✗ DISPROVED · no witness").  This is the
// depth-0 headline; the underline swatch beside it is presentation chrome (a
// CSS echo of the editor mark) that carries no text.
function verdictTokenText(status, qualifier) {
  const glyph = TOKEN_GLYPH[status] || "✗";
  const label = TOKEN_LABEL[status] || String(status).toUpperCase();
  return glyph + " " + label + (qualifier ? " · " + qualifier : "");
}

// The grey off-obligation token: a state-at-cursor view is NOT a verdict, and
// its approximate nature is pinned to the token so compact can never drop it.
const CONTEXT_TOKEN_TEXT = "◦ CONTEXT · approximate";

// A VC's code anchor `[file:]line:col` (1-based, as the editor shows the
// caret), derived purely from the obligation so the terminal tool and the
// browser render the identical string.  The file segment appears only in
// multi-file mode (where the dump tags each VC with its unit); single-buffer
// obligations carry no file and read as a bare `line:col`.
function anchorText(vc) {
  const line = vc.start.line + 1;
  const col = vc.start.col + 1;
  return (vc.file ? vc.file + ":" : "") + line + ":" + col;
}

// ---------------------------------------------------------------------------
// STATUS zone roll-up: a fail-closed buffer verdict folded over the same
// obligations the PROOF pane renders, PLUS the compile outcome the browser
// feeds in (compileError / errorCount).  Kept here so the roll-up rule is the
// single source, exactly like the per-obligation vocabulary above.
//
// HONESTY (fail-closed): `✓ verified · N/N` appears ONLY when the buffer
// compiles AND every obligation proved.  A compile error shows NO obligation
// verdict at all — the obligations read `unavailable`, never proved/verified.
// A disproved obligation dominates an unproved one (a genuine falsehood is
// louder than an automation gap).  `opts` =
// { compiles, errorCount, unavailable }.
function statusRollup(vcs, opts) {
  vcs = vcs || [];
  opts = opts || {};
  const compiles = opts.compiles !== false;
  const errorCount = Number(opts.errorCount) > 0 ? Number(opts.errorCount) : 0;
  const suppliedSummary = opts.obligationSummary;
  const aggregate = suppliedSummary
    ? adaptObligationSummary(suppliedSummary, vcs, suppliedSummary.hidden)
    : { summary: summaryFromVcs(vcs, 0), valid: true };
  const summary = aggregate.summary;
  const counts = summary.statuses || {};
  const total = Number(summary.total) || 0;
  const proved = Number(counts.proved) || 0;
  const disproved = Number(counts.disproved) || 0;
  const unavailable = Number(counts.unavailable) || 0;
  const solverError = Number(counts["solver-error"]) || 0;
  const unknown =
    (Number(counts.unproved) || 0) +
    (Number(counts.failed) || 0) +
    (Number(counts.unknown) || 0);
  const other = unavailable + solverError + unknown;
  const outcome = opts.outcome && typeof opts.outcome.kind === "string"
    ? opts.outcome.kind
    : null;
  const unavailableOutcome = {
    "backend-unavailable": "check unavailable · backend",
    "compiler-unavailable": "check unavailable · compiler",
    "compiler-crashed": "check unavailable · compiler crashed",
    timeout: "check unavailable · timeout",
    "invalid-request": "check unavailable · request",
    unknown: "check unavailable",
  };

  if (!aggregate.valid) {
    return {
      status: "unavailable",
      glyph: "⚠",
      label: "obligations unavailable",
      detail: "the compiler returned a malformed obligation summary",
      counts: { total, proved, disproved, other },
    };
  }

  if (outcome && unavailableOutcome[outcome]) {
    return {
      status: "unavailable",
      glyph: "⚠",
      label: unavailableOutcome[outcome],
      detail: opts.outcome.message || null,
      counts: { total, proved, disproved, other },
    };
  }
  if (outcome === "syntax" || outcome === "type-mode") {
    return {
      status: "error",
      glyph: "✗",
      label:
        outcome === "syntax"
          ? (errorCount > 1 ? errorCount + " syntax errors" : "syntax error")
          : (errorCount > 1 ? errorCount + " type/mode errors" : "type/mode error"),
      detail: "obligations unavailable until the source errors are fixed",
      counts: { total, proved, disproved, other },
    };
  }

  // A compile / type error: obligations are UNAVAILABLE, never a verdict.
  if (!compiles) {
    return {
      status: "error",
      glyph: "✗",
      label:
        errorCount > 1 ? errorCount + " type/mode errors" : "type/mode error",
      detail: "obligations unavailable until the errors are fixed",
      counts: { total, proved, disproved, other },
    };
  }
  // The compile completed, but no trustworthy VC sidecar was available (the
  // legacy-compiler degrade path).  Never turn an absent dump into the green
  // "no obligations" verdict.
  if (opts.unavailable) {
    return {
      status: "unavailable",
      glyph: "⚠",
      label: "obligations unavailable",
      detail: "the compiler did not provide per-obligation data",
      counts: { total, proved, disproved, other },
    };
  }
  if (disproved > 0) {
    return {
      status: "disproved",
      glyph: "✗",
      label: disproved + " disproved",
      detail: proved + "/" + total + " obligations proved",
      counts: { total, proved, disproved, other },
    };
  }
  if (unavailable > 0) {
    return {
      status: "unavailable",
      glyph: "⚠",
      label: unavailable + " backend unavailable",
      detail: proved + "/" + total + " obligations proved",
      counts: { total, proved, disproved, other },
    };
  }
  if (solverError > 0) {
    return {
      status: "solver-error",
      glyph: "⚠",
      label: solverError + " solver error" + (solverError > 1 ? "s" : ""),
      detail: proved + "/" + total + " obligations proved",
      counts: { total, proved, disproved, other },
    };
  }
  if (other > 0) {
    return {
      status: "unproved",
      glyph: "⚠",
      label: other + " unproved",
      detail: proved + "/" + total + " obligations proved",
      counts: { total, proved, disproved, other },
    };
  }
  if (total > 0) {
    return {
      status: "verified",
      glyph: "✓",
      label: "verified · " + proved + "/" + total,
      detail: null,
      counts: { total, proved, disproved, other },
    };
  }
  return {
    status: "ok",
    glyph: "✓",
    label: "no obligations",
    detail: null,
    counts: { total, proved, disproved, other },
  };
}

// ---------------------------------------------------------------------------
// The /vcs adapter (moved verbatim from app.js) -- the one integration point
// between the compiler's per-obligation dump and the UI.
// ---------------------------------------------------------------------------

// A predicate carries a pretty `display` (schema v2, source-like) and the
// `raw` app-syntax text; `display` falls back to `raw` so v1 and v2 render the
// same way.  Also accepts a bare string (a plainer payload) defensively.
function adaptPredicate(pred) {
  if (pred && typeof pred === "object") {
    const raw = pred.raw != null ? String(pred.raw) : "";
    return { display: pred.display != null ? String(pred.display) : raw, raw };
  }
  const text = pred != null ? String(pred) : "";
  return { display: text, raw: text };
}

// A hypothesis: an optional binder `name` (schema v2 origin; positional
// h0/h1 in the renderer otherwise), its predicate, and an optional source
// `span` (present when the origin is recoverable) that makes it clickable.
function validateEditorSpan(span, context) {
  if (!span || typeof span !== "object") return null;
  // Adapter-normalized spans normally omit the raw ghost bit. If malformed
  // data retains it, only an explicit false value is placeable.
  if (span.ghost !== undefined && span.ghost !== false) return null;
  const file = span.file == null ? null : span.file;
  if (file !== null && (typeof file !== "string" || !file)) return null;
  context = context || {};
  let lines = null;
  if (context.linesByFile) {
    if (file === null || !Object.prototype.hasOwnProperty.call(context.linesByFile, file)) {
      return null;
    }
    lines = context.linesByFile[file];
  } else if (Array.isArray(context.lines)) {
    if (context.expectedFile != null && file != null && file !== context.expectedFile) {
      return null;
    }
    lines = context.lines;
  }
  const point = (value) =>
    value &&
    typeof value === "object" &&
    Number.isSafeInteger(value.line) &&
    Number.isSafeInteger(value.col) &&
    value.line >= 0 &&
    value.col >= 0
      ? { line: value.line, col: value.col }
      : null;
  const start = point(span.start);
  const end = point(span.end);
  if (!start || !end) return null;
  if (start.line > end.line || (start.line === end.line && start.col > end.col)) {
    return null;
  }
  if (lines !== null) {
    if (
      !Array.isArray(lines) ||
      start.line >= lines.length ||
      end.line >= lines.length ||
      start.col > String(lines[start.line]).length ||
      end.col > String(lines[end.line]).length
    ) {
      return null;
    }
  }
  return { file, start, end };
}

function adaptHyp(raw, spanContext) {
  const pred = adaptPredicate(
    raw && (raw.display != null || raw.raw != null) ? raw : raw && raw.text
  );
  const span = validateEditorSpan(raw && raw.span, spanContext);
  return {
    name: raw && raw.name != null ? String(raw.name) : null,
    // The origin kind (schema v2): "binder" for a real bound variable, else
    // "contract-argument"/"application"/"branch" for a fact named after a
    // callee/function/condition rather than a variable in scope here.  The
    // off-obligation "known here" view keeps only binders (see stateAtCursor).
    kind: raw && raw.kind != null ? String(raw.kind) : null,
    display: pred.display,
    raw: pred.raw,
    span,
    // Tri-state: false/true only when the active backend actually reports fact
    // usage.  null means no capability; it must never inherit Lean's fade.
    used: raw && typeof raw.used === "boolean" ? raw.used : null,
    // Every site that introduced this proposition, spans validated against the
    // buffer.  `null` means the provenance is not fully known -- an older
    // payload without the field, a malformed entry, or a span that will not
    // place -- and a consumer asking "did this site's fact go unread" has to
    // treat such a fact as possibly belonging to any site.
    producers: adaptProducers(raw && raw.producers, spanContext),
    // Per-backend reading of THIS fact, for a cross-check.  A backend absent
    // from the map reported no accounting; that is not the same as reporting
    // that it left the fact unread.
    usedBy: adaptUsedBy(raw && raw.used_by),
  };
}

function adaptProducers(raw, spanContext) {
  if (!Array.isArray(raw)) return null;
  const producers = [];
  for (const entry of raw) {
    if (!entry || typeof entry !== "object") return null;
    const span = validateEditorSpan(entry.span, spanContext);
    if (!span) return null;
    producers.push({
      name: entry.name != null ? String(entry.name) : null,
      kind: entry.kind != null ? String(entry.kind) : null,
      span,
    });
  }
  return producers;
}

function adaptUsedBy(raw) {
  if (!raw || typeof raw !== "object" || Array.isArray(raw)) return null;
  const usedBy = {};
  let any = false;
  for (const backend of ["lean", "z3", "oxsmt"]) {
    if (typeof raw[backend] === "boolean") {
      usedBy[backend] = raw[backend];
      any = true;
    }
  }
  return any ? usedBy : null;
}

function adaptBackendResult(raw) {
  if (!raw || !["lean", "z3", "oxsmt"].includes(raw.backend)) return null;
  return {
    backend: raw.backend,
    status: normalizeStatus(raw.status),
    detail: raw.detail != null ? String(raw.detail) : null,
    factUsage: raw.fact_usage === true,
  };
}

function summaryFromVcs(vcs, hidden) {
  const statuses = {
    proved: 0,
    disproved: 0,
    unproved: 0,
    "solver-error": 0,
    unavailable: 0,
    unknown: 0,
  };
  (vcs || []).forEach((vc) => {
    const status = normalizeStatus(vc.status);
    statuses[status] = (statuses[status] || 0) + 1;
  });
  const hiddenCount = Number(hidden) > 0 ? Number(hidden) : 0;
  statuses.unknown += hiddenCount;
  const hiddenStatuses = {
    proved: 0,
    disproved: 0,
    unproved: 0,
    "solver-error": 0,
    unavailable: 0,
    unknown: hiddenCount,
  };
  return {
    total: (vcs || []).length + hiddenCount,
    statuses,
    hidden: hiddenCount,
    hidden_statuses: hiddenStatuses,
  };
}

function adaptObligationSummary(raw, vcs, hidden) {
  if (!raw || typeof raw !== "object" || !raw.statuses) {
    return { summary: summaryFromVcs(vcs, hidden), valid: raw == null };
  }
  if (!Number.isSafeInteger(hidden) || hidden < 0) {
    return { summary: summaryFromVcs(vcs, 1), valid: false };
  }
  const statuses = {};
  const hiddenStatuses = {};
  let sum = 0;
  let hiddenSum = 0;
  const rawHiddenStatuses = raw.hidden_statuses;
  if (!rawHiddenStatuses || typeof rawHiddenStatuses !== "object") {
    return { summary: summaryFromVcs(vcs, Math.max(1, hidden)), valid: false };
  }
  for (const status of [
    "proved",
    "disproved",
    "unproved",
    "solver-error",
    "unavailable",
    "unknown",
  ]) {
    const value = raw.statuses[status];
    const hiddenValue = rawHiddenStatuses[status];
    if (
      !Number.isSafeInteger(value) ||
      value < 0 ||
      !Number.isSafeInteger(hiddenValue) ||
      hiddenValue < 0 ||
      hiddenValue > value
    ) {
      return {
        summary: summaryFromVcs(vcs, Math.max(1, hidden)),
        valid: false,
      };
    }
    statuses[status] = value;
    hiddenStatuses[status] = hiddenValue;
    sum += value;
    hiddenSum += hiddenValue;
  }
  const total = raw.total;
  const summaryHidden = raw.hidden;
  if (
    !Number.isSafeInteger(total) ||
    total < 0 ||
    total !== sum ||
    !Number.isSafeInteger(summaryHidden) ||
    summaryHidden < 0 ||
    summaryHidden > total ||
    summaryHidden !== hidden ||
    hiddenSum !== summaryHidden ||
    vcs.length !== total - summaryHidden
  ) {
    return {
      summary: summaryFromVcs(vcs, Math.max(1, hidden)),
      valid: false,
    };
  }
  return {
    summary: {
      total,
      statuses,
      hidden: summaryHidden,
      hidden_statuses: hiddenStatuses,
    },
    valid: true,
  };
}

// The one integration point between the compiler's per-obligation dump and
// the UI.  It PURELY normalizes the /vcs payload into { vcs, unavailable,
// hidden } -- the shape the pane and source marks render from -- consuming
// schema v2 (display/origin) with a fallback to v1 raw text.
function adaptVcs(payload, spanContext) {
  const declaredHidden = payload && payload.hidden;
  const hiddenValid = Number.isSafeInteger(declaredHidden) && declaredHidden >= 0;
  const hidden = hiddenValid
    ? declaredHidden
    : 0;
  if (!payload || !Array.isArray(payload.vcs)) {
    const aggregate = adaptObligationSummary(
      payload && payload.obligation_summary,
      [],
      hidden
    );
    return {
      vcs: [],
      unavailable: true,
      hidden,
      summary: aggregate.summary,
      unavailableReason:
        !hiddenValid || !aggregate.valid
          ? "malformed-vc-data"
          : payload && payload.unavailable_reason
          ? String(payload.unavailable_reason)
          : "unknown",
    };
  }
  const normalized = payload.vcs
    .map((raw, index) => {
      const span = validateEditorSpan(
        raw && raw.span
          ? { ...raw.span, file: raw.file != null ? raw.file : raw.span.file }
          : null,
        spanContext
      );
      if (!span) return null;
      return {
        id: raw.id != null ? raw.id : index,
        // The unit this VC anchors in (multi-file): drives which buffer paints
        // its mark and which tab a cross-unit obligation jumps to.  Null on the
        // single-buffer path, where every VC belongs to the one buffer.
        file: span.file,
        // Normalized against the allowlist before it reaches the "vc-"/"badge-"
        // class-attribute sink; an unrecognized verdict fails closed.
        status: normalizeStatus(raw.status),
        kind: raw.kind || "",
        start: span.start,
        end: span.end,
        goal: adaptPredicate(raw.goal),
        hypotheses: (raw.hypotheses || []).map((hyp) => adaptHyp(hyp, spanContext)),
        counterexample:
          Array.isArray(raw.counterexample) && raw.counterexample.length
            ? raw.counterexample
            : null,
        // The solver diagnostic (populated on a failure) and the positive
        // theorem the compiler emitted (shown behind a disclosure).
        detail: raw.detail != null ? String(raw.detail) : null,
        lean: raw.generated_lean != null ? String(raw.generated_lean) : null,
        backends: Array.isArray(raw.backends)
          ? raw.backends.map(adaptBackendResult).filter((x) => x !== null)
          : null,
      };
    })
    .filter((vc) => vc !== null);
  const aggregate = adaptObligationSummary(
    payload.obligation_summary,
    normalized,
    hidden
  );
  const unavailable = !!payload.unavailable || !hiddenValid || !aggregate.valid;
  return {
    vcs: normalized,
    unavailable,
    hidden,
    summary: aggregate.summary,
    unavailableReason: !hiddenValid || !aggregate.valid
      ? "malformed-vc-data"
      : payload.unavailable_reason
      ? String(payload.unavailable_reason)
      : "unknown",
  };
}

// ---------------------------------------------------------------------------
// Calls that introduced a proposition no obligation read
// ---------------------------------------------------------------------------

// A call whose only product is a proposition earns its place by being read.
// Whether it was read is a property of the WHOLE accepted result, not of any
// one obligation, so this folds over every obligation the run produced and
// answers only when the fold is complete.  Every gap -- an obligation that
// could not be placed, a fact whose introducers are not fully reported, a
// backend that reported no accounting, a result that was not accepted --
// leaves the question open, and an open question produces no answer.  Saying
// nothing costs a reader a hint; saying it wrongly costs them a call their
// proof depends on.

function lemmaSpanKey(span) {
  if (!span || !span.start || !span.end) return null;
  return (
    (span.file || "") +
    ":" +
    span.start.line +
    ":" +
    span.start.col +
    "-" +
    span.end.line +
    ":" +
    span.end.col
  );
}

// Which backends this obligation was actually decided by, or null outside a
// cross-check.  Only a backend that advertises fact usage can be asked
// whether it read a fact; one that cannot is not evidence either way, so its
// silence must not read as "left it unread".
function obligationBackends(vc) {
  if (!Array.isArray(vc.backends) || vc.backends.length === 0) return null;
  return vc.backends.map((b) => b.backend);
}

// Was this fact read while deciding this obligation?  "unknown" whenever the
// accounting is absent for any backend that decided it: under a cross-check
// two backends may reach the same verdict off different facts, so a fact is
// read if ANY of them read it, and the question can only be closed when all
// of them answered.
function factUsage(hyp, backends) {
  if (backends === null) {
    if (typeof hyp.used !== "boolean") return "unknown";
    return hyp.used ? "used" : "unread";
  }
  const usedBy = hyp.usedBy;
  if (!usedBy) return "unknown";
  let used = false;
  for (const backend of backends) {
    if (typeof usedBy[backend] !== "boolean") return "unknown";
    if (usedBy[backend]) used = true;
  }
  return used ? "used" : "unread";
}

// [obligations] must be EVERY obligation of the accepted result, across every
// unit -- a fold over a subset would call a call unread on the strength of
// having not looked.  [complete] is the caller's assertion that it is; it
// carries what only the caller knows (a superseded response, an invalid unit,
// an obligation dropped before this model saw it).
function unnecessaryLemmaCalls(options) {
  const opts = options || {};
  const lemmaCalls = opts.lemmaCalls;
  const obligations = Array.isArray(opts.obligations) ? opts.obligations : null;
  const empty = { calls: [], backendScope: [] };
  if (!opts.complete) return empty;
  if (obligations === null) return empty;
  // Absent channel: an older compiler that never names such a call is not a
  // buffer that holds none.
  if (!Array.isArray(lemmaCalls) || lemmaCalls.length === 0) return empty;
  // Not an accepted result.  A fact's accounting comes from the proof that
  // read it; where there is no proof there is no accounting, and an
  // obligation that failed may well have needed the very fact whose site is
  // in question.
  if (!obligations.every((vc) => vc.status === "proved")) return empty;

  const backendScope = new Set();
  const evidence = new Map();
  for (const vc of obligations) {
    const backends = obligationBackends(vc);
    if (backends === null) {
      if (opts.backend) backendScope.add(String(opts.backend));
    } else {
      for (const backend of backends) backendScope.add(backend);
    }
    for (const hyp of vc.hypotheses || []) {
      // A fact whose introducers are not fully reported could have come from
      // any of these calls, so no call can be cleared while one is present.
      if (!Array.isArray(hyp.producers)) return empty;
      const usage = factUsage(hyp, backends);
      for (const producer of hyp.producers) {
        const key = lemmaSpanKey(producer.span);
        if (key === null) return empty;
        const entry = evidence.get(key) || { used: false, unknown: false };
        if (usage === "used") entry.used = true;
        if (usage === "unknown") entry.unknown = true;
        evidence.set(key, entry);
      }
    }
  }

  const calls = [];
  for (const call of lemmaCalls) {
    const key = lemmaSpanKey(call);
    if (key === null) continue;
    // The compiler did not see the proposition reach the fact environment, so
    // there is nothing to say it went unread.
    if (call.introduced !== true) continue;
    const entry = evidence.get(key);
    if (entry && (entry.used || entry.unknown)) continue;
    calls.push({
      file: call.file != null ? call.file : null,
      start: call.start,
      end: call.end,
      name: call.name != null ? String(call.name) : null,
    });
  }
  return { calls, backendScope: [...backendScope].sort() };
}

const LEMMA_UNUSED_HINT = "lemma facts unused by every verification condition";

// The hover text names the backends the answer holds for, so a reader is
// never left to assume it covers ones that were not consulted.
function lemmaUnusedHint(backendScope) {
  if (!Array.isArray(backendScope) || backendScope.length === 0) {
    return LEMMA_UNUSED_HINT;
  }
  return LEMMA_UNUSED_HINT + " (" + backendScope.join(", ") + ")";
}

// ---------------------------------------------------------------------------
// Cursor / span geometry (moved verbatim from app.js).
// ---------------------------------------------------------------------------

// Containment is INCLUSIVE of both edges: a caret at the start column and a
// caret at the end column both count as "on" the span.  A one-character value
// like `7` spans [c, c+1); with an exclusive end the caret resting just after
// the value (ch == end) would go blank, so the line-end caret right after an
// argument never showed its obligation.  Inclusive-end matches how the editor
// reads "the caret is on this token" from either side.
function contains(range, position) {
  const afterStart =
    position.line > range.start.line ||
    (position.line === range.start.line && position.ch >= range.start.col);
  const beforeEnd =
    position.line < range.end.line ||
    (position.line === range.end.line && position.ch <= range.end.col);
  return afterStart && beforeEnd;
}

function spanSize(range) {
  return (
    (range.end.line - range.start.line) * 1000000 +
    range.end.col -
    range.start.col
  );
}

// The structured CURSOR-zone readout shared by the browser and its fidelity
// test.  Types and modes are independent compiler facts: choose the tightest
// containing range from each channel, then show both when both exist.  A dual
// checked/imposed label is used only when that selected type range is the exact
// compiler-established imposition range; a containing imposition range must
// not relabel a smaller branch expression.  The compiler's strings are kept
// verbatim.
function cursorReadoutLines(
  expressionTypes,
  refinementTypes,
  identifierModes,
  cursor,
  imposedTypes
) {
  const smallest = (ranges) => {
    const matches = (Array.isArray(ranges) ? ranges : []).filter(
      (range) => range && range.start && range.end && contains(range, cursor)
    );
    matches.sort((left, right) => spanSize(left) - spanSize(right));
    return matches[0] || null;
  };
  const typeRange = smallest(
    (Array.isArray(expressionTypes) ? expressionTypes : []).concat(
      Array.isArray(refinementTypes) ? refinementTypes : []
    )
  );
  const modeRange = smallest(identifierModes);
  const lines = [];
  const sameSpan = (left, right) =>
    left &&
    right &&
    left.start.line === right.start.line &&
    left.start.col === right.start.col &&
    left.end.line === right.end.line &&
    left.end.col === right.end.col;
  const imposedRange = typeRange
    ? (Array.isArray(imposedTypes) ? imposedTypes : []).find(
        (range) =>
          range &&
          range.start &&
          range.end &&
          sameSpan(range, typeRange) &&
          String(range.imposed_type) === String(typeRange.type)
      )
    : null;
  if (imposedRange) {
    lines.push({
      label: "checked: ",
      text: String(imposedRange.checked_type),
      kind: "checked",
    });
    lines.push({
      label: "imposed: ",
      text: String(imposedRange.imposed_type),
      kind: "imposed",
    });
  } else if (typeRange && typeRange.type != null) {
    lines.push({ label: "", text: String(typeRange.type), kind: "type" });
  }
  if (modeRange && modeRange.mode != null)
    lines.push({ label: "", text: String(modeRange.mode), kind: "mode" });
  if (!lines.length)
    lines.push({
      label: "",
      text: "No inferred expression type at the cursor.",
      kind: "empty",
    });
  return lines;
}

function cursorReadout(
  expressionTypes,
  refinementTypes,
  identifierModes,
  cursor,
  imposedTypes
) {
  return cursorReadoutLines(
    expressionTypes,
    refinementTypes,
    identifierModes,
    cursor,
    imposedTypes
  )
    .map((line) => line.label + line.text)
    .join("\n");
}

// A span endpoint carries a UTF-16 column as `col` (VC/hypothesis spans) or
// `ch` (a CodeMirror cursor); read whichever is present so the same comparator
// serves both.
function posCol(point) {
  return point.col != null ? point.col : point.ch != null ? point.ch : 0;
}

// Lexical order on {line, col|ch}: negative if `a` precedes `b`.
function posCmp(a, b) {
  return a.line !== b.line ? a.line - b.line : posCol(a) - posCol(b);
}

// Order two obligations under the caret so the INNERMOST (smallest containing
// span) wins.  On a true size tie -- e.g. a shared-edge caret that the inclusive
// end places in two adjacent same-size spans -- prefer the span being ENTERED
// (its start is exactly at the caret); otherwise keep the dump's order (a stable
// sort leaves them in VC id order).  This matches the optimal attribution in
// review/caret_attribution_audit.py (innermost, then id).  Two branch VCs that
// share the whole-`if` span (the compiler's coarse `location`, tracked as #144)
// tie here and neither starts at an interior caret, so the deterministic winner
// stands -- that residue is expected until the per-branch span lands.
function vcOrder(a, b, cursor) {
  const bySize = spanSize(a) - spanSize(b);
  if (bySize !== 0) return bySize;
  const startsHere = (vc) =>
    vc.start.line === cursor.line && vc.start.col === cursor.ch ? 0 : 1;
  return startsHere(a) - startsHere(b);
}

// A kind-derived label for a fact the dump left unnamed.  Each phrase reads as
// what the fact IS -- a category, never an OCaml identifier -- so a labelled
// fact can never masquerade as a source variable that isn't there (the phrases
// contain a space or are plainly a noun, so `branch condition : x >= 0` cannot
// be mistaken for a binder).  Only kinds that genuinely carry no source binder
// are mapped; a real bound variable (kind "binder") keeps the positional form
// below, because it IS a variable whose name the dump merely did not recover.
var KIND_LABEL = {
  branch: "branch condition",
  annotation: "annotation",
  "contract-argument": "argument",
  application: "result",
};

// A hypothesis's label: its binder name when the dump recovered a meaningful
// one; else a phrase derived from the fact's `kind` (a branch condition, an
// argument, ...) so an anonymous fact reads as what it is rather than an opaque
// h0/h1; else a positional h0/h1/... as a last resort (an unnamed real binder,
// or an unrecognized kind).  A literal "_" binder carries no information as a
// label, so it too falls through to a phrase/positional form.
function hypLabel(hyp, index) {
  if (hyp.name && hyp.name !== "_") return hyp.name;
  var kind = hyp.kind;
  if (kind && kind !== "binder" && KIND_LABEL[kind]) return KIND_LABEL[kind];
  return "h" + index;
}

// The approximate off-obligation state: the facts known "here" derived purely
// from the obligations the dump already carries, so the pane is never dead.
//
// HONESTY (load-bearing).  Two filters keep this view from asserting a fact
// that does not hold at the caret:
//
//  1. ONLY REAL IN-SCOPE BINDERS.  A fact enters only when its origin kind is
//     "binder" -- an actual bound variable (a parameter or a let).  Every other
//     kind is a fact named after something that is NOT a variable live here and
//     must never masquerade as one: a "contract-argument" is a concrete value
//     at a *call site* carrying the *callee's* parameter name (so `a : 3 > 0`
//     asserts the literal 3 under the parameter name `a`); an "application" is
//     a result-of-call fact named after the function; a "branch" condition
//     (null name) holds only inside its arm.  Excluding all non-binder kinds
//     removes the fabricated facts outright.
//
//  2. SCOPED TO THE ENCLOSING DEFINITION.  A binder is only in scope within its
//     own definition.  The compiler attaches a binder fact to an obligation
//     ONLY where that binder is in scope, so the union of the spans of the
//     obligations that carry a binder -- extended back to its declaration line
//     -- is a sound over-approximation of its scope footprint.  A fact is shown
//     only when the caret's line lies within that footprint, which keeps a
//     sibling definition's parameter (whose footprint is that other
//     definition's lines) from leaking here.
//
// What remains is still an APPROXIMATION and may UNDER-report (a binder live
// past the last obligation that mentions it, or a null-named monotone fact, is
// omitted); the pane labels the view "approximate" accordingly.  It never
// OVER-reports a fact whose binding is out of scope, and never labels a
// concrete call-site value as a parameter.
//
// A fact is shown when its binder is introduced at or above the caret (its
// origin span ends before the caret) and the caret is within its footprint.
// Facts are de-duplicated by label+predicate and presented in source order.
function stateAtCursor(vcs, cursor) {
  // Per-binder footprint: identity is name + origin span; the footprint spans
  // from the binder's declaration line to the last obligation line carrying it.
  const binderIdentity = (h) =>
    h.name +
    "|::|" +
    h.span.start.line +
    ":" +
    posCol(h.span.start) +
    "-" +
    h.span.end.line +
    ":" +
    posCol(h.span.end);
  const footprint = new Map();
  vcs.forEach((vc) => {
    vc.hypotheses.forEach((h) => {
      if (h.kind !== "binder" || !h.name || !h.span) return;
      const key = binderIdentity(h);
      const cur = footprint.get(key);
      const startLine = h.span.start.line;
      const endLine = vc.end.line;
      if (!cur) footprint.set(key, { startLine, endLine });
      else {
        cur.startLine = Math.min(cur.startLine, startLine);
        cur.endLine = Math.max(cur.endLine, endLine);
      }
    });
  });

  const seen = new Set();
  const facts = [];
  vcs.forEach((vc) => {
    vc.hypotheses.forEach((h) => {
      if (h.kind !== "binder" || !h.name || !h.span) return;
      if (posCmp(h.span.end, cursor) > 0) return;
      const fp = footprint.get(binderIdentity(h));
      if (!fp || cursor.line < fp.startLine || cursor.line > fp.endLine) return;
      const key = h.name + "|::|" + h.display;
      if (seen.has(key)) return;
      seen.add(key);
      facts.push(h);
    });
  });
  facts.sort((a, b) => posCmp(a.span.start, b.span.start));
  return facts;
}

// ---------------------------------------------------------------------------
// The view-model: proofPaneModel(vcs, cursor, opts) -> a pure description of
// exactly what the proof pane shows for `cursor`, with `opts` = { compact,
// fadeUnused } baked in (so both sinks render only what is actually visible).
//
// `vcs` is the already-adapted, already-file-filtered obligation list (the
// caller filters by active unit in multi-file mode, exactly as the browser
// does before rendering the pane).  `cursor` is { line, ch } (0-based, editor
// coordinates).  `unavailable`/`hidden` come from the same adaptVcs result.
// ---------------------------------------------------------------------------

// One context row, shared by the obligation view and the state-at-cursor view.
// `display` is raw text (the browser tokenizes it for colour; the text sink
// prints it verbatim -- both have identical textual content).
function contextRow(hyp, index, fadeUnused) {
  return {
    label: hypLabel(hyp, index),
    display: hyp.display,
    faded: !!fadeUnused && hyp.used === false,
    linked: !!hyp.span,
  };
}

// The obligation sub-model: every piece of the PROOF pane, in order, each
// tagged with the depth (0/1/2) at which it appears.  Nothing is gated by
// compact here -- that gating is a per-depth decision made in paneBodyLines and
// the browser renderer, so the sinks stay pure formatters of ONE description.
//
// Depth 0 = the goal line, which carries the verdict (status glyph + colour +
//   welded honesty qualifier), plus the hypotheses.
// Depth 1 = the proof state (kind tag, code anchor, refutation reason, notes,
//   sibling count) -- shown in full, hidden in compact.
// Depth 2 = the escape hatches (raw predicate, solver detail, generated Lean)
//   -- ALWAYS one click away as a disclosure, in both views.
function obligationModel(vc, overlapping, opts, selectionIndex, selectionCount) {
  const fadeUnused = opts ? opts.fadeUnused !== false : true;
  const status = vc.status;

  // The honesty qualifier welded to a disproved token in BOTH views: a
  // concrete witness distinguishes a validated counterexample from a bare
  // refutation (grind often refutes without yielding a model).
  //
  // `· no witness` is shown only when a witness would be MEANINGFUL -- i.e. the
  // goal has >= 1 free variable a counterexample model could assign.  A GROUND
  // goal like `2 = 1` has none, so `no witness` there is vacuous noise: the `✗`
  // verdict alone is honest, and the qualifier is suppressed.  Witness-relevance
  // signal (client-side): the disproved VC carries >= 1 hypothesis, OR its
  // generated Lean quantifies over >= 1 free variable (a `v_N` binder -- present
  // even when the goal's free var is not itself a named hypothesis, which the
  // hypothesis count alone would miss).  A concrete witness, when present, is
  // always shown regardless.
  const witnessRelevant =
    vc.hypotheses.length > 0 || /\bv_\d+\b/.test(vc.lean || "");
  const tokenQualifier =
    status === "disproved"
      ? vc.counterexample
        ? "witness"
        : witnessRelevant
        ? "no witness"
        : null
      : null;

  let counterexample = null;
  // The refutation detail (depth 1).  The no-witness FACT already rides the
  // token at depth 0; this is the expanded reason / concrete model shown in
  // full.  A witness on any non-disproved status is only ever a candidate.
  if (status === "disproved") {
    if (vc.counterexample) {
      counterexample = {
        heading: "candidate counterexample (unbounded-int model)",
        witness: vc.counterexample,
        noneText: null,
      };
    } else {
      counterexample = {
        heading: "refutation",
        witness: null,
        noneText:
          "Disproved: the solver refuted this goal but produced no concrete witness.",
      };
    }
  } else if (vc.counterexample) {
    counterexample = {
      heading: "candidate counterexample (unbounded-int model)",
      witness: vc.counterexample,
      noneText: null,
    };
  }

  return {
    status,
    // Depth-0 headline pieces.  The verdict is conveyed by COLORING the goal
    // line (see paneBodyLines / renderVc): a small status glyph leads the line
    // and the whole line is tinted by verdict.  A disproved goal keeps its
    // welded honesty qualifier (`· no witness` / `· witness`) so a refutation
    // can never be skimmed as anything else.  There is no separate loud token
    // line or underline swatch any more.
    goalGlyph: TOKEN_GLYPH[status] || "✗",
    goalQualifier: tokenQualifier,
    anchor: anchorText(vc),
    goalDisplay: vc.goal.display,
    // Depth-1 pieces.  The obligation kind is a small dim tag (annotation /
    // contract), full-only.
    kindLabel: vc.kind || null,
    context: vc.hypotheses.map((h, i) => contextRow(h, i, fadeUnused)),
    // Disproved gets its refutation section (depth 1); every other non-proved
    // status gets a note so "unproved" is never skimmed as "disproved".
    statusNote: status === "disproved" ? null : STATUS_NOTE[status] || null,
    counterexample,
    overlapping: overlapping,
    overlapIndex: selectionIndex,
    overlapCount: selectionCount,
    // Depth-2 escape hatches (disclosures, present in both views when the data
    // exists).  The raw app-syntax appears only when it adds something over the
    // pretty display (schema v2).
    rawPredicate:
      vc.goal.raw && vc.goal.raw !== vc.goal.display ? vc.goal.raw : null,
    solverDetail: vc.detail || null,
    // The positive theorem the compiler handed the solver (a complete,
    // self-contained Lean file), the honesty escape hatch.
    lean: vc.lean || null,
  };
}

// The verdict legend: shown only while some obligation failed (exactly when
// the red underlines need explaining).  Mirrors renderLegend's entry set.
function legendModel(vcs) {
  const anyFail = vcs.some((vc) => FAILED_STATUSES.includes(vc.status));
  if (!anyFail) return { visible: false, entries: [] };
  // "disproved (refuted)" -- NOT "(counterexample)": a disproved goal is refuted
  // (its negation proved), but grind often yields no concrete witness, so the
  // key must not imply a counterexample always exists.
  const entries = [
    ["proved", "proved"],
    ["disproved", "disproved (refuted)"],
    ["unproved", "unproved (no witness)"],
    ["solver-error", "solver error (no verdict)"],
  ];
  if (vcs.some((vc) => vc.status === "unavailable")) {
    entries.push(["unavailable", "backend unavailable (no verdict)"]);
  }
  // Only key the unknown swatch when an unknown obligation is actually present
  // -- it is a rare fail-closed anomaly, not part of the everyday taxonomy.
  if (vcs.some((vc) => vc.status === "unknown")) {
    entries.push(["unknown", "unknown (no verdict)"]);
  }
  return { visible: true, entries };
}

// The full pane view-model.  Precedence mirrors renderProofPane exactly:
// on-a-VC ("obligation") > unavailable placeholder > state-at-cursor
// ("context") > generic placeholder.
function proofPaneModel(vcs, cursor, opts) {
  vcs = vcs || [];
  opts = opts || {};
  const unavailable = !!opts.unavailable;
  const unavailableReason = opts.unavailableReason || "unknown";
  const hidden = Number(opts.hidden) > 0 ? Number(opts.hidden) : 0;
  // The compact toggle is the ONE collapse primitive: it drives whether depth-1
  // lines are emitted at all (see paneBodyLines).  It is stored on the vm so
  // both sinks gate identically.
  const compact = !!opts.compact;

  const legend = legendModel(vcs);

  // Pick the INNERMOST containing obligation (vcOrder: smallest span, then the
  // span entered at the caret, then dump/id order).  Two branch VCs that share
  // the whole-`if` span tie and neither starts at an interior caret, so the
  // deterministic winner stands; giving each branch its own sub-span is the
  // compiler-side fix (#144), not something the pane can attribute here.
  const here = vcs.filter((vc) => contains(vc, cursor));
  here.sort((a, b) => vcOrder(a, b, cursor));
  const selected = here.findIndex(
    (candidate) =>
      opts.selectedVcId !== undefined && candidate.id === opts.selectedVcId
  );
  const selectionIndex = selected >= 0 ? selected : 0;
  const vc = here[selectionIndex];

  if (vc) {
    return {
      mode: "obligation",
      // The old "mode:" header line is gone -- the loud verdict token carries
      // the mode now (spec cut), so paneMode is empty in every case.
      paneMode: "",
      compact,
      vc,
      facts: null,
      obligation: obligationModel(
        vc,
        here.length - 1,
        opts,
        selectionIndex,
        here.length
      ),
      overlappingVcs: here,
      contextFacts: null,
      placeholder: null,
      hiddenCount: hidden,
      legend,
    };
  }

  // "unavailable" (the check did not complete) is shown distinctly from a
  // program that genuinely reported no obligations.  Only trustworthy dump data
  // can seed the off-obligation "known here" view, so it is gated behind this.
  if (unavailable) {
    return {
      mode: "unavailable",
      paneMode: "",
      compact,
      vc: null,
      facts: null,
      obligation: null,
      overlappingVcs: [],
      contextFacts: null,
      // Unavailability also covers a successful legacy compile whose compiler
      // cannot emit the VC sidecar, so do not claim compilation failed here.
      placeholder: {
        "type-error": "Obligation data unavailable: fix the source error.",
        "compiler-lacks-vc-data":
          "Obligation data unavailable: this compiler does not provide VC data.",
        "verification-not-run":
          "Verification was not run (typecheck only).",
        "backend-unavailable":
          "Obligation data unavailable: the selected backend could not run.",
        "compiler-unavailable":
          "Obligation data unavailable: the compiler could not run.",
        "compiler-crashed":
          "Obligation data unavailable: the compiler check failed.",
        timeout: "Obligation data unavailable: the check timed out.",
        "malformed-vc-data":
          "Obligation data unavailable: the compiler returned malformed VC data.",
        "check-failed": "Obligation data unavailable: the check failed.",
        unknown: "Obligation data unavailable.",
      }[unavailableReason] || "Obligation data unavailable.",
      hiddenCount: hidden,
      legend,
    };
  }

  const facts = stateAtCursor(vcs, cursor);
  if (facts.length) {
    const fadeUnused = opts.fadeUnused !== false;
    return {
      mode: "context",
      paneMode: "",
      compact,
      vc: null,
      facts,
      obligation: null,
      overlappingVcs: [],
      contextFacts: facts.map((h, i) => contextRow(h, i, fadeUnused)),
      placeholder: null,
      hiddenCount: hidden,
      legend,
    };
  }

  return {
    mode: "empty",
    paneMode: "",
    compact,
    vc: null,
    facts: null,
    obligation: null,
    overlappingVcs: [],
    contextFacts: null,
    placeholder: vcs.length
      ? "Move the cursor onto a marked obligation."
      : "No obligations reported for this buffer.",
    hiddenCount: hidden,
    legend,
  };
}

// ---------------------------------------------------------------------------
// Text serialization -- the terminal sink.
//
// These produce the EXACT text the browser DOM exposes as .textContent for
// #pane-body / #pane-mode / #legend, with NO added whitespace or separators:
// the browser concatenates its child text nodes with nothing between them, so
// the faithful mirror must too.  This is the property the anti-drift test locks
// (tests/test_pane_fidelity.js): CLI text (ANSI-stripped) === DOM textContent.
// The CLI adds ANSI colour on top; stripping it recovers exactly this text.
// ---------------------------------------------------------------------------

// The interactive-chrome element classes renderVc emits (the Lean action
// buttons and the tooltip prose): browser-only, zero textual footprint, so
// both the DOM serializer (anti-drift test) and the text renderer skip them.
// (The old verdict-token underline swatch is gone -- the verdict now rides the
// goal line's colour, so there is no swatch element to skip.)
const CHROME_CLASSES = [
  "lean-actions",
  "lean-hint",
  "lean-btn",
  "overlap-control",
];

// The honesty caveat under the state-at-cursor view (matches renderStateAtCursor).
const STATE_NOTE_TEXT =
  "Approximate: facts introduced textually above the cursor, derived from " +
  "nearby obligations. Branch conditions are omitted, and a binding introduced " +
  "inside a branch or other nested scope may still appear below that scope " +
  "where it is no longer in scope. Treat this as a hint, not a guarantee of " +
  "what holds here.";

// The witness preamble (matches renderWitness): its own line, the witness
// following on the next.
const WITNESS_PREFIX_LINE =
  "goal is false when (candidate; under Lean's unbounded-Int model, may not " +
  "be a valid machine int):";

// Normalize a readable block string: rstrip each line, drop leading/trailing
// blank lines, but PRESERVE internal blanks (the generated Lean has one).  Both
// the text renderer and the DOM serializer run this, so structural blanks
// cannot cause spurious drift.
function normalizeReadable(str) {
  const lines = String(str)
    .split("\n")
    .map((l) => l.replace(/[ \t]+$/, ""));
  while (lines.length && lines[0] === "") lines.shift();
  while (lines.length && lines[lines.length - 1] === "") lines.pop();
  return lines.join("\n");
}

// Push `text` (which may contain newlines) as one-line segments of kind `kind`.
function pushLines(out, text, kind, extra) {
  const parts = String(text).split("\n");
  // A block's trailing newline (the generated Lean and solver detail end with
  // one) is the block boundary in the DOM, not a blank content line -- so drop
  // a single trailing "" to match the block-aware DOM serialization.
  if (parts.length > 1 && parts[parts.length - 1] === "") parts.pop();
  parts.forEach((line, i) => {
    out.push(
      Object.assign({ text: line, kind: i === 0 ? kind : "cont" }, extra || {})
    );
  });
}

function hiddenNoteText(count) {
  if (!count) return "";
  return (
    count + " obligation" + (count > 1 ? "s" : "") + " with no source location (not shown)"
  );
}

// The readable body as an array of single-line segments { text, kind, depth
// [, faded, label, status] }.  `kind` drives the CLI's colour; `depth` (0/1/2)
// is the disclosure level.  The plain readable text is `segments.map(s => s.text)`.
//
// The depth model is the ONE collapse primitive (spec).  COMPACT is truly
// minimal, matching vox1's "goal then hypotheses, nothing else":
//   depth 0 (ALWAYS, compact + full) -- the ⊢ goal line, which CARRIES the
//     verdict (a status glyph at the right + verdict colour + any welded honesty
//     qualifier such as `· no witness`), and the hypotheses (unused ones faded).
//     FULL labels each hypothesis `name : predicate`; COMPACT shows the BARE
//     predicate only (the label is a hover away -- the pane row and its editor
//     source span highlight each other).  Nothing else in compact.  Off an
//     obligation, compact shows NOTHING (no CONTEXT line -- honest, nothing to
//     caveat).
//   depth 1 (FULL only) -- the code anchor, the kind tag, the refutation
//     reason / counterexample, status notes, the sibling count, the three
//     escape-hatch disclosures ([raw predicate] / [solver detail] / [generated
//     Lean]), and (off an obligation) the CONTEXT token + facts + caveat.  The
//     compact checkbox IS this collapse.
// Because compact DROPS every non-depth-0 line from the emitted list (rather
// than merely hiding it), the terminal tool and the browser -- which both
// render exactly this list -- agree byte-for-byte at each compact level.
function paneBodyLines(vm) {
  const out = [];
  const full = !vm.compact;
  if (vm.mode === "obligation") {
    const ob = vm.obligation;
    // --- depth 0: goal, hypotheses (always; the minimal compact view) --------
    // The verdict rides the GOAL line: the line is coloured by verdict and a
    // small status glyph sits at the END (right) of the line (see renderVc /
    // colorSeg).  A disproved goal keeps its welded `· no witness` / `· witness`
    // qualifier with the goal (glyph to its right) so honesty is never dropped.
    // No separate loud token line, no underline swatch.
    pushLines(
      out,
      "⊢ " +
        ob.goalDisplay +
        (ob.goalQualifier ? " · " + ob.goalQualifier : "") +
        "  " +
        ob.goalGlyph,
      "goal",
      { depth: 0, status: ob.status }
    );
    // Full only: the code anchor and the kind tag sit between the goal and the
    // hypotheses (metadata header), so compact stays token/goal/hyps.
    if (full) {
      out.push({ text: ob.anchor, kind: "anchor", depth: 1 });
      if (ob.kindLabel) {
        out.push({ text: ob.kindLabel + " obligation", kind: "kind", depth: 1 });
      }
    }
    // The hypotheses: unused ones faded (vox1's rule).  Depth 0 -- shown in the
    // compact view too.  FULL labels each row `name : predicate` (the #157
    // binder / kind-based label); COMPACT shows the BARE predicate only -- the
    // label is a hover away (the row highlights its source span in the editor,
    // and vice-versa), so compact stays minimal without hiding anything: the
    // predicate itself is still shown.
    ob.context.forEach((row) => {
      pushLines(out, full ? row.label + " : " + row.display : row.display, "hyp", {
        faded: row.faded,
        label: row.label,
        depth: 0,
      });
    });
    // --- depth 1: the rest of the proof state + escape hatches (full only) ---
    if (full) {
      if (ob.statusNote) pushLines(out, ob.statusNote, "note", { depth: 1 });
      if (ob.counterexample) {
        out.push({ text: ob.counterexample.heading, kind: "heading", depth: 1 });
        if (ob.counterexample.witness) {
          out.push({ text: WITNESS_PREFIX_LINE, kind: "cex", depth: 1 });
          pushLines(out, ob.counterexample.witness.join("\n"), "cex", { depth: 1 });
        } else if (ob.counterexample.noneText) {
          pushLines(out, ob.counterexample.noneText, "cex", { depth: 1 });
        }
      }
      if (ob.overlapping > 0) {
        out.push({
          text:
            "+" +
            ob.overlapping +
            " more obligation" +
            (ob.overlapping > 1 ? "s" : "") +
            " here",
          kind: "also",
          depth: 1,
        });
      }
      // The escape-hatch disclosures are FULL ONLY (reachable by unchecking
      // compact); each still renders as a collapsed <details> in the browser.
      if (ob.rawPredicate) {
        out.push({ text: "[raw predicate]", kind: "summary", depth: 1 });
        pushLines(out, ob.rawPredicate, "raw", { depth: 1 });
      }
      if (ob.solverDetail) {
        out.push({ text: "[solver detail]", kind: "summary", depth: 1 });
        pushLines(out, ob.solverDetail, "detail", { depth: 1 });
      }
      if (ob.lean) {
        out.push({ text: "[generated Lean]", kind: "summary", depth: 1 });
        pushLines(out, ob.lean, "lean", { depth: 1 });
      }
    }
  } else if (vm.mode === "context") {
    // Off an obligation, COMPACT shows NOTHING in the PROOF zone (honest: with
    // no facts shown there is nothing to caveat).  FULL shows the grey CONTEXT
    // token (not a verdict), the approximate facts, and the verbatim caveat
    // riding with them -- all depth 1, so the compact view emits nothing here.
    if (full) {
      out.push({ text: CONTEXT_TOKEN_TEXT, kind: "token", status: "context", depth: 1 });
      vm.contextFacts.forEach((row) => {
        pushLines(out, row.label + " : " + row.display, "hyp", {
          faded: row.faded,
          label: row.label,
          depth: 1,
        });
      });
      pushLines(out, STATE_NOTE_TEXT, "note", { depth: 1 });
    }
  } else {
    pushLines(out, vm.placeholder || "", "placeholder", { depth: 0 });
  }
  if (full && vm.hiddenCount) {
    out.push({ text: hiddenNoteText(vm.hiddenCount), kind: "also", depth: 1 });
  }
  return out;
}

// #pane-body as readable plain text (block-aware, chrome-stripped).
function paneBodyReadable(vm) {
  return normalizeReadable(paneBodyLines(vm).map((s) => s.text).join("\n"));
}

// #pane-mode .textContent.
function paneModeText(vm) {
  return vm.paneMode || "";
}

// The verdict legend, one label per line (the browser shows separate chips; a
// run-together line would be unreadable).  Empty when the legend is hidden.
function legendReadable(vm) {
  if (!vm.legend || !vm.legend.visible) return "";
  return vm.legend.entries.map((e) => e[1]).join("\n");
}

// The canonical, position-identifying serialization of a whole pane (mode +
// body + legend), used to dedupe panes for the cursor->pane map.  Panes that
// differ in any visible surface get distinct keys.
function paneText(vm) {
  return (
    "mode\u001f" +
    paneModeText(vm) +
    "\u001fbody\u001f" +
    paneBodyReadable(vm) +
    "\u001flegend\u001f" +
    legendReadable(vm)
  );
}

// ---------------------------------------------------------------------------
// The static cursor->pane map (primary terminal artifact).
//
// For every caret position in the document (each line `l`, each column
// 0..len(line) inclusive) it computes the REAL pane view-model, dedupes the
// panes by their canonical text globally, and lays a per-column glyph ruler
// under each source line whose glyph indexes the global legend directly.  The
// construction is pure (each cell IS a point-query pane), so the point-query
// anti-drift lock already validates every cell's content; this only adds the
// dedupe/index.  Formatting to a printable transcript lives in the CLI.
// ---------------------------------------------------------------------------

// The single-char glyph alphabet for panes 0..61, assigned in order of first
// appearance.  `·` is reserved for the empty/placeholder ("no pane") pane.
const MAP_GLYPHS =
  "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
const MAP_PLACEHOLDER_GLYPH = "·";

// Split a document into caret lines exactly as the editor sees them: a UTF-16
// code-unit view, one cell per column 0..len inclusive (empty lines have one
// cell -- the trailing caret).  `\r\n?` is normalized so a caret column matches
// CodeMirror's `ch`.
function mapLines(source) {
  return String(source).replace(/\r\n?/g, "\n").split("\n");
}

// Build the cursor->pane map for a whole file.  Returns the deduped pane
// legend (in first-appearance order) and, per source line, the glyph ruler
// (one glyph per caret column) plus any line-local overflow remaps.
function buildCursorMap(vcs, source, opts) {
  const lines = mapLines(source);
  const info = new Map(); // paneText -> { vm, order }
  const order = []; // paneText keys, first-appearance order
  const grid = []; // per line: array of paneText keys, one per caret column

  lines.forEach((text, line) => {
    const row = [];
    for (let ch = 0; ch <= text.length; ch++) {
      const vm = proofPaneModel(vcs, { line, ch }, opts);
      const key = paneText(vm);
      if (!info.has(key)) {
        info.set(key, { vm, order: order.length });
        order.push(key);
      }
      row.push(key);
    }
    grid.push(row);
  });

  // Assign a global glyph + legend id to each distinct pane.  The empty pane
  // takes the reserved placeholder glyph; the rest take 0-9,a-z,A-Z in order.
  // Beyond 62 real panes, a pane has no global single-char glyph (glyph=null)
  // and is referenced only by its integer legend id ("#<n>") via line-local
  // remaps -- the rare overflow path.
  const glyphOf = new Map();
  const legend = [];
  let next = 0;
  order.forEach((key, idx) => {
    const vm = info.get(key).vm;
    let glyph;
    let legendId;
    if (vm.mode === "empty") {
      glyph = MAP_PLACEHOLDER_GLYPH;
      legendId = MAP_PLACEHOLDER_GLYPH;
    } else if (next < MAP_GLYPHS.length) {
      glyph = MAP_GLYPHS[next];
      legendId = glyph;
      next += 1;
    } else {
      glyph = null;
      legendId = "#" + idx;
    }
    glyphOf.set(key, glyph);
    legend.push({
      key,
      glyph,
      legendId,
      vm,
      mode: vm.paneMode,
      body: paneBodyReadable(vm),
      legendText: legendReadable(vm),
    });
  });

  // Per-line rulers.  A line that references an overflow pane (glyph=null)
  // substitutes a line-local free glyph for it and records the remap; all
  // other lines carry no remap.
  const outLines = lines.map((text, line) => {
    const keys = grid[line];
    const remaps = [];
    const localGlyph = new Map();
    const usedOnLine = new Set();
    keys.forEach((k) => {
      const g = glyphOf.get(k);
      if (g !== null && g !== undefined) usedOnLine.add(g);
    });
    const freePool = (MAP_GLYPHS + MAP_PLACEHOLDER_GLYPH).split("");
    const takeFree = () => {
      for (const c of freePool) {
        if (!usedOnLine.has(c)) {
          usedOnLine.add(c);
          return c;
        }
      }
      return "?"; // >62 distinct panes on one line: not expected to occur.
    };
    const ruler = keys
      .map((k) => {
        const g = glyphOf.get(k);
        if (g !== null && g !== undefined) return g;
        if (!localGlyph.has(k)) {
          const lg = takeFree();
          localGlyph.set(k, lg);
          remaps.push({ glyph: lg, legendId: info.get(k) ? "#" + info.get(k).order : "#?" });
        }
        return localGlyph.get(k);
      })
      .join("");
    return { text, line, ruler, remaps };
  });

  return { lines: outLines, legend };
}

// Resolve a ruler glyph (optionally with a line's overflow remaps) back to the
// legend entry it denotes -- the inverse the anti-drift map check relies on.
function resolveGlyph(map, lineIndex, glyph) {
  const line = map.lines[lineIndex];
  if (line) {
    const remap = line.remaps.find((r) => r.glyph === glyph);
    if (remap) {
      return map.legend.find((e) => e.legendId === remap.legendId) || null;
    }
  }
  return map.legend.find((e) => e.glyph === glyph) || null;
}

// ---------------------------------------------------------------------------
// Exports (browser: globals via top-level declarations; node: module.exports).
// ---------------------------------------------------------------------------

if (typeof module !== "undefined" && module.exports) {
  module.exports = {
    VC_STATUSES,
    normalizeStatus,
    FAILED_STATUSES,
    BADGE_HINT,
    STATUS_LABEL,
    statusLabel,
    STATUS_NOTE,
    TOKEN_GLYPH,
    TOKEN_LABEL,
    verdictTokenText,
    CONTEXT_TOKEN_TEXT,
    anchorText,
    statusRollup,
    adaptPredicate,
    adaptHyp,
    validateEditorSpan,
    adaptBackendResult,
    summaryFromVcs,
    adaptObligationSummary,
    adaptVcs,
    adaptProducers,
    adaptUsedBy,
    lemmaSpanKey,
    factUsage,
    unnecessaryLemmaCalls,
    LEMMA_UNUSED_HINT,
    lemmaUnusedHint,
    contains,
    spanSize,
    cursorReadoutLines,
    cursorReadout,
    posCol,
    posCmp,
    vcOrder,
    hypLabel,
    stateAtCursor,
    proofPaneModel,
    obligationModel,
    legendModel,
    paneBodyLines,
    paneBodyReadable,
    paneModeText,
    legendReadable,
    paneText,
    normalizeReadable,
    CHROME_CLASSES,
    mapLines,
    buildCursorMap,
    resolveGlyph,
    MAP_GLYPHS,
    MAP_PLACEHOLDER_GLYPH,
  };
}
