// Layer 5: the thin UI layer. All non-trivial logic lives in
// selection.js (tested under node); this file wires CodeMirror to the
// /check and /goal endpoints and renders the proof pane. Kept small on
// purpose.

const SAMPLE = `let rec total_ dbl n =
  if n <= 0 then 0 else 2 + dbl (n - 1)
[@@vox.decreases n]

[%%vox.lean {lean|
theorem dbl_nonneg (n : Int) (h : 0 <= n) : dbl n >= 0 := by
  fun_induction dbl n <;> omega
|lean}]

let use () =
  let refine_ ok = (dbl 0 : int{ _ = 0 }) in
  ok
`;

const cm = CodeMirror.fromTextArea(document.getElementById("code"), {
  mode: "text/x-ocaml",
  lineNumbers: true,
  value: SAMPLE,
});
cm.setValue(SAMPLE);

let regions = [];
let errors = [];
let revision = 0;
let applied = -1;
let marks = [];
// The source of the example currently loaded (or the initial SAMPLE), so
// we can warn before discarding hand edits when a new one is picked.
let lastLoaded = SAMPLE;

const statusEl = document.getElementById("status");
const modeEl = document.getElementById("pane-mode");
const bodyEl = document.getElementById("pane-body");

function esc(s) {
  return String(s).replace(/[&<>]/g, (c) =>
    ({ "&": "&amp;", "<": "&lt;", ">": "&gt;" }[c])
  );
}

function setStatus(cls, text) {
  statusEl.className = cls;
  statusEl.textContent = text;
}

function clearMarks() {
  marks.forEach((m) => m.clear());
  marks = [];
}

function markRegions() {
  clearMarks();
  regions.forEach((r) => {
    let cls = null;
    if (r.kind === "vc") cls = "vc-" + (r.status || "unknown");
    else if (r.kind === "block") cls = "vc-block";
    if (!cls) return;
    marks.push(
      cm.markText(
        { line: r.start.line, ch: r.start.col },
        { line: r.end.line, ch: r.end.col },
        { className: cls }
      )
    );
  });
}

async function check() {
  const source = cm.getValue();
  revision += 1;
  const rev = revision;
  setStatus("status-checking", "checking…");
  try {
    const resp = await postJSON("/check", { source, revision: rev });
    if (resp.revision < applied) return; // stale
    applied = resp.revision;
    regions = resp.regions || [];
    errors = resp.errors || [];
    markRegions();
    setStatus(
      resp.ok ? "status-ok" : "status-fail",
      resp.ok ? "verified ✓" : "errors ✗"
    );
    renderPane();
  } catch (e) {
    setStatus("status-fail", "server error");
  }
}

function renderPane() {
  const c = cm.getCursor();
  const sel = Selection.selectRegion(regions, { line: c.line, col: c.ch });
  const r = sel.region;
  modeEl.textContent = r ? sel.mode + " · " + r.kind : "no selection";
  let html = "";
  if (!r) {
    html = '<p class="placeholder">No verification condition here.</p>';
  } else if (r.kind === "vc") {
    html = renderVc(r);
  } else if (r.kind === "theorem") {
    html = renderTheorem(r) + liveButton();
  } else if (r.kind === "block") {
    html =
      '<p>Inside a <code>[%%vox.lean]</code> block.</p>' + liveButton();
  }
  html += renderErrors();
  bodyEl.innerHTML = html;
  const btn = document.getElementById("live-btn");
  if (btn) btn.addEventListener("click", liveGoal);
}

function badge(status) {
  const s = status || "unknown";
  return '<span class="badge badge-' + s + '">' + s + "</span>";
}

function renderVc(r) {
  let h = "<h3>goal" + badge(r.status) + "</h3>";
  h += '<div class="goal">' + esc(r.goal) + "</div>";
  h += renderHyps(r.hypotheses);
  if (r.counterexample && r.counterexample.length) {
    h += "<h3>counterexample</h3>";
    h += '<div class="cex">' + esc(r.counterexample.join("\n")) + "</div>";
  }
  return h;
}

function renderTheorem(r) {
  let h = "<h3>theorem " + esc(r.name) + " (static)</h3>";
  h += '<div class="goal">' + esc(r.goal) + "</div>";
  return h + renderHyps(r.hypotheses);
}

function renderHyps(hyps) {
  if (!hyps || !hyps.length) return "<h3>hypotheses</h3><div class='hyp'>—</div>";
  return (
    "<h3>hypotheses</h3>" +
    hyps.map((x) => '<div class="hyp">' + esc(x) + "</div>").join("")
  );
}

function liveButton() {
  return '<p><button id="live-btn">Get live Lean goal at cursor</button></p>';
}

function renderErrors() {
  if (!errors.length) return "";
  return (
    '<div id="errors"><h3>errors</h3>' +
    errors
      .map((e) => '<div class="err">' + esc(e.message || "") + "</div>")
      .join("") +
    "</div>"
  );
}

async function liveGoal() {
  const c = cm.getCursor();
  const btn = document.getElementById("live-btn");
  if (btn) btn.textContent = "querying Lean…";
  try {
    const resp = await postJSON("/goal", {
      source: cm.getValue(),
      line: c.line,
      col: c.ch,
      revision: applied,
    });
    let h;
    if (resp.status === "ok" && resp.goals.length) {
      h =
        "<h3>live Lean goal</h3>" +
        resp.goals
          .map((g) => '<div class="goal live-goals">' + esc(g) + "</div>")
          .join("");
    } else {
      h = '<p class="placeholder">' + esc(resp.detail || resp.status) + "</p>";
    }
    bodyEl.insertAdjacentHTML("afterbegin", h);
  } catch (e) {
    if (btn) btn.textContent = "Lean query failed";
  }
}

async function postJSON(path, body) {
  const resp = await fetch(path, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
  return resp.json();
}

// Cursor moves: pane only (client-side, no network).
cm.on("cursorActivity", renderPane);

// Idle debounce + explicit trigger.
let timer = null;
cm.on("change", () => {
  clearMarks();
  if (timer) clearTimeout(timer);
  timer = setTimeout(check, 900);
});
document.getElementById("check-btn").addEventListener("click", check);
cm.addKeyMap({
  "Ctrl-Enter": check,
  "Cmd-Enter": check,
});

// Examples dropdown: populated from /examples, each choice loads the
// source and re-checks. A confirm() guards unsaved edits.
const examplesEl = document.getElementById("examples");

async function loadExamples() {
  let list;
  try {
    const resp = await fetch("/examples");
    list = (await resp.json()).examples || [];
  } catch (e) {
    return; // no examples served; leave the dropdown as-is
  }
  list.forEach((ex) => {
    const opt = document.createElement("option");
    opt.value = ex.name;
    opt.textContent = (ex.verifies ? "" : "✗ ") + ex.title;
    opt.title = ex.description;
    examplesEl.appendChild(opt);
  });
}

async function pickExample(name) {
  if (cm.getValue() !== lastLoaded &&
      !confirm("Discard your edits and load this example?")) {
    return false;
  }
  try {
    const resp = await fetch("/examples/" + encodeURIComponent(name));
    if (!resp.ok) return false;
    const source = await resp.text();
    lastLoaded = source;
    cm.setValue(source);
    check();
    return true;
  } catch (e) {
    setStatus("status-fail", "could not load example");
    return false;
  }
}

examplesEl.addEventListener("change", async () => {
  const name = examplesEl.value;
  if (!name) return;
  await pickExample(name);
  examplesEl.value = ""; // reset to the "Examples…" label
});

loadExamples();

// Testability hook for the headless browser smoke test.
window.__vox = { cm, check, renderPane, pickExample, loadExamples };

check();
