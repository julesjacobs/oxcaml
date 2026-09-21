import { EditorView, keymap, hoverTooltip } from "@codemirror/view";
import { EditorState } from "@codemirror/state";
import { basicSetup } from "codemirror";
import {
  StreamLanguage,
  syntaxHighlighting,
  defaultHighlightStyle,
} from "@codemirror/language";
import { oCaml } from "@codemirror/legacy-modes/mode/mllike";
import { autocompletion } from "@codemirror/autocomplete";
import { setDiagnostics } from "@codemirror/lint";
import { createGoalPanel } from "./goals.js";
import { lessons } from "./lessons.js";
import "./style.css";

const stored = (key, fallback) => {
  try {
    return localStorage.getItem(key) ?? fallback;
  } catch {
    return fallback;
  }
};
let selected =
  lessons.find((l) => l.id === location.hash.slice(1)) ?? lessons[0];
let revision = 0,
  compiler = null,
  runner = null,
  solver = null,
  assets = null,
  ready = false,
  timer,
  deadline,
  runDeadline;
let metadata = null,
  lastBytecode = null,
  proofs = 0,
  output = "",
  activeTab = "output";
const app = document.querySelector("#app");
app.innerHTML = `
<header><a class="brand" href="#windows">Vox</a><select id="lesson" aria-label="Lesson">${lessons.map((l) => `<option value="${l.id}">${l.chapter} ${l.label}</option>`).join("")}</select></header>
<main><article class="lesson-copy"><h1 id="title"></h1><p id="intro"></p><p id="body"></p><p id="challenge" class="challenge"></p><details class="lesson-note"><summary>More</summary><p id="detail"></p></details></article>
<section class="lab" aria-label="Interactive Vox editor">
<div class="editor-top"><span id="check-status" role="status" aria-live="polite">Loading…</span><details class="tools"><summary aria-label="Editor options">•••</summary><div class="tool-menu"><button id="run">Run <kbd>⌘ ↵</kbd></button><label><input type="checkbox" id="auto" checked> Run as you type</label><button id="reset">Reset example</button><button id="download">Download code</button><small>Hover for types<br>Ctrl-Space for completion<br>Alt-click for definition</small></div></details></div>
<div id="editor"></div><div class="results"><div class="result-tabs" role="tablist"><button role="tab" aria-selected="true" data-tab="output">Output</button><button role="tab" aria-selected="false" data-tab="goals">Goals</button><button role="tab" aria-selected="false" data-tab="types">Types</button><span id="run-status"></span></div><div id="diagnostic" class="diagnostic" hidden></div><pre id="result" aria-live="polite"></pre><section id="goals" class="goals" aria-label="Proof goals" hidden></section></div>
</section></main>`;
const $ = (s) => document.querySelector(s);
const status = (text, state = "busy") => {
  $("#check-status").textContent = text;
  $("#check-status").dataset.state = state;
};
function renderResult() {
  document
    .querySelectorAll("[data-tab]")
    .forEach((t) =>
      t.setAttribute("aria-selected", String(t.dataset.tab === activeTab)),
    );
  $("#goals").hidden = activeTab !== "goals";
  $("#result").hidden = activeTab === "goals";
  $("#result").textContent =
    activeTab === "types"
      ? (metadata?.signature ??
        "The inferred interface appears after successful checking.")
      : output || "No output.";
}
function appendOutput(text) {
  output += text + "\n";
  if (output.length > 100000) {
    output = output.slice(0, 100000) + "\nOutput limit reached.";
    runner?.terminate();
    clearTimeout(runDeadline);
    $("#run-status").textContent = "Stopped: output limit";
  }
  renderResult();
}
function byteOffset(offset) {
  const encoded = new TextEncoder().encode(view.state.doc.toString());
  return new TextDecoder().decode(encoded.subarray(0, Math.max(0, offset)))
    .length;
}
function convertedEntries(entries) {
  return entries.map((e) => ({
    ...e,
    from: byteOffset(e.from),
    to: byteOffset(e.to),
    definition: e.definition >= 0 ? byteOffset(e.definition) : undefined,
  }));
}
function completion(context) {
  const word = context.matchBefore(/[\w']*/);
  if (!word || (!context.explicit && !word.text)) return null;
  const keywords = [
    "let",
    "rec",
    "in",
    "match",
    "with",
    "fun",
    "function",
    "type",
    "module",
    "struct",
    "end",
    "if",
    "then",
    "else",
    "refine_",
    "assume_",
    "ghost_",
    "total",
    "ghost",
  ];
  const names = [
    ...context.state.doc
      .toString()
      .matchAll(/\b(?:let\s+(?:rec\s+)?|type\s+|module\s+)([a-zA-Z_][\w']*)/g),
  ].map((m) => m[1]);
  return {
    from: word.from,
    options: [...new Set([...names, ...keywords])].map((label) => ({
      label,
      type: keywords.includes(label) ? "keyword" : "variable",
    })),
    validFor: /^[\w']*$/,
  };
}
const view = new EditorView({
  parent: $("#editor"),
  state: EditorState.create({
    doc: stored("vox:lesson:" + selected.id, selected.code),
    extensions: [
      basicSetup,
      StreamLanguage.define(oCaml),
      syntaxHighlighting(defaultHighlightStyle),
      EditorState.tabSize.of(2),
      autocompletion({ override: [completion] }),
      hoverTooltip((editor, pos) => {
        const entry = metadata?.hovers
          .filter((e) => e.from <= pos && e.to >= pos)
          .sort((a, b) => a.to - a.from - (b.to - b.from))[0];
        if (!entry) return null;
        return {
          pos: entry.from,
          end: entry.to,
          above: true,
          create() {
            const dom = document.createElement("pre");
            dom.className = "type-tooltip";
            dom.textContent = entry.text;
            return { dom };
          },
        };
      }),
      keymap.of([
        {
          key: "Mod-Enter",
          run: () => {
            start();
            return true;
          },
        },
        {
          key: "Alt-Enter",
          run: () => {
            start();
            return true;
          },
        },
      ]),
      EditorView.domEventHandlers({
        click: (event, editor) => {
          if (!event.altKey) return false;
          const pos = editor.posAtCoords({
            x: event.clientX,
            y: event.clientY,
          });
          const entry = metadata?.hovers
            .filter(
              (e) => e.definition !== undefined && e.from <= pos && e.to >= pos,
            )
            .sort((a, b) => a.to - a.from - (b.to - b.from))[0];
          if (!entry) return false;
          editor.dispatch({
            selection: { anchor: entry.definition },
            scrollIntoView: true,
          });
          return true;
        },
      }),
      EditorView.updateListener.of((update) => {
        if (!update.docChanged) return;
        try {
          localStorage.setItem(
            "vox:lesson:" + selected.id,
            update.state.doc.toString(),
          );
        } catch {}
        edited();
      }),
      EditorView.contentAttributes.of({
        "aria-label": "Vox source code",
        spellcheck: "false",
      }),
    ],
  }),
});
const goalPanel = createGoalPanel($("#goals"), (from, to) => {
  const length = view.state.doc.length;
  view.dispatch({
    selection: {
      anchor: Math.min(length, byteOffset(from)),
      head: Math.min(length, byteOffset(to)),
    },
    scrollIntoView: true,
  });
  view.focus();
});
function cancel() {
  clearTimeout(timer);
  clearTimeout(deadline);
  clearTimeout(runDeadline);
  compiler?.terminate();
  runner?.terminate();
  compiler = null;
  runner = null;
  solver?.postMessage({ type: "cancel", revision });
}
function edited() {
  cancel();
  revision++;
  goalPanel.clear();
  metadata = null;
  lastBytecode = null;
  output = "";
  renderResult();
  view.dispatch(setDiagnostics(view.state, []));
  $("#diagnostic").hidden = true;

  $("#run-status").textContent = "";
  status(ready ? "Edited — waiting to check" : "Loading compiler and solver…");
  if ($("#auto").checked) timer = setTimeout(start, 500);
}
function runProgram(bytecode, rev) {
  runner?.terminate();
  runner = new Worker("/assets/compiler-worker.js");
  $("#run-status").textContent = "Running";
  output = "";
  renderResult();
  runner.onmessage = ({ data }) => {
    if (data.revision !== revision) return;
    if (data.type === "stdout") appendOutput(data.text);
    if (data.type === "stderr") {
      appendOutput(data.text);
      $("#run-status").textContent = "Runtime message";
    }
    if (data.type === "executed") {
      clearTimeout(runDeadline);
      $("#run-status").textContent =
        data.exitCode === 0 ? "" : `Failed · exit ${data.exitCode}`;
      runner?.terminate();
      runner = null;
    }
    if (data.type === "fatal") {
      clearTimeout(runDeadline);
      appendOutput(data.message);
      $("#run-status").textContent = "Execution failed";
      runner?.terminate();
      runner = null;
    }
  };
  runner.onerror = (e) => {
    if (rev !== revision) return;
    clearTimeout(runDeadline);
    appendOutput(e.message);
    $("#run-status").textContent = "Execution failed";
    runner?.terminate();
  };
  runner.postMessage({ mode: "run", revision: rev, bytecode });
  runDeadline = setTimeout(() => {
    if (rev !== revision) return;
    runner?.terminate();
    runner = null;
    $("#run-status").textContent = "Stopped · 3 s limit";
    appendOutput("Execution stopped after 3 seconds.");
  }, 3000);
}
function diagnostic(data) {
  if (proofs) activeTab = "goals";
  renderResult();
  const from = Math.min(view.state.doc.length, byteOffset(data.from));
  const to = Math.min(
    view.state.doc.length,
    Math.max(from + 1, byteOffset(data.to)),
  );
  view.dispatch(
    setDiagnostics(view.state, [
      { from, to, severity: "error", message: data.message },
    ]),
  );
  $("#diagnostic").hidden = false;
  $("#diagnostic").textContent = data.message;
  status("Check failed", "error");
  $("#run-status").textContent = "";
}
function start() {
  if (!ready) {
    status("Loading compiler and solver…");
    return;
  }
  cancel();
  revision++;
  const rev = revision,
    source = view.state.doc.toString();
  goalPanel.clear();
  metadata = null;
  lastBytecode = null;
  proofs = 0;
  output = "";
  renderResult();
  view.dispatch(setDiagnostics(view.state, []));
  $("#diagnostic").hidden = true;

  status("Starting compiler");
  $("#run-status").textContent = "";
  compiler = new Worker("/assets/compiler-worker.js");
  compiler.onmessage = ({ data }) => {
    if (data.revision !== revision) return;
    if (data.type === "solve") {
      const opaqueFunctions = [...data.proof.opaqueFunctions];
      if (data.script.includes("(declare-fun int63_mul "))
        opaqueFunctions.push("machine-integer multiplication");
      if (data.script.includes("(declare-fun int63_lsr_unspecified "))
        opaqueFunctions.push("out-of-range logical shifts");
      goalPanel.result({
        index: data.proof.index,
        script: data.script,
        opaqueFunctions,
      });
      solver.postMessage(data);
      return;
    }
    if (data.type === "proof-status") goalPanel.result(data);
    if (data.type === "checking") status("Checking types");
    if (data.type === "proof") {
      goalPanel.add(data);
      proofs = data.index;
      status("Verifying");
    }
    if (data.type === "metadata") {
      metadata = { ...data, hovers: convertedEntries(data.hovers) };
      renderResult();
    }
    if (data.type === "diagnostic") {
      clearTimeout(deadline);
      diagnostic(data);
      compiler?.terminate();
      compiler = null;
    }
    if (data.type === "compiler-output") {
      console.info("Vox:", data.text);
    }
    if (data.type === "fatal") {
      clearTimeout(deadline);
      diagnostic({ from: 0, to: 1, message: data.message });
      compiler?.terminate();
      compiler = null;
    }
    if (data.type === "bytecode") {
      clearTimeout(deadline);
      lastBytecode = data.bytecode;
      status(proofs ? "Verified" : "Type-checked", "ok");
      compiler?.terminate();
      compiler = null;
      runProgram(lastBytecode, rev);
    }
  };
  compiler.onerror = (e) => {
    if (rev !== revision) return;
    clearTimeout(deadline);
    diagnostic({ from: 0, to: 1, message: e.message });
    compiler?.terminate();
    compiler = null;
  };
  compiler.postMessage({ mode: "compile", revision: rev, source, assets });
  deadline = setTimeout(() => {
    if (rev !== revision) return;
    cancel();
    status("Checking stopped · 30 s limit", "error");
    $("#run-status").textContent = "";
  }, 30000);
}
function selectLesson(lesson) {
  cancel();
  activeTab = "output";
  selected = lesson;
  location.hash = lesson.id;
  $("#lesson").value = lesson.id;
  $("#title").textContent = lesson.title;
  $("#intro").textContent = lesson.intro;
  $("#body").innerHTML = lesson.body;
  $("#challenge").innerHTML = lesson.challenge;
  $("#detail").textContent = lesson.detail;
  const doc = stored("vox:lesson:" + lesson.id, lesson.code);
  view.dispatch({
    changes: { from: 0, to: view.state.doc.length, insert: doc },
  });
  output = "";
  goalPanel.clear();
  metadata = null;
  renderResult();
  if (ready) start();
}
$("#lesson").onchange = () =>
  selectLesson(lessons.find((l) => l.id === $("#lesson").value));
document.querySelectorAll("[data-tab]").forEach(
  (b) =>
    (b.onclick = () => {
      activeTab = b.dataset.tab;
      document
        .querySelectorAll("[data-tab]")
        .forEach((t) => t.setAttribute("aria-selected", String(t === b)));
      renderResult();
    }),
);
document.addEventListener("click", event => {
  const tools = $(".tools");
  if (!tools.contains(event.target) || event.target.closest(".tool-menu button")) tools.open = false;
});
document.addEventListener("keydown", event => { if (event.key === "Escape") $(".tools").open = false; });
$("#run").onclick = start;
$("#auto").onchange = () => {
  if ($("#auto").checked) start();
  else clearTimeout(timer);
};
$("#reset").onclick = () => {
  view.dispatch({
    changes: { from: 0, to: view.state.doc.length, insert: selected.code },
  });
  view.focus();
};
$("#download").onclick = () => {
  const url = URL.createObjectURL(
    new Blob([view.state.doc.toString()], { type: "text/plain" }),
  );
  const a = document.createElement("a");
  a.href = url;
  a.download = selected.id + ".ml";
  a.click();
  URL.revokeObjectURL(url);
};
selectLesson(selected);
async function loadAssets() {
  if (!crossOriginIsolated)
    throw new Error(
      "This page needs cross-origin isolation headers to run Z3. Start it using npm run dev or npm run preview.",
    );
  const fetchBuffer = async (url) => {
    const r = await fetch(url);
    if (!r.ok) throw new Error(`Could not load ${url}: ${r.status}`);
    return r.arrayBuffer();
  };
  const [compiler, data, manifest] = await Promise.all([
    fetchBuffer("/assets/compiler.byte"),
    fetchBuffer("/assets/stdlib.data"),
    fetch("/assets/stdlib-manifest.json").then((r) => r.json()),
  ]);
  const share = (buffer) => {
    const shared = new SharedArrayBuffer(buffer.byteLength);
    new Uint8Array(shared).set(new Uint8Array(buffer));
    return shared;
  };
  assets = { compiler: share(compiler), data: share(data), manifest };
  await new Promise((resolve, reject) => {
    solver = new Worker("/assets/solver-worker.js");
    solver.onerror = (e) => reject(new Error(e.message));
    solver.onmessage = ({ data }) => {
      if (data.type === "proof-result" && data.revision === revision)
        goalPanel.result(data);
      if (data.type === "ready") {
        resolve();
      }
      if (data.type === "fatal") reject(new Error(data.message));
      if (data.type === "solver-error") console.error(data.message);
    };
  });
  ready = true;
  start();
}
loadAssets().catch((error) => {
  clearTimeout(timer);
  status("Could not start", "error");
  output = error.message;
  renderResult();
});
window.addEventListener("hashchange", () => {
  const lesson = lessons.find((l) => l.id === location.hash.slice(1));
  if (lesson && lesson !== selected) selectLesson(lesson);
});
window.addEventListener("beforeunload", () => {
  cancel();
  solver?.terminate();
});
