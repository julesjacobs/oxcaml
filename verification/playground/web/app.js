// The playground page: an editor, an example picker, and the results of the
// checker, which runs in worker.js.
'use strict';

const $ = (id) => document.getElementById(id);
const exampleSelect = $('example');
const checkButton = $('check');
const stopButton = $('stop');
const verdict = $('verdict');
const verdictText = $('verdict-text');
const verdictDetail = $('verdict-detail');
const diagnostics = $('diagnostics');
const erased = $('erased');
const lambda = $('lambda');

const isMac = /Mac|iPhone|iPad/.test(navigator.platform);
$('shortcut').textContent = isMac ? '⌘ Enter' : 'Ctrl+Enter';

const editor = CodeMirror($('editor'), {
  mode: 'text/x-ocaml',
  lineNumbers: true,
  indentUnit: 2,
  tabSize: 2,
  extraKeys: {
    'Ctrl-Enter': () => check(),
    'Cmd-Enter': () => check(),
    Tab: (cm) => cm.execCommand('insertSoftTab'),
  },
});

// ---- Examples

let examples = [];
let current = null; // the selected example
const originals = new Map(); // file -> source as shipped
const edits = new Map(); // file -> the visitor's edited source

async function loadExamples() {
  const response = await fetch('examples/index.json');
  examples = await response.json();
  for (const example of examples) {
    const option = document.createElement('option');
    option.value = example.file;
    option.textContent = example.title;
    exampleSelect.append(option);
  }
  await Promise.all(examples.map(async (example) => {
    const text = await (await fetch('examples/' + example.file)).text();
    originals.set(example.file, text);
  }));
  const fromHash = examples.find((e) => '#' + e.file.replace(/\.ml$/, '') === location.hash);
  select((fromHash || examples[0]).file);
}

function select(file) {
  if (current) edits.set(current.file, editor.getValue());
  current = examples.find((e) => e.file === file);
  exampleSelect.value = file;
  $('summary').textContent = current.summary;
  $('command').textContent = `ocamlc -extension refinement_types -c ${file}`;
  editor.setValue(edits.get(file) ?? originals.get(file));
  editor.clearHistory();
  history.replaceState(null, '', '#' + file.replace(/\.ml$/, ''));
  clearResult();
  check();
}

exampleSelect.addEventListener('change', () => select(exampleSelect.value));
$('restore').addEventListener('click', () => {
  edits.delete(current.file);
  editor.setValue(originals.get(current.file));
  clearResult();
});

// ---- The checker

let worker = null;
let ready = false;
let pending = null; // the running check: { id, file, source, started, timer }
let requested = false; // a check is queued while the worker loads or runs
let nextId = 0;

function startWorker(autoCheck = true) {
  ready = false;
  checkButton.disabled = true;
  worker = new Worker('worker.js');
  worker.onmessage = ({ data }) => {
    if (data.ready) {
      ready = true;
      checkButton.disabled = false;
      const { z3Version, revision } = data.ready;
      $('versions').textContent = `Vox at commit ${revision}; Z3 ${z3Version}. ` +
        `Checker ready ${(performance.now() / 1000).toFixed(1)} s after the page started loading.`;
      if ((autoCheck || requested) && current) check();
    } else if (data.failed) {
      show('failed', 'The checker could not be loaded', data.failed);
    } else if (pending && data.id === pending.id) {
      finish(data);
    }
  };
  worker.onerror = (event) => {
    show('failed', 'The checker stopped', event.message || 'An error occurred in the worker.');
    if (pending) clearTimeout(pending.timer);
    pending = null;
  };
}

function check() {
  if (!current) return;
  if (!ready || pending) {
    requested = true;
    return;
  }
  requested = false;
  const id = ++nextId;
  pending = {
    id,
    file: current.file,
    source: editor.getValue(),
    started: performance.now(),
    // A check that runs long can be stopped; the worker is then restarted.
    timer: setTimeout(() => { stopButton.hidden = false; }, 4000),
  };
  checkButton.disabled = true;
  show('checking', 'Checking…', '');
  worker.postMessage({ id, name: current.file, source: pending.source });
}

stopButton.addEventListener('click', () => {
  if (!pending) return;
  clearTimeout(pending.timer);
  pending = null;
  requested = false;
  worker.terminate();
  stopButton.hidden = true;
  show('stopped', 'Stopped', 'The check was stopped. Restarting the checker…');
  startWorker(false);
});

checkButton.addEventListener('click', () => check());

function finish({ result, error }) {
  const { file, source, started, timer } = pending;
  clearTimeout(timer);
  pending = null;
  stopButton.hidden = true;
  checkButton.disabled = false;
  // The visitor switched examples during the check: its result is not shown.
  if (file !== current.file) {
    requested = false;
    check();
    return;
  }
  if (requested) {
    requested = false;
    if (source !== editor.getValue()) {
      check();
      return;
    }
  }
  if (error) {
    show('failed', 'The checker failed', error);
    return;
  }
  const seconds = ((performance.now() - started) / 1000).toFixed(2);
  const queries = result.queries === 0 ? 'No solver queries'
    : result.queries === 1 ? '1 solver query' : `${result.queries} solver queries`;
  const timing = `${queries}, ${seconds} s.`;
  if (result.status === 0) {
    const warned = /^Warning/m.test(result.output);
    show('accepted', warned ? 'Accepted, with warnings' : 'Accepted',
      `Every refinement, termination measure and ghost check is proved. ${timing}`);
  } else if (result.status === 3) {
    show('unsupported', 'Not checked in the browser',
      'The browser build cannot check this program; see the message below. This is not a Vox verdict.');
  } else {
    show('rejected', 'Rejected', `The compiler rejects this program. ${timing}`);
  }
  renderDiagnostics(result, source);
  if (result.lambda) {
    lambda.textContent = result.lambda;
    erased.hidden = false;
  }
  if (source === editor.getValue()) {
    markEditor(result);
  } else {
    verdict.dataset.state = 'stale';
    verdictDetail.textContent = 'The program has changed since this check. Press Check again.';
  }
}

function show(state, text, detail) {
  verdict.dataset.state = state;
  verdictText.textContent = text;
  verdictDetail.textContent = detail;
}

function clearResult() {
  diagnostics.hidden = true;
  diagnostics.textContent = '';
  closePeek();
  erased.hidden = true;
  lambda.textContent = '';
  clearMarks();
  if (ready && !pending) show('idle', 'Not checked yet', 'Press Check to check this program.');
}

editor.on('change', () => {
  if (!pending && verdict.dataset.state !== 'loading') {
    clearMarks();
    if (['accepted', 'rejected', 'unsupported'].includes(verdict.dataset.state)) {
      verdict.dataset.state = 'stale';
      verdictDetail.textContent = 'The program has changed since this check. Press Check again.';
    }
  }
});

// ---- Diagnostics

// The compiler's messages, with every location it prints as a link. The
// checker reports where the output prints each location of a message (its
// main location, and those of notes such as "The refinement is stated
// here."), with the location itself; see vox_playground.ml. Locations inside
// a message's text ("at file "x.ml", line 4, characters 23-71") are found by
// their printed form.

const encoder = new TextEncoder();
const decoder = new TextDecoder();

// The string index of a UTF-8 byte offset in [text].
function indexOfByte(text, byte) {
  return decoder.decode(encoder.encode(text).subarray(0, byte)).length;
}

// The editor position of a location's position ({ line, column }, the column
// in bytes) in [source].
function editorPosition(source, { line, column }) {
  const text = source.split('\n')[line - 1] ?? '';
  return { line: line - 1, ch: indexOfByte(text, column) };
}

const INLINE = /file "([^"]+)", (?:line (\d+)|lines (\d+)-(\d+)), characters (\d+)-(\d+)/g;

let shown = null; // { source, locations } of the displayed result

function renderDiagnostics(result, source) {
  diagnostics.textContent = '';
  closePeek();
  const output = result.output;
  shown = { source, locations: [] };
  if (!output) {
    diagnostics.hidden = true;
    return;
  }
  diagnostics.hidden = false;
  // Structured locations: the link is the location's first line, without
  // the colon that ends it.
  const spans = [];
  for (const location of result.locations) {
    const first = indexOfByte(output, location.first);
    let last = output.indexOf('\n', first);
    if (last < 0) last = output.length;
    if (output[last - 1] === ':') last -= 1;
    spans.push({ first, last, location });
  }
  spans.sort((a, b) => a.first - b.first);
  let cursor = 0;
  const plain = (text) => {
    // Locations inside message text.
    let at = 0;
    for (const match of text.matchAll(INLINE)) {
      diagnostics.append(text.slice(at, match.index));
      const [, file, line, firstLine, lastLine, start, end] = match;
      diagnostics.append(link(match[0], {
        role: 'inline', file,
        start: { line: Number(line || firstLine), column: Number(start) },
        end: { line: Number(line || lastLine), column: Number(end) },
      }));
      at = match.index + match[0].length;
    }
    diagnostics.append(text.slice(at));
  };
  for (const { first, last, location } of spans) {
    if (first < cursor) continue;
    plain(output.slice(cursor, first));
    diagnostics.append(link(output.slice(first, last), location));
    cursor = last;
  }
  plain(output.slice(cursor));
}

function link(text, location) {
  const element = document.createElement('a');
  element.href = '#';
  element.className = 'location';
  element.textContent = text;
  shown.locations.push(location);
  element.addEventListener('click', (event) => {
    event.preventDefault();
    go(location);
  });
  element.addEventListener('mouseenter', () => preview(location, element));
  element.addEventListener('focus', () => preview(location, element));
  element.addEventListener('mouseleave', endPreview);
  element.addEventListener('blur', endPreview);
  return element;
}

// Is the location in the program in the editor? (It may have been edited
// since the check; its positions then refer to the lines as they were.)
function local(location) {
  return location.file === current.file;
}

function range(location) {
  const source = shown && shown.source === editor.getValue() ? shown.source : editor.getValue();
  return {
    from: editorPosition(source, location.start),
    to: editorPosition(source, location.end),
  };
}

let hoverMark = null;

function preview(location, element) {
  endPreview();
  if (local(location)) {
    const { from, to } = range(location);
    hoverMark = editor.markText(from, to, { className: 'vox-hover' });
    // The range is out of view: show it next to the link.
    const view = editor.getViewport();
    if (from.line < view.from || to.line >= view.to || !visible(from, to)) {
      tooltip(element, current.file, editor.getValue(), location);
    }
  } else {
    sourceOf(location.file).then((text) => {
      if (document.activeElement === element || element.matches(':hover')) {
        tooltip(element, location.file, text, location);
      }
    });
  }
}

function visible(from, to) {
  const scroll = editor.getScrollInfo();
  const top = editor.charCoords(from, 'local').top;
  const bottom = editor.charCoords(to, 'local').bottom;
  return top >= scroll.top && bottom <= scroll.top + scroll.clientHeight;
}

function endPreview() {
  if (hoverMark) hoverMark.clear();
  hoverMark = null;
  const tip = document.getElementById('tooltip');
  if (tip) tip.remove();
}

function go(location) {
  endPreview();
  if (local(location)) {
    closePeek();
    const { from, to } = range(location);
    editor.focus();
    editor.setSelection(from, to);
    editor.scrollIntoView({ from, to }, 60);
    const flash = editor.markText(from, to, { className: 'vox-flash' });
    setTimeout(() => flash.clear(), 900);
  } else {
    sourceOf(location.file).then((text) => peek(location.file, text, location));
  }
}

// A few lines of [text] around the location, with its range emphasized.
function excerpt(text, location) {
  const lines = text === null ? [] : text.split('\n');
  const figure = document.createElement('pre');
  if (!lines.length || location.start.line > lines.length) {
    figure.textContent = `The source of ${location.file} is not included in the playground.`;
    return figure;
  }
  const first = Math.max(1, location.start.line - 2);
  const last = Math.min(lines.length, location.end.line + 2);
  const width = String(last).length;
  for (let n = first; n <= last; n++) {
    const line = lines[n - 1];
    const from = n === location.start.line ? indexOfByte(line, location.start.column) : 0;
    const to = n === location.end.line ? indexOfByte(line, location.end.column)
      : n > location.start.line && n < location.end.line ? line.length
      : n === location.start.line ? line.length : 0;
    const inside = n >= location.start.line && n <= location.end.line;
    figure.append(`${String(n).padStart(width)} | `);
    if (inside) {
      const mark = document.createElement('mark');
      mark.textContent = line.slice(from, to);
      figure.append(line.slice(0, from), mark, line.slice(to) + '\n');
    } else {
      figure.append(line + '\n');
    }
  }
  return figure;
}

function tooltip(element, file, text, location) {
  endPreviewTooltip();
  const tip = document.createElement('div');
  tip.id = 'tooltip';
  tip.className = 'location-tooltip';
  tip.setAttribute('role', 'tooltip');
  const title = document.createElement('p');
  title.textContent = file === current.file ? `${file}, line ${location.start.line}` : `${file} (read-only)`;
  tip.append(title, excerpt(text, location));
  document.body.append(tip);
  const box = element.getBoundingClientRect();
  tip.style.left = `${Math.max(8, Math.min(box.left, window.innerWidth - tip.offsetWidth - 8)) + window.scrollX}px`;
  tip.style.top = `${box.bottom + 4 + window.scrollY}px`;
}

function endPreviewTooltip() {
  const tip = document.getElementById('tooltip');
  if (tip) tip.remove();
}

// A read-only view of another file's source, below the messages.
function peek(file, text, location) {
  closePeek();
  const panel = document.createElement('div');
  panel.id = 'peek';
  panel.className = 'peek';
  const head = document.createElement('p');
  const close = document.createElement('button');
  close.type = 'button';
  close.textContent = 'Close';
  close.addEventListener('click', closePeek);
  head.append(`${file}, line ${location.start.line} (read-only)`, close);
  panel.append(head, excerpt(text, location));
  diagnostics.after(panel);
  close.focus();
}

function closePeek() {
  const panel = document.getElementById('peek');
  if (panel) panel.remove();
}

// Interface sources shipped with the page, fetched when first shown.
const sources = new Map();

function sourceOf(file) {
  const name = file.split('/').pop();
  if (!sources.has(name)) {
    sources.set(name, (async () => {
      for (const directory of ['lib/src/ocaml/', 'lib/src/vox/']) {
        const response = await fetch(directory + name);
        if (response.ok) return response.text();
      }
      return null;
    })());
  }
  return sources.get(name);
}

// The editor keeps a mark on each message's main location in this program.
let marks = [];

function clearMarks() {
  for (const mark of marks) mark.clear();
  editor.eachLine((line) => editor.removeLineClass(line, 'background'));
  marks = [];
  endPreview();
}

function markEditor(result) {
  clearMarks();
  const output = result.output;
  const locations = [...result.locations].sort((a, b) => a.first - b.first);
  locations.forEach((location, i) => {
    if (location.role !== 'main' || location.file !== current.file) return;
    const { from, to } = range(location);
    // The message: what the output prints after the location and its
    // excerpt, up to the next location.
    const next = i + 1 < locations.length ? locations[i + 1].first : encoder.encode(output).length;
    const message = output.slice(indexOfByte(output, location.last), indexOfByte(output, next));
    marks.push(editor.markText(from, to, {
      className: `vox-mark vox-${location.severity}`,
      attributes: { title: message.trim() },
    }));
    editor.addLineClass(from.line, 'background', `vox-line-${location.severity}`);
  });
}

// ---- Start

if (!window.crossOriginIsolated) {
  show('failed', 'This page needs cross-origin isolation',
    'Z3\'s WebAssembly build uses threads, which browsers allow only on cross-origin ' +
    'isolated pages. Serve the playground with the headers in its README (serve.py ' +
    'sends them), or over HTTP(S) so that the included service worker can add them; ' +
    'opening index.html as a file does not work.');
  loadExamples();
} else {
  startWorker();
  loadExamples();
}
