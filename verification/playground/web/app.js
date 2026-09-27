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
  if (ready) check();
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
let pending = null; // { id, started, timer }
let nextId = 0;

function startWorker() {
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
      if (current) check();
    } else if (data.failed) {
      show('failed', 'The checker could not be loaded', data.failed);
    } else if (pending && data.id === pending.id) {
      finish(data);
    }
  };
  worker.onerror = (event) => {
    show('failed', 'The checker stopped', event.message || 'An error occurred in the worker.');
    pending = null;
  };
}

function check() {
  if (!ready || !current) return;
  if (pending) return;
  const id = ++nextId;
  pending = {
    id,
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
  worker.terminate();
  stopButton.hidden = true;
  show('stopped', 'Stopped', 'The check was stopped. Restarting the checker…');
  startWorker();
});

checkButton.addEventListener('click', () => check());

function finish({ result, error }) {
  const { source, started, timer } = pending;
  clearTimeout(timer);
  pending = null;
  stopButton.hidden = true;
  checkButton.disabled = false;
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
  renderDiagnostics(result.output, source);
  if (result.lambda) {
    lambda.textContent = result.lambda;
    erased.hidden = false;
  }
  if (source === editor.getValue()) markEditor(result.output);
}

function show(state, text, detail) {
  verdict.dataset.state = state;
  verdictText.textContent = text;
  verdictDetail.textContent = detail;
}

function clearResult() {
  diagnostics.hidden = true;
  diagnostics.textContent = '';
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

// A location as the compiler prints it. Character positions are byte
// offsets in the line.
const LOCATION = /^File "([^"]*)", (?:line (\d+)|lines (\d+)-(\d+)), characters (\d+)-(\d+):$/;

// The compiler's messages as blocks, each starting at a location.
function blocks(output) {
  const result = [];
  let block = null;
  for (const line of output.split('\n')) {
    const match = LOCATION.exec(line);
    if (match) {
      const [, file, line1, first, last, start, end] = match;
      block = {
        file,
        from: { line: Number(line1 || first), byte: Number(start) },
        to: { line: Number(line1 || last), byte: Number(end) },
        lines: [line],
      };
      result.push(block);
    } else if (block) {
      block.lines.push(line);
    }
  }
  for (const b of result) {
    const text = b.lines.join('\n');
    b.kind = /^Error/m.test(text) ? 'error' : /^Warning/m.test(text) ? 'warning' : 'note';
  }
  return result;
}

// The editor position of a byte offset in a line of [source].
function position(source, { line, byte }) {
  const text = source.split('\n')[line - 1] ?? '';
  const bytes = new TextEncoder().encode(text);
  const ch = new TextDecoder().decode(bytes.subarray(0, Math.min(byte, bytes.length))).length;
  return { line: line - 1, ch };
}

function renderDiagnostics(output, source) {
  diagnostics.textContent = '';
  if (!output) {
    diagnostics.hidden = true;
    return;
  }
  diagnostics.hidden = false;
  for (const line of output.replace(/\n$/, '').split('\n')) {
    const match = LOCATION.exec(line);
    if (match && match[1] === current.file) {
      const [, , line1, first, , start] = match;
      const link = document.createElement('a');
      link.href = '#';
      link.textContent = line;
      const from = position(source, { line: Number(line1 || first), byte: Number(start) });
      link.addEventListener('click', (event) => {
        event.preventDefault();
        editor.focus();
        editor.setCursor(from);
        editor.scrollIntoView(from, 80);
      });
      diagnostics.append(link, '\n');
    } else {
      diagnostics.append(line + '\n');
    }
  }
}

let marks = [];

function clearMarks() {
  for (const mark of marks) mark.clear();
  editor.eachLine((line) => editor.removeLineClass(line, 'background'));
  marks = [];
}

function markEditor(output) {
  clearMarks();
  const source = editor.getValue();
  for (const block of blocks(output)) {
    if (block.file !== current.file) continue;
    const from = position(source, block.from);
    const to = position(source, block.to);
    const title = block.lines.slice(block.lines.findIndex((l) => /^(Error|Warning)/.test(l)))
      .join('\n').trim();
    marks.push(editor.markText(from, to, {
      className: `vox-mark vox-${block.kind}`,
      attributes: { title: block.kind === 'note' ? block.lines.slice(-2).join('\n').trim() : title },
    }));
    if (block.kind !== 'note') editor.addLineClass(from.line, 'background', `vox-line-${block.kind}`);
  }
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
