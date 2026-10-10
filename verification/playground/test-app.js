'use strict';

const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const test = require('node:test');
const vm = require('node:vm');

const source = fs.readFileSync(path.join(__dirname, 'web/app.js'), 'utf8');
function section(first, next) {
  const start = source.indexOf(first);
  const end = source.indexOf(next, start);
  assert.ok(start >= 0 && end > start);
  return source.slice(start, end);
}

function harness() {
  const context = vm.createContext({
    current: { file: 'main.ml' }, value: 'let x = 1',
    ready: true, pending: null, requested: false, nextId: 0,
    editor: { getValue: () => context.value },
    performance: { now: () => 100 }, setTimeout: () => 1,
    clearTimeout() {}, worker: { postMessage() {} },
    checkButton: {}, stopButton: {}, erased: { hidden: true },
    lambda: { textContent: '' }, verdict: { dataset: {} },
    verdictDetail: {}, show() {}, renderDiagnostics() {}, markEditor() {},
  });
  vm.runInContext(section('function check()', 'stopButton.addEventListener(')
    + section('function finish(', 'function show('), context);
  return context;
}

test('a new check clears the previous erased program', () => {
  for (const result of [
    { result: { status: 2, output: 'Error: Unbound value missing',
      lambda: null } },
    { result: { status: 3, output: 'This program cannot be checked',
      lambda: null } },
    { error: 'Worker error' },
  ]) {
    const h = harness();
    h.check();
    h.finish({ result: { status: 0, output: '', queries: 0,
      lambda: 'let x = 1' } });
    assert.equal(h.erased.hidden, false);
    assert.equal(h.lambda.textContent, 'let x = 1');
    h.value = 'let x = missing';
    h.check();
    assert.equal(h.erased.hidden, true);
    assert.equal(h.lambda.textContent, '');
    h.finish(result.result
      ? { result: { queries: 0, ...result.result } } : result);
    assert.equal(h.erased.hidden, true);
    assert.equal(h.lambda.textContent, '');
  }
});
