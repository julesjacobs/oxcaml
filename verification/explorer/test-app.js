'use strict';

const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const test = require('node:test');
const vm = require('node:vm');

const source = fs.readFileSync(path.join(__dirname, 'app/app.js'), 'utf8');
function section(first, next) {
  const start = source.indexOf(`  ${first}`);
  const end = source.indexOf(`  ${next}`, start);
  assert.ok(start >= 0 && end > start);
  return source.slice(start, end);
}

function classes() {
  const values = new Set();
  return {
    add: (...names) => names.forEach((n) => values.add(n)),
    remove: (...names) => names.forEach((n) => values.delete(n)),
    toggle: (name, on) => on ? values.add(name) : values.delete(name),
    contains: (name) => values.has(name),
  };
}

function harness() {
  const root = { path: '/', dir: true };
  const da = { path: 'a/', dir: true, parent: root };
  const db = { path: 'b/', dir: true, parent: root };
  const a = { path: 'a.ml', lang: 'ocaml', lines: 100, parent: da };
  const b = { path: 'b.ml', lang: 'ocaml', lines: 100, parent: db };
  const requests = [], built = [], headers = [];
  const elements = new Map();
  const element = (id) => {
    if (!elements.has(id)) elements.set(id, { classList: classes() });
    return elements.get(id);
  };
  const visible = [1, 2, 20, 21, 22, 100].map((i) => ({
    id: `L${i}`, classList: classes(),
  }));
  const lines = { offsetTop: 30 };
  const code = {
    innerHTML: '', scrollTop: 0, clientHeight: 360,
    querySelector: () => code.innerHTML === 'built' ? lines : null,
    querySelectorAll: (selector) => selector === '.ln.hl'
      ? visible.filter((el) => el.classList.contains('hl')) : visible,
  };
  const S = {
    root, focus: root, file: null, entry: null, range: null,
    byPath: new Map([['/', root], [da.path, da], [db.path, db],
      [a.path, a], [b.path, b]]),
    demoById: new Map(), content: { tours: [] },
  };
  const context = vm.createContext({
    S, code, LH: 18, phone: { matches: true },
    document: { body: { classList: classes() },
      getElementById: (id) => visible.find((el) => el.id === id) },
    $: element, location: { hash: '' },
    esc: String, githubUrl: (n) => n.path,
    resize() {}, layout() {}, render() {}, renderChunks() {},
    markActiveNote() {}, ensureVisible() {}, descend: (n) => n,
    focusFor: (n) => n.parent,
    fileHeader: (n, range) => headers.push([n, range]),
    fetchSource: (n) => new Promise((resolve, reject) => {
      requests.push({ n, resolve, reject });
    }),
    buildCode: (n, entry) => {
      built.push(n); S.entry = entry; code.innerHTML = 'built';
    },
    panelNode: (n) => { S.panel = n.path; },
    panelTours: () => { S.panel = 'tours'; },
    panelTour: () => { S.panel = 'tour'; },
    panelTourIntro: () => { S.panel = 'tour-intro'; },
    view: (n) => { S.focus = n; }, crumbsAfter() {},
  });
  const rangeStart = source.includes('  function boundedRange(')
    ? 'function boundedRange(' : 'function applyRange(';
  vm.runInContext('let fileRequest = 0, routeRequest = 0;\n'
    + section('async function openFile(', 'function buildCode(')
    + section(rangeStart, 'function githubUrl(')
    + section('function closeFile(', '// ')
    + section('function parseHash(', 'async function route(')
    + section('async function route(', 'function crumbsAfter('), context);
  const entry = () => ({ lines: Array(100).fill('x') });
  return { context, S, a, b, root, requests, built, headers, code, visible,
    entry,
    open: (n, range) => context.openFile(n, range),
    route: (hash) => {
      context.location.hash = hash;
      return context.route();
    },
  };
}

test('same-file navigation while loading keeps the latest lines', async () => {
  const h = harness();
  const first = h.open(h.a, [1, 2]);
  const second = h.open(h.a, [20, 22]);
  for (const r of h.requests) r.resolve(h.entry());
  await Promise.all([first, second]);
  assert.equal(h.S.file, h.a);
  assert.deepEqual(Array.from(h.S.range), [20, 22]);
  assert.deepEqual(h.visible.filter((el) => el.classList.contains('hl'))
    .map((el) => el.id), ['L20', 'L21', 'L22']);
});

test('A-B-A rejects the first A completion', async () => {
  const h = harness();
  const first = h.open(h.a, [1, 2]);
  const middle = h.open(h.b, null);
  const last = h.open(h.a, [20, 22]);
  h.requests[0].resolve(h.entry());
  await first;
  assert.deepEqual(h.built, []);
  h.requests[2].resolve(h.entry());
  await last;
  h.requests[1].resolve(h.entry());
  await middle;
  assert.deepEqual(h.built, [h.a]);
});

test('close and file switch discard stale source and stale entry', async () => {
  const h = harness();
  const first = h.open(h.a, null);
  h.requests[0].resolve(h.entry());
  await first;
  const second = h.open(h.b, null);
  assert.equal(h.S.entry, null);
  h.context.closeFile();
  h.requests[1].resolve(h.entry());
  await second;
  assert.equal(h.S.file, null);
  assert.equal(h.S.entry, null);
  assert.equal(h.code.innerHTML, '');
});

test('failed source can be retried by same-file navigation', async () => {
  const h = harness();
  const first = h.open(h.a, null);
  h.requests[0].reject(new Error('unavailable'));
  await first;
  const second = h.open(h.a, [20, 22]);
  assert.equal(h.requests.length, 2);
  h.requests[1].resolve(h.entry());
  await second;
  assert.deepEqual(h.built, [h.a]);
});

test('late route completion preserves directory and tour panels', async () => {
  for (const hash of ['#p/', '#tours', '#tour/example/1']) {
    const h = harness();
    h.S.content.tours.push({ id: 'example', stops: [
      { path: h.b.path, lines: [20, 22] },
    ] });
    const first = h.route('#p/a.ml:L1-L2');
    await h.route(hash);
    const panel = h.S.panel, focus = h.S.focus;
    for (const r of h.requests) r.resolve(h.entry());
    await first;
    await Promise.resolve();
    assert.equal(h.S.panel, panel);
    assert.equal(h.S.focus, focus);
  }
});

test('line selection is bounded by source and rendered lines', () => {
  const h = harness();
  h.S.entry = h.entry(); h.code.innerHTML = 'built';
  for (const range of [
    '[9007199254740992, 9007199254740992]',
    '[1, Infinity]', '[1, 9007199254740991]',
    '[20, 22]', '[22, 20]', '[0, 2]',
  ]) {
    vm.runInContext(`applyRange(${range}, true)`, h.context,
      { timeout: 100 });
  }
  assert.deepEqual(h.visible.filter((el) => el.classList.contains('hl'))
    .map((el) => el.id), ['L1', 'L2']);
  assert.ok(Number.isFinite(h.code.scrollTop));
});

function viewHarness() {
  const root = { path: '/' };
  const a = { path: 'a/', parent: root };
  const b = { path: 'b/', parent: root };
  const nodes = [root, a, b];
  const layout = () => new Map(nodes.map((n, i) => [n, {
    x: i * 20, y: i * 20, w: 100, h: 100,
  }]));
  const S = { focus: a, layout: layout(), W: 200, H: 200, anim: 0 };
  const frames = [], draws = [], renders = [];
  const phone = { matches: false };
  const context = vm.createContext({
    S, phone, reduceMotion: false,
    document: { visibilityState: 'visible' },
    performance: { now: () => 0 },
    octx: { clearRect() {} },
    requestAnimationFrame: (frame) => frames.push(frame),
    layout,
    render: () => renders.push(S.focus),
    renderList: () => renders.push(S.focus),
    drawMap: (rectAt, focus) => {
      draws.push(focus);
      for (const n of nodes) rectAt(n);
    },
    isAncestor: (ancestor, n) => {
      for (; n; n = n.parent) if (n === ancestor) return true;
      return false;
    },
  });
  vm.runInContext(section('const ease =', 'function hit('), context);
  return { S, root, a, b, frames, draws, renders, phone,
    view: (focus, opts) => context.view(focus, opts),
  };
}

async function settleView() {
  for (let i = 0; i < 4; i++) await Promise.resolve();
}

test('newer common-ancestor navigation cancels an older sibling zoom',
  async () => {
    const h = viewHarness();
    const first = h.view(h.b);
    const latest = h.view(h.root);
    const activeAnimation = h.S.anim;
    h.frames.shift()(500);
    await settleView();
    assert.equal(h.S.focus, h.root);
    assert.equal(h.S.anim, activeAnimation);
    assert.equal(h.frames.length, 1);
    assert.deepEqual(h.draws, []);
    h.frames.shift()(500);
    await Promise.all([first, latest]);
    assert.equal(h.S.focus, h.root);
    assert.equal(h.S.anim, 0);
  });

test('instant navigation invalidates stale frames after animation IDs repeat',
  async () => {
    const h = viewHarness();
    const first = h.view(h.b);
    await h.view(h.root, { instant: true });
    const latest = h.view(h.a);
    h.frames.shift()(500);
    await settleView();
    assert.deepEqual(h.draws, []);
    assert.equal(h.S.focus, h.a);
    assert.ok(h.S.anim);
    assert.equal(h.frames.length, 1);
    h.frames.shift()(500);
    await Promise.all([first, latest]);
    assert.equal(h.S.focus, h.a);
    assert.equal(h.renders.at(-1), h.a);
    assert.equal(h.S.anim, 0);
  });

test('an uninterrupted sibling zoom completes both animation legs',
  async () => {
    const h = viewHarness();
    const moving = h.view(h.b);
    h.frames.shift()(500);
    await settleView();
    assert.equal(h.S.focus, h.b);
    assert.equal(h.frames.length, 1);
    h.frames.shift()(500);
    await moving;
    assert.equal(h.S.focus, h.b);
    assert.equal(h.renders.at(-1), h.b);
    assert.equal(h.draws.length, 2);
    assert.equal(h.S.anim, 0);
  });

test('phone navigation cancels desktop animation state', async () => {
  const h = viewHarness();
  const first = h.view(h.b);
  h.phone.matches = true;
  await h.view(h.root);
  h.frames.shift()(500);
  await first;
  assert.equal(h.S.focus, h.root);
  assert.equal(h.S.anim, 0);
  assert.equal(h.renders.at(-1), h.root);
  assert.deepEqual(h.draws, []);
});
