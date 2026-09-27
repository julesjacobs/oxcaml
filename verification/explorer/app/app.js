// The Vox source explorer: a zoomable treemap of the repository at one
// commit, coloured by Vox's changes against the upstream merge base, with
// descriptions, a file view and tours. Static: it reads data/*.json and
// src/<path>.txt, written by build.py.
'use strict';
(() => {
  const version = document.currentScript.dataset.version;
  const $ = (id) => document.getElementById(id);
  const escText = (s) => String(s).replace(/[&<>]/g, (c) => (c === '&' ? '&amp;' : c === '<' ? '&lt;' : '&gt;'));
  const esc = (s) => String(s).replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c]);
  const fmt = (n) => n.toLocaleString('en-US');
  const kfmt = (n) => (n >= 1e6 ? (n / 1e6).toFixed(n >= 1e7 ? 0 : 1) + 'M' : n >= 1e3 ? (n / 1e3).toFixed(n >= 1e4 ? 0 : 1) + 'k' : String(n));
  const pct = (a, b) => (b ? Math.round((100 * a) / b) : 0);
  const LANG = { ocaml: 'OCaml', c: 'C', other: 'Other', binary: 'Binary' };
  const reduceMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  const phone = window.matchMedia('(max-width: 760px)');

  const S = {
    tree: null, content: null, demos: [], demoById: new Map(), byPath: new Map(), root: null, all: [],
    focus: null, layout: null, selected: null, hover: null,
    scope: 'compiler', filter: 'all', demo: null, size: 'lines', outline: false,
    file: null, range: null, tour: null, stop: 0, panel: 'node', anim: 0, W: 0, H: 0,
  };

  // ---------------------------------------------------------------- data

  function load() {
    const get = (name) => fetch(`data/${name}.json?v=${version}`, { cache: 'no-cache' }).then((r) => {
      if (!r.ok) throw new Error(`data/${name}.json: ${r.status}`);
      return r.json();
    });
    return Promise.all([get('tree'), get('content'), get('demos')]).then(([tree, content, demos]) => {
      S.tree = tree;
      S.content = content;
      S.demos = demos.demos;
      for (const d of S.demos) S.demoById.set(d.id, d);
      buildTree();
    });
  }

  function node(name, path, parent, dir) {
    const n = { name, path, parent, dir, depth: parent ? parent.depth + 1 : 0, lines: 0, v: 0, voxLines: 0, demos: null };
    if (dir) Object.assign(n, { children: [], files: 0, newFiles: 0, newLines: 0, modFiles: 0, modAdded: 0, modRemoved: 0, demoCounts: new Map() });
    S.byPath.set(path, n);
    S.all.push(n);
    return n;
  }

  function buildTree() {
    const root = (S.root = node('', '/', null, true));
    for (const row of S.tree.files) {
      const [path, lines, lang, status, added = 0, removed = 0, ranges = '', cat = 0] = row;
      const parts = path.split('/');
      let d = root;
      for (let i = 0; i < parts.length - 1; i++) {
        const p = parts.slice(0, i + 1).join('/') + '/';
        let next = S.byPath.get(p);
        if (!next) {
          next = node(parts[i], p, d, true);
          d.children.push(next);
        }
        d = next;
      }
      const f = node(parts[parts.length - 1], path, d, false);
      Object.assign(f, { lines, lang, status, added, removed, ranges: ranges || '', cat: !!cat });
      f.voxLines = status === 'n' ? lines : status === 'm' ? added : 0;
      d.children.push(f);
    }
    // Demo membership: the page's listed sources, the files it quotes, and
    // the modules its line counts cover.
    for (const demo of S.demos) {
      const roles = new Map();
      const add = (p, role, own) => {
        if (!roles.has(p)) roles.set(p, { role, own });
        else if (own) roles.get(p).own = true;
      };
      for (const s of demo.sources) add(s.path, s.role || 'Listed on the demo page', true);
      for (const s of demo.sources) roles.get(s.path).main = true;
      for (const p of demo.own) add(p, 'Module of the demo', true);
      for (const p of demo.quoted) add(p, 'Quoted on the demo page', true);
      for (const p of demo.shared) add(p, 'Shared module the demo uses', false);
      demo.files = roles;
      for (const [p, r] of roles) {
        const f = S.byPath.get(p);
        if (!f) continue;
        (f.demos || (f.demos = [])).push({ id: demo.id, role: r.role, own: r.own, main: !!r.main });
        if (r.own) for (let d = f.parent; d; d = d.parent) d.demoCounts.set(demo.id, (d.demoCounts.get(demo.id) || 0) + 1);
      }
    }
    (function sum(n) {
      if (!n.dir) return;
      for (const c of n.children) {
        sum(c);
        if (c.dir) {
          for (const k of ['lines', 'files', 'newFiles', 'newLines', 'modFiles', 'modAdded', 'modRemoved', 'voxLines']) n[k] += c[k];
        } else {
          n.lines += c.lines;
          n.files += 1;
          n.voxLines += c.voxLines;
          if (c.status === 'n') { n.newFiles++; n.newLines += c.lines; }
          if (c.status === 'm') { n.modFiles++; n.modAdded += c.added; n.modRemoved += c.removed; }
        }
      }
    })(root);
    S.focus = root;
  }

  function inScope(path) {
    const scope = S.tree.scopes.find((s) => s.id === S.scope);
    if (!scope || !scope.prefixes) return true;
    return scope.prefixes.some((p) => path.startsWith(p));
  }

  function computeValues() {
    const demo = S.filter === 'demo' && S.demo ? S.demoById.get(S.demo) : null;
    (function sum(n) {
      if (!n.dir) {
        let keep = inScope(n.path);
        if (keep && S.filter === 'vox') keep = n.status !== 'u';
        if (keep && S.filter === 'upstream') keep = n.status !== 'n';
        if (keep && demo) keep = demo.files.has(n.path);
        n.v = keep ? (S.size === 'vox' ? n.voxLines : n.lines) : 0;
        n.vf = n.v > 0 ? 1 : 0;
        return n.v;
      }
      let v = 0, vf = 0;
      for (const c of n.children) { v += sum(c); vf += c.vf; }
      n.vf = vf;
      n.children.sort((a, b) => b.v - a.v || b.lines - a.lines);
      n.v = v;
      return v;
    })(S.root);
  }

  const desc = (n) => (S.content.descriptions[n.path] || null);
  const isAncestor = (a, b) => { for (let n = b; n; n = n.parent) if (n === a) return true; return false; };
  const catalogueUrl = (html) => html.replaceAll('{catalogue}', S.tree.catalogue);
  const encodePath = (p) => (p === '/' ? '' : p.split('/').map(encodeURIComponent).join('/'));

  // -------------------------------------------------------------- layout

  const HEAD = 15, HEAD_W = 44, HEAD_H = 30;

  function squarify(nodes, x0, y0, x1, y1, place) {
    const ratio = (1 + Math.sqrt(5)) / 2;
    let value = 0;
    for (const n of nodes) value += n.v;
    let i0 = 0;
    const count = nodes.length;
    while (i0 < count) {
      const dx = x1 - x0, dy = y1 - y0;
      let i1 = i0;
      let sum = nodes[i1].v, min = sum, max = sum;
      const alpha = Math.max(dy / dx, dx / dy) / (value * ratio);
      let beta = sum * sum * alpha;
      let best = Math.max(max / beta, beta / min);
      for (i1 = i0 + 1; i1 < count; i1++) {
        const v = nodes[i1].v;
        sum += v;
        if (v < min) min = v;
        if (v > max) max = v;
        beta = sum * sum * alpha;
        const r = Math.max(max / beta, beta / min);
        if (r > best) { sum -= v; break; }
        best = r;
      }
      if (dx < dy) {
        const y = value ? y0 + (dy * sum) / value : y1;
        let x = x0;
        for (let i = i0; i < i1; i++) {
          const w = sum ? ((x1 - x0) * nodes[i].v) / sum : 0;
          place(nodes[i], x, y0, w, y - y0);
          x += w;
        }
        y0 = y;
      } else {
        const x = value ? x0 + (dx * sum) / value : x1;
        let y = y0;
        for (let i = i0; i < i1; i++) {
          const h = sum ? ((y1 - y0) * nodes[i].v) / sum : 0;
          place(nodes[i], x0, y, x - x0, h);
          y += h;
        }
        x0 = x;
      }
      value -= sum;
      i0 = i1;
    }
  }

  function inner(r, isFocus) {
    if (isFocus) return { x: r.x, y: r.y, w: r.w, h: r.h, head: 0 };
    const head = r.w >= HEAD_W && r.h >= HEAD_H ? HEAD : 0;
    const pad = r.w > 12 && r.h > 12 ? 2 : r.w > 4 && r.h > 4 ? 1 : 0;
    return { x: r.x + pad, y: r.y + (head || pad), w: r.w - 2 * pad, h: r.h - (head || pad) - pad, head };
  }

  function layout(focus, W, H) {
    const L = new Map();
    (function place(n, x, y, w, h, isFocus) {
      const r = { x, y, w, h };
      L.set(n, r);
      if (!n.dir || w < 1 || h < 1) return;
      const kids = n.children.filter((c) => c.v > 0);
      if (!kids.length) return;
      const i = inner(r, isFocus);
      if (i.w <= 0 || i.h <= 0) return;
      squarify(kids, i.x, i.y, i.x + i.w, i.y + i.h, (c, cx, cy, cw, ch) => place(c, cx, cy, cw, ch, false));
    })(focus, 0, 0, W, H, true);
    return L;
  }

  // -------------------------------------------------------------- colour

  const GREY = { ocaml: [188, 195, 201], c: [170, 179, 187], other: [214, 218, 222], binary: [226, 229, 231] };
  const NEW = [61, 111, 148], MOD_LO = [244, 230, 198], MOD_HI = [192, 127, 16];
  const mix = (a, b, t) => [0, 1, 2].map((i) => Math.round(a[i] + (b[i] - a[i]) * t));
  const rgb = (c) => `rgb(${c[0]},${c[1]},${c[2]})`;

  function colour(n) {
    if (!n.dir) {
      if (n.status === 'n') return NEW;
      if (n.status === 'm') return mix(MOD_LO, MOD_HI, Math.sqrt(Math.min(1, n.added / Math.max(1, n.lines))));
      return GREY[n.lang] || GREY.other;
    }
    let c = [196, 202, 207];
    if (n.lines) {
      if (n.modAdded) c = mix(c, MOD_HI, Math.min(1, Math.sqrt(n.modAdded / n.lines)));
      if (n.newLines) c = mix(c, NEW, n.newLines / n.lines);
    }
    return c;
  }

  // ---------------------------------------------------------------- draw

  const canvas = $('map-canvas'), overlay = $('map-overlay');
  const ctx = canvas.getContext('2d'), octx = overlay.getContext('2d');
  const FONT = '11px -apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif';
  const BOLD = '600 11px -apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif';

  function resize() {
    const box = $('map').getBoundingClientRect();
    const dpr = window.devicePixelRatio || 1;
    S.W = Math.max(10, Math.floor(box.width));
    S.H = Math.max(10, Math.floor(box.height));
    for (const c of [canvas, overlay]) {
      c.width = S.W * dpr;
      c.height = S.H * dpr;
      c.getContext('2d').setTransform(dpr, 0, 0, dpr, 0, 0);
    }
  }

  function outlined(n) {
    if (!n.demos) return 0;
    if (S.filter === 'demo' && S.demo) {
      const m = n.demos.find((d) => d.id === S.demo);
      return m ? (m.main ? 3 : m.own ? 2 : 1) : 0;
    }
    return S.outline && n.demos.some((d) => d.own) ? 2 : 0;
  }

  const widths = new Map();
  function measure(text, font) {
    const key = font + '\u0000' + text;
    let w = widths.get(key);
    if (w === undefined) {
      ctx.font = font;
      w = ctx.measureText(text).width;
      if (widths.size > 20000) widths.clear();
      widths.set(key, w);
    }
    return w;
  }

  function label(text, x, y, w, font, colour) {
    const room = w - 6;
    if (room < 12) return;
    let shown = text;
    if (measure(text, font) > room) {
      let lo = 0, hi = text.length;
      while (lo < hi) {
        const mid = (lo + hi + 1) >> 1;
        if (measure(text.slice(0, mid) + '…', font) <= room) lo = mid; else hi = mid - 1;
      }
      if (lo < 2) return;
      shown = text.slice(0, lo) + '…';
    }
    ctx.font = font;
    ctx.fillStyle = colour;
    ctx.fillText(shown, x + 3, y);
  }

  function drawMap(rectOf, root) {
    ctx.clearRect(0, 0, S.W, S.H);
    ctx.textBaseline = 'middle';
    const outlines = [];
    (function draw(n, isFocus) {
      const r = rectOf(n);
      if (!r || r.w < 0.4 || r.h < 0.4 || r.x > S.W || r.y > S.H || r.x + r.w < 0 || r.y + r.h < 0) return;
      if (!n.dir) {
        const c = colour(n);
        ctx.fillStyle = rgb(c);
        if (r.w > 2.5 && r.h > 2.5) ctx.fillRect(r.x + 0.5, r.y + 0.5, r.w - 1, r.h - 1);
        else ctx.fillRect(r.x, r.y, r.w, r.h);
        if (r.w > 40 && r.h > 13) {
          const dark = n.status === 'n' || (n.status === 'm' && n.added / Math.max(1, n.lines) > 0.3);
          label(n.name, r.x, r.y + Math.min(r.h / 2, 9), r.w, FONT, dark ? '#fff' : '#2b343b');
          if (r.h > 28 && r.w > 50) label(kfmt(n.lines), r.x, r.y + 21, r.w, FONT, dark ? '#e3edf4' : '#5d6870');
        }
        const o = outlined(n);
        if (o) outlines.push([r, o]);
        return;
      }
      if (!isFocus) {
        ctx.fillStyle = n.depth % 2 ? '#e4e8eb' : '#edf0f2';
        ctx.fillRect(r.x, r.y, r.w, r.h);
        if (r.w < 7 || r.h < 7) {
          ctx.fillStyle = rgb(colour(n));
          ctx.fillRect(r.x + 0.5, r.y + 0.5, Math.max(0.5, r.w - 1), Math.max(0.5, r.h - 1));
          return;
        }
        const i = inner(r, false);
        if (i.head) {
          const size = kfmt(n.v);
          const sw = r.w > 90 ? measure(size, FONT) + 8 : 0;
          label(n.name + '/', r.x, r.y + 8, r.w - sw, BOLD, '#27323a');
          if (sw) {
            ctx.font = FONT;
            ctx.fillStyle = '#66727b';
            ctx.fillText(size, r.x + r.w - sw + 4, r.y + 8);
          }
        }
      }
      for (const c of n.children) {
        if (c.v <= 0 && !S.anim) continue;
        draw(c, false);
      }
    })(root, true);
    for (const [r, o] of outlines) {
      const w = o === 3 ? 3 : o === 2 ? 1.5 : 1;
      ctx.lineWidth = w;
      ctx.strokeStyle = o === 3 ? '#b3413a' : o === 2 ? '#1c2730' : '#7d8c97';
      ctx.setLineDash(o === 1 ? [3, 2] : []);
      ctx.strokeRect(r.x + w / 2, r.y + w / 2, Math.max(1, r.w - w), Math.max(1, r.h - w));
    }
    ctx.setLineDash([]);
  }

  function drawOverlay() {
    octx.clearRect(0, 0, S.W, S.H);
    if (S.anim) return;
    const mark = (n, colour, width) => {
      let r = S.layout.get(n);
      if (!r) {
        // Not laid out (filtered away or too small): mark the nearest visible ancestor.
        for (let a = n.parent; a && !r; a = a.parent) r = S.layout.get(a);
        if (!r) return;
      }
      let { x, y, w, h } = r;
      if (w < 8) { x -= (8 - w) / 2; w = 8; }
      if (h < 8) { y -= (8 - h) / 2; h = 8; }
      octx.lineWidth = width;
      octx.strokeStyle = colour;
      octx.strokeRect(x + width / 2, y + width / 2, w - width, h - width);
    };
    if (S.selected && S.selected !== S.focus) mark(S.selected, '#b3413a', 2.5);
    if (S.hover && S.hover !== S.selected && S.hover !== S.focus) mark(S.hover, '#16232c', 2);
  }

  function render() {
    if (phone.matches) return renderList();
    const L = S.layout;
    drawMap((n) => L.get(n), S.focus);
    drawOverlay();
  }

  const ease = (t) => (t < 0.5 ? 4 * t * t * t : 1 - Math.pow(-2 * t + 2, 3) / 2);
  const lerp = (a, b, t) => ({ x: a.x + (b.x - a.x) * t, y: a.y + (b.y - a.y) * t, w: a.w + (b.w - a.w) * t, h: a.h + (b.h - a.h) * t });
  const collapse = (r) => ({ x: r.x + r.w / 2, y: r.y + r.h / 2, w: 0, h: 0 });
  const affine = (from, W, H) => {
    const sx = W / Math.max(from.w, 1e-6), sy = H / Math.max(from.h, 1e-6);
    return (r) => ({ x: (r.x - from.x) * sx, y: (r.y - from.y) * sy, w: r.w * sx, h: r.h * sy });
  };

  function animate(rectAt, root, duration) {
    return new Promise((resolve) => {
      const token = ++S.anim;
      const start = performance.now();
      octx.clearRect(0, 0, S.W, S.H);
      function frame(now) {
        if (token !== S.anim) return resolve(false);
        const t = Math.min(1, (now - start) / duration);
        const e = ease(t);
        drawMap((n) => rectAt(n, e), root);
        if (t < 1) requestAnimationFrame(frame);
        else resolve(true);
      }
      requestAnimationFrame(frame);
    });
  }

  // Move the view to `focus`, animating from the current layout. Zooming
  // into a descendant grows it to fill the view while the rest flies out;
  // zooming out is the reverse; anything else goes through the common
  // ancestor. Filter changes morph each rectangle to its new place.
  async function view(focus, opts = {}) {
    if (phone.matches) {
      S.focus = focus;
      S.layout = new Map();
      renderList();
      return;
    }
    const from = S.focus, L1 = S.layout;
    const W = S.W, H = S.H;
    const L2 = layout(focus, W, H);
    S.focus = focus;
    S.layout = L2;
    if (!L1 || reduceMotion || opts.instant || document.visibilityState !== 'visible') {
      S.anim = 0;
      render();
      return;
    }
    const d = opts.duration || 420;
    if (from === focus) {
      await animate((n, t) => {
        const a = L1.get(n), b = L2.get(n);
        if (!a && !b) return null;
        return lerp(a || collapse(b), b || collapse(a), t);
      }, focus, d);
    } else if (isAncestor(from, focus)) {
      const A = affine(L1.get(focus), W, H);
      await animate((n, t) => {
        const a = L1.get(n);
        if (isAncestor(focus, n)) {
          const b = L2.get(n) || (a && A(a));
          if (!b) return null;
          return lerp(a || collapse(b), b, t);
        }
        return a ? lerp(a, A(a), t) : null;
      }, from, d);
    } else if (isAncestor(focus, from)) {
      const B = affine(L2.get(from), W, H);
      await animate((n, t) => {
        const b = L2.get(n);
        if (isAncestor(from, n)) {
          const a = L1.get(n) || (b && B(b));
          if (!a) return null;
          return lerp(a, b || collapse(a), t);
        }
        return b ? lerp(B(b), b, t) : null;
      }, focus, d);
    } else {
      let common = from;
      while (!isAncestor(common, focus)) common = common.parent;
      S.focus = from;
      S.layout = L1;
      await view(common, { duration: d * 0.7 });
      if (S.focus !== common) return;
      await view(focus, { duration: d * 0.7 });
      return;
    }
    if (S.focus === focus) {
      S.anim = 0;
      render();
    }
  }

  function hit(x, y) {
    const L = S.layout;
    let n = S.focus;
    for (;;) {
      if (!n.dir) return n;
      const r = L.get(n);
      if (n !== S.focus && (r.w < 7 || r.h < 7)) return n;
      let next = null;
      for (const c of n.children) {
        const q = L.get(c);
        if (q && c.v > 0 && x >= q.x && x < q.x + q.w && y >= q.y && y < q.y + q.h) { next = c; break; }
      }
      if (!next) return n;
      n = next;
    }
  }

  // ----------------------------------------------------- phone tree list

  function renderList() {
    const list = $('tree-list');
    list.hidden = false;
    const kids = S.focus.children.filter((c) => c.v > 0);
    const max = kids.length ? kids[0].v : 1;
    list.innerHTML = (S.focus.parent ? `<li data-path="${esc(S.focus.parent.path)}" class="is-dir up"><span class="chip"></span><span class="name">..</span></li>` : '')
      + kids.map((c) => `<li data-path="${esc(c.path)}" class="${c.dir ? 'is-dir' : 'is-file'}"><span class="chip" style="background:${rgb(colour(c))}"></span>`
        + `<span class="name">${esc(c.name)}${c.dir ? '/' : ''}</span><span class="sizebar" style="width:${Math.max(2, Math.round((60 * c.v) / max))}px"></span>`
        + `<span class="n">${kfmt(c.v)}${c.voxLines ? ` · Vox ${kfmt(c.voxLines)}` : ''}</span></li>`).join('');
  }

  // ------------------------------------------------------------- tooltip

  function statusText(n) {
    if (n.dir) {
      const parts = [`${fmt(n.files)} files`, `${fmt(n.lines)} lines`];
      const vox = [];
      if (n.newFiles) vox.push(`${fmt(n.newFiles)} new files (${fmt(n.newLines)} lines)`);
      if (n.modFiles) vox.push(`${fmt(n.modFiles)} modified (+${fmt(n.modAdded)} −${fmt(n.modRemoved)})`);
      return parts.join(' · ') + (vox.length ? ` · Vox: ${vox.join(', ')}` : ' · no Vox changes');
    }
    const base = `${fmt(n.lines)} lines · ${LANG[n.lang] || n.lang}`;
    if (n.status === 'n') return `${base} · new in Vox`;
    if (n.status === 'm') return `${base} · modified by Vox: +${fmt(n.added)} −${fmt(n.removed)} (${pct(n.added, n.lines)}% of lines)`;
    return `${base} · upstream, unchanged`;
  }

  function showTip(n, x, y) {
    const tip = $('tip');
    if (!n || n === S.focus) { tip.hidden = true; return; }
    const d = desc(n);
    const dirPart = n.parent && n.parent.path !== '/' ? n.parent.path : '';
    let html = `<div class="tip-path"><span>${esc(dirPart)}</span>${esc(n.name)}${n.dir ? '/' : ''}</div><div class="tip-stats">${esc(statusText(n))}</div>`;
    if (d && d.what) html += `<p>${esc(d.what.short)}</p>`;
    if (d && d.vox) html += `<p class="tip-vox">${esc(d.vox.short)}</p>`;
    if (!n.dir && n.demos) {
      const names = n.demos.filter((m) => m.own).map((m) => S.demoById.get(m.id).title);
      const shared = n.demos.filter((m) => !m.own).length;
      if (names.length) html += `<p>Demo: ${esc(names.join(', '))}</p>`;
      else if (shared) html += `<p>Used by ${shared} demo${shared > 1 ? 's' : ''}</p>`;
    }
    tip.innerHTML = html;
    tip.hidden = false;
    const w = tip.offsetWidth, h = tip.offsetHeight;
    tip.style.left = `${Math.min(x + 14, S.W - w - 4)}px`;
    tip.style.top = `${y + 16 + h > S.H ? Math.max(0, y - h - 10) : y + 16}px`;
  }

  // -------------------------------------------------------- highlighting

  const OCAML_KEYWORDS = new Set('and as assert begin class constraint do done downto else end exception external false for fun function functor if in include inherit initializer lazy let match method module mutable new nonrec object of open or private rec sig struct then to true try type val virtual when while with'.split(' '));
  const OCAML_MODES = new Set('ghost total immutable immutable_data unique local global read write portable contended many once aliased unyielding stateless stateful nonportable uncontended shared ghost_ refine_ assume_ unreachable_ borrow_ exclave_ stack_ mod kind_ layout_'.split(' '));
  const OCAML_TOKEN = /\(\*|"|\{([a-z_]*)\||\[@{0,2}[A-Za-z_][\w'.]*|'(?:\\(?:[\\'"ntbr ]|[0-9]{3}|x[0-9a-fA-F]{2}|o[0-7]{3})|[^'\\\n])'|\b(?:0[xX][0-9a-fA-F_]+|[0-9][0-9_]*(?:\.[0-9_]*)?(?:[eE][+-]?[0-9_]+)?[lLnZsx]?)\b|[A-Za-z_][\w']*|->|===|:=|@@|&&|\|\||<=|>=|<>|[=+*/<>@|:;-]/g;
  const C_KEYWORDS = new Set('auto break case char const continue default do double else enum extern float for goto if inline int long register restrict return short signed sizeof static struct switch typedef union unsigned void volatile while bool true false NULL uintnat intnat value CAMLprim CAMLparam0 CAMLparam1 CAMLparam2 CAMLparam3 CAMLparam4 CAMLparam5 CAMLlocal1 CAMLlocal2 CAMLlocal3 CAMLlocal4 CAMLreturn CAMLreturnT CAMLexport CAMLextern'.split(' '));
  const C_TOKEN = /\/\*|\/\/[^\n]*|"|'(?:\\.|[^'\\\n])+'|^[ \t]*#[ \t]*[a-z]+|\b(?:0[xX][0-9a-fA-F]+|[0-9]+(?:\.[0-9]+)?)[uUlLfF]*\b|[A-Za-z_]\w*/gm;

  function tokens(src, lang, emit) {
    const ocaml = lang === 'ocaml';
    const re = ocaml ? OCAML_TOKEN : C_TOKEN;
    re.lastIndex = 0;
    let cursor = 0, m;
    while ((m = re.exec(src))) {
      if (m.index > cursor) emit(src.slice(cursor, m.index), null);
      const tok = m[0];
      let end = re.lastIndex, cls = null;
      if (ocaml) {
        if (tok === '(*') {
          let depth = 1, i = end;
          while (i < src.length && depth) {
            if (src.charCodeAt(i) === 40 && src.charCodeAt(i + 1) === 42) { depth++; i += 2; }
            else if (src.charCodeAt(i) === 42 && src.charCodeAt(i + 1) === 41) { depth--; i += 2; }
            else i++;
          }
          end = i; cls = 'comment';
        } else if (tok === '"') {
          end = stringEnd(src, end); cls = 'string';
        } else if (m[1] !== undefined) {
          const close = '|' + m[1] + '}';
          const j = src.indexOf(close, end);
          end = j < 0 ? src.length : j + close.length; cls = 'string';
        } else if (tok[0] === "'") cls = 'string';
        else if (tok[0] === '[' || OCAML_MODES.has(tok)) cls = 'mode';
        else if (OCAML_KEYWORDS.has(tok)) cls = 'keyword';
        else if (tok[0] >= '0' && tok[0] <= '9') cls = 'number';
        else if (tok[0] >= 'A' && tok[0] <= 'Z') cls = 'constructor';
        else if (!/[a-z_]/.test(tok[0])) cls = 'operator';
      } else {
        if (tok === '/*') {
          const j = src.indexOf('*/', end);
          end = j < 0 ? src.length : j + 2; cls = 'comment';
        } else if (tok.startsWith('//')) cls = 'comment';
        else if (tok === '"') { end = stringEnd(src, end); cls = 'string'; }
        else if (tok[0] === "'") cls = 'string';
        else if (tok.trim()[0] === '#') cls = 'pre';
        else if (C_KEYWORDS.has(tok)) cls = 'keyword';
        else if (tok[0] >= '0' && tok[0] <= '9') cls = 'number';
      }
      emit(src.slice(m.index, end), cls);
      cursor = end;
      re.lastIndex = end;
    }
    if (cursor < src.length) emit(src.slice(cursor), null);
  }

  function stringEnd(src, i) {
    while (i < src.length) {
      const c = src.charCodeAt(i);
      if (c === 92) i += 2;
      else if (c === 34) return i + 1;
      else i++;
    }
    return src.length;
  }

  // The source as one HTML string per line; a token spanning lines is
  // split so that every line is self-contained.
  function highlight(src, lang) {
    src = src.replace(/\r\n?/g, '\n');
    const out = [];
    let cur = '';
    const emit = (text, cls) => {
      const parts = text.split('\n');
      for (let i = 0; i < parts.length; i++) {
        if (i > 0) { out.push(cur); cur = ''; }
        if (parts[i]) cur += cls ? `<span class="tok-${cls}">${escText(parts[i])}</span>` : escText(parts[i]);
      }
    };
    if ((lang === 'ocaml' || lang === 'c') && src.length < 3e6) tokens(src, lang, emit);
    else emit(src, null);
    out.push(cur);
    if (src.endsWith('\n')) out.pop();
    return out;
  }

  // ------------------------------------------------------------ file view

  const LH = 18, CHUNK = 250;
  const cache = new Map();
  const code = $('code');

  function fetchSource(n) {
    if (cache.has(n.path)) return Promise.resolve(cache.get(n.path));
    return fetch(`src/${encodePath(n.path)}.txt?v=${version}`).then((r) => {
      if (!r.ok) throw new Error(`${r.status}`);
      return r.text();
    }).then((text) => {
      const lines = highlight(text, n.lang);
      const entry = { lines, marks: marks(n, lines.length) };
      cache.set(n.path, entry);
      if (cache.size > 12) cache.delete(cache.keys().next().value);
      return entry;
    });
  }

  function marks(n, count) {
    const mk = new Uint8Array(count + 2), del = new Set();
    if (n.status === 'n') mk.fill(3);
    for (const part of n.ranges ? n.ranges.split(',') : []) {
      const m = /^(\d+)([+~-])(\d+)(?:-(\d+))?$/.exec(part);
      if (!m) continue;
      const start = +m[1], k = m[2], len = +m[3];
      if (k === '-') { del.add(start); continue; }
      for (let i = start; i < start + len && i <= count; i++) mk[i] = k === '+' ? 1 : 2;
    }
    return { mk, del };
  }

  function noteLines(n) {
    const d = desc(n);
    const map = new Map();
    if (d && d.notes) d.notes.forEach((note, i) => {
      for (let l = note.lines[0]; l <= note.lines[1]; l++) if (!map.has(l)) map.set(l, i);
    });
    return map;
  }

  async function openFile(n, range) {
    const left = $('left');
    $('file').hidden = false;
    left.classList.add('file-open');
    document.body.classList.add('file-open');
    const same = S.file === n;
    S.file = n;
    S.range = range;
    fileHeader(n, range);
    if (!same) {
      code.innerHTML = '<div class="message">Loading…</div>';
      if (!phone.matches) { resize(); S.layout = layout(S.focus, S.W, S.H); render(); }
      if (n.lang === 'binary') {
        code.innerHTML = `<div class="message">A binary file or link; see it <a href="${esc(githubUrl(n))}">on GitHub</a>.</div>`;
        return;
      }
      let entry;
      try { entry = await fetchSource(n); } catch (e) {
        if (S.file === n) code.innerHTML = `<div class="message">Could not load the file (${esc(e.message)}).</div>`;
        return;
      }
      if (S.file !== n) return;
      buildCode(n, entry);
    }
    applyRange(range, !same || range);
  }

  function buildCode(n, entry) {
    const count = entry.lines.length;
    let banner = '';
    if (n.status === 'n') banner = `New in Vox: all ${fmt(count)} lines.`;
    else if (n.status === 'm') banner = `Modified by Vox: +${fmt(n.added)} −${fmt(n.removed)} lines against upstream <a href="${esc(S.tree.repository)}/commit/${S.tree.base}">${S.tree.base.slice(0, 10)}</a>. <span class="legend-inline"><i class="sw sw-a"></i>added <i class="sw sw-c"></i>changed <i class="sw sw-del"></i>removed</span>`;
    const chunks = [];
    for (let i = 0; i < count; i += CHUNK) chunks.push(`<div class="chunk" data-i="${i}" style="height:${Math.min(CHUNK, count - i) * LH}px"></div>`);
    code.innerHTML = (banner ? `<div class="banner">${banner}</div>` : '') + `<div class="lines">${chunks.join('')}</div>`;
    code.scrollTop = 0;
    code.scrollLeft = 0;
    S.notes = noteLines(n);
    S.entry = entry;
    renderChunks();
  }

  function lineHtml(n, entry, i) {
    const k = entry.marks.mk[i];
    const cls = ['ln'];
    if (entry.marks.del.has(i)) cls.push('del');
    if (i === 1 && entry.marks.del.has(0)) cls.push('del0');
    if (S.range && i >= S.range[0] && i <= S.range[1]) cls.push('hl');
    const note = S.notes.get(i);
    if (note !== undefined) cls.push('nt');
    let tag = '';
    if (note !== undefined && S.notes.get(i - 1) !== note) {
      const d = desc(n).notes[note];
      tag = `<span class="note-tag" data-note="${note}">${esc(d.title || 'Note')}</span>`;
    }
    const mk = k === 1 ? ' mk-a' : k === 2 ? ' mk-c' : k === 3 ? ' mk-n' : '';
    return `<div class="${cls.join(' ')}" id="L${i}"><span class="g"><a class="num" href="#p/${encodePath(n.path)}:L${i}" data-n="${i}">${i}</a><span class="mk${mk}"></span></span><span class="t">${entry.lines[i - 1] || ' '}${tag}</span></div>`;
  }

  function renderChunks() {
    if (!S.entry) return;
    const lines = code.querySelector('.lines');
    if (!lines) return;
    const top = code.scrollTop - lines.offsetTop - 600, bottom = code.scrollTop - lines.offsetTop + code.clientHeight + 600;
    for (const chunk of lines.children) {
      if (chunk.dataset.done) continue;
      const i = +chunk.dataset.i;
      const y0 = i * LH, y1 = y0 + chunk.offsetHeight;
      if (y1 < top || y0 > bottom) continue;
      let html = '';
      const end = Math.min(i + CHUNK, S.entry.lines.length);
      for (let l = i + 1; l <= end; l++) html += lineHtml(S.file, S.entry, l);
      chunk.innerHTML = html;
      chunk.dataset.done = '1';
    }
  }

  function applyRange(range, scroll) {
    for (const el of code.querySelectorAll('.ln.hl')) el.classList.remove('hl');
    if (range) {
      for (let l = range[0]; l <= range[1]; l++) {
        const el = document.getElementById(`L${l}`);
        if (el) el.classList.add('hl');
      }
    }
    if (scroll && range && S.entry) {
      const lines = code.querySelector('.lines');
      const len = range[1] - range[0] + 1;
      const room = Math.floor(code.clientHeight / LH);
      const before = len < room - 6 ? Math.min(4, Math.floor((room - len) / 3)) : 2;
      code.scrollTop = lines.offsetTop + (range[0] - 1 - before) * LH;
      renderChunks();
      for (let l = range[0]; l <= range[1]; l++) {
        const el = document.getElementById(`L${l}`);
        if (el) el.classList.add('hl');
      }
    }
    markActiveNote();
  }

  function githubUrl(n, range) {
    const kind = n.dir ? 'tree' : 'blob';
    const path = n.path === '/' ? '' : encodePath(n.path.replace(/\/$/, ''));
    return `${S.tree.repository}/${kind}/${S.tree.revision}/${path}${range ? `#L${range[0]}${range[1] !== range[0] ? `-L${range[1]}` : ''}` : ''}`;
  }

  function catalogueSource(n, range) {
    return `${S.tree.catalogue}sources/${encodePath(n.path)}.html${range ? `#L${range[0]}` : ''}`;
  }

  function fileHeader(n, range) {
    $('file-path').textContent = n.path;
    const badge = n.status === 'n' ? '<span class="badge badge-n">Vox-new</span>' : n.status === 'm' ? `<span class="badge badge-m">Vox-modified +${fmt(n.added)} −${fmt(n.removed)}</span>` : '<span class="badge">Upstream</span>';
    $('file-meta').innerHTML = `${badge} ${fmt(n.lines)} lines${range ? ` · lines ${range[0]}–${range[1]}` : ''}`;
    $('file-links').innerHTML = `<a href="${esc(githubUrl(n, range))}" target="_blank" rel="noopener">GitHub</a>`
      + (n.cat ? `<a href="${esc(catalogueSource(n, range))}" target="_blank" rel="noopener">Catalogue</a>` : '')
      + `<a href="src/${esc(encodePath(n.path))}.txt" target="_blank" rel="noopener">Raw</a>`;
  }

  function closeFile() {
    S.file = null;
    S.entry = null;
    S.range = null;
    $('file').hidden = true;
    $('left').classList.remove('file-open', 'file-max');
    document.body.classList.remove('file-open');
    code.innerHTML = '';
    if (!phone.matches) { resize(); S.layout = layout(S.focus, S.W, S.H); render(); }
  }

  // ---------------------------------------------------------------- panel

  const panel = $('panel');

  function block(title, b, vox) {
    if (!b) return '';
    return `<div class="${vox ? 'vox' : 'what'}">${title ? `<h3>${title}</h3>` : ''}<p class="short">${esc(b.short)}</p>`
      + (b.long ? `<details><summary>More</summary><div class="long">${catalogueUrl(b.long)}</div></details>` : '') + '</div>';
  }

  function demoCard() {
    if (S.filter !== 'demo' || !S.demo) return '';
    const d = S.demoById.get(S.demo);
    const tour = S.content.tours.find((t) => t.demo === d.id);
    return `<div class="demo-card"><p class="kind">Showing one demo <span class="status status-${esc(d.status)}">${esc(d.status.replace('-', ' '))}</span></p>`
      + `<h2>${esc(d.title)}</h2><p>${inlineMd(d.blurb)}</p>`
      + `<div class="links"><a href="${esc(S.tree.catalogue)}specs/${esc(d.id)}.html" target="_blank" rel="noopener">Catalogue page</a>`
      + (tour ? `<a href="#tour/${esc(tour.id)}">Take the tour</a>` : '') + `<a href="#" data-action="clear-demo">Show all files</a></div>`
      + `<h3>Its main files</h3><ul class="rows">${d.sources.map((s) => `<li class="stacked"><a href="#p/${encodePath(s.path)}">${esc(s.path.split('/').pop())}</a><span class="role">${inlineMd(s.role)}</span></li>`).join('')}</ul>`
      + `<p class="kind">On the map: these files in red, the rest of its ${fmt(d.own.length)} modules in black, and ${fmt(d.shared.length)} shared modules dashed.</p></div>`;
  }

  function inlineMd(s) {
    return esc(s || '').replace(/`([^`]+)`/g, '<code>$1</code>').replace(/\*\*([^*]+)\*\*/g, '<strong>$1</strong>');
  }

  function tourBanner() {
    if (!S.tour || S.panel === 'tour') return '';
    const t = S.content.tours.find((x) => x.id === S.tour);
    return `<p class="kind"><a href="#tour/${esc(t.id)}/${S.stop + 1}">← Back to the tour “${esc(t.title)}”, stop ${S.stop + 1}</a></p>`;
  }

  function panelNode(n) {
    S.panel = 'node';
    const d = desc(n) || {};
    let html = tourBanner() + demoCard();
    const title = n.path === '/' ? 'The repository' : n.path;
    html += `<h2>${esc(title)}</h2><p class="kind">${n.dir ? 'Directory' : 'File'} · ${esc(statusText(n))}</p>`;
    if (!n.dir) {
      const range = S.file === n ? S.range : null;
      html += `<div class="links">${S.file === n ? '' : `<a href="#p/${encodePath(n.path)}">Open the source</a>`}<a href="${esc(githubUrl(n, range))}" target="_blank" rel="noopener">GitHub at ${S.tree.revision.slice(0, 10)}</a>`
        + (n.cat ? `<a href="${esc(catalogueSource(n, range))}" target="_blank" rel="noopener">Catalogue</a>` : '') + '</div>';
    } else if (n.path !== '/') {
      html += `<div class="links"><a href="${esc(githubUrl(n))}" target="_blank" rel="noopener">GitHub at ${S.tree.revision.slice(0, 10)}</a></div>`;
    }
    if (n.path === '/' && !d.what) html += intro();
    html += block('What this is', d.what, false);
    if (d.vox) html += block('What Vox changed here', d.vox, true);
    else if (n.voxLines && n.dir) html += `<div class="vox none-vox"><h3>What Vox changed here</h3><p class="none">Not described yet. The numbers above and the list below come from the diff.</p></div>`;
    else if (n.status === 'm' || n.status === 'n') html += `<div class="vox none-vox"><h3>What Vox changed here</h3><p class="none">Not described yet.</p></div>`;
    if (!d.what && n.path !== '/') html += `<p class="none">No description of this ${n.dir ? 'directory' : 'file'} yet.</p>`;
    if (n.path === '/' && d.what) html += intro();
    if (d.notes && d.notes.length) {
      html += `<h3>Notes on the code</h3>` + d.notes.map((note, i) => `<div class="note" data-note="${i}"><div class="note-head">`
        + `<strong>${esc(note.title || 'Note')}</strong> · lines ${note.lines[0]}–${note.lines[1]}${note.kind === 'what' ? '' : ' · Vox'}</div>${catalogueUrl(note.html)}</div>`).join('');
    }
    if (!n.dir && n.demos) {
      html += `<h3>Demos</h3><ul class="rows">` + n.demos.map((m) => {
        const demo = S.demoById.get(m.id);
        return `<li class="stacked"><a href="#demo/${esc(m.id)}">${esc(demo.title)}</a><span class="role">${inlineMd(m.role)} · <a href="${esc(S.tree.catalogue)}specs/${esc(m.id)}.html" target="_blank" rel="noopener">page</a></span></li>`;
      }).join('') + '</ul>';
    }
    const stops = [];
    for (const t of S.content.tours) t.stops.forEach((s, i) => {
      if (n.dir ? s.path.startsWith(n.path === '/' ? '' : n.path) : s.path === n.path) stops.push([t, s, i]);
    });
    if (stops.length && n.path !== '/') {
      html += `<h3>Tour stops here</h3><ul class="rows">` + stops.slice(0, 10).map(([t, s, i]) => `<li class="stacked"><a href="#tour/${esc(t.id)}/${i + 1}">${esc(s.title)}</a><span class="role">${esc(t.title)}, stop ${i + 1}</span></li>`).join('') + '</ul>';
    }
    if (n.dir) {
      if (n.demoCounts.size && n.path !== '/') {
        const list = [...n.demoCounts].sort((a, b) => b[1] - a[1]);
        html += `<h3>Demos with files here</h3><ul class="rows">` + list.slice(0, 12).map(([id, c]) => `<li><a href="#demo/${esc(id)}">${esc(S.demoById.get(id).title)}</a><span class="n">${c} file${c > 1 ? 's' : ''}</span></li>`).join('') + (list.length > 12 ? `<li><span class="name n">and ${list.length - 12} more</span></li>` : '') + '</ul>';
      }
      if (n.voxLines) {
        const files = [];
        (function walk(x) { if (x.dir) x.children.forEach(walk); else if (x.voxLines) files.push(x); })(n);
        files.sort((a, b) => b.voxLines - a.voxLines);
        html += `<h3>Vox's largest changes here</h3><ul class="rows">` + files.slice(0, 12).map((f) => `<li><a href="#p/${encodePath(f.path)}" title="${esc(f.path)}">${esc(f.path.slice(n.path === '/' ? 0 : n.path.length))}</a>`
          + (f.status === 'n' ? `<span class="n plus">new, ${fmt(f.lines)}</span>` : `<span class="n"><span class="plus">+${fmt(f.added)}</span> <span class="minus">−${fmt(f.removed)}</span></span>`) + '</li>').join('')
          + (files.length > 12 ? `<li><span class="name n">${fmt(files.length - 12)} more files changed</span></li>` : '') + '</ul>';
      }
      const kids = n.children.filter((c) => c.v > 0);
      if (kids.length) {
        html += `<h3>Contents${S.filter !== 'all' || S.size !== 'lines' ? ' (as filtered)' : ''}</h3><ul class="rows">` + kids.slice(0, 40).map((c) => {
          const cd = desc(c);
          return `<li><a href="#p/${encodePath(c.path)}" title="${esc(cd && cd.what ? cd.what.short : c.path)}">${esc(c.name)}${c.dir ? '/' : ''}</a><span class="n">${kfmt(c.lines)}${c.voxLines ? ` · <span class="plus">Vox ${kfmt(c.voxLines)}</span>` : ''}</span></li>`;
        }).join('') + (kids.length > 40 ? `<li><span class="name n">and ${kids.length - 40} more</span></li>` : '') + '</ul>';
      }
    }
    panel.innerHTML = html;
    panel.scrollTop = 0;
    markActiveNote();
    setToursButton(false);
  }

  function intro() {
    const vox = S.content.tours.filter((t) => t.kind === 'vox');
    const r = S.root;
    return `<div class="intro"><p>The whole repository at commit <a href="${esc(S.tree.repository)}/commit/${S.tree.revision}">${S.tree.revision.slice(0, 10)}</a>, one rectangle per file, sized by lines. `
      + `Grey is upstream OxCaml; amber is a file Vox modified, darker the larger the share of its lines changed; blue is a file Vox added. `
      + `Vox's changes are measured against the upstream merge base <a href="${esc(S.tree.repository)}/commit/${S.tree.base}">${S.tree.base.slice(0, 10)}</a>.</p>`
      + `<p>In view: ${fmt(S.root.v)} ${S.size === 'vox' ? 'Vox lines' : 'lines'}. In the repository: ${fmt(r.files)} files, ${fmt(r.lines)} lines; Vox added ${fmt(r.newFiles)} files (${fmt(r.newLines)} lines) and modified ${fmt(r.modFiles)} (+${fmt(r.modAdded)} −${fmt(r.modRemoved)}).</p>`
      + (vox.length ? `<p>Start with the tour ${vox.map((t) => `<a href="#tour/${esc(t.id)}">${esc(t.title)}</a>`).join(', ')}, or see <a href="#tours">all tours</a>.</p>` : '')
      + `<p class="kind">Not shown: ${Object.keys(S.tree.excluded).map((p) => `<code>${esc(p)}</code>`).join(', ')}.</p></div>`;
  }

  function markActiveNote() {
    for (const el of panel.querySelectorAll('.note')) {
      const d = S.file && desc(S.file);
      const note = d && d.notes ? d.notes[+el.dataset.note] : null;
      el.classList.toggle('active', !!(note && S.range && S.range[0] <= note.lines[1] && S.range[1] >= note.lines[0]));
    }
  }

  function setToursButton(on) {
    $('tours-button').setAttribute('aria-current', on ? 'true' : 'false');
  }

  function panelTours() {
    S.panel = 'tours';
    const vox = S.content.tours.filter((t) => t.kind === 'vox'), demo = S.content.tours.filter((t) => t.kind === 'demo');
    const item = (t) => `<li><a class="title" href="#tour/${esc(t.id)}">${esc(t.title)}</a><span class="n">${t.stops.length} stops</span>${t.summary ? `<div class="summary">${catalogueUrl(t.summary)}</div>` : ''}</li>`;
    const without = S.demos.filter((d) => !demo.some((t) => t.demo === d.id));
    panel.innerHTML = `<h2>Tours</h2><p class="kind">Each stop zooms the map to a place in the source, opens the file at the lines that matter and explains them. Use ← and → to move between stops.</p>`
      + (vox.length ? `<h3>Vox's source</h3><ul class="tour-list">${vox.map(item).join('')}</ul>` : '')
      + (demo.length ? `<h3>Demos</h3><ul class="tour-list">${demo.map(item).join('')}</ul>` : '')
      + (without.length ? `<p class="kind">No tour yet for ${without.length} of the ${S.demos.length} demos; the <a href="#demo/${esc(without[0].id)}">demo filter</a> still shows where each lives.</p>` : '');
    panel.scrollTop = 0;
    setToursButton(true);
  }

  function panelTour(t, i) {
    S.panel = 'tour';
    const stop = t.stops[i];
    const demo = t.demo && S.demoById.get(t.demo);
    const where = stop.lines ? `${stop.path}:${stop.lines[0]}–${stop.lines[1]}` : stop.path;
    panel.innerHTML = `<div class="tour-head"><span>Tour · ${esc(t.title)}</span><a href="#tours">All tours</a></div>`
      + `<div class="tour-nav"><button type="button" class="text" data-action="prev" ${i === 0 ? 'disabled' : ''}>← Previous</button>`
      + `<span class="count">Stop ${i + 1} of ${t.stops.length}</span>`
      + `<button type="button" class="text" data-action="next" ${i === t.stops.length - 1 ? 'disabled' : ''}>Next →</button></div>`
      + `<h2>${esc(stop.title)}</h2><p class="tour-where"><a href="#p/${encodePath(stop.path)}${stop.lines ? `:L${stop.lines[0]}-L${stop.lines[1]}` : ''}">${esc(where)}</a></p>`
      + `<div class="tour-text">${catalogueUrl(stop.html)}</div>`
      + (demo ? `<p class="kind"><a href="${esc(S.tree.catalogue)}specs/${esc(demo.id)}.html" target="_blank" rel="noopener">The demo's catalogue page</a> · <a href="#demo/${esc(demo.id)}">Show only its files</a></p>` : '')
      + `<h3>Stops</h3><ol class="stops">${t.stops.map((s, k) => `<li class="${k === i ? 'current' : ''}"><a href="#tour/${esc(t.id)}/${k + 1}">${esc(s.title)}</a></li>`).join('')}</ol>`
      + `<p class="kbd">← → move between stops · Esc leaves the tour</p>`;
    panel.scrollTop = 0;
    setToursButton(true);
  }

  function panelTourIntro(t) {
    S.panel = 'tour-intro';
    const demo = t.demo && S.demoById.get(t.demo);
    panel.innerHTML = `<div class="tour-head"><span>Tour</span><a href="#tours">All tours</a></div><h2>${esc(t.title)}</h2>`
      + (t.summary ? `<div class="tour-text">${catalogueUrl(t.summary)}</div>` : '')
      + (demo ? `<p class="kind"><a href="${esc(S.tree.catalogue)}specs/${esc(demo.id)}.html" target="_blank" rel="noopener">The demo's catalogue page</a></p>` : '')
      + `<div class="tour-nav"><a class="button" href="#tour/${esc(t.id)}/1">Start the tour →</a><span class="count">${t.stops.length} stops</span></div>`
      + `<h3>Stops</h3><ol class="stops">${t.stops.map((s, k) => `<li><a href="#tour/${esc(t.id)}/${k + 1}">${esc(s.title)}</a></li>`).join('')}</ol>`;
    panel.scrollTop = 0;
    setToursButton(true);
  }

  // ----------------------------------------------------------- navigation

  function crumbs() {
    const chain = [];
    for (let n = S.focus; n; n = n.parent) chain.unshift(n);
    const f = S.focus;
    const shown = `${fmt(f.vf)} files · ${kfmt(f.v)} ${S.size === 'vox' ? 'Vox lines' : 'lines'} shown`
      + (f.vf !== f.files ? ` <span title="including files outside the scope and filter">(of ${fmt(f.files)} files, ${kfmt(f.lines)} lines)</span>` : ` · Vox ${kfmt(f.voxLines)}`);
    $('crumbs').innerHTML = chain.map((n, i) => (i === chain.length - 1
      ? `<span class="here">${esc(n.path === '/' ? 'repository' : n.name)}</span>`
      : `<a href="#p/${encodePath(n.path)}">${esc(n.path === '/' ? 'repository' : n.name)}</a><span class="sep">/</span>`)).join('')
      + `<span class="crumb-stats">${shown}</span>`;
  }

  function focusFor(n) {
    // A file is shown in its directory, unless it is already large enough
    // to see where it is.
    if (n.dir) return n;
    const r = S.layout && S.layout.get(n);
    if (isAncestor(S.focus, n) && r && r.w >= 30 && r.h >= 14 && S.focus !== S.root) return S.focus;
    return n.parent;
  }

  function parseHash() {
    const h = location.hash.slice(1);
    if (!h) return { kind: 'path', path: '/' };
    if (h === 'tours') return { kind: 'tours' };
    let m = /^tour\/([^/]+)(?:\/(\d+))?$/.exec(h);
    if (m) return { kind: 'tour', id: decodeURIComponent(m[1]), stop: m[2] ? +m[2] : 0 };
    m = /^demo\/(.+)$/.exec(h);
    if (m) return { kind: 'demo', id: decodeURIComponent(m[1]) };
    m = /^p\/(.*?)(?::L(\d+)(?:-L?(\d+))?)?$/.exec(h);
    if (m) {
      const path = m[1].split('/').map(decodeURIComponent).join('/') || '/';
      return { kind: 'path', path, range: m[2] ? [+m[2], +(m[3] || m[2])] : null };
    }
    return { kind: 'path', path: '/' };
  }

  async function route() {
    const r = parseHash();
    $('search-results').hidden = true;
    if (r.kind === 'tours') { panelTours(); return; }
    if (r.kind === 'demo') {
      if (!S.demoById.has(r.id)) return;
      setFilter('demo', r.id, false);
      const d = S.demoById.get(r.id);
      let lca = null;
      for (const s of d.sources) {
        const f = S.byPath.get(s.path);
        if (!f) continue;
        if (!lca) lca = f.parent;
        while (!isAncestor(lca, f)) lca = lca.parent;
      }
      S.selected = null;
      closeFile();
      crumbsAfter(view(lca || S.root));
      panelNode(lca || S.root);
      return;
    }
    if (r.kind === 'tour') {
      const t = S.content.tours.find((x) => x.id === r.id);
      if (!t) { panelTours(); return; }
      S.tour = t.id;
      if (!r.stop) { panelTourIntro(t); return; }
      const i = Math.min(Math.max(r.stop, 1), t.stops.length) - 1;
      S.stop = i;
      const stop = t.stops[i];
      const n = S.byPath.get(stop.path);
      if (!n) return;
      ensureVisible(n);
      const focus = stop.focus ? S.byPath.get(stop.focus) : n.dir ? n : n.parent;
      S.selected = n.dir ? null : n;
      panelTour(t, i);
      if (n.dir) closeFile();
      else openFile(n, stop.lines);
      crumbsAfter(view(focus || n.parent));
      return;
    }
    const n = S.byPath.get(r.path) || S.byPath.get(r.path + '/') || S.root;
    ensureVisible(n);
    if (n.dir) {
      S.selected = null;
      if (S.file) closeFile();
      panelNode(n);
      crumbsAfter(view(n));
    } else {
      S.selected = n;
      const focus = focusFor(n);
      await openFile(n, r.range);
      panelNode(n);
      crumbsAfter(view(focus));
    }
  }

  function crumbsAfter(promise) {
    crumbs();
    return Promise.resolve(promise).then(() => { crumbs(); drawOverlay(); });
  }

  // A deep link to a file hidden by the current scope or filter widens
  // them, so that the map can show where it is.
  function ensureVisible(n) {
    if (n.v > 0 || n === S.root) return;
    if (!inScope(n.path)) {
      S.scope = 'all';
      $('scope').value = 'all';
    }
    computeValues();
    if (n.v > 0) { S.layout = layout(S.focus, S.W, S.H); return; }
    S.filter = 'all';
    S.size = 'lines';
    $('filter').value = 'all';
    $('size').value = 'lines';
    $('demo-label').hidden = true;
    computeValues();
    S.layout = layout(S.focus, S.W, S.H);
  }

  function go(hash) {
    if (location.hash === hash) route();
    else location.hash = hash;
  }

  function setFilter(filter, demo, refresh = true) {
    S.filter = filter;
    S.demo = filter === 'demo' ? (demo || S.demo || S.demos[0].id) : null;
    $('filter').value = filter;
    $('demo-label').hidden = filter !== 'demo';
    if (S.demo) $('demo').value = S.demo;
    computeValues();
    if (refresh) refreshView();
  }

  function refreshView() {
    let f = S.focus;
    while (f !== S.root && f.v <= 0) f = f.parent;
    view(f).then(() => { crumbs(); drawOverlay(); });
    crumbs();
    if (S.panel === 'node') panelNode(S.file || f);
  }

  // --------------------------------------------------------------- search

  let results = [], active = 0;
  function search(q) {
    const box = $('search-results');
    const terms = q.toLowerCase().split(/\s+/).filter(Boolean);
    if (!terms.length) { box.hidden = true; return; }
    const found = [];
    for (const n of S.all) {
      if (n === S.root) continue;
      const p = n.path.toLowerCase();
      if (!terms.every((t) => p.includes(t))) continue;
      const name = n.name.toLowerCase(), last = terms[terms.length - 1];
      const score = name === last ? 0 : name.startsWith(last) ? 1 : name.includes(last) ? 2 : 3;
      found.push([score, n.dir ? 0 : 1, -n.lines, n]);
    }
    found.sort((a, b) => a[0] - b[0] || a[1] - b[1] || a[2] - b[2]);
    results = found.slice(0, 14).map((x) => x[3]);
    active = 0;
    box.innerHTML = results.length ? results.map((n, i) => {
      const dir = n.parent && n.parent.path !== '/' ? n.parent.path : '';
      return `<li role="option" data-i="${i}" aria-selected="${i === 0}"><span class="dir">${esc(dir)}</span><b>${esc(n.name)}${n.dir ? '/' : ''}</b> <span class="dir">${kfmt(n.lines)}</span></li>`;
    }).join('') : '<li class="dir">No match</li>';
    box.hidden = false;
  }

  function pick(i) {
    const n = results[i];
    if (!n) return;
    $('search').value = '';
    $('search-results').hidden = true;
    $('search').blur();
    go(`#p/${encodePath(n.path)}`);
  }

  // --------------------------------------------------------------- events

  function bind() {
    const scope = $('scope');
    scope.innerHTML = S.tree.scopes.map((s) => `<option value="${esc(s.id)}">${esc(s.label)}</option>`).join('');
    scope.value = S.scope;
    scope.addEventListener('change', () => { S.scope = scope.value; computeValues(); refreshView(); });
    $('demo').innerHTML = S.demos.map((d) => `<option value="${esc(d.id)}">${esc(d.title)}</option>`).join('');
    $('filter').addEventListener('change', (e) => {
      if (e.target.value === 'demo') go(`#demo/${$('demo').value || S.demos[0].id}`);
      else setFilter(e.target.value);
    });
    $('demo').addEventListener('change', (e) => go(`#demo/${e.target.value}`));
    $('size').addEventListener('change', (e) => { S.size = e.target.value; computeValues(); refreshView(); });
    $('outline').addEventListener('change', (e) => { S.outline = e.target.checked; render(); });
    $('revision').innerHTML = `at <a href="${esc(S.tree.repository)}/commit/${S.tree.revision}" title="${S.tree.revision}">${S.tree.revision.slice(0, 10)}</a>`;

    canvas.addEventListener('mousemove', (e) => {
      if (S.anim) return;
      const b = canvas.getBoundingClientRect();
      const n = hit(e.clientX - b.left, e.clientY - b.top);
      if (n !== S.hover) { S.hover = n; drawOverlay(); }
      showTip(n, e.clientX - b.left, e.clientY - b.top);
    });
    canvas.addEventListener('mouseleave', () => { S.hover = null; drawOverlay(); $('tip').hidden = true; });
    canvas.addEventListener('click', (e) => {
      if (S.anim) return;
      const b = canvas.getBoundingClientRect();
      const x = e.clientX - b.left, y = e.clientY - b.top;
      const target = hit(x, y);
      $('tip').hidden = true;
      if (target === S.focus) return;
      const r = S.layout.get(target);
      if (target.dir && r && inner(r, false).head && y < r.y + HEAD) return go(`#p/${encodePath(target.path)}`);
      if (!target.dir && r && r.w > 40 && r.h > 13) return go(`#p/${encodePath(target.path)}`);
      let child = target;
      while (child.parent !== S.focus) child = child.parent;
      go(`#p/${encodePath(child.path)}`);
    });
    canvas.addEventListener('contextmenu', (e) => { e.preventDefault(); zoomOut(); });
    $('tree-list').addEventListener('click', (e) => {
      const li = e.target.closest('li[data-path]');
      if (li) go(`#p/${encodePath(li.dataset.path)}`);
    });

    code.addEventListener('scroll', () => requestAnimationFrame(renderChunks), { passive: true });
    code.addEventListener('click', (e) => {
      const num = e.target.closest('.num');
      if (num) {
        e.preventDefault();
        const l = +num.dataset.n;
        const range = e.shiftKey && S.range ? [Math.min(S.range[0], l), Math.max(S.range[1], l)] : [l, l];
        history.replaceState(null, '', `#p/${encodePath(S.file.path)}:L${range[0]}${range[1] !== range[0] ? `-L${range[1]}` : ''}`);
        S.range = range;
        applyRange(range, false);
        fileHeader(S.file, range);
        if (S.panel === 'node') panelNode(S.file);
        return;
      }
      const tag = e.target.closest('.note-tag');
      if (tag) selectNote(+tag.dataset.note);
    });
    $('file-close').addEventListener('click', () => {
      if (S.panel === 'tour') S.tour = null;
      go(`#p/${encodePath(S.focus.path)}`);
    });
    $('file-max').addEventListener('click', () => {
      $('left').classList.toggle('file-max');
      if (!$('left').classList.contains('file-max')) { resize(); S.layout = layout(S.focus, S.W, S.H); render(); }
      renderChunks();
    });

    panel.addEventListener('click', (e) => {
      const a = e.target.closest('[data-action]');
      if (a) {
        e.preventDefault();
        const act = a.dataset.action;
        if (act === 'next' || act === 'prev') step(act === 'next' ? 1 : -1);
        if (act === 'clear-demo') { setFilter('all'); history.replaceState(null, '', `#p/${encodePath(S.focus.path)}`); }
        return;
      }
      const note = e.target.closest('.note');
      if (note && !e.target.closest('a')) selectNote(+note.dataset.note);
    });

    const input = $('search');
    input.addEventListener('input', () => search(input.value));
    input.addEventListener('keydown', (e) => {
      const box = $('search-results');
      if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
        e.preventDefault();
        active = Math.max(0, Math.min(results.length - 1, active + (e.key === 'ArrowDown' ? 1 : -1)));
        for (const li of box.children) li.setAttribute('aria-selected', String(+li.dataset.i === active));
      } else if (e.key === 'Enter') pick(active);
      else if (e.key === 'Escape') { box.hidden = true; input.blur(); }
    });
    input.addEventListener('blur', () => setTimeout(() => { $('search-results').hidden = true; }, 150));
    $('search-results').addEventListener('mousedown', (e) => {
      const li = e.target.closest('li[data-i]');
      if (li) { e.preventDefault(); pick(+li.dataset.i); }
    });

    document.addEventListener('keydown', (e) => {
      if (e.target.closest && e.target.closest('input,select,textarea')) return;
      if (e.metaKey || e.ctrlKey || e.altKey) return;
      if ((e.key === 'ArrowRight' || e.key === 'ArrowLeft') && S.tour && (S.panel === 'tour' || S.panel === 'tour-intro')) {
        e.preventDefault();
        step(e.key === 'ArrowRight' ? 1 : -1);
      } else if (e.key === 'Escape') {
        if (S.panel === 'tour' || S.panel === 'tour-intro') { S.tour = null; go(`#p/${encodePath(S.focus.path)}`); }
        else if (S.file) go(`#p/${encodePath(S.focus.path)}`);
        else zoomOut();
      } else if (e.key === 'Backspace') zoomOut();
      else if (e.key === '/') { e.preventDefault(); input.focus(); }
    });

    window.addEventListener('hashchange', route);
    new ResizeObserver(() => {
      if (phone.matches) return;
      resize();
      if (S.focus) { S.layout = layout(S.focus, S.W, S.H); render(); }
    }).observe($('map'));
    phone.addEventListener('change', () => {
      $('tree-list').hidden = !phone.matches;
      resize();
      S.layout = layout(S.focus, S.W, S.H);
      render();
    });
  }

  function selectNote(i) {
    const note = desc(S.file).notes[i];
    history.replaceState(null, '', `#p/${encodePath(S.file.path)}:L${note.lines[0]}-L${note.lines[1]}`);
    S.range = note.lines.slice();
    applyRange(S.range, true);
    fileHeader(S.file, S.range);
    markActiveNote();
  }

  function step(delta) {
    const t = S.content.tours.find((x) => x.id === S.tour);
    if (!t) return;
    const next = S.panel === 'tour-intro' ? (delta > 0 ? 0 : -1) : S.stop + delta;
    if (next < 0 || next >= t.stops.length) return;
    go(`#tour/${t.id}/${next + 1}`);
  }

  function zoomOut() {
    if (S.focus.parent) go(`#p/${encodePath(S.focus.parent.path)}`);
  }

  // ----------------------------------------------------------------- main

  load().then(() => {
    const started = performance.now();
    bind();
    computeValues();
    $('tree-list').hidden = !phone.matches;
    resize();
    S.layout = layout(S.root, S.W, S.H);
    render();
    route().then(() => {
      window.__explorer = { ready: true, readyAt: performance.now(), layoutMs: performance.now() - started, S, render, layout };
    });
  }).catch((e) => {
    panel.innerHTML = `<h2>Could not load the explorer</h2><p>${esc(e.message)}</p>`;
    throw e;
  });
})();
