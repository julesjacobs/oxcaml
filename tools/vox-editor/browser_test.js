// Layer 6: in-browser smoke test.
//
// Spawns the Python server, drives the real page in headless Chrome via
// puppeteer-core, and asserts the proof pane behaves: the sample program
// verifies, a VC shows its goal/hypotheses, and a live Lean goal can be
// fetched from inside a [%%vox.lean] block.
//
// Run: node browser_test.js
// Requires: puppeteer-core (installed under /tmp/vox-pptr), /usr/bin/
// google-chrome, a built ocamlc (VOX_OCAMLC or the sibling _build), and
// the pinned Lean (VOX_LEAN or its nix path). Exits non-zero on failure.

const assert = require("assert");
const fs = require("fs");
const os = require("os");
const { spawn } = require("child_process");
const path = require("path");
const { pathToFileURL } = require("url");

// puppeteer-core (>= 23) is ESM-only; load it with a dynamic import.
async function loadPuppeteer() {
  const entry = require.resolve("puppeteer-core", {
    paths: ["/tmp/vox-pptr/node_modules"],
  });
  return (await import(pathToFileURL(entry).href)).default;
}

const HERE = __dirname;
// The real binary; /usr/bin/google-chrome is a bwrap+profile wrapper that
// fights puppeteer for the shared profile.
const CHROME = fs.existsSync("/opt/google/chrome/chrome")
  ? "/opt/google/chrome/chrome"
  : "/usr/bin/google-chrome";
const PORT = 8731;

function waitFor(predicate, timeoutMs, desc) {
  return new Promise((resolve, reject) => {
    const start = Date.now();
    const tick = () => {
      Promise.resolve(predicate()).then((v) => {
        if (v) return resolve(v);
        if (Date.now() - start > timeoutMs) return reject(new Error("timeout: " + desc));
        setTimeout(tick, 100);
      }, reject);
    };
    tick();
  });
}

async function main() {
  const server = spawn("python3", ["-u", path.join(HERE, "server.py"), "--port", String(PORT)], {
    cwd: HERE,
    env: Object.assign({}, process.env, { NO_PROXY: "127.0.0.1,localhost" }),
    stdio: ["ignore", "pipe", "pipe"],
  });
  let serverOut = "";
  server.stdout.on("data", (d) => (serverOut += d));
  server.stderr.on("data", (d) => (serverOut += d));

  let browser;
  try {
    await waitFor(() => serverOut.includes("vox-editor on"), 15000, "server start");

    const puppeteer = await loadPuppeteer();
    const profile = fs.mkdtempSync(path.join(os.tmpdir(), "voxchrome"));
    browser = await puppeteer.launch({
      executablePath: CHROME,
      headless: true,
      userDataDir: profile,
      args: [
        "--no-sandbox",
        "--no-proxy-server",
        "--disable-gpu",
        "--disable-dev-shm-usage",
        "--disable-extensions",
        "--disable-component-extensions-with-background-pages",
        "--no-first-run",
        "--no-default-browser-check",
        "--homepage=about:blank",
      ],
    });
    const pages = await browser.pages();
    const page = pages.length ? pages[0] : await browser.newPage();
    page.on("pageerror", (e) => console.log("PAGEERROR:", e.message));
    page.on("console", (m) => {
      if (m.type() === "error") console.log("CONSOLE.error:", m.text());
    });
    await page.goto("http://127.0.0.1:" + PORT + "/", {
      waitUntil: "domcontentloaded",
      timeout: 20000,
    });
    await page.waitForSelector(".CodeMirror", { timeout: 10000 });

    // The default example (the len/append/nth walkthrough) auto-checks
    // on load; wait for the verdict.
    await waitFor(
      () => page.$eval("#status", (e) => e.textContent),
      30000,
      "status present"
    );
    const status = await waitFor(
      async () => {
        const t = await page.$eval("#status", (e) => e.textContent);
        return /verified|errors/.test(t) ? t : false;
      },
      60000,
      "check to finish"
    );
    console.log("ok - default example status:", status.trim());
    assert.ok(status.includes("verified"), "default should verify: " + status);

    const buf = await page.evaluate(() => window.__vox.cm.getValue());
    assert.ok(
      /let rec nth/.test(buf) && /append/.test(buf),
      "default is the len/append/nth walkthrough"
    );
    assert.strictEqual(
      await page.$eval("#examples", (e) => e.value),
      "nth",
      "dropdown reflects the default example"
    );
    console.log("ok - default is the walkthrough, selected in the dropdown");

    // On load the editor opens on the example's suggested teaching line
    // (nth: line 21, the impossible Nil arm) and the pane shows that VC --
    // its obligation is the impossible `false`, provable because the bound
    // makes the arm dead.
    await waitFor(
      () => page.evaluate(() => window.__vox.cm.getCursor().line === 20),
      5000,
      "cursor on the suggested line (21)"
    );
    const paneOnLoad = await waitFor(
      async () => {
        const t = await page.$eval("#pane-body", (e) => e.textContent);
        return /goal/.test(t) ? t : false;
      },
      5000,
      "suggested VC pane on load"
    );
    assert.ok(
      /false/.test(paneOnLoad),
      "the Nil-arm obligation is false: " + paneOnLoad.slice(0, 100)
    );
    console.log("ok - opens with cursor on the suggested line, pane on that VC");

    // Cursor on the first VC region: the pane shows its goal.
    const vcPos = await page.evaluate(() => {
      const r = window.__vox.getRegions().find((x) => x.kind === "vc");
      return r ? { line: r.start.line, col: r.start.col } : null;
    });
    assert.ok(vcPos, "the walkthrough has a VC region");
    await page.evaluate((p) => window.__vox.cm.setCursor({ line: p.line, ch: p.col }), vcPos);
    await waitFor(
      async () => {
        const t = await page.$eval("#pane-body", (e) => e.textContent);
        return /goal/.test(t) ? t : false;
      },
      5000,
      "vc pane"
    );
    console.log("ok - VC pane shows a goal");

    // COMPACT is the default: goal first, hypotheses after, nothing
    // else -- no context rows, no turnstile, no cursor-type line.
    const compactView = await page.evaluate(() => {
      const body = document.getElementById("pane-body");
      const rows = Array.from(body.children).map((e) => e.tagName + ":" + e.className);
      return {
        checked: document.getElementById("compact-box").checked,
        hasCtx: !!body.querySelector(".ctx"),
        hasTurnstile: !!body.querySelector(".turnstile"),
        hasCursorType: !!body.querySelector(".cursor-type"),
        goalBeforeHyp:
          rows.findIndex((r) => /goal/.test(r)) <
          rows.findIndex((r) => /hyp/.test(r)),
      };
    });
    assert.ok(compactView.checked, "compact checkbox is checked by default");
    assert.ok(!compactView.hasCtx, "compact: no context rows");
    assert.ok(!compactView.hasTurnstile, "compact: no turnstile");
    assert.ok(!compactView.hasCursorType, "compact: no cursor-type line");
    assert.ok(compactView.goalBeforeHyp, "compact: goal renders before hypotheses");
    console.log("ok - compact default: goal then hypotheses, nothing else");

    // Pane colorization: goal / hypothesis rows are tokenized with the SAME
    // vox mode as the buffer -- they carry cm-* token spans, and the
    // refinement-interior italic class -- while their textContent stays
    // byte-for-byte the predicate (so provenance keys + layout are intact).
    const expGoalText = await page.evaluate(() => {
      const r = window.__vox.getRegions().find((x) => x.kind === "vc");
      return window.Selection.splitSpanSuffix(r.goal).text;
    });
    const paneColor = await page.evaluate(() => {
      const el = document.querySelector("#pane-body .goal");
      if (!el) return null;
      return {
        tokens: el.querySelectorAll('span[class*="cm-"]').length,
        text: el.textContent,
      };
    });
    assert.ok(paneColor, "a goal row is present in the pane");
    assert.ok(
      paneColor.tokens > 0,
      "goal row is tokenized (has cm-* spans): " + JSON.stringify(paneColor)
    );
    assert.strictEqual(
      paneColor.text,
      expGoalText,
      "tokenized goal row textContent is byte-for-byte the predicate"
    );
    const paneItalic = await page.evaluate(() => {
      const el = document.querySelector("#pane-body .cm-vox-refine-body");
      return el ? getComputedStyle(el).fontStyle : null;
    });
    assert.strictEqual(
      paneItalic,
      "italic",
      "refinement-interior text renders italic in the pane: " + paneItalic
    );
    console.log(
      "ok - pane rows tokenized (" + paneColor.tokens +
        " spans), italic, textContent preserved"
    );

    // Uncheck compact for the FULL proof-state assertions below.
    await page.evaluate(() => window.__vox.setCompact(false));

    // Proof-state extras: the pane shows the VC's variables with their
    // OxCaml type and Lean sort (context rows), the goal behind a
    // turnstile, and the type of the expression under the cursor.
    const state = await page.evaluate(() => {
      const ctx = Array.from(document.querySelectorAll("#pane-body .ctx")).map(
        (e) => e.textContent
      );
      return {
        ctx,
        turnstile: !!document.querySelector("#pane-body .turnstile"),
        cursorType: (document.querySelector("#pane-body .cursor-type") || {})
          .textContent || null,
      };
    });
    assert.ok(state.ctx.length > 0, "context rows present");
    assert.ok(
      state.ctx.some((t) => /: *(int|ilist)/.test(t)),
      "a context row shows an OxCaml type: " + JSON.stringify(state.ctx)
    );
    // The "~" prefix is CSS ::before content, invisible to textContent.
    // Sorts render as READABLE labels (Int, ilist, opaque) -- never the
    // raw solver spelling.
    const sortLabels = await page.evaluate(() =>
      Array.from(document.querySelectorAll("#pane-body .ctx-lean")).map(
        (e) => e.textContent
      )
    );
    // Labels render ONLY when they add information beyond the OxCaml
    // type (e.g. "opaque"); duplicates and compound solver spellings
    // are suppressed -- so zero labels is legitimate. What must never
    // appear is a raw solver spelling.
    assert.ok(
      sortLabels.every((t) => t && !/^Vox/.test(t) && !/\s/.test(t)),
      "any rendered sort label is readable: " + JSON.stringify(sortLabels)
    );
    assert.ok(state.turnstile, "goal renders behind a turnstile");
    // -annot covers EXPRESSIONS, so the type-at-cursor line needs the
    // cursor on one (the VC start above sits on a type annotation).
    const cursorType = await page.evaluate(() => {
      const cm = window.__vox.cm;
      const idx = cm.getValue().indexOf("nth t (i - 1)");
      cm.setCursor(cm.posFromIndex(idx + 1)); // inside `nth`
      window.__vox.renderPane();
      const el = document.querySelector("#pane-body .cursor-type");
      return el
        ? el.textContent
        : "null (types=" + window.__vox.getTypes().length + ", pos=" +
          JSON.stringify(cm.getCursor()) + ")";
    });
    assert.ok(
      cursorType && /:/.test(cursorType),
      "type-at-cursor line present: " + cursorType
    );
    console.log(
      "ok - proof state: context + sorts + turnstile; cursor type: " +
        JSON.stringify(cursorType)
    );

    // Column-precise tracking (full mode): cursor near `h` in the
    // then-branch of nth's Cons arm must show the BRANCH state (i = 0
    // holds there), never the same-line else-branch obligation with
    // `not (i = 0)`.
    const branch = await page.evaluate(() => {
      const cm = window.__vox.cm;
      const idx = cm.getValue().indexOf("then h else");
      cm.setCursor(cm.posFromIndex(idx + 5)); // on `h`
      window.__vox.renderPane();
      return document.getElementById("pane-body").textContent;
    });
    assert.ok(
      !branch.includes("not (i = 0)"),
      "then-branch must not show the else obligation: " + branch.slice(0, 200)
    );
    assert.ok(
      /i = 0/.test(branch),
      "then-branch shows its own fact i = 0: " + branch.slice(0, 200)
    );
    console.log("ok - column-precise: then-branch shows i = 0, not the else VC");

    // Context rows hover like hypotheses: highlighting the BINDER.
    const ctxHover = await page.evaluate(() => {
      const row = document.querySelector("#pane-body .ctx.prov");
      if (!row) return { present: false };
      row.dispatchEvent(new MouseEvent("mouseenter", { bubbles: true }));
      const marks = window.__vox.cm
        .getAllMarks()
        .filter((m) => m.className === "vox-prov-hl");
      const painted = marks.length === 1
        ? window.__vox.cm.getRange(marks[0].find().from, marks[0].find().to)
        : null;
      row.dispatchEvent(new MouseEvent("mouseleave", { bubbles: true }));
      const cleared =
        window.__vox.cm.getAllMarks().filter((m) => m.className === "vox-prov-hl")
          .length === 0;
      return { present: true, painted, cleared,
               name: row.querySelector(".ctx-name").textContent };
    });
    assert.ok(ctxHover.present, "a hoverable context row exists");
    assert.strictEqual(
      ctxHover.painted,
      ctxHover.name.replace(/#\d+$/, "").replace(/@\d+$/, ""),
      "hovering the row highlights the binder: " + JSON.stringify(ctxHover)
    );
    assert.ok(ctxHover.cleared, "context hover clears on mouse-out");
    console.log("ok - context row hover highlights the binder:",
      JSON.stringify(ctxHover.painted));

    // Provenance hover: pick a VC that carries spans (preferring one that
    // ALSO has a span-less hypothesis), open it, and assert that hovering a
    // spanned row paints the source span and mouse-out clears it, while the
    // span-less hypothesis gets no hover affordance.
    const prov = await page.evaluate(() => {
      const cands = window.__vox
        .getRegions()
        .filter((r) => r.kind === "vc")
        .map((r) => {
          const hs = r.hyp_spans || [];
          return {
            r,
            spanned: (r.goal_span ? 1 : 0) + hs.filter(Boolean).length,
            spanless: hs.filter((s) => !s).length,
            first: r.goal_span || hs.find(Boolean) || null,
          };
        })
        .filter((c) => c.spanned > 0);
      // Prefer a VC exercising BOTH a spanned row and a span-less hyp.
      cands.sort(
        (a, b) => (b.spanless > 0) - (a.spanless > 0) || b.spanned - a.spanned
      );
      const c = cands[0];
      return c
        ? {
            start: c.r.start,
            nSpanned: c.spanned,
            nSpanless: c.spanless,
            first: c.first,
          }
        : null;
    });
    assert.ok(prov, "the walkthrough has a VC carrying provenance spans");
    // Since match negations became subsumed-or-spanned, the walkthrough may
    // legitimately have NO span-less hypothesis; the no-affordance check
    // below runs only when one exists.
    await page.evaluate(
      (p) => window.__vox.cm.setCursor({ line: p.start.line, ch: p.start.col }),
      prov
    );
    // One hover-sensitive goal/hyp row per span (context rows carry
    // their own binder spans and are counted separately).
    await waitFor(
      async () =>
        (await page.$$(".hyp.prov, .goal.prov")).length === prov.nSpanned,
      5000,
      "one .prov row per span"
    );
    console.log("ok - " + prov.nSpanned + " hover-sensitive rows, one per span");

    // A span-less hypothesis renders as a plain .hyp with no .prov affordance.
    if (prov.nSpanless > 0) {
      const hasPlainHyp = await page.$$eval(".hyp", (els) =>
        els.some((e) => !e.classList.contains("prov"))
      );
      assert.ok(hasPlainHyp, "a span-less hypothesis has no hover affordance");
      console.log("ok - span-less hypothesis has no hover affordance");
    } else {
      console.log("ok - no span-less hypothesis in this VC (all spanned)");
    }

    // No provenance highlight before hovering.
    const before = await page.evaluate(
      () =>
        window.__vox.cm
          .getAllMarks()
          .filter((m) => m.className === "vox-prov-hl").length
    );
    assert.strictEqual(before, 0, "no provenance highlight before hover");

    // Hover the first spanned row (the goal): a single mark appears over the
    // exact source span, and its text matches getRange of markFromSpan.
    // Lean-style layout puts the goal row last; hover it specifically
    // and compare against the goal's own span.
    await page.$eval(".goal.prov", (el) =>
      el.dispatchEvent(new MouseEvent("mouseenter", { bubbles: true }))
    );
    const marked = await page.evaluate(() => {
      // find() carries an extra `sticky` field; keep only {line, ch}.
      const pos = (p) => ({ line: p.line, ch: p.ch });
      const ms = window.__vox.cm
        .getAllMarks()
        .filter((m) => m.className === "vox-prov-hl");
      if (ms.length !== 1) return { count: ms.length };
      const range = ms[0].find();
      return {
        count: 1,
        range: { from: pos(range.from), to: pos(range.to) },
        text: window.__vox.cm.getRange(range.from, range.to),
      };
    });
    assert.strictEqual(marked.count, 1, "exactly one highlight while hovering");
    const expected = await page.evaluate((span) => {
      const r = window.Selection.markFromSpan(span);
      return { range: r, text: window.__vox.cm.getRange(r.from, r.to) };
    }, prov.first);
    assert.deepStrictEqual(
      marked.range,
      expected.range,
      "highlight covers markFromSpan(span): " + JSON.stringify(marked.range)
    );
    assert.strictEqual(
      marked.text,
      expected.text,
      "highlight text is the span's source: " + JSON.stringify(marked.text)
    );
    assert.ok(marked.text.length > 0, "the span covers some source text");
    console.log("ok - hover paints the source span:", JSON.stringify(marked.text));

    // Mouse-out clears it.
    await page.$eval(".goal.prov", (el) =>
      el.dispatchEvent(new MouseEvent("mouseleave", { bubbles: true }))
    );
    const after = await page.evaluate(
      () =>
        window.__vox.cm
          .getAllMarks()
          .filter((m) => m.className === "vox-prov-hl").length
    );
    assert.strictEqual(after, 0, "highlight cleared on mouse-out");
    console.log("ok - highlight clears on mouse-out");

    // Cursor at an uncovered line that has a region above it: empty state
    // plus a nearest-jump secondary, and NO in-block claim / live button.
    const gap = await page.evaluate(() => {
      const rs = window.__vox.getRegions();
      const covered = new Set();
      rs.forEach((r) => {
        for (let l = r.start.line; l <= r.end.line; l++) covered.add(l);
      });
      const minR = Math.min(...rs.map((r) => r.start.line));
      const maxL = window.__vox.cm.lastLine();
      for (let l = minR + 1; l <= maxL; l++) if (!covered.has(l)) return l;
      return null;
    });
    assert.ok(gap !== null, "found an uncovered line below a region");
    await page.evaluate((l) => window.__vox.cm.setCursor({ line: l, ch: 0 }), gap);
    // Off-obligation the pane now shows the PROGRAM POINT state (or the
    // plain empty message where the walker never visited); either way it
    // must not claim in-block membership or offer the live button.
    const empty = await waitFor(
      async () => {
        const mode = await page.$eval("#pane-mode", (e) => e.textContent);
        const body = await page.$eval("#pane-body", (e) => e.textContent);
        // Either a program-point state or the single empty message
        // (the header stays blank off-region -- no duplicate text).
        if (!/program point/.test(mode) && !/No obligation/.test(body))
          return false;
        return body;
      },
      5000,
      "off-region pane (program point or empty)"
    );
    console.log("ok - off-region line shows program point / empty state");

    // Program-point view: on a line with NO obligation (the `match a
    // with` header inside append), the pane shows the state of "here" --
    // the variables in scope -- instead of a bare empty message.
    const point = await page.evaluate(() => {
      const cm = window.__vox.cm;
      const idx = cm.getValue().indexOf("match a with");
      cm.setCursor(cm.posFromIndex(idx + 2));
      window.__vox.renderPane();
      return {
        mode: document.getElementById("pane-mode").textContent,
        ctx: Array.from(document.querySelectorAll("#pane-body .ctx")).map(
          (e) => e.textContent
        ),
      };
    });
    assert.strictEqual(
      point.mode,
      "program point",
      "pane mode says so; got " + JSON.stringify(point)
    );
    assert.ok(
      point.ctx.some((t) => /^a : ilist/.test(t)) &&
        point.ctx.some((t) => /^b : ilist/.test(t)),
      "the state shows a and b in scope: " + JSON.stringify(point.ctx)
    );
    console.log("ok - program-point view shows scope off-obligation");
    assert.ok(!/Inside a/.test(empty), "must not claim in-block: " + empty.slice(0, 120));
    assert.strictEqual(
      await page.$("#live-btn"),
      null,
      "no live-goal button off-region"
    );
    assert.ok(await page.$("#jump-btn"), "offers a nearest-region jump");

    // The bug: a cursor in the file header (line 0, above every region)
    // used to get a bare empty state. Now the nearest fallback searches
    // downward too and offers the first obligation with a ↓ arrow.
    await page.evaluate(() => window.__vox.cm.setCursor({ line: 0, ch: 0 }));
    const headerJump = await waitFor(
      async () => {
        const b = await page.$("#jump-btn");
        return b ? page.$eval("#jump-btn", (e) => e.textContent) : false;
      },
      5000,
      "nearest jump above all regions"
    );
    assert.ok(
      /↓/.test(headerJump),
      "header cursor points DOWN to the nearest obligation: " + headerJump
    );
    console.log("ok - cursor above all regions finds the obligation below (↓)");

    // Live typing: an edit triggers the fast (no-Lean) pass, which
    // repaints the region map within ~half a second -- so hypotheses at
    // the cursor track the buffer -- carrying verdicts of
    // content-unchanged obligations; the slower full check then lands
    // the new obligation's verdict and the verified status.
    await page.evaluate(() => {
      const cm = window.__vox.cm;
      cm.replaceRange("\nlet extra : int{ _ >= 0 } = 5\n", {
        line: cm.lineCount(),
        ch: 0,
      });
    });
    const typed = await waitFor(
      async () =>
        await page.evaluate(() => {
          if (window.__vox.getLastCheckFast() !== true) return false;
          const rs = window.__vox.getRegions();
          const nu = rs.find(
            (r) => r.kind === "vc" && /5 >= 0/.test(r.goal || "")
          );
          if (!nu) return false;
          const carried = rs.find(
            (r) => r.kind === "vc" && /Cons \(h, r\)/.test(r.goal || "")
          );
          return {
            newStatus: nu.status,
            carried: carried ? carried.status : null,
          };
        }),
      8000,
      "fast pass paints the freshly-typed obligation"
    );
    assert.strictEqual(typed.newStatus, "unknown", "new VC awaits its verdict");
    assert.strictEqual(typed.carried, "proved", "untouched VC keeps its verdict");
    console.log("ok - typing repaints via the fast pass, verdicts carried");
    await waitFor(
      async () => {
        const t = await page.$eval("#status", (e) => e.textContent);
        return /verified/.test(t) ? t : false;
      },
      30000,
      "full check settles after typing"
    );
    const extraStatus = await page.evaluate(() => {
      const r = window.__vox
        .getRegions()
        .find((x) => x.kind === "vc" && /5 >= 0/.test(x.goal || ""));
      return r && r.status;
    });
    assert.strictEqual(extraStatus, "proved", "full check proves the new VC");
    console.log("ok - full check follows and proves the new obligation");

    // Examples dropdown: pick fib (a [%%vox.lean] block example) and
    // drive its static block theorem + a live Lean goal.
    await page.evaluate(() => {
      window.confirm = () => true; // never block the headless run
    });
    const optCount = await page.$eval("#examples", (e) => e.options.length);
    assert.ok(optCount > 1, "examples dropdown populated: " + optCount);
    await page.evaluate(() => {
      const sel = document.getElementById("examples");
      sel.value = "fib";
      sel.dispatchEvent(new Event("change"));
    });
    const loaded = await waitFor(
      async () => {
        const t = await page.evaluate(() => window.__vox.cm.getValue());
        return /fib_rec|total_ fib/.test(t) ? t : false;
      },
      10000,
      "fib loaded into editor"
    );
    console.log("ok - picked fib from the dropdown");
    assert.ok(!/let rec nth/.test(loaded), "walkthrough replaced by fib");
    const fibStatus = await waitFor(
      async () => {
        const t = await page.$eval("#status", (e) => e.textContent);
        return /verified|errors/.test(t) ? t : false;
      },
      60000,
      "fib check"
    );
    console.log("ok - fib checks:", fibStatus.trim());
    assert.ok(fibStatus.includes("verified"), "fib should verify: " + fibStatus);

    // Cursor inside a block theorem: static goal should appear. Wait for
    // fib's regions to land (the check response updates them async).
    const thmPos = await waitFor(
      () =>
        page.evaluate(() => {
          const r = window.__vox.getRegions().find((x) => x.kind === "theorem");
          return r ? { line: r.start.line + 1, col: 2 } : false;
        }),
      10000,
      "fib block theorem region"
    );
    await page.evaluate((p) => window.__vox.cm.setCursor({ line: p.line, ch: p.col }), thmPos);
    const paneThm = await waitFor(
      async () => {
        const t = await page.$eval("#pane-body", (e) => e.textContent);
        return /theorem/.test(t) ? t : false;
      },
      5000,
      "theorem pane"
    );
    console.log("ok - block theorem shows a static goal");
    assert.ok(/fib/.test(paneThm), "static goal mentions fib: " + paneThm.slice(0, 160));

    // Click the live-goal button and wait for the real Lean proof state.
    await page.click("#live-btn");
    const live = await waitFor(
      async () => {
        const el = await page.$(".live-goals");
        if (!el) return false;
        return page.$eval(".live-goals", (e) => e.textContent);
      },
      60000,
      "live goal"
    );
    console.log("ok - live Lean goal fetched");
    assert.ok(live.includes("⊢"), "live goal has a turnstile: " + live.slice(0, 160));

    // The user's fib flow, both reported bugs at once.  Keep the verified
    // source, then DELETE the [%%vox.lean] block (the fast-doubling lemmas)
    // exactly as the user did.
    const fibSrc = await page.evaluate(() => window.__vox.cm.getValue());
    const deleted = await page.evaluate(() => {
      const cm = window.__vox.cm;
      const lines = cm.getValue().split("\n");
      let s = -1;
      let e = -1;
      for (let i = 0; i < lines.length; i++) {
        // the block OPENER carries "{lean|"; a bare "[%%vox.lean]" in the
        // header comment must not match (deleting from there would take
        // the reflected `fib` def with it -> a hard error, not per-VC).
        if (s < 0 && lines[i].includes("[%%vox.lean {lean|")) s = i;
        if (s >= 0 && lines[i].includes("|lean}]")) {
          e = i;
          break;
        }
      }
      if (s < 0 || e < 0) return null;
      cm.replaceRange("", { line: s, ch: 0 }, { line: e + 1, ch: 0 });
      return { s, e };
    });
    assert.ok(deleted, "found and deleted the [%%vox.lean] block");
    // The edit fires both passes on its own; a "failed"/"unproved" status
    // can ONLY come from the full (Lean) pass -- the fast dry-run never
    // solves -- so polling for the coexistence below is race-proof.
    const FAILED = ["failed", "disproved", "unproved"];
    // BUG 2: on the failing check, the doubling obligations fail but the
    // rest still PROVE -- the compiler attributes a per-VC verdict, so the
    // editor keeps green badges on the survivors instead of greying all.
    const coexist = await waitFor(
      () =>
        page.evaluate((FAILED) => {
          const rs = window.__vox.getRegions().filter((r) => r.kind === "vc");
          const proved = rs.filter((r) => r.status === "proved").length;
          const failed = rs.filter((r) => FAILED.includes(r.status)).length;
          if (proved === 0 || failed === 0) return false;
          return {
            proved,
            failed,
            // the fib(2k) doubling obligation needs the deleted lemma
            doublingFailed: rs.filter(
              (r) =>
                /fib \(2 \* k\)/.test(r.goal || "") &&
                FAILED.includes(r.status)
            ).length,
            status: document.getElementById("status").textContent,
          };
        }, FAILED),
      90000,
      "full check attributes per-VC verdicts (proved + failed coexist)"
    );
    assert.ok(
      coexist.doublingFailed > 0,
      "a fib(2k) obligation is among the failures: " + JSON.stringify(coexist)
    );
    // The status bar summarises the split, not a flat "errors".
    assert.ok(
      /\d+ unproved \/ \d+ proved/.test(coexist.status),
      "status summarises partial success: " + coexist.status
    );
    console.log(
      "ok - deleting the block: proved and failed VCs coexist (" +
        JSON.stringify(coexist) + ")"
    );

    // Visual contract, part 1: an UNPROVED goal (no validated
    // counterexample) underlines DASHED red.  Cursor onto the failing
    // doubling VC first so CodeMirror renders that line's marker.
    const upos = await page.evaluate((FAILED) => {
      const r = window.__vox
        .getRegions()
        .find(
          (x) =>
            x.kind === "vc" &&
            x.status === "unproved" &&
            /fib \(2 \* k\)/.test(x.goal || "")
        );
      return r ? { line: r.start.line, col: r.start.col } : false;
    }, FAILED);
    assert.ok(upos, "an unproved doubling VC region exists");
    await page.evaluate((p) => window.__vox.cm.setCursor(p), upos);
    const unprovedMark = await waitFor(
      () =>
        page.evaluate(() => {
          const el = document.querySelector(".vc-unproved");
          if (!el) return false;
          const s = getComputedStyle(el);
          return { style: s.borderBottomStyle, color: s.borderBottomColor };
        }),
      5000,
      "the .vc-unproved underline"
    );
    assert.strictEqual(
      unprovedMark.style,
      "dashed",
      "unproved underline is dashed: " + JSON.stringify(unprovedMark)
    );
    // No solid-red or wavy line masquerading as this failure.
    assert.strictEqual(
      await page.$(".vc-failed"),
      null,
      "no legacy solid-red .vc-failed underline here"
    );
    console.log("ok - unproved goal underlines DASHED red:", JSON.stringify(unprovedMark));

    // BUG 1: RESTORE the block; every proved doubling obligation names the
    // lemma it used, NOT "<arithmetic>" (which contradicted the fact that
    // the goal fails without the lemma).
    await page.evaluate((src) => {
      window.__vox.setCompact(false);
      window.__vox.cm.setValue(src); // fires both passes
    }, fibSrc);
    await waitFor(
      async () => {
        const fast = await page.evaluate(() => window.__vox.getLastCheckFast());
        const t = await page.$eval("#status", (e) => e.textContent);
        return fast === false && /verified/.test(t) ? t : false;
      },
      60000,
      "fib verifies again after restoring the block"
    );
    // The x doubling obligation: goal is `... = fib (2 * k)` (not `+ 1`).
    const xPos = await waitFor(
      () =>
        page.evaluate(() => {
          const r = window.__vox.getRegions().find(
            (x) =>
              x.kind === "vc" &&
              /fib \(2 \* k\)/.test(x.goal || "") &&
              !/fib \(2 \* k \+ 1\)/.test(x.goal || "")
          );
          return r ? { line: r.start.line, col: r.start.col } : false;
        }),
      10000,
      "the fib(2k) doubling VC region"
    );
    await page.evaluate(
      (p) => window.__vox.cm.setCursor({ line: p.line, ch: p.col }),
      xPos
    );
    const usedRow = await waitFor(
      async () =>
        page.evaluate(() => {
          const row = document.querySelector("#pane-body .used");
          return row ? row.textContent : false;
        }),
      5000,
      "used-lemmas row for the doubling obligation"
    );
    assert.ok(
      /fib_double/.test(usedRow),
      "doubling obligation names its lemma: " + usedRow
    );
    assert.ok(
      !/arithmetic/.test(usedRow),
      "lemma-backed goal is NOT reported arithmetic-only: " + usedRow
    );
    console.log("ok - restored: doubling obligation names fib_double, not <arithmetic>");
    await page.evaluate(() => window.__vox.setCompact(false));

    // Visual contract, part 2: ALL THREE failure-display classes in ONE
    // file -- a validated-false goal (disproved -> SOLID red, with the
    // counterexample surfaced), a still-proved goal (proved -> GREEN), and
    // a goal grind gives up on with no witness (unproved -> DASHED red).
    // Assert they coexist with the right visuals, the counterexample is
    // reachable ON the disproved goal, the status bar counts disproofs
    // apart from unproved, and the legend explains the colours.
    const TRI_SRC = [
      "let wrong (n : int{ _ > 0 }) : int{ _ = n + 1 } = refine_ n",
      "let ok (x : int{ _ > 0 }) : int{ _ >= 0 } = refine_ (x + 1)",
      "let sq (x : int) : int{ _ >= 0 } = refine_ (x * x)",
      "",
    ].join("\n");
    await page.evaluate((src) => {
      window.__vox.setCompact(false);
      window.__vox.cm.setValue(src);
    }, TRI_SRC);
    const triStatus = await waitFor(
      async () => {
        const fast = await page.evaluate(() => window.__vox.getLastCheckFast());
        if (fast !== false) return false;
        return page.evaluate(() => {
          const rs = window.__vox.getRegions().filter((r) => r.kind === "vc");
          const by = (s) => rs.filter((r) => r.status === s).length;
          if (by("disproved") !== 1 || by("proved") !== 1 || by("unproved") !== 1)
            return false;
          return document.getElementById("status").textContent;
        });
      },
      90000,
      "tri-class file: disproved + proved + unproved coexist"
    );
    console.log("ok - all three failure-display classes coexist in one file");
    // Status bar counts disproofs SEPARATELY from unproved (no longer
    // lumping a genuinely-false goal under the milder "unproved").
    assert.ok(
      /1 disproved \/ 1 unproved \/ 1 proved/.test(triStatus),
      "status bar separates disproved from unproved: " + triStatus
    );
    console.log("ok - status bar: " + triStatus.trim());

    // The legend appears (only on failure) and shows the three families,
    // each swatch wearing its own vc-* underline (single source of truth).
    const legend = await page.evaluate(() => {
      const el = document.getElementById("legend");
      if (!el || el.hidden) return false;
      return Array.from(el.querySelectorAll(".leg")).map((s) => ({
        text: s.textContent,
        cls: s.className,
        style: getComputedStyle(s).borderBottomStyle,
      }));
    });
    assert.ok(legend && legend.length === 3, "legend shows three families: " + JSON.stringify(legend));
    assert.ok(
      legend.some((i) => /vc-proved/.test(i.cls) && i.style === "solid"),
      "legend: proved is solid: " + JSON.stringify(legend)
    );
    assert.ok(
      legend.some(
        (i) => /vc-disproved/.test(i.cls) && i.style === "solid" && /counterexample/.test(i.text)
      ),
      "legend: disproved is solid + names the counterexample: " + JSON.stringify(legend)
    );
    assert.ok(
      legend.some((i) => /vc-unproved/.test(i.cls) && i.style === "dashed"),
      "legend: unproved is dashed: " + JSON.stringify(legend)
    );
    console.log("ok - legend explains the three families");

    // Per-class underline: cursor onto each VC (renders its line's marker),
    // read the computed border style + colour.
    const markStyleFor = async (status) => {
      const pos = await page.evaluate((s) => {
        const r = window.__vox
          .getRegions()
          .find((x) => x.kind === "vc" && x.status === s);
        return r ? { line: r.start.line, col: r.start.col } : null;
      }, status);
      assert.ok(pos, "a " + status + " VC region exists");
      await page.evaluate((p) => window.__vox.cm.setCursor(p), pos);
      return waitFor(
        () =>
          page.evaluate((s) => {
            const el = document.querySelector(".vc-" + s);
            if (!el) return false;
            const cs = getComputedStyle(el);
            return { style: cs.borderBottomStyle, color: cs.borderBottomColor };
          }, status),
        5000,
        ".vc-" + status + " underline"
      );
    };
    const provedMark = await markStyleFor("proved");
    const disprovedMark = await markStyleFor("disproved");
    const unprovedMark2 = await markStyleFor("unproved");
    assert.strictEqual(provedMark.style, "solid", "proved underline solid: " + JSON.stringify(provedMark));
    assert.strictEqual(disprovedMark.style, "solid", "disproved underline solid: " + JSON.stringify(disprovedMark));
    assert.strictEqual(unprovedMark2.style, "dashed", "unproved underline dashed: " + JSON.stringify(unprovedMark2));
    assert.notStrictEqual(provedMark.color, disprovedMark.color, "green (proved) != red (disproved)");
    assert.strictEqual(
      disprovedMark.color,
      unprovedMark2.color,
      "disproved and unproved share the red fail colour: " +
        JSON.stringify({ disprovedMark, unprovedMark2 })
    );
    console.log(
      "ok - underlines: proved solid green, disproved SOLID red, unproved DASHED red"
    );

    // The validated counterexample is reachable ON the disproved goal
    // (its pane), labelled and with the falsifying assignment -- not only
    // in a global error strip.
    const dpos = await page.evaluate(() => {
      const r = window.__vox.getRegions().find((x) => x.status === "disproved");
      return { line: r.start.line, col: r.start.col };
    });
    await page.evaluate((p) => window.__vox.cm.setCursor(p), dpos);
    const cexPane = await waitFor(
      async () => {
        const t = await page.$eval("#pane-body", (e) => e.textContent);
        return /counterexample \(validated\)/.test(t) ? t : false;
      },
      5000,
      "validated counterexample reachable on the disproved goal"
    );
    assert.ok(
      /goal is false when/.test(cexPane) && /n = /.test(cexPane),
      "the counterexample names a falsifying assignment for n: " + cexPane.slice(0, 200)
    );
    const dBadge = await page
      .$eval("#pane-body .badge-disproved", (e) => e.textContent)
      .catch(() => null);
    assert.ok(dBadge && /disproved/.test(dBadge), "the goal badge reads 'disproved': " + dBadge);
    console.log("ok - counterexample reachable on the disproved goal, badge 'disproved'");

    // Screenshots of the tri-state pane in BOTH themes (only when asked,
    // via VOX_SHOTS=<dir> -- keeps the test portable otherwise).
    if (process.env.VOX_SHOTS) {
      const dir = process.env.VOX_SHOTS;
      await page.screenshot({ path: path.join(dir, "tristate_dark.png") });
      await page.click("#theme-btn");
      await new Promise((r) => setTimeout(r, 300));
      await page.evaluate((p) => window.__vox.cm.setCursor(p), dpos);
      await page.screenshot({ path: path.join(dir, "tristate_light.png") });
      await page.click("#theme-btn"); // restore dark for the theme test below
      await new Promise((r) => setTimeout(r, 200));
      console.log("ok - wrote tristate_{dark,light}.png to " + dir);
    }
    await page.evaluate(() => window.__vox.setCompact(true));

    // Examples dropdown: pick reverse (fully verified, but its borrow/slice
    // framing VCs are ASSUMED). They must badge as "trusted", not the grey
    // "unknown" that reads as "didn't verify".
    await page.evaluate(() => {
      const sel = document.getElementById("examples");
      sel.value = "reverse";
      sel.dispatchEvent(new Event("change"));
    });
    await waitFor(
      async () => {
        const t = await page.evaluate(() => window.__vox.cm.getValue());
        return /revinv|McCarthy|reverse/.test(t) ? t : false;
      },
      10000,
      "reverse loaded into editor"
    );
    const revStatus = await waitFor(
      async () => {
        const t = await page.$eval("#status", (e) => e.textContent);
        return /verified|errors/.test(t) ? t : false;
      },
      60000,
      "reverse check"
    );
    assert.ok(revStatus.includes("verified"), "reverse should verify: " + revStatus);
    // A trusted (assumed) VC region exists; put the cursor on it (which
    // scrolls it into CodeMirror's rendered viewport) and check both the
    // source underline and the pane badge.
    const trustedPos = await waitFor(
      () =>
        page.evaluate(() => {
          const r = window.__vox
            .getRegions()
            .find((x) => x.kind === "vc" && x.status === "trusted");
          return r ? { line: r.start.line, col: r.start.col } : false;
        }),
      10000,
      "a trusted VC region"
    );
    console.log("ok - reverse has trusted (assumed) VCs");
    await page.evaluate(
      (p) => window.__vox.cm.setCursor({ line: p.line, ch: p.col }),
      trustedPos
    );
    // Distinct .vc-trusted underline (not the grey .vc-unknown) on the now
    // in-view assumed VC.
    await waitFor(
      () => page.$(".vc-trusted").then((el) => !!el),
      5000,
      "the .vc-trusted underline"
    );
    assert.strictEqual(
      await page.$(".vc-unknown"),
      null,
      "no grey 'unknown' underline on a fully verified file"
    );
    // The pane shows a "trusted" badge, not "unknown".
    const trustedBadge = await waitFor(
      async () => {
        const el = await page.$(".badge-trusted");
        return el ? page.$eval(".badge-trusted", (e) => e.textContent) : false;
      },
      5000,
      "trusted badge in pane"
    );
    assert.strictEqual(trustedBadge.trim(), "trusted", "badge reads 'trusted'");
    assert.strictEqual(
      await page.$("#pane-body .badge-unknown"),
      null,
      "the assumed VC is not badged 'unknown'"
    );
    console.log("ok - assumed VC pane badge reads 'trusted'");

    // Used-lemmas row (-vox-explain-proofs): load a source whose VC is
    // closed by a [%%vox.lean] block theorem, so the report names that
    // theorem.  Full mode shows a "used lemmas" row; the name matches a
    // theorem region, so it hover-highlights the theorem's source span.
    // Compact mode shows no such row.
    const USED_SRC = [
      "type ilist =",
      "  | Nil",
      "  | Cons of int * ilist",
      "",
      "let rec total_ len (l : ilist) : int =",
      "  match l with",
      "  | Nil -> 0",
      "  | Cons (_, t) -> 1 + len t",
      "",
      "[%%vox.lean {lean|",
      "theorem len_nn (l : Vox_Input_ilist) : 0 <= len l := by",
      "  induction l <;> grind",
      "grind_pattern len_nn => len l",
      "|lean}]",
      "",
      "let use_it (l : ilist) : int{ _ >= 0 } = refine_ (len l)",
    ].join("\n");
    await page.evaluate(async (src) => {
      window.__vox.setCompact(false);
      window.__vox.cm.setValue(src);
      await window.__vox.check(false);
    }, USED_SRC);
    await waitFor(
      async () => {
        const t = await page.$eval("#status", (e) => e.textContent);
        return /verified|errors/.test(t) ? t : false;
      },
      60000,
      "used-lemmas source check"
    );
    // Put the cursor on the use_it obligation (goal mentions len).
    const usePos = await waitFor(
      () =>
        page.evaluate(() => {
          const r = window.__vox
            .getRegions()
            .find((x) => x.kind === "vc" && /len l >= 0/.test(x.goal || ""));
          return r ? { line: r.start.line, col: r.start.col } : false;
        }),
      10000,
      "the use_it VC region"
    );
    await page.evaluate(
      (p) => window.__vox.cm.setCursor({ line: p.line, ch: p.col }),
      usePos
    );
    // Full mode: a "used lemmas" row names len_nn, and the name is a
    // hover-sensitive .prov (it matches the theorem region).
    const usedFull = await waitFor(
      async () =>
        page.evaluate(() => {
          const row = document.querySelector("#pane-body .used");
          if (!row) return false;
          const named = row.querySelector(".used-name.prov");
          return {
            text: row.textContent,
            hasProv: !!named,
            name: named ? named.textContent : null,
          };
        }),
      5000,
      "the used-lemmas row (full mode)"
    );
    assert.ok(/used lemmas/.test(usedFull.text), "row labelled: " + usedFull.text);
    assert.ok(/len_nn/.test(usedFull.text), "row names len_nn: " + usedFull.text);
    assert.ok(usedFull.hasProv, "the block-theorem name is hover-sensitive");
    console.log("ok - used-lemmas row names the block theorem:", usedFull.name);

    // Hovering the used name highlights the theorem's source span.
    const usedHover = await page.evaluate(() => {
      const el = document.querySelector("#pane-body .used-name.prov");
      el.dispatchEvent(new MouseEvent("mouseenter", { bubbles: true }));
      const marks = window.__vox.cm
        .getAllMarks()
        .filter((m) => m.className === "vox-prov-hl");
      const text =
        marks.length === 1
          ? window.__vox.cm.getRange(marks[0].find().from, marks[0].find().to)
          : null;
      el.dispatchEvent(new MouseEvent("mouseleave", { bubbles: true }));
      const cleared =
        window.__vox.cm
          .getAllMarks()
          .filter((m) => m.className === "vox-prov-hl").length === 0;
      return { text, cleared };
    });
    assert.ok(
      usedHover.text && /len_nn/.test(usedHover.text),
      "hover highlights the theorem source: " + JSON.stringify(usedHover.text)
    );
    assert.ok(usedHover.cleared, "used-lemma hover clears on mouse-out");
    console.log("ok - hovering the used lemma highlights its theorem span");

    // Compact mode shows no used-lemmas row.
    const usedCompact = await page.evaluate(() => {
      window.__vox.setCompact(false); // ensure re-render baseline
      window.__vox.setCompact(true);
      return !!document.querySelector("#pane-body .used");
    });
    assert.ok(!usedCompact, "compact mode hides the used-lemmas row");
    await page.evaluate(() => window.__vox.setCompact(false));
    console.log("ok - compact mode hides the used-lemmas row");

    // Wrapped-predicate regression (the qsort bug): the compiler's Format
    // dumper breaks a long conjunction across physical lines (continuation
    // at column 0, after a `&&`); vc_index must rejoin it so the pane shows
    // the WHOLE goal, not a fragment truncated at `&&` with the rest
    // bleeding out.  This 7-conjunct refinement wraps under the dumper's
    // margin.
    const WRAP_SRC =
      "let f (x : int{ _ >= 0 }) : int{ _ >= 0 && _ >= 1 && _ >= 2 " +
      "&& _ >= 3 && _ >= 4 && _ >= 5 && _ >= 6 } =\n  refine_ (x + 100)\n";
    await page.evaluate((src) => {
      window.__vox.setCompact(false);
      window.__vox.cm.setValue(src);
    }, WRAP_SRC);
    await waitFor(
      async () => {
        const fast = await page.evaluate(() => window.__vox.getLastCheckFast());
        const t = await page.$eval("#status", (e) => e.textContent);
        return fast === false && /verified/.test(t) ? t : false;
      },
      60000,
      "wrapped-goal file verifies"
    );
    const wrapGoal = await page.evaluate(() => {
      const r = window.__vox
        .getRegions()
        .find((x) => x.kind === "vc" && /x \+ 100/.test(x.goal || ""));
      return r ? r.goal : null;
    });
    assert.ok(wrapGoal, "found the wrapped-goal VC");
    assert.ok(
      /x \+ 100 >= 6/.test(wrapGoal),
      "goal reassembled through the LAST conjunct: " + wrapGoal
    );
    assert.ok(
      !/&&\s*$/.test(wrapGoal),
      "goal is not truncated at a dangling &&: " + wrapGoal
    );
    // The pane renders the whole goal (cursor on the VC).
    const wrapPos = await page.evaluate(() => {
      const r = window.__vox
        .getRegions()
        .find((x) => x.kind === "vc" && /x \+ 100/.test(x.goal || ""));
      return { line: r.start.line, col: r.start.col };
    });
    await page.evaluate((p) => window.__vox.cm.setCursor(p), wrapPos);
    const paneGoal = await waitFor(
      async () => {
        const t = await page.$eval("#pane-body", (e) => e.textContent);
        return /x \+ 100 >= 0/.test(t) ? t : false;
      },
      5000,
      "wrapped goal rendered in the pane"
    );
    assert.ok(
      /x \+ 100 >= 6/.test(paneGoal),
      "pane shows the whole goal through >= 6, no bleed: " + paneGoal.slice(0, 200)
    );
    console.log("ok - wrapped predicate rejoined: pane shows the complete goal");

    // Task #70 on the ACTUAL qsort example: both defects at once.  Its
    // split3 obligations carry long wrapped conjunctions over anonymous
    // tuple-component values.  (1) hypotheses/goal must render COMPLETE
    // (not truncated at `&&`); (2) those anonymous values must display as
    // `anonN`, never the alarming Lean-metavar-looking `?N`.
    await page.evaluate(() => {
      const s = document.getElementById("examples");
      s.value = "qsort";
      s.dispatchEvent(new Event("change"));
    });
    await waitFor(
      async () =>
        /split3|let rec qsort/.test(
          await page.evaluate(() => window.__vox.cm.getValue())
        ),
      10000,
      "qsort loaded"
    );
    // The fast pass populates regions from the dump (no Lean needed); find a
    // VC carrying a WRAPPED conjunction over an anonymous value (raw region
    // text keeps `*unknownN*` for byte-for-byte hover; the pane transforms).
    const qsortVc = await waitFor(
      () =>
        page.evaluate(() => {
          const rs = window.__vox.getRegions().filter((r) => r.kind === "vc");
          const r = rs.find(
            (x) =>
              /\*unknown\d+\*/.test(x.goal || "") &&
              (x.goal.match(/&&/g) || []).length >= 2
          );
          return r
            ? { line: r.start.line, col: r.start.col, goal: r.goal }
            : false;
        }),
      60000,
      "a qsort VC with a wrapped conjunction over an anonymous value"
    );
    // Defect 1: the region goal is COMPLETE -- reassembled, not truncated.
    assert.ok(
      !/&&\s*$/.test(qsortVc.goal),
      "qsort VC goal is not truncated at a dangling &&: " + qsortVc.goal.slice(0, 160)
    );
    // Put the cursor on it and read the rendered pane.
    await page.evaluate(
      (p) => window.__vox.cm.setCursor({ line: p.line, ch: p.col }),
      qsortVc
    );
    const qPane = await waitFor(
      async () => {
        const t = await page.$eval("#pane-body", (e) => e.textContent);
        return /goal/.test(t) ? t : false;
      },
      5000,
      "qsort VC pane"
    );
    // Defect 2: anonymous values render as anonN, never `?N` (metavar-look).
    assert.ok(/anon\d/.test(qPane), "pane shows anonymized values as anonN: " + qPane.slice(0, 200));
    assert.ok(
      !/\?\d/.test(qPane),
      "pane must NOT show ?N metavar-style placeholders: " + qPane.slice(0, 200)
    );
    // And the raw *unknownN* internal name never leaks to the user.
    assert.ok(!/\*unknown/.test(qPane), "raw *unknownN* must not reach the pane");
    console.log("ok - qsort: complete wrapped hypotheses + anonN placeholders (no ?N, no *unknown*)");
    await page.evaluate(() => window.__vox.setCompact(true));

    // Theme: dark is the default (no OS sniffing); the toolbar toggle
    // flips to light, and the choice persists across a reload.
    const readBg = () =>
      page.evaluate(() =>
        getComputedStyle(document.documentElement).getPropertyValue("--bg").trim()
      );
    const darkBg = await readBg();
    assert.strictEqual(darkBg, "#10141a", "default theme is dark: " + darkBg);
    await page.click("#theme-btn");
    const lightBg = await readBg();
    assert.strictEqual(lightBg, "#ffffff", "toggle switches to light: " + lightBg);
    console.log("ok - theme toggle flips dark -> light:", darkBg, "->", lightBg);
    // Persist across a reload (localStorage), no OS-preference influence.
    await page.reload({ waitUntil: "domcontentloaded", timeout: 20000 });
    await page.waitForSelector(".CodeMirror", { timeout: 10000 });
    const afterReload = await readBg();
    assert.strictEqual(
      afterReload,
      "#ffffff",
      "light choice persists across reload: " + afterReload
    );
    console.log("ok - light choice persists across reload");

    console.log("\nALL BROWSER TESTS PASSED");
  } finally {
    if (browser) await browser.close();
    server.kill("SIGTERM");
  }
}

main().catch((e) => {
  console.error("BROWSER TEST FAILED:", e.message);
  process.exit(1);
});
