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
    const empty = await waitFor(
      async () => {
        const t = await page.$eval("#pane-body", (e) => e.textContent);
        return /No obligation at the cursor/.test(t) ? t : false;
      },
      5000,
      "empty state off-region"
    );
    console.log("ok - empty state at an off-region line");
    assert.ok(!/Inside a/.test(empty), "must not claim in-block: " + empty.slice(0, 120));
    assert.strictEqual(
      await page.$("#live-btn"),
      null,
      "no live-goal button off-region"
    );
    assert.ok(await page.$("#jump-btn"), "offers a nearest-region jump");

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
