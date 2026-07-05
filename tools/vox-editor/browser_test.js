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

    // The sample auto-checks on load; wait for the verdict.
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
    console.log("ok - status after check:", status.trim());
    assert.ok(status.includes("verified"), "sample should verify: " + status);

    // Put the cursor on the VC (the `dbl 0` refinement, source line 10).
    await page.evaluate(() => window.__vox.cm.setCursor({ line: 10, ch: 20 }));
    const paneVc = await waitFor(
      async () => {
        const t = await page.$eval("#pane-body", (e) => e.textContent);
        return t.includes("goal") ? t : false;
      },
      5000,
      "vc pane"
    );
    console.log("ok - VC pane shows a goal");
    assert.ok(/dbl 0|= 0/.test(paneVc), "VC goal text: " + paneVc.slice(0, 120));

    // Put the cursor inside the block theorem; static goal should appear.
    await page.evaluate(() => window.__vox.cm.setCursor({ line: 6, ch: 4 }));
    const paneThm = await waitFor(
      async () => {
        const t = await page.$eval("#pane-body", (e) => e.textContent);
        return t.includes("dbl_nonneg") ? t : false;
      },
      5000,
      "theorem pane"
    );
    console.log("ok - block theorem shows static goal");
    assert.ok(paneThm.includes("dbl n >= 0"), "static goal: " + paneThm.slice(0, 160));

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
    // Genuine Lean proof state: a turnstile goal and a typed hypothesis.
    // (The exact goal depends on where in the proof the cursor sits; we
    // assert it is real Lean output, not a specific tactic state.)
    assert.ok(live.includes("⊢"), "live goal has a turnstile: " + live.slice(0, 160));
    assert.ok(/Int/.test(live), "live goal has a hypothesis: " + live.slice(0, 160));

    // Cursor NOT in any region (blank line 8, below the block): the pane
    // must show an empty state and must NOT claim we are in a block or
    // offer a live-goal button (the reported bug).
    await page.evaluate(() => window.__vox.cm.setCursor({ line: 8, ch: 0 }));
    const empty = await waitFor(
      async () => {
        const t = await page.$eval("#pane-body", (e) => e.textContent);
        return /No obligation at the cursor/.test(t) ? t : false;
      },
      5000,
      "empty state off-region"
    );
    console.log("ok - empty state when cursor is at no region");
    assert.ok(!/Inside a/.test(empty), "must not claim in-block: " + empty.slice(0, 120));
    assert.strictEqual(
      await page.$("#live-btn"),
      null,
      "no live-goal button when not in a block"
    );
    assert.ok(await page.$("#jump-btn"), "offers a nearest-region jump");

    // Examples dropdown: it is populated, and picking one loads that
    // source into the editor and re-checks to a verdict.
    await page.evaluate(() => {
      window.confirm = () => true; // never block the headless run
    });
    const optCount = await page.$eval("#examples", (e) => e.options.length);
    assert.ok(optCount > 1, "examples dropdown populated: " + optCount);
    await page.evaluate(() => {
      const sel = document.getElementById("examples");
      sel.value = "overview";
      sel.dispatchEvent(new Event("change"));
    });
    const loaded = await waitFor(
      async () => {
        const t = await page.evaluate(() => window.__vox.cm.getValue());
        return t.includes("let div") ? t : false;
      },
      10000,
      "example loaded into editor"
    );
    console.log("ok - example loaded into editor");
    assert.ok(!loaded.includes("total_ dbl"), "sample replaced by the example");
    const exStatus = await waitFor(
      async () => {
        const t = await page.$eval("#status", (e) => e.textContent);
        return /verified|errors/.test(t) ? t : false;
      },
      60000,
      "picked example to check"
    );
    console.log("ok - picked example checks:", exStatus.trim());
    assert.ok(exStatus.includes("verified"), "picked example verifies: " + exStatus);

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
