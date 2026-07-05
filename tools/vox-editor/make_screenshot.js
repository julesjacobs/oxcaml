// Scripted, committed screenshot of the vox editor for the docs page.
//
// Spawns the server, drives the real page in headless Chrome (the same
// machinery as browser_test.js), lets the default example (the
// len/append/nth walkthrough) load and auto-check, places the cursor on
// the meatiest VC (a goal WITH hypotheses in the pane), and captures a
// crisp PNG sized for display in the ~960px docs column.
//
// Sizing: viewport is 940x600 CSS px at deviceScaleFactor 2, so the PNG
// is ~1880x1200 and its text is legible at 940px display width (no
// high-res-rescaled-tiny). Dark theme ships; a light variant is also
// written when light mode is available.
//
// Run: node make_screenshot.js  (writes docs/vox/editor.png [+ -light]).
// Requires: puppeteer-core (/tmp/vox-pptr), Chrome, a built ocamlc, Lean.

const fs = require("fs");
const os = require("os");
const { spawn } = require("child_process");
const path = require("path");
const { pathToFileURL } = require("url");

async function loadPuppeteer() {
  const entry = require.resolve("puppeteer-core", {
    paths: ["/tmp/vox-pptr/node_modules"],
  });
  return (await import(pathToFileURL(entry).href)).default;
}

const HERE = __dirname;
const CHROME = fs.existsSync("/opt/google/chrome/chrome")
  ? "/opt/google/chrome/chrome"
  : "/usr/bin/google-chrome";
const PORT = 8737;
const OUT_DIR = path.join(HERE, "..", "..", "docs", "vox");
const VIEWPORT = { width: 940, height: 600, deviceScaleFactor: 2 };

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

async function capture(page, file) {
  const target = path.join(OUT_DIR, file);
  await page.screenshot({ path: target, type: "png" });
  const bytes = fs.statSync(target).size;
  console.log(
    "wrote %s  (%dx%d px, %d KB)",
    target,
    VIEWPORT.width * VIEWPORT.deviceScaleFactor,
    VIEWPORT.height * VIEWPORT.deviceScaleFactor,
    Math.round(bytes / 1024)
  );
}

async function main() {
  const server = spawn(
    "python3",
    ["-u", path.join(HERE, "server.py"), "--port", String(PORT)],
    {
      cwd: HERE,
      env: Object.assign({}, process.env, { NO_PROXY: "127.0.0.1,localhost" }),
      stdio: ["ignore", "pipe", "pipe"],
    }
  );
  let serverOut = "";
  server.stdout.on("data", (d) => (serverOut += d));
  server.stderr.on("data", (d) => (serverOut += d));

  let browser;
  try {
    await waitFor(() => serverOut.includes("vox-editor on"), 15000, "server start");
    const puppeteer = await loadPuppeteer();
    const profile = fs.mkdtempSync(path.join(os.tmpdir(), "voxshot"));
    browser = await puppeteer.launch({
      executablePath: CHROME,
      headless: true,
      userDataDir: profile,
      defaultViewport: VIEWPORT,
      args: [
        "--no-sandbox",
        "--no-proxy-server",
        "--disable-gpu",
        "--disable-dev-shm-usage",
        "--disable-extensions",
        "--force-device-scale-factor=2",
        "--hide-scrollbars",
        "--no-first-run",
        "--no-default-browser-check",
        "--homepage=about:blank",
      ],
    });
    const pages = await browser.pages();
    const page = pages.length ? pages[0] : await browser.newPage();
    await page.setViewport(VIEWPORT);
    await page.goto("http://127.0.0.1:" + PORT + "/", {
      waitUntil: "domcontentloaded",
      timeout: 20000,
    });
    await page.waitForSelector(".CodeMirror", { timeout: 10000 });

    // Default example (walkthrough) auto-checks: wait for the verdict.
    await waitFor(
      async () => {
        const t = await page.$eval("#status", (e) => e.textContent);
        return /verified|errors/.test(t) ? t : false;
      },
      60000,
      "check to finish"
    );

    // Put the cursor on the meatiest VC (most hypotheses) so the pane
    // shows a goal WITH hypotheses -- the append induction step / nth's
    // recursive-call precondition.
    const picked = await waitFor(
      () =>
        page.evaluate(() => {
          const vcs = window.__vox.getRegions().filter((r) => r.kind === "vc");
          if (!vcs.length) return false;
          vcs.sort(
            (a, b) => (b.hypotheses || []).length - (a.hypotheses || []).length
          );
          const r = vcs[0];
          window.__vox.cm.setCursor({ line: r.start.line, ch: r.start.col });
          window.__vox.cm.focus();
          return { line: r.start.line, goal: r.goal, hyps: (r.hypotheses || []).length };
        }),
      10000,
      "a VC with hypotheses"
    );
    console.log(
      "cursor on VC at line %d: goal=%j (%d hypotheses)",
      picked.line + 1,
      picked.goal,
      picked.hyps
    );
    // Let the pane render and the editor settle its scroll.
    await waitFor(
      async () => {
        const t = await page.$eval("#pane-body", (e) => e.textContent);
        return /goal/.test(t) ? t : false;
      },
      5000,
      "pane goal"
    );
    await new Promise((r) => setTimeout(r, 300));

    await page.emulateMediaFeatures([
      { name: "prefers-color-scheme", value: "dark" },
    ]);
    await new Promise((r) => setTimeout(r, 150));
    await capture(page, "editor.png");

    await page.emulateMediaFeatures([
      { name: "prefers-color-scheme", value: "light" },
    ]);
    await new Promise((r) => setTimeout(r, 150));
    await capture(page, "editor-light.png");

    console.log("\nSCREENSHOTS DONE");
  } finally {
    if (browser) await browser.close();
    server.kill("SIGTERM");
  }
}

main().catch((e) => {
  console.error("SCREENSHOT FAILED:", e.message);
  process.exit(1);
});
