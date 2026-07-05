// Scripted demo video of the vox editor for the docs page hero.
//
// Same puppeteer harness as make_screenshot.js. Records a ~20s loop that
// tells the whole story on the default walkthrough:
//   green/verified  ->  cursor on nth's precondition VC (goal+hypotheses)
//   ->  weaken the bound  i < len l  to  i <= len l  (the page's canonical
//       failure: the Nil arm becomes reachable)  ->  check  ->  red
//       underline + the concrete counterexample in the pane
//   ->  fix it back  ->  check  ->  green again.
// Recording starts and ends on the same green VC frame so it loops
// seamlessly. Dark theme; 940x600 CSS @ deviceScaleFactor 2.
//
// Format: puppeteer's page.screencast() -> VP9 webm (needs ffmpeg on PATH,
// which it shells out to). Output docs/vox/editor.webm.
//
// Run: node make_demo_video.js   Requires: puppeteer-core (/tmp/vox-pptr),
// Chrome, ffmpeg, a built ocamlc, Lean.

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
const PORT = 8739;
const OUT = path.join(HERE, "..", "..", "docs", "vox", "editor.webm");
const VIEWPORT = { width: 940, height: 600, deviceScaleFactor: 2 };

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function waitFor(predicate, timeoutMs, desc) {
  return new Promise((resolve, reject) => {
    const start = Date.now();
    const tick = () => {
      Promise.resolve(predicate()).then((v) => {
        if (v) return resolve(v);
        if (Date.now() - start > timeoutMs) return reject(new Error("timeout: " + desc));
        setTimeout(tick, 80);
      }, reject);
    };
    tick();
  });
}

const statusText = (page) => page.$eval("#status", (e) => e.textContent);
const waitStatus = (page, re, ms, desc) =>
  waitFor(async () => (re.test(await statusText(page)) ? true : false), ms, desc);

// Put the cursor on nth's recursive-call precondition VC (the meatiest,
// most hypotheses) and wait for the pane to show its goal.
async function cursorOnPreconditionVc(page) {
  await page.evaluate(() => {
    const vcs = window.__vox.getRegions().filter((r) => r.kind === "vc");
    vcs.sort((a, b) => (b.hypotheses || []).length - (a.hypotheses || []).length);
    const r = vcs[0];
    window.__vox.cm.setCursor({ line: r.start.line, ch: r.start.col });
    window.__vox.cm.focus();
  });
  await waitFor(
    async () => /goal/.test(await page.$eval("#pane-body", (e) => e.textContent)),
    5000,
    "precondition VC pane"
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
  let recorder;
  try {
    await waitFor(() => serverOut.includes("vox-editor on"), 15000, "server start");
    const puppeteer = await loadPuppeteer();
    const profile = fs.mkdtempSync(path.join(os.tmpdir(), "voxvid"));
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

    // Pre-record: default walkthrough loads + verifies; land on the VC.
    await waitStatus(page, /verified|errors/, 60000, "initial check");
    await cursorOnPreconditionVc(page);
    await sleep(300);

    // Record the break-and-fix loop.
    recorder = await page.screencast({ path: OUT });
    await sleep(4000); // hold: green, VC goal + hypotheses

    // Weaken the bound: i < len l  ->  i <= len l (insert '=' after '<').
    await page.evaluate(() => {
      const cm = window.__vox.cm;
      const i = cm.getValue().indexOf("_ < len l");
      cm.setCursor(cm.posFromIndex(i + 3)); // just after the '<'
      cm.focus();
    });
    await sleep(500);
    await page.keyboard.type("=");
    await sleep(1300); // let the viewer read the changed bound (i <= len l)
    await page.click("#check-btn");
    await waitStatus(page, /errors/, 60000, "failure check");

    // Show the failure on the now-reachable Nil arm, with counterexample.
    await page.evaluate(() => {
      const bad = window.__vox
        .getRegions()
        .find((r) => r.kind === "vc" && (r.status === "failed" || r.counterexample));
      if (bad) window.__vox.cm.setCursor({ line: bad.start.line, ch: bad.start.col });
    });
    await waitFor(
      async () =>
        /counterexample/.test(await page.$eval("#pane-body", (e) => e.textContent)),
      5000,
      "counterexample pane"
    );
    await sleep(5500); // hold: red underline + counterexample (the money moment)

    // Fix it back: delete the '=' we typed.
    await page.evaluate(() => {
      const cm = window.__vox.cm;
      const i = cm.getValue().indexOf("_ <= len l");
      cm.setCursor(cm.posFromIndex(i + 4)); // just after the '='
      cm.focus();
    });
    await sleep(500);
    await page.keyboard.press("Backspace");
    await sleep(1300); // let the viewer read the restored bound (i < len l)
    await page.click("#check-btn");
    await waitStatus(page, /verified/, 60000, "recovery check");

    await cursorOnPreconditionVc(page);
    await sleep(4000); // hold: green again (loop point)

    await recorder.stop();
    recorder = null;
    const bytes = fs.statSync(OUT).size;
    console.log("wrote %s  (%dx%d, %d KB)", OUT,
      VIEWPORT.width * VIEWPORT.deviceScaleFactor,
      VIEWPORT.height * VIEWPORT.deviceScaleFactor,
      Math.round(bytes / 1024));
    console.log("\nDEMO VIDEO DONE");
  } finally {
    if (recorder) await recorder.stop().catch(() => {});
    if (browser) await browser.close();
    server.kill("SIGTERM");
  }
}

main().catch((e) => {
  console.error("DEMO VIDEO FAILED:", e.message);
  process.exit(1);
});
