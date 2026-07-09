// vox-web browser smoke test.
//
// Serves the static bundle with `python3 -m http.server` (NO app server:
// the /check pipeline is a Web Worker + MockBackend), drives the page in
// headless Chrome, and asserts the worker returned the mock /check and
// the UI rendered the VCs — proving the static-bundle + worker + backend
// pipeline works browser-only.
//
// Run: node web_test.js   (needs puppeteer-core under /tmp/vox-pptr and
// /opt/google/chrome/chrome). Exits non-zero on failure.

const assert = require("assert");
const { spawn } = require("child_process");
const path = require("path");
const { pathToFileURL } = require("url");
const fs = require("fs");

async function loadPuppeteer() {
  const entry = require.resolve("puppeteer-core", { paths: ["/tmp/vox-pptr/node_modules"] });
  return (await import(pathToFileURL(entry).href)).default;
}

const HERE = __dirname;
const CHROME = fs.existsSync("/opt/google/chrome/chrome")
  ? "/opt/google/chrome/chrome"
  : "/usr/bin/google-chrome";
const PORT = 8749;

function sleep(ms) {
  return new Promise((r) => setTimeout(r, ms));
}

async function main() {
  const server = spawn("python3", ["-m", "http.server", String(PORT), "--bind", "127.0.0.1"], {
    cwd: HERE,
    stdio: "ignore",
  });
  const puppeteer = await loadPuppeteer();
  let browser;
  try {
    await sleep(600); // let the server bind
    browser = await puppeteer.launch({
      executablePath: CHROME,
      headless: "new",
      args: ["--no-sandbox", "--disable-gpu"],
    });
    const page = await browser.newPage();
    // Static-only: bypass any ambient proxy for localhost.
    await page.goto(`http://127.0.0.1:${PORT}/index.html`, { waitUntil: "load" });

    // Wait for the worker's mock /check to land.
    await page.waitForFunction("window.__vox && window.__vox.lastResult", { timeout: 8000 });

    const result = await page.evaluate("window.__vox.lastResult");
    const vcs = (result.regions || []).filter((r) => r.kind === "vc");
    assert.strictEqual(vcs.length, 2, "2 VCs from worker");
    assert.ok(vcs.some((v) => v.status === "proved"), "a proved VC");
    assert.ok(vcs.some((v) => v.status === "failed"), "a failed VC");

    const status = await page.$eval("#status", (e) => e.textContent);
    assert.strictEqual(status, "not verified", "status reflects mock result");

    const nProved = await page.$$eval(".vc.proved", (els) => els.length);
    const nFailed = await page.$$eval(".vc.failed", (els) => els.length);
    assert.strictEqual(nProved, 1, "one .vc.proved rendered");
    assert.strictEqual(nFailed, 1, "one .vc.failed rendered");

    const lean = await page.$eval("#lean", (e) => e.textContent);
    assert.ok(lean.includes("generated Lean"), "generated-lean pane populated");

    console.log("web_test: OK (static bundle + worker + MockBackend, no app server)");
  } finally {
    if (browser) await browser.close();
    server.kill("SIGKILL");
  }
}

main().catch((e) => {
  console.error("web_test FAILED:", e);
  process.exit(1);
});
