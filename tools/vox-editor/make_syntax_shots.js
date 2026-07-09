// Screenshot harness for the vox syntax highlighting.
//
// Spawns the editor server, then for a curated set of examples captures
// the editor pane in both themes (and, for a couple, the stock-mllike
// "before" for comparison and a scroll to the [%%vox.lean] block).  The
// PNGs land in OUT so they can be eyeballed.
//
// Run: node make_syntax_shots.js
// (Highlighting is entirely client-side, so the compiler/Lean are not
// needed here; the server just serves the static assets.)

const fs = require("fs");
const os = require("os");
const path = require("path");
const { spawn } = require("child_process");
const { pathToFileURL } = require("url");

const HERE = __dirname;
const OUT = "/tmp/vox-shots";
const PORT = 8753;
const CHROME = fs.existsSync("/opt/google/chrome/chrome")
  ? "/opt/google/chrome/chrome"
  : "/usr/bin/google-chrome";
const OCAMLC =
  "/usr/local/home/jujacobs/oxcamls/vox-editor/_build/_bootinstall/bin/ocamlc.opt";

async function loadPuppeteer() {
  const entry = require.resolve("puppeteer-core", {
    paths: ["/tmp/vox-pptr/node_modules"],
  });
  return (await import(pathToFileURL(entry).href)).default;
}

function waitFor(pred, timeoutMs, desc) {
  return new Promise((resolve, reject) => {
    const start = Date.now();
    const tick = () =>
      Promise.resolve(pred()).then((v) => {
        if (v) return resolve(v);
        if (Date.now() - start > timeoutMs) return reject(new Error("timeout: " + desc));
        setTimeout(tick, 100);
      }, reject);
    tick();
  });
}

// The curated shot list.  before:true also captures the stock mllike mode;
// scrollTo jumps CodeMirror to a 0-based line before the shot.
const SHOTS = [
  { name: "overview" },
  { name: "nth", before: true },
  { name: "mutable" },
  { name: "quant" },
  { name: "tuples" },
  { name: "fib", scrollTo: 30, tag: "lean", before: true },
];

async function main() {
  fs.mkdirSync(OUT, { recursive: true });
  const server = spawn(
    "python3",
    ["-u", path.join(HERE, "server.py"), "--port", String(PORT), "--no-lean"],
    {
      cwd: HERE,
      env: Object.assign({}, process.env, {
        NO_PROXY: "127.0.0.1,localhost",
        VOX_OCAMLC: OCAMLC,
      }),
      stdio: ["ignore", "pipe", "pipe"],
    }
  );
  let out = "";
  server.stdout.on("data", (d) => (out += d));
  server.stderr.on("data", (d) => (out += d));

  let browser;
  try {
    await waitFor(() => out.includes("vox-editor on"), 15000, "server start");
    const puppeteer = await loadPuppeteer();
    const profile = fs.mkdtempSync(path.join(os.tmpdir(), "voxshot"));
    browser = await puppeteer.launch({
      executablePath: CHROME,
      headless: true,
      userDataDir: profile,
      defaultViewport: { width: 1360, height: 900 },
      args: [
        "--no-sandbox", "--no-proxy-server", "--disable-gpu",
        "--disable-dev-shm-usage", "--disable-extensions",
        "--no-first-run", "--no-default-browser-check",
      ],
    });
    const pages = await browser.pages();
    const page = pages.length ? pages[0] : await browser.newPage();
    await page.goto("http://127.0.0.1:" + PORT + "/", {
      waitUntil: "domcontentloaded",
      timeout: 20000,
    });
    await page.waitForSelector(".CodeMirror", { timeout: 10000 });

    async function setTheme(theme) {
      await page.evaluate((t) => {
        if (t === "light") document.documentElement.dataset.theme = "light";
        else delete document.documentElement.dataset.theme;
      }, theme);
    }
    async function setSource(src, mode, scrollTo) {
      await page.evaluate(
        (s, m, line) => {
          const cm = window.__vox.cm;
          cm.setOption("mode", m);
          cm.setValue(s);
          cm.refresh();
          if (line != null) cm.scrollIntoView({ line: line, ch: 0 }, 200);
        },
        src, mode, scrollTo == null ? null : scrollTo
      );
      // Let CodeMirror finish laying out the newly highlighted viewport.
      await new Promise((r) => setTimeout(r, 250));
    }
    async function shot(file) {
      const el = await page.$("#editor-pane");
      await el.screenshot({ path: path.join(OUT, file) });
      console.log("wrote", file);
    }

    for (const s of SHOTS) {
      const src = fs.readFileSync(
        path.join(HERE, "examples", s.name + ".ml"), "utf8"
      );
      const base = s.name + (s.tag ? "-" + s.tag : "");
      for (const theme of ["dark", "light"]) {
        await setTheme(theme);
        if (s.before) {
          await setSource(src, "text/x-ocaml", s.scrollTo);
          await shot(base + "-" + theme + "-before.png");
        }
        await setSource(src, "text/x-vox", s.scrollTo);
        await shot(base + "-" + theme + "-after.png");
      }
    }
    console.log("\nDONE ->", OUT);
  } finally {
    if (browser) await browser.close();
    server.kill("SIGTERM");
  }
}

main().catch((e) => {
  console.error("SHOT HARNESS FAILED:", e.message);
  process.exit(1);
});
