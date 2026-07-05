// Scripted demo video of the vox editor for the docs page hero.
//
// Same puppeteer harness as make_screenshot.js. Records a ~25-30s loop
// that narrates the whole story on the default walkthrough. The plot
// hinges on a one-character edit that is nearly invisible at page size,
// so this script injects two PRESENTATION-ONLY overlays from the capture
// side (the product UI itself is unchanged):
//
//   1. a staged CAPTION bar pinned to the bottom of the editor pane
//      (dark-theme styled, never over the code, the edited line, or the
//      proof pane), whose text changes with each beat; and
//   2. a highlight RING drawn over the exact CodeMirror coordinates of
//      the bound text `_ < len l`, so a cold viewer sees where to look
//      and watches the `=` appear INSIDE the ring.
//
// Beats (captions in sentence case, matching what the frames show):
//   green/verified  ->  "nth is verified -- the bound proves the Nil arm
//       can never run"
//   ->  ring the bound + weaken it  i < len l  ->  i <= len l  (insert
//       '=' after '<'; the page's canonical failure: the Nil arm becomes
//       reachable)  ->  check  ->  red underline + concrete counterexample
//       (i = 0 on the empty list) held as the money frame
//   ->  restore the bound (ring it, delete the '=')  ->  check  ->  green.
// Recording starts and ends on the same green VC frame + caption so it
// loops seamlessly. Dark theme; 940x600 CSS @ deviceScaleFactor 2.
//
// Format: puppeteer's page.screencast() -> VP9 webm (needs ffmpeg on PATH,
// which it shells out to); a size-guard re-encode keeps it under budget.
// Output docs/vox/editor.webm.
//
// Run: node make_demo_video.js   Requires: puppeteer-core (/tmp/vox-pptr),
// Chrome, ffmpeg, a built ocamlc, Lean.

const fs = require("fs");
const os = require("os");
const { spawn, spawnSync } = require("child_process");
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
const MAX_BYTES = 3.4 * 1024 * 1024; // keep well under the 3.5 MB budget

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

// Inject the two presentation-only overlays (caption bar + edit ring) and
// expose window.__demo to drive them. Styled from the page's theme
// variables so it reads as part of the dark UI without touching product
// code. The caption is pinned inside the editor pane's empty bottom strip,
// so it never covers the code, the edited line, or the proof pane.
async function injectOverlays(page) {
  await page.evaluate(() => {
    const css = `
      #demo-cap {
        position: fixed; z-index: 10000; box-sizing: border-box;
        background: rgba(20, 25, 32, 0.95);
        border: 1px solid var(--border, #2b333d);
        border-left: 3px solid var(--accent, #4c9be8);
        border-radius: 6px; padding: 8px 13px;
        font: 13px/1.4 var(--mono, monospace);
        box-shadow: 0 6px 22px rgba(0, 0, 0, 0.55);
        opacity: 0; transition: opacity 240ms ease; pointer-events: none;
      }
      #demo-cap .lbl {
        display: block; color: var(--accent, #4c9be8);
        font-size: 10px; letter-spacing: 0.8px; text-transform: uppercase;
        margin-bottom: 3px;
      }
      #demo-cap .txt { display: block; color: var(--fg, #d5dae1); }
      #demo-cap code {
        color: #eaf2fc; background: rgba(76, 155, 232, 0.18);
        padding: 0 3px; border-radius: 3px;
      }
      #demo-ring {
        position: fixed; z-index: 9999; pointer-events: none;
        border: 2px solid var(--accent, #4c9be8); border-radius: 6px;
        background: rgba(76, 155, 232, 0.10);
        box-shadow: 0 0 0 3px rgba(76, 155, 232, 0.16),
                    0 0 16px 2px rgba(76, 155, 232, 0.45);
        opacity: 0;
        transition: opacity 220ms ease, left 200ms ease, width 200ms ease;
      }
    `;
    const st = document.createElement("style");
    st.textContent = css;
    document.head.appendChild(st);

    const cap = document.createElement("div");
    cap.id = "demo-cap";
    cap.innerHTML = '<span class="lbl"></span><span class="txt"></span>';
    document.body.appendChild(cap);

    const ring = document.createElement("div");
    ring.id = "demo-ring";
    document.body.appendChild(ring);

    const place = () => {
      const p = document.getElementById("editor-pane").getBoundingClientRect();
      const m = 14;
      cap.style.left = p.left + m + "px";
      cap.style.width = p.width - 2 * m + "px";
      cap.style.bottom = m + "px";
    };
    place();
    window.addEventListener("resize", place);

    const setText = (label, html) => {
      cap.querySelector(".lbl").textContent = label || "";
      cap.querySelector(".txt").innerHTML = html || "";
    };
    window.__demo = {
      // Immediate (no fade-out): used for the first, pre-record frame.
      captionNow(label, html) {
        setText(label, html);
        cap.style.opacity = "1";
      },
      // Cross-fade to a new caption.
      caption(label, html) {
        cap.style.opacity = "0";
        setTimeout(() => {
          setText(label, html);
          cap.style.opacity = "1";
        }, 200);
      },
      // Draw / move the ring around the current bound text (whichever of
      // `_ < len l` / `_ <= len l` is present), computed from live
      // CodeMirror coordinates so it tracks the exact glyphs.
      ring(show) {
        if (!show) {
          ring.style.opacity = "0";
          return;
        }
        const cm = window.__vox.cm;
        const v = cm.getValue();
        let str = "_ <= len l";
        let idx = v.indexOf(str);
        if (idx < 0) {
          str = "_ < len l";
          idx = v.indexOf(str);
        }
        if (idx < 0) return;
        const a = cm.posFromIndex(idx);
        const b = cm.posFromIndex(idx + str.length);
        const ca = cm.charCoords(a, "window");
        const cb = cm.charCoords(b, "window");
        const padX = 5;
        const padY = 4;
        ring.style.left = ca.left - padX + "px";
        ring.style.top = ca.top - padY + "px";
        ring.style.width = cb.left - ca.left + 2 * padX + "px";
        ring.style.height = ca.bottom - ca.top + 2 * padY + "px";
        ring.style.opacity = "1";
      },
    };
  });
}

const CAP = {
  green: [
    "verified",
    "nth is verified — the bound proves the <code>Nil</code> arm can never run",
  ],
  weaken: [
    "weaken the bound",
    "change <code>_ &lt; len l</code> to <code>_ &lt;= len l</code>",
  ],
  refuted: [
    "the compiler refutes it",
    "the <code>Nil</code> arm is reachable now — counterexample <code>i = 0</code> on the empty list",
  ],
  restore: [
    "restore the bound",
    "put <code>_ &lt; len l</code> back — and it is verified again",
  ],
};

// Re-encode with ffmpeg if the native screencast overshoots the size
// budget. VP9 CRF over a 2x-oversampled, mostly-static text screen stays
// crisp at the ~940px display width. Returns the final byte size.
function guardSize(file) {
  let bytes = fs.statSync(file).size;
  if (bytes <= MAX_BYTES) return { bytes, reencoded: false };
  const tmp = file + ".reencode.webm";
  const r = spawnSync(
    "ffmpeg",
    ["-y", "-i", file, "-c:v", "libvpx-vp9", "-crf", "34", "-b:v", "0",
     "-pix_fmt", "yuv420p", "-an", tmp],
    { stdio: "ignore" }
  );
  if (r.status === 0 && fs.existsSync(tmp)) {
    fs.renameSync(tmp, file);
    bytes = fs.statSync(file).size;
    return { bytes, reencoded: true };
  }
  if (fs.existsSync(tmp)) fs.unlinkSync(tmp);
  return { bytes, reencoded: false };
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

    // Pre-record: default walkthrough loads + verifies; land on the VC,
    // inject the overlays, and raise the opening caption so frame 1 is
    // already narrated (and matches the closing frame for a clean loop).
    await waitStatus(page, /verified|errors/, 60000, "initial check");
    await cursorOnPreconditionVc(page);
    await injectOverlays(page);
    await page.evaluate((c) => window.__demo.captionNow(c[0], c[1]), CAP.green);
    await sleep(400);

    recorder = await page.screencast({ path: OUT });
    await sleep(4500); // hold: green, VC goal + hypotheses, caption up

    // Weaken the bound: point the ring at it, then insert '=' INSIDE it.
    await page.evaluate((c) => window.__demo.caption(c[0], c[1]), CAP.weaken);
    await page.evaluate(() => window.__demo.ring(true));
    await sleep(3000); // read the caption + register where the ring is
    await page.evaluate(() => {
      const cm = window.__vox.cm;
      const i = cm.getValue().indexOf("_ < len l");
      cm.setCursor(cm.posFromIndex(i + 3)); // just after the '<'
      cm.focus();
    });
    await sleep(600);
    await page.keyboard.type("=");
    await page.evaluate(() => window.__demo.ring(true)); // re-fit to `_ <= len l`
    await sleep(2800); // watch the '=' sit inside the ring (i <= len l)

    await page.click("#check-btn");
    await page.evaluate(() => window.__demo.ring(false)); // clear so the red arm shows
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
    await page.evaluate((c) => window.__demo.caption(c[0], c[1]), CAP.refuted);
    await sleep(6800); // hold: red underline + counterexample (the money moment)

    // Fix it back: ring the bound, delete the '=' we typed.
    await page.evaluate((c) => window.__demo.caption(c[0], c[1]), CAP.restore);
    await page.evaluate(() => window.__demo.ring(true));
    await sleep(700);
    await page.evaluate(() => {
      const cm = window.__vox.cm;
      const i = cm.getValue().indexOf("_ <= len l");
      cm.setCursor(cm.posFromIndex(i + 4)); // just after the '='
      cm.focus();
    });
    await sleep(500);
    await page.keyboard.press("Backspace");
    await page.evaluate(() => window.__demo.ring(true)); // re-fit to `_ < len l`
    await sleep(2600); // read the restored bound (i < len l)

    await page.click("#check-btn");
    await waitStatus(page, /verified/, 60000, "recovery check");
    await page.evaluate(() => window.__demo.ring(false));

    await cursorOnPreconditionVc(page);
    await page.evaluate((c) => window.__demo.caption(c[0], c[1]), CAP.green);
    await sleep(4500); // hold: green again (loop point, matches the open)

    await recorder.stop();
    recorder = null;

    const { bytes, reencoded } = guardSize(OUT);
    console.log(
      "wrote %s  (%dx%d, %d KB%s)",
      OUT,
      VIEWPORT.width * VIEWPORT.deviceScaleFactor,
      VIEWPORT.height * VIEWPORT.deviceScaleFactor,
      Math.round(bytes / 1024),
      reencoded ? ", re-encoded to fit budget" : ""
    );
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
