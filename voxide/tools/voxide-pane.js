#!/usr/bin/env node
"use strict";

// voxide-pane -- show PRECISELY what the vox2 IDE right (proof) pane displays,
// from the terminal, queryable by exact cursor position.
//
// It renders from the IDE's OWN pane logic: pane_model.js is the single shared
// model the browser (app.js) and this tool both consume, so the terminal output
// cannot drift from what the user sees (the anti-drift lock in
// tests/test_pane_fidelity.js enforces CLI text == browser DOM textContent).
//
// PRIMARY mode (--map): a static cursor->pane map of a whole file -- a per-column
// glyph ruler under each source line indexing a global legend of the unique
// panes.  Point-query mode (--line/--col) is the building block; both share the
// exact same model.
//
// The /vcs payload can come from a captured dump (--vcs-json, deterministic,
// offline) or live from the worktree compiler (a throwaway server on an
// ephemeral port -- never the user's live :8471).
//
// Usage: voxide-pane <file.ml> [options]   (see --help)

const fs = require("fs");
const path = require("path");
const cp = require("child_process");

const ROOT = path.resolve(__dirname, "..");
const model = require(path.join(ROOT, "pane_model.js"));

// ---------------------------------------------------------------------------
// CLI parsing
// ---------------------------------------------------------------------------

const HELP = `voxide-pane -- terminal mirror of the vox2 IDE proof pane

  voxide-pane <file.ml> [options]

Modes
  --map                 PRIMARY: static cursor->pane map of the whole file
                        (per-column glyph ruler + global pane legend)
  --line L --col C      point query: the pane at caret line L, column C
                        (1-based, exactly as the editor shows the cursor)

Obtaining the obligations (/vcs)
  --vcs-json FILE       read a captured /vcs (or /workspace-check) payload
                        instead of invoking the compiler (offline, deterministic)
  --server URL          POST to an already-running server's /vcs (must NOT be
                        the user's live editor server)
  --ocamlc PATH         vox2 ocamlc.opt for live mode (else \$VOX2_OCAMLC);
                        a throwaway server on an ephemeral port is used
  --file NAME           multi-file: show the pane for the unit NAME (filters a
                        file-tagged payload to that active unit)

Output
  --section body|mode|legend|all   which surface to print (default all)
  --json                emit the raw view-model (point query) or map model
  --runs                map: run-length ruler instead of per-column (dense files)
  --compact on|off      proof-pane "compact" toggle (default on, as in the UI)
  --no-color            plain text (no ANSI); also auto-off when not a TTY

Output file (SAVED by default -- the map/pane is a durable artifact)
  (default)             --map -> <file>.panemap.txt next to the source;
                        a point query -> <file>.pane-L<L>C<C>.txt
                        (.json when --json). The written path is printed to stderr.
                        The saved file is plain text (ANSI stripped).
  --out PATH            write to PATH instead of the derived default
  --stdout              also echo the output to stdout
  --no-file             do not write a file (stdout only)

Exit status is non-zero (with a clear message) if the pane cannot be produced;
the tool never fabricates a pane.`;

function parseArgs(argv) {
  const opts = {
    file: null,
    map: false,
    line: null,
    col: null,
    vcsJson: null,
    server: null,
    ocamlc: process.env.VOX2_OCAMLC || null,
    unit: null,
    section: "all",
    json: false,
    runs: false,
    compact: true,
    color: process.stdout.isTTY === true,
    // Output SAVES to a file by default (the map/pane is a durable artifact
    // agents read).  --out overrides the path; --stdout also echoes; --no-file
    // suppresses the file (stdout only).
    out: null,
    stdout: false,
    noFile: false,
  };
  const positional = [];
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    const next = () => argv[++i];
    switch (a) {
      case "-h":
      case "--help":
        opts.help = true;
        break;
      case "--map":
        opts.map = true;
        break;
      case "--line":
        opts.line = Number(next());
        break;
      case "--col":
        opts.col = Number(next());
        break;
      case "--vcs-json":
        opts.vcsJson = next();
        break;
      case "--server":
        opts.server = next();
        break;
      case "--ocamlc":
        opts.ocamlc = next();
        break;
      case "--file":
        opts.unit = next();
        break;
      case "--section":
        opts.section = next();
        break;
      case "--json":
        opts.json = true;
        break;
      case "--runs":
        opts.runs = true;
        break;
      case "--compact":
        opts.compact = next() !== "off";
        break;
      case "--no-color":
        opts.color = false;
        break;
      case "--color":
        opts.color = true;
        break;
      case "--out":
        opts.out = next();
        break;
      case "--stdout":
        opts.stdout = true;
        break;
      case "--no-file":
        opts.noFile = true;
        break;
      default:
        positional.push(a);
    }
  }
  if (positional.length) opts.file = positional[0];
  return opts;
}

function die(message, code) {
  process.stderr.write("voxide-pane: " + message + "\n");
  process.exit(code == null ? 2 : code);
}

// ---------------------------------------------------------------------------
// ANSI colour (stripping it must recover the canonical pane text exactly).
// ---------------------------------------------------------------------------

const ANSI = {
  reset: "\x1b[0m",
  bold: "\x1b[1m",
  dim: "\x1b[2m",
  red: "\x1b[31m",
  green: "\x1b[32m",
  yellow: "\x1b[33m",
  blue: "\x1b[34m",
  magenta: "\x1b[35m",
  cyan: "\x1b[36m",
  gray: "\x1b[90m",
};

function colorizer(enabled) {
  return (code, text) => (enabled ? code + text + ANSI.reset : text);
}

function stripAnsi(s) {
  return s.replace(/\x1b\[[0-9;]*m/g, "");
}

const STATUS_COLOR = {
  proved: ANSI.green,
  disproved: ANSI.red,
  unproved: ANSI.yellow,
  "solver-error": ANSI.yellow,
  failed: ANSI.red,
  unknown: ANSI.magenta,
  context: ANSI.gray,
};

// ---------------------------------------------------------------------------
// Obtaining the /vcs payload
// ---------------------------------------------------------------------------

// Spin up a throwaway server on an ephemeral port (never the user's live
// editor), POST the source to /vcs, and return the payload.  The server is
// killed before returning.
function liveVcsViaServer(source, ocamlc, unit) {
  return new Promise((resolve, reject) => {
    const env = Object.assign({}, process.env);
    if (!env.TMPDIR) env.TMPDIR = "/usr/local/home/jujacobs/tmp";
    const args = ["server.py", "--port", "0"];
    if (ocamlc) args.push("--ocamlc", ocamlc);
    const child = cp.spawn("python3", args, { cwd: ROOT, env });
    let out = "";
    let err = "";
    let settled = false;
    const fail = (e) => {
      if (settled) return;
      settled = true;
      try {
        child.kill();
      } catch (_) {}
      reject(e);
    };
    child.stdout.on("data", (d) => {
      out += String(d);
      const m = /http:\/\/127\.0\.0\.1:(\d+)\//.exec(out);
      if (m && !settled) {
        const port = m[1];
        const body = JSON.stringify({
          source,
          revision: 1,
          path: unit || "buffer.ml",
        });
        fetch("http://127.0.0.1:" + port + "/vcs", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body,
        })
          .then((r) => r.json())
          .then((payload) => {
            settled = true;
            try {
              child.kill();
            } catch (_) {}
            resolve(payload);
          })
          .catch(fail);
      }
    });
    child.stderr.on("data", (d) => {
      err += String(d);
    });
    child.on("error", fail);
    child.on("exit", (code) => {
      if (!settled) {
        fail(
          new Error(
            "throwaway server exited (code " +
              code +
              ")" +
              (err ? ": " + err.trim() : "")
          )
        );
      }
    });
  });
}

// POST to an already-running server's /vcs.
function vcsViaServerUrl(url, source, unit) {
  const base = url.replace(/\/$/, "");
  return fetch(base + "/vcs", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ source, revision: 1, path: unit || "buffer.ml" }),
  }).then((r) => {
    if (!r.ok) throw new Error("server returned HTTP " + r.status);
    return r.json();
  });
}

async function obtainPayload(opts, source) {
  if (opts.vcsJson) {
    let raw;
    try {
      raw = fs.readFileSync(opts.vcsJson, "utf8");
    } catch (e) {
      die("cannot read --vcs-json file: " + e.message);
    }
    try {
      return JSON.parse(raw);
    } catch (e) {
      die("--vcs-json is not valid JSON: " + e.message);
    }
  }
  if (opts.server) return vcsViaServerUrl(opts.server, source, opts.unit);
  if (typeof fetch !== "function") {
    die("live mode needs node >= 18 (global fetch); use --vcs-json instead");
  }
  if (!opts.ocamlc) {
    die(
      "no compiler for live mode: pass --ocamlc PATH or set VOX2_OCAMLC, or use --vcs-json"
    );
  }
  return liveVcsViaServer(source, opts.ocamlc, opts.unit);
}

// Adapt a payload and filter to the active unit (multi-file): only obligations
// tagged for the active unit reach the pane, exactly as the browser filters by
// the active tab.  --file names the active unit explicitly; WITHOUT it, a
// multi-file payload follows its OWN `active` field (never pooling a foreign
// unit's obligations into the pane -- a Client.ml caret must not surface Lib.ml
// obligations).  A single-buffer payload tags no VC with a file and carries no
// `active`, so no filter applies and every VC is used, as before.
function adaptAndFilter(payload, unit) {
  const adapted = model.adaptVcs(payload);
  let vcs = adapted.vcs;
  const active = unit != null ? unit : (payload && payload.active) || null;
  const multiFile = adapted.vcs.some((vc) => vc.file != null);
  if (active != null && multiFile) {
    vcs = vcs.filter((vc) => (vc.file || null) === active);
  }
  return { vcs, unavailable: adapted.unavailable, hidden: adapted.hidden };
}

// ---------------------------------------------------------------------------
// Point-query rendering
// ---------------------------------------------------------------------------

// Colour one readable body segment.  The plain text of the whole body is
// exactly model.paneBodyReadable(vm) -- stripping the ANSI recovers it
// byte-for-byte -- because colour only wraps the (rstripped) segment text.
function colorSeg(seg, c) {
  const t = seg.text.replace(/[ \t]+$/, "");
  if (t === "") return "";
  switch (seg.kind) {
    case "token":
      // The off-obligation grey CONTEXT token ("◦ CONTEXT · approximate", full
      // only) -- the obligation verdict now rides the goal line, so the only
      // remaining token is the context one.
      return c(
        ANSI.bold + (STATUS_COLOR[seg.status] || ANSI.dim),
        t
      );
    case "anchor":
      return c(ANSI.gray, t);
    case "goal":
      // The goal line CARRIES the verdict: coloured by status (green proved /
      // red disproved / amber unproved|solver-error), bold, with the leading
      // glyph and any welded `· no witness` qualifier in the same colour.
      return c(ANSI.bold + (STATUS_COLOR[seg.status] || ""), t);
    case "hyp": {
      const i = t.indexOf(" : ");
      if (i < 0) return t;
      return c(seg.faded ? ANSI.gray : ANSI.cyan, t.slice(0, i)) + t.slice(i);
    }
    case "heading":
      return c(ANSI.bold, t);
    case "note":
      return c(ANSI.yellow, t);
    case "summary":
    case "kind":
    case "also":
    case "placeholder":
      return c(ANSI.dim, t);
    default:
      // cont / raw / detail / lean / cex: verbatim (theorem text, witnesses).
      return t;
  }
}

// Drop leading/trailing blank lines (matching model.normalizeReadable), where a
// blank line is one that is empty after ANSI is stripped.
function normalizeColoredLines(lines) {
  const out = lines.slice();
  const blank = (l) => stripAnsi(l) === "";
  while (out.length && blank(out[0])) out.shift();
  while (out.length && blank(out[out.length - 1])) out.pop();
  return out;
}

// The readable, coloured #pane-body.  Built from the shared model's line
// segments so it stays in lockstep with what the browser renders.
function colorBody(vm, c) {
  const lines = model.paneBodyLines(vm).map((s) => colorSeg(s, c));
  return normalizeColoredLines(lines).join("\n");
}

// The verdict legend, one label per line (colour by status).
function colorLegend(vm, c) {
  if (!vm.legend || !vm.legend.visible) return "";
  return vm.legend.entries
    .map(([status, label]) =>
      STATUS_COLOR[status] ? c(STATUS_COLOR[status], label) : label
    )
    .join("\n");
}

// Render a single section.  The PLAIN text of body/mode/legend equals the
// block-aware, chrome-stripped projection the anti-drift lock checks against
// the rendered DOM.
function renderSection(section, vm, c) {
  if (section === "mode") return model.paneModeText(vm);
  if (section === "legend") return colorLegend(vm, c);
  if (section === "body") return colorBody(vm, c);
  // "all": a decorated, human view (body + verdict legend).  Not byte-compared;
  // the per-section outputs above are the fidelity surface.  (The redesign
  // blanked paneMode, so no "mode:" header is rendered here anymore.)
  const parts = [];
  parts.push(colorBody(vm, c));
  const leg = colorLegend(vm, c);
  if (leg) parts.push(c(ANSI.dim, "verdict legend:") + "\n" + leg);
  return parts.join("\n\n");
}

function runPointQuery(opts, ctx) {
  const line = opts.line - 1;
  const ch = opts.col - 1;
  const vm = model.proofPaneModel(ctx.vcs, { line, ch }, ctx.paneOpts);
  if (opts.json) return JSON.stringify(vm, null, 2);
  const c = colorizer(opts.color);
  return renderSection(opts.section, vm, c);
}

// ---------------------------------------------------------------------------
// Map rendering
// ---------------------------------------------------------------------------

function runMap(opts, ctx) {
  const map = model.buildCursorMap(ctx.vcs, ctx.source, ctx.paneOpts);
  if (opts.json) {
    // Drop the (circular-free but bulky) vm from each legend entry for JSON.
    const slim = {
      legend: map.legend.map((e) => ({
        glyph: e.glyph,
        legendId: e.legendId,
        mode: e.mode,
        body: e.body,
        legendText: e.legendText,
      })),
      lines: map.lines.map((l) => ({
        line: l.line + 1,
        text: l.text,
        ruler: l.ruler,
        remaps: l.remaps,
      })),
    };
    return JSON.stringify(slim, null, 2);
  }
  const c = colorizer(opts.color);
  const out = [];
  const label =
    (opts.file ? path.basename(opts.file) : "buffer") +
    " — cursor→pane map  (compact=" +
    (opts.compact ? "on" : "off") +
    ")";
  out.push(c(ANSI.bold, label));
  out.push("");
  const width = String(map.lines.length).length;
  const gutter = (n) => String(n).padStart(width) + " | ";
  const rulerPad = " ".repeat(width + 3);
  map.lines.forEach((l) => {
    out.push(c(ANSI.gray, gutter(l.line + 1)) + l.text);
    if (opts.runs) {
      out.push(rulerPad + runLengths(l.ruler));
    } else {
      out.push(rulerPad + c(ANSI.cyan, l.ruler));
    }
    if (l.remaps.length) {
      out.push(
        rulerPad +
          c(
            ANSI.dim,
            "(map: " + l.remaps.map((r) => r.glyph + "→" + r.legendId).join(" ") + ")"
          )
      );
    }
  });
  out.push("");
  out.push(c(ANSI.bold, "Legend"));
  map.legend.forEach((e) => {
    out.push(legendEntryText(e, c));
  });
  return out.join("\n");
}

// A compact run-length rendering of a ruler string: `4-16:a 17:·`.
function runLengths(ruler) {
  const parts = [];
  let i = 0;
  while (i < ruler.length) {
    let j = i;
    while (j < ruler.length && ruler[j] === ruler[i]) j++;
    parts.push(j - 1 > i ? i + "-" + (j - 1) + ":" + ruler[i] : i + ":" + ruler[i]);
    i = j;
  }
  return parts.join(" ");
}

// One legend entry: `<id>  <mode> <body text>` with multi-line bodies indented,
// and the verdict legend (if any) noted.  The body text is the canonical pane
// text (what the point query prints), so an agent reads glyph -> this entry
// directly.
function legendEntryText(e, c) {
  const id = e.legendId;
  const head = c(ANSI.bold, id) + "  " + (e.mode ? c(ANSI.dim, "[" + e.mode + "] ") : "");
  const bodyLines = (e.body || "(no pane)").split("\n");
  const indent = "   ";
  const lines = [head + bodyLines[0]];
  for (let i = 1; i < bodyLines.length; i++) lines.push(indent + bodyLines[i]);
  if (e.legendText) lines.push(indent + c(ANSI.dim, "verdict legend: ") + e.legendText);
  return lines.join("\n");
}

// ---------------------------------------------------------------------------
// Output delivery: SAVE to a file by default (durable artifact).
// ---------------------------------------------------------------------------

// The default output path next to the source: <base>.panemap.<ext> for the map,
// <base>.pane-L<L>C<C>.<ext> for a point query.  `base` is the source file, or
// the --vcs-json file with its .vcs.json/.json suffix stripped; null if neither
// exists (then output goes to stdout).
function derivePath(opts, kind) {
  if (opts.out) return opts.out;
  const base = opts.file
    ? opts.file
    : opts.vcsJson
    ? opts.vcsJson.replace(/\.vcs\.json$/i, "").replace(/\.json$/i, "")
    : null;
  if (!base) return null;
  const ext = opts.json ? ".json" : ".txt";
  if (kind === "map") return base + ".panemap" + ext;
  return base + ".pane-L" + opts.line + "C" + opts.col + ext;
}

// Deliver `text`: write it to a file by default (ANSI-stripped, so the saved
// artifact is plain), printing the path to stderr; echo to stdout when --stdout
// (or when no file path can be derived).  --no-file writes only to stdout.
function deliver(opts, text, defaultPath) {
  // Always terminate with exactly one newline (never depend on whether the
  // pane text already ends in one -- generated Lean does), so a consumer that
  // strips a single trailing newline recovers the canonical text exactly.
  const body = text + "\n";
  const target = opts.noFile ? null : defaultPath;
  if (target) {
    try {
      fs.writeFileSync(target, stripAnsi(body));
    } catch (e) {
      die("cannot write output file " + target + ": " + e.message);
    }
    process.stderr.write("voxide-pane: wrote " + target + "\n");
    if (opts.stdout) process.stdout.write(body);
    return;
  }
  if (!opts.noFile && !defaultPath) {
    process.stderr.write(
      "voxide-pane: no output path derivable (no source file / --out); printing to stdout\n"
    );
  }
  process.stdout.write(body);
}

// ---------------------------------------------------------------------------
// main
// ---------------------------------------------------------------------------

async function main() {
  const opts = parseArgs(process.argv.slice(2));
  if (opts.help) {
    process.stdout.write(HELP + "\n");
    return;
  }
  const needSource = opts.map || !opts.vcsJson;
  let source = "";
  if (opts.file) {
    try {
      source = fs.readFileSync(opts.file, "utf8");
    } catch (e) {
      die("cannot read source file: " + e.message);
    }
  } else if (needSource) {
    die("a source <file.ml> is required (for --map, and for live mode)");
  }

  let payload;
  try {
    payload = await obtainPayload(opts, source);
  } catch (e) {
    die("could not obtain verification data: " + (e && e.message ? e.message : e));
  }
  if (payload && payload.error) {
    die("server error: " + payload.error);
  }

  const adapted = adaptAndFilter(payload, opts.unit);
  const ctx = {
    vcs: adapted.vcs,
    source,
    paneOpts: {
      compact: opts.compact,
      fadeUnused: true,
      unavailable: adapted.unavailable,
      hidden: adapted.hidden,
    },
  };

  if (opts.map) {
    deliver(opts, runMap(opts, ctx), derivePath(opts, "map"));
    return;
  }
  if (opts.line != null || opts.col != null) {
    if (!(opts.line >= 1) || !(opts.col >= 1)) {
      die("--line and --col are 1-based and required for a point query");
    }
    deliver(opts, runPointQuery(opts, ctx), derivePath(opts, "point"));
    return;
  }
  die("choose a mode: --map (whole file) or --line L --col C (point query)");
}

main().catch((e) => {
  die(e && e.stack ? e.stack : String(e));
});
