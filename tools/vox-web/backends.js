// vox-web check backends.
//
// A CheckBackend turns { source, revision } into the SAME `/check`
// JSON the server-based editor's app.js already consumes
// (regions/vcs/spans/errors/generated_lean), and { source,line,col }
// into the `/goal` JSON.  The whole point of vox-web is that this seam
// is pluggable: the front end and worker never know whether the answer
// came from a server, a canned fixture, or an in-browser jsoo compiler.
//
// Three implementations:
//   RemoteBackend    — POSTs to an HTTP endpoint (the existing server.py
//                      or any /check+/goal host).  Works today.
//   MockBackend      — returns a deterministic canned response with NO
//                      network at all, so the entire UI runs fully
//                      static/offline.  This is the part that genuinely
//                      "works in the browser with zero server".
//   InBrowserBackend — the Tier-2 target: elaborate via a jsoo'd
//                      ocamlc worker (no Lean), proofs via a small
//                      remote endpoint.  Stubbed; see NOTES.md for why
//                      it can't be realized in this environment and
//                      exactly what goes here.
//
// Loadable three ways: Web Worker (importScripts -> self.VoxBackends),
// Node (module.exports, for tests), and a plain <script> (window).

(function (root) {
  "use strict";

  // ---- MockBackend --------------------------------------------------
  // A canned /check for a two-line program with one proved VC and one
  // failed VC, plus a canned /goal.  Shapes match server.py exactly
  // (0-based line+col; regions with kind "vc"; status proved/failed).
  const MOCK_CHECK = {
    ok: false,
    regions: [
      {
        kind: "vc",
        start: { line: 1, col: 2 },
        end: { line: 1, col: 20 },
        goal: "0 <= i",
        hypotheses: ["n = len a", "0 <= n"],
        status: "proved",
        vckind: "prove",
        goal_span: { start: { line: 2, col: 2 }, end: { line: 2, col: 20 } },
        hyp_spans: [null, null],
      },
      {
        kind: "vc",
        start: { line: 3, col: 2 },
        end: { line: 3, col: 24 },
        goal: "i < len a",
        hypotheses: ["i = n"],
        status: "failed",
        vckind: "prove",
        counterexample: ["i = 5", "n = 5", "len a = 5"],
        goal_span: { start: { line: 4, col: 2 }, end: { line: 4, col: 24 } },
        hyp_spans: [null],
      },
    ],
    errors: [
      {
        message: "vox: could not prove i < len a",
        start: { line: 3, col: 2 },
        end: { line: 3, col: 24 },
        goal: "i < len a",
        hypotheses: ["i = n"],
        counterexample: ["i = 5", "n = 5", "len a = 5"],
      },
    ],
    generated_lean: "-- (mock) generated Lean would appear here\nexample : True := trivial\n",
  };

  const MOCK_GOAL = {
    status: "ok",
    goals: ["⊢ i < len a"],
    detail: "(mock) live proof state",
  };

  class MockBackend {
    constructor() {
      this.name = "mock";
    }
    async check(source, revision) {
      return Object.assign({ revision }, MOCK_CHECK);
    }
    async goal(source, line, col, revision) {
      return Object.assign({ revision }, MOCK_GOAL);
    }
  }

  // ---- RemoteBackend ------------------------------------------------
  class RemoteBackend {
    constructor(opts) {
      this.name = "remote";
      this.baseUrl = (opts && opts.baseUrl) || "";
      // Injectable for Node tests; defaults to global fetch in browser.
      this._fetch = (opts && opts.fetch) || (typeof fetch !== "undefined" ? fetch : null);
    }
    async _post(path, body) {
      if (!this._fetch) throw new Error("no fetch available");
      const resp = await this._fetch(this.baseUrl + path, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(body),
      });
      return await resp.json();
    }
    async check(source, revision) {
      return this._post("/check", { source, revision });
    }
    async goal(source, line, col, revision) {
      return this._post("/goal", { source, line, col, revision });
    }
  }

  // ---- InBrowserBackend (Tier-2 target; stub) -----------------------
  // Replace the throws with:
  //   check:  run jsoo'd `ocamlc -vox-dump-vc-provenance -vox-dry-run`
  //           in a Worker over a virtual FS of stdlib .cmi's (NO Lean),
  //           parse VC shapes (port of vc_index.parse_dump), then fetch
  //           proof verdicts from a small remote /prove endpoint and
  //           merge pass/fail/counterexample into the regions.
  //   goal:   remote (Lean LSP) only.
  // See tools/vox-web/NOTES.md "What WOULD work" for the full plan and
  // why it is not realizable in this environment (no jsoo, no Lean-wasm).
  class InBrowserBackend {
    constructor() {
      this.name = "in-browser";
    }
    async check() {
      throw new Error(
        "InBrowserBackend not available: needs a jsoo-compiled ocamlc " +
          "(js_of_ocaml is not installable here) — see NOTES.md"
      );
    }
    async goal() {
      throw new Error("InBrowserBackend.goal needs a remote Lean endpoint — see NOTES.md");
    }
  }

  function make(kind, opts) {
    switch (kind) {
      case "mock":
        return new MockBackend();
      case "remote":
        return new RemoteBackend(opts);
      case "in-browser":
        return new InBrowserBackend();
      default:
        throw new Error("unknown backend: " + kind);
    }
  }

  const api = { MockBackend, RemoteBackend, InBrowserBackend, make, MOCK_CHECK, MOCK_GOAL };
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  root.VoxBackends = api;
})(typeof self !== "undefined" ? self : this);
