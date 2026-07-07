#!/usr/bin/env python3
"""Layer 3: the vox-editor HTTP server (stdlib only).

Endpoints:

  POST /check {source, revision, fast?}
      Compile the source and return a unified list of REGIONS (0-based
      line/col), the errors, and the generated Lean.  A region is a
      verification condition (with goal/hypotheses/status/counterexample),
      a static block theorem (binders as hypotheses, stated goal), or a
      whole [%%vox.lean] block (for routing/highlight).  The client picks
      the innermost enclosing region as the cursor moves — no server call
      per cursor move.  With fast:true the Lean solve is skipped: the
      ~20ms dry-run compile still yields every VC's goal/hypotheses/spans
      and all elaboration errors, so the client can refresh the pane as
      the user types (statuses come back "unknown"; the client carries
      verdicts over by content until the next full check).

  POST /goal {source, line, col, revision}
      Live proof state at a 0-based cursor inside a block, via the Lean
      LSP (slow; used on an explicit action, not per cursor move).

  GET /            index.html
  GET /<file>      static assets (app.js, selection.js, style.css, vendor)

vc_index reports 1-based lines; everything crossing this boundary is
normalised to 0-based line + 0-based col (CodeMirror convention).
"""

import json
import os
import socketserver
import tempfile
import urllib.parse
from http.server import BaseHTTPRequestHandler, HTTPServer
from typing import Any, Dict, List, Optional, Tuple, cast

import lean_bridge  # pyright: ignore[reportImplicitRelativeImport]
import vc_index  # pyright: ignore[reportImplicitRelativeImport]

HERE = os.path.dirname(os.path.abspath(__file__))
EXAMPLES_DIR = os.path.join(HERE, "examples")

_CONTENT_TYPES = {
    ".html": "text/html; charset=utf-8",
    ".js": "application/javascript; charset=utf-8",
    ".css": "text/css; charset=utf-8",
    ".json": "application/json; charset=utf-8",
}


def _loc0(loc: Dict[str, int]) -> Dict[str, int]:
    """Convert a vc_index location (1-based line, 0-based col) to 0-based."""
    return {"line": loc["line"] - 1, "col": loc["col"]}


def _as_int(v: object, default: int = 0) -> int:
    try:
        return int(str(v))
    except (TypeError, ValueError):
        return default


def build_check_response(
    source: str,
    revision: int,
    ocamlc: str,
    lean: Optional[str],
) -> Dict[str, object]:
    """Compile ``source`` and produce the /check payload."""
    scratch = tempfile.mkdtemp(prefix="voxeditor")
    path = os.path.join(scratch, "input.ml")
    with open(path, "w") as fh:
        fh.write(source)
    index = vc_index.build_index(path, ocamlc, lean=lean, cwd=scratch)
    regions: List[Dict[str, object]] = []
    for vc in cast(List[Dict[str, Any]], index["vcs"]):
        region: Dict[str, object] = {
            "kind": "vc",
            "start": _loc0(vc["start"]),
            "end": _loc0(vc["end"]),
            "goal": vc["goal"],
            "hypotheses": vc["hypotheses"],
            "status": vc["status"],
            "vckind": vc["kind"],
        }
        # Provenance spans (from -vox-dump-vc-provenance): the goal's origin
        # and one per hypothesis, parallel to ``hypotheses`` (each may be
        # null when the compiler had no meaningful span, or absent entirely
        # under an old compiler).  Unlike the region's own start/end these
        # are passed through in the compiler's native 1-based-line /
        # 0-based-col convention; the client's markFromSpan converts to
        # CodeMirror's 0-based lines at the single point of use.
        region["goal_span"] = vc.get("goal_span")
        region["hyp_spans"] = vc.get("hyp_spans", [])
        # The VC's variables with OxCaml type + Lean sort (the pane's
        # context section); empty under an old compiler.
        region["scope"] = vc.get("scope", [])
        region["module_hypotheses"] = vc.get("module_hypotheses", [])
        region["module_hyp_spans"] = vc.get("module_hyp_spans", [])
        # The lemmas grind used to close this VC (-vox-explain-proofs);
        # None under an old compiler or without a solver.
        region["used"] = vc.get("used")
        # The hypotheses grind did not reference in the proof it found
        # (-vox-explain-proofs), and the parallel per-hypothesis used-flag
        # the pane fades on.  unused_hyps is None under an old compiler /
        # without a solver; hyp_used defaults to all-true then.
        region["unused_hyps"] = vc.get("unused_hyps")
        region["hyp_used"] = vc.get("hyp_used", [])
        if "counterexample" in vc:
            region["counterexample"] = vc["counterexample"]
        if "lean_msg" in vc:
            region["lean_msg"] = vc["lean_msg"]
        regions.append(region)
    # Block outlines (whole-block regions) and static theorem regions.
    for b in lean_bridge.find_lean_blocks(source):
        sline, scol = lean_bridge.offset_to_linecol(source, b.content_offset)
        eline, ecol = lean_bridge.offset_to_linecol(
            source, b.content_offset + len(b.content)
        )
        regions.append(
            {
                "kind": "block",
                "start": {"line": sline, "col": scol},
                "end": {"line": eline, "col": ecol},
            }
        )
    for t in lean_bridge.theorems_in_source(source):
        regions.append(
            {
                "kind": "theorem",
                "start": t["start"],
                "end": t["end"],
                "name": t["name"],
                "hypotheses": t["hypotheses"],
                "goal": t["goal"],
            }
        )
    generated = None
    if lean is not None:
        gen = lean_bridge.capture_generated(path, ocamlc, lean, cwd=scratch)
        if gen is not None:
            generated = lean_bridge.to_self_contained(gen)
    # Expression types from -annot, for type-at-cursor: converted to the
    # client's 0-based lines here (cols are already 0-based).
    types = [
        {
            "start": _loc0(cast(Dict[str, int], t["start"])),
            "end": _loc0(cast(Dict[str, int], t["end"])),
            "type": t["type"],
        }
        for t in cast(List[Dict[str, Any]], index.get("types", []))
    ]
    # Program-point states (facts + scope at every walked expression's
    # entry): start/end to 0-based; hyp/scope spans stay in the
    # compiler's convention like the regions' provenance spans.
    states = [
        {
            "start": _loc0(cast(Dict[str, int], st["start"])),
            "end": _loc0(cast(Dict[str, int], st["end"])),
            "hypotheses": st["hypotheses"],
            "hyp_spans": st["hyp_spans"],
            "module_hypotheses": st.get("module_hypotheses", []),
            "module_hyp_spans": st.get("module_hyp_spans", []),
            "scope": st["scope"],
        }
        for st in cast(List[Dict[str, Any]], index.get("states", []))
    ]
    return {
        "revision": revision,
        "ok": index["ok"],
        "regions": regions,
        "states": states,
        "errors": [_error0(e) for e in cast(List[Dict[str, object]], index["errors"])],
        "generated_lean": generated,
        "types": types,
    }


def _error0(err: Dict[str, object]) -> Dict[str, object]:
    out = dict(err)
    if "start" in err and isinstance(err["start"], dict):
        out["start"] = _loc0(err["start"])
    if "end" in err and isinstance(err["end"], dict):
        out["end"] = _loc0(err["end"])
    return out


def build_goal_response(
    source: str,
    line: int,
    col: int,
    revision: int,
    ocamlc: str,
    lean: Optional[str],
) -> Dict[str, object]:
    """Live proof state at a 0-based cursor (must be inside a block)."""
    if lean is None:
        return {
            "revision": revision,
            "status": "unsupported",
            "goals": [],
            "detail": "server started without a Lean solver",
        }
    scratch = tempfile.mkdtemp(prefix="voxeditorgoal")
    path = os.path.join(scratch, "input.ml")
    with open(path, "w") as fh:
        fh.write(source)
    bg = lean_bridge.goal_at_source_pos(path, ocamlc, lean, line, col, cwd=scratch)
    out = bg.to_json()
    out["revision"] = revision
    return out


def find_ocamlc() -> Optional[str]:
    env = os.environ.get("VOX_OCAMLC")
    if env and os.path.exists(env):
        return env
    root = os.path.dirname(os.path.dirname(HERE))
    cand = os.path.join(root, "_build", "_bootinstall", "bin", "ocamlc.opt")
    return cand if os.path.exists(cand) else None


def find_lean() -> Optional[str]:
    env = os.environ.get("VOX_LEAN")
    if env and os.path.exists(env):
        return env
    pinned = "/nix/store/h6z4nr52r2x6v7ygqg59cl8nzjg0yxcy-lean4-4.31.0/bin/lean"
    return pinned if os.path.exists(pinned) else None


class Handler(BaseHTTPRequestHandler):
    ocamlc = ""
    lean: Optional[str] = None

    def log_message(self, format: str, *args: Any) -> None:
        pass  # quiet

    def _json(self, code: int, obj: Dict[str, object]) -> None:
        body = json.dumps(obj).encode("utf-8")
        self.send_response(code)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def _read_body(self) -> Dict[str, object]:
        length = int(self.headers.get("Content-Length", "0"))
        raw = self.rfile.read(length) if length else b"{}"
        parsed = json.loads(raw.decode("utf-8"))
        return parsed if isinstance(parsed, dict) else {}

    def _endpoint(self) -> str:
        # A proxy in the environment makes clients send the absolute-form
        # request URI; take just the path component either way.
        return urllib.parse.urlsplit(self.path).path

    def do_POST(self) -> None:
        try:
            body = self._read_body()
        except Exception as e:
            self._json(400, {"error": "bad JSON: %s" % e})
            return
        source = str(body.get("source", ""))
        revision = _as_int(body.get("revision", 0))
        endpoint = self._endpoint()
        if endpoint == "/check":
            fast = bool(body.get("fast", False))
            resp = build_check_response(
                source, revision, self.ocamlc, None if fast else self.lean
            )
            resp["fast"] = fast
            self._json(200, resp)
        elif endpoint == "/goal":
            line = _as_int(body.get("line", 0))
            col = _as_int(body.get("col", 0))
            self._json(
                200,
                build_goal_response(
                    source, line, col, revision, self.ocamlc, self.lean
                ),
            )
        else:
            self._json(404, {"error": "no such endpoint"})

    def _send_file(self, target: str, ctype: str) -> None:
        with open(target, "rb") as fh:
            data = fh.read()
        self.send_response(200)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(data)))
        # The assets are a live dev tool served through a proxy: without
        # this, browsers cache app.js and users keep running last week's
        # client after a plain reload.
        self.send_header("Cache-Control", "no-cache")
        self.end_headers()
        self.wfile.write(data)

    def _serve_example(self, name: str) -> None:
        """Serve a single curated example source by its index name.

        Uses the same no-traversal discipline as the static assets: the
        resolved path must stay within the examples dir."""
        target = os.path.normpath(os.path.join(EXAMPLES_DIR, name + ".ml"))
        if not target.startswith(EXAMPLES_DIR + os.sep) or not os.path.isfile(target):
            self._json(404, {"error": "no such example"})
            return
        self._send_file(target, "text/plain; charset=utf-8")

    def do_GET(self) -> None:
        path = self._endpoint()
        if path == "/favicon.ico":
            self.send_response(204)
            self.end_headers()
            return
        # Curated examples: /examples is the index, /examples/<name> the
        # source of one example (see make_examples.py).
        if path == "/examples":
            index = os.path.join(EXAMPLES_DIR, "index.json")
            if not os.path.isfile(index):
                self._json(404, {"error": "no examples index"})
                return
            self._send_file(index, "application/json; charset=utf-8")
            return
        if path.startswith("/examples/"):
            self._serve_example(path[len("/examples/") :])
            return
        if path == "/":
            path = "/index.html"
        # Restrict to files in the tools dir (no traversal).
        rel = path.lstrip("/")
        target = os.path.normpath(os.path.join(HERE, rel))
        if not target.startswith(HERE) or not os.path.isfile(target):
            self._json(404, {"error": "not found"})
            return
        ext = os.path.splitext(target)[1]
        ctype = _CONTENT_TYPES.get(ext, "application/octet-stream")
        self._send_file(target, ctype)


class ThreadingHTTPServer(socketserver.ThreadingMixIn, HTTPServer):
    daemon_threads = True


def make_server(
    port: int, ocamlc: str, lean: Optional[str]
) -> Tuple[ThreadingHTTPServer, int]:
    Handler.ocamlc = ocamlc
    Handler.lean = lean
    httpd = ThreadingHTTPServer(("127.0.0.1", port), Handler)
    return httpd, httpd.server_address[1]


def main() -> None:
    import argparse

    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=8000)
    ap.add_argument("--ocamlc", default=None)
    ap.add_argument("--lean", default=None)
    ap.add_argument(
        "--no-lean",
        action="store_true",
        help="disable the Lean solver (VC shapes only)",
    )
    args = ap.parse_args()
    ocamlc = args.ocamlc or find_ocamlc()
    if ocamlc is None:
        raise SystemExit("no ocamlc found; set --ocamlc or VOX_OCAMLC")
    lean = None if args.no_lean else (args.lean or find_lean())
    httpd, port = make_server(args.port, ocamlc, lean)
    print(
        "vox-editor on http://127.0.0.1:%d  (ocamlc=%s, lean=%s)"
        % (port, ocamlc, lean or "<none>"),
        flush=True,
    )
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        httpd.shutdown()


if __name__ == "__main__":
    main()
