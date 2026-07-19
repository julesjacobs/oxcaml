#!/usr/bin/env python3
"""Local stdlib-only HTTP server for the vox2 browser IDE."""

import argparse
import inspect
import json
import os
import select
import socket
import socketserver
import sys
import threading
import urllib.parse
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Sequence, Tuple

import compiler as compiler_adapter  # pyright: ignore[reportImplicitRelativeImport]
import workspace  # pyright: ignore[reportImplicitRelativeImport]


HERE = Path(__file__).resolve().parent
MAX_SOURCE_BYTES = 1_000_000
STATIC_FILES = frozenset(
    ("index.html", "app.js", "pane_model.js", "style.css", "vox-mode.js")
)
VENDOR = HERE / "vendor"

CONTENT_TYPES = {
    ".html": "text/html; charset=utf-8",
    ".js": "application/javascript; charset=utf-8",
    ".css": "text/css; charset=utf-8",
    ".json": "application/json; charset=utf-8",
}

CheckFunction = Callable[[str, int, str, str], Dict[str, Any]]
VcsFunction = Callable[[str, int, str, str], Dict[str, Any]]
WorkspaceFunction = Callable[[Any, str, int, str, str], Dict[str, Any]]
SignatureFunction = Callable[[str, int, str, str], Dict[str, Any]]
WorkspaceSignatureFunction = Callable[[Any, str, int, str, str], Dict[str, Any]]

# A workspace request carries several buffers; cap the count defensively (the
# per-request body is already bounded by MAX_SOURCE_BYTES).  Slice 1 sends a
# fixed three-file set, well under this.
MAX_WORKSPACE_FILES = 32

# Product default for a fresh page. Keep this decision in one obvious place:
# preferred_backend applies capability/configuration fallbacks before exposing
# it to the client.
DEFAULT_BACKEND = "oxsmt"


def _as_int(value: object, default: int = 0) -> int:
    try:
        return int(str(value))
    except (TypeError, ValueError):
        return default


def _decode_json(raw: bytes) -> Dict[str, Any]:
    value = json.loads(raw.decode("utf-8"))
    if not isinstance(value, dict):
        raise ValueError("JSON body must be an object")
    return value


def preferred_backend(
    options: Sequence[str], solver_configuration: Dict[str, bool]
) -> str:
    """Choose a usable initial backend, degrading old/unconfigured setups."""
    if (
        DEFAULT_BACKEND in options
        and solver_configuration.get(DEFAULT_BACKEND) is True
    ):
        return DEFAULT_BACKEND
    if "lean" in options:
        return "lean"
    return options[0] if options else "lean"


def backend_configuration(
    ocamlc: str, available_backends: Optional[Sequence[str]] = None
) -> Dict[str, Any]:
    options = tuple(
        available_backends or compiler_adapter.backend_options(ocamlc)
    )
    solver_configuration = compiler_adapter.backend_solver_configuration(ocamlc)
    return {
        "backend_options": list(options),
        "backend_solver_configuration": solver_configuration,
        "default_backend": preferred_backend(options, solver_configuration),
    }


def _validate_workspace_files(value: object) -> Optional[List[Dict[str, Any]]]:
    """Validate the ``files`` array of a workspace request.

    Returns the normalized ``[{name, source}]`` list, or ``None`` if the shape
    is wrong (too many files, a non-object entry, a non-string name/source, or
    a source with a lone surrogate).  Filename *safety* (basename, extension,
    traversal) is enforced downstream in ``check_workspace``; this guards only
    the request envelope so the server stays a total function of its input.
    """
    if not isinstance(value, list) or not value or len(value) > MAX_WORKSPACE_FILES:
        return None
    files = []
    for entry in value:
        if not isinstance(entry, dict):
            return None
        name = entry.get("name")
        source = entry.get("source", "")
        if not isinstance(name, str) or not isinstance(source, str):
            return None
        try:
            source.encode("utf-8")
        except UnicodeEncodeError:
            return None
        files.append({"name": name, "source": source})
    return files


def _call_compiler(
    function: Callable[..., Dict[str, Any]],
    arguments: Sequence[Any],
    cancel_check: Optional[Callable[[], bool]],
) -> Dict[str, Any]:
    """Pass cancellation only across adapter boundaries that declare it."""
    try:
        supports_cancel = "cancel_check" in inspect.signature(function).parameters
    except (TypeError, ValueError):
        supports_cancel = False
    if cancel_check is not None and supports_cancel:
        return function(*arguments, cancel_check=cancel_check)
    return function(*arguments)


class CancellationLane:
    """One latest-revision-wins compiler lane shared by request threads."""

    def __init__(self) -> None:
        self.lock = threading.Lock()
        self.active: Optional[threading.Event] = None
        self.revision = -1

    def begin(self, revision: int) -> threading.Event:
        token = threading.Event()
        with self.lock:
            if self.active is not None and revision < self.revision:
                token.set()
            else:
                previous = self.active
                self.active = token
                self.revision = revision
                if previous is not None:
                    previous.set()
        return token

    def end(self, token: threading.Event) -> None:
        with self.lock:
            if self.active is token:
                self.active = None
                self.revision = -1


def process_post(
    endpoint: str,
    raw: bytes,
    checker: CheckFunction,
    ocamlc: str,
    vcs_provider: VcsFunction,
    workspace_checker: WorkspaceFunction = compiler_adapter.check_workspace,
    available_backends: Optional[Sequence[str]] = None,
    signature_checker: SignatureFunction = compiler_adapter.signature_for_source,
    workspace_signature_checker: WorkspaceSignatureFunction = (
        compiler_adapter.signature_for_workspace
    ),
    cancel_check: Optional[Callable[[], bool]] = None,
) -> Tuple[int, Dict[str, Any]]:
    """Turn one POST body into a status and JSON payload without I/O."""
    try:
        body = _decode_json(raw)
    except (
        UnicodeDecodeError,
        json.JSONDecodeError,
        RecursionError,
        ValueError,
    ) as exc:
        return 400, {"error": f"bad request: {exc}"}

    revision = _as_int(body.get("revision", 0))
    options = tuple(
        available_backends or compiler_adapter.backend_options(ocamlc)
    )
    solver_configuration = compiler_adapter.backend_solver_configuration(ocamlc)
    backend = body.get("backend", "lean")
    if not isinstance(backend, str) or backend not in compiler_adapter.BACKENDS:
        return 400, {"error": "backend must be lean, z3, oxsmt, or cross"}
    if backend not in options:
        return 400, {"error": f"compiler does not support backend {backend}"}
    # /workspace-check compiles a set of buffers together (multi-file /
    # module-context checking): one authoritative -c pass over the whole set,
    # returning per-file diagnostics and file-tagged VCs.  The client owns the
    # buffer set and sends it in full each time, so the server stays stateless.
    if endpoint in ("/workspace-check", "/workspace-signature"):
        files = _validate_workspace_files(body.get("files"))
        if files is None:
            return 400, {"error": "files must be a non-empty array of {name, source}"}
        active = body.get("active", "")
        if not isinstance(active, str):
            return 400, {"error": "active must be a string"}
        try:
            function = (
                workspace_checker
                if endpoint == "/workspace-check"
                else workspace_signature_checker
            )
            payload = _call_compiler(
                function,
                (files, active, revision, ocamlc, backend),
                cancel_check,
            )
            payload.setdefault("backend", backend)
            payload.setdefault("backend_options", list(options))
            payload["backend_solver_configuration"] = solver_configuration
            return 200, payload
        except compiler_adapter.CompileCancelled:
            return 499, {"error": "request cancelled"}
        except Exception:
            return 500, {"error": "internal compiler service error"}

    if endpoint not in ("/check", "/verify", "/vcs", "/signature"):
        return 404, {"error": "no such endpoint"}

    source = body.get("source", "")
    if not isinstance(source, str):
        return 400, {"error": "source must be a string"}
    try:
        source.encode("utf-8")
    except UnicodeEncodeError:
        return 400, {"error": "source must contain only Unicode scalar values"}

    # /vcs is the per-obligation dump that feeds the cursor-following proof
    # pane and the per-VC source marks.  It runs one verification-capable
    # compile of the live buffer (-vox-dump-vc-json) and translates the real
    # schema-v1 document into the flat shape the frontend adapter consumes.
    # An empty buffer, or any run/parse failure, degrades to an empty list.
    if endpoint == "/vcs":
        try:
            payload = vcs_provider(source, revision, ocamlc, backend)
            payload.setdefault("backend", backend)
            payload.setdefault("backend_options", list(options))
            payload["backend_solver_configuration"] = solver_configuration
            return 200, payload
        except Exception:
            return 500, {"error": "internal compiler service error"}

    if endpoint == "/signature":
        try:
            payload = _call_compiler(
                signature_checker,
                (source, revision, ocamlc, backend),
                cancel_check,
            )
            payload.setdefault("backend", backend)
            return 200, payload
        except compiler_adapter.CompileCancelled:
            return 499, {"error": "request cancelled"}
        except Exception:
            return 500, {"error": "internal compiler service error"}

    try:
        result = _call_compiler(
            checker, (source, revision, ocamlc, backend), cancel_check
        )
    except compiler_adapter.CompileCancelled:
        return 499, {"error": "request cancelled"}
    except Exception:
        return 500, {"error": "internal compiler service error"}

    result.setdefault("backend", backend)
    result.setdefault("backend_options", list(options))
    result["backend_solver_configuration"] = solver_configuration
    if endpoint == "/check":
        return 200, result
    # Backward-compatible transport projection for old clients.  The current
    # editor has no Verify action and never calls this endpoint: /check already
    # returns this outcome together with diagnostics, cursor data, and VCs.
    verification = result.get("verification") or {
        "status": "none",
        "message": "",
        "obligations": False,
    }
    verify_errors = [
        error
        for error in result.get("errors", [])
        if error.get("kind") == "verification"
    ]
    return 200, {
        "revision": result.get("revision", revision),
        "ok": bool(result.get("ok")),
        "verification": verification,
        "errors": verify_errors,
        "backend": backend,
        "backend_options": list(options),
        "backend_solver_configuration": solver_configuration,
    }


def resolve_static_path(endpoint: str) -> Optional[Path]:
    """Resolve an allowlisted frontend asset, or return None."""
    try:
        relative = urllib.parse.unquote(endpoint, errors="strict").lstrip("/")
        parts = Path(relative).parts
        if relative in STATIC_FILES:
            allowed_root = HERE
        elif parts and parts[0] == "vendor":
            allowed_root = VENDOR
        else:
            return None
        target = (HERE / relative).resolve()
        inside = os.path.commonpath((str(allowed_root), str(target))) == str(
            allowed_root
        )
        if inside and target.is_file():
            return target
    except (OSError, UnicodeError, ValueError):
        pass
    return None


class Handler(BaseHTTPRequestHandler):
    ocamlc = ""
    checker: CheckFunction = staticmethod(compiler_adapter.check_source)
    vcs_provider: VcsFunction = staticmethod(compiler_adapter.vcs_for_source)
    workspace_checker: WorkspaceFunction = staticmethod(
        compiler_adapter.check_workspace
    )
    signature_checker: SignatureFunction = staticmethod(
        compiler_adapter.signature_for_source
    )
    workspace_signature_checker: WorkspaceSignatureFunction = staticmethod(
        compiler_adapter.signature_for_workspace
    )
    available_backends: Sequence[str] = ("lean",)
    cancellation_lane = CancellationLane()

    def log_message(self, format: str, *args: Any) -> None:
        pass

    def _endpoint(self) -> str:
        # Absolute-form request targets can appear when a proxy is configured.
        return urllib.parse.urlsplit(self.path).path

    def _json(self, status: int, payload: Dict[str, Any]) -> None:
        data = json.dumps(payload).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(data)))
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        try:
            self.wfile.write(data)
        except (BrokenPipeError, ConnectionResetError):
            pass

    def _client_disconnected(self) -> bool:
        """Non-blocking peer-close probe used while a compiler is running."""
        try:
            readable, _, _ = select.select([self.connection], [], [], 0)
            if not readable:
                return False
            return (
                self.connection.recv(
                    1, socket.MSG_PEEK | socket.MSG_DONTWAIT
                )
                == b""
            )
        except (BlockingIOError, InterruptedError):
            return False
        except OSError:
            return True

    def _read_body(self) -> bytes:
        length = _as_int(self.headers.get("Content-Length", "0"))
        if length < 0 or length > MAX_SOURCE_BYTES:
            raise ValueError("request body is too large")
        return self.rfile.read(length) if length else b"{}"

    def do_POST(self) -> None:
        try:
            endpoint = self._endpoint()
            raw = self._read_body()
        except ValueError as exc:
            self._json(400, {"error": f"bad request: {exc}"})
            return
        except Exception:
            self._json(500, {"error": "internal server error"})
            return
        cancellable = endpoint in {
            "/check",
            "/verify",
            "/signature",
            "/workspace-check",
            "/workspace-signature",
        }
        token = None
        if cancellable:
            try:
                request_revision = _as_int(_decode_json(raw).get("revision", 0))
            except Exception:
                request_revision = 0
            token = self.cancellation_lane.begin(request_revision)
        try:
            try:
                status, payload = process_post(
                    endpoint,
                    raw,
                    self.checker,
                    self.ocamlc,
                    self.vcs_provider,
                    self.workspace_checker,
                    self.available_backends,
                    self.signature_checker,
                    self.workspace_signature_checker,
                    (
                        lambda: token.is_set() or self._client_disconnected()
                        if token is not None
                        else None
                    ),
                )
            except Exception:
                status = 500
                payload = {"error": "internal server error"}
        finally:
            if token is not None:
                self.cancellation_lane.end(token)
        self._json(status, payload)

    def _send_file(self, path: Path, content_type: str) -> None:
        data = path.read_bytes()
        self.send_response(200)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(data)))
        self.send_header("Cache-Control", "no-cache")
        self.end_headers()
        self.wfile.write(data)

    def do_GET(self) -> None:
        try:
            endpoint = self._endpoint()
        except (UnicodeError, ValueError):
            self._json(404, {"error": "not found"})
            return
        if endpoint == "/favicon.ico":
            self.send_response(204)
            self.end_headers()
            return
        # File explorer (slice 3): /ls is the allowlisted read-only tree of
        # the curated examples root; /file?path=<id> serves one file's text
        # after the same traversal check (workspace.resolve); /examples is
        # the curated index (titles / expected verification / teaching
        # cursor lines) the picker reads.
        if endpoint == "/ls":
            self._json(200, workspace.list_tree())
            return
        if endpoint == "/file":
            query = urllib.parse.parse_qs(urllib.parse.urlsplit(self.path).query)
            path_id = (query.get("path") or [""])[0]
            target = workspace.resolve(path_id)
            if target is None:
                self._json(404, {"error": "no such file"})
                return
            self._send_file(Path(target), "text/plain; charset=utf-8")
            return
        if endpoint == "/examples":
            index = workspace.examples_index_path()
            if index is None:
                self._json(404, {"error": "no examples index"})
                return
            self._send_file(Path(index), "application/json; charset=utf-8")
            return
        if endpoint == "/config":
            self._json(
                200,
                backend_configuration(self.ocamlc, self.available_backends),
            )
            return
        if endpoint == "/":
            endpoint = "/index.html"
        target = resolve_static_path(endpoint)
        if target is None:
            self._json(404, {"error": "not found"})
            return
        content_type = CONTENT_TYPES.get(
            target.suffix.lower(), "application/octet-stream"
        )
        self._send_file(target, content_type)


class ThreadingHTTPServer(socketserver.ThreadingMixIn, HTTPServer):
    daemon_threads = True


def make_server(
    port: int,
    ocamlc: str,
    checker: Optional[CheckFunction] = None,
    vcs_provider: Optional[VcsFunction] = None,
    workspace_checker: Optional[WorkspaceFunction] = None,
    available_backends: Optional[Sequence[str]] = None,
    signature_checker: Optional[SignatureFunction] = None,
    workspace_signature_checker: Optional[WorkspaceSignatureFunction] = None,
) -> Tuple[ThreadingHTTPServer, int]:
    options = tuple(available_backends or compiler_adapter.backend_options(ocamlc))
    configured = type(
        "ConfiguredHandler",
        (Handler,),
        {
            "ocamlc": ocamlc,
            "checker": staticmethod(checker or compiler_adapter.check_source),
            "vcs_provider": staticmethod(
                vcs_provider or compiler_adapter.vcs_for_source
            ),
            "workspace_checker": staticmethod(
                workspace_checker or compiler_adapter.check_workspace
            ),
            "signature_checker": staticmethod(
                signature_checker or compiler_adapter.signature_for_source
            ),
            "workspace_signature_checker": staticmethod(
                workspace_signature_checker
                or compiler_adapter.signature_for_workspace
            ),
            "available_backends": options,
            "cancellation_lane": CancellationLane(),
        },
    )
    httpd = ThreadingHTTPServer(("127.0.0.1", port), configured)
    return httpd, int(httpd.server_address[1])


def one_shot(method: str, target: str, raw: bytes, ocamlc: str) -> Dict[str, Any]:
    """Socket-free request bridge for the agent-visible textual harness."""
    endpoint = urllib.parse.urlsplit(target).path
    if method == "POST":
        status, payload = process_post(
            endpoint,
            raw,
            compiler_adapter.check_source,
            ocamlc,
            compiler_adapter.vcs_for_source,
            compiler_adapter.check_workspace,
            compiler_adapter.backend_options(ocamlc),
        )
        return {"status": status, "json": payload}
    if endpoint == "/ls":
        return {"status": 200, "json": workspace.list_tree()}
    if endpoint == "/examples":
        index = workspace.examples_index_path()
        if index is None:
            return {"status": 404, "json": {"error": "no examples index"}}
        return {"status": 200, "text": Path(index).read_text(encoding="utf-8")}
    if endpoint == "/config":
        return {"status": 200, "json": backend_configuration(ocamlc)}
    if endpoint == "/file":
        query = urllib.parse.parse_qs(urllib.parse.urlsplit(target).query)
        path_id = (query.get("path") or [""])[0]
        resolved = workspace.resolve(path_id)
        if resolved is None:
            return {"status": 404, "json": {"error": "no such file"}}
        return {"status": 200, "text": Path(resolved).read_text(encoding="utf-8")}
    return {"status": 404, "json": {"error": "not found"}}


def main() -> None:
    parser = argparse.ArgumentParser(description="Serve the local vox2 IDE.")
    parser.add_argument("--port", type=int, default=8000)
    parser.add_argument("--ocamlc", help="path to the vox2 ocamlc.opt")
    parser.add_argument(
        "--one-shot",
        nargs=2,
        metavar=("METHOD", "TARGET"),
        help="serve one request from stdin/stdout without binding a socket",
    )
    args = parser.parse_args()

    ocamlc = compiler_adapter.find_ocamlc(args.ocamlc)
    if ocamlc is None:
        raise SystemExit("no vox2 compiler found; pass --ocamlc or set VOX2_OCAMLC")
    if args.one_shot:
        method, target = args.one_shot
        result = one_shot(method.upper(), target, sys.stdin.buffer.read(), ocamlc)
        sys.stdout.write(json.dumps(result))
        return
    httpd, port = make_server(args.port, ocamlc)
    print(
        f"voxide on http://127.0.0.1:{port}/  "
        f"(ocamlc={ocamlc}; "
        f"backends={','.join(compiler_adapter.backend_options(ocamlc))})",
        flush=True,
    )
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        httpd.server_close()


if __name__ == "__main__":
    main()
