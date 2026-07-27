#!/usr/bin/env python3
"""Compiler adapter for the vox2 browser IDE.

Locations returned by this module are 0-based line/column pairs, matching
CodeMirror.  The compiler itself prints 1-based lines and 0-based UTF-8 byte
columns.
"""

import json
import os
import re
import signal
import subprocess
import tempfile
import time
from functools import lru_cache
from pathlib import Path
from typing import Any, Callable, Dict, List, Mapping, Optional, Sequence, Tuple


HERE = Path(__file__).resolve().parent
WORKTREE = HERE.parent

_FILE = r'(?:File "[^"]*", )?'
_LOC_SINGLE = re.compile(_FILE + r"[Ll]ine (\d+), characters (\d+)-(\d+):")
_LOC_MULTI = re.compile(_FILE + r"[Ll]ines (\d+)-(\d+), characters (\d+)-(\d+):")
_ANNOT_LOC = re.compile(r'^"[^"]*" (\d+) (\d+) (\d+) "[^"]*" (\d+) (\d+) (\d+)\s*$')
_ERROR = re.compile(r"^Error(?: \([^)]*\))?:\s*(.*)$")

# The vox2 compiler performs refinement verification as part of the ordinary
# ``-c`` type-checking pass (``Vox_verify.verify_structure`` runs
# unconditionally on every implementation), discharging each obligation with
# Lean.  A discharge failure surfaces as an ordinary located compiler error
# whose message begins with this phrase, which is how a verification failure is
# told apart from a plain type/mode error.
_VERIFY_PREFIX = "Refinement verification failed"

# A refinement type prints with a predicate over the ``_`` hole inside braces,
# e.g. ``int{ (app[Stdlib!.>=] _ 3) }``.  This distinguishes a refined type
# from a record/object type (whose braces hold field names, not a bare hole),
# and is used only to phrase the positive indicator ("obligations discharged"
# vs "no refinements to verify") -- it never gates whether verification runs.
_REFINEMENT_IN_TYPE = re.compile(r"\{[^{}]*(?<![A-Za-z0-9_])_(?![A-Za-z0-9_])[^{}]*\}")

VERIFICATION_BACKENDS = ("lean", "z3", "oxsmt", "cross")
NO_VERIFICATION_BACKEND = "none"
BACKENDS = (*VERIFICATION_BACKENDS, NO_VERIFICATION_BACKEND)
SOLVER_ENVIRONMENT = {
    "z3": "VOXIDE_SMT_SOLVER",
    "oxsmt": "VOXIDE_OXSMT_SOLVER",
}


@lru_cache(maxsize=None)
def backend_options(ocamlc: str) -> Tuple[str, ...]:
    """Backends the compiler advertises, with a legacy-Lean fallback.

    The currently deployed compiler predates ``-vox-backend``.  Detect the
    additive flag once per binary and keep its normal Lean invocation flag-free
    when absent, so this IDE remains inert-safe during the compiler rollout.
    """
    try:
        completed = subprocess.run(
            [ocamlc, "-help"],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            encoding="utf-8",
            errors="replace",
            timeout=5,
            check=False,
        )
    except (OSError, subprocess.TimeoutExpired):
        return ("lean",)
    help_text = completed.stdout + completed.stderr
    options = (
        list(VERIFICATION_BACKENDS)
        if "-vox-backend" in help_text
        else ["lean"]
    )
    if "-vox-type-only" in help_text:
        options.append(NO_VERIFICATION_BACKEND)
    return tuple(options)


@lru_cache(maxsize=None)
def supports_vc_dump(ocamlc: str) -> bool:
    """Whether ``ocamlc`` can attach the VC sidecar to the normal check.

    Older deployed compilers still typecheck and verify during ``-c``, but do
    not understand the additive dump flag.  Keep that path flag-free and mark
    its per-obligation data unavailable instead of breaking the whole check.
    """
    try:
        completed = subprocess.run(
            [ocamlc, "-help"],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            encoding="utf-8",
            errors="replace",
            timeout=5,
            check=False,
        )
    except (OSError, subprocess.TimeoutExpired):
        return False
    return "-vox-dump-vc-json" in completed.stdout + completed.stderr


def solver_commands() -> Dict[str, str]:
    """Configured external solver commands, without inferred defaults."""
    commands = {}
    for solver, variable in SOLVER_ENVIRONMENT.items():
        command = os.environ.get(variable)
        if command is not None and command.strip():
            commands[solver] = command
    return commands


@lru_cache(maxsize=None)
def supports_in_process_oxsmt(ocamlc: str) -> bool:
    """Whether ``ocamlc`` discharges oxsmt obligations without a command."""
    try:
        completed = subprocess.run(
            [ocamlc, "-help"],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            encoding="utf-8",
            errors="replace",
            timeout=5,
            check=False,
        )
    except (OSError, subprocess.TimeoutExpired):
        return False
    help_text = completed.stdout + completed.stderr
    return (
        "-vox-oxsmt-solver" in help_text
        and "Ignored by in-process oxsmt" in help_text
    )


def backend_solver_configuration(ocamlc: Optional[str] = None) -> Dict[str, bool]:
    """Public capability facts used to annotate, not filter, backends."""
    commands = solver_commands()
    return {
        "z3": "z3" in commands,
        "oxsmt": "oxsmt" in commands
        or (ocamlc is not None and supports_in_process_oxsmt(ocamlc)),
    }


def _backend_arguments(ocamlc: str, backend: str) -> List[str]:
    if backend not in BACKENDS:
        raise ValueError(f"unsupported verification backend: {backend}")
    options = backend_options(ocamlc)
    if backend not in options:
        raise ValueError(f"compiler does not support verification backend: {backend}")
    if backend == NO_VERIFICATION_BACKEND:
        return ["-vox-type-only"]
    if backend == "lean" and not any(
        option in options for option in ("z3", "oxsmt", "cross")
    ):
        return []
    arguments = ["-vox-backend", backend]
    commands = solver_commands()
    if backend in ("z3", "cross") and "z3" in commands:
        arguments.extend(["-vox-smt-solver", commands["z3"]])
    if backend in ("oxsmt", "cross") and "oxsmt" in commands:
        arguments.extend(["-vox-oxsmt-solver", commands["oxsmt"]])
    return arguments


class CompileCancelled(Exception):
    """The caller superseded a compile; its process group has been reaped."""


def _error_kind(message: str) -> str:
    """Classify a source diagnostic without upgrading infrastructure failure."""
    if message.startswith(_VERIFY_PREFIX):
        return "verification"
    if "syntax error" in message.lower():
        return "syntax"
    return "type-mode"


def _outcome(
    kind: str, message: str = "", source_located: bool = False
) -> Dict[str, Any]:
    """One structured boundary between the compiler process and the UI."""
    return {
        "kind": kind,
        "message": message,
        "source_located": source_located,
    }


def _unavailable_reason_for_outcome(kind: str) -> Optional[str]:
    return {
        "syntax": "type-error",
        "type-mode": "type-error",
        "compiler-unavailable": "compiler-unavailable",
        "compiler-crashed": "compiler-crashed",
        "timeout": "timeout",
        "backend-unavailable": "backend-unavailable",
    }.get(kind)


def _primary_outcome(
    checked: "subprocess.CompletedProcess[str]",
    errors: Sequence[Dict[str, Any]],
    vcs_payload: Mapping[str, Any],
) -> Dict[str, Any]:
    """Classify only the authoritative compile, never a presentation follow-up."""
    if any(error.get("kind") == "syntax" for error in errors):
        error = next(error for error in errors if error.get("kind") == "syntax")
        return _outcome("syntax", str(error.get("message", "")), "start" in error)
    if any(error.get("kind") == "type-mode" for error in errors):
        error = next(error for error in errors if error.get("kind") == "type-mode")
        return _outcome("type-mode", str(error.get("message", "")), "start" in error)
    statuses = _as_dict(vcs_payload.get("obligation_summary")).get("statuses", {})
    if isinstance(statuses, dict) and int(statuses.get("unavailable", 0) or 0) > 0:
        return _outcome("backend-unavailable", "The selected backend was unavailable.")
    if any(error.get("kind") == "verification" for error in errors):
        error = next(error for error in errors if error.get("kind") == "verification")
        return _outcome(
            "verification", str(error.get("message", "")), "start" in error
        )
    summary = _as_dict(vcs_payload.get("obligation_summary"))
    total = int(summary.get("total", 0) or 0)
    if total and int(statuses.get("proved", 0) or 0) != total:
        return _outcome("verification", "Some obligations were not discharged.")
    if checked.returncode != 0:
        detail = (checked.stderr + checked.stdout).strip()
        return _outcome("compiler-crashed", detail or "The compiler exited unexpectedly.")
    return _outcome("ok")


def _has_refinements(signature: str, types: Sequence[Dict[str, Any]]) -> bool:
    """Best-effort test for whether the buffer carries refinement predicates.

    Checks the inferred expression types (which cover internal ascriptions)
    and any ``val`` line of the inferred signature.  Used only to word the
    verification indicator; a false negative merely downgrades "verified" to
    "no refinements", never the reverse.
    """
    for entry in types:
        if _REFINEMENT_IN_TYPE.search(str(entry.get("type", ""))):
            return True
    for line in signature.splitlines():
        stripped = line.lstrip()
        if not stripped.startswith("val "):
            continue
        # Look for the refinement pattern only in the type (after the first
        # ":"), never in the value name: an index-operator binding such as
        # ``val ( .@{} ) : ...`` carries braces in its *name*, which must not
        # be mistaken for a refinement (that would falsely claim "verified").
        _, _, annotation = stripped.partition(":")
        if _REFINEMENT_IN_TYPE.search(annotation):
            return True
    return False


def _verification_status(
    errors: Sequence[Dict[str, Any]],
    signature_or_types: Any,
    types_or_summary: Any,
    summary: Optional[Mapping[str, Any]] = None,
) -> Dict[str, Any]:
    """Summarize the verification outcome of one check for the editor."""
    if summary is None and isinstance(types_or_summary, Mapping):
        signature = ""
        types = signature_or_types
        summary = types_or_summary
    else:
        signature = str(signature_or_types)
        types = types_or_summary
        summary = summary or _obligation_summary([], [])
    verify_errors = [e for e in errors if e.get("kind") == "verification"]
    total = int(summary.get("total", 0) or 0)
    statuses = summary.get("statuses", {})
    statuses = statuses if isinstance(statuses, dict) else {}
    has_refinements = total > 0 or _has_refinements(signature, types)
    if verify_errors:
        return {
            "status": "failed",
            "message": verify_errors[0]["message"],
            "obligations": True,
        }
    if errors:
        return {
            "status": "blocked",
            "message": "Verification runs once the type errors are fixed.",
            "obligations": has_refinements,
        }
    if total > 0 and int(statuses.get("proved", 0) or 0) != total:
        return {
            "status": "failed",
            "message": "Some obligations were not discharged.",
            "obligations": True,
        }
    if has_refinements:
        return {
            "status": "verified",
            "message": "All refinement obligations discharged.",
            "obligations": True,
        }
    return {
        "status": "none",
        "message": "No refinements to verify.",
        "obligations": False,
    }


def _type_only_verification(errors: Sequence[Dict[str, Any]]) -> Dict[str, Any]:
    """Honest verification channel for a ``-vox-type-only`` check."""
    if errors:
        message = "Verification was not run; fix the type errors to finish checking."
    else:
        message = "Typecheck completed; verification was not run."
    return {"status": "not-run", "message": message, "obligations": False}


def find_ocamlc(override: Optional[str] = None) -> Optional[str]:
    """Find the vox2 compiler, honoring CLI then environment overrides."""
    requested = override or os.environ.get("VOX2_OCAMLC")
    if requested:
        path = Path(requested).expanduser().resolve()
        return str(path) if path.is_file() else None
    for relative in (
        "_install/bin/ocamlc.opt",
        "_build/_bootinstall/bin/ocamlc.opt",
    ):
        candidate = WORKTREE / relative
        if candidate.is_file():
            return str(candidate)
    return None


def _utf16_col(source_lines: Sequence[str], line: int, byte_col: int) -> int:
    """Convert a UTF-8 byte offset to a CodeMirror UTF-16 column."""
    if line < 0 or line >= len(source_lines):
        return byte_col
    encoded = source_lines[line].encode("utf-8")
    prefix = encoded[: max(0, byte_col)].decode("utf-8", errors="ignore")
    return len(prefix.encode("utf-16-le")) // 2


def parse_loc(
    header: str, source_lines: Sequence[str]
) -> Optional[Tuple[Dict[str, int], Dict[str, int]]]:
    """Parse a diagnostic location as 0-based UTF-16 coordinates."""
    match = _LOC_SINGLE.match(header)
    if match is not None:
        line = int(match.group(1)) - 1
        return (
            {
                "line": line,
                "col": _utf16_col(source_lines, line, int(match.group(2))),
            },
            {
                "line": line,
                "col": _utf16_col(source_lines, line, int(match.group(3))),
            },
        )
    match = _LOC_MULTI.match(header)
    if match is not None:
        start_line = int(match.group(1)) - 1
        end_line = int(match.group(2)) - 1
        return (
            {
                "line": start_line,
                "col": _utf16_col(source_lines, start_line, int(match.group(3))),
            },
            {
                "line": end_line,
                "col": _utf16_col(source_lines, end_line, int(match.group(4))),
            },
        )
    return None


def parse_any_error(text: str, source_lines: Sequence[str]) -> Optional[Dict[str, Any]]:
    """Extract the first ordinary OCaml error from compiler output."""
    lines = text.splitlines()
    for index, line in enumerate(lines):
        error_match = _ERROR.match(line)
        if error_match is None:
            continue
        location = None
        for previous in range(index - 1, -1, -1):
            if _ERROR.match(lines[previous]) is not None:
                break
            location = parse_loc(lines[previous], source_lines)
            if location is not None:
                break
        message = [error_match.group(1).strip()]
        for continuation in lines[index + 1 :]:
            if continuation.startswith((" ", "\t")) and continuation.strip():
                message.append(continuation.strip())
            else:
                break
        error: Dict[str, Any] = {"message": " ".join(message)}
        if location is not None:
            error["start"], error["end"] = location
        return error
    return None


def parse_annot(text: str, source_lines: Sequence[str]) -> List[Dict[str, Any]]:
    """Extract expression types from a textual .annot file.

    The returned coordinates are already normalized to 0-based lines and
    columns for the browser.
    """
    result: List[Dict[str, Any]] = []
    lines = text.splitlines()
    location: Optional[Tuple[Dict[str, int], Dict[str, int]]] = None
    index = 0
    while index < len(lines):
        match = _ANNOT_LOC.match(lines[index])
        if match is not None:
            line1, bol1, char1, line2, bol2, char2 = (
                int(group) for group in match.groups()
            )
            start_line = line1 - 1
            end_line = line2 - 1
            location = (
                {
                    "line": start_line,
                    "col": _utf16_col(source_lines, start_line, char1 - bol1),
                },
                {
                    "line": end_line,
                    "col": _utf16_col(source_lines, end_line, char2 - bol2),
                },
            )
            index += 1
            continue
        if lines[index].startswith("type(") and location is not None:
            body: List[str] = []
            index += 1
            while index < len(lines) and lines[index] != ")":
                body.append(lines[index].strip())
                index += 1
            result.append(
                {
                    "start": location[0],
                    "end": location[1],
                    "type": " ".join(body),
                }
            )
        index += 1
    return result


def extract_signature(output: str) -> str:
    """Normalize successful ``ocamlc -i`` stdout for display."""
    return output.strip()


# The compiler drives Lean per obligation with its own 30s timeout, so the
# subprocess budget must sit above that to let the compiler report a real
# "verification failed (solver error)" rather than being killed mid-discharge.
# A single slow request is harmless: the client debounces, single-flights, and
# drops any response that a newer edit has superseded.  Note this budget is per
# compiler invocation, not cumulative across obligations: a buffer with many
# slow obligations can still exceed it (each obligation gets the compiler's own
# 30s), in which case the run is reported as a compiler-run failure.
_CHECK_TIMEOUT_SECONDS = 60


def _run(
    ocamlc: str,
    arguments: List[str],
    scratch: str,
    timeout: int = _CHECK_TIMEOUT_SECONDS,
    cancel_check: Optional[Callable[[], bool]] = None,
) -> "subprocess.CompletedProcess[str]":
    # Run in its own session so the whole process group can be reaped on
    # timeout: the compiler spawns Lean through a shell/`timeout` wrapper, and
    # killing only the direct child would leave those grandchildren running
    # past the deadline.  Decode leniently so a stray non-UTF-8 byte in a
    # diagnostic cannot crash the adapter.
    process = subprocess.Popen(
        [ocamlc, *arguments],
        cwd=scratch,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        encoding="utf-8",
        errors="replace",
        start_new_session=True,
    )
    deadline = time.monotonic() + timeout
    while True:
        if cancel_check is not None and cancel_check():
            _kill_process_group(process)
            process.communicate()
            raise CompileCancelled()
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            _kill_process_group(process)
            process.communicate()
            raise subprocess.TimeoutExpired(process.args, timeout)
        try:
            stdout, stderr = process.communicate(timeout=min(0.1, remaining))
            break
        except subprocess.TimeoutExpired:
            continue
    return subprocess.CompletedProcess(process.args, process.returncode, stdout, stderr)


def _kill_process_group(process: "subprocess.Popen[str]") -> None:
    """Reap the compiler and every solver descendant in its private session."""
    try:
        os.killpg(process.pid, signal.SIGKILL)
    except (ProcessLookupError, PermissionError):
        process.kill()


def check_source(
    source: str,
    revision: int,
    ocamlc: str,
    backend: str = "lean",
    cancel_check: Optional[Callable[[], bool]] = None,
) -> Dict[str, Any]:
    """Typecheck and verify one buffer, returning every IDE result channel.

    The VC dump flag composes with ``-annot``: the authoritative ``-c`` pass
    therefore produces diagnostics, expression types, and discharged
    obligations together.  Signature inference is a separate presentation-only
    request, so this authoritative result never waits for or depends on ``-i``.
    """
    source_lines = source.split("\n")
    with tempfile.TemporaryDirectory(prefix="voxide-") as scratch:
        source_path = Path(scratch) / "input.ml"
        source_path.write_text(source, encoding="utf-8")
        dump_path = Path(scratch) / "vcs.json"

        try:
            backend_args = _backend_arguments(ocamlc, backend)
            dump_args = (
                ["-vox-dump-vc-json", "vcs.json"]
                if backend != NO_VERIFICATION_BACKEND and supports_vc_dump(ocamlc)
                else []
            )
            checked = _run(
                ocamlc,
                [*backend_args, "-c", "-annot", *dump_args, "input.ml"],
                scratch,
                cancel_check=cancel_check,
            )
        except CompileCancelled:
            raise
        except subprocess.TimeoutExpired:
            message = "The compiler check timed out."
            return {
                "revision": revision,
                "ok": False,
                "outcome": _outcome("timeout", message),
                "errors": [{"message": message, "kind": "timeout"}],
                "types": [],
                "signature": {"status": "not-requested", "text": "", "error": ""},
                "verification": {
                    "status": "unavailable",
                    "message": message,
                    "obligations": False,
                },
                **_vcs_unavailable(
                    revision,
                    backend,
                    backend_options(ocamlc),
                    reason="timeout",
                    ocamlc=ocamlc,
                ),
            }
        except OSError as exc:
            message = f"could not run vox2 compiler: {exc}"
            return {
                "revision": revision,
                "ok": False,
                "outcome": _outcome("compiler-unavailable", message),
                "errors": [{"message": message, "kind": "compiler-unavailable"}],
                "types": [],
                "signature": {"status": "not-requested", "text": "", "error": ""},
                "verification": {
                    "status": "unavailable",
                    "message": message,
                    "obligations": False,
                },
                **_vcs_unavailable(
                    revision,
                    backend,
                    backend_options(ocamlc),
                    reason="compiler-unavailable",
                    ocamlc=ocamlc,
                ),
            }

        diagnostics = checked.stderr + checked.stdout
        errors: List[Dict[str, Any]] = []
        parsed = parse_any_error(diagnostics, source_lines)
        if parsed is not None:
            errors.append(parsed)
        elif checked.returncode != 0:
            detail = diagnostics.strip()
            if not detail:
                detail = f"compiler exited {checked.returncode}"
            errors.append({"message": detail, "kind": "compiler-crashed"})

        annot_path = Path(scratch) / "input.annot"
        types = []
        if annot_path.is_file():
            types = parse_annot(annot_path.read_text(encoding="utf-8"), source_lines)

        for error in errors:
            error.setdefault("kind", _error_kind(error["message"]))

        if backend == NO_VERIFICATION_BACKEND:
            vc_payload = _vcs_unavailable(
                revision,
                backend,
                backend_options(ocamlc),
                reason="verification-not-run",
                ocamlc=ocamlc,
            )
        else:
            vc_payload = _vcs_from_dump_path(
                dump_path,
                source_lines,
                checked.returncode,
                revision,
                backend,
                backend_options(ocamlc),
                types,
                ocamlc=ocamlc,
            )
        outcome = _primary_outcome(checked, errors, vc_payload)
        if backend == NO_VERIFICATION_BACKEND and outcome["kind"] == "ok":
            outcome = _outcome("checked-no-verification")
        reason_for_outcome = _unavailable_reason_for_outcome(outcome["kind"])
        if (
            backend != NO_VERIFICATION_BACKEND
            and reason_for_outcome is not None
            and vc_payload.get("unavailable")
        ):
            vc_payload["unavailable_reason"] = reason_for_outcome
        summary = _as_dict(vc_payload.get("obligation_summary"))
        return {
            "revision": revision,
            "backend": backend,
            "backend_options": list(backend_options(ocamlc)),
            "ok": outcome["kind"] in ("ok", "checked-no-verification"),
            "outcome": outcome,
            "errors": errors,
            "types": types,
            "signature": {"status": "not-requested", "text": "", "error": ""},
            "verification": (
                _type_only_verification(errors)
                if backend == NO_VERIFICATION_BACKEND
                else _verification_status(errors, types, summary)
            ),
            **vc_payload,
            "backend_solver_configuration": backend_solver_configuration(ocamlc),
        }


def signature_for_source(
    source: str,
    revision: int,
    ocamlc: str,
    backend: str = "lean",
    cancel_check: Optional[Callable[[], bool]] = None,
) -> Dict[str, Any]:
    """Presentation-only signature inference, isolated from the check verdict."""
    with tempfile.TemporaryDirectory(prefix="voxide-signature-") as scratch:
        (Path(scratch) / "input.ml").write_text(source, encoding="utf-8")
        try:
            inferred = _run(
                ocamlc,
                [*_backend_arguments(ocamlc, backend), "-i", "input.ml"],
                scratch,
                cancel_check=cancel_check,
            )
        except CompileCancelled:
            raise
        except subprocess.TimeoutExpired:
            signature = {
                "status": "unavailable",
                "text": "",
                "error": "Signature inference timed out.",
            }
        except OSError as exc:
            signature = {
                "status": "unavailable",
                "text": "",
                "error": f"Could not run signature inference: {exc}",
            }
        else:
            if inferred.returncode == 0:
                text = extract_signature(inferred.stdout)
                signature = {
                    "status": "available" if text else "empty",
                    "text": text,
                    "error": "",
                }
            else:
                detail = (inferred.stderr + inferred.stdout).strip()
                signature = {
                    "status": "unavailable",
                    "text": "",
                    "error": detail or "Module signature inference failed.",
                }
        return {"revision": revision, "backend": backend, "signature": signature}


# ---------------------------------------------------------------------------
# Per-VC dump (proof pane).
#
# The vox2 compiler's ``-vox-dump-vc-json FILE`` writes one JSON document
# (schema version 1) recording every refinement obligation it discharged,
# including obligations processed before a later verification error aborts the
# compile.  This module runs that dump and translates schema v1 into the flat
# per-VC shape the frontend adapter (``adaptVcs``) consumes.  Every buffer --
# curated example or hand-edited scratch -- is fed from this real dump.
# ---------------------------------------------------------------------------

# Discharge verdict (schema v1) -> the status the proof pane paints and badges.
# ``not-proved`` (automation gave up, no counterexample) reads as "unproved";
# ``solver-error`` (the solver itself failed, e.g. a timeout) is kept distinct
# from a genuine rejection.  Any verdict outside this map fails closed to
# "unknown" (see ``translate_vc``), so an unrecognized status is never mistaken
# for a discharge.
_VC_STATUS = {
    "proved": "proved",
    "disproved": "disproved",
    "not-proved": "unproved",
    "solver-error": "solver-error",
    "unknown": "unknown",
    "unavailable": "unavailable",
}

_SUMMARY_STATUSES = tuple(_VC_STATUS.values())


def _empty_status_counts() -> Dict[str, int]:
    return {status: 0 for status in _SUMMARY_STATUSES}


def _raw_vc_status(raw: object) -> str:
    raw = raw if isinstance(raw, dict) else {}
    discharge = raw.get("discharge")
    discharge = discharge if isinstance(discharge, dict) else {}
    return _VC_STATUS.get(str(discharge.get("status", "")), "unknown")


def _obligation_summary(
    conditions: Sequence[object], placeable: Sequence[bool]
) -> Dict[str, Any]:
    """Status counts for every dumped VC, including span-less obligations."""
    statuses = _empty_status_counts()
    hidden_statuses = _empty_status_counts()
    for index, raw in enumerate(conditions):
        status = _raw_vc_status(raw)
        statuses[status] += 1
        if index >= len(placeable) or not placeable[index]:
            hidden_statuses[status] += 1
    return {
        "total": len(conditions),
        "statuses": statuses,
        "hidden": sum(hidden_statuses.values()),
        "hidden_statuses": hidden_statuses,
    }

# VC origin (schema v1) -> the informational label the pane shows in full mode.
_VC_KIND = {
    "annotation": "annotation",
    "contract-argument": "contract",
    "seal-implication": "seal",
}


def _coerce_int(value: object, default: int) -> int:
    try:
        return int(str(value))
    except (TypeError, ValueError):
        return default


def _as_dict(value: object) -> Dict[str, Any]:
    return value if isinstance(value, dict) else {}


# The compiler drives the solver through a throwaway Lean file in ``$TMPDIR``
# (e.g. ``/tmp/vox2-vc6891af.lean``) and the solver's
# diagnostic quotes that absolute, per-run path.  It is an internal scratch file
# with a random name -- meaningless (and non-deterministic) to the user -- so
# replace the whole path token with a stable ``vc.lean`` before the detail is
# shown, leaving the diagnostic's line/column and message intact.
_LEAN_SCRATCH_PATH = re.compile(r"\S*vox2-\S*?\.lean")


def _scrub_detail(detail: object) -> Optional[str]:
    """A solver ``detail`` with the internal temp Lean path relabeled."""
    if not detail:
        return None
    return _LEAN_SCRATCH_PATH.sub("vc.lean", str(detail))


def _vc_anchor_span(
    vc: Dict[str, Any],
    lines_by_file: Mapping[str, Sequence[str]],
    expected_file: Optional[str] = None,
) -> Optional[Dict[str, Any]]:
    """Pick the source span that anchors a VC in the buffer.

    ``location`` is the value/subterm the obligation actually constrains (the
    argument value for a contract argument, the annotated sub-expression for an
    annotation), which is where the caret must land to see the mark -- not
    ``program_point``, the enclosing site (the whole call, incl. the callee
    name).  Preferring ``location`` keeps a contract-argument mark on its value
    rather than smearing it across the call syntax.  ``location`` is ghost for
    an annotation whose obligation has no narrower sub-span, in which case this
    falls through to ``program_point``; if every candidate is ghost the VC is
    dropped -- an unplaceable mark is worse than none.
    """
    for key in ("location", "program_point"):
        span = vc.get(key)
        normalized = _normalize_emitted_span(span, lines_by_file, expected_file)
        if normalized is not None:
            return normalized
    goal = vc.get("goal")
    if isinstance(goal, dict):
        normalized = _normalize_emitted_span(
            goal.get("source_span"), lines_by_file, expected_file
        )
        if normalized is not None:
            return normalized
    return None


def _editor_point(point: Dict[str, Any], source_lines: Sequence[str]) -> Dict[str, int]:
    # Schema v1 spans are 1-based lines / 0-based UTF-8 byte columns; the editor
    # wants 0-based lines / 0-based UTF-16 columns, as the diagnostics already do.
    line = max(0, _coerce_int(point.get("line"), 1) - 1)
    return {
        "line": line,
        "col": _utf16_col(source_lines, line, _coerce_int(point.get("column"), 0)),
    }


def _editor_span(
    span: Dict[str, Any], source_lines: Sequence[str]
) -> Dict[str, Dict[str, int]]:
    return {
        "start": _editor_point(_as_dict(span.get("start")), source_lines),
        "end": _editor_point(_as_dict(span.get("end")), source_lines),
    }


def _predicate(pred: object) -> Dict[str, str]:
    """A predicate's pretty ``display`` and its ``raw`` text.

    Schema v2 adds a source-like ``display`` (e.g. ``_ >= 3``); schema v1 has
    only ``text`` (the instantiated app-syntax, e.g. ``(app[Stdlib!.>=] _ 3)``).
    ``display`` falls back to the raw text so the same renderer serves both
    schema versions.  Whitespace is preserved -- the goal display keeps its
    structure -- so no collapsing happens here.
    """
    pred = _as_dict(pred)
    raw = str(pred.get("text", ""))
    display = pred.get("display")
    return {"display": str(display) if display else raw, "raw": raw}


def _fact_producers(
    fact: Mapping[str, Any],
    lines_by_file: Mapping[str, Sequence[str]],
    expected_file: Optional[str] = None,
) -> Optional[List[Dict[str, Any]]]:
    """Every site the compiler says introduced this fact, in editor spans.

    The compiler reports ``also_introduced_by``: the sites other than
    ``origin`` whose identical proposition the fact environment folded into
    this one entry.  Usually there are none.  ``origin`` is always one of the
    introducers, so the answer is it followed by those, and a compiler that
    reports no such field at all yields ``None`` -- a consumer that has to
    know every introducer of a fact must then treat the fact as having
    unknown provenance rather than reading ``origin`` as the complete answer,
    since a single ``origin`` is exactly what a fold leaves behind.

    An entry whose span will not place -- a ghost location, or a site in a
    file this view does not hold -- is dropped and the rest kept.  That is
    safe in the one direction that matters: a call this editor can decide is
    a call whose own span placed, so a dropped entry can never be the call
    being decided, and dropping it can only withhold evidence about some
    other site, never manufacture evidence against one.  Nullifying the whole
    list instead would throw away the placeable entries alongside it, and
    with them the only record that a call was read.
    """
    raw = fact.get("also_introduced_by")
    if not isinstance(raw, list):
        return None
    producers: List[Dict[str, Any]] = []
    for entry in [fact.get("origin")] + list(raw):
        if entry is None:
            continue
        entry = _as_dict(entry)
        normalized = _normalize_emitted_span(
            entry.get("span"), lines_by_file, expected_file
        )
        if normalized is None:
            continue
        name = entry.get("name")
        kind = entry.get("kind")
        producers.append(
            {
                "name": str(name) if name else None,
                "kind": str(kind) if kind else None,
                "span": normalized,
            }
        )
    return producers


def _backend_unused_facts(
    discharge: Mapping[str, Any]
) -> Optional[Dict[str, Optional[List[int]]]]:
    """Per-backend unread-fact indices, or ``None`` outside a cross-check.

    A backend that reported no accounting maps to ``None``; that is not the
    same as reporting that it read everything, and the two must not be
    conflated by a consumer asking whether every backend left a fact unread.
    """
    raw_results = discharge.get("backends")
    if not isinstance(raw_results, list):
        return None
    per_backend: Dict[str, Optional[List[int]]] = {}
    for raw in raw_results:
        raw = _as_dict(raw)
        backend = str(raw.get("backend", ""))
        if backend not in ("lean", "z3", "oxsmt"):
            continue
        unused = raw.get("unused_facts")
        if isinstance(unused, list) and all(
            isinstance(index, int) and not isinstance(index, bool)
            for index in unused
        ):
            per_backend[backend] = list(unused)
        else:
            per_backend[backend] = None
    return per_backend or None


def _used_by(
    index: int, per_backend: Optional[Mapping[str, Optional[List[int]]]]
) -> Optional[Dict[str, bool]]:
    """Which backends read the fact at ``index``.

    Only backends that reported an accounting appear.  The caller compares the
    keys against the backends the obligation actually ran on: a missing key is
    a backend whose reading is unknown, never one that did not read the fact.
    """
    if not per_backend:
        return None
    used: Dict[str, bool] = {}
    for backend, unused in per_backend.items():
        if unused is None:
            continue
        used[backend] = index not in unused
    return used or None


def _hypothesis(
    fact: object,
    source_lines: Sequence[str],
    index: int = 0,
    per_backend: Optional[Mapping[str, Optional[List[int]]]] = None,
) -> Dict[str, Any]:
    """One fact as a named, optionally source-linked hypothesis.

    Schema v2 adds ``origin`` = {name, span}: the binder name (rendered as the
    hypothesis label, positional ``h0``/``h1`` otherwise -- handled by the
    frontend) and the source span (making the hypothesis clickable).  Both are
    absent under v1 and for kinds that carry no recoverable origin.
    """
    fact = _as_dict(fact)
    predicate = _predicate(fact)
    origin = _as_dict(fact.get("origin"))
    name = origin.get("name")
    kind = origin.get("kind")
    span = origin.get("span")
    editor_span = None
    normalized = _normalize_emitted_span(
        span, {"input.ml": source_lines}, expected_file="input.ml"
    )
    if normalized is not None:
        editor_span = {"start": normalized["start"], "end": normalized["end"]}
    hypothesis = {
        "name": str(name) if name else None,
        # The origin kind (``binder``/``contract-argument``/``application``/
        # ``branch``): the pane's off-obligation "known here" view keeps only
        # real in-scope binders, so this distinguishes a bound variable from a
        # concrete call-site value that merely carries the callee's parameter
        # name.
        "kind": str(kind) if kind else None,
        "display": predicate["display"],
        "raw": predicate["raw"],
        "span": editor_span,
    }
    # Preserve capability absence.  All three backends report fact usage
    # today, each from its own reading -- an unsat core, an assumption core,
    # an unused-variable diagnostic -- and any of them can decline to.
    # Treating an omitted value as true would manufacture usage.
    if "used" in fact and isinstance(fact.get("used"), bool):
        hypothesis["used"] = fact["used"]
    hypothesis["producers"] = _fact_producers(
        fact, {"input.ml": source_lines}, expected_file="input.ml"
    )
    hypothesis["used_by"] = _used_by(index, per_backend)
    return hypothesis


def _backend_results(discharge: Dict[str, Any]) -> Optional[List[Dict[str, Any]]]:
    raw_results = discharge.get("backends")
    if not isinstance(raw_results, list):
        return None
    results = []
    for raw in raw_results:
        if not isinstance(raw, dict):
            continue
        backend = str(raw.get("backend", ""))
        if backend not in ("lean", "z3", "oxsmt"):
            continue
        status = _VC_STATUS.get(str(raw.get("status", "")), "unknown")
        results.append(
            {
                "backend": backend,
                "status": status,
                "detail": _scrub_detail(raw.get("detail")),
                "fact_usage": bool(raw.get("fact_usage", False)),
            }
        )
    return results


def translate_vc(
    vc: Dict[str, Any], index: int, source_lines: Sequence[str]
) -> Optional[Dict[str, Any]]:
    """Translate one schema VC into the structured shape ``adaptVcs`` expects.

    Consumes schema v2 (``display``/``origin``) when present and falls back to
    schema v1 raw text, so this works unchanged against both versions.
    """
    anchor = _vc_anchor_span(
        vc, {"input.ml": source_lines}, expected_file="input.ml"
    )
    if anchor is None:
        return None
    discharge = _as_dict(vc.get("discharge"))
    status = _VC_STATUS.get(str(discharge.get("status", "")), "unknown")
    raw_kind = str(vc.get("kind", ""))
    facts = vc.get("facts")
    facts = facts if isinstance(facts, list) else []
    per_backend = _backend_unused_facts(discharge)
    hypotheses = [
        _hypothesis(fact, source_lines, index, per_backend)
        for index, fact in enumerate(facts)
    ]
    counterexample = discharge.get("counterexample")
    return {
        "id": index,
        "status": status,
        "kind": _VC_KIND.get(raw_kind, raw_kind),
        "span": {"start": anchor["start"], "end": anchor["end"]},
        "goal": _predicate(vc.get("goal")),
        "hypotheses": hypotheses,
        "counterexample": [counterexample] if counterexample else None,
        # Extra fields the pane surfaces alongside the goal: the solver
        # diagnostic (with the internal temp path relabeled) and the positive
        # theorem (behind a disclosure).
        "detail": _scrub_detail(discharge.get("detail")),
        "generated_lean": vc.get("generated_lean") or None,
        "backends": _backend_results(discharge),
    }


def refinement_types(
    document: object, source_lines: Sequence[str]
) -> List[Dict[str, Any]]:
    """Type-at-cursor ranges for the subterms of every refinement predicate.

    Schema v2's optional ``refinement_expression_types`` array carries one
    ``{location, type}`` entry per node of each refinement predicate (the hole
    ``_``, literals, operators, and the whole predicate), rendered source-like
    (``int``/``bool``/``int{ _ > 0 }``, never raw app-syntax).  These are the
    types the ``.annot`` file cannot see -- a predicate is not a program
    expression -- so the cursor readout folds them in alongside the ordinary
    expression types.  The shape returned here matches ``parse_annot``
    (``{start, end, type}``, 0-based/UTF-16) so the client treats both alike;
    the client picks the smallest containing span, so a subterm wins over the
    enclosing annotation.  A ghost or malformed entry is dropped (honesty: only
    a placeable, compiler-emitted type is ever shown)."""
    document = _as_dict(document)
    entries = document.get("refinement_expression_types")
    if not isinstance(entries, list):
        return []
    result: List[Dict[str, Any]] = []
    for entry in entries:
        entry = _as_dict(entry)
        location = entry.get("location")
        type_text = entry.get("type")
        if not type_text:
            continue
        span = _normalize_emitted_span(
            location, {"input.ml": source_lines}, expected_file="input.ml"
        )
        if span is None:
            continue
        result.append(
            {"start": span["start"], "end": span["end"], "type": str(type_text)}
        )
    return result


def refinement_types_by_file(
    document: object, lines_by_file: Mapping[str, Sequence[str]]
) -> List[Dict[str, Any]]:
    """Strict file-tagged refinement-predicate types for workspace cursors."""
    document = _as_dict(document)
    entries = document.get("refinement_expression_types")
    if not isinstance(entries, list):
        return []
    result: List[Dict[str, Any]] = []
    for raw in entries:
        entry = _as_dict(raw)
        location = entry.get("location")
        type_text = entry.get("type")
        span = _normalize_emitted_span(location, lines_by_file)
        if span is None or not type_text:
            continue
        result.append(
            {
                "file": span["file"],
                "start": span["start"],
                "end": span["end"],
                "type": str(type_text),
            }
        )
    return result


def lemma_calls(
    document: object,
    lines_by_file: Mapping[str, Sequence[str]],
    expected_file: Optional[str] = None,
) -> Optional[List[Dict[str, Any]]]:
    """Call sites whose only product is a proposition, in editor spans.

    ``None`` when the compiler did not report the channel: an older binary
    that never names a lemma call is not a buffer without any, so a consumer
    must stay silent rather than conclude there are none to talk about.  An
    entry the compiler reported but whose span will not place is dropped and
    turns the whole channel unknown -- a call the editor cannot point at is a
    call it cannot decide.
    """
    document = _as_dict(document)
    entries = document.get("lemma_calls")
    if not isinstance(entries, list):
        return None
    result: List[Dict[str, Any]] = []
    for raw in entries:
        entry = _as_dict(raw)
        span = _normalize_emitted_span(
            entry.get("span"), lines_by_file, expected_file
        )
        if span is None:
            return None
        introduced = entry.get("introduced")
        if not isinstance(introduced, bool):
            return None
        name = entry.get("name")
        result.append(
            {
                "file": span.get("file"),
                "start": span["start"],
                "end": span["end"],
                "name": str(name) if name else None,
                "introduced": introduced,
            }
        )
    return result


def _same_editor_point(left: object, right: object) -> bool:
    left = _as_dict(left)
    right = _as_dict(right)
    return (
        left.get("line") == right.get("line")
        and left.get("col") == right.get("col")
    )


def _same_editor_span(left: object, right: object) -> bool:
    left = _as_dict(left)
    right = _as_dict(right)
    return _same_editor_point(left.get("start"), right.get("start")) and (
        _same_editor_point(left.get("end"), right.get("end"))
    )


def _raw_span_points(
    span: Dict[str, Any]
) -> Optional[Tuple[Tuple[int, int], Tuple[int, int]]]:
    """Strict schema coordinates, without the display adapter's defaults."""
    start = span.get("start")
    end = span.get("end")
    if not isinstance(start, dict) or not isinstance(end, dict):
        return None
    values = (
        start.get("line"),
        start.get("column"),
        end.get("line"),
        end.get("column"),
    )
    if any(type(value) is not int for value in values):
        return None
    start_line, start_col, end_line, end_col = values
    if start_line < 1 or end_line < 1 or start_col < 0 or end_col < 0:
        return None
    return (start_line, start_col), (end_line, end_col)


def _normalize_emitted_span(
    span: object,
    lines_by_file: Mapping[str, Sequence[str]],
    expected_file: Optional[str] = None,
) -> Optional[Dict[str, Any]]:
    """Validate one compiler span completely before coordinate conversion.

    The shared adapter contract requires a known file, explicit ``ghost=false``,
    integral non-negative coordinates, ordered endpoints, existing lines, UTF-8
    boundary columns, and columns within their source lines.  Invalid spans are
    omitted; this function never supplies coordinate defaults.
    """
    if not isinstance(span, dict):
        return None
    file = span.get("file")
    if type(file) is not str or file not in lines_by_file:
        return None
    if expected_file is not None and file != expected_file:
        return None
    if type(span.get("ghost")) is not bool or span.get("ghost") is not False:
        return None
    points = _raw_span_points(span)
    if points is None:
        return None
    start, end = points
    source_lines = lines_by_file[file]
    if start > end or start[0] > len(source_lines) or end[0] > len(source_lines):
        return None
    for line, column in (start, end):
        encoded = source_lines[line - 1].encode("utf-8")
        if column > len(encoded):
            return None
        try:
            encoded[:column].decode("utf-8")
        except UnicodeDecodeError:
            return None
    return {
        "file": file,
        "start": _editor_point(_as_dict(span.get("start")), source_lines),
        "end": _editor_point(_as_dict(span.get("end")), source_lines),
    }


def _valid_raw_span(
    span: object,
    source_lines: Sequence[str],
    expected_file: str,
) -> bool:
    """Compatibility predicate backed by the shared normalization contract."""
    return (
        _normalize_emitted_span(
            span, {expected_file: source_lines}, expected_file=expected_file
        )
        is not None
    )


def _raw_span_contains(outer: Dict[str, Any], inner: Dict[str, Any]) -> bool:
    """Whether two already-validated spans contain, in byte coordinates."""
    if outer.get("file") != inner.get("file"):
        return False
    outer_points = _raw_span_points(outer)
    inner_points = _raw_span_points(inner)
    if outer_points is None or inner_points is None:
        return False
    return (
        outer_points[0] <= inner_points[0]
        and inner_points[1] <= outer_points[1]
    )


def _raw_source_slice(
    span: Dict[str, Any], source_lines: Sequence[str]
) -> Optional[str]:
    """Read an emitted single-line byte span from this exact buffer state."""
    start = _as_dict(span.get("start"))
    end = _as_dict(span.get("end"))
    try:
        start_line = int(start["line"]) - 1
        end_line = int(end["line"]) - 1
        start_col = int(start["column"])
        end_col = int(end["column"])
    except (KeyError, TypeError, ValueError):
        return None
    if start_line != end_line or not 0 <= start_line < len(source_lines):
        return None
    encoded = source_lines[start_line].encode("utf-8")
    if not 0 <= start_col <= end_col <= len(encoded):
        return None
    try:
        return encoded[start_col:end_col].decode("utf-8")
    except UnicodeDecodeError:
        return None


def imposed_types(
    document: object,
    source_lines: Sequence[str],
    expression_types: Sequence[Dict[str, Any]],
    expected_file: str,
) -> List[Dict[str, Any]]:
    """Soundly established refinement-imposition sites for the cursor pane.

    Schema-v2 annotation VCs tie three independently emitted facts together:
    ``provenance.source_span`` is the annotation/imposition anchor,
    ``goal.source_span`` is its conclusion predicate, and
    ``refinement_expression_types`` types every predicate subterm.  The unique
    typed subterm whose emitted source span is exactly ``_`` supplies the bare
    checked skeleton.  The exact-span ``.annot`` entry supplies the imposed
    type shown today.

    Every link is required and ambiguity rejects the whole site.  In
    particular this does not strip or parse a printed type, and legacy dumps
    lacking provenance simply produce no entries.
    """
    document = _as_dict(document)
    if document.get("schema_version") != 2:
        return []
    conditions = document.get("verification_conditions")
    predicate_types = document.get("refinement_expression_types")
    if not isinstance(conditions, list) or not isinstance(
        predicate_types, list
    ):
        return []

    by_anchor: Dict[Tuple[int, int, int, int], Dict[str, Any]] = {}
    rejected = set()
    for raw_vc in conditions:
        vc = _as_dict(raw_vc)
        provenance = _as_dict(vc.get("provenance"))
        if (
            vc.get("kind") != "annotation"
            or provenance.get("kind") != "annotation"
        ):
            continue
        anchor = provenance.get("source_span")
        goal = _as_dict(vc.get("goal"))
        predicate_span = goal.get("source_span")
        related = provenance.get("related_spans")
        if (
            not _valid_raw_span(anchor, source_lines, expected_file)
            or not _valid_raw_span(
                predicate_span, source_lines, expected_file
            )
            or not isinstance(related, list)
            or not any(
                _as_dict(item).get("role") == "subject"
                and _valid_raw_span(
                    _as_dict(item).get("span"),
                    source_lines,
                    expected_file,
                )
                for item in related
            )
        ):
            continue
        anchor = _as_dict(anchor)
        predicate_span = _as_dict(predicate_span)

        holes = []
        for raw_entry in predicate_types:
            entry = _as_dict(raw_entry)
            location = entry.get("location")
            checked_type = entry.get("type")
            if (
                not _valid_raw_span(location, source_lines, expected_file)
                or not checked_type
                or not _raw_span_contains(predicate_span, location)
                or _raw_source_slice(location, source_lines) != "_"
            ):
                continue
            holes.append(str(checked_type))
        if len(holes) != 1:
            continue

        editor_anchor = _editor_span(anchor, source_lines)
        imposed = {
            str(entry.get("type"))
            for entry in expression_types
            if entry.get("type") is not None
            and _same_editor_span(entry, editor_anchor)
        }
        if len(imposed) != 1:
            continue
        key = (
            editor_anchor["start"]["line"],
            editor_anchor["start"]["col"],
            editor_anchor["end"]["line"],
            editor_anchor["end"]["col"],
        )
        fact = {
            "start": editor_anchor["start"],
            "end": editor_anchor["end"],
            "checked_type": holes[0],
            "imposed_type": next(iter(imposed)),
        }
        previous = by_anchor.get(key)
        if previous is not None and previous != fact:
            rejected.add(key)
        else:
            by_anchor[key] = fact

    return [fact for key, fact in by_anchor.items() if key not in rejected]


def identifier_modes(
    document: object, source_lines: Sequence[str]
) -> List[Dict[str, Any]]:
    """Mode-at-cursor ranges for identifier binders and reads.

    ``identifier_modes`` is emitted by the VC dump as ``{location, mode}``.
    Normalize its locations to the same editor coordinates as type ranges and
    preserve the compiler's user-facing mode string verbatim.
    """
    document = _as_dict(document)
    entries = document.get("identifier_modes")
    if not isinstance(entries, list):
        return []
    result: List[Dict[str, Any]] = []
    for entry in entries:
        entry = _as_dict(entry)
        location = entry.get("location")
        mode = entry.get("mode")
        if not mode:
            continue
        span = _normalize_emitted_span(
            location, {"input.ml": source_lines}, expected_file="input.ml"
        )
        if span is None:
            continue
        result.append(
            {"start": span["start"], "end": span["end"], "mode": str(mode)}
        )
    return result


def identifier_modes_by_file(
    document: object, lines_by_file: Mapping[str, Sequence[str]]
) -> List[Dict[str, Any]]:
    """File-tagged mode ranges for a multi-file workspace dump."""
    document = _as_dict(document)
    entries = document.get("identifier_modes")
    if not isinstance(entries, list):
        return []
    result: List[Dict[str, Any]] = []
    for entry in entries:
        entry = _as_dict(entry)
        location = entry.get("location")
        mode = entry.get("mode")
        if not mode:
            continue
        span = _normalize_emitted_span(location, lines_by_file)
        if span is None:
            continue
        result.append(
            {
                "file": span["file"],
                "start": span["start"],
                "end": span["end"],
                "mode": str(mode),
            }
        )
    return result


def _vcs_available(
    revision: int,
    vcs: List[Dict[str, Any]],
    hidden: int,
    obligation_summary: Optional[Dict[str, Any]] = None,
    refinement_type_ranges: Optional[List[Dict[str, Any]]] = None,
    identifier_mode_ranges: Optional[List[Dict[str, Any]]] = None,
    imposed_type_ranges: Optional[List[Dict[str, Any]]] = None,
    backend: str = "lean",
    options: Sequence[str] = ("lean",),
    ocamlc: Optional[str] = None,
    lemma_call_sites: Optional[List[Dict[str, Any]]] = None,
) -> Dict[str, Any]:
    """A completed dump: ``vcs`` is authoritative (an empty list is a genuine
    "no obligations").  ``hidden`` counts obligations dropped for want of a
    placeable source span, so the pane can report a count that would otherwise
    silently shrink.  ``refinement_types`` are the per-subterm cursor types of
    the buffer's refinement predicates; ``identifier_modes`` cover identifier
    binders and reads.  Both cursor channels are additive and empty when the
    compiler reports none."""
    return {
        "revision": revision,
        "vcs": vcs,
        "unavailable": False,
        "hidden": hidden,
        "obligation_summary": obligation_summary
        or _obligation_summary([], []),
        "unavailable_reason": None,
        "refinement_types": refinement_type_ranges or [],
        "identifier_modes": identifier_mode_ranges or [],
        "imposed_types": imposed_type_ranges or [],
        # ``None`` (not ``[]``) when the compiler reported no lemma-call
        # channel or one that would not place: absent evidence, not evidence
        # that the buffer holds no such call.
        "lemma_calls": lemma_call_sites,
        "backend": backend,
        "backend_options": list(options),
        "backend_solver_configuration": backend_solver_configuration(ocamlc),
    }


def _vcs_unavailable(
    revision: int,
    backend: str = "lean",
    options: Sequence[str] = ("lean",),
    reason: str = "unknown",
    ocamlc: Optional[str] = None,
) -> Dict[str, Any]:
    """No trustworthy VC data (the compile did not run, or the dump was
    missing, unreadable, or malformed).  Distinct from an empty ``vcs``: the
    pane must not present this as "no obligations"."""
    return {
        "revision": revision,
        "vcs": [],
        "unavailable": True,
        "hidden": 0,
        "obligation_summary": _obligation_summary([], []),
        "unavailable_reason": reason,
        "refinement_types": [],
        "identifier_modes": [],
        "imposed_types": [],
        "lemma_calls": None,
        "backend": backend,
        "backend_options": list(options),
        "backend_solver_configuration": backend_solver_configuration(ocamlc),
    }


def _vcs_from_dump_path(
    dump_path: Path,
    source_lines: Sequence[str],
    returncode: int,
    revision: int,
    backend: str,
    options: Sequence[str],
    expression_types: Sequence[Dict[str, Any]] = (),
    ocamlc: Optional[str] = None,
) -> Dict[str, Any]:
    """Translate one completed compile's VC sidecar into the frontend shape."""
    if not dump_path.is_file():
        return _vcs_unavailable(
            revision,
            backend,
            options,
            reason="compiler-lacks-vc-data",
            ocamlc=ocamlc,
        )
    try:
        document = json.loads(dump_path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return _vcs_unavailable(
            revision, backend, options, reason="malformed-vc-data", ocamlc=ocamlc
        )
    conditions = (
        document.get("verification_conditions")
        if isinstance(document, dict)
        else None
    )
    if not isinstance(conditions, list):
        return _vcs_unavailable(
            revision, backend, options, reason="malformed-vc-data", ocamlc=ocamlc
        )
    vcs: List[Dict[str, Any]] = []
    placeable: List[bool] = []
    for raw in conditions:
        if not isinstance(raw, dict):
            placeable.append(False)
            continue
        translated = translate_vc(raw, len(vcs), source_lines)
        placeable.append(translated is not None)
        if translated is not None:
            vcs.append(translated)
    hidden = len(conditions) - len(vcs)
    if returncode != 0 and not conditions:
        return _vcs_unavailable(
            revision, backend, options, reason="check-failed", ocamlc=ocamlc
        )
    return _vcs_available(
        revision,
        vcs,
        hidden,
        _obligation_summary(conditions, placeable),
        refinement_types(document, source_lines),
        identifier_modes(document, source_lines),
        imposed_types(
            document, source_lines, expression_types, expected_file="input.ml"
        ),
        backend,
        options,
        ocamlc,
        lemma_calls(
            document, {"input.ml": source_lines}, expected_file="input.ml"
        ),
    )


def vcs_for_source(
    source: str, revision: int, ocamlc: str, backend: str = "lean"
) -> Dict[str, Any]:
    """Run the vox2 compiler's VC dump on one buffer and translate it.

    This is a single verification-capable compile (``-c`` with the dump flag);
    it needs neither ``-annot`` nor ``-i``, so it costs one compiler run.  A
    completed dump yields ``unavailable: false`` (even with an empty ``vcs``);
    a failure to run, dump, read, or parse yields ``unavailable: true`` so the
    pane never mistakes a crashed dump for a program with no obligations.
    """
    options = backend_options(ocamlc)
    if backend == NO_VERIFICATION_BACKEND:
        return _vcs_unavailable(
            revision,
            backend,
            options,
            reason="verification-not-run",
            ocamlc=ocamlc,
        )
    if not source.strip():
        return _vcs_available(
            revision,
            [],
            0,
            _obligation_summary([], []),
            backend=backend,
            options=options,
            ocamlc=ocamlc,
        )
    source_lines = source.split("\n")
    with tempfile.TemporaryDirectory(prefix="voxide-vcs-") as scratch:
        (Path(scratch) / "input.ml").write_text(source, encoding="utf-8")
        dump_path = Path(scratch) / "vcs.json"
        try:
            checked = _run(
                ocamlc,
                [
                    *_backend_arguments(ocamlc, backend),
                    "-c",
                    "-vox-dump-vc-json",
                    "vcs.json",
                    "input.ml",
                ],
                scratch,
            )
        except (OSError, subprocess.TimeoutExpired) as exc:
            reason = (
                "timeout"
                if isinstance(exc, subprocess.TimeoutExpired)
                else "compiler-unavailable"
            )
            return _vcs_unavailable(
                revision, backend, options, reason=reason, ocamlc=ocamlc
            )
        # The sidecar is written at process exit even when a verification
        # failure aborts the compile.  Translate it before the temporary
        # directory is removed.
        return _vcs_from_dump_path(
            dump_path,
            source_lines,
            checked.returncode,
            revision,
            backend,
            options,
            ocamlc=ocamlc,
        )


# ---------------------------------------------------------------------------
# Multi-file workspace check (slice 6).
#
# A workspace is a set of editable buffers (``Foo.mli``, ``Foo.ml``, ...)
# compiled *together* in one ``ocamlc -c`` invocation, in dependency order.
# Because the VC dump accumulates process-globally and every span carries a
# ``file`` field (schema v2), a single compile over the whole set yields one
# unified document spanning all units, each obligation routable back to its
# buffer.  Compiling the units together also makes ``.ml``/``.mli`` conformance
# real (the earlier unit's ``.cmi`` is written before the next unit is typed),
# so a seal/refinement mismatch surfaces as an ordinary located error, and
# cross-unit references resolve against the sibling ``.cmi``.
#
# The server stays a pure, stateless function of the request: the client sends
# the full buffer set each time (no server-side buffer state).  This mirrors
# the single-buffer discipline above; ``check_source``/``vcs_for_source`` are
# left untouched so the single-buffer path is unaffected.
# ---------------------------------------------------------------------------

# Extensions a workspace buffer may carry.  ``.md`` docs are client-side only
# and never reach a compile set (as in the single-buffer path), so only the
# two compilation-unit extensions are accepted here.
_WORKSPACE_EXT = frozenset((".ml", ".mli"))

# A compilation-unit filename validated before it becomes a filesystem path:
# a bare basename (no directory separators, no ``..``), a servable extension,
# and a stem that is a legal OCaml compilation-unit name (the module identity a
# cross-reference resolves is the capitalized stem).  This is new attack
# surface the single fixed ``input.ml`` never had.
_UNIT_STEM = re.compile(r"^[A-Za-z][A-Za-z0-9_']*$")

# File-attributing variants of the diagnostic-location regexes: the ``File
# "<name>"`` prefix is *captured* (the single-buffer ``_FILE`` discards it) so
# each diagnostic can be routed to the buffer it belongs to.  Leading
# whitespace is tolerated because a conformance error prints indented
# ``File "Foo.mli", ...`` sub-locations.
_FILE_CAP = r'\s*(?:File "([^"]*)", )?'
_LOC_SINGLE_F = re.compile(_FILE_CAP + r"[Ll]ine (\d+), characters (\d+)-(\d+):")
_LOC_MULTI_F = re.compile(_FILE_CAP + r"[Ll]ines (\d+)-(\d+), characters (\d+)-(\d+):")
# A bare ``File "Foo.ml", line N:`` header with no character range (printed as
# the primary location of a module-conformance error): carries the file for
# routing even though it gives no column span.
_LOC_FILEONLY = re.compile(r'\s*File "([^"]*)", [Ll]ine[s]? (\d+)')


def _valid_unit_name(name: object) -> bool:
    """Whether ``name`` is a safe, servable compilation-unit basename."""
    if not isinstance(name, str) or not name:
        return False
    if name != os.path.basename(name):
        return False
    if "/" in name or "\\" in name or "\x00" in name or name in (".", ".."):
        return False
    stem, ext = os.path.splitext(name)
    if ext not in _WORKSPACE_EXT:
        return False
    return _UNIT_STEM.match(stem) is not None


def _build_order(names: Sequence[str]) -> List[str]:
    """Order the buffer set for a single ``-c`` compile (slice 1).

    Interfaces are listed before their implementations so ``ocamlc -c`` writes
    each ``Foo.cmi`` from ``Foo.mli`` before typing ``Foo.ml`` against it;
    distinct modules keep their given (client-declared) order, which for the
    fixed demo set is already dependency order.  Scanned (``ocamldep``) ordering
    for an arbitrary N-file workspace is deferred to slice 2.
    """
    stems: List[str] = []
    for name in names:
        stem = os.path.splitext(name)[0]
        if stem not in stems:
            stems.append(stem)
    return sorted(
        names,
        key=lambda n: (
            stems.index(os.path.splitext(n)[0]),
            0 if n.endswith(".mli") else 1,
        ),
    )


def _loc_with_file(
    header: str, lines_by_file: Mapping[str, Sequence[str]]
) -> Optional[Tuple[Optional[str], Dict[str, int], Dict[str, int]]]:
    """Parse a located diagnostic header, capturing its file.

    Returns ``(file, start, end)`` with 0-based UTF-16 columns computed against
    that file's own lines (falling back to the raw byte column if the file is
    not one of the open buffers), or ``None`` if the header carries no
    line/character location.
    """
    match = _LOC_SINGLE_F.match(header)
    if match is not None:
        file = match.group(1)
        lines = lines_by_file.get(file or "", [])
        line = int(match.group(2)) - 1
        return (
            file,
            {"line": line, "col": _utf16_col(lines, line, int(match.group(3)))},
            {"line": line, "col": _utf16_col(lines, line, int(match.group(4)))},
        )
    match = _LOC_MULTI_F.match(header)
    if match is not None:
        file = match.group(1)
        lines = lines_by_file.get(file or "", [])
        start_line = int(match.group(2)) - 1
        end_line = int(match.group(3)) - 1
        return (
            file,
            {
                "line": start_line,
                "col": _utf16_col(lines, start_line, int(match.group(4))),
            },
            {"line": end_line, "col": _utf16_col(lines, end_line, int(match.group(5)))},
        )
    return None


def parse_all_errors(
    text: str,
    lines_by_file: Mapping[str, Sequence[str]],
    default_file: str,
) -> List[Dict[str, Any]]:
    """Extract every top-level OCaml error, each attributed to its file.

    Unlike the single-buffer ``parse_any_error`` (which returns only the first
    error), this returns all of them so an error in one unit never hides an
    error in another.  Each error's ``file`` comes from the nearest preceding
    ``File "..."`` header (or the located header), defaulting to
    ``default_file`` (the active buffer) when the compiler printed none.
    """
    out: List[Dict[str, Any]] = []
    lines = text.splitlines()
    for index, line in enumerate(lines):
        error_match = _ERROR.match(line)
        if error_match is None:
            continue
        file: Optional[str] = None
        location: Optional[Tuple[Dict[str, int], Dict[str, int]]] = None
        for previous in range(index - 1, -1, -1):
            if _ERROR.match(lines[previous]) is not None:
                break
            loc = _loc_with_file(lines[previous], lines_by_file)
            if loc is not None:
                if loc[0] and file is None:
                    file = loc[0]
                if location is None:
                    location = (loc[1], loc[2])
            if file is None:
                only = _LOC_FILEONLY.match(lines[previous])
                if only is not None:
                    file = only.group(1)
            if file is not None and location is not None:
                break
        message = [error_match.group(1).strip()]
        for continuation in lines[index + 1 :]:
            if continuation.startswith((" ", "\t")) and continuation.strip():
                message.append(continuation.strip())
            else:
                break
        error: Dict[str, Any] = {
            "message": " ".join(message),
            "file": file or default_file,
        }
        if location is not None:
            error["start"], error["end"] = location
        error["kind"] = _error_kind(error["message"])
        out.append(error)
    return out


def _editor_span_f(
    span: Dict[str, Any], lines_by_file: Mapping[str, Sequence[str]]
) -> Dict[str, Any]:
    """Convert a schema span (carrying its own ``file``) to editor coordinates.

    Unlike the single-buffer ``_editor_span``, each span is converted against
    *its own* file's lines -- a hypothesis can originate in a different unit
    than the VC it discharges -- and the resulting editor span keeps its
    ``file`` so the frontend can route the mark (and jump across units).
    """
    file = span.get("file")
    lines = lines_by_file.get(str(file) if file is not None else "", [])
    return {
        "file": str(file) if file is not None else None,
        "start": _editor_point(_as_dict(span.get("start")), lines),
        "end": _editor_point(_as_dict(span.get("end")), lines),
    }


def _hypothesis_f(
    fact: object,
    lines_by_file: Mapping[str, Sequence[str]],
    index: int = 0,
    per_backend: Optional[Mapping[str, Optional[List[int]]]] = None,
) -> Dict[str, Any]:
    """One fact as a hypothesis, with its origin span converted per-file."""
    fact = _as_dict(fact)
    predicate = _predicate(fact)
    origin = _as_dict(fact.get("origin"))
    name = origin.get("name")
    kind = origin.get("kind")
    span = origin.get("span")
    editor_span = None
    normalized = _normalize_emitted_span(span, lines_by_file)
    if normalized is not None:
        editor_span = normalized
    hypothesis = {
        "name": str(name) if name else None,
        "kind": str(kind) if kind else None,
        "display": predicate["display"],
        "raw": predicate["raw"],
        "span": editor_span,
    }
    if "used" in fact and isinstance(fact.get("used"), bool):
        hypothesis["used"] = fact["used"]
    hypothesis["producers"] = _fact_producers(fact, lines_by_file)
    hypothesis["used_by"] = _used_by(index, per_backend)
    return hypothesis


def _translate_vc_f(
    vc: Dict[str, Any], index: int, lines_by_file: Mapping[str, Sequence[str]]
) -> Optional[Dict[str, Any]]:
    """Translate one VC, tagging it (and each hypothesis span) with its file."""
    anchor = _vc_anchor_span(vc, lines_by_file)
    if anchor is None:
        return None
    editor_anchor = anchor
    discharge = _as_dict(vc.get("discharge"))
    status = _VC_STATUS.get(str(discharge.get("status", "")), "unknown")
    raw_kind = str(vc.get("kind", ""))
    facts = vc.get("facts")
    facts = facts if isinstance(facts, list) else []
    counterexample = discharge.get("counterexample")
    return {
        "id": index,
        "file": editor_anchor.get("file"),
        "status": status,
        "kind": _VC_KIND.get(raw_kind, raw_kind),
        "span": {"start": editor_anchor["start"], "end": editor_anchor["end"]},
        "goal": _predicate(vc.get("goal")),
        "hypotheses": [
            _hypothesis_f(
                fact, lines_by_file, index, _backend_unused_facts(discharge)
            )
            for index, fact in enumerate(facts)
        ],
        "counterexample": [counterexample] if counterexample else None,
        "detail": _scrub_detail(discharge.get("detail")),
        "generated_lean": vc.get("generated_lean") or None,
        "backends": _backend_results(discharge),
    }


def _file_verification(
    errors: Sequence[Dict[str, Any]],
    summary: Mapping[str, Any],
    unavailable: bool = False,
) -> Dict[str, Any]:
    """Summarize one unit from all its VCs, including unplaceable ones."""
    total = int(summary.get("total", 0) or 0)
    statuses = summary.get("statuses", {})
    statuses = statuses if isinstance(statuses, dict) else {}
    verify_errors = [e for e in errors if e.get("kind") == "verification"]
    if verify_errors:
        return {
            "status": "failed",
            "message": verify_errors[0]["message"],
            "obligations": True,
        }
    if errors:
        return {
            "status": "blocked",
            "message": "Verification runs once the type errors are fixed.",
            "obligations": total > 0,
        }
    if unavailable:
        return {
            "status": "unavailable",
            "message": "Obligation data unavailable.",
            "obligations": total > 0,
        }
    if total == 0:
        return {
            "status": "none",
            "message": "No refinements to verify.",
            "obligations": False,
        }
    if int(statuses.get("proved", 0) or 0) != total:
        return {
            "status": "failed",
            "message": "Some obligations were not discharged.",
            "obligations": True,
        }
    return {
        "status": "verified",
        "message": "All refinement obligations discharged.",
        "obligations": True,
    }


def _workspace_verification(per_file: Dict[str, Dict[str, Any]]) -> Dict[str, Any]:
    """Fold the per-unit outcomes into one workspace-wide verdict."""
    statuses = [entry["verification"]["status"] for entry in per_file.values()]
    if "not-run" in statuses:
        return {
            "status": "not-run",
            "message": "Workspace typecheck completed; verification was not run.",
            "obligations": False,
        }
    if "unavailable" in statuses:
        return {
            "status": "unavailable",
            "message": "Obligation data unavailable for a unit.",
            "obligations": True,
        }
    if "failed" in statuses:
        return {
            "status": "failed",
            "message": "A unit did not verify.",
            "obligations": True,
        }
    if "blocked" in statuses:
        return {
            "status": "blocked",
            "message": "Fix the type errors to verify.",
            "obligations": True,
        }
    if "verified" in statuses:
        return {
            "status": "verified",
            "message": "All units verified.",
            "obligations": True,
        }
    return {
        "status": "none",
        "message": "No refinements to verify.",
        "obligations": False,
    }


def _raw_vc_file(raw: object) -> Optional[str]:
    """Best available emitted file identity, independent of span placeability."""
    raw = raw if isinstance(raw, dict) else {}
    candidates = [raw.get("location"), raw.get("program_point")]
    goal = raw.get("goal")
    if isinstance(goal, dict):
        candidates.append(goal.get("source_span"))
    for span in candidates:
        if isinstance(span, dict) and isinstance(span.get("file"), str):
            return str(span["file"])
    return None


def _summary_for_file(
    conditions: Sequence[object], placeable: Sequence[bool], file: str
) -> Dict[str, Any]:
    selected = []
    selected_placeable = []
    for index, raw in enumerate(conditions):
        if _raw_vc_file(raw) == file:
            selected.append(raw)
            selected_placeable.append(index < len(placeable) and placeable[index])
    return _obligation_summary(selected, selected_placeable)


def _outcome_for_unit(
    errors: Sequence[Dict[str, Any]], summary: Mapping[str, Any], unavailable: bool
) -> Dict[str, Any]:
    for kind in ("syntax", "type-mode"):
        matching = [error for error in errors if error.get("kind") == kind]
        if matching:
            error = matching[0]
            return _outcome(kind, str(error.get("message", "")), "start" in error)
    for kind in (
        "backend-unavailable",
        "compiler-unavailable",
        "compiler-crashed",
        "timeout",
    ):
        matching = [error for error in errors if error.get("kind") == kind]
        if matching:
            return _outcome(kind, str(matching[0].get("message", "")))
    statuses = summary.get("statuses", {})
    statuses = statuses if isinstance(statuses, dict) else {}
    if int(statuses.get("unavailable", 0) or 0) > 0:
        return _outcome("backend-unavailable", "The selected backend was unavailable.")
    if any(error.get("kind") == "verification" for error in errors):
        error = next(error for error in errors if error.get("kind") == "verification")
        return _outcome(
            "verification", str(error.get("message", "")), "start" in error
        )
    total = int(summary.get("total", 0) or 0)
    if total and int(statuses.get("proved", 0) or 0) != total:
        return _outcome("verification", "Some obligations were not discharged.")
    if unavailable:
        return _outcome("compiler-crashed", "Obligation data unavailable.")
    return _outcome("ok")


def _workspace_unavailable_response(
    names: Sequence[str],
    active: str,
    revision: int,
    kind: str,
    reason: str,
    message: str,
) -> Dict[str, Any]:
    return {
        "revision": revision,
        "ok": False,
        "outcome": _outcome(kind, message),
        "active": active,
        "files": {
            name: {
                "errors": [{"message": message, "kind": kind, "file": name}],
                "outcome": _outcome(kind, message),
                "verification": {
                    "status": "unavailable",
                    "message": message,
                    "obligations": False,
                },
                "obligation_summary": _obligation_summary([], []),
                **(
                    {
                        "signature": {
                            "status": "not-requested",
                            "text": "",
                            "error": "",
                        }
                    }
                    if name == active
                    else {}
                ),
            }
            for name in names
        },
        "vcs": [],
        "unavailable": True,
        "unavailable_reason": reason,
        "hidden": 0,
        "obligation_summary": _obligation_summary([], []),
        "identifier_modes": [],
        "refinement_types": [],
        "workspace_verification": {
            "status": "unavailable",
            "message": message,
            "obligations": False,
        },
    }


def check_workspace(
    files: Sequence[Dict[str, Any]],
    active: str,
    revision: int,
    ocamlc: str,
    backend: str = "lean",
    cancel_check: Optional[Callable[[], bool]] = None,
) -> Dict[str, Any]:
    """Typecheck + verify a set of buffers compiled together, results per file.

    One ``-c -annot -vox-dump-vc-json`` invocation over the ordered set (the
    three flags compose, so diagnostics, per-unit ``.annot`` types, and the
    unified VC dump come from a single compile).  Signature inference is a
    separate presentation-only request.  Every diagnostic and VC is tagged
    with the unit it belongs to.
    """
    names = [str(f.get("name", "")) for f in files]
    if (
        not names
        or not all(_valid_unit_name(n) for n in names)
        or len(set(names)) != len(names)
    ):
        response = _workspace_unavailable_response(
            [],
            active,
            revision,
            "invalid-request",
            "check-failed",
            "invalid workspace file set",
        )
        response["error"] = "invalid workspace file set"
        return response
    if active not in names:
        active = names[0]

    sources = {str(f.get("name")): str(f.get("source", "")) for f in files}
    lines_by_file = {name: src.split("\n") for name, src in sources.items()}
    order = _build_order(names)
    active_stem = os.path.splitext(active)[0]

    with tempfile.TemporaryDirectory(prefix="voxide-ws-") as scratch:
        for name in names:
            (Path(scratch) / name).write_text(sources[name], encoding="utf-8")
        dump_path = Path(scratch) / "vcs.json"
        type_only = backend == NO_VERIFICATION_BACKEND
        try:
            checked = _run(
                ocamlc,
                [
                    *_backend_arguments(ocamlc, backend),
                    "-c",
                    "-annot",
                    *(
                        []
                        if type_only
                        else ["-vox-dump-vc-json", "vcs.json"]
                    ),
                    *order,
                ],
                scratch,
                cancel_check=cancel_check,
            )
        except CompileCancelled:
            raise
        except subprocess.TimeoutExpired:
            return _workspace_unavailable_response(
                names,
                active,
                revision,
                "timeout",
                "timeout",
                "The compiler check timed out.",
            )
        except OSError as exc:
            message = f"could not run vox2 compiler: {exc}"
            return _workspace_unavailable_response(
                names,
                active,
                revision,
                "compiler-unavailable",
                "compiler-unavailable",
                message,
            )

        diagnostics = checked.stderr + checked.stdout
        errors = parse_all_errors(diagnostics, lines_by_file, active)

        # A nonzero exit with no located ``Error:`` line is a crash the parser
        # cannot attribute (e.g. ``Fatal error: ...``, or a compiler assertion).
        # Mirror the single-buffer guard (check_source): surface it as an error
        # so the run is not-ok, rather than letting an empty ``errors`` paint a
        # false green ("workspace typechecks ✓").  It is charged to the active
        # unit since the failure is not tied to a printed file.
        if not errors and checked.returncode != 0:
            detail = diagnostics.strip() or f"compiler exited {checked.returncode}"
            errors.append(
                {"message": detail, "file": active, "kind": "compiler-crashed"}
            )

        # The active implementation's inferred expression types (.annot).  An
        # active .mli has none; presentation-only signature inference is off this
        # authoritative request's critical path.
        types: List[Dict[str, Any]] = []
        if active.endswith(".ml"):
            annot_path = Path(scratch) / (active_stem + ".annot")
            if annot_path.is_file():
                types = parse_annot(
                    annot_path.read_text(encoding="utf-8"),
                    lines_by_file.get(active, []),
                )
        # The VC dump: written at process exit even when a verification failure
        # aborts the compile, so a failing workspace still yields the VCs
        # discharged before the abort.  Its absence means the run did not
        # complete normally.
        vcs: List[Dict[str, Any]] = []
        hidden = 0
        unavailable = False
        unavailable_reason: Optional[str] = None
        obligation_summary = _obligation_summary([], [])
        identifier_mode_ranges: List[Dict[str, Any]] = []
        refinement_type_ranges: List[Dict[str, Any]] = []
        active_imposed_types: List[Dict[str, Any]] = []
        conditions: List[object] = []
        placeable: List[bool] = []
        lemma_call_sites: Optional[List[Dict[str, Any]]] = None
        if type_only:
            unavailable = True
            unavailable_reason = "verification-not-run"
        elif dump_path.is_file():
            try:
                document = json.loads(dump_path.read_text(encoding="utf-8"))
            except (OSError, ValueError):
                document = None
            conditions = (
                document.get("verification_conditions")
                if isinstance(document, dict)
                else None
            )
            if isinstance(conditions, list):
                for raw in conditions:
                    if not isinstance(raw, dict):
                        placeable.append(False)
                        continue
                    translated = _translate_vc_f(raw, len(vcs), lines_by_file)
                    placeable.append(translated is not None)
                    if translated is not None:
                        vcs.append(translated)
                hidden = len(conditions) - len(vcs)
                obligation_summary = _obligation_summary(conditions, placeable)
                identifier_mode_ranges = identifier_modes_by_file(
                    document, lines_by_file
                )
                refinement_type_ranges = refinement_types_by_file(
                    document, lines_by_file
                )
                lemma_call_sites = lemma_calls(document, lines_by_file)
                if active.endswith(".ml"):
                    active_imposed_types = imposed_types(
                        document,
                        lines_by_file.get(active, []),
                        types,
                        expected_file=active,
                    )
            else:
                unavailable = True
                unavailable_reason = "malformed-vc-data"
                conditions = []
        else:
            unavailable = True
            unavailable_reason = "compiler-lacks-vc-data"

    # H2 (deferred, slice 1): the verdict folds only over the *open* units.  A
    # VC whose anchor file is not one of the buffers (e.g. an obligation that
    # lands in a staged external dependency) is not reflected in any unit's
    # verdict here -- it is not reachable in the editable-only slice-1 set (every
    # compiled unit is an open buffer), but a later slice that stages external
    # deps must fold such VCs in (or surface them via a cross_unit channel)
    # before trusting "verified".
    per_file: Dict[str, Dict[str, Any]] = {}
    unattributed_hidden = any(
        not (index < len(placeable) and placeable[index])
        and _raw_vc_file(raw) not in names
        for index, raw in enumerate(conditions)
    )
    for name in names:
        file_errors = [e for e in errors if e.get("file") == name]
        file_summary = _summary_for_file(conditions, placeable, name)
        file_unavailable = unavailable or unattributed_hidden
        file_outcome = _outcome_for_unit(
            file_errors, file_summary, file_unavailable and not type_only
        )
        if type_only and file_outcome["kind"] == "ok":
            file_outcome = _outcome("checked-no-verification")
        entry: Dict[str, Any] = {
            "errors": file_errors,
            "outcome": file_outcome,
            "obligation_summary": file_summary,
            "verification": (
                _type_only_verification(file_errors)
                if type_only
                else _file_verification(file_errors, file_summary, file_unavailable)
            ),
        }
        if name == active:
            entry["types"] = types
            entry["signature"] = {
                "status": "not-requested",
                "text": "",
                "error": "",
            }
            entry["imposed_types"] = active_imposed_types
        per_file[name] = entry

    outcome = _primary_outcome(
        checked,
        errors,
        {"obligation_summary": obligation_summary},
    )
    if type_only and outcome["kind"] == "ok":
        outcome = _outcome("checked-no-verification")
    elif unavailable and outcome["kind"] == "ok":
        outcome = _outcome("compiler-crashed", "Obligation data unavailable.")
    reason_for_outcome = _unavailable_reason_for_outcome(outcome["kind"])
    if unavailable and not type_only and reason_for_outcome is not None:
        unavailable_reason = reason_for_outcome

    return {
        "revision": revision,
        "backend": backend,
        "backend_options": list(backend_options(ocamlc)),
        "backend_solver_configuration": backend_solver_configuration(ocamlc),
        "active": active,
        "ok": outcome["kind"] in ("ok", "checked-no-verification"),
        "outcome": outcome,
        "files": per_file,
        "vcs": vcs,
        "unavailable": unavailable,
        "unavailable_reason": unavailable_reason,
        "hidden": hidden,
        "obligation_summary": obligation_summary,
        "identifier_modes": identifier_mode_ranges,
        "refinement_types": refinement_type_ranges,
        "lemma_calls": lemma_call_sites,
        "workspace_verification": _workspace_verification(per_file),
    }


def signature_for_workspace(
    files: Sequence[Dict[str, Any]],
    active: str,
    revision: int,
    ocamlc: str,
    backend: str = "lean",
    cancel_check: Optional[Callable[[], bool]] = None,
) -> Dict[str, Any]:
    """Presentation-only workspace signature, guarded by active unit/revision."""
    names = [str(file.get("name", "")) for file in files]
    if (
        not names
        or not all(_valid_unit_name(name) for name in names)
        or len(set(names)) != len(names)
        or active not in names
    ):
        return {
            "revision": revision,
            "active": active,
            "backend": backend,
            "signature": {
                "status": "unavailable",
                "text": "",
                "error": "Invalid workspace file set.",
            },
        }
    if active.endswith(".mli"):
        return {
            "revision": revision,
            "active": active,
            "backend": backend,
            "signature": {"status": "interface", "text": "", "error": ""},
        }
    sources = {str(file["name"]): str(file.get("source", "")) for file in files}
    with tempfile.TemporaryDirectory(prefix="voxide-ws-signature-") as scratch:
        for name, source in sources.items():
            (Path(scratch) / name).write_text(source, encoding="utf-8")
        try:
            prepared = _run(
                ocamlc,
                [*_backend_arguments(ocamlc, backend), "-c", *_build_order(names)],
                scratch,
                cancel_check=cancel_check,
            )
            if prepared.returncode != 0:
                detail = (prepared.stderr + prepared.stdout).strip()
                signature = {
                    "status": "unavailable",
                    "text": "",
                    "error": detail or "Workspace preparation failed.",
                }
            else:
                inferred = _run(
                    ocamlc,
                    [*_backend_arguments(ocamlc, backend), "-i", active],
                    scratch,
                    cancel_check=cancel_check,
                )
                if inferred.returncode == 0:
                    text = extract_signature(inferred.stdout)
                    signature = {
                        "status": "available" if text else "empty",
                        "text": text,
                        "error": "",
                    }
                else:
                    detail = (inferred.stderr + inferred.stdout).strip()
                    signature = {
                        "status": "unavailable",
                        "text": "",
                        "error": detail or "Module signature inference failed.",
                    }
        except CompileCancelled:
            raise
        except subprocess.TimeoutExpired:
            signature = {
                "status": "unavailable",
                "text": "",
                "error": "Signature inference timed out.",
            }
        except OSError as exc:
            signature = {
                "status": "unavailable",
                "text": "",
                "error": f"Could not run signature inference: {exc}",
            }
    return {
        "revision": revision,
        "active": active,
        "backend": backend,
        "signature": signature,
    }
