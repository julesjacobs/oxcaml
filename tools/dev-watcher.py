#!/usr/bin/env python3

# The non-watcher utilities of the dev loop. The background watcher this file
# was named for is gone (the loop is synchronous; see
# design-docs/dev-loop-sync.md); what remains is what the loop still uses:
#   stop               kill a watcher left running by a pre-synchronous tree
#                      (legacy migration; a no-op otherwise)
#   prepare-test-root  compose the dev test root under _build/dev/runtest
#   diff               show a test's newest fresh output against its reference

import argparse
import fcntl
import os
from pathlib import Path
import shutil
import signal
import subprocess
import sys
import time


if sys.version_info < (3, 7):
    raise SystemExit(
        "dev watcher: python 3.7 or newer is required "
        f"(running {sys.version.split()[0]} from {sys.executable}); "
        "put a newer python3 first on PATH"
    )


ROOT = Path(__file__).resolve().parent.parent
STATE = ROOT / "_build" / "dev"
PID_FILE = STATE / "watcher.pid"
CHILD_PID_FILE = STATE / "dune.pid"
LOCK_FILE = STATE / "lock"


def read_pid(path):
    try:
        return int(path.read_text().strip())
    except (FileNotFoundError, ValueError):
        return None


def alive(pid):
    if pid is None:
        return False
    try:
        os.kill(pid, 0)
        return True
    except ProcessLookupError:
        return False
    except PermissionError:
        return True


def locked():
    STATE.mkdir(parents=True, exist_ok=True)
    lock = LOCK_FILE.open("a+")
    fcntl.flock(lock, fcntl.LOCK_EX)
    return lock


def clean_stale_state():
    if not alive(read_pid(PID_FILE)):
        PID_FILE.unlink(missing_ok=True)
        CHILD_PID_FILE.unlink(missing_ok=True)


def stop_watcher():
    with locked():
        clean_stale_state()
        pid = read_pid(PID_FILE)
        if pid is None:
            return
        try:
            os.kill(pid, signal.SIGTERM)
        except ProcessLookupError:
            clean_stale_state()
            return

    deadline = time.monotonic() + 7
    while time.monotonic() < deadline and alive(pid):
        time.sleep(0.05)
    if alive(pid):
        raise SystemExit(f"dev watcher {pid} did not stop")
    print("dev: watcher stopped")


def stop(_args):
    stop_watcher()


def announce(message):
    print(f"dev: {message}", flush=True)


# Where a test's fresh output can end up. dev-test uses the dev root; dev-test-all
# uses _runtest; and the promote path for a whole directory runs ocamltest with
# OCAMLTESTDIR under the test's own directory, which the dev root symlinks back
# into the source tree.
ARTIFACT_ROOTS = ("_build/dev/runtest/testsuite", "_runtest/testsuite", "testsuite")


def newest(paths):
    return max(paths, key=lambda path: path.stat().st_mtime)


def within(path, parts):
    """Whether [parts] occurs as a contiguous run of directories in [path]."""
    candidate = path.parts
    return any(
        candidate[index:index + len(parts)] == parts
        for index in range(len(candidate) - len(parts) + 1)
    )


def find_artifacts(directory, patterns):
    """Newest artifact matching one of [patterns], from within the test's own
    directory under each artifact root.

    Scoping to that directory is not optional: 609 of this tree's test files share
    a basename with a test elsewhere (test.ml alone occurs 83 times), so a
    basename search across the roots readily finds an unrelated test's output. The
    directory is matched anywhere in the path rather than as a prefix, because
    each root nests it differently and ocamltest adds a per-test level.
    """
    parts = Path(directory).parts
    for pattern in patterns:
        found = [
            path
            for root in ARTIFACT_ROOTS
            for base in [ROOT / root]
            if base.is_dir()
            for path in base.rglob(pattern)
            if path.is_file() and within(path, parts)
        ]
        if found:
            return newest(found)
    return None


def diff(args):
    """Show a test's newest fresh output against what it is compared to.

    Expect tests are the subtle case: the -principal pass writes
    <test>.corrected.corrected, which supersedes <test>.corrected, so promoting
    the latter by hand drops the principal updates silently.
    """
    source = ROOT / "testsuite" / args.test
    if not source.is_file():
        announce(f"no such test: {source}")
        return 2
    stem = source.name.rsplit(".", 1)[0]
    # args.test is "tests/<dir>/<file>.ml"; artifacts sit under the same
    # "tests/<dir>" path within whichever root ran the test.
    directory = str(Path(args.test).parent)

    corrected = find_artifacts(
        directory,
        [f"{source.name}.corrected.corrected", f"{source.name}.corrected"],
    )
    if corrected is not None:
        announce(f"corrected output {corrected.relative_to(ROOT)}")
        announce("promote with `make dev-promote`, never by copying this file")
        return show_diff(source, corrected)

    output = find_artifacts(directory, [f"{stem}.output", f"{stem}.result"])
    if output is None:
        announce(f"no fresh output for {source.relative_to(ROOT)}")
        announce("run `make dev-test TEST=...` first; note that prepare-test-root")
        announce("discards the previous run's artifacts")
        return 1

    announce(f"program output {output.relative_to(ROOT)}")
    reference = source.parent / f"{stem}.reference"
    if not reference.is_file():
        announce(f"no reference {reference.relative_to(ROOT)} yet; the output is:")
        print(output.read_text(errors="replace"), end="")
        return 0
    return show_diff(reference, output)


def show_diff(reference, output):
    subprocess.run(["diff", "-u", str(reference), str(output)], cwd=ROOT)
    return 0


def link(source, destination):
    destination.symlink_to(
        source.resolve(), target_is_directory=source.is_dir()
    )


def prepare_test_root_locked():
    source_root = ROOT / "_runtest"
    runtime_stdlib = (
        ROOT
        / "_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib"
    )
    if not source_root.is_dir() or not runtime_stdlib.is_dir():
        raise SystemExit(
            "dev: run `make install` once before using development tests"
        )

    destination = STATE / "runtest"
    temporary = STATE / "runtest.new"
    shutil.rmtree(temporary, ignore_errors=True)
    temporary.mkdir(parents=True)

    overridden = {
        "ocamlc",
        "ocamlc.byte",
        "ocamlc.opt",
        "ocamlopt",
        "ocamlopt.byte",
        "ocamlopt.opt",
        "ocamlrun",
        "ocamlrund",
        "ocamlruni",
        "ocamltest",
        "runtime",
        "stdlib",
        "testsuite",
    }
    for entry in source_root.iterdir():
        if entry.name not in overridden:
            link(entry, temporary / entry.name)

    link(ROOT / "_build/dev-dune/default/main.bc", temporary / "ocamlc.byte")
    (temporary / "ocamlc").symlink_to("ocamlc.byte")
    link(
        ROOT / "_build/dev-dune/default/main_native.exe",
        temporary / "ocamlc.opt",
    )
    link(
        ROOT / "_build/dev-dune/default/boot_ocamlopt.exe",
        temporary / "ocamlopt.opt",
    )
    # ocamltest resolves its "ocamlopt.byte" action to $srcdir/ocamlopt
    # (ocamltest/ocaml_files.ml), so without these the whole flavour fails with
    # "cannot find file .../ocamlopt" rather than running against the dev build.
    link(
        ROOT / "_build/dev-dune/default/boot_ocamlopt.exe",
        temporary / "ocamlopt.byte",
    )
    (temporary / "ocamlopt").symlink_to("ocamlopt.byte")
    for name in ("ocamlrun", "ocamlrund", "ocamlruni"):
        link(
            ROOT / f"_build/runtime_stdlib_install/bin/{name}",
            temporary / name,
        )
    stdlib = temporary / "stdlib"
    stdlib.mkdir()
    for entry in runtime_stdlib.iterdir():
        if entry.name != "stublibs":
            link(entry, stdlib / entry.name)

    stublibs = stdlib / "stublibs"
    stublibs.mkdir()
    dev_stubs = {
        stub.name: stub
        for stub in (ROOT / "_build/dev-dune/default").rglob("dll*.so")
    }
    for stub in (source_root / "stdlib/stublibs").iterdir():
        if stub.name not in dev_stubs:
            link(stub, stublibs / stub.name)
    for name, stub in dev_stubs.items():
        link(stub, stublibs / name)

    runtime = temporary / "runtime"
    runtime.mkdir()
    for entry in (source_root / "runtime").iterdir():
        if entry.name not in {
            "caml", "ocamlrun", "ocamlrund", "ocamlruni", "threads.h"
        }:
            link(entry, runtime / entry.name)
    (runtime / "caml").symlink_to("../stdlib/caml")
    link(ROOT / "runtime/caml/threads.h", runtime / "threads.h")
    for name in ("ocamlrun", "ocamlrund", "ocamlruni"):
        (runtime / name).symlink_to(f"../{name}")

    ocamltest = temporary / "ocamltest"
    ocamltest.mkdir()
    link(
        ROOT / "_build/dev-dune/default/ocamltest/ocamltest.native",
        ocamltest / "ocamltest",
    )

    source_testsuite = source_root / "testsuite"
    testsuite = temporary / "testsuite"
    testsuite.mkdir()
    for entry in source_testsuite.iterdir():
        if entry.name not in {"tests", "tools"}:
            link(entry, testsuite / entry.name)

    source_tools = source_testsuite / "tools"
    tools = testsuite / "tools"
    tools.mkdir()
    for entry in source_tools.iterdir():
        if entry.name not in {"expect", "expectnat"}:
            link(entry, tools / entry.name)
    for name in ("expect", "expectnat"):
        executable = ROOT / f"_build/main/oxcaml/testsuite/tools/{name}.exe"
        if executable.exists():
            link(executable, tools / name)

    tests = testsuite / "tests"
    tests.mkdir()
    replacements = {"asmcomp", "asmgen", "lib-extensions"}
    for entry in (ROOT / "testsuite/tests").iterdir():
        if entry.name not in replacements:
            link(entry, tests / entry.name)
    for name in replacements:
        link(ROOT / "oxcaml/testsuite/tests" / name, tests / name)

    old = STATE / "runtest.old"
    shutil.rmtree(old, ignore_errors=True)
    if destination.exists():
        destination.rename(old)
    temporary.rename(destination)
    shutil.rmtree(old, ignore_errors=True)


def prepare_test_root(_args):
    with locked():
        prepare_test_root_locked()


def parser():
    result = argparse.ArgumentParser()
    commands = result.add_subparsers(dest="action", required=True)

    stop_parser = commands.add_parser("stop")
    stop_parser.set_defaults(function=stop)

    diff_parser = commands.add_parser("diff")
    diff_parser.add_argument("--test", required=True)
    diff_parser.set_defaults(function=diff)

    test_root_parser = commands.add_parser("prepare-test-root")
    test_root_parser.set_defaults(function=prepare_test_root)
    return result


def main():
    args = parser().parse_args()
    result = args.function(args)
    return result if isinstance(result, int) else 0


if __name__ == "__main__":
    sys.exit(main())
