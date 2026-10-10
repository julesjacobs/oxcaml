#!/usr/bin/env python3
"""Check the active HM dependency closure with an installed native compiler."""

import argparse
import json
from pathlib import Path
import re
import shutil
import subprocess
import time


def module_name(path):
    return path.stem[0].upper() + path.stem[1:]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("worktree", type=Path)
    parser.add_argument("output", type=Path, help="new output directory")
    parser.add_argument(
        "--runtime-view", action="store_true",
        help="after checking, emit raw Lambda and source-mapped assembly",
    )
    args = parser.parse_args()
    root, output = args.worktree.resolve(), args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    vox = root / "testsuite/tests/vox"
    fixture = (vox / "hm_routed_infer_demo.ml").read_text()
    names = re.search(r'all_modules = "([^"]+)"', fixture).group(1).split()
    paths = [
        vox / name if (vox / name).exists()
        else root / "verification/library" / name
        for name in names
    ]
    modules = {module_name(p): p for p in paths if p.suffix == ".ml"}
    result = subprocess.run(
        [str(root / "_install/bin/ocamldep"), "-modules", *map(str, paths)],
        capture_output=True, text=True, check=True,
    )
    dependencies = {}
    for line in result.stdout.splitlines():
        name, _, imports = line.partition(":")
        dependencies.setdefault(module_name(Path(name)), set()).update(
            imports.split()
        )
    seen = set()

    def visit(module):
        if module in seen or module not in modules:
            return
        seen.add(module)
        for dependency in dependencies.get(module, ()):
            visit(dependency)

    for module in (
        "Verified_hm", "Hm_routed_infer", "Hm_effective_sound", "Hm_effective_complete"
    ):
        visit(module)
    selected = [p for p in paths if module_name(p) in seen]
    for path in selected:
        shutil.copyfile(path, output / path.name)
    compiler = root / "_install/bin/ocamlopt.opt"
    command = [
        str(compiler), "-nostdlib", "-I", str(root / "_install/lib/ocaml"),
        "-extension", "refinement_types", "-w", "-58",
        "-stop-after", "typing", "-c",
    ]
    (output / "reject_control.ml").write_text(
        "let bad : {x : int | false} = let x = 0 in refine_ x\n"
    )
    control = subprocess.run(
        command + ["reject_control.ml"], cwd=output,
        capture_output=True, text=True,
    )
    (output / "reject-control.log").write_text(control.stdout + control.stderr)
    if (control.returncode == 0
            or "Refinement could not be proved" not in control.stderr):
        raise RuntimeError("Verification control failed; see reject-control.log")
    measured_command = command + [
        "-dtimings", "-dtimings-precision", "6", *[p.name for p in selected]
    ]
    start = time.perf_counter()
    with (output / "checking.log").open("w") as log:
        checked = subprocess.run(
            measured_command, cwd=output, stdout=log, stderr=subprocess.STDOUT
        )
    report = {
        "compiler": str(compiler), "exit": checked.returncode,
        "seconds": time.perf_counter() - start,
        "source_files": len(selected),
        "hm_ml_files": sum(p.parent == vox and p.suffix == ".ml" for p in selected),
        "command": measured_command, "control_rejected": True,
    }
    (output / "result.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps({k: v for k, v in report.items() if k != "command"}),
          flush=True)
    if checked.returncode or not args.runtime_view:
        return checked.returncode
    runtime = output / "runtime"
    runtime.mkdir()
    for path in selected:
        shutil.copyfile(path, runtime / path.name)
    runtime_command = [
        str(compiler), "-nostdlib", "-I", str(root / "_install/lib/ocaml"),
        "-extension", "refinement_types", "-w", "-58", "-g", "-S",
        "-drawlambda", "-dump-into-file", "-c", *[p.name for p in selected],
    ]
    with (runtime / "build.log").open("w") as log:
        emitted = subprocess.run(
            runtime_command, cwd=runtime, stdout=log, stderr=subprocess.STDOUT
        )
    (runtime / "index.json").write_text(json.dumps({
        "command": runtime_command,
        "exit": emitted.returncode,
        "scope": "All module declarations and initializers, not a runtime "
                 "reachability or source-LOC count. Assembly .loc directives "
                 "refer to the copied source snapshots. Raw Lambda is before "
                 "optimization; assembly is after optimization.",
        "modules": [
            {"source": str(p), "snapshot": p.name,
             "lambda": p.stem + ".cmx.dump", "assembly": p.stem + ".s"}
            for p in selected if p.suffix == ".ml"
        ],
    }, indent=2) + "\n")
    return emitted.returncode


if __name__ == "__main__":
    raise SystemExit(main())
