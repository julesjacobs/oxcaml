"""Compare what the compiled program computes with what the backend proves.

Stage one runs every case in a compiled program, under bytecode and under
native code, folded and behind [Sys.opaque_identity], and records what came
out.  Stage two asks the verifier, for each case, to prove that the same
operation over the same literal operands yields that same recorded answer,
and to refuse answers the machine did not produce.  A disagreement between
the two stages is the failure this gate exists to report.

Each obligation is compiled on its own, because the compiler stops at the
first one it cannot discharge and a single disagreement would otherwise hide
every later case.
"""

import argparse
import concurrent.futures
import os
import shutil
import subprocess
import sys
import tempfile
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import differential_cases as model  # noqa: E402  # pyright: ignore[reportImplicitRelativeImport]

SKIP = 125


class GateFailure(Exception):
    """The gate could not be run, as distinct from the gate finding a defect."""


# The verifier's own report when it will not discharge an obligation.  Any
# other compiler failure is a fault in the gate, not a verdict, and is
# reported as such rather than counted as a refusal.
REFUSAL = "Refinement verification failed"


class Observation:
    """What the machine produced for one case."""

    def __init__(self, key):
        self.key = key
        self.answers = {}

    def record(self, engine, folded, opaque):
        self.answers[(engine, "folded")] = folded
        self.answers[(engine, "opaque")] = opaque

    @property
    def agreed(self):
        return len(set(self.answers.values())) == 1

    @property
    def answer(self):
        """The machine's single answer, or None if it did not give one."""
        if not self.agreed:
            return None
        text = next(iter(self.answers.values()))
        return None if text.startswith("!") else text

    def describe(self):
        return " ".join(
            "%s/%s=%s" % (engine, form, text)
            for (engine, form), text in sorted(self.answers.items())
        )


def parse_value(sort, text):
    return text == "true" if sort == "bool" else int(text)


def run(command, cwd=None, env=None):
    return subprocess.run(
        command,
        cwd=cwd,
        env=env,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        universal_newlines=True,
    )


def observe(cases, scratch, compilers, environment):
    """Run every case in a compiled program, once per available engine."""
    source = os.path.join(scratch, "differential_observe.ml")
    with open(source, "w") as handle:
        handle.write(model.observation_program(cases))

    observations = {case.key: Observation(case.key) for case in cases}
    sizes = []
    engines = []
    for engine, compiler, extension in compilers:
        program = os.path.join(scratch, "observe." + extension)
        built = run(
            compiler + ["-w", "-a", "-o", program, source], cwd=scratch, env=environment
        )
        if built.returncode != 0:
            raise GateFailure(
                "%s observation program did not build:\n%s"
                % (engine, built.stdout[-2000:])
            )
        ran = run([program], cwd=scratch, env=environment)
        if ran.returncode != 0:
            raise GateFailure(
                "%s observation program did not run:\n%s" % (engine, ran.stdout[-2000:])
            )
        for line in ran.stdout.splitlines():
            fields = line.split("\t")
            if len(fields) != 3:
                raise GateFailure("unreadable observation line: %r" % line)
            key, folded, opaque = fields
            if key == "#int_size":
                sizes.append(int(folded))
                continue
            if key not in observations:
                raise GateFailure("unexpected observation key: %r" % key)
            observations[key].record(engine, folded, opaque)
        engines.append(engine)

    for size in sizes:
        if size != model.WIDTH:
            raise GateFailure(
                "this gate's boundary values assume a %d-bit int; the "
                "compiler reports %d" % (model.WIDTH, size)
            )
    missing = [key for key, seen in observations.items() if not seen.answers]
    if missing:
        raise GateFailure(
            "no observation for %d cases, first %s" % (len(missing), missing[0])
        )
    return observations, engines


class Probe:
    """One obligation: a case, an answer, and whether it should be proved."""

    def __init__(self, case, witness, expect_proved, reason):
        self.case = case
        self.witness = witness
        self.expect_proved = expect_proved
        self.reason = reason
        self.proved = None
        self.detail = None

    def describe(self):
        return "%s : %s{ _ = %s }" % (
            self.case.render(),
            self.case.sort,
            model.literal(self.witness),
        )


def build_probes(cases, observations, sentinels_everywhere):
    """Turn observations into obligations, with the answer expected of each.

    The machine's own answer is expected to be proved wherever the operation
    is modelled and the machine gave one answer; everywhere else it must be
    refused.  Answers the machine did not produce must always be refused: a
    backend that proves one disagrees with the machine, and a backend that
    proves both that answer and the machine's is not saying anything at all.
    """
    probes = []
    sampled = set()
    for case in cases:
        observed = observations[case.key]
        answer = observed.answer
        value = None if answer is None else parse_value(case.sort, answer)
        specified = observed.agreed and answer is not None
        if answer is not None:
            expect = case.modelled and specified
            probes.append(
                Probe(
                    case,
                    value,
                    expect,
                    "machine answer" if expect else "no committed meaning",
                )
            )
        group = (case.family, case.operator)
        wanted = (
            sentinels_everywhere
            or case.family == "divmod"
            or not case.modelled
            or not specified
            or group not in sampled
        )
        sampled.add(group)
        if wanted:
            for witness in model.wrong_witnesses(case, value):
                probes.append(Probe(case, witness, False, "not the machine's"))
    return probes


def discharge(probe, index, scratch, compiler, environment):
    """Compile one obligation on its own and report whether it was proved."""
    directory = os.path.join(scratch, "probe%05d" % index)
    os.mkdir(directory)
    try:
        source = os.path.join(directory, "probe.ml")
        with open(source, "w") as handle:
            handle.write(model.obligation_program(probe.case, probe.witness))
        result = run(
            compiler + ["-w", "-a", "-c", "probe.ml"],
            cwd=directory,
            env=environment,
        )
        if result.returncode == 0:
            probe.proved = True
        elif REFUSAL in result.stdout:
            probe.proved = False
            probe.detail = result.stdout
        else:
            probe.proved = None
            probe.detail = result.stdout
        return probe
    finally:
        shutil.rmtree(directory, ignore_errors=True)


def unavailable(probes):
    """Obligations the backend could not answer at all, solver included."""
    return [
        probe
        for probe in probes
        if probe.proved is False and "unavailable" in (probe.detail or "")
    ]


def report(probes, cases, engines, observations, elapsed, arguments):
    lines = [
        "differential gate: backend=%s profile=%s jobs=%d"
        % (arguments.backend, arguments.profile, arguments.jobs),
        "observation: %d-bit int, engines %s" % (model.WIDTH, ", ".join(engines)),
    ]

    diverged = [key for key, seen in observations.items() if not seen.agreed]
    if diverged:
        lines.append(
            "machine gave no single answer for %d cases (refusal required): %s"
            % (len(diverged), ", ".join(sorted(diverged)[:4]))
        )

    families = {}
    for probe in probes:
        entry = families.setdefault(probe.case.family, [0, 0, 0])
        entry[0] += 1
        if probe.proved is not probe.expect_proved:
            entry[2] += 1
        elif probe.expect_proved:
            entry[1] += 1

    broken = [probe for probe in probes if probe.proved is None]
    disagreements = [
        probe
        for probe in probes
        if probe.proved is not None and probe.proved != probe.expect_proved
    ]

    lines.append(
        "cases=%d obligations=%d proved=%d refused=%d disagreements=%d "
        "errors=%d"
        % (
            len(cases),
            len(probes),
            sum(1 for probe in probes if probe.proved is True),
            sum(1 for probe in probes if probe.proved is False),
            len(disagreements),
            len(broken),
        )
    )
    for family in sorted(families):
        total, proved, wrong = families[family]
        lines.append(
            "  %-8s obligations=%-4d agreed=%-4d proved=%-4d disagreements=%d"
            % (family, total, total - wrong, proved, wrong)
        )
    lines.append(
        "elapsed %.1fs (%.3fs per obligation)"
        % (elapsed, elapsed / max(len(probes), 1))
    )

    for probe in disagreements[:8]:
        lines.append(
            "  DISAGREEMENT %s expected %s, backend %s [%s]"
            % (
                probe.describe(),
                "proved" if probe.expect_proved else "refused",
                "proved" if probe.proved else "refused",
                probe.reason,
            )
        )
        lines.append("    machine: %s" % observations[probe.case.key].describe())
    if len(disagreements) > 8:
        lines.append("  ... and %d more" % (len(disagreements) - 8))
    for probe in broken[:4]:
        lines.append("  ERROR %s" % probe.describe())
        lines.append("    %s" % (probe.detail or "").strip()[:400])
    return lines, disagreements, broken


def finish(lines, status):
    text = "\n".join(lines)
    print(text)
    response = os.environ.get("ocamltest_response")
    if response and status != 0:
        with open(response, "w") as handle:
            handle.write(text + "\n")
    return status


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--ocamlrun", required=True)
    parser.add_argument("--ocamlc", required=True)
    parser.add_argument("--ocamlc-opt", default="")
    parser.add_argument("--ocamlopt-opt", default="")
    parser.add_argument("--backend", required=True)
    parser.add_argument("--profile", required=True, choices=sorted(model.PROFILES))
    parser.add_argument("--jobs", type=int, default=2)
    parser.add_argument(
        "--sentinels-everywhere",
        action="store_true",
        help="probe an answer the machine did not give for every case, not "
        "only for the sampled ones",
    )
    arguments = parser.parse_args()

    scratch_root = os.environ.get("TMPDIR")
    if not scratch_root or not os.path.isdir(scratch_root):
        return finish(["TMPDIR must name a private scratch directory"], 1)

    environment = dict(os.environ)
    # Every obligation must reach the solver: a cached verdict would say what
    # some earlier compiler decided, not what this one does.
    environment["VOX_SOLVER_CACHE"] = "0"

    byte = [arguments.ocamlrun, arguments.ocamlc]
    native = arguments.ocamlopt_opt
    fast = arguments.ocamlc_opt
    prover = [fast] if fast and os.access(fast, os.X_OK) else byte
    prover = prover + ["-vox-backend", arguments.backend]

    compilers = [("bytecode", byte, "byte")]
    if native and os.access(native, os.X_OK):
        compilers.append(("native", [native], "exe"))

    cases = model.cases(arguments.profile)
    scratch = tempfile.mkdtemp(prefix="differential-", dir=scratch_root)
    try:
        observations, engines = observe(cases, scratch, compilers, environment)
        probes = build_probes(cases, observations, arguments.sentinels_everywhere)

        started = time.time()
        jobs = 1 if arguments.backend == "lean" else max(arguments.jobs, 1)
        with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as pool:
            list(
                pool.map(
                    lambda pair: discharge(
                        pair[1], pair[0], scratch, prover, environment
                    ),
                    enumerate(probes),
                )
            )
        elapsed = time.time() - started
    finally:
        shutil.rmtree(scratch, ignore_errors=True)

    absent = unavailable(probes)
    if len(absent) == len(probes) and probes:
        return finish(
            [
                "the %s backend answered no obligation; it is not available "
                "here, so this gate cannot say whether the two meanings agree"
                % arguments.backend
            ],
            SKIP,
        )

    lines, disagreements, broken = report(
        probes, cases, engines, observations, elapsed, arguments
    )
    if absent:
        lines.append(
            "  %d obligations came back unavailable rather than answered" % len(absent)
        )
    status = 1 if disagreements or broken or absent else 0
    return finish(lines, status)


if __name__ == "__main__":
    try:
        sys.exit(main())
    except GateFailure as failure:
        sys.exit(finish(["the gate could not be run", str(failure)], 1))
