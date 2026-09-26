#!/usr/bin/env python3
"""Compile and link the RSA client with only public interfaces available."""

from pathlib import Path
import shutil
import subprocess
import tempfile


ROOT = Path(__file__).resolve().parents[3]
LIBRARY = ROOT / "verification/library"
CLIENT = Path(__file__).with_name("rsa_public_client.ml")
UNITS = (
    "vox_rsa_spec",
    "vox_rsa_arithmetic",
    "vox_rsa_number_theory",
    "vox_rsa_fermat",
    "vox_rsa",
)


def check(compiler, extension):
    compiler = ROOT / "_install/bin" / compiler
    with tempfile.TemporaryDirectory(prefix="vox-rsa-boundary-") as temporary:
        build = Path(temporary)
        implementation = build / "implementation"
        public = build / "public"
        implementation.mkdir()
        public.mkdir()
        flags = ["-extension", "refinement_types", "-smt-timeout", "10000"]

        def run(arguments, directory):
            subprocess.run([str(compiler), *arguments], cwd=directory, check=True)

        for unit in UNITS:
            interface = LIBRARY / (unit + ".mli")
            if interface.exists():
                run([*flags, "-c", str(interface), "-o", unit + ".cmi"],
                    implementation)
            run([*flags, "-c", str(LIBRARY / (unit + ".ml")),
                 "-o", unit + "." + extension], implementation)

        for unit in ("vox_rsa_spec", "vox_rsa"):
            shutil.copyfile(implementation / (unit + ".cmi"),
                            public / (unit + ".cmi"))
        shutil.copyfile(CLIENT, public / CLIENT.name)
        run([*flags, "-c", CLIENT.name, "-o",
             "rsa_public_client." + extension], public)
        executable = build / "client"
        objects = [str(implementation / (u + "." + extension)) for u in UNITS]
        run([*objects, str(public / ("rsa_public_client." + extension)),
             "-o", str(executable)], implementation)
        subprocess.run([str(executable)], cwd=public, check=True)
        print(f"{compiler.name}: public-only RSA client passed", flush=True)


if __name__ == "__main__":
    check("ocamlc", "cmo")
    check("ocamlopt", "cmx")
