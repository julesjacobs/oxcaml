#!/usr/bin/env python3
"""Cross-check raw blocks in both directions against liblz4 1.x.

Usage: python3 verification/benchmarks/lz4_interop.py PATH_TO_OCAMLC_OR_OCAMLOPT
"""

import ctypes
import ctypes.util
import pathlib
import random
import subprocess
import sys
import tempfile

ROOT = pathlib.Path(__file__).resolve().parents[2]
MAX_BLOCK = 4 * 1024 * 1024


def liblz4():
    path = ctypes.util.find_library("lz4")
    if path is None:
        raise RuntimeError("liblz4 is required for this interoperability check")
    lib = ctypes.CDLL(path)
    lib.LZ4_compressBound.argtypes = [ctypes.c_int]
    lib.LZ4_compressBound.restype = ctypes.c_int
    lib.LZ4_compress_default.argtypes = [
        ctypes.c_void_p, ctypes.c_void_p, ctypes.c_int, ctypes.c_int
    ]
    lib.LZ4_compress_default.restype = ctypes.c_int
    lib.LZ4_decompress_safe.argtypes = [
        ctypes.c_void_p, ctypes.c_void_p, ctypes.c_int, ctypes.c_int
    ]
    lib.LZ4_decompress_safe.restype = ctypes.c_int
    return lib


def compress(lib, source):
    bound = lib.LZ4_compressBound(len(source))
    output = ctypes.create_string_buffer(bound)
    size = lib.LZ4_compress_default(source, output, len(source), bound)
    assert size > 0
    return output.raw[:size]


def decompress(lib, block, capacity):
    output = ctypes.create_string_buffer(max(1, capacity))
    size = lib.LZ4_decompress_safe(block, output, len(block), capacity)
    assert size >= 0, (len(block), capacity)
    return output.raw[:size]


def run(codec, mode, data):
    process = subprocess.run(
        [str(codec), mode], input=data, stdout=subprocess.PIPE,
        stderr=subprocess.PIPE, check=True
    )
    return process.stdout


def corpus():
    rng = random.Random(20260924)
    yield b""
    for length in list(range(1, 40)) + [64, 255, 256, 511, 4096, 65535, 65536]:
        yield bytes(rng.randrange(256) for _ in range(length))
        for period in [1, 2, 3, 4, 7, 16, 32]:
            pattern = bytes(rng.randrange(256) for _ in range(period))
            yield (pattern * (length // period + 1))[:length]
    for _ in range(1000):
        length = rng.randrange(1, 2048)
        period = rng.randrange(1, min(length, 64) + 1)
        pattern = bytes(rng.randrange(256) for _ in range(period))
        yield (pattern * (length // period + 1))[:length]


def main():
    compiler = pathlib.Path(sys.argv[1]).resolve()
    library_dir = compiler.parent.parent / "lib/ocaml/vox"
    archive = "vox_borrow.cmxa" if compiler.name.startswith("ocamlopt") else "vox_borrow.cma"
    lib = liblz4()
    with tempfile.TemporaryDirectory() as directory:
        temporary = pathlib.Path(directory)
        codec = temporary / "lz4_interop"
        subprocess.run([
            str(compiler), "-I", str(library_dir),
            "-extension", "refinement_types", "-principal",
            str(library_dir / archive),
            str(ROOT / "verification/benchmarks/lz4_interop_cli.ml"),
            "-o", str(codec),
        ], check=True)
        count = 0
        for source in corpus():
            assert len(source) <= MAX_BLOCK
            from_lib = compress(lib, source)
            from_vox = run(codec, "encode", source)
            assert run(codec, "decode", from_lib) == source
            assert decompress(lib, from_vox, len(source)) == source
            count += 1
    print(f"{count} raw LZ4 blocks passed in both directions")


if __name__ == "__main__":
    main()
