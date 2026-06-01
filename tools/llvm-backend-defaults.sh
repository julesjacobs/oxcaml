#!/usr/bin/env bash

# Shared defaults for local LLVM-backend validation. Set LLVM_BACKEND_FLAGS= to
# intentionally test without the extra LLVM codegen flags.
default_llvm_backend_flags="-mllvm -oxcaml-regalloc-call-split-remainders"

llvm_backend_flags_value () {
  printf '%s\n' "${LLVM_BACKEND_FLAGS-$default_llvm_backend_flags}"
}

llvm_backend_ocamlparam () {
  local wrapper=$1
  local flags
  flags=$(llvm_backend_flags_value)
  if [ -n "$flags" ]; then
    printf '_,llvm-backend=1,llvm-path=%s,llvm-flags=%s\n' "$wrapper" "$flags"
  else
    printf '_,llvm-backend=1,llvm-path=%s\n' "$wrapper"
  fi
}
