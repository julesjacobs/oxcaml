#!/usr/bin/env bash
set -euo pipefail

log=${LLVM_WRAPPER_LOG:-}
if [ -n "$log" ]; then
  printf '%q ' "$0" "$@" >> "$log"
  printf '\n' >> "$log"
fi

out=
input=
mode=asm
emit_llvm=0
compile_only=0
llc_opt_level=${LLVM_WRAPPER_LLC_OPT_LEVEL:-3}
llc_args=()
opt_args=()
assembler=${LLVM_WRAPPER_ASSEMBLER:-as}
assembler_triple=${LLVM_WRAPPER_ASSEMBLER_TRIPLE:-x86_64-unknown-linux-gnu}

if [ -n "${LLVM_WRAPPER_PGO_KIND:-}" ]; then
  opt_args+=("-pgo-kind=$LLVM_WRAPPER_PGO_KIND")
fi
if [ -n "${LLVM_WRAPPER_PROFILE_FILE:-}" ]; then
  opt_args+=("-profile-file=$LLVM_WRAPPER_PROFILE_FILE")
fi
if [ -n "${LLVM_WRAPPER_OPT_EXTRA_ARGS:-}" ]; then
  read -r -a extra_opt_args <<< "$LLVM_WRAPPER_OPT_EXTRA_ARGS"
  opt_args+=("${extra_opt_args[@]}")
fi

set_llc_opt_level_from_arg() {
  if [ -n "${LLVM_WRAPPER_LLC_OPT_LEVEL:-}" ]; then
    return
  fi
  case "$1" in
    -O0) llc_opt_level=0 ;;
    -O|-O1) llc_opt_level=1 ;;
    -O2|-Os|-Oz) llc_opt_level=2 ;;
    -O3|-O4|-Ofast) llc_opt_level=3 ;;
  esac
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    -o)
      out=${2:?missing argument for -o}
      shift 2
      ;;
    -x)
      if [ "${2:-}" = "ir" ]; then
        mode=ir
      fi
      shift 2
      ;;
    -emit-llvm)
      emit_llvm=1
      shift
      ;;
    -c)
      compile_only=1
      shift
      ;;
    -O*)
      set_llc_opt_level_from_arg "$1"
      shift
      ;;
    -S|-Wno-override-module|-g|-Wno-trigraphs)
      shift
      ;;
    -mllvm)
      llc_args+=("${2:?missing argument for -mllvm}")
      shift 2
      ;;
    -mavx)
      llc_args+=("-mattr=+avx")
      shift
      ;;
    -mavx2)
      llc_args+=("-mattr=+avx,+avx2")
      shift
      ;;
    -fno-omit-frame-pointer)
      llc_args+=("--frame-pointer=all")
      shift
      ;;
    -fPIC|-fpic)
      llc_args+=("--relocation-model=pic")
      shift
      ;;
    -fomit-frame-pointer|-momit-leaf-frame-pointer)
      shift
      ;;
    -*)
      shift
      ;;
    *)
      input=$1
      shift
      ;;
  esac
done

if [ -z "$out" ] || [ -z "$input" ]; then
  echo "usage: llvm-rs4gc-llc-wrapper.sh -o OUT [-x ir] INPUT" >&2
  exit 2
fi

if [ "$mode" = "ir" ]; then
  pipeline=${LLVM_WRAPPER_OPT_PIPELINE:-'default<O3>,rewrite-statepoints-for-gc,verify'}
  if [ "$emit_llvm" = 1 ]; then
    opt "${opt_args[@]}" -S -passes="$pipeline" "$input" -o "$out"
  else
    tmp=$(mktemp --suffix=.ll)
    cleanup() {
      status=$?
      if [ "$status" -ne 0 ] && [ -n "${LLVM_WRAPPER_KEEP_DIR:-}" ]; then
        mkdir -p "$LLVM_WRAPPER_KEEP_DIR"
        cp "$input" "$LLVM_WRAPPER_KEEP_DIR/input.$$.ll" 2>/dev/null || true
        cp "$tmp" "$LLVM_WRAPPER_KEEP_DIR/optimized.$$.ll" 2>/dev/null || true
      fi
      rm -f "$tmp"
    }
    trap cleanup EXIT
    opt "${opt_args[@]}" -S -passes="$pipeline" "$input" -o "$tmp"
    llc -O"$llc_opt_level" --relocation-model=pic \
      "${llc_args[@]}" "$tmp" -o "$out"
  fi
elif [ "$compile_only" = 1 ]; then
  case "$assembler" in
    as)
      as "$input" -o "$out"
      ;;
    llvm-mc)
      llvm-mc -triple="$assembler_triple" -filetype=obj "$input" -o "$out"
      ;;
    *)
      echo "unsupported assembler: $assembler" >&2
      exit 2
      ;;
  esac
else
  echo "unsupported wrapper invocation" >&2
  exit 2
fi
