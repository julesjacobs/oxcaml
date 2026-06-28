#!/bin/sh

set -eu

build_dir=$(pwd)
host_arch=$(uname -m)
host_system=$(uname -s)
src="$build_dir/amd64_tail_call_generated.ml"
exe="$build_dir/amd64_tail_call_generated.exe"
ir="$build_dir/amd64_tail_call_generated.ll"
asm="$build_dir/amd64_tail_call_generated.s"
stdout_file="$build_dir/amd64_tail_call.out"
extra_link_flags=""

case "$host_system:$host_arch" in
  Linux:x86_64 | Linux:amd64) extra_link_flags="-ccopt -no-pie" ;;
esac

search_dir=$build_dir
ocamlopt=""
while [ "$search_dir" != "/" ]; do
  if [ -f "$search_dir/ocamlopt.opt" ] && [ -x "$search_dir/ocamlopt.opt" ]; then
    ocamlopt="$search_dir/ocamlopt.opt"
    break
  fi
  search_dir=$(dirname "$search_dir")
done

if [ -z "$ocamlopt" ]; then
  if [ -n "${OCAMLSRCDIR:-}" ] && [ -x "$OCAMLSRCDIR/ocamlopt.opt" ]; then
    ocamlopt="$OCAMLSRCDIR/ocamlopt.opt"
  else
    ocamlopt="_build/install/main/bin/ocamlopt.opt"
  fi
fi

stdlib_flags=""
ocamlopt_dir=$(dirname "$ocamlopt")
for stdlib_dir in \
  "$ocamlopt_dir/lib/ocaml" \
  "$ocamlopt_dir/_install/lib/ocaml" \
  "$ocamlopt_dir/../lib/ocaml" \
  "$ocamlopt_dir/../../runtime_stdlib_install/lib/ocaml_runtime_stdlib"
do
  if [ -f "$stdlib_dir/stdlib.cmi" ]; then
    stdlib_flags="-I $stdlib_dir"
    break
  fi
done

cat > "$src" <<'EOF'
let[@inline never] apply f x = f x

let[@inline never] inc x = x + 1

let () =
  let result = apply inc (Sys.opaque_identity 41) in
  if result <> 42 then Printf.ksprintf failwith "expected 42, got %d" result;
  print_endline "ok"
EOF

"$ocamlopt" $stdlib_flags -O3 -S -keep-llvmir -llvm-backend \
  $extra_link_flags \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" -o "$exe" "$src"

"$exe" > "$stdout_file"
grep -q "^ok$" "$stdout_file"

awk '
  /define .*__apply_.*_code/ { in_apply = 1 }
  # Accept fpcc and nofpcc; this test cares about tail-call lowering, not the
  # frame-pointer configuration used by the compiler build.
  in_apply && /musttail call oxcaml_.*fpcc .*"statepoint-id"="0"/ {
    found = 1
  }
  in_apply && /^}/ { exit found ? 0 : 1 }
  END { exit found ? 0 : 1 }
' "$ir"

awk '
  /^_?caml.*__apply_.*_code:/ {
    in_apply = 1
    found = 1
    saw_tail_jump = 0
    saw_call = 0
    next
  }
  in_apply && /^[[:space:]]*callq?[[:space:]]/ { saw_call = 1 }
  in_apply && /^[[:space:]]*jmpq?[[:space:]]/ && /TAILCALL/ {
    saw_tail_jump = 1
  }
  in_apply && /^[.]Lfunc_end/ {
    exit found && saw_tail_jump && !saw_call ? 0 : 1
  }
  END { exit found && saw_tail_jump && !saw_call ? 0 : 1 }
' "$asm"
