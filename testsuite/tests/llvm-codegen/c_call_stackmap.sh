#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/c_call_stackmap_generated.ml"
out="$build_dir/c_call_stackmap_generated.exe"
asm="$build_dir/c_call_stackmap_generated.s"
stdout_file="$build_dir/c_call_stackmap_stdout.txt"

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

cat > "$src" <<'EOF'
let marshal oc base x =
  let pos = Out_channel.pos oc in
  Marshal.to_channel oc x [];
  Int64.to_int (Int64.sub pos base)

let () =
  let path = Filename.temp_file "llvm-c-call" ".bin" in
  let oc = open_out_bin path in
  let n = marshal oc 0L [1; 2; 3; 4] in
  close_out oc;
  Sys.remove path;
  Printf.printf "%d\n" n
EOF

"$ocamlopt" -O3 -g -S -llvm-backend \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$out" "$src"

"$out" > "$stdout_file"
grep -q "^0$" "$stdout_file"

first_c_call_return_label=$(
  awk '
    /callq[[:space:]]+caml_c_call/ { after_first_c_call = 1; next }
    after_first_c_call && /^[.]Ltmp[0-9]+:/ {
      sub(":", "", $1);
      print $1;
      exit
    }
  ' "$asm"
)

if [ -z "$first_c_call_return_label" ]; then
  echo "could not find return label after first caml_c_call" >&2
  exit 1
fi

grep -q "[.]long[[:space:]]*$first_c_call_return_label-" "$asm"
