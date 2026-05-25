#!/usr/bin/env bash

set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)

runtime_install=${RUNTIME_INSTALL:-$repo/_llvm_stage5_bootstrap_build/install/runtime_stdlib}
main_install=${MAIN_INSTALL:-$repo/_llvm_stage5_main_build/install/main}
stage_install=${STAGE_INSTALL:-$repo/_llvm_stage5_install}

runtime_lib=${RUNTIME_LIB:-$runtime_install/lib/ocaml_runtime_stdlib}
main_lib=${MAIN_LIB:-$main_install/lib/ocaml}

runtime_install=$(cd "$runtime_install" && pwd)
main_install=$(cd "$main_install" && pwd)
stage_install=$(cd "$(dirname "$stage_install")" && pwd)/$(basename "$stage_install")
runtime_lib=$(cd "$runtime_lib" && pwd)
main_lib=$(cd "$main_lib" && pwd)

require_path () {
  if [ ! -e "$1" ]; then
    echo "missing required path: $1" >&2
    exit 1
  fi
}

copy_tree_contents () {
  local src=$1
  local dst=$2
  mkdir -p "$dst"
  cp -L -R "$src"/* "$dst"/
}

wrap_ocamllib_tool () {
  local tool=$1
  local base
  local run_with_ocamlrun=0
  base=$(basename "$tool")
  [ -L "$tool" ] && return 0
  [ -f "$tool" ] || return 0
  [ -x "$tool" ] || return 0

  mv "$tool" "$tool.real"
  if IFS= read -r first_line < "$tool.real" \
      && [ "$first_line" = "#!/usr/local/bin/ocamlrun" ]; then
    run_with_ocamlrun=1
  fi
  cat > "$tool" <<EOF
#!/usr/bin/env sh
case "\$0" in
  */*) tool="\$0" ;;
  *) tool=\$(command -v "\$0") ;;
esac
bindir=\$(CDPATH= cd "\$(dirname "\$tool")" && pwd -P) || exit 127
if [ "\${OCAMLLIB+x}" != x ]; then
  OCAMLLIB=\$(CDPATH= cd "\$bindir/../lib/ocaml" && pwd -P) || exit 127
  export OCAMLLIB
fi
if [ $run_with_ocamlrun = 1 ]; then
  exec "\$bindir/ocamlrun" "\$bindir/$base.real" "\$@"
fi
exec "\$bindir/$base.real" "\$@"
EOF
  chmod +x "$tool"
}

require_path "$runtime_install/bin/ocamlrun"
require_path "$runtime_lib/stdlib.cmxa"
require_path "$main_install/bin/ocamlopt.opt"
require_path "$main_lib/str/str.cmxs"

mkdir -p "$runtime_lib/dynlink"
touch "$runtime_lib/dynlink.cmxa" "$runtime_lib/dynlink/dynlink.cmxa"

rm -rf "$stage_install"
mkdir -p "$stage_install/bin" "$stage_install/lib/ocaml"

copy_tree_contents "$runtime_install/bin" "$stage_install/bin"
copy_tree_contents "$main_install/bin" "$stage_install/bin"

(
  cd "$stage_install/bin"
  for file in *.opt; do
    [ -e "$file" ] || continue
    link=${file%.opt}
    rm -f "$link"
    ln -s "$file" "$link"
  done
)

copy_tree_contents "$runtime_lib" "$stage_install/lib/ocaml"
rm -f \
  "$stage_install/lib/ocaml/META" \
  "$stage_install/lib/ocaml/dune-package" \
  "$stage_install/lib/ocaml/Makefile.config" \
  "$stage_install/lib/ocaml/dynlink.cmxa" \
  "$stage_install/lib/ocaml/dynlink/dynlink.cmxa"

copy_tree_contents "$main_lib" "$stage_install/lib/ocaml"
rm -f "$stage_install/lib/ocaml/META" "$stage_install/lib/ocaml/dune-package"

mkdir -p "$stage_install/lib/stublibs"
if [ -d "$stage_install/lib/ocaml/stublibs" ]; then
  cp -L "$stage_install"/lib/ocaml/stublibs/* "$stage_install/lib/stublibs"/ \
    2>/dev/null || true
fi
rm -f "$stage_install/lib/ocaml/ld.conf"
cat > "$stage_install/lib/ocaml/ld.conf" <<EOF
$stage_install/lib/ocaml/stublibs
$stage_install/lib/stublibs
$stage_install/lib/ocaml
EOF

for tool in "$stage_install/bin"/*; do
  case "$(basename "$tool")" in
    *.real|ocamlrun|ocamlrund|ocamlruni) ;;
    *) wrap_ocamllib_tool "$tool" ;;
  esac
done

"$stage_install/bin/ocamlopt.opt" -config >/tmp/oxcaml-stage-install-config
grep -q "^standard_library: $stage_install/lib/ocaml$" \
  /tmp/oxcaml-stage-install-config
grep -q "^native_dynlink: true$" /tmp/oxcaml-stage-install-config

require_path "$stage_install/lib/ocaml/stdlib.cmxa"
require_path "$stage_install/lib/ocaml/str/str.cmxs"
require_path "$stage_install/lib/ocaml/unix/unix.cmxs"

cat <<EOF
Stage LLVM install ready:
  $stage_install

Compiler:
  $stage_install/bin/ocamlopt.opt

Stdlib:
  $stage_install/lib/ocaml
EOF
