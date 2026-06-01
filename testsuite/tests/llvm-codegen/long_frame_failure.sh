#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/long_frame_generated.ll"
asm="$build_dir/long_frame_generated.s"

cat > "$src" <<'EOF'
target triple = "arm64-apple-macosx14.0.0"

%ret = type { { i64, i64 }, { i64 } }

declare oxcaml_nofpcc %ret @camlLong_frame__opaque(i64, i64, i64) gc "oxcaml"

define oxcaml_nofpcc %ret @camlLong_frame__entry(i64 %ds, i64 %alloc, i64 %arg) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
entry:
  %big = alloca [40000 x i8], align 16
  call void @llvm.lifetime.start.p0(i64 40000, ptr %big)
  %r = call oxcaml_nofpcc %ret @camlLong_frame__opaque(i64 %ds, i64 %alloc, i64 %arg) "statepoint-id"="0" [ "deopt"() ]
  call void @llvm.lifetime.end.p0(i64 40000, ptr %big)
  ret %ret %r
}

declare void @llvm.lifetime.start.p0(i64 immarg, ptr nocapture)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr nocapture)

!0 = !{ i32 1, !"oxcaml_module", !"Long_frame" }
!llvm.module.flags = !{ !0 }
EOF

"${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" -S -x ir \
  -target arm64-apple-macosx14.0.0 \
  -o "$asm" "$src"

awk '
  /_camlLong_frame__frametable:/ { in_frametable = 1; next }
  in_frametable && /\.short[[:space:]]+32767[[:space:]]*$/ { state = 1; next }
  state == 1 && /\.long[[:space:]]+40032[[:space:]]*$/ { found = 1; exit }
  END { exit found ? 0 : 1 }
' "$asm"
