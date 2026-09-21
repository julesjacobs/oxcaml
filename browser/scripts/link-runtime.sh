#!/bin/sh
set -eu
cd "$(dirname "$0")/.."
: "${EMCC:=emcc}"
"$EMCC" -O2 -sMEMORY64=2 -sALLOW_MEMORY_GROWTH=1 -sINITIAL_MEMORY=67108864 -sMAXIMUM_MEMORY=2147483648 -sSTACK_SIZE=8388608 -sMODULARIZE=1 -sEXPORT_ES6=1 -sENVIRONMENT=web,worker,node -sFORCE_FILESYSTEM=1 -sEXIT_RUNTIME=1 '-sEXPORTED_RUNTIME_METHODS=["FS","callMain","ENV"]' -I .runtime-source/runtime .build/prims.c compiler/bridge.c .runtime-source/runtime/libcamlrun.a -lm -o public/assets/vox-runtime.js
