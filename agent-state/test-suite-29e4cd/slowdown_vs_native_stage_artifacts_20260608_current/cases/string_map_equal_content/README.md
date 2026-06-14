# string_map_equal_content

- suite: micro
- native: 0.255126s
- llvm: 0.266549s
- ratio llvm/native: 1.0448x

## Layout
- `raw/`: source, input LLVM IR, full native/LLVM asm, raw RS4GC print, full MIR.
- `stages/`: whole-module before/after RS4GC and full post-isel MIR.
- `functions/<symbol>/`: per-function slices.

## Function Counts
| function | before statepoints | after statepoints | after gc-live | after relocates | after exnroot | MIR statepoints | MIR sp refs | LLVM instr/sp | native instr/sp |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `camlString_map_equal_content__find_7_33_code` | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 90/6 | 30/6 |
| `camlString_map_equal_content__find_opt_16_34_code` | 0 | 1 | 1 | 1 | 0 | 1 | 0 | 103/9 | 38/7 |
| `camlString_map_equal_content__run_6_52_code` | 0 | 11 | 10 | 14 | 0 | 11 | 1 | 439/74 | 377/80 |
