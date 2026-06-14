# array_binary_search_string

- suite: micro
- native: 0.414541s
- llvm: 0.447139s
- ratio llvm/native: 1.0786x

## Layout
- `raw/`: source, input LLVM IR, full native/LLVM asm, raw RS4GC print, full MIR.
- `stages/`: whole-module before/after RS4GC and full post-isel MIR.
- `functions/<symbol>/`: per-function slices.

## Function Counts
| function | before statepoints | after statepoints | after gc-live | after relocates | after exnroot | MIR statepoints | MIR sp refs | LLVM instr/sp | native instr/sp |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `camlArray_binary_search_string__find_4_13_code` | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 107/7 | 43/7 |
| `camlArray_binary_search_string__run_6_15_code` | 0 | 10 | 9 | 11 | 0 | 10 | 1 | 348/63 | 361/75 |
