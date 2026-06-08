# try_find_miss_rare

- suite: micro
- native: 0.955841s
- llvm: 0.995194s
- ratio llvm/native: 1.0412x

## Layout
- `raw/`: source, input LLVM IR, full native/LLVM asm, raw RS4GC print, full MIR.
- `stages/`: whole-module before/after RS4GC and full post-isel MIR.
- `functions/<symbol>/`: per-function slices.

## Function Counts
| function | before statepoints | after statepoints | after gc-live | after relocates | after exnroot | MIR statepoints | MIR sp refs | LLVM instr/sp | native instr/sp |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `camlTry_find_miss_rare__run_5_12_code` | 0 | 3 | 3 | 1 | 12 | 3 | 1 | 389/79 | 273/71 |
| `camlTry_find_miss_rare__scan_4_11_code` | 0 | 2 | 2 | 4 | 0 | 2 | 0 | 58/11 | 51/11 |
