# boyer

- suite: mini
- native: 0.087929s
- llvm: 0.091329s
- ratio llvm/native: 1.0387x

## Layout
- `raw/`: source, input LLVM IR, full native/LLVM asm, raw RS4GC print, full MIR.
- `stages/`: whole-module before/after RS4GC and full post-isel MIR.
- `functions/<symbol>/`: per-function slices.

## Function Counts
| function | before statepoints | after statepoints | after gc-live | after relocates | after exnroot | MIR statepoints | MIR sp refs | LLVM instr/sp | native instr/sp |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `camlBoyer__rewrite_12_32_code` | 0 | 3 | 3 | 6 | 0 | 3 | 1 | 41/12 | 40/13 |
| `camlBoyer__rewrite_with_lemmas_13_33_code` | 0 | 5 | 5 | 10 | 89 | 5 | 1 | 95/45 | 64/21 |
| `camlBoyer__unify1_10_30_code` | 0 | 2 | 2 | 3 | 55 | 2 | 0 | 110/40 | 103/29 |
