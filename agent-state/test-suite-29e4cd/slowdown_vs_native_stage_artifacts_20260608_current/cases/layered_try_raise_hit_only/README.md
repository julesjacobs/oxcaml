# layered_try_raise_hit_only

- suite: micro
- native: 0.624385s
- llvm: 0.679841s
- ratio llvm/native: 1.0888x

## Layout
- `raw/`: source, input LLVM IR, full native/LLVM asm, raw RS4GC print, full MIR.
- `stages/`: whole-module before/after RS4GC and full post-isel MIR.
- `functions/<symbol>/`: per-function slices.

## Function Counts
| function | before statepoints | after statepoints | after gc-live | after relocates | after exnroot | MIR statepoints | MIR sp refs | LLVM instr/sp | native instr/sp |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `camlLayered_try_raise_hit_only__find_5_13_code` | 0 | 2 | 2 | 1 | 9 | 2 | 1 | 48/18 | 40/16 |
| `camlLayered_try_raise_hit_only__open_layers_6_14_code` | 0 | 6 | 4 | 4 | 0 | 6 | 1 | 56/5 | 58/9 |
| `camlLayered_try_raise_hit_only__run_7_15_code` | 0 | 3 | 1 | 1 | 0 | 3 | 1 | 178/42 | 200/51 |
