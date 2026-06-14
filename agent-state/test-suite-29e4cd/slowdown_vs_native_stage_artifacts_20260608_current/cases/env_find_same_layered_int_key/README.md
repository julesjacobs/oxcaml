# env_find_same_layered_int_key

- suite: micro
- native: 0.377016s
- llvm: 0.408431s
- ratio llvm/native: 1.0833x

## Layout
- `raw/`: source, input LLVM IR, full native/LLVM asm, raw RS4GC print, full MIR.
- `stages/`: whole-module before/after RS4GC and full post-isel MIR.
- `functions/<symbol>/`: per-function slices.

## Function Counts
| function | before statepoints | after statepoints | after gc-live | after relocates | after exnroot | MIR statepoints | MIR sp refs | LLVM instr/sp | native instr/sp |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `camlEnv_find_same_layered_int_key__find_same_without_locks_5_13_code` | 0 | 2 | 2 | 2 | 20 | 2 | 1 | 51/21 | 47/18 |
| `camlEnv_find_same_layered_int_key__open_layers_6_14_code` | 0 | 6 | 4 | 4 | 0 | 6 | 1 | 56/5 | 58/9 |
| `camlEnv_find_same_layered_int_key__run_7_15_code` | 0 | 3 | 1 | 1 | 0 | 3 | 1 | 179/42 | 201/51 |
