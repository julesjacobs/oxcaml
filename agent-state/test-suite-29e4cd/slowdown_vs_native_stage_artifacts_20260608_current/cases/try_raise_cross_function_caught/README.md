# try_raise_cross_function_caught

- suite: micro
- native: 0.486454s
- llvm: 0.539629s
- ratio llvm/native: 1.1093x

## Layout
- `raw/`: source, input LLVM IR, full native/LLVM asm, raw RS4GC print, full MIR.
- `stages/`: whole-module before/after RS4GC and full post-isel MIR.
- `functions/<symbol>/`: per-function slices.

## Function Counts
| function | before statepoints | after statepoints | after gc-live | after relocates | after exnroot | MIR statepoints | MIR sp refs | LLVM instr/sp | native instr/sp |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `camlTry_raise_cross_function_caught__find_5_12_code` | 0 | 2 | 0 | 0 | 0 | 2 | 1 | 45/21 | 32/15 |
| `camlTry_raise_cross_function_caught__probe_4_11_code` | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 8/1 | 7/1 |
| `camlTry_raise_cross_function_caught__run_6_13_code` | 0 | 2 | 0 | 0 | 0 | 2 | 1 | 169/37 | 190/46 |
