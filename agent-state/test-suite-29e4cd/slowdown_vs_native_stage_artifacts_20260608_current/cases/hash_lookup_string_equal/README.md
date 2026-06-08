# hash_lookup_string_equal

- suite: micro
- native: 0.727716s
- llvm: 0.813447s
- ratio llvm/native: 1.1178x

## Layout
- `raw/`: source, input LLVM IR, full native/LLVM asm, raw RS4GC print, full MIR.
- `stages/`: whole-module before/after RS4GC and full post-isel MIR.
- `functions/<symbol>/`: per-function slices.

## Function Counts
| function | before statepoints | after statepoints | after gc-live | after relocates | after exnroot | MIR statepoints | MIR sp refs | LLVM instr/sp | native instr/sp |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `camlHash_lookup_string_equal__find_4_13_code` | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 25/7 | 24/7 |
| `camlHash_lookup_string_equal__fn$5bhash_lookup_string_equal.ml$3a27$2c37$2d$2d40$5d_8_8_code` | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 104/17 | 124/23 |
| `camlHash_lookup_string_equal__run_6_15_code` | 0 | 6 | 4 | 6 | 0 | 6 | 1 | 133/37 | 131/37 |
