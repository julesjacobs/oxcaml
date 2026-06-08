# string_tree_first_byte_diff

- suite: micro
- native: 0.406945s
- llvm: 0.427476s
- ratio llvm/native: 1.0505x

## Layout
- `raw/`: source, input LLVM IR, full native/LLVM asm, raw RS4GC print, full MIR.
- `stages/`: whole-module before/after RS4GC and full post-isel MIR.
- `functions/<symbol>/`: per-function slices.

## Function Counts
| function | before statepoints | after statepoints | after gc-live | after relocates | after exnroot | MIR statepoints | MIR sp refs | LLVM instr/sp | native instr/sp |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `camlString_tree_first_byte_diff__find_6_14_code` | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 87/7 | 28/7 |
| `camlString_tree_first_byte_diff__key_4_12_code` | 0 | 4 | 4 | 4 | 0 | 4 | 0 | 115/20 | 135/28 |
| `camlString_tree_first_byte_diff__run_7_15_code` | 0 | 9 | 7 | 13 | 0 | 9 | 1 | 318/70 | 334/79 |
