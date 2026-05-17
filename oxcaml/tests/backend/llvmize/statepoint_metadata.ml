external opaque : 'a -> 'a = "%opaque"

external too_many :
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int = "" "too_many"

(* This file checks the current metadata ABI between [Llvmize] and the custom
   LLVM frametable printer.

   The integer in ["statepoint-id"] currently carries more than one fact:
   - allocation size in the high 16 bits;
   - stack offset in the low 16 bits;
   - the low bit marks a [caml_call_gc] frame for either an allocation or a
     poll.

   These tests intentionally check the current encoding. If the encoding
   changes, this test should change with the frametable printer. *)

let[@inline never] [@local never] allocate_pair x y =
  opaque (opaque x, opaque y)

let[@inline never] [@local never] allocate_ref x = opaque (ref (opaque x))

let[@inline never] [@local never] call_with_stack_args () =
  too_many 1 2 3 4 5 6 7 8 9 10 11 12

let[@inline never] [@local never] call_with_stack_args_inside_trap () =
  try too_many 1 2 3 4 5 6 7 8 9 10 11 12 with _ -> 0
