module State = Hmc_wasm_program_state
module Machine = Hmc_heap_machine
module Guard = Hmc_failed_guard_calls
module Failed = Hmc_failed_guard_model
type result = {failed_guard : Guard.optional @@ ghost;state : State.running; fuel : {n : Wasm_code.count | not (n === Wasm_code.Zero)}; exhausted : Machine.exhaustion option}
