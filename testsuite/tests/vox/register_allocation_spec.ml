type operand = Reg of int | Imm of int
type operation = Add | Subtract | Equal | Less_than
type instruction =
  | Move of int * operand * int
  | Binary of int * operation * operand * operand * int
  | Jump of int
  | Branch of operand * int * int
  | Return of operand

type program = { code : instruction list; registers : int; inputs : int list }
type state = Running of int * int list | Done of int | Stuck

let[@def] rec (nth @ total) xs n =
  match xs with
  | [] -> None
  | x :: rest -> if n = 0 then Some x else nth rest (n - 1)

let[@def] rec (write @ total) xs n value =
  match xs with
  | [] -> None
  | x :: rest ->
    if n = 0 then Some (value :: rest)
    else match write rest (n - 1) value with
      | None -> None
      | Some rest -> Some (x :: rest)

let[@def] rec (zeros @ total) n = if n <= 0 then [] else 0 :: zeros (n - 1)
[@@decreases n]

let[@def] (value @ total) file operand =
  match operand with Reg r -> nth file r | Imm n -> Some n

let[@def] (apply @ total) operation left right = match operation with
  | Add -> left + right
  | Subtract -> left - right
  | Equal -> if left = right then 1 else 0
  | Less_than -> if left < right then 1 else 0

let[@def] (execute @ total) instruction file =
  match instruction with
    | Move (dst, operand, next) ->
      (match value file operand with
       | None -> Stuck
       | Some word ->
         match write file dst word with
         | None -> Stuck
         | Some file -> Running (next, file))
    | Binary (dst, operation, left, right, next) ->
      (match value file left, value file right with
       | Some left, Some right ->
         (match write file dst (apply operation left right) with
          | None -> Stuck
          | Some file -> Running (next, file))
       | _ -> Stuck)
    | Jump next -> Running (next, file)
    | Branch (condition, yes, no) ->
      (match value file condition with
       | None -> Stuck
       | Some word -> Running ((if word = 0 then no else yes), file))
    | Return operand ->
      (match value file operand with None -> Stuck | Some word -> Done word)

let[@def] (step @ total) code state = match state with
  | Done _ | Stuck -> state
  | Running (pc, file) ->
    match nth code pc with
    | None -> Stuck
    | Some instruction -> execute instruction file

type fuel = Z | S of fuel [@@inductive]

let[@def] rec (advance @ total) code fuel state = match fuel with
  | Z -> state
  | S rest -> advance code rest (step code state)

let[@def] rec (length @ total) xs = match xs with
  | [] -> 0
  | _ :: rest -> 1 + length rest

let[@def] (valid_reg @ total) count reg = 0 <= reg && reg < count

let[@def] (valid_operand @ total) count operand = match operand with
  | Imm _ -> true
  | Reg reg -> valid_reg count reg

let[@def] rec (all_valid_reg @ total) count regs = match regs with
  | [] -> true
  | reg :: rest -> valid_reg count reg && all_valid_reg count rest

let[@def] (valid_instruction @ total) registers nodes instruction =
  let valid_label label = valid_reg nodes label in
  match instruction with
  | Move (dst, operand, next) ->
    valid_reg registers dst && valid_operand registers operand && valid_label next
  | Binary (dst, _, left, right, next) ->
    valid_reg registers dst && valid_operand registers left
    && valid_operand registers right && valid_label next
  | Jump next -> valid_label next
  | Branch (condition, yes, no) ->
    valid_operand registers condition && valid_label yes && valid_label no
  | Return operand -> valid_operand registers operand

let[@def] rec (all_valid_instructions @ total) registers nodes code =
  match code with
  | [] -> true
  | instruction :: rest ->
    valid_instruction registers nodes instruction
    && all_valid_instructions registers nodes rest

let[@def] (valid @ total) program =
  let nodes = length program.code in
  0 < nodes && nodes <= 64 && 0 < program.registers && program.registers <= 32
  && all_valid_reg program.registers program.inputs
  && all_valid_instructions program.registers nodes program.code

type allocation = {
  code : instruction list;
  physical : int;
  source_registers : int;
  source_inputs : int list;
  input_slots : (int * int) list;
}

let[@def] rec (same_shape @ total) xs ys = match xs, ys with
  | [], [] -> true
  | _ :: xs, _ :: ys -> same_shape xs ys
  | _ -> false

let[@def] rec (load_inputs @ total) file registers values =
  match registers, values with
  | [], [] -> Some file
  | reg :: registers, value :: values ->
    (match write file reg value with
     | None -> None
     | Some file -> load_inputs file registers values)
  | _ -> None

let[@def] (source_initial @ total) (program : program) args =
  match load_inputs (zeros program.registers) program.inputs args with
  | None -> Stuck
  | Some file -> Running (0, file)

let[@def] rec (load_slots @ total) file source slots = match slots with
  | [] -> Some file
  | (reg, physical) :: rest ->
    (match load_slots file source rest, nth source reg with
     | Some file, Some value -> write file physical value
     | _ -> None)

let[@def] (target_initial @ total)
    physical source_registers source_inputs input_slots args =
  let loaded =
    if same_shape source_inputs args then
      match load_inputs (zeros source_registers) source_inputs args with
      | None -> None
      | Some source ->
        load_slots (zeros physical) source input_slots
    else None in
  match loaded with
  | None -> Stuck
  | Some file -> Running (0, file)

let[@def] (initial_of_allocation @ total) (allocation : allocation) args =
  target_initial allocation.physical allocation.source_registers
    allocation.source_inputs allocation.input_slots args

let[@def] (observable_equal @ total) source target =
  match source, target with
  | Done left, Done right -> left = right
  | Running (pc, _), Running (other_pc, _) -> pc = other_pc
  | _ -> false
