module B = Wasm_u32
module W = Hmc_word64
module V = Hmc_tagged_cell
module G = Hmc_cfg_ir
module Index = Hmc_u32_index
module Literal = Hmc_wasm_literal_load
type slot = {number : W.limb | number <= 268435452}
let[@def] (slot_limit @ total) (unit : unit) : W.limb = 268435452
let[@def] (slot_tag @ total) (number : slot) : B.u32 = 48 + 16 * number
let[@def] (slot_payload @ total) (number : slot) : B.u32 = 56 + 16 * number
type fragment = Local of slot * W.limb | Literal of V.value * W.limb | Jump of W.limb | Branch of W.limb * W.limb [@@inductive]
let[@def] (admitted @ total) (instruction : G.instruction @ immutable) (capacity : W.limb) = match instruction with
  | G.Load (G.Local index, _, _, next) -> Index.fits index (slot_limit ()) && Index.fits next capacity
  | G.Load (atom, _, _, next) -> (match Literal.literal atom with None -> false | Some _ -> Index.fits next capacity)
  | G.Jump next -> Index.fits next capacity
  | G.Branch (yes, no) -> Index.fits yes capacity && Index.fits no capacity
  | _ -> false
let[@def] (corresponds @ total) (instruction : G.instruction @ immutable) (fragment : fragment @ immutable) = ghost_ (
  match instruction, fragment with
  | G.Load (G.Local index, _, _, next), Local (number, pc) -> Index.represents index number && Index.represents next pc
  | G.Load (atom, _, _, next), Literal (value, pc) -> Literal.literal atom === Some value && Index.represents next pc
  | G.Jump next, Jump pc -> Index.represents next pc
  | G.Branch (yes, no), Branch (yes_pc, no_pc) -> Index.represents yes yes_pc && Index.represents no no_pc
  | _ -> false)
let (lower @ total) : (instruction : G.instruction) @ immutable -> (capacity : W.limb) ->
    {out : fragment option | match out with None -> not (admitted instruction capacity)
      | Some fragment -> admitted instruction capacity && corresponds instruction fragment} @ immutable = fun instruction capacity ->
  ghost_ (admitted_def instruction capacity);
  match instruction with
  | G.Load (G.Local index, _, _, next) -> (match Index.encode (slot_limit ()) index with
    | None -> None
    | Some number -> (match Index.encode capacity next with
      | None -> None
      | Some pc ->
        ghost_ (slot_limit_def ());
        let out = Local (number, pc) in ghost_ (corresponds_def instruction out); Some out))
  | G.Load (atom, _, _, next) -> (match Literal.literal atom with
    | None -> None
    | Some value -> (match Index.encode capacity next with
      | None -> None
      | Some pc -> let out = Literal (value, pc) in ghost_ (corresponds_def instruction out); Some out))
  | G.Jump next -> (match Index.encode capacity next with
    | None -> None
    | Some pc -> let out = Jump pc in ghost_ (corresponds_def instruction out); Some out)
  | G.Branch (yes, no) -> (match Index.encode capacity yes with
    | None -> None
    | Some yes_pc -> (match Index.encode capacity no with
      | None -> None
      | Some no_pc -> let out = Branch (yes_pc, no_pc) in ghost_ (corresponds_def instruction out); Some out))
  | _ -> None
let[@def] (emit @ total) (fragment : fragment @ immutable) (base_local : B.u32) = match fragment with
  | Local (number, pc) -> Hmc_wasm_local_load.emit (slot_tag number) (slot_payload number) pc base_local
  | Literal (value, pc) -> Literal.emit pc value base_local
  | Jump pc -> Hmc_wasm_pc_update.emit pc base_local
  | Branch (yes, no) -> Hmc_wasm_branch.emit yes no base_local
