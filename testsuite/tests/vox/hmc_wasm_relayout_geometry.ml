module D = Hm_declarative
module G = Hmc_cfg_ir
module Codec = Hmc_pointer_frame_codec
module Copy = Wasm_parallel_copy
module Index = Hmc_u32_index
module W = Hmc_word64
let[@def] rec (size @ total) (index : D.index @ immutable) = ghost_ (match index with D.Z -> 0 | D.S rest -> 1 + size rest)
let rec (size_represents @ total) : (index : D.index) @ immutable -> (number : W.limb) ->
    {u : unit | Index.represents index number} -> {u : unit | size index = number} @ ghost =
  fun index number premise -> ghost_ (
    size_def index; Index.represents_def index number;
    match index with D.Z -> () | D.S rest -> size_represents rest (number - 1) ())
let[@def] rec (split @ total) (plan : Copy.plan @ immutable) (source : int) (destination : int) (length : D.index @ immutable) = ghost_ (
  match length with
  | D.Z -> Some plan
  | D.S remaining -> (match plan with
    | Copy.Copy (a, b, Copy.Copy (c, d, rest)) ->
      if a = 16 + 16 * source && b = 16 + 16 * destination && c = 24 + 16 * source && d = 24 + 16 * destination
      then split rest (source + 1) (destination + 1) remaining else None
    | _ -> None))

let[@def] (two @ total) (plan : Copy.plan @ immutable) (a : int) (b : int) (n : D.index @ immutable)
    (c : int) (d : int) (m : D.index @ immutable) = ghost_ (
  match split plan a b n with None -> false | Some rest -> split rest c d m === Some Copy.End)
let[@def] (four @ total) (plan : Copy.plan @ immutable) (a : int) (b : int) (n : D.index @ immutable)
    (c : int) (d : int) (m : D.index @ immutable) (e : int) (f : int) (k : D.index @ immutable)
    (g : int) (h : int) (l : D.index @ immutable) = ghost_ (
  match split plan a b n with None -> false | Some rest ->
    match split rest c d m with None -> false | Some rest -> two rest e f k g h l)
let[@def] (matches @ total) (signature : G.signature @ immutable) (instruction : G.instruction @ immutable)
    (capacity : W.limb) (max_pc : W.limb) (plan : Copy.plan @ immutable) (pc : W.limb) (required : int) = ghost_ (
  let env_size = Codec.locals_size signature.G.locals in
  let temp_size = Codec.temporaries_size signature.G.temporaries in
  let env = size env_size in
  let temps = size temp_size in
  Index.fits env_size capacity && Index.fits temp_size capacity &&
  pc <= max_pc && required <= capacity && 2 + env + temps <= capacity &&
  match instruction with
  | G.Save_environment next -> Index.represents next pc && required = 2 + 2 * env + temps
    && two plan 2 (2 + env) env_size (2 + env) (2 + 2 * env) temp_size
  | G.Save_value next | G.Bind next | G.Restore next ->
    Index.represents next pc && (match signature.G.temporaries with
    | G.Environment (context, schema) ->
      let saved_size = Codec.locals_size context in
      let rest_size = Codec.temporaries_size schema in
      let saved = size saved_size in
      let rest = size rest_size in
      Index.fits saved_size capacity && Index.fits rest_size capacity &&
      2 + env + saved + rest <= capacity &&
      (match instruction with
      | G.Restore _ -> required = 2 + saved + rest
        && two plan (2 + env) 2 saved_size (2 + env + saved) (2 + saved) rest_size
      | G.Save_value _ -> required = 3 + 2 * saved + rest
        && four plan (2 + env) 2 saved_size 1 (2 + saved) (D.S D.Z)
          (2 + env) (3 + saved) saved_size (2 + env + saved) (3 + 2 * saved) rest_size
      | _ -> required = 3 + 2 * saved + rest
        && four plan 1 2 (D.S D.Z) (2 + env) 3 saved_size
          (2 + env) (3 + saved) saved_size (2 + env + saved) (3 + 2 * saved) rest_size)
    | _ -> false)
  | _ -> false)
