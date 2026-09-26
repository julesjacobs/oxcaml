module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module Codec = Hmc_pointer_frame_codec
module Index = Hmc_u32_index
module Copy = Wasm_parallel_copy
module E = Wasm_execution
module PC = Hmc_wasm_pc_update
module Geometry = Hmc_wasm_relayout_geometry
type count = {n : W.limb | n <= 268435452}
let[@def] (limit @ total) (unit : unit) : W.limb = 268435452
let[@def] rec (range_is @ total) (plan : Copy.plan @ immutable) (source : int) (destination : int)
    (length : D.index @ immutable) (tail : Copy.plan @ immutable) = ghost_ (
  match length with
  | D.Z -> plan === tail
  | D.S remaining -> (match plan with
    | Copy.Copy (a, b, Copy.Copy (c, d, rest)) -> a = 16 + 16 * source && b = 16 + 16 * destination
      && c = 24 + 16 * source && d = 24 + 16 * destination && range_is rest (source + 1) (destination + 1) remaining tail
    | _ -> false))
let rec (range @ total) : (source : count) -> (destination : count) -> (count : count) ->
    (tail : Copy.plan) @ immutable -> (length : D.index) @ immutable ->
    {u : unit | Index.represents length count && source + count <= 268435452 && destination + count <= 268435452} ->
    {plan : Copy.plan | range_is plan source destination length tail && Geometry.split plan source destination length === Some tail} @ immutable = fun source destination count tail length premise ->
  ghost_ (Index.represents_def length count);
  match length with
  | D.Z -> ghost_ (range_is_def tail source destination length tail; Geometry.split_def tail source destination length); tail
  | D.S remaining ->
    let rest = range (source + 1) (destination + 1) (count - 1) tail remaining () in
    let plan = Copy.Copy (16 + 16 * source, 16 + 16 * destination,
      Copy.Copy (24 + 16 * source, 24 + 16 * destination, rest)) in
    ghost_ (range_is_def plan source destination length tail; Geometry.split_def plan source destination length); plan
type fragment = {copies : Copy.plan; pc : W.limb; required : count}
let[@def] (encodable @ total) (signature : G.signature @ immutable) (instruction : G.instruction @ immutable)
    (capacity : count) (max_pc : W.limb) = ghost_ (
  let env_size = Codec.locals_size signature.G.locals in
  let temp_size = Codec.temporaries_size signature.G.temporaries in
  let env = Geometry.size env_size in let temps = Geometry.size temp_size in
  Index.fits env_size capacity && Index.fits temp_size capacity && 2 + env + temps <= capacity
  && match instruction with
  | G.Save_environment next -> Index.fits next max_pc && 2 + 2 * env + temps <= capacity
  | G.Save_value next | G.Bind next | G.Restore next -> Index.fits next max_pc
    && (match signature.G.temporaries with
      | G.Environment (saved_context, rest_schema) ->
        let saved_size = Codec.locals_size saved_context in let rest_size = Codec.temporaries_size rest_schema in
        let saved = Geometry.size saved_size in let rest = Geometry.size rest_size in
        Index.fits saved_size capacity && Index.fits rest_size capacity && 2 + env + saved + rest <= capacity
        && (match instruction with G.Restore _ -> 2 + saved + rest <= capacity | _ -> 3 + 2 * saved + rest <= capacity)
      | _ -> false)
  | _ -> false)
let (build @ total) (signature : G.signature @ immutable) (instruction : G.instruction @ immutable)
    (capacity : count) (max_pc : W.limb) :
    {out : fragment option | match out with None -> not (encodable signature instruction capacity max_pc) | Some fragment ->
      encodable signature instruction capacity max_pc && Geometry.matches signature instruction capacity max_pc fragment.copies fragment.pc fragment.required} @ immutable =
  ghost_ (encodable_def signature instruction capacity max_pc);
  match Index.encode capacity (Codec.locals_size signature.G.locals), Index.encode capacity (Codec.temporaries_size signature.G.temporaries) with
  | Some env, Some temporaries ->
    ghost_ (Geometry.size_represents (Codec.locals_size signature.G.locals) env ();
      Geometry.size_represents (Codec.temporaries_size signature.G.temporaries) temporaries ());
    if 2 + env + temporaries > capacity then None else
    (match instruction with
    | G.Save_environment next ->
      let required = 2 + 2 * env + temporaries in
      if required > capacity then None else
      (match Index.encode max_pc next with
      | None -> None
      | Some pc ->
        let rest = range (2 + env) (2 + 2 * env) temporaries Copy.End (Codec.temporaries_size signature.G.temporaries) () in
        let copies = range 2 (2 + env) env rest (Codec.locals_size signature.G.locals) () in
        ghost_ (Geometry.two_def copies 2 (2 + env) (Codec.locals_size signature.G.locals)
          (2 + env) (2 + 2 * env) (Codec.temporaries_size signature.G.temporaries);
          Geometry.matches_def signature instruction capacity max_pc copies pc required);
        Some {copies; pc; required})
    | G.Save_value next | G.Bind next | G.Restore next ->
      (match signature.G.temporaries with
      | G.Environment (saved_context, rest_schema) ->
        (match Index.encode capacity (Codec.locals_size saved_context), Index.encode capacity (Codec.temporaries_size rest_schema) with
        | Some saved, Some rest_count ->
          ghost_ (Geometry.size_represents (Codec.locals_size saved_context) saved ();
            Geometry.size_represents (Codec.temporaries_size rest_schema) rest_count ());
          if 2 + env + saved + rest_count > capacity then None else
          let required = match instruction with G.Restore _ -> 2 + saved + rest_count | _ -> 3 + 2 * saved + rest_count in
          if required > capacity then None else
          (match Index.encode max_pc next with
          | None -> None
          | Some pc ->
            ghost_ (Index.represents_def (D.S D.Z) 1; Index.represents_def D.Z 0);
            let copies = match instruction with
              | G.Restore _ ->
                let rest = range (2 + env + saved) (2 + saved) rest_count Copy.End (Codec.temporaries_size rest_schema) () in
                let copies = range (2 + env) 2 saved rest (Codec.locals_size saved_context) () in
                ghost_ (Geometry.two_def copies (2 + env) 2 (Codec.locals_size saved_context)
                  (2 + env + saved) (2 + saved) (Codec.temporaries_size rest_schema)); copies
              | G.Save_value _ ->
                let rest = range (2 + env + saved) (3 + 2 * saved) rest_count Copy.End (Codec.temporaries_size rest_schema) () in
                let saved_copy = range (2 + env) (3 + saved) saved rest (Codec.locals_size saved_context) () in
                let value_copy = range 1 (2 + saved) 1 saved_copy (D.S D.Z) () in
                let copies = range (2 + env) 2 saved value_copy (Codec.locals_size saved_context) () in
                ghost_ (Geometry.two_def saved_copy (2 + env) (3 + saved) (Codec.locals_size saved_context)
                  (2 + env + saved) (3 + 2 * saved) (Codec.temporaries_size rest_schema);
                  Geometry.four_def copies (2 + env) 2 (Codec.locals_size saved_context) 1 (2 + saved) (D.S D.Z)
                    (2 + env) (3 + saved) (Codec.locals_size saved_context) (2 + env + saved) (3 + 2 * saved) (Codec.temporaries_size rest_schema)); copies
              | _ ->
                let rest = range (2 + env + saved) (3 + 2 * saved) rest_count Copy.End (Codec.temporaries_size rest_schema) () in
                let saved_copy = range (2 + env) (3 + saved) saved rest (Codec.locals_size saved_context) () in
                let env_copy = range (2 + env) 3 saved saved_copy (Codec.locals_size saved_context) () in
                let copies = range 1 2 1 env_copy (D.S D.Z) () in
                ghost_ (Geometry.two_def saved_copy (2 + env) (3 + saved) (Codec.locals_size saved_context)
                  (2 + env + saved) (3 + 2 * saved) (Codec.temporaries_size rest_schema);
                  Geometry.four_def copies 1 2 (D.S D.Z) (2 + env) 3 (Codec.locals_size saved_context)
                    (2 + env) (3 + saved) (Codec.locals_size saved_context) (2 + env + saved) (3 + 2 * saved) (Codec.temporaries_size rest_schema)); copies in
            ghost_ (Geometry.matches_def signature instruction capacity max_pc copies pc required);
            Some {copies; pc; required})
        | _ -> None)
      | _ -> None)
    | _ -> None)
  | _ -> None
let[@def] (emit @ total) (fragment : fragment @ immutable) (base_local : B.u32) =
  E.append (Copy.emit fragment.copies base_local) (PC.emit fragment.pc base_local)
let[@def] (finish @ total) (memory : B.bytes @ immutable) (base : B.u32) (pc : W.limb) : B.bytes option @ immutable =
  Wasm_memory.store memory base (PC.offset ()) (Wasm_scalar.I64 (Hmc_wasm_header_update.number pc))
let (correct @ total) : (fragment : fragment) @ immutable -> (base_local : B.u32) ->
    (state : Wasm_memory_execution.state) @ immutable -> (base : B.u32) ->
    (copied : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    {u : unit | Wasm_locals.get state.Wasm_memory_execution.machine.E.locals base_local === Some (Wasm_scalar.I32 base)
      && Copy.apply fragment.copies state.Wasm_memory_execution.memory base === Some copied
      && finish copied base fragment.pc === Some after} ->
    {u : unit | Wasm_memory_execution.run (emit fragment base_local) state ===
      Wasm_memory_execution.Done {Wasm_memory_execution.memory = after; machine = state.Wasm_memory_execution.machine}} @ ghost =
  fun fragment base_local state base copied after premise -> ghost_ (
    finish_def copied base fragment.pc;
    Copy.correct fragment.copies base_local state base copied ();
    PC.correct fragment.pc base_local {Wasm_memory_execution.memory = copied; machine = state.Wasm_memory_execution.machine} base after ();
    emit_def fragment base_local;
    Wasm_memory_execution.append_correct (Copy.emit fragment.copies base_local) (PC.emit fragment.pc base_local) state)
