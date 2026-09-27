(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 readonly_files = "wasm_differential_harness.js";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml wasm_u32.ml wasm_i32.ml wasm_i64.ml wasm_instruction.ml wasm_code.ml wasm_scalar.ml wasm_locals.ml wasm_execution.ml wasm_word_memory.ml hmc_tagged_cell.ml hmc_linear_bytes.ml wasm_memory.ml wasm_memory_execution.ml wasm_control.ml wasm_framing.ml wasm_data_section.ml wasm_export_section.ml wasm_control_compose.ml wasm_execution_budget.ml wasm_nesting.ml wasm_control_codec.ml wasm_functions.ml wasm_instruction_stream.ml wasm_local_declarations.ml wasm_function_body.ml wasm_index_vector.ml wasm_code_section.ml wasm_signature_section.ml wasm_function_sections.ml wasm_global_entry.ml wasm_globals.ml wasm_global_section.ml wasm_limits_section.ml wasm_section.ml wasm_module.ml wasm_binary_module.ml wasm_global_execution.ml wasm_control_lift.ml wasm_instance_control.ml wasm_calls.ml wasm_binary_execution.ml wasm_static_types.ml wasm_static_control.ml wasm_static_typing.ml wasm_static_module.ml wasm_differential_gen.ml wasm_differential.ml";
 has-node;
 native;
*)

(* Differential test of the Vox WebAssembly model against an engine (Node).
   Each case is a module from Wasm_differential_gen, encoded by that file's
   own encoder. The model decodes, validates and runs the bytes
   (Wasm_static_module.bytes_valid, Wasm_binary_execution.run); Node validates,
   instantiates and runs the same bytes with wasm_differential_harness.js. Both
   set the exported global payload to the case's input before calling run. The
   two must agree on validity and, for valid modules, on trap versus return,
   the returned value, every global and every byte of final memory. The
   model's fuel and host call limits and its unsupported instructions give no
   verdict and are counted.

   Without arguments it runs the fixed configuration of the ocamltest test.
   Options: -count N -seed S -from I -fuel STEPS -capacity DEPTH -pages P
   -harness FILE -keep DIR (write disagreeing modules) -minimize -verbose *)

module G = Wasm_differential_gen
module B = Wasm_u32
module C = Wasm_code
module S = Wasm_scalar

(* ---------- The model ---------- *)

let byte n : B.byte = if 0 <= n && n < 256 then n else invalid_arg "byte"
let bytes_of_string s =
  let rec go i acc = if i < 0 then acc else go (i - 1) (B.Byte (byte (Char.code s.[i]), acc)) in
  go (String.length s - 1) B.End
let string_of_bytes bytes =
  let b = Buffer.create 65536 in
  let rec go = function B.End -> () | B.Byte (x, rest) -> Buffer.add_char b (Char.chr x); go rest in
  go bytes; Buffer.contents b
let count n = let rec go n acc = if n = 0 then acc else go (n - 1) (C.Succ acc) in go n C.Zero

let show_value = function
  | S.I32 n -> Printf.sprintf "i32:%d" n
  | S.I64 w -> Printf.sprintf "i64:%Lu"
      (Int64.logor (Int64.shift_left (Int64.of_int w.Hmc_word64.hi) 32) (Int64.of_int w.Hmc_word64.lo))
let rec values = function S.Empty -> [] | S.Push (v, rest) -> show_value v :: values rest

type model =
  | Invalid
  | Not_materialized
  | Finished of { result : string; globals : string list; memory : string; tag : int; payload : int }
  | Trap | Type_error | Host_limit | Not_supported | Out_of_fuel

let model ~fuel ~capacity ~input s =
  let bytes = bytes_of_string s in
  if not (Wasm_static_module.bytes_valid bytes) then Invalid else
  let exports = match Wasm_binary_module.decode bytes with
    | Some (image, _) -> image.Wasm_binary_module.exports
    | None -> failwith "a valid module does not decode" in
  match Wasm_binary_execution.run fuel bytes input capacity with
  | Wasm_binary_execution.Rejected -> Not_materialized
  | Wasm_binary_execution.Result (Wasm_calls.Finished state) ->
    let execution = state.Wasm_global_execution.execution in
    let result = match values execution.Wasm_memory_execution.machine.Wasm_execution.stack with
      | [] -> "void" | [v] -> v | vs -> "stack:" ^ String.concat "," vs in
    Finished { result; globals = values state.Wasm_global_execution.globals.Wasm_globals.values;
      memory = string_of_bytes execution.Wasm_memory_execution.memory;
      tag = exports.Wasm_export_section.tag; payload = exports.Wasm_export_section.payload }
  | Wasm_binary_execution.Result Wasm_calls.Trap -> Trap
  | Wasm_binary_execution.Result Wasm_calls.Type_error -> Type_error
  | Wasm_binary_execution.Result Wasm_calls.Host_limit -> Host_limit
  | Wasm_binary_execution.Result Wasm_calls.Not_supported -> Not_supported
  | Wasm_binary_execution.Result (Wasm_calls.Running _) -> Out_of_fuel

(* ---------- The engine ---------- *)

type engine = { valid : bool; outcome : string; fields : (string * string) list }

let parse line =
  let fields = List.filter_map (fun part -> match String.index_opt part '=' with
    | Some i -> Some (String.sub part 0 i, String.sub part (i + 1) (String.length part - i - 1))
    | None -> None) (String.split_on_char ' ' line) in
  let get k = try List.assoc k fields with Not_found -> "missing" in
  (int_of_string (get "id"), { valid = get "valid" = "1"; outcome = get "outcome"; fields })

let field e k = try List.assoc k e.fields with Not_found -> "missing"

let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in close_in ic; s
let write_file path s = let oc = open_out_bin path in output_string oc s; close_out oc
let remove path = if Sys.file_exists path then Sys.remove path

let harness = ref "wasm_differential_harness.js"
let timeout = ref 3000

(* Run the harness on ids [from, to) of dir. *)
let engine dir from to_ =
  let out = Filename.concat dir "engine.txt" in
  let command = Printf.sprintf "node %s %s %d %d %d > %s" (Filename.quote !harness)
      (Filename.quote dir) from to_ !timeout (Filename.quote out) in
  if Sys.command command <> 0 then failwith ("harness failed: " ^ command);
  let table = Hashtbl.create 64 in
  let ic = open_in out in
  (try while true do
      let line = input_line ic in
      if line <> "" then let (id, e) = parse line in Hashtbl.replace table id e
    done with End_of_file -> ());
  close_in ic; remove out; table

(* ---------- Cases ---------- *)

type kind = Generated | Mutated of string | Corrupted of string | Outside of string | Unmaterialized of string
type case = { id : int; kind : kind; module_ : G.module_ option; style : G.style;
              canonical : string; variant : string option; input : Hmc_word64.t }

let kind_name = function
  | Generated -> "generated" | Mutated _ -> "mutated" | Corrupted _ -> "corrupted"
  | Outside _ -> "outside the subset" | Unmaterialized _ -> "not instantiated by the model"
let kind_detail = function
  | Generated -> "generated" | Mutated s | Corrupted s | Outside s | Unmaterialized s -> s

let build input id kind style m =
  { id; kind; module_ = Some m; style; canonical = G.encode style m; input;
    variant = Some (G.encode { style with G.export_globals = true } m) }

(* The input a case's module runs on, drawn apart from the module so that the
   modules do not depend on it. *)
let input ~seed id =
  let r = Random.State.make [| seed; id; 1 |] in
  let lo = Random.State.full_int r 0x100000000 in
  let hi = Random.State.full_int r 0x100000000 in
  if lo < 0 || lo >= 0x100000000 || hi < 0 || hi >= 0x100000000 then invalid_arg "input"
  else { Hmc_word64.lo; hi }

let make ~seed ~pages id =
  let input = input ~seed id in
  let build = build input in
  let r = Random.State.make [| seed; id |] in
  let settings = { G.guarded = not (G.chance r 0.15); unsupported = G.chance r 0.15; max_pages = pages } in
  let m = G.generate r settings in
  let pad = if G.chance r 0.3 then Some (Random.State.bits r) else None in
  let base = { G.plain_style with pad } in
  let roll = G.rint r 100 in
  if roll < 60 then build id Generated base m
  else if roll < 80 then
    (match G.mutate r m with
     | Some (name, m, style) -> build id (Mutated name) { style with G.pad } m
     | None -> build id Generated base m)
  else if roll < 87 then
    let (name, bytes) = G.corrupt r (G.encode base m) (String.length m.G.data) in
    { id; kind = Corrupted name; module_ = None; style = base; canonical = bytes; variant = None; input }
  else
    let (name, m, style, materialization) = G.outside r m in
    build id (if materialization then Unmaterialized name else Outside name) { style with G.pad } m

(* ---------- Comparison ---------- *)

(* Limit: a module the model accepts that exceeds an implementation limit the
   JS API sets for engines (the core specification allows such limits). Engines
   enforce some limits when validating and others, depending on the version,
   only when instantiating (Node 22 checks the table size at instantiation,
   Node 25 at validation), so both count. *)
type verdict = Agree of string | Expected of string | Limit of string | No_verdict of string | Disagree of string

let first_difference a b =
  if String.length a <> String.length b then
    Printf.sprintf "memory size %d vs %d" (String.length a) (String.length b)
  else begin
    let i = ref 0 in
    while !i < String.length a && a.[!i] = b.[!i] do incr i done;
    Printf.sprintf "memory byte %d: model %d, engine %d" !i (Char.code a.[!i]) (Char.code b.[!i])
  end

let limit_mutation case = match case.kind with
  | Mutated ("more locals than the engine allows" | "table above the engine's size limit") -> true
  | _ -> false

let compare_case dir case model engine =
  let subset = field engine "subset" in
  let inside = subset = "yes" in
  match model, engine.valid with
  | _, true when not inside && (match case.kind with Generated | Mutated _ -> true | _ -> false) ->
    Disagree ("harness: a generated module is outside the model's subset (" ^ subset ^ ")")
  | _, true when inside && (match case.kind with Outside _ -> true | _ -> false) ->
    Disagree "harness: a module meant to leave the subset is inside it"
  | Invalid, false -> Agree "both reject"
  | Invalid, true ->
    if inside then Disagree "model rejects a valid module in its binary subset"
    else Expected ("model rejects a valid module outside its binary subset ("
                   ^ String.sub subset 3 (String.length subset - 3) ^ ")")
  | _, false when limit_mutation case ->
    Limit ("model validates a module above a JS API implementation limit: " ^ field engine "detail")
  | _, false -> Disagree ("model validates a module the engine rejects: " ^ field engine "detail")
  | _, true when not inside ->
    Disagree ("model validates a module the subset recognizer rejects (" ^ subset ^ ")")
  | _, true when limit_mutation case && engine.outcome = "instantiate_error"
                 && String.starts_with ~prefix:"RangeError:" (field engine "detail") ->
    Limit ("model validates a module above a JS API implementation limit: " ^ field engine "detail")
  | Not_materialized, true -> No_verdict ("model does not instantiate (engine: " ^ engine.outcome ^ ")")
  | Out_of_fuel, true -> No_verdict ("model fuel exhausted (engine: " ^ engine.outcome ^ ")")
  | Host_limit, true -> No_verdict ("model host call depth (engine: " ^ engine.outcome ^ ")")
  | Not_supported, true -> No_verdict ("model instruction not supported (engine: " ^ engine.outcome ^ ")")
  | Type_error, true -> Disagree ("model type error on a valid module (engine: " ^ engine.outcome ^ ")")
  | Trap, true ->
    (match engine.outcome with
     | "trap" -> Agree ("both trap (engine: " ^ String.map (fun c -> if c = '_' then ' ' else c) (field engine "detail") ^ ")")
     | "stack_overflow" -> No_verdict "engine stack limit (model: trap)"
     | o -> Disagree ("model traps, engine: " ^ o))
  | Finished f, true ->
    (match engine.outcome with
     | "stack_overflow" -> No_verdict "engine stack limit (model: return)"
     | "return" ->
       let global k = match List.nth_opt f.globals k with Some v -> v | None -> "missing" in
       let problems = List.concat [
         (if field engine "value" <> f.result then
            [Printf.sprintf "result: model %s, engine %s" f.result (field engine "value")] else []);
         (if field engine "tag" <> global f.tag then
            [Printf.sprintf "tag: model %s, engine %s" (global f.tag) (field engine "tag")] else []);
         (if field engine "payload" <> global f.payload then
            [Printf.sprintf "payload: model %s, engine %s" (global f.payload) (field engine "payload")] else []);
         (let path = Filename.concat dir (string_of_int case.id ^ ".memory") in
          if not (Sys.file_exists path) then ["engine memory missing"] else
            let memory = read_file path in
            if memory <> f.memory then [first_difference f.memory memory] else []);
         (match case.variant with
          | None -> []
          | Some _ ->
            let expected = match f.globals with [] -> "none" | gs -> String.concat "," gs in
            (if field engine "variant" <> "return" then ["engine variant: " ^ field engine "variant"] else []) @
            (if field engine "variant_value" <> f.result then ["engine variant result differs"] else []) @
            (if field engine "globals" <> expected then
               [Printf.sprintf "globals: model %s, engine %s" expected (field engine "globals")] else [])) ] in
       if problems = [] then Agree "both return the same result, globals and memory"
       else Disagree ("model and engine return different states: " ^ String.concat "; " problems)
     | o -> Disagree ("model returns " ^ f.result ^ ", engine: " ^ o ^ " " ^ field engine "detail"))

(* ---------- Running cases ---------- *)

let model_seconds = ref 0.0
type config = { fuel : C.count; capacity : C.count; dir : string }

let show_word (w : Hmc_word64.t) =
  Printf.sprintf "%Lu" (Int64.logor (Int64.shift_left (Int64.of_int w.Hmc_word64.hi) 32) (Int64.of_int w.Hmc_word64.lo))

let write_case dir case =
  write_file (Filename.concat dir (string_of_int case.id ^ ".wasm")) case.canonical;
  write_file (Filename.concat dir (string_of_int case.id ^ ".input")) (show_word case.input);
  Option.iter (write_file (Filename.concat dir (string_of_int case.id ^ ".variant.wasm"))) case.variant

let clean_case dir id =
  List.iter (fun suffix -> remove (Filename.concat dir (string_of_int id ^ suffix)))
    [".wasm"; ".variant.wasm"; ".memory"; ".input"]

(* Evaluate cases whose ids are distinct and within [from, to_). *)
let evaluate config cases from to_ =
  List.iter (write_case config.dir) cases;
  let engines = engine config.dir from to_ in
  let results = List.map (fun case ->
    let start = Sys.time () in
    let m = model ~fuel:config.fuel ~capacity:config.capacity ~input:case.input case.canonical in
    model_seconds := Sys.time () -. start;
    let verdict = match Hashtbl.find_opt engines case.id with
      | None -> Disagree "engine produced no result (harness)"
      | Some e -> compare_case config.dir case m e in
    (case, !model_seconds, verdict)) cases in
  List.iter (fun case -> clean_case config.dir case.id) cases;
  results

(* ---------- Coverage: instructions the model executes ---------- *)

let coverage = Hashtbl.create 64
let instruction_name (i : Wasm_instruction.t) = match i with
  | Wasm_instruction.Plain p -> Printf.sprintf "%s" (match p with
    | Wasm_instruction.Unreachable -> "unreachable" | Nop -> "nop" | Else -> "else" | End -> "end"
    | Return -> "return" | Drop -> "drop" | Select -> "select" | I32_eqz -> "i32.eqz" | I32_eq -> "i32.eq"
    | I32_ne -> "i32.ne" | I32_lt_u -> "i32.lt_u" | I32_gt_u -> "i32.gt_u" | I32_le_u -> "i32.le_u"
    | I32_ge_u -> "i32.ge_u" | I64_eqz -> "i64.eqz" | I64_eq -> "i64.eq" | I64_lt_u -> "i64.lt_u"
    | I32_add -> "i32.add" | I32_sub -> "i32.sub" | I32_mul -> "i32.mul" | I32_and -> "i32.and"
    | I32_or -> "i32.or" | I64_add -> "i64.add" | I64_sub -> "i64.sub" | I64_and -> "i64.and"
    | I64_or -> "i64.or" | I64_shl -> "i64.shl" | I64_shr_u -> "i64.shr_u" | I32_wrap_i64 -> "i32.wrap_i64"
    | I64_extend_i32_u -> "i64.extend_i32_u")
  | I32_const _ -> "i32.const" | I64_const _ -> "i64.const" | Br _ -> "br" | Br_if _ -> "br_if"
  | Call _ -> "call" | Local_get _ -> "local.get" | Local_set _ -> "local.set" | Local_tee _ -> "local.tee"
  | Global_get _ -> "global.get" | Global_set _ -> "global.set" | Block -> "block" | Loop -> "loop" | If -> "if"
  | Call_indirect _ -> "call_indirect" | I32_load _ -> "i32.load" | I64_load _ -> "i64.load"
  | I32_store _ -> "i32.store" | I64_store _ -> "i64.store"

(* Step the model as Wasm_binary_execution.run does, counting what it executes. *)
let trace ~fuel ~capacity ~input s =
  let bytes = bytes_of_string s in
  let bump k = Hashtbl.replace coverage k (1 + Option.value ~default:0 (Hashtbl.find_opt coverage k)) in
  match Wasm_binary_module.decode bytes with
  | Some (image, B.End) when Wasm_binary_execution.materialized image ->
    let module_ = image.Wasm_binary_module.module_ in
    let rec go fuel c = if fuel > 0 then begin
      (match c.Wasm_calls.current.Wasm_instance_control.body.Wasm_control.code with
       | Wasm_control.Empty -> bump "(end of block or function)"
       | Wasm_control.Block _ -> bump "block" | Wasm_control.Loop _ -> bump "loop" | Wasm_control.If _ -> bump "if"
       | Wasm_control.Instruction (i, _) -> bump (instruction_name i));
      match Wasm_calls.step module_ c with
      | Wasm_calls.Running c -> go (fuel - 1) c
      | Wasm_calls.Finished _ -> bump "(outcome: return)" | Wasm_calls.Trap -> bump "(outcome: trap)"
      | _ -> bump "(outcome: other)"
    end in
    (match Wasm_globals.set image.Wasm_binary_module.globals
             image.Wasm_binary_module.exports.Wasm_export_section.payload (S.I64 input) with
     | None -> ()
     | Some globals ->
       match Wasm_calls.start module_ image.Wasm_binary_module.exports.Wasm_export_section.run
               image.Wasm_binary_module.data globals capacity with
       | Wasm_calls.Running c -> go fuel c
       | _ -> ())
  | _ -> ()

(* ---------- Minimization ---------- *)

let rec size code = List.fold_left (fun n i -> n + 1 + match i with
  | G.Block b | G.Loop b -> size b | G.If (y, n) -> size y + size n | _ -> 0) 0 code

(* Apply f to the k-th instruction in preorder. *)
let edit_at k f code =
  let position = ref (-1) in
  let rec go l = List.concat_map (fun i ->
    incr position;
    if !position = k then f i else
      [match i with
       | G.Block b -> G.Block (go b) | G.Loop b -> G.Loop (go b)
       | G.If (y, n) -> let y = go y in G.If (y, go n)
       | i -> i]) l in
  go code

(* Delete the k-th instruction in preorder and the len - 1 siblings after it. *)
let delete_run k len code =
  let position = ref (-1) in
  let rec go = function
    | [] -> []
    | i :: rest ->
      incr position;
      if !position = k then drop (len - 1) rest
      else
        let i = match i with
          | G.Block b -> G.Block (go b) | G.Loop b -> G.Loop (go b)
          | G.If (y, n) -> let y = go y in G.If (y, go n)
          | i -> i in
        i :: go rest
  and drop n l = if n <= 0 then l else match l with [] -> [] | _ :: t -> drop (n - 1) t in
  go code

let candidates (m : G.module_) =
  let per_function i (f : G.func) =
    let n = size f.G.body in
    let set body = { m with G.funcs = List.mapi (fun j g -> if j = i then { g with G.body } else g) m.G.funcs } in
    List.concat (List.init n (fun k ->
      [ set (delete_run k 3 f.G.body); set (delete_run k 2 f.G.body);
        set (edit_at k (fun _ -> []) f.G.body);
        set (edit_at k (function G.Block b | G.Loop b -> b | G.If (y, n) -> [G.Drop] @ y @ n | i -> [i]) f.G.body);
        set (edit_at k (function G.I32_const _ -> [G.I32_const 0l] | G.I64_const _ -> [G.I64_const 0L] | i -> [i]) f.G.body) ])) in
  let emptied = List.mapi (fun i (f : G.func) ->
    let body = match f.G.result with
      | None -> [] | Some G.I32 -> [G.I32_const 0l] | Some G.I64 -> [G.I64_const 0L] in
    if f.G.body = body then None
    else Some { m with G.funcs = List.mapi (fun j g -> if j = i then { g with G.body } else g) m.G.funcs })
    m.G.funcs |> List.filter_map Fun.id in
  let zero = String.make (String.length m.G.data) '\000' in
  emptied @
  (if m.G.data <> zero then [{ m with G.data = zero }] else []) @
  (if List.length m.G.funcs > 1 && m.G.run <> List.length m.G.funcs - 1 then
     [{ m with G.funcs = List.filteri (fun i _ -> i < List.length m.G.funcs - 1) m.G.funcs;
          elems = List.map (fun e -> min e (List.length m.G.funcs - 2)) m.G.elems }] else []) @
  List.concat (List.mapi per_function m.G.funcs)

let same_class a b =
  let prefix s = match String.index_opt s ':' with Some i -> String.sub s 0 i | None -> s in
  prefix a = prefix b

let minimize config case reason =
  match case.module_ with
  | None -> None
  | Some m ->
    let rec loop m reason rounds =
      if rounds = 0 then (m, reason) else
      let batch = List.filter (fun c -> c <> m) (candidates m) in
      let rec chunks l = if l = [] then [] else
          let k = min 200 (List.length l) in
          List.filteri (fun i _ -> i < k) l :: chunks (List.filteri (fun i _ -> i >= k) l) in
      let rec search = function
        | [] -> None
        | chunk :: rest ->
          let cases = List.mapi (fun i c -> build case.input i case.kind case.style c) chunk in
          let results = evaluate config cases 0 (List.length cases) in
          match List.find_opt (fun (_, _, v) -> match v with Disagree r -> same_class r reason | _ -> false) results with
          | Some (c, _, Disagree r) -> Some (Option.get c.module_, r)
          | _ -> search rest in
      match search (chunks batch) with
      | None -> (m, reason)
      | Some (m, reason) -> loop m reason (rounds - 1) in
    Some (loop m reason 200)

(* ---------- Printing modules ---------- *)

let vt = function G.I32 -> "i32" | G.I64 -> "i64"
let rt = function None -> "" | Some t -> " (result " ^ vt t ^ ")"
let op_name n = match n with
  | 0x45 -> "i32.eqz" | 0x46 -> "i32.eq" | 0x47 -> "i32.ne" | 0x49 -> "i32.lt_u" | 0x4b -> "i32.gt_u"
  | 0x4d -> "i32.le_u" | 0x4f -> "i32.ge_u" | 0x50 -> "i64.eqz" | 0x51 -> "i64.eq" | 0x54 -> "i64.lt_u"
  | 0x6a -> "i32.add" | 0x6b -> "i32.sub" | 0x6c -> "i32.mul" | 0x71 -> "i32.and" | 0x72 -> "i32.or"
  | 0x7c -> "i64.add" | 0x7d -> "i64.sub" | 0x83 -> "i64.and" | 0x84 -> "i64.or" | 0x86 -> "i64.shl"
  | 0x88 -> "i64.shr_u" | 0xa7 -> "i32.wrap_i64" | 0xad -> "i64.extend_i32_u"
  | n -> Printf.sprintf "(opcode 0x%02x)" n
let rec print_code indent code =
  List.iter (fun i ->
    let line s = Printf.printf "%s%s\n" indent s in
    match i with
    | G.Unreachable -> line "unreachable" | G.Nop -> line "nop" | G.Drop -> line "drop"
    | G.Select -> line "select" | G.Return -> line "return"
    | G.Block b -> line "block"; print_code (indent ^ "  ") b; line "end"
    | G.Loop b -> line "loop"; print_code (indent ^ "  ") b; line "end"
    | G.If (y, n) -> line "if"; print_code (indent ^ "  ") y;
      if n <> [] then (line "else"; print_code (indent ^ "  ") n); line "end"
    | G.Br d -> line (Printf.sprintf "br %d" d) | G.Br_if d -> line (Printf.sprintf "br_if %d" d)
    | G.Call f -> line (Printf.sprintf "call %d" f)
    | G.Call_indirect t -> line (Printf.sprintf "call_indirect (type %d)" t)
    | G.Local_get k -> line (Printf.sprintf "local.get %d" k)
    | G.Local_set k -> line (Printf.sprintf "local.set %d" k)
    | G.Local_tee k -> line (Printf.sprintf "local.tee %d" k)
    | G.Global_get k -> line (Printf.sprintf "global.get %d" k)
    | G.Global_set k -> line (Printf.sprintf "global.set %d" k)
    | G.Load (t, a, o) -> line (Printf.sprintf "%s.load offset=%d align=%d" (vt t) o (1 lsl a))
    | G.Store (t, a, o) -> line (Printf.sprintf "%s.store offset=%d align=%d" (vt t) o (1 lsl a))
    | G.I32_const v -> line (Printf.sprintf "i32.const %ld" v)
    | G.I64_const v -> line (Printf.sprintf "i64.const %Ld" v)
    | G.Op n -> line (op_name n)
    | G.Raw l -> line ("(bytes" ^ String.concat "" (List.map (Printf.sprintf " %02x") l) ^ ")")) code

let print_module (m : G.module_) =
  List.iteri (fun k t -> Printf.printf "  (type %d (func%s))\n" k (rt t)) m.G.types;
  Printf.printf "  (table %d %d funcref) (elem (i32.const 0) %s)\n" m.G.table_min m.G.table_max
    (String.concat " " (List.map string_of_int m.G.elems));
  let nonzero = String.fold_left (fun n c -> if c <> '\000' then n + 1 else n) 0 m.G.data in
  Printf.printf "  (memory %d %d) (data: %d bytes, %d nonzero)\n" m.G.mem_min m.G.mem_max
    (String.length m.G.data) nonzero;
  List.iteri (fun k g -> Printf.printf "  (global %d %s %s %Ld)\n" k
    (if g.G.mutable_ then "(mut " ^ vt g.G.ty ^ ")" else vt g.G.ty) (vt g.G.ty) g.G.init) m.G.globals;
  Printf.printf "  (export run: func %d; tag: global %d; payload: global %d)\n" m.G.run m.G.tag m.G.payload;
  List.iteri (fun k f ->
    Printf.printf "  (func %d (type %d)%s (locals %s)\n" k f.G.type_index (rt f.G.result)
      (String.concat " " (List.map vt f.G.locals));
    print_code "    " f.G.body; print_string "  )\n") m.G.funcs

(* ---------- Main ---------- *)

let () =
  let count_ = ref 40 and seed = ref 20260927 and from = ref 0 and fuel = ref 300000
  and capacity = ref 400 and pages = ref 1 and keep = ref "" and minimize_ = ref false
  and verbose = ref false and coverage_ = ref false and detail = ref false and minimize_limit = ref 5 in
  Arg.parse [
    "-count", Arg.Set_int count_, "N modules";
    "-seed", Arg.Set_int seed, "S seed";
    "-from", Arg.Set_int from, "I first case id";
    "-fuel", Arg.Set_int fuel, "STEPS model step budget";
    "-capacity", Arg.Set_int capacity, "DEPTH model host call depth";
    "-pages", Arg.Set_int pages, "P largest memory in pages";
    "-harness", Arg.Set_string harness, "FILE engine harness";
    "-timeout", Arg.Set_int timeout, "MS engine time limit per module";
    "-keep", Arg.Set_string keep, "DIR write disagreeing modules here";
    "-minimize", Arg.Set minimize_, " minimize disagreements";
    "-minimize-limit", Arg.Set_int minimize_limit, "N minimize at most the first N disagreements (default 5)";
    "-verbose", Arg.Set verbose, " print every case";
    "-detail", Arg.Set detail, " split the tally by engine messages and outcomes (engine-specific)";
    "-coverage", Arg.Set coverage_, " count the instructions the model executes on agreeing cases" ] (fun _ -> ()) "wasm_differential [options]";
  let dir = Filename.temp_dir "wasm-differential" "" in
  let config = { fuel = count !fuel; capacity = count !capacity; dir } in
  let tally = Hashtbl.create 32 in
  let bump key = Hashtbl.replace tally key (1 + Option.value ~default:0 (Hashtbl.find_opt tally key)) in
  let disagreements = ref [] in
  let batch = 100 in
  let last = !from + !count_ in
  let rec loop start =
    if start < last then begin
      let stop = min last (start + batch) in
      let cases = List.init (stop - start) (fun i -> make ~seed:!seed ~pages:!pages (start + i)) in
      let results = evaluate config cases start stop in
      List.iter (fun (case, seconds, verdict) ->
        bump ("kind: " ^ kind_name case.kind);
        let key = match verdict with
          | Agree s ->
            if !coverage_ && s <> "both reject" then trace ~fuel:!fuel ~capacity:config.capacity ~input:case.input case.canonical;
            "agree: " ^ s
          | Expected s -> "expected: " ^ s
          | No_verdict s -> "no verdict: " ^ s
          | Limit s -> "engine limit: " ^ (match String.index_opt s ':' with Some i -> String.sub s 0 i | None -> s)
          | Disagree s -> disagreements := (case, s) :: !disagreements; "disagree" in
        let coarse = match String.index_opt key '(' with
          | Some i when not !detail -> String.sub key 0 (i - 1) | _ -> key in
        bump coarse;
        if !verbose then Printf.printf "case %d (%s: %s): %s [model %.2fs]\n%!" case.id (kind_name case.kind)
            (kind_detail case.kind) key seconds) results;
      loop stop
    end in
  loop !from;
  Printf.printf "Differential test of the WebAssembly model against Node: %d modules, seed %d, ids %d-%d\n"
    !count_ !seed !from (last - 1);
  Printf.printf "Model fuel %d steps, host call depth %d; memories of at most %d page(s)\n" !fuel !capacity !pages;
  let keys = List.sort compare (Hashtbl.fold (fun k v acc -> (k, v) :: acc) tally []) in
  List.iter (fun (k, v) -> Printf.printf "  %-78s %6d\n" k v) keys;
  if !coverage_ then begin
    Printf.printf "Executed by the model on agreeing cases (steps):\n";
    List.iter (fun (k, v) -> Printf.printf "  %-40s %10d\n" k v)
      (List.sort compare (Hashtbl.fold (fun k v acc -> (k, v) :: acc) coverage []))
  end;
  let disagreements = List.rev !disagreements in
  Printf.printf "Disagreements: %d\n" (List.length disagreements);
  List.iteri (fun index (case, reason) ->
    Printf.printf "case %d (%s: %s, input %s): %s\n" case.id (kind_name case.kind) (kind_detail case.kind)
      (show_word case.input) reason;
    if !keep <> "" then write_file (Filename.concat !keep (Printf.sprintf "case-%d-%d.wasm" !seed case.id)) case.canonical;
    if !minimize_ && index < !minimize_limit then
      match minimize config case reason with
      | None -> ()
      | Some (m, reason) ->
        Printf.printf "  minimized: %s\n" reason;
        print_module m;
        if !keep <> "" then
          write_file (Filename.concat !keep (Printf.sprintf "case-%d-%d-min.wasm" !seed case.id)) (G.encode case.style m))
    disagreements;
  (try Sys.rmdir dir with Sys_error _ -> ())
