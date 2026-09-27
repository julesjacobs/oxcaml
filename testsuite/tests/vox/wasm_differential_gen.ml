(* Random WebAssembly modules for differential testing of the Vox WebAssembly
   model (wasm_*.ml) against an engine. The syntax and the binary encoder here
   are written independently of the model's: nothing below uses a Wasm_ module,
   so the model's decoder is tested on bytes it did not produce.

   A case is a module in the model's supported subset, generated valid by
   construction; or such a module with one mutation that usually makes it
   invalid; or a valid module encoded or extended outside the model's binary
   subset, which the model should reject; or a valid module the model decodes
   but will not instantiate (a data segment or table initializer that does not
   cover the declared memory or table). *)

type vt = I32 | I64
type rt = vt option

type instr =
  | Unreachable | Nop | Drop | Select | Return
  | Block of instr list | Loop of instr list | If of instr list * instr list
  | Br of int | Br_if of int
  | Call of int | Call_indirect of int
  | Local_get of int | Local_set of int | Local_tee of int
  | Global_get of int | Global_set of int
  | Load of vt * int * int            (* type, alignment exponent, offset *)
  | Store of vt * int * int
  | I32_const of int32
  | I64_const of int64
  | Op of int                         (* an instruction without immediates *)
  | Raw of int list                   (* bytes emitted as they are *)

type func = { result : rt; type_index : int; locals : vt list; body : instr list }
type global = { ty : vt; mutable_ : bool; init : int64 }
type module_ = {
  types : rt list;
  funcs : func list;
  table_min : int; table_max : int; elems : int list;
  mem_min : int; mem_max : int;
  globals : global list;
  run : int; tag : int; payload : int;
  data : string;
}

(* How to encode. The model reads every u32 field with a LEB128 decoder of up
   to five bytes, except a few it reads as fixed bytes; [pad] randomly pads
   only the former. The other flags leave the model's binary subset. *)
type style = {
  pad : int option;              (* seed for padding decisions; None: minimal *)
  short_consts : bool;           (* minimal-length constants (outside) *)
  grouped_locals : bool;         (* local entries with a count above one (outside) *)
  custom_section : bool;         (* a custom section (outside) *)
  memory_without_max : bool;     (* memory limits without a maximum (outside) *)
  extra_export : bool;           (* a fifth export (outside) *)
  param_type : bool;             (* an extra type with a parameter (outside) *)
  mistyped_global : int option;  (* global k initialized with the other type (invalid) *)
  export_globals : bool;         (* export every global as g<k>, and payload: the engine's view *)
}

let plain_style = { pad = None; short_consts = false; grouped_locals = false;
  custom_section = false; memory_without_max = false; extra_export = false;
  param_type = false; mistyped_global = None; export_globals = false }

(* ---------- Encoder ---------- *)

type out = { buf : Buffer.t; rng : Random.State.t option }

let byte o n = Buffer.add_char o.buf (Char.chr (n land 0xff))

(* Unsigned LEB128, minimal or (when padding) up to five bytes. *)
let u32 o n =
  let rec groups n = if n < 128 then 1 else 1 + groups (n lsr 7) in
  let minimal = groups n in
  let length = match o.rng with
    | Some r when Random.State.int r 3 = 0 -> minimal + Random.State.int r (6 - minimal)
    | _ -> minimal in
  for i = 0 to length - 1 do
    let group = (n lsr (7 * i)) land 0x7f in
    byte o (if i < length - 1 then group lor 0x80 else group)
  done

let rec sleb o v =
  let group = v land 0x7f and rest = v asr 7 in
  if (rest = 0 && group land 0x40 = 0) || (rest = -1 && group land 0x40 <> 0) then byte o group
  else (byte o (group lor 0x80); sleb o rest)

let rec sleb64 o v =
  let group = Int64.to_int (Int64.logand v 0x7fL) and rest = Int64.shift_right v 7 in
  if (rest = 0L && group land 0x40 = 0) || (rest = -1L && group land 0x40 <> 0) then byte o group
  else (byte o (group lor 0x80); sleb64 o rest)

let i32_const style o (v : int32) =
  byte o 0x41;
  if style.short_consts then sleb o (Int32.to_int v) else begin
    let v = Int32.to_int v in
    for i = 0 to 4 do
      let group = (v asr (7 * i)) land 0x7f in
      byte o (if i < 4 then group lor 0x80 else group)
    done
  end

let i64_const style o (v : int64) =
  byte o 0x42;
  if style.short_consts then sleb64 o v else
    for i = 0 to 9 do
      let group = Int64.to_int (Int64.logand (Int64.shift_right v (7 * i)) 0x7fL) in
      byte o (if i < 9 then group lor 0x80 else group)
    done

let valtype = function I32 -> 0x7f | I64 -> 0x7e

let rec instr style o = function
  | Unreachable -> byte o 0x00
  | Nop -> byte o 0x01
  | Drop -> byte o 0x1a
  | Select -> byte o 0x1b
  | Return -> byte o 0x0f
  | Block body -> byte o 0x02; byte o 0x40; instrs style o body; byte o 0x0b
  | Loop body -> byte o 0x03; byte o 0x40; instrs style o body; byte o 0x0b
  | If (yes, no) ->
    byte o 0x04; byte o 0x40; instrs style o yes;
    let omit = no = [] && (match o.rng with Some r -> Random.State.bool r | None -> true) in
    if not omit then (byte o 0x05; instrs style o no);
    byte o 0x0b
  | Br d -> byte o 0x0c; u32 o d
  | Br_if d -> byte o 0x0d; u32 o d
  | Call f -> byte o 0x10; u32 o f
  | Call_indirect t -> byte o 0x11; u32 o t; byte o 0x00
  | Local_get i -> byte o 0x20; u32 o i
  | Local_set i -> byte o 0x21; u32 o i
  | Local_tee i -> byte o 0x22; u32 o i
  | Global_get i -> byte o 0x23; u32 o i
  | Global_set i -> byte o 0x24; u32 o i
  | Load (t, a, off) -> byte o (if t = I32 then 0x28 else 0x29); u32 o a; u32 o off
  | Store (t, a, off) -> byte o (if t = I32 then 0x36 else 0x37); u32 o a; u32 o off
  | I32_const v -> i32_const style o v
  | I64_const v -> i64_const style o v
  | Op n -> byte o n
  | Raw l -> List.iter (byte o) l
and instrs style o l = List.iter (instr style o) l

let section style id b fill =
  let o = { buf = Buffer.create 256;
    rng = Option.map (fun s -> Random.State.make [| s; id; 1 |]) style.pad } in
  fill o;
  let frame = { buf = b; rng = Option.map (fun s -> Random.State.make [| s; id; 2 |]) style.pad } in
  byte frame id; u32 frame (Buffer.length o.buf); Buffer.add_buffer b o.buf

let name o s = u32 { o with rng = None } (String.length s); String.iter (fun c -> byte o (Char.code c)) s

let rec group_locals = function
  | [] -> []
  | t :: rest ->
    (match group_locals rest with
     | (n, t') :: groups when t' = t -> (n + 1, t) :: groups
     | groups -> (1, t) :: groups)

let encode style m =
  let b = Buffer.create 70000 in
  Buffer.add_string b "\000asm\001\000\000\000";
  if style.custom_section then
    section style 0 b (fun o -> name o "vox"; Buffer.add_string o.buf "differential");
  section style 1 b (fun o ->
    let extra = if style.param_type then 1 else 0 in
    u32 o (List.length m.types + extra);
    List.iter (fun r -> byte o 0x60; byte o 0x00;
      match r with None -> byte o 0x00 | Some t -> byte o 0x01; byte o (valtype t)) m.types;
    if style.param_type then (byte o 0x60; byte o 0x01; byte o 0x7f; byte o 0x00));
  section style 3 b (fun o ->
    u32 o (List.length m.funcs); List.iter (fun f -> u32 o f.type_index) m.funcs);
  section style 4 b (fun o ->
    u32 o 1; byte o 0x70; byte o 0x01; u32 o m.table_min; u32 o m.table_max);
  section style 5 b (fun o ->
    u32 o 1;
    if style.memory_without_max then (byte o 0x00; u32 o m.mem_min)
    else (byte o 0x01; u32 o m.mem_min; u32 o m.mem_max));
  section style 6 b (fun o ->
    u32 o (List.length m.globals);
    List.iteri (fun k g ->
      byte o (valtype g.ty); byte o (if g.mutable_ then 1 else 0);
      let ty = if style.mistyped_global = Some k then (if g.ty = I32 then I64 else I32) else g.ty in
      (match ty with
       | I32 -> i32_const style o (Int64.to_int32 g.init)
       | I64 -> i64_const style o g.init);
      byte o 0x0b) m.globals);
  section style 7 b (fun o ->
    let exports =
      [ ("run", 0, m.run); ("memory", 2, 0) ] @
      (if style.export_globals then
         List.mapi (fun k _ -> (Printf.sprintf "g%d" k, 3, k)) m.globals @ [ ("payload", 3, m.payload) ]
       else [ ("tag", 3, m.tag); ("payload", 3, m.payload) ]) @
      (if style.extra_export then [ ("extra", 0, m.run) ] else []) in
    u32 o (List.length exports);
    List.iter (fun (n, kind, index) -> name o n; byte o kind; u32 o index) exports);
  section style 9 b (fun o ->
    u32 o 1; u32 o 0; i32_const style o 0l; byte o 0x0b;
    u32 o (List.length m.elems); List.iter (u32 o) m.elems);
  section style 10 b (fun o ->
    u32 o (List.length m.funcs);
    List.iter (fun f ->
      let body = { buf = Buffer.create 256; rng = o.rng } in
      let entries = if style.grouped_locals then group_locals f.locals
        else List.map (fun t -> (1, t)) f.locals in
      u32 body (List.length entries);
      List.iter (fun (n, t) -> u32 { body with rng = None } n; byte body (valtype t)) entries;
      instrs style body f.body; byte body 0x0b;
      u32 o (Buffer.length body.buf); Buffer.add_buffer o.buf body.buf) m.funcs);
  section style 11 b (fun o ->
    u32 o 1; u32 o 0; i32_const style o 0l; byte o 0x0b;
    u32 o (String.length m.data); Buffer.add_string o.buf m.data);
  Buffer.contents b

(* ---------- Generator ---------- *)

let rint r n = if n <= 0 then 0 else Random.State.int r n
let chance r p = Random.State.float r 1.0 < p
let pick r l = List.nth l (rint r (List.length l))
let indices p a = List.filter (fun i -> p a.(i)) (List.init (Array.length a) Fun.id)

type env = {
  r : Random.State.t;
  types : rt array;
  results : rt array;          (* result type of each function *)
  globals : global array;
  fuel : int option;           (* the global that bounds loop iterations and calls *)
  mem : int;                   (* memory size in bytes *)
  table : int;
  unsupported : bool;          (* use instructions the model decodes but does not run *)
  mutable budget : int;
}
type fenv = { e : env; locals : vt array; result : rt }

let i32_value r = match rint r 10 with
  | 0 | 1 | 2 -> Int32.of_int (rint r 10)
  | 3 | 4 -> pick r [0l; 1l; -1l; Int32.max_int; Int32.min_int; 0x7fffl; 0x8000l; 0xffffl; 0x10000l; 0xfffffff8l]
  | 5 -> Int32.of_int (rint r 70000)
  | _ -> Random.State.bits32 r
let i64_value r = match rint r 10 with
  | 0 | 1 | 2 -> Int64.of_int (rint r 10)
  | 3 | 4 -> pick r [0L; 1L; -1L; Int64.max_int; Int64.min_int; 0xffffffffL; 0x100000000L; 0x7fffffffL; 0x80000000L]
  | _ -> Random.State.bits64 r
let const r = function I32 -> I32_const (i32_value r) | I64 -> I64_const (i64_value r)
let any_type r = if Random.State.bool r then I32 else I64

let i32_binary = [0x46; 0x47; 0x49; 0x4b; 0x4d; 0x4f; 0x6a; 0x6b]  (* eq ne lt_u gt_u le_u ge_u add sub *)
let i32_unrun = [0x6c; 0x71; 0x72]                                  (* mul and or *)
let i64_binary = [0x7c; 0x7d]                                        (* add sub *)
let i64_unrun = [0x83; 0x84; 0x86; 0x88]                             (* and or shl shr_u *)
let i64_compare = [0x51; 0x54]                                       (* eq lt_u *)

let default = function None -> [] | Some t -> [const (Random.State.make [| 0 |]) t]
let decrement f = [Global_get f; I32_const 1l; Op 0x6b; Global_set f]

let rec leaf fe t =
  let r = fe.e.r in
  let locals = indices (fun v -> v = t) fe.locals in
  let globals = indices (fun g -> g.ty = t) fe.e.globals in
  match rint r 4 with
  | 0 when locals <> [] -> [Local_get (pick r locals)]
  | 1 when globals <> [] -> [Global_get (pick r globals)]
  | _ -> [const r t]

and address fe n d width =
  let r = fe.e.r and mem = fe.e.mem in
  match rint r 12 with
  | 0 | 1 | 2 | 3 | 4 | 5 -> [I32_const (Int32.of_int (rint r 300))]
  | 6 | 7 -> [I32_const (Int32.of_int (max 0 (mem - width - rint r 12 + 4)))]
  | 8 -> [I32_const (Int32.of_int (rint r (max 1 mem)))]
  | 9 -> [I32_const (i32_value r)]
  | _ -> expr fe n I32 (d - 1)

and memarg r t =
  let natural = if t = I32 then 2 else 3 in
  let offset = match rint r 12 with
    | 0 | 1 | 2 | 3 | 4 | 5 | 6 -> 0
    | 7 | 8 | 9 -> rint r 64
    | _ -> pick r [65536; 0xffffffff; 0x7fffffff; rint r 70000; 65535] in
  (rint r (natural + 1), offset)

and expr fe n t d : instr list =
  let e = fe.e in
  let r = e.r in
  e.budget <- e.budget - 1;
  if d <= 0 || e.budget <= 0 then leaf fe t else
  match rint r 18, t with
  | (0 | 1 | 2), _ -> leaf fe t
  | 3, I32 -> (match rint r 3 with
      | 0 -> expr fe n I32 (d - 1) @ [Op 0x45]
      | 1 -> expr fe n I64 (d - 1) @ [Op 0x50]
      | _ -> expr fe n I64 (d - 1) @ [Op 0xa7])
  | 3, I64 -> expr fe n I32 (d - 1) @ [Op 0xad]
  | (4 | 5 | 6), I32 ->
    if chance r 0.2 then expr fe n I64 (d - 1) @ expr fe n I64 (d - 1) @ [Op (pick r i64_compare)]
    else
      let ops = if e.unsupported && chance r 0.3 then i32_unrun else i32_binary in
      let left = expr fe n I32 (d - 1) in
      let middle = if chance r 0.1 then statement fe n (d - 1) else [] in
      left @ middle @ expr fe n I32 (d - 1) @ [Op (pick r ops)]
  | (4 | 5 | 6), I64 ->
    let ops = if e.unsupported && chance r 0.3 then i64_unrun else i64_binary in
    let left = expr fe n I64 (d - 1) in
    let middle = if chance r 0.1 then statement fe n (d - 1) else [] in
    left @ middle @ expr fe n I64 (d - 1) @ [Op (pick r ops)]
  | 7, _ ->
    let a = expr fe n t (d - 1) in let b = expr fe n t (d - 1) in
    a @ b @ expr fe n I32 (d - 1) @ [Select]
  | (8 | 9), _ ->
    let (align, offset) = memarg r t in
    address fe n d (if t = I32 then 4 else 8) @ [Load (t, align, offset)]
  | 10, _ ->
    (match indices (fun x -> x = Some t) e.results with
     | [] -> leaf fe t
     | fs -> [Call (pick r fs)])
  | 11, _ ->
    (match indices (fun x -> x = Some t) e.types with
     | [] -> leaf fe t
     | ts -> table_index fe n d @ [Call_indirect (pick r ts)])
  | 12, _ ->
    (match indices (fun v -> v = t) fe.locals with
     | [] -> leaf fe t
     | ls -> expr fe n t (d - 1) @ [Local_tee (pick r ls)])
  | 13, _ -> let s = statement fe n (d - 1) in s @ expr fe n t (d - 1)
  | _ -> leaf fe t

and table_index fe n d =
  let r = fe.e.r in
  match rint r 10 with
  | 0 | 1 | 2 | 3 | 4 | 5 | 6 -> [I32_const (Int32.of_int (rint r (max 1 fe.e.table)))]
  | 7 -> [I32_const (Int32.of_int (fe.e.table + rint r 3))]
  | 8 -> [I32_const (i32_value r)]
  | _ -> expr fe n I32 (d - 1)

(* A statement leaves the operand stack as it found it and does not end the
   enclosing sequence, though it may contain branches out of it. *)
and statement fe n d : instr list =
  let e = fe.e in
  let r = e.r in
  e.budget <- e.budget - 1;
  if d <= 0 || e.budget <= 0 then [Nop] else
  match rint r 16 with
  | 0 | 1 -> let t = any_type r in expr fe n t (d - 1) @ [Drop]
  | 2 | 3 ->
    if Array.length fe.locals = 0 then [Nop] else
      let i = rint r (Array.length fe.locals) in expr fe n fe.locals.(i) (d - 1) @ [Local_set i]
  | 4 ->
    (match indices (fun g -> g.mutable_) e.globals |> List.filter (fun g -> Some g <> e.fuel) with
     | [] -> [Nop]
     | gs -> let g = pick r gs in expr fe n e.globals.(g).ty (d - 1) @ [Global_set g])
  | 5 | 6 ->
    let t = any_type r in
    let (align, offset) = memarg r t in
    let a = address fe n d (if t = I32 then 4 else 8) in
    a @ expr fe n t (d - 1) @ [Store (t, align, offset)]
  | 7 -> [Block (sequence fe (n + 1) (d - 1))]
  | 8 -> loop fe n d
  | 9 ->
    let c = expr fe n I32 (d - 1) in
    let yes = sequence fe (n + 1) (d - 1) in
    let no = if chance r 0.5 then sequence fe (n + 1) (d - 1) else [] in
    c @ [If (yes, no)]
  | 10 ->
    (match indices (fun x -> x = None) e.results with
     | [] -> [Nop]
     | fs -> [Call (pick r fs)])
  | 11 ->
    (match indices (fun x -> x = None) e.types with
     | [] -> [Nop]
     | ts -> table_index fe n d @ [Call_indirect (pick r ts)])
  | 12 | 13 ->
    let l = rint r (n + 1) in
    let cond = expr fe n I32 (d - 1) in
    if l = n then (match fe.result with
      | None -> cond @ [Br_if l]
      | Some t -> let v = expr fe n t (d - 1) in v @ cond @ [Br_if l; Drop])
    else cond @ [Br_if l]
  | 14 -> [Nop]
  | _ -> let t = any_type r in expr fe n t (d - 1) @ [Drop]

and loop fe n d =
  let r = fe.e.r in
  match fe.e.fuel with
  | Some f ->
    let body = sequence fe (n + 2) (d - 1) in
    let back = if chance r 0.6 then expr fe (n + 2) I32 (d - 1) @ [Br_if 0] else [] in
    [Block [Loop ([Global_get f; Op 0x45; Br_if 1] @ decrement f @ body @ back)]]
  | None ->
    let body = sequence fe (n + 1) (d - 1) in
    let back = if chance r 0.3 then expr fe (n + 1) I32 (d - 1) @ [Br_if 0] else [] in
    [Loop (body @ back)]

(* Code valid after an unconditional branch, where the stack is polymorphic. *)
and dead r = pick r [
  []; []; [Drop]; [Op 0x6a; Drop]; [Select; Drop]; [Op 0x50; Br_if 0; Drop];
  [I32_const 1l; Op 0x6a; Drop]; [Op 0x7c; Op 0x50; Drop]; [Load (I32, 2, 0); Drop];
  [Nop; Drop; Drop]; [Op 0xa7; Op 0x45; Drop]; [Br 0]; [Unreachable]; [Return];
  [I64_const 1L; Select; Op 0x50; Drop]; [I32_const 1l; Select; Op 0x45; Drop];
  [Select; Op 0x45; Drop]; [Block [I32_const 1l; Drop]]; [Op 0x45; Op 0x45; Drop];
  [I32_const 0l; Br_if 0; Drop]; [Op 0x7d; I64_const 3L; Op 0x51; Drop] ]

and terminator fe n d =
  let r = fe.e.r in
  let value () = match fe.result with None -> [] | Some t -> expr fe n t (d - 1) in
  let junk () = if chance r 0.3 then expr fe n (any_type r) (d - 1) else [] in
  let code = match rint r 10 with
    | 0 | 1 | 2 | 3 | 4 ->
      let l = rint r (n + 1) in
      let j = junk () in
      if l = n then j @ value () @ [Br l] else j @ [Br l]
    | 5 | 6 | 7 | 8 -> let j = junk () in j @ value () @ [Return]
    | _ -> [Unreachable] in
  code @ (if chance r 0.3 then dead r else [])

(* A sequence of statements; it may end with a branch, a return or a trap. *)
and sequence fe n d =
  let r = fe.e.r in
  let length = rint r 5 in
  let rec go k =
    if k = 0 || fe.e.budget <= 0 then []
    else if chance r 0.08 then terminator fe n d
    else let s = statement fe n d in s @ go (k - 1) in
  go length

(* Code after unreachable that misuses the polymorphic stack. *)
let dead_type_error r = Unreachable :: pick r [
  [I64_const 1L; Select; Op 0x45; Drop]; [I32_const 1l; I64_const 1L; Select; Drop];
  [I32_const 1l; Op 0x50; Drop]; [I32_const 1l; Op 0x45]; [I64_const 1L; Op 0x6a; Drop];
  [I32_const 1l; I64_const 2L; I32_const 0l; Select; Drop]; [Select; Op 0x45; Op 0x50; Drop] ]

let rec terminated = function
  | [] -> false
  | [ (Br _ | Return | Unreachable) ] -> true
  | _ :: rest -> terminated rest

let body fe =
  let e = fe.e in
  let r = e.r in
  let guard = match e.fuel with
    | None -> []
    | Some f -> [Global_get f; Op 0x45; If (default fe.result @ [Return], [])] @ decrement f in
  let rec statements k acc = if k = 0 || e.budget <= 0 then acc else
      statements (k - 1) (acc @ (if chance r 0.05 then terminator fe 0 4 else statement fe 0 4)) in
  let code = statements (1 + rint r 8) [] in
  (* The function's result: after a terminator no value is needed, but a
     fall-through end needs exactly one. *)
  let finish = match fe.result with None -> [] | Some t -> expr fe 0 t 4 in
  guard @ code @ (if chance r 0.1 && terminated code then [] else finish)

let data r size =
  let b = Bytes.make size '\000' in
  if size > 0 then begin
    for i = 0 to min size 320 - 1 do if chance r 0.7 then Bytes.set b i (Char.chr (rint r 256)) done;
    for i = max 0 (size - 64) to size - 1 do Bytes.set b i (Char.chr (rint r 256)) done;
    for _ = 1 to 16 do Bytes.set b (rint r size) (Char.chr (rint r 256)) done
  end;
  Bytes.to_string b

(* Settings for one module. *)
type settings = { guarded : bool; unsupported : bool; max_pages : int }

let generate r s =
  let nfuncs = 1 + rint r 5 in
  let results = Array.init nfuncs (fun _ -> match rint r 3 with 0 -> None | 1 -> Some I32 | _ -> Some I64) in
  let used = List.sort_uniq compare (Array.to_list results) in
  let extra = List.init (rint r 3) (fun _ -> match rint r 3 with 0 -> None | 1 -> Some I32 | _ -> Some I64) in
  let types = Array.of_list (List.map snd (List.sort compare
    (List.map (fun t -> (Random.State.bits r, t)) (used @ extra)))) in
  let nglobals = rint r 5 in
  let random_global () = let ty = any_type r in
    { ty; mutable_ = chance r 0.7;
      init = (match ty with I32 -> Int64.logand (Int64.of_int32 (i32_value r)) 0xffffffffL | I64 -> i64_value r) } in
  let globals = List.init nglobals (fun _ -> random_global ()) in
  let fuel, globals =
    if s.guarded then
      let k = rint r (nglobals + 1) in
      let g = { ty = I32; mutable_ = true; init = Int64.of_int (1 + rint r 300) } in
      Some k, List.filteri (fun i _ -> i < k) globals @ [g] @ List.filteri (fun i _ -> i >= k) globals
    else if globals = [] then None, [random_global ()] else None, globals in
  let globals = Array.of_list globals in
  let pages = match rint r 10 with 0 -> 0 | 1 when s.max_pages >= 2 -> 2 | _ -> min 1 s.max_pages in
  let table = match rint r 6 with 0 -> 0 | _ -> 1 + rint r 4 in
  let e = { r; types; results; globals; fuel; mem = pages * 65536; table;
            unsupported = s.unsupported; budget = 0 } in
  let funcs = Array.to_list (Array.mapi (fun _ result ->
    let locals = List.init (rint r 6) (fun _ -> any_type r) in
    let fe = { e; locals = Array.of_list locals; result } in
    e.budget <- 20 + rint r 120;
    let code = body fe in
    let type_index = pick r (indices (fun x -> x = result) types) in
    { result; type_index; locals; body = code }) results) in
  let mem_max = match rint r 8 with 0 -> 65536 | 1 -> pages | _ -> pages + rint r 4 in
  let m = { types = Array.to_list types; funcs;
    table_min = table; table_max = table + (if chance r 0.5 then 0 else rint r 10);
    elems = List.init table (fun _ -> rint r nfuncs);
    mem_min = pages; mem_max; globals = Array.to_list globals;
    run = (if chance r 0.6 then 0 else rint r nfuncs);
    tag = rint r (Array.length globals); payload = rint r (Array.length globals);
    data = data r (pages * 65536) } in
  (* The host sets payload to the input, so it must be a mutable i64 global:
     keep the one drawn if it is, else take the first such global, else add
     one. This draws nothing, so the rest of the module is as drawn. *)
  let settable g = g.ty = I64 && g.mutable_ in
  if settable globals.(m.payload) then m else
  match List.find_index settable m.globals with
  | Some k -> { m with payload = k }
  | None -> { m with payload = Array.length globals;
                     globals = m.globals @ [ { ty = I64; mutable_ = true; init = 0L } ] }

(* ---------- Mutations ---------- *)

(* Replace the k-th instruction satisfying [p], counted over nested bodies, by
   [f] of it, for a random k. *)
let rewrite r p f code =
  let count = ref 0 in
  let rec scan l = List.iter (fun i -> if p i then incr count;
    match i with Block b | Loop b -> scan b | If (y, n) -> scan y; scan n | _ -> ()) l in
  scan code;
  if !count = 0 then None else begin
    let target = rint r !count and k = ref 0 in
    let rec go l = List.concat_map (fun i ->
      let i' = match i with
        | Block b -> Block (go b) | Loop b -> Loop (go b)
        | If (y, n) -> let y = go y in If (y, go n)
        | i -> i in
      if p i then (let hit = !k = target in incr k; if hit then f i' else [i']) else [i']) l in
    Some (go code)
  end

let in_function r m p f =
  let order = List.sort compare (List.mapi (fun i _ -> (Random.State.bits r, i)) m.funcs) in
  let rec attempt = function
    | [] -> None
    | (_, k) :: rest ->
      let fn = List.nth m.funcs k in
      match rewrite r p (f fn) fn.body with
      | None -> attempt rest
      | Some body -> Some { m with funcs = List.mapi (fun i g -> if i = k then { g with body } else g) m.funcs } in
  attempt order

let any _ = true
let all_ops = i32_binary @ i32_unrun @ i64_binary @ i64_unrun @ i64_compare @ [0x45; 0x50; 0xa7; 0xad]

(* A mutation of the module's syntax; most make it invalid, not all. *)
let mutate r m : (string * module_ * style) option =
  let nf = List.length m.funcs and nt = List.length m.types and ng = List.length m.globals in
  let s = plain_style in
  let module_ name m = Option.map (fun m -> (name, m, s)) m in
  match rint r 28 with
  | 22 -> module_ "type error in unreachable code" (in_function r m any (fun _ i -> i :: dead_type_error r))
  | 23 -> module_ "valid code after unreachable" (in_function r m any (fun _ i -> i :: Unreachable :: dead r))
  | 24 -> module_ "malformed integer encoding" (in_function r m any (fun _ i -> [i; Raw (pick r [
      [0x20; 0x80; 0x80; 0x80; 0x80; 0x10 + rint r 0x70];                (* u32 fifth byte too large *)
      [0x20; 0x80; 0x80; 0x80; 0x80; 0x80; 0x00];                        (* u32 of six bytes *)
      [0x41; 0x80; 0x80; 0x80; 0x80; 8 + rint r 112];                    (* i32 last byte not a sign extension *)
      [0x42; 0x80; 0x80; 0x80; 0x80; 0x80; 0x80; 0x80; 0x80; 0x80; 1 + rint r 126];
      [0x41; 0x80; 0x80; 0x80; 0x80; 0x80; 0x00];                        (* i32 of six bytes *)
      [0x28; 0x02; 0x80; 0x80; 0x80; 0x80; 0x10 + rint r 0x70] ]); Drop]))  (* offset fifth byte too large *)
  | 25 -> module_ "index near 2^32" (in_function r m any (fun _ i -> [i; pick r
      [Local_get 0xffffffff; Global_get 0xfffffffe; Call 0xffffffff; Global_set 0xffffffff; Local_set 0xfffffff0]]))
  | 26 -> Some ("table above the engine's size limit", { m with table_min = 10_000_001; table_max = 10_000_001 + rint r 3 }, s)
  | 27 -> Some ("more locals than the engine allows", { m with funcs = List.mapi (fun i (f : func) ->
      if i = 0 then { f with locals = f.locals @ List.init 50_001 (fun _ -> I32) } else f) m.funcs }, s)
  | 0 -> module_ "operator replaced" (in_function r m (function Op _ -> true | _ -> false)
      (fun _ _ -> [Op (pick r all_ops)]))
  | 1 -> module_ "local index out of range" (in_function r m (function Local_get _ | Local_set _ | Local_tee _ -> true | _ -> false)
      (fun fn i -> let bad = List.length fn.locals + rint r 2 in
        [match i with Local_get _ -> Local_get bad | Local_set _ -> Local_set bad | _ -> Local_tee bad]))
  | 2 -> module_ "global.set of an immutable or missing global" (in_function r m (function Global_set _ | Global_get _ -> true | _ -> false)
      (fun _ i ->
        let immutable = List.filter (fun k -> not (List.nth m.globals k).mutable_) (List.init ng Fun.id) in
        let g = if immutable <> [] && chance r 0.7 then pick r immutable else ng + rint r 2 in
        match i with Global_get _ -> [Global_get 0; Global_set g] | _ -> [Global_set g]))
  | 3 -> module_ "branch depth increased" (in_function r m (function Br _ | Br_if _ -> true | _ -> false)
      (fun _ i -> [match i with Br d -> Br (d + 1 + rint r 2) | Br_if d -> Br_if (d + 1 + rint r 2) | i -> i]))
  | 4 -> module_ "drop removed" (in_function r m (function Drop -> true | _ -> false) (fun _ _ -> []))
  | 5 -> module_ "extra value" (in_function r m any (fun _ i -> [i; const r (any_type r)]))
  | 6 -> module_ "call index out of range" (in_function r m any (fun _ i -> [i; Call (nf + rint r 2)]))
  | 7 -> module_ "call_indirect type out of range" (in_function r m any
      (fun _ i -> [i; I32_const 0l; Call_indirect (nt + rint r 2)]))
  | 8 -> module_ "alignment above natural" (in_function r m (function Load _ | Store _ -> true | _ -> false)
      (fun _ i -> [match i with
        | Load (t, _, o) -> Load (t, (if t = I32 then 3 else 4) + rint r 2, o)
        | Store (t, _, o) -> Store (t, (if t = I32 then 3 else 4) + rint r 2, o)
        | i -> i]))
  | 9 -> module_ "select of mixed types" (in_function r m any
      (fun _ i -> [i; I32_const 1l; I64_const 2L; I32_const 1l; Select; Drop]))
  | 10 -> module_ "if without condition" (in_function r m any (fun _ i -> [i; If ([], [])]))
  | 11 -> module_ "stray else" (in_function r m any (fun _ i -> [i; Raw [0x05]]))
  | 12 -> module_ "instruction removed" (in_function r m any (fun _ _ -> []))
  | 13 -> Some ("memory maximum above 65536", { m with mem_max = 65537 + rint r 3 }, s)
  | 14 -> Some ("memory minimum above maximum", { m with mem_max = m.mem_min - 1 }, s) |> Option.map (fun x -> if m.mem_min = 0 then ("memory minimum above maximum", { m with mem_min = 1; mem_max = 0; data = String.make 65536 '\000' }, s) else x)
  | 15 -> Some ("table minimum above maximum", { m with table_max = m.table_min - 1 }, s) |> Option.map (fun x -> if m.table_min = 0 then ("table minimum above maximum", { m with table_min = 1; table_max = 0; elems = [0] }, s) else x)
  | 16 -> Some ("element index out of range", { m with elems = (nf + rint r 2) :: (match m.elems with [] -> [] | _ :: t -> t); table_min = max 1 m.table_min; table_max = max m.table_max (max 1 m.table_min) }, s)
  | 17 -> Some ("run export out of range", { m with run = nf + rint r 2 }, s)
  | 18 -> Some ("global export out of range", (if chance r 0.5 then { m with tag = ng + rint r 2 } else { m with payload = ng + rint r 2 }), s)
  | 19 -> Some ("type index out of range", { m with funcs = List.mapi (fun i f -> if i = 0 then { f with type_index = nt + rint r 2 } else f) m.funcs }, s)
  | 20 -> Some ("global initializer of the other type", m, { s with mistyped_global = Some (rint r ng) })
  | _ -> module_ "function result changed" (Some { m with funcs = List.mapi (fun i (f : func) ->
      if i = 0 then begin
        let result = match f.result with None -> Some I32 | Some I32 -> Some I64 | Some I64 -> None in
        match List.filter (fun k -> List.nth m.types k = result) (List.init nt Fun.id) with
        | [] -> f
        | ks -> { f with result; type_index = pick r ks }
      end else f) m.funcs })

(* Byte-level corruption, outside the data segment payload. *)
(* The section containing byte i, and the offset of i in its payload. *)
let locate (bytes : string) i =
  let rec leb p shift acc =
    let c = Char.code bytes.[p] in
    let acc = acc lor ((c land 0x7f) lsl shift) in
    if c land 0x80 = 0 then (acc, p + 1) else leb (p + 1) (shift + 7) acc in
  let rec go p =
    if p >= String.length bytes then "past the end" else
    let id = Char.code bytes.[p] in
    let (size, start) = leb (p + 1) 0 0 in
    if i < start then Printf.sprintf "header of section %d" id
    else if i < start + size then Printf.sprintf "section %d, payload byte %d" id (i - start)
    else go (start + size) in
  if i < 8 then "module header" else go 8

let corrupt r (bytes : string) (data_length : int) =
  let prefix = String.length bytes - data_length in
  let b = Bytes.of_string bytes in
  match rint r 3 with
  | 0 -> let n = 8 + rint r (prefix - 8) in
    (Printf.sprintf "bytes truncated at %d (%s)" n (locate bytes n), String.sub bytes 0 n)
  | 1 ->
    let i = 8 + rint r (prefix - 8) in
    let before = Char.code (Bytes.get b i) in
    let after = before lxor (1 lsl rint r 8) in
    Bytes.set b i (Char.chr after);
    (Printf.sprintf "byte %d flipped from 0x%02x to 0x%02x (%s)" i before after (locate bytes i), Bytes.to_string b)
  | _ ->
    let i = 8 + rint r (prefix - 8) in
    let c = rint r 256 in
    (Printf.sprintf "byte 0x%02x inserted at %d (%s)" c i (locate bytes i),
     String.sub bytes 0 i ^ String.make 1 (Char.chr c) ^ String.sub bytes i (String.length bytes - i))

(* A valid module outside the model's binary subset, or which the model
   decodes but does not instantiate. *)
let outside r m : string * module_ * style * bool =
  let s = plain_style in
  let insert statement = match in_function r m any (fun _ i -> [i] @ statement) with
    | Some m -> m
    | None -> { m with funcs = List.mapi (fun i f -> if i = 0 then { f with body = statement @ f.body } else f) m.funcs } in
  match rint r 12 with
  | 0 -> ("minimal-length constants", m, { s with short_consts = true }, false)
  | 1 -> ("grouped local declarations",
      { m with funcs = List.mapi (fun i (f : func) -> if i = 0 then { f with locals = f.locals @ [I32; I32] } else f) m.funcs },
      { s with grouped_locals = true }, false)
  | 2 -> ("custom section", m, { s with custom_section = true }, false)
  | 3 -> ("memory without maximum", m, { s with memory_without_max = true }, false)
  | 4 -> ("fifth export", m, { s with extra_export = true }, false)
  | 5 -> ("type with a parameter", m, { s with param_type = true }, false)
  | 6 -> ("block with a result type",
      insert [Raw [0x02; 0x7f]; I32_const 5l; Raw [0x0b]; Drop], s, false)
  | 7 -> ("instruction outside the subset",
      insert (pick r [
        [I32_const 3l; I32_const 5l; Op 0x73; Drop];              (* i32.xor *)
        [Raw [0x3f; 0x00]; Drop];                                 (* memory.size *)
        [I64_const 3L; I64_const 5L; Op 0x7e; Drop];              (* i64.mul *)
        [I32_const 3l; I32_const 5l; Op 0x48; Drop];              (* i32.lt_s *)
        [I32_const 0l; Raw [0x2d; 0x00; 0x00]; Drop];             (* i32.load8_u *)
        [I64_const 3L; I64_const 5L; Op 0x52; Drop];              (* i64.ne *)
        [I32_const 1l; I32_const 2l; I32_const 0l; Raw [0x1c; 0x01; 0x7f]; Drop] ]), s, false) (* typed select *)
  | 8 when m.mem_min > 0 ->
    ("data segment shorter than memory", { m with data = String.sub m.data 0 (rint r (String.length m.data)) }, s, true)
  | 9 when m.table_min > 0 ->
    ("table initializer shorter than table", { m with elems = List.tl m.elems }, s, true)
  | 10 -> ("data segment longer than memory", { m with data = m.data ^ String.make (1 + rint r 16) '\001' }, s, true)
  | _ -> ("minimal-length constants", m, { s with short_consts = true }, false)
