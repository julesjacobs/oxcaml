module B = Wasm_u32
module C = Wasm_code
module I = Wasm_instruction
module S = Wasm_scalar
module L = Wasm_locals
module E = Wasm_execution
module M = Wasm_memory
module X = Wasm_memory_execution
module W = Wasm_frame_write
module Lower = Wasm_memory_lowering

module Prefix = Wasm_instruction_prefix

let[@def] rec (partial @ total) (writes : W.writes @ immutable) (before : B.bytes @ immutable)
    (base : B.u32) (locals : S.stack @ immutable) (current : B.bytes @ immutable) = ghost_ (
  current === before || match writes with
  | W.End -> false
  | W.Write (offset, source, rest) -> match L.get locals source with
    | Some (S.I64 word) -> (match M.store before base offset (S.I64 word) with
      | None -> false | Some next -> partial rest next base locals current)
    | _ -> false)

let rec (prefix @ total) : (writes : W.writes) @ immutable -> (base_local : B.u32) ->
    (state : X.state) @ immutable -> (base : B.u32) -> (expected : B.bytes) @ immutable ->
    (fuel : C.count) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && W.apply writes state.X.memory base state.X.machine.E.locals === Some expected} ->
    {u : unit | match X.run (Prefix.take fuel (W.emit writes base_local)) state with
      | X.Done after -> after.X.machine.E.locals === state.X.machine.E.locals
        && partial writes state.X.memory base state.X.machine.E.locals after.X.memory
      | _ -> false} @ ghost = fun writes base_local state base expected fuel premise -> ghost_ (
    W.emit_def writes base_local; W.apply_def writes state.X.memory base state.X.machine.E.locals;
    partial_def writes state.X.memory base state.X.machine.E.locals state.X.memory;
    match writes with
    | W.End -> Prefix.take_def fuel C.Empty; X.run_def C.Empty state
    | W.Write (offset, source, rest) ->
      let locals = state.X.machine.E.locals in let stack = state.X.machine.E.stack in
      let tail = W.emit rest base_local in
      Lower.write_code_def M.W64 offset base_local source; Lower.store_instruction_def M.W64 offset;
      E.append_def (Lower.write_code M.W64 offset base_local source) tail;
      E.append_def (C.Next (I.Local_get source, C.Next (I.I64_store (3, offset), C.Empty))) tail;
      E.append_def (C.Next (I.I64_store (3, offset), C.Empty)) tail;
      E.append_def C.Empty tail;
      Prefix.take_def fuel (W.emit writes base_local);
      match fuel with
      | C.Zero -> X.run_def C.Empty state
      | C.Succ remaining ->
        let addressed = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (S.I32 base, stack)}} in
        X.run_def (Prefix.take fuel (W.emit writes base_local)) state;
        X.step_def (I.Local_get base_local) state; E.step_def (I.Local_get base_local) state.X.machine;
        Prefix.take_def remaining (C.Next (I.Local_get source, C.Next (I.I64_store (3, offset), tail)));
        match remaining with
        | C.Zero -> X.run_def C.Empty addressed
        | C.Succ remaining -> match L.get locals source with
          | Some (S.I64 word) ->
            let operands = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (S.I64 word, addressed.X.machine.E.stack)}} in
            X.run_def (Prefix.take (C.Succ remaining) (C.Next (I.Local_get source, C.Next (I.I64_store (3, offset), tail)))) addressed;
            X.step_def (I.Local_get source) addressed; E.step_def (I.Local_get source) addressed.X.machine;
            Prefix.take_def remaining (C.Next (I.I64_store (3, offset), tail));
            (match remaining with
            | C.Zero -> X.run_def C.Empty operands
            | C.Succ remaining ->
              X.run_def (Prefix.take (C.Succ remaining) (C.Next (I.I64_store (3, offset), tail))) operands;
              X.step_def (I.I64_store (3, offset)) operands;
              X.write_def M.W64 offset operands; X.compatible_def (S.I64 word) M.W64;
              M.width_def (S.I64 word);
              match M.store state.X.memory base offset (S.I64 word) with
              | None -> ()
              | Some memory ->
                let next = {X.memory; machine = state.X.machine} in
                prefix rest base_local next base expected remaining ();
                match X.run (Prefix.take remaining tail) next with
                | X.Done after -> partial_def writes state.X.memory base locals after.X.memory
                | _ -> ())
          | _ -> ())

module P = Hmc_linear_preservation
module Bytes = Hmc_linear_bytes
module V = Hmc_tagged_cell
module Index = Hmc_u32_index
module Bounds = Hmc_linear_bounds
module D = Hm_declarative

let[@def] rec (bounded @ total) (writes : W.writes @ immutable) (base : B.u32)
    (low : B.u32) (high : B.u32) = ghost_ (match writes with
  | W.End -> true
  | W.Write (offset, _, rest) -> low <= base + offset && base + offset + 8 <= high
    && bounded rest base low high)

let rec (absent_suffix @ total) : (memory : B.bytes) @ immutable -> (start : B.u32) -> (stop : B.u32) ->
    {u : unit | start <= stop && Bytes.drop memory start === None} ->
    {u : unit | Bytes.drop memory stop === None} @ ghost = fun memory start stop premise -> ghost_ (
      Bytes.drop_def memory start; Bytes.drop_def memory stop;
      match memory with B.End -> () | B.Byte (_, rest) -> absent_suffix rest (start - 1) (stop - 1) ())

let rec (suffix_weaken @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (start : B.u32) -> (stop : B.u32) ->
    {u : unit | start <= stop && Bytes.drop before start === Bytes.drop after start} ->
    {u : unit | Bytes.drop before stop === Bytes.drop after stop} @ ghost =
  fun before after start stop premise -> ghost_ (
    Bytes.drop_def before start; Bytes.drop_def after start;
    Bytes.drop_def before stop; Bytes.drop_def after stop;
    if start = 0 then () else match before, after with
    | B.Byte (_, a), B.Byte (_, b) -> suffix_weaken a b (start - 1) (stop - 1) ()
    | B.End, B.Byte (_, b) -> absent_suffix b (start - 1) (stop - 1) ()
    | B.Byte (_, a), B.End -> absent_suffix a (start - 1) (stop - 1) ()
    | _ -> ())

let rec (prefix_transitive @ total) : (count : B.u32) -> (a : B.bytes) @ immutable ->
    (b : B.bytes) @ immutable -> (c : B.bytes) @ immutable ->
    {u : unit | P.equal_prefix count a b && P.equal_prefix count b c} ->
    {u : unit | P.equal_prefix count a c} @ ghost = fun count a b c premise -> ghost_ (
      P.equal_prefix_def count a b; P.equal_prefix_def count b c; P.equal_prefix_def count a c;
      if count = 0 then () else match a, b, c with
      | B.Byte (_, a), B.Byte (_, b), B.Byte (_, c) -> prefix_transitive (count - 1) a b c ()
      | _ -> ())

let (word_count @ total) : unit -> {u : unit | Index.represents (M.count M.W64) (Wasm_cell.eight ())} @ ghost =
  fun () -> ghost_ (
    Wasm_cell.eight_def (); M.count_def M.W64; V.eight_def D.Z; V.four_def D.Z; V.four_def (V.four D.Z);
    Index.represents_def (M.count M.W64) 8;
    Index.represents_def (D.S (D.S (D.S (D.S (D.S (D.S (D.S D.Z))))))) 7;
    Index.represents_def (D.S (D.S (D.S (D.S (D.S (D.S D.Z)))))) 6;
    Index.represents_def (D.S (D.S (D.S (D.S (D.S D.Z))))) 5;
    Index.represents_def (D.S (D.S (D.S (D.S D.Z)))) 4;
    Index.represents_def (D.S (D.S (D.S D.Z))) 3;
    Index.represents_def (D.S (D.S D.Z)) 2;
    Index.represents_def (D.S D.Z) 1; Index.represents_def D.Z 0)

let (store_region @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (base : B.u32) -> (offset : B.u32) -> (word : Hmc_word64.t) @ immutable ->
    (low : B.u32) -> (high : B.u32) ->
    {u : unit | low <= base + offset && base + offset + 8 <= high
      && M.store before base offset (S.I64 word) === Some after} ->
    {u : unit | P.equal_prefix low before after && Bytes.drop before high === Bytes.drop after high
      && V.length after === V.length before} @ ghost = fun before after base offset word low high premise -> ghost_ (
      M.width_def (S.I64 word); M.size_def M.W64; M.address_def base offset M.W64;
      let payload = M.encode (S.I64 word) in
      P.before_store before (base + offset) payload after ();
      P.shrink (base + offset) low before after ();
      word_count (); Wasm_cell.eight_def (); Bounds.range_def (V.length payload) (base + offset) (base + offset + 8);
      P.after_store before after (base + offset) (base + offset + 8) payload ();
      suffix_weaken before after (base + offset + 8) high ())

let rec (reflexive @ total) : (memory : B.bytes) @ immutable -> (count : B.u32) ->
    {u : unit | Bounds.covers memory count} -> {u : unit | P.equal_prefix count memory memory} @ ghost =
  fun memory count premise -> ghost_ (
    Bounds.covers_def memory count; Bytes.drop_def memory count; P.equal_prefix_def count memory memory;
    if count = 0 then () else match memory with
    | B.End -> () | B.Byte (_, rest) -> Bounds.covers_def rest (count - 1); reflexive rest (count - 1) ())

let rec (partial_region @ total) : (writes : W.writes) @ immutable ->
    (before : B.bytes) @ immutable -> (base : B.u32) -> (locals : S.stack) @ immutable ->
    (current : B.bytes) @ immutable -> (low : B.u32) -> (high : B.u32) ->
    {u : unit | bounded writes base low high && Bounds.covers before low
      && partial writes before base locals current} ->
    {u : unit | P.equal_prefix low before current && Bytes.drop before high === Bytes.drop current high
      && V.length current === V.length before} @ ghost = fun writes before base locals current low high premise -> ghost_ (
      partial_def writes before base locals current; bounded_def writes base low high;
      if current === before then reflexive before low () else match writes with
      | W.End -> ()
      | W.Write (offset, source, rest) -> match L.get locals source with
        | Some (S.I64 word) -> (match M.store before base offset (S.I64 word) with
          | None -> ()
          | Some next ->
            store_region before next base offset word low high ();
            Bounds.same_length before next low ();
            partial_region rest next base locals current low high ();
            prefix_transitive low before next current ())
        | _ -> ())

let rec (reflect @ total) : (writes : W.writes) @ immutable -> (base_local : B.u32) ->
    (state : X.state) @ immutable -> (base : B.u32) -> (after : X.state) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && X.run (W.emit writes base_local) state === X.Done after} ->
    {u : unit | W.apply writes state.X.memory base state.X.machine.E.locals === Some after.X.memory
      && after.X.machine === state.X.machine} @ ghost = fun writes base_local state base after premise -> ghost_ (
      W.emit_def writes base_local; W.apply_def writes state.X.memory base state.X.machine.E.locals;
      match writes with
      | W.End -> X.run_def C.Empty state
      | W.Write (offset, source, rest) ->
        let locals = state.X.machine.E.locals in let stack = state.X.machine.E.stack in
        let tail = W.emit rest base_local in
        Lower.write_code_def M.W64 offset base_local source; Lower.store_instruction_def M.W64 offset;
        E.append_def (Lower.write_code M.W64 offset base_local source) tail;
        E.append_def (C.Next (I.Local_get source, C.Next (I.I64_store (3, offset), C.Empty))) tail;
        E.append_def (C.Next (I.I64_store (3, offset), C.Empty)) tail; E.append_def C.Empty tail;
        X.run_def (W.emit writes base_local) state;
        X.step_def (I.Local_get base_local) state; E.step_def (I.Local_get base_local) state.X.machine;
        let addressed = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (S.I32 base, stack)}} in
        X.run_def (C.Next (I.Local_get source, C.Next (I.I64_store (3, offset), tail))) addressed;
        X.step_def (I.Local_get source) addressed; E.step_def (I.Local_get source) addressed.X.machine;
        match L.get locals source with
        | None -> ()
        | Some value ->
          let operands = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (value, addressed.X.machine.E.stack)}} in
          X.run_def (C.Next (I.I64_store (3, offset), tail)) operands;
          X.step_def (I.I64_store (3, offset)) operands; X.write_def M.W64 offset operands;
          X.compatible_def value M.W64; M.width_def value;
          match value with
          | S.I32 _ -> ()
          | S.I64 word -> match M.store state.X.memory base offset value with
            | None -> ()
            | Some memory -> reflect rest base_local {X.memory; machine = state.X.machine} base after ())

let (region @ total) : (writes : W.writes) @ immutable -> (base_local : B.u32) ->
    (state : X.state) @ immutable -> (base : B.u32) -> (after : X.state) @ immutable ->
    (low : B.u32) -> (high : B.u32) -> (fuel : C.count) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && X.run (W.emit writes base_local) state === X.Done after
      && bounded writes base low high && Bounds.covers state.X.memory low} ->
    {u : unit | match X.run (Prefix.take fuel (W.emit writes base_local)) state with
      | X.Done current -> current.X.machine.E.locals === state.X.machine.E.locals
        && partial writes state.X.memory base state.X.machine.E.locals current.X.memory
        && P.equal_prefix low state.X.memory current.X.memory
        && Bytes.drop state.X.memory high === Bytes.drop current.X.memory high
        && V.length current.X.memory === V.length state.X.memory
      | _ -> false} @ ghost = fun writes base_local state base after low high fuel premise -> ghost_ (
        reflect writes base_local state base after ();
        prefix writes base_local state base after.X.memory fuel ();
        match X.run (Prefix.take fuel (W.emit writes base_local)) state with
        | X.Done current -> partial_region writes state.X.memory base state.X.machine.E.locals current.X.memory low high ()
        | _ -> ())
