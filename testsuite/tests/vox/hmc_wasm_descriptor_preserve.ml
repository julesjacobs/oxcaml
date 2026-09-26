module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module P = Wasm_local_preservation
module Read = Wasm_limb_read
module Double = Wasm_local_double
module Advance = Hmc_wasm_allocation_advance
module Address = Hmc_wasm_descriptor_address
module Load = Hmc_wasm_descriptor_load
module Offsets = Hmc_wasm_descriptor_reads
module Select = Hmc_wasm_descriptor_select
let (limb @ total) : (offset : B.u32) -> (base : B.u32) -> (destination : B.u32) -> (local : B.u32) ->
    {u : unit | destination <> local} -> {u : unit | P.preserves (Read.emit offset base destination) local} @ ghost =
  fun offset base destination local premise -> ghost_ (
    Read.emit_def offset base destination;
    P.preserves_def C.Empty local;
    P.instruction_preserves_def (I.Local_set destination) local; P.preserves_def (C.Next (I.Local_set destination, C.Empty)) local;
    P.instruction_preserves_def (I.I32_load (2, offset)) local; P.preserves_def (C.Next (I.I32_load (2, offset), C.Next (I.Local_set destination, C.Empty))) local;
    P.instruction_preserves_def (I.Local_get base) local; P.preserves_def (C.Next (I.Local_get base, C.Next (I.I32_load (2, offset), C.Next (I.Local_set destination, C.Empty)))) local)
let (double @ total) : (source : B.u32) -> (destination : B.u32) -> (local : B.u32) ->
    {u : unit | destination <> local} -> {u : unit | P.preserves (Double.emit source destination) local} @ ghost =
  fun source destination local premise -> ghost_ (
    Double.emit_def source destination;
    P.preserves_def C.Empty local;
    P.instruction_preserves_def (I.Local_set destination) local; P.preserves_def (C.Next (I.Local_set destination, C.Empty)) local;
    P.instruction_preserves_def (I.Plain I.I32_add) local; P.preserves_def (C.Next (I.Plain I.I32_add, C.Next (I.Local_set destination, C.Empty))) local;
    P.instruction_preserves_def (I.Local_get source) local; P.preserves_def (C.Next (I.Local_get source, C.Next (I.Plain I.I32_add, C.Next (I.Local_set destination, C.Empty)))) local;
    P.instruction_preserves_def (I.Local_get source) local; P.preserves_def (C.Next (I.Local_get source, C.Next (I.Local_get source, C.Next (I.Plain I.I32_add, C.Next (I.Local_set destination, C.Empty))))) local)
let (advance @ total) : (base : B.u32) -> (destination : B.u32) -> (local : B.u32) ->
    {u : unit | destination <> local} -> {u : unit | P.preserves (Advance.emit base destination) local} @ ghost =
  fun base destination local premise -> ghost_ (
    Advance.emit_def base destination;
    P.preserves_def C.Empty local;
    P.instruction_preserves_def (I.Local_set destination) local; P.preserves_def (C.Next (I.Local_set destination, C.Empty)) local;
    P.instruction_preserves_def (I.Plain I.I32_add) local; P.preserves_def (C.Next (I.Plain I.I32_add, C.Next (I.Local_set destination, C.Empty))) local;
    P.instruction_preserves_def (I.I32_const base) local; P.preserves_def (C.Next (I.I32_const base, C.Next (I.Plain I.I32_add, C.Next (I.Local_set destination, C.Empty)))) local;
    P.instruction_preserves_def (I.Local_get destination) local; P.preserves_def (C.Next (I.Local_get destination, C.Next (I.I32_const base, C.Next (I.Plain I.I32_add, C.Next (I.Local_set destination, C.Empty))))) local)
let (address @ total) : (base : B.u32) -> (source : B.u32) -> (destination : B.u32) -> (local : B.u32) ->
    {u : unit | destination <> local} -> {u : unit | P.preserves (Address.emit base source destination) local} @ ghost =
  fun base source destination local premise -> ghost_ (
    double source destination local (); double destination destination local (); advance base destination local ();
    P.append (Double.emit destination destination) (Advance.emit base destination) local ();
    P.append (Double.emit destination destination) (E.append (Double.emit destination destination) (Advance.emit base destination)) local ();
    P.append (Double.emit destination destination) (E.append (Double.emit destination destination) (E.append (Double.emit destination destination) (Advance.emit base destination))) local ();
    P.append (Double.emit destination destination) (E.append (Double.emit destination destination) (E.append (Double.emit destination destination) (E.append (Double.emit destination destination) (Advance.emit base destination)))) local ();
    P.append (Double.emit source destination) (E.append (Double.emit destination destination) (E.append (Double.emit destination destination) (E.append (Double.emit destination destination) (E.append (Double.emit destination destination) (Advance.emit base destination))))) local ();
    Address.emit_def base source destination)
let (load @ total) : (base : B.u32) -> (slots : Load.slots) @ immutable -> (local : B.u32) ->
    {u : unit | slots.Load.start <> local && slots.Load.captures <> local && slots.Load.recursive <> local} ->
    {u : unit | P.preserves (Load.emit base slots) local} @ ghost =
  fun base slots local premise -> ghost_ (
    limb (Offsets.start_offset ()) base slots.Load.start local ();
    limb (Offsets.captures_offset ()) base slots.Load.captures local ();
    limb (Offsets.recursive_offset ()) base slots.Load.recursive local ();
    P.append (Read.emit (Offsets.captures_offset ()) base slots.Load.captures)
      (Read.emit (Offsets.recursive_offset ()) base slots.Load.recursive) local ();
    P.append (Read.emit (Offsets.start_offset ()) base slots.Load.start)
      (E.append (Read.emit (Offsets.captures_offset ()) base slots.Load.captures) (Read.emit (Offsets.recursive_offset ()) base slots.Load.recursive)) local ();
    Load.emit_def base slots)
let (correct @ total) : (base : B.u32) -> (source : B.u32) -> (destination : B.u32) -> (slots : Load.slots) @ immutable ->
    (before : X.state) @ immutable -> (after : X.state) @ immutable -> (local : B.u32) ->
    {u : unit | destination <> local && slots.Load.start <> local && slots.Load.captures <> local && slots.Load.recursive <> local
      && X.run (Select.emit base source destination slots) before === X.Done after} ->
    {u : unit | L.get before.X.machine.E.locals local === L.get after.X.machine.E.locals local} @ ghost =
  fun base source destination slots before after local premise -> ghost_ (
    address base source destination local (); load destination slots local ();
    P.append (Address.emit base source destination) (Load.emit destination slots) local ();
    Select.emit_def base source destination slots;
    P.correct (Select.emit base source destination slots) before after local ())
