[@@@ocaml.warning "+a-40-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

(* Keep in sync with [Arch.operation_is_pure], [Arch.operation_can_raise],
   [Arch.operation_allocates]. *)
module Memory_access = Vectorize_utils.Memory_access

let memory_access : Arch.specific_operation -> Memory_access.t option =
 fun op ->
  let create ?first_memory_arg_index desc =
    Some (Memory_access.create ?first_memory_arg_index desc)
  in
  match op with
  | Ifar_poll ->
    (* Conservative, don't reorder across poll instructions. In practice, there
       are not many poll instructions present at this stage, because poll
       insertion pass currently happens after vectorize. *)
    create Arbitrary
  | Ifar_alloc _ -> create Alloc
  | Ilea _ | Istore_int _ | Ioffset_loc _ | Ifloatarithmem _ | Ishiftarith _
  | Imuladd | Imulsub | Inegmulf | Imuladdf | Inegmuladdf | Imulsubf
  | Inegmulsubf | Isqrtf | Ibswap _ | Isextend32 | Izextend32 | Irdtsc | Irdpmc
  | Ilfence | Isfence | Imfence | Ipackf32 | Imove32 | Isignext _ | Isimd _
  | Isimd_mem _ | Icldemote _ | Iprefetch _ ->
    (* Conservative. we don't have any specific operations with memory
       operations at the moment. *)
    if Arch.operation_is_pure op then None else create Memory_access.Arbitrary
  | Illvm_intrinsic intr ->
    Misc.fatal_errorf
      "Vectorize specific: Unexpected llvm_intrinsic %s: not using LLVM backend"
      intr

let is_seed_store (op : Arch.specific_operation) =
  match op with
  | Ilea _ | Istore_int _ | Ioffset_loc _ | Ifloatarithmem _ | Ifar_poll
  | Ifar_alloc _ | Ishiftarith _ | Imuladd | Imulsub | Inegmulf | Imuladdf
  | Inegmuladdf | Imulsubf | Inegmulsubf | Isqrtf | Ibswap _ | Isextend32
  | Izextend32 | Irdtsc | Irdpmc | Ilfence | Isfence | Imfence | Ipackf32
  | Imove32 | Isignext _ | Isimd _ | Isimd_mem _ | Icldemote _ | Iprefetch _ ->
    None
  | Illvm_intrinsic intr ->
    Misc.fatal_errorf
      "Vectorize specific.is_seed_store: Unexpected llvm_intrinsic %s: not \
       using LLVM backend"
      intr
