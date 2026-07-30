(* Per-executable link plan (magic LINKPLAN1): the result of link pass 1, split into two
   artifacts a build system can schedule and cache independently.

   The stable/volatile split is the load-bearing design declaration. Every field is
   annotated with the consumer that needs it.

   - <base>.plan-stable — everything the stable startup object (Asmlink stable codegen)
     and the final ld command line need, and NOTHING that changes on an
     implementation-only leaf edit. Carries no CRCs, so it is byte-identical across such
     edits (the early-cutoff seam).
   - <base>.plan-volatile — the exact caml_globals_map (per-unit interface/impl CRCs +
     interface-only entries), stored verbatim as make_globals_map produced it, so the
     plan-driven volatile object is byte-identical to the scan-driven one. Changes on a
     leaf edit; the volatile object is cheap to re-emit.

   Both are produced by one PLAN action (pass 1 run once), typically consuming
   [-cmxa-summaries]. *)

module CU = Compilation_unit

(* One entry of the ordered link set. No CRCs: identities and dependency NAMES only, so
   the stable plan is unchanged by an implementation-only leaf edit. *)
type unit_stable =
  { us_name : CU.t (* startup name/global tables; join key for the volatile plan *)
  ; us_defines : CU.t list (* startup name_list + globals_map symbols *)
  ; us_cmx_dep_names : CU.t list (* manual-module-init unit_deps_table (names only) *)
  }

type stable =
  { version : string (* Config.cmx_magic_number; reject cross-compiler reuse *)
  ; units : unit_stable list (* ordered tolink set: startup codegen + unit_deps *)
  ; ml_objfiles : string list (* the .a/.o inputs to the final ld command line *)
  ; genfns_entries : Cmx_format.generic_fns
      (* stable startup: GC roots / generic-fn table *)
  ; cached_imported_units :
      CU.t list (* stable startup code/frame tables under -use-cached-generic-functions *)
  ; ccobjs : string list (* final ld command line *)
  ; ccopts : string list (* final ld command line *)
  ; manual_module_init : bool (* stable startup codegen shape *)
  ; use_cached_generic_functions : bool (* stable startup codegen shape *)
  ; output_complete_object : bool (* stable startup codegen shape *)
  }

(* The exact caml_globals_map: (unit, own interface CRC, own impl CRC, symbols). Defined
   units carry [Some impl_crc]; interface-only entries carry [None].
   [Linkenv.make_globals_map]'s output stored verbatim (order included) — the only
   leaf-edit-varying part. *)
type volatile =
  { globals_map : (CU.t * Digest.t option * Digest.t option * Symbol.t list) list }

val stable_magic : string
val volatile_magic : string

(* Build both projections from a completed pass 1. [genfns_entries] and
   [cached_imported_units] are the already-extracted forms (Generic_fns.Tbl.entries /
   Generic_fns.imported_units), matching Asmlink's startup-key construction. *)
val build
  :  linkenv:Linkenv.t
  -> units_tolink:Linkenv.unit_link_info list
  -> ml_objfiles:string list
  -> genfns_entries:Cmx_format.generic_fns
  -> cached_imported_units:CU.t list
  -> stable * volatile

(* Write <base>.plan-stable and <base>.plan-volatile (magic-prefixed, marshaled).
   Deterministic: same pass-1 result -> byte-identical files. *)
val write : base:string -> stable -> volatile -> unit

(* Readers (magic + compiler-version checked; fatal on mismatch/corruption), for the
   plan-consuming actions and the negative-control gates. *)
val read_stable : string -> stable
val read_volatile : string -> volatile

(* Reconstruct the pass-1 [units_tolink] from the plan (impl CRCs from the volatile
   globals_map; dependency imports rebuilt with [crc = None]). Used by the plan-consuming
   final link so it never scans a [.cmxa]. *)
val units_tolink : stable -> volatile -> Linkenv.unit_link_info list

(* CU.Name strings of the units carrying an impl CRC (the linked units whose CRC a leaf
   edit perturbs), for gates/tools. *)
val defined_unit_names : volatile -> string list
