(* Lean per-library summary (magic CMXASUM3): the single source of truth for the
   [.cmxasum] projection format, shared by the producer (ocamlopt -emit-cmxa-summary, and
   the standalone linkbench emitter) and the consumer (optlink's -cmxa-summaries pass-1
   path).

   A summary keeps, per unit, its own cmi export CRC, impl CRC, defines, force_link, and
   cmx dependency NAMES (for reachability); plus per-library generic-fn contributions,
   ccobjs/ccopts, requires_metaprogramming. It drops the interface/implementation import
   CRC ARRAYS (the 89-93% bulk that exists only to feed the link-time consistency check)
   and external_symbols. It carries NO dependency CRCs (only dep names), so a library's
   summary is a function of that library's own [.cmxa] alone. Unit identities are full
   [Compilation_unit.t] values (marshal-structural): names alone drop
   parameterized-library instance arguments, collapsing distinct instances. *)

open Cmx_format

type t

(* 9-byte format tag written at the head of every [.cmxasum]. *)
val magic : string

(* Canonical, byte-deterministic projection of a library's metadata. [bump], if given,
   replaces that unit's impl CRC with a perturbed value (leaf-edit test only); production
   emission passes no [bump]. *)
val project : ?bump:string -> library_infos -> t

(* Read a [.cmxa], project it, and write the summary to [dst]. Producer entry point: the
   in-compiler [-emit-cmxa-summary] mode and the standalone emitter both go through here,
   so their bytes are identical by construction. *)
val emit : src:string -> dst:string -> unit

(* Directory of summaries to consume, from [-cmxa-summaries]; [None] when unset. *)
val dir : unit -> string option

(* Read the summary for a library and inflate it to a synthetic [library_infos] so pass
   1's [scan_file] is unchanged. [obj_name] is the pre-resolution name (tried first);
   [file_name] is the Load_path-resolved path (fallback). Any read failure is a
   [Misc.fatal_errorf] naming the summary and library. *)
val read : dir:string -> obj_name:string -> file_name:string -> library_infos

(* Lower-level pieces, exposed for the standalone emitter's batch/list modes. *)
val write_to_channel : out_channel -> t -> unit
val units_of : t -> Compilation_unit.t list
