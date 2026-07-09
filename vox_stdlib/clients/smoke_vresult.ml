(* Per-module SMOKE client (dead-law check §6.7): a few-line goal per shipped
   law, forcing each to fire. Verified against Vresult.cmi + VoxSig_Vresult.olean
   (+ Voption for to_option's result type). Because the model defs are NOT
   `expose`d, a client cannot unfold them; each base goal is dischargeable ONLY
   via the corresponding law, so these goals prove the laws are LIVE.
   Post-#53 (finding C1): a raw constructor application (Vok x / Verror e) is
   reflectable and now passes INLINE to the dependent op -- C1 let-binds removed.

   HOF section (WP-1): map/map_error/bind/fold/to_option. Exposed ADT -> EXACT
   output available (map_ok_exact / fold_ok_exact extract it). The substrate is
   REUSED from Voption (vo_rHolds etc.); relation goals name block abbrevs. *)
open Vresult

[%%vox.lean {lean|
@[grind, expose] abbrev rSucc : Int -> Int -> Prop := fun a b => b = a + 1
@[grind, expose] abbrev rAdd  : Int -> Int -> Int -> Prop := fun a x c => c = a + x
|lean}]

let smoke_ok (x : int) : bool{ _ = true } = Vresult.is_ok (Vresult.Vok x)
let smoke_iserr (e : int) : bool{ _ = true } = Vresult.is_error (Vresult.Verror e)
let smoke_get (d : int) (x : int) : int{ _ = x } = Vresult.get_ok_or d (Vresult.Vok x)
let smoke_geterr (d : int) (e : int) : int{ _ = e } = Vresult.get_err_or d (Vresult.Verror e)

(* map: transforms the Ok payload r-related (relational, symbolic). *)
let map_rel (s : t) : t{ vr_maprel rSucc s _ } =
  Vresult.map (fun a b -> b = a + 1) (fun z -> z + 1) s
(* map: EXACT on a concrete Ok. *)
let map_ok_exact (x : int) : int{ _ = x + 1 } =
  let s = Vok x in
  let m = Vresult.map (fun a b -> b = a + 1) (fun z -> z + 1) s in
  Vresult.get_ok_or 0 m
(* map_error: transforms the Error payload; leaves Ok unchanged. *)
let maperr_rel (s : t) : t{ vr_maperr rSucc s _ } =
  Vresult.map_error (fun a b -> b = a + 1) (fun z -> z + 1) s
(* bind: result carries the bind relation. *)
let bind_rel (s : t) : t{ vr_bindrel rSucc s _ } =
  Vresult.bind (fun a b -> b = a + 1) (fun x -> Vok (x + 1)) s
(* fold: EXACT on a concrete Ok (add step). *)
let fold_ok_exact (init : int) (x : int) : int{ _ = init + x } =
  Vresult.fold (fun a y c -> c = a + y) (fun a y -> a + y) init (Vok x)
(* fold: Error returns init. *)
let fold_err_exact (init : int) (e : int) : int{ _ = init } =
  Vresult.fold (fun a y c -> c = a + y) (fun a y -> a + y) init (Verror e)
(* to_option: Ok x -> Some x carries the bridge relation. *)
let to_opt_rel (s : t) : Voption.t{ vr_to_opt_rel s _ } =
  Vresult.to_option s
