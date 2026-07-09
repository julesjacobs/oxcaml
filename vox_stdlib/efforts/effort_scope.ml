(* WP-6 Effort A -- a nested-scope symbol table over Vmap (+ Vlist via keys).

   A scope chain is a single Vmap: entering a binding is [Vmap.add], and because
   [add] prepends and [find] matches the most-recent binding, an INNER binding
   shadows an OUTER one -- exactly lexical-scope resolution. The correctness
   story a reviewer recognizes: shadowing (inner wins), pass-through (unrelated
   names untouched), and definedness (a resolvable name is present).

   Composes: Vmap.empty/add/find/mem/keys, the [mopt] eliminator, Vlist.length.
   A client-defined [m_resolve] gives the resolve helper an honest spec over the
   [mopt] result, so downstream claims (shadowing) see through it. *)
[@@@warning "-6-32-26-27"]
open Vlist
open Vmap

[%%vox.lean {lean|
-- resolve's model: a found binding's value, else the default.
@[grind, expose] def m_resolve (dflt : Int) : Vox_Vmap_mopt -> Int
  | .MMiss => dflt
  | .MFound v => v
|lean}]

(* enter a binding (push into the current/innermost scope). *)
let enter (k : int) (v : int) (s : Vmap.t) : Vmap.t{ _ = m_add k v s } =
  Vmap.add k v s

(* resolve a name to its value, or a default if unbound -- spec'd via m_resolve
   over the find result, so callers reason about it. *)
let resolve (k : int) (dflt : int) (s : Vmap.t) : int{ _ = m_resolve dflt (m_find k s) } =
  match Vmap.find k s with MFound v -> v | MMiss -> dflt

(* SHADOWING (the core claim): an inner binding of k wins over an outer one. *)
let shadow_inner_wins (k : int) (v1 : int) (v2 : int) (s : Vmap.t) : int{ _ = v2 } =
  let outer = enter k v1 s in
  let inner = enter k v2 outer in
  resolve k 0 inner

(* PASS-THROUGH: binding a different key k' does not perturb k's resolution. *)
let unrelated_untouched (k : int) (k' : int) (v : int) (s : Vmap.t{ k <> k' })
  : int{ _ = m_resolve 0 (m_find k s) } =
  let s' = enter k' v s in
  resolve k 0 s'

(* DEFINEDNESS: a name just entered is present (mem = true). *)
let entered_is_present (k : int) (v : int) (s : Vmap.t) : bool{ _ = true } =
  Vmap.mem k (enter k v s)

(* an empty scope resolves nothing to the default. *)
let empty_resolves_default (k : int) (dflt : int) : int{ _ = dflt } =
  resolve k dflt (Vmap.empty ())

(* the set of defined names is enumerable (keys -> Vlist); its size is the
   scope's cardinality. A reviewer reads: "you can list what's in scope." *)
let defined_count (s : Vmap.t) : int =
  Vlist.length (Vmap.keys s)

(* ============================ FINDINGS (Effort A) ============================
   Vmap + Vlist compose cleanly for scoped resolution — the smoothest of the
   three efforts. Verified: shadowing (inner wins), pass-through, definedness,
   empty-default, key enumeration.

   F-A1 (MINOR): Vmap ships no value-extractor for the [mopt] result (a
   [get_or]-style "found value or default"). A client must define its own spec
   fn over the eliminator ([m_resolve] here) to give a resolve helper a spec that
   downstream claims see through. Without it, a spec-less resolve is opaque and
   shadowing does not close. Suggest: ship [Vmap.find_or : int -> int -> t -> int]
   with an [m_resolve]-style spec, or a [Voption] bridge.

   F-A2 (NONE, positive): add's first-binding-wins / prepend semantics
   (m_find_add_eq / m_find_add_ne) are exactly right for lexical shadowing; the
   correctness story fell out of the shipped laws with no workaround. *)
