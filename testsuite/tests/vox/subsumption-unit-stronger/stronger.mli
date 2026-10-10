(* Weaker than stronger.ml in every declaration.  stronger.ml makes no
   refinement introduction of its own (externals are trusted and plain
   bindings reuse their types), so the only proofs of this unit are the
   inclusion obligations (DESIGN.md 3.3.5, "generate"). *)

(* A weaker postcondition: r = x implies r >= x. *)
val f : (x : int) -> {r : int | r >= x}

(* A stronger precondition and a weaker, non-dependent postcondition: the
   implementation's binder x is bound to the caller's argument. *)
val g : {x : int | x > 0} -> {r : int | r > 0}

(* A lemma re-exported at a weaker statement. *)
val lem : (x : int) -> {u : unit | x + 0 = x} @@ total

(* A dropped refinement (stage 1, no proof). *)
val h : int -> int
