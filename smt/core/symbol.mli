(** Interned symbols (ADR-0003 Decision 4). A symbol is a small [int] id; identity is the
    id, so [equal]/[hash] are O(1). Interning is by name and {b idempotent}: the same name
    always maps to the same id, so a fixed sequence of declarations yields identical ids
    across runs (INVARIANTS.md I6).

    Names live in a process-global table keyed by id (this is why [name] needs no
    environment); ids are handed out in first-encounter order. Ranks live in {!Env}, not
    here. *)

type t = private int

(** [intern name] returns the id for [name], allocating a fresh one the first time and
    returning the same id on every later call (idempotent). This is the sole
    symbol-creation path; {!Env} and {!Context} call it. *)
val intern : string -> t

val equal : t -> t -> bool
val hash : t -> int
val name : t -> string
