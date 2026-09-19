module P = Ghost_pref
module H = P.Heap
module T = Vox_table_storage
module M = Vox_table_model

(** Hash and equality are pure, total, and stable on immutable keys. *)
module Make (Key : Vox_table_map.Key) : sig
  module Spec : module type of Vox_table_invariant.Make (Key)

  type ('a : immutable_data) t = (Key.t, 'a) T.t
  type ('a : immutable_data) created = {
    table : 'a t @@ aliased;
    view : 'a Spec.view @@ aliased;
    state : (Key.t, 'a) M.state P.token @@ ghost;
  }
  type ('a : immutable_data) result = #{
    view : 'a Spec.view @@ aliased;
    state : (Key.t, 'a) M.state P.token @@ ghost;
  }

  (** Allocate an empty table and extend ownership with its fresh region. *)
  val create : ('a : immutable_data).
    (token : (Key.t, 'a) M.state P.token) @ unique ghost ->
    {r : 'a created | Spec.valid r.view && r.view.model === M.initial 16 None &&
      not (H.mem (P.own token) (T.location r.table)) &&
      P.own r.state === H.put (P.own token) (T.location r.table) r.view.model}
        @ unique

  val length : ('a : immutable_data).
    (table : 'a t) @ immutable ->
    (view : {v : 'a Spec.view | Spec.valid v}) @ immutable ->
    (token : {t : (Key.t, 'a) M.state P.token | H.at (P.own t) (T.location
      table) === Some
      view.model})
      @ local read ghost -> {size : int | size = view.model.size}

  (** The actual returned option equals lookup in the owned table's map. *)
  val find_opt : ('a : immutable_data).
    (table : 'a t) @ immutable ->
    (view : {v : 'a Spec.view | Spec.valid v}) @ immutable -> (key : Key.t) @
      immutable ->
    (token : {t : (Key.t, 'a) M.state P.token | H.at (P.own t) (T.location
      table) === Some
      view.model})
      @ local read ghost ->
    {value : 'a option | value === Spec.Map.lookup view.model.slots key} @
      immutable

  (** Return the stored value, or raise [Not_found] for an absent key. *)
  val find : ('a : immutable_data).
    (table : 'a t) @ immutable ->
    (view : {v : 'a Spec.view | Spec.valid v}) @ immutable -> (key : Key.t) @
      immutable ->
    (token : {t : (Key.t, 'a) M.state P.token | H.at (P.own t) (T.location
      table) === Some
      view.model})
      @ local read ghost ->
    {value : 'a | Spec.Map.lookup view.model.slots key === Some value} @
      immutable

  val mem : ('a : immutable_data).
    (table : 'a t) @ immutable ->
    (view : {v : 'a Spec.view | Spec.valid v}) @ immutable -> (key : Key.t) @
      immutable ->
    (token : {t : (Key.t, 'a) M.state P.token | H.at (P.own t) (T.location
      table) === Some
      view.model})
      @ local read ghost ->
    {present : bool | present = (match Spec.Map.lookup view.model.slots key with
      | None -> false | Some _ -> true)}

  (** Insert or replace one binding, preserving the other owned regions.
      Rebuilding preserves bindings and the identity of aliased handles. *)
  val replace : ('a : immutable_data).
    (table : 'a t) @ immutable ->
    (before : {v : 'a Spec.view | Spec.valid v}) @ immutable ->
    (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
    (token : {t : (Key.t, 'a) M.state P.token | H.at (P.own t) (T.location
      table) === Some
      before.model})
      @ unique read_write ghost ->
    {r : 'a result | Spec.valid r.#view &&
      Spec.Map.same r.#view.model.slots (Spec.Map.put before.model.slots key
        value) &&
      P.own r.#state === H.put (P.own token) (T.location table) r.#view.model}
        @ unique

  (** Remove the key if present, preserving every other binding and region. *)
  val remove : ('a : immutable_data).
    (table : 'a t) @ immutable ->
    (before : {v : 'a Spec.view | Spec.valid v}) @ immutable -> (key : Key.t)
      @ immutable ->
    (token : {t : (Key.t, 'a) M.state P.token | H.at (P.own t) (T.location
      table) === Some
      before.model})
      @ unique read_write ghost ->
    {r : 'a result | Spec.valid r.#view &&
      Spec.Map.same r.#view.model.slots (Spec.Map.erase before.model.slots
        key) &&
      P.own r.#state === H.put (P.own token) (T.location table) r.#view.model}
        @ unique

  (** Clear bindings and retained payloads while keeping the allocated
    capacity. *)
  val clear : ('a : immutable_data).
    (table : 'a t) @ immutable ->
    (before : {v : 'a Spec.view | Spec.valid v}) @ immutable ->
    (token : {t : (Key.t, 'a) M.state P.token | H.at (P.own t) (T.location
      table) === Some
      before.model})
      @ unique read_write ghost ->
    {r : 'a result | Spec.valid r.#view &&
      r.#view.model === M.initial before.model.capacity None &&
      P.own r.#state === H.put (P.own token) (T.location table) r.#view.model}
        @ unique
end
