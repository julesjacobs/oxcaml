module T = Vox_table_storage
module P = Ghost_pref
module H = P.Heap
module M = Vox_table_model
module Probe = Vox_table_probe
module W = Vox_table_wrap

external[@layout_poly] raise_any : ('a : any).
  exn -> 'a @ portable unique = "%raise"

module Make (Key : Vox_table_map.Key) = struct
  module Migrate = Vox_table_migrate.Make (Key)
  module Insert = Migrate.Insert
  module Mutation = Migrate.Mutation
  module I = Migrate.I

  let (double_capacity @ total) (capacity : W.capacity) :
      {u : unit | not (capacity <= 536870912) ||
        16 <= capacity + capacity && capacity + capacity <= 1073741824 &&
        (capacity + capacity) land (capacity + capacity - 1) = 0} @ ghost =
          ghost_ ()

  let (double_plan @ total) (capacity : int) (plan : Probe.plan @ immutable) :
      {u : unit | not (16 <= capacity && capacity <= 536870912 && Probe.valid
        plan &&
        capacity = W.scale16 (Probe.groups plan)) ||
        Probe.valid (Probe.Twice plan) &&
        capacity + capacity = W.scale16 (Probe.groups (Probe.Twice plan))}
      @ ghost = ghost_ (
    Probe.groups_range plan;
    W.scale16_def (Probe.groups plan);
    Probe.valid_def (Probe.Twice plan); Probe.groups_def (Probe.Twice plan);
    W.scale16_def (Probe.groups (Probe.Twice plan));
    ())

  let rec rebuild_into : ('a : immutable_data).
      (table : (Key.t, 'a) T.t) @ immutable ->
      (before : {v : 'a I.view | I.valid v}) @ immutable ->
      (capacity : W.capacity) ->
      (plan : {p : Probe.plan | Probe.valid p &&
        capacity = W.scale16 (Probe.groups p)}) @ immutable ghost ->
      (token : {t : P.token | H.at (P.own t) (T.location table) === Some
        before.model})
        @ unique read_write ghost ->
      {r : 'a Mutation.result | I.valid r.#view &&
        I.Map.same r.#view.model.slots before.model.slots &&
        P.own r.#state === H.put (P.own token) (T.location table) r.#view.model}
      @ unique = fun table before capacity plan token ->
    let fresh : 'a Mutation.created = Mutation.create_capacity capacity plan
      (P.empty ()) in
    ghost_ (
      I.valid_def fresh.view; I.valid_def before; I.shape_def before.model;
      M.initial_def capacity (Mutation.empty_entry fresh.table);
      Migrate.Proof.empty_start before.model.slots capacity
        (Mutation.empty_entry fresh.table));
    let copied = Migrate.copy table before fresh.table fresh.view 0 (borrow_
      token) fresh.state in
    if copied.#complete then begin
      ghost_ (H.put_law (H.empty ()) (T.location fresh.table) fresh.view.model
        copied.#view.model);
      let state = T.replace_storage table {T.model = before.model} fresh.table
        {T.model = copied.#view.model} copied.#state token in
      (#{Mutation.view = copied.#view; state} : 'a Mutation.result)
    end else if capacity > 536870912 then raise_any (Invalid_argument
      "Vox_flat_hashtbl: capacity exhausted")
    else begin
      ghost_ (double_capacity capacity; double_plan capacity plan);
      let next_capacity = capacity + capacity in
      let next_plan = ghost_ (Probe.Twice plan) in
      rebuild_into table before (refine_ next_capacity) next_plan token
    end

  let rebuild : ('a : immutable_data).
      (table : (Key.t, 'a) T.t) @ immutable ->
      (before : {v : 'a I.view | I.valid v}) @ immutable ->
      (token : {t : P.token | H.at (P.own t) (T.location table) === Some
        before.model})
        @ unique read_write ghost ->
      {r : 'a Mutation.result | I.valid r.#view &&
        I.Map.same r.#view.model.slots before.model.slots &&
        P.own r.#state === H.put (P.own token) (T.location table) r.#view.model}
      @ unique = fun table before token ->
    let capacity = T.capacity table {T.model = before.model} (borrow_ token) in
    let deleted = T.deleted table {T.model = before.model} (borrow_ token) in
    ghost_ (I.valid_def before; I.shape_def before.model; I.power_of_two_def
      capacity);
    if deleted >= capacity lsr 3 then
      rebuild_into table before (refine_ capacity) before.plan token
    else if capacity > 536870912 then raise_any (Invalid_argument
      "Vox_flat_hashtbl: capacity exhausted")
    else begin
      ghost_ (double_capacity (refine_ capacity); double_plan capacity
        before.plan);
      let next_capacity = capacity + capacity in
      let next_plan = ghost_ (Probe.Twice before.plan) in
      rebuild_into table before (refine_ next_capacity) next_plan token
    end
end
