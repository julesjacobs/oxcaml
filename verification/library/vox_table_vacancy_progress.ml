module M = Vox_table_model
module S = Vox_sequence
module W = Vox_table_wrap

module Make (Key : Vox_table_map.Key)
    (Read : module type of Vox_table_read_proofs.Make (Key)) = struct
  module Cover = Vox_table_coverage.Make (Key) (Read.I)
  module Count = Vox_table_occupancy.Make (Key) (Read.I)

  let (full_scan @ total) : ('a : immutable_data).
      (view : 'a Read.I.view) @ immutable -> (hash : int) -> (index : int) ->
      {u : unit | not (Read.I.valid view &&
        Read.I.empty_free view.model hash (view.model.capacity lsr 4) &&
        0 <= index && index < view.model.capacity) ||
        not (M.control view.model index === Some 128)} @ ghost =
    fun view hash index -> ghost_ (
      if Read.I.valid view &&
         Read.I.empty_free view.model hash (view.model.capacity lsr 4) &&
         0 <= index && index < view.model.capacity then begin
        Read.I.valid_def view; Read.I.shape_def view.model;
        Read.I.power_of_two_def view.model.capacity;
        let rank, lane = Cover.cover view.model.capacity view.plan hash index in
        Cover.empty_free_at view.model hash (view.model.capacity lsr 4) rank;
        Read.group_in_shape view.model hash rank;
        Read.matching_control view.model (Read.I.group view.model.capacity
          hash rank)
          128 lane;
        ()
      end else ())

  let (occupied_at @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable -> (index : int) ->
      {u : unit | not (Read.I.shape model && Read.I.cells_valid model
        model.slots 0Z &&
        0 <= index && index < model.capacity &&
        not (M.control model index === Some 128)) ||
        (match S.at model.slots (Bigint.of_int index),
          S.at model.controls (Bigint.of_int index) with
         | Some entry, Some byte -> Count.occupied entry byte | _ -> false)} @
           ghost =
    fun model index -> ghost_ (
      Read.slot_present model index;
      M.slot_def model index; M.control_def model index;
      match M.slot model index with
      | Some entry ->
        Read.cell_at model model.slots 0Z (Bigint.of_int index) entry;
        (match M.control model index with
         | Some byte ->
           Count.occupied_def entry byte;
           (match entry with
            | Some (key, _) ->
              let (_ : {u : unit | (Key.hash key land 127) <> 254}) =
                () in ()
            | None -> ())
         | None -> ())
      | None -> ())

  let (not_exhausted @ total) : ('a : immutable_data).
      (view : 'a Read.I.view) @ immutable -> (hash : int) ->
      {u : unit | not (Read.I.valid view) ||
        not (Read.I.empty_free view.model hash (view.model.capacity lsr 4))} @
          ghost =
    fun view hash -> ghost_ (
      if Read.I.valid view &&
         Read.I.empty_free view.model hash (view.model.capacity lsr 4) then
           begin
        Read.I.valid_def view; Read.I.shape_def view.model;
        Count.full_count view.model.slots view.model.controls
          view.model.capacity
          (fun index -> ghost_ (
            if 0 <= index && index < view.model.capacity then begin
              full_scan view hash index;
              occupied_at view.model index;
              ()
            end else ()));
        Count.reserve_positive view.model.capacity;
        Count.not_full view.model.capacity view.model.size view.model.deleted
          (Read.I.reserve view.model.capacity);
        ()
      end else ())
end
