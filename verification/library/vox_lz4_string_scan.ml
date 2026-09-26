module F = Vox_lz4_fast_plan_model
module M = Vox_lz4_string_match
module P = Vox_lz4_general_plan
module A = Vox_iarray
module B = Borrow_iarray.Owned_array
module T = Vox_lz4_mutable_scan
module V = Vox_string_view

let rec scan :
    (model : {m : char iarray | Iarray.length m <= 4194304}) @ ghost ->
    (source : {s : string | V.contents s === model}) ->
    (entries : F.hashes) @ ghost ->
    (table : {t : int B.t | Iarray.length (B.contents t) = 65536
      && T.agrees (B.contents t) entries 65536}) @ unique ->
    (position : {p : int | 0 <= p && p <= Iarray.length model}) ->
    (anchor : {a : int | 0 <= a && a <= position}) ->
    (fuel : {f : int | f = Iarray.length model - position + 1}) ->
    (pending : {p : P.pending | P.valid_pending model 0 anchor p}) ->
    {r : P.plan | Vox_lz4_spec_plan.valid_plan model 0 r
      && r === F.scan model entries position anchor fuel pending} =
  fun model source entries table position anchor fuel pending ->
    ghost_ (F.scan_def model entries position anchor fuel pending);
    if position > V.length source - 12 then begin
      let _ = B.into_iarray table in
      ghost_ (Vox_lz4_spec_plan.valid_plan_def model anchor P.End);
      P.build_plan model 0 anchor pending P.End
    end else begin
      let hash = M.hash4 source model position in
      let before = ghost_ (B.contents (borrow_ table)) in
      ghost_ (T.agrees_get before entries 65536 hash);
      let candidate = B.get_int (borrow_ table) hash in
      let table = B.set_int table hash position in
      ghost_ (
        A.updated_length before hash position;
        T.agrees_set before entries 65536 hash position);
      let entries = ghost_ ((hash, position) :: entries) in
      let limit = V.length source - 5 - position in
      match M.choose_match source model position limit candidate with
      | None ->
        scan model source entries table (position + 1) anchor (fuel - 1) pending
      | Some choice ->
        let next = position + choice.length in
        let step = { P.position; distance = choice.distance;
                     length = choice.length } in
        ghost_ (
          Vox_lz4_spec_plan.valid_plan_def model next P.End;
          Vox_lz4_spec_plan.valid_plan_def model anchor (P.Sequence (step, P.End));
          P.valid_pending_def model 0 next
            (P.More (anchor, step, pending)));
        scan model source entries table next next (fuel - choice.length)
          (P.More (anchor, step, pending))
    end
[@@decreases fuel]

let from_source :
    (model : {m : char iarray | Iarray.length m <= 4194304}) @ ghost ->
    (source : {s : string | V.contents s === model}) ->
    {r : P.plan | Vox_lz4_spec_plan.valid_plan model 0 r && r === F.from_source model} =
  fun model source ->
    let table = B.of_iarray T.empty_table in
    ghost_ (
      F.from_source_def model;
      P.valid_pending_def model 0 0 P.Stop);
    scan model source (ghost_ []) table 0 0 (V.length source + 1) P.Stop
