module M = Vox_lz4_general_match
module P = Vox_lz4_general_plan

include Vox_lz4_spec_hashes

let[@def] rec (scan @ total) :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    (entries : hashes) ->
    (position : {p : int | 0 <= p && p <= Iarray.length source}) ->
    (anchor : {a : int | 0 <= a && a <= position}) ->
    (fuel : {f : int | f = Iarray.length source - position + 1}) ->
    (pending : {p : P.pending | P.valid_pending source 0 anchor p}) ->
    {r : P.plan | Vox_lz4_spec_plan.valid_plan source 0 r} =
  fun source entries position anchor fuel pending ->
    if position > Iarray.length source - 12 then begin
      ghost_ (Vox_lz4_spec_plan.valid_plan_def source anchor P.End);
      P.build_plan source 0 anchor pending P.End
    end else begin
      let hash = Vox_lz4_spec_match.hash4 source position in
      let candidate = Vox_lz4_spec_hashes.lookup hash entries in
      let entries = (hash, position) :: entries in
      let limit = Iarray.length source - 5 - position in
      match Vox_lz4_spec_match.choose_match source position limit candidate with
      | None ->
        scan source entries (position + 1) anchor (fuel - 1) pending
      | Some choice ->
        let next = position + choice.length in
        let step = { P.position; distance = choice.distance;
                     length = choice.length } in
        ghost_ (
          Vox_lz4_spec_plan.valid_plan_def source next P.End;
          Vox_lz4_spec_plan.valid_plan_def source anchor (P.Sequence (step, P.End));
          P.valid_pending_def source 0 next
            (P.More (anchor, step, pending)));
        scan source entries next next (fuel - choice.length)
          (P.More (anchor, step, pending))
    end
[@@decreases fuel]

let[@def] (from_source @ total) :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    {r : P.plan | Vox_lz4_spec_plan.valid_plan source 0 r} =
  fun source ->
    ghost_ (P.valid_pending_def source 0 0 P.Stop);
    scan source [] 0 0 (Iarray.length source + 1) P.Stop
