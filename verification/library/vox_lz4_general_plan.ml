module M = Vox_lz4_general_match
module S = Vox_sequence
module D = Vox_lz4_packed
module R = Vox_lz4_roundtrip
module Raw = Raw_memory
module P = Ghost_pref

type sequence = {
  position : int;
  distance : int;
  length : int;
}

type plan = End | Sequence of sequence * plan [@@inductive]

let[@def] rec (valid_plan @ total) (source : char iarray @ immutable)
    (anchor : int) (plan : plan @ immutable) = ghost_ (
  match plan with
  | End -> 0 <= anchor && anchor <= Iarray.length source
  | Sequence (step, rest) ->
    0 <= anchor && anchor <= step.position
    && step.position <= Iarray.length source - 12
    && 0 < step.distance && step.distance <= 65535
    && step.distance <= step.position
    && 4 <= step.length
    && step.length <= Iarray.length source - 5 - step.position
    && M.source_matches_distance source step.position
         step.distance step.length
    && valid_plan source (step.position + step.length) rest)

type pending = Stop | More of int * sequence * pending [@@inductive]

let[@def] rec (valid_pending @ total)
    (source : char iarray @ immutable) (start : int) (endpoint : int)
    (pending : pending @ immutable) = ghost_ (
  match pending with
  | Stop -> start = endpoint
  | More (anchor, step, rest) ->
    step.position + step.length = endpoint
    && valid_plan source anchor (Sequence (step, End))
    && valid_pending source start anchor rest)

let rec (build_plan @ total) :
    (source : char iarray) @ ghost ->
    (start : {a : int | 0 <= a && a <= Iarray.length source}) ->
    (endpoint : {a : int | 0 <= a && a <= Iarray.length source}) ->
    (pending : {p : pending | valid_pending source start endpoint p}) ->
    (suffix : {p : plan | valid_plan source endpoint p}) ->
    {r : plan | valid_plan source start r} =
  fun source start endpoint pending suffix ->
    ghost_ (valid_pending_def source start endpoint pending);
    match pending with
    | Stop -> suffix
    | More (anchor, step, rest) ->
      ghost_ (valid_plan_def source anchor (Sequence (step, End)));
      let anchor : {a : int | 0 <= a && a <= Iarray.length source} =
        refine_ anchor in
      ghost_ (
        valid_plan_def source endpoint suffix;
        valid_plan_def source anchor (Sequence (step, suffix)));
      build_plan source start anchor rest (Sequence (step, suffix))

let rec (plan_from_hints @ total) :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    (hints : {h : int iarray |
      Iarray.length h = Iarray.length source}) ->
    (position : {p : int | 0 <= p && p <= Iarray.length source}) ->
    (anchor : {a : int | 0 <= a && a <= position}) ->
    (fuel : {f : int | f = Iarray.length source - position + 1}) ->
    (pending : {p : pending | valid_pending source 0 anchor p}) ->
    {r : plan | valid_plan source 0 r} =
  fun source hints position anchor fuel pending ->
    if position > Iarray.length source - 12 then begin
      ghost_ (valid_plan_def source anchor End);
      build_plan source 0 anchor pending End
    end else begin
      let hint = S.iarray_get hints position in
      let limit = Iarray.length source - 5 - position in
      match M.choose_match source position limit hint with
      | None -> plan_from_hints source hints (position + 1) anchor
          (fuel - 1) pending
      | Some choice ->
        let next = position + choice.length in
        let step = { position; distance = choice.distance;
                     length = choice.length } in
        ghost_ (
          valid_plan_def source next End;
          valid_plan_def source anchor (Sequence (step, End));
          valid_pending_def source 0 next
            (More (anchor, step, pending)));
        plan_from_hints source hints next next
          (fuel - choice.length) (More (anchor, step, pending))
    end
[@@decreases fuel]

let (from_hints @ total) :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    (hints : {h : int iarray |
      Iarray.length h = Iarray.length source}) ->
    {r : plan | valid_plan source 0 r} =
  fun source hints ->
    ghost_ (valid_pending_def source 0 0 Stop);
    plan_from_hints source hints 0 0 (Iarray.length source + 1) Stop

let[@def] rec (apply_plan @ total) (heap : P.heap @ immutable)
    (block : Raw.t @ immutable) (source : char iarray @ immutable)
    (anchor : int) (plan : plan @ immutable) = ghost_ (
  match plan with
  | End ->
    D.literal_heap heap block anchor source anchor
      (Iarray.length source - anchor)
  | Sequence (step, rest) ->
    let after_literals =
      D.literal_heap heap block anchor source anchor
        (step.position - anchor) in
    let after_match =
      D.copy_heap after_literals block step.position
        step.distance step.length in
    apply_plan after_match block source
      (step.position + step.length) rest)

let rec (plan_reconstructs_source @ total) :
    (heap : P.heap) -> (block : Raw.t) ->
    (source : char iarray) -> (anchor : int) ->
    (plan : plan) ->
    {u : unit | not (valid_plan source anchor plan
      && R.output_matches heap block source anchor)
      || R.output_matches (apply_plan heap block source anchor plan)
           block source (Iarray.length source)} @ ghost =
  fun heap block source anchor plan -> ghost_ (
    valid_plan_def source anchor plan;
    apply_plan_def heap block source anchor plan;
    (match plan with
    | End ->
      if valid_plan source anchor plan
         && R.output_matches heap block source anchor then
        M.copy_literals_preserves_source heap block source anchor
          (Iarray.length source - anchor)
    | Sequence (step, rest) ->
      if valid_plan source anchor plan
         && R.output_matches heap block source anchor then begin
        let after_literals =
          D.literal_heap heap block anchor source anchor
            (step.position - anchor) in
        let after_match =
          D.copy_heap after_literals block step.position
            step.distance step.length in
        M.sequence_preserves_source heap block source anchor
          (step.position - anchor) step.distance step.length;
        plan_reconstructs_source after_match block source
          (step.position + step.length) rest
      end);
    ())
