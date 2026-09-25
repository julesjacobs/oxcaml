module F = Vox_lz4_fast_plan_model
module M = Vox_lz4_general_match
module P = Vox_lz4_general_plan
module A = Vox_iarray
module B = Borrow_iarray.Owned_array

let[@def] rec (agrees @ total) (table : int iarray @ immutable)
    (entries : F.hashes @ immutable) (count : int) = ghost_ (
  if count <= 0 then true
  else A.at table (count - 1) === Some (F.lookup (count - 1) entries)
    && agrees table entries (count - 1))
[@@decreases count]

let rec (agrees_get @ total) :
    (table : int iarray) -> (entries : F.hashes) ->
    (count : {n : int | 0 <= n}) ->
    (index : int) ->
    {u : unit | not (agrees table entries count && 0 <= index && index < count)
      || A.at table index === Some (F.lookup index entries)} @ ghost =
  fun table entries count index -> ghost_ (
    agrees_def table entries count;
    if count > 0 && 0 <= index && index < count - 1 then
      agrees_get table entries (count - 1) index;
    ())
[@@decreases count]

let rec (agrees_set @ total) :
    (table : int iarray) -> (entries : F.hashes) ->
    (count : {n : int | 0 <= n && n <= Iarray.length table}) ->
    (index : {i : int | 0 <= i && i < Iarray.length table}) ->
    (position : int) ->
    {u : unit | not (agrees table entries count)
      || agrees (A.updated table index position)
           ((index, position) :: entries) count} @ ghost =
  fun table entries count index position -> ghost_ (
    agrees_def table entries count;
    agrees_def (A.updated table index position)
      ((index, position) :: entries) count;
    if count > 0 && agrees table entries count then begin
      A.updated_read table index position (count - 1);
      F.lookup_def (count - 1) ((index, position) :: entries);
      agrees_set table entries (count - 1) index position
    end;
    ())
[@@decreases count]

let rec (check_empty @ total) :
    (table : int iarray) ->
    (count : {n : int | 0 <= n && n <= Iarray.length table}) ->
    {r : bool | r = agrees table [] count} =
  fun table count ->
    ghost_ (agrees_def table [] count);
    if count = 0 then true
    else begin
      let value = A.get table (count - 1) in
      ghost_ (
        A.at_get table (count - 1);
        F.lookup_def (count - 1) []);
      if value <> -1 then false else check_empty table (count - 1)
    end
[@@decreases count]

let empty_table : {t : int iarray | Iarray.length t = 65536
    && agrees t [] 65536} =
  let table = Iarray.init 65536 (fun _ -> -1) in
  if Iarray.length table = 65536 && check_empty table 65536 then table
  else failwith "Vox_lz4_mutable_scan: invalid empty table"

let rec scan :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    (entries : F.hashes) @ ghost ->
    (table : {t : int B.t | Iarray.length (B.contents t) = 65536
      && agrees (B.contents t) entries 65536}) @ unique ->
    (position : {p : int | 0 <= p && p <= Iarray.length source}) ->
    (anchor : {a : int | 0 <= a && a <= position}) ->
    (fuel : {f : int | f = Iarray.length source - position + 1}) ->
    (pending : {p : P.pending | P.valid_pending source 0 anchor p}) ->
    {r : P.plan | P.valid_plan source 0 r
      && r === F.scan source entries position anchor fuel pending} =
  fun source entries table position anchor fuel pending ->
    ghost_ (F.scan_def source entries position anchor fuel pending);
    if position > Iarray.length source - 12 then begin
      let _ = B.into_iarray table in
      ghost_ (P.valid_plan_def source anchor P.End);
      P.build_plan source 0 anchor pending P.End
    end else begin
      let hash = M.hash4 source position in
      let before = ghost_ (B.contents (borrow_ table)) in
      ghost_ (agrees_get before entries 65536 hash);
      let candidate = B.get (borrow_ table) hash in
      let table = B.set table hash position in
      ghost_ (
        A.updated_length before hash position;
        agrees_set before entries 65536 hash position);
      let entries = ghost_ ((hash, position) :: entries) in
      let limit = Iarray.length source - 5 - position in
      match M.choose_match source position limit candidate with
      | None ->
        scan source entries table (position + 1) anchor (fuel - 1) pending
      | Some choice ->
        let next = position + choice.length in
        let step = { P.position; distance = choice.distance;
                     length = choice.length } in
        ghost_ (
          P.valid_plan_def source next P.End;
          P.valid_plan_def source anchor (P.Sequence (step, P.End));
          P.valid_pending_def source 0 next
            (P.More (anchor, step, pending)));
        scan source entries table next next (fuel - choice.length)
          (P.More (anchor, step, pending))
    end
[@@decreases fuel]

let from_source :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    {r : P.plan | P.valid_plan source 0 r && r === F.from_source source} =
  fun source ->
    let table = B.of_iarray empty_table in
    ghost_ (
      F.from_source_def source;
      P.valid_pending_def source 0 0 P.Stop);
    scan source (ghost_ []) table 0 0 (Iarray.length source + 1) P.Stop
