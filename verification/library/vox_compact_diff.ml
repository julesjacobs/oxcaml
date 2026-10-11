open Vox_compact_diff_spec
module S = Vox_sequence
module F = Vox_diff_spec

type ('a : logical_data) equality =
  (x : 'a) -> (y : 'a) -> {same : bool | same = (x === y)}

let rec (split @ total) (n : int) (old : 'a list) :
    {r : ('a list * 'a list) option | let k = Bigint.of_int n in
      match r with
      | None -> n < 0 || S.length old < k
      | Some (prefix, tail) -> 0 <= n && k <= S.length old
        && prefix === S.take k old && tail === S.drop k old} =
  ghost_ (S.length_def old);
  ghost_ (S.take_def (Bigint.of_int n) old);
  ghost_ (S.drop_def (Bigint.of_int n) old);
  if n < 0 then None else if n = 0 then Some ([], old) else
  match old with
  | [] -> None
  | x :: xs ->
    match split (n - 1) xs with
    | None -> None
    | Some (prefix, tail) -> Some (x :: prefix, tail)

let rec (apply @ total) (equal : 'a equality @ total)
    (old : 'a list) (edits : 'a diff) :
    {r : 'a list option | r === patch edits old} =
  ghost_ (patch_def edits old);
  match edits with
  | [] -> (match old with [] -> Some [] | _ :: _ -> None)
  | Keep n :: rest ->
    (match split n old with
     | None -> None
     | Some (prefix, tail) ->
       match apply equal tail rest with
       | None -> None | Some fresh -> Some (S.append prefix fresh))
  | Delete x :: rest ->
    (match old with
     | y :: tail when equal x y -> apply equal tail rest
     | _ -> None)
  | Insert x :: rest ->
    (match apply equal old rest with
     | None -> None | Some fresh -> Some (x :: fresh))

let rec (invert_cost @ total) (edits : 'a diff) :
    {u : unit | cost (inverse edits) = cost edits} =
  inverse_def edits;
  cost_def edits;
  cost_def (inverse edits);
  (match edits with [] -> () | _ :: rest -> invert_cost rest);
  ()

let (invert @ total) (edits : 'a diff) :
    {r : 'a diff | r === inverse edits && cost r = cost edits} =
  ghost_ (invert_cost edits);
  inverse edits

let rec (invert_correct @ total) (edits : 'a diff)
    (old : 'a list) (fresh : 'a list) :
    {u : unit | if relates edits old fresh
      then relates (inverse edits) fresh old else true} =
  ghost_ (
    relates_def edits old fresh;
    relates_def (inverse edits) fresh old;
    patch_def edits old;
    inverse_def edits;
    patch_def (inverse edits) fresh;
    (if relates edits old fresh then
     match edits with
     | [] -> ()
     | Keep n :: rest ->
       let k = Bigint.of_int n in
       let prefix = S.take k old in
       let tail = S.drop k old in
       S.cut old k;
       (match patch rest tail with
        | None -> ()
        | Some next ->
          relates_def rest tail next;
          invert_correct rest tail next;
          relates_def (inverse rest) next tail;
          S.append_length prefix next;
          S.append_split prefix next;
          S.length_def next)
     | Delete _ :: rest ->
       (match old with
        | [] -> ()
        | _ :: tail ->
          relates_def rest tail fresh;
          invert_correct rest tail fresh;
          relates_def (inverse rest) fresh tail)
     | Insert _ :: rest ->
       (match patch rest old with
        | None -> ()
        | Some next ->
          relates_def rest old next;
          invert_correct rest old next;
          relates_def (inverse rest) next old)));
  ()

let[@def] add_keep (edits : 'a diff) =
  match edits with
  | Keep n :: rest when 0 <= n && n < max_int -> Keep (n + 1) :: rest
  | _ -> Keep 1 :: edits

let (add_keep_correct @ total) (edits : 'a diff) (x : 'a)
    (old : 'a list) :
    {u : unit | cost (add_keep edits) = cost edits
      && patch (add_keep edits) (x :: old) ===
        (match patch edits old with None -> None | Some fresh -> Some (x :: fresh))} =
  ghost_ (
    add_keep_def edits;
    cost_def edits;
    cost_def (add_keep edits);
    patch_def (add_keep edits) (x :: old);
    S.length_def (x :: old);
    (match edits with
     | Keep n :: rest when 0 <= n && n < max_int ->
       patch_def edits old;
       S.take_def (Bigint.of_int (n + 1)) (x :: old);
       S.drop_def (Bigint.of_int (n + 1)) (x :: old);
       (match patch rest (S.drop (Bigint.of_int n) old) with
        | None -> ()
        | Some fresh -> S.append_def (x :: S.take (Bigint.of_int n) old) fresh)
     | _ ->
       S.take_def 1Z (x :: old);
       S.take_def 0Z old;
       S.drop_def 1Z (x :: old);
       S.drop_def 0Z old;
       (match patch edits old with
        | None -> ()
        | Some fresh -> S.append_def [x] fresh; S.append_def [] fresh)));
  ()

let rec (compress @ total) (full : 'a F.diff) :
    {edits : 'a diff | relates edits (F.source full) (F.target full)
      && cost edits = F.cost full} =
  ghost_ (F.source_def full);
  ghost_ (F.target_def full);
  ghost_ (F.cost_def full);
  match full with
  | [] ->
    let edits : 'a diff = [] in
    ghost_ (patch_def edits []);
    ghost_ (relates_def edits [] []);
    ghost_ (cost_def edits);
    edits
  | op :: rest ->
    let tail = compress rest in
    ghost_ (relates_def tail (F.source rest) (F.target rest));
    let edits = match op with
      | F.Keep x ->
        ghost_ (add_keep_correct tail x (F.source rest));
        add_keep tail
      | F.Delete x ->
        let edits = Delete x :: tail in
        ghost_ (patch_def edits (x :: F.source rest));
        ghost_ (cost_def edits);
        edits
      | F.Insert x ->
        let edits = Insert x :: tail in
        ghost_ (patch_def edits (F.source rest));
        ghost_ (cost_def edits);
        edits in
    ghost_ (relates_def edits (F.source full) (F.target full));
    edits

let rec (prefix_keeps @ total) (prefix : 'a list) (full : 'a F.diff) :
    {r : 'a F.diff | F.source r === S.append prefix (F.source full)
      && F.target r === S.append prefix (F.target full)
      && F.cost r = F.cost full} =
  ghost_ (S.append_def prefix (F.source full));
  ghost_ (S.append_def prefix (F.target full));
  match prefix with
  | [] -> full
  | x :: xs ->
    let tail = prefix_keeps xs full in
    let r = F.Keep x :: tail in
    ghost_ (F.source_def r);
    ghost_ (F.target_def r);
    ghost_ (F.cost_def r);
    r

let rec (expand @ total) (old : 'a list) (edits : 'a diff) :
    {full : 'a F.diff | match patch edits old with
      | None -> true
      | Some fresh -> F.source full === old && F.target full === fresh
        && F.cost full = cost edits} =
  ghost_ (patch_def edits old);
  ghost_ (cost_def edits);
  match edits with
  | [] ->
    let r : 'a F.diff = [] in
    ghost_ (F.source_def r); ghost_ (F.target_def r); ghost_ (F.cost_def r);
    r
  | Keep n :: rest ->
    (match split n old with
     | None -> []
     | Some (prefix, tail) ->
       let full = expand tail rest in
       ghost_ (S.cut old (Bigint.of_int n));
       prefix_keeps prefix full)
  | Delete x :: rest ->
    (match old with
     | [] -> []
     | _ :: tail ->
       let full = expand tail rest in
       let r = F.Delete x :: full in
       ghost_ (F.source_def r); ghost_ (F.target_def r); ghost_ (F.cost_def r);
       r)
  | Insert x :: rest ->
    let full = expand old rest in
    let r = F.Insert x :: full in
    ghost_ (F.source_def r); ghost_ (F.target_def r); ghost_ (F.cost_def r);
    r

let diff : type (a : logical_data). a equality @ total ->
    (old : a list) -> (fresh : a list) ->
    {r : a optimal_diff | r.old === old && r.fresh === fresh} =
  fun equal old fresh ->
  let full = Vox_diff.diff equal old fresh in
  let edits = compress full.edits in
  { old = ghost_ old; fresh = ghost_ fresh; edits;
    optimality = ghost_ (fun other ->
      relates_def other old fresh;
      let expanded = expand old other in
      full.optimality expanded;
      ()) }
