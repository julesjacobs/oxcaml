open Vox_diff_spec
open Vox_diff

let (verified_client @ total) : (old : int list) -> (fresh : int list) ->
    (other : script) ->
    {r : (script, error) result | match r with Error _ -> true | Ok s ->
      apply old s === Some fresh
      && apply fresh (invert s) === Some old
      && (if apply old other === Some fresh then cost s <= cost other else true)} =
    fun old fresh other ->
  let result = diff old fresh in
  match result with
  | Error _ -> result
  | Ok s ->
    let computed : {s : script | cost s = minimum_cost old fresh} = s in
    ghost_ (optimal_at old fresh computed other);
    ghost_ (inverse_patch s);
    result

let (accepted @ total) :
    (old : {xs : int list | size xs <= 1000000Z}) ->
    (fresh : {xs : int list | size xs <= 1000000Z}) ->
    (other : script) ->
    {s : script | apply old s === Some fresh
      && apply fresh (invert s) === Some old
      && cost (invert s) = cost s
      && (if apply old other === Some fresh
          then cost s <= cost other else true)} = fun old fresh other ->
  let result = diff old fresh in
  match result with
  | Error Input_too_large -> unreachable_ ()
  | Ok script ->
    let computed : {s : script | cost s = minimum_cost old fresh} =
      script in
    ghost_ (optimal_at old fresh computed other);
    ghost_ (inverse_patch script);
    ghost_ (invert_correct script);
    script

let (apply_equation @ total) : (old : int list) -> (script : script) ->
  {u : unit | apply old script === (match script with
    | [] -> (match old with [] -> Some [] | _ :: _ -> None)
    | Keep x :: rest ->
      (match old with
       | y :: ys when x = y ->
         (match apply ys rest with
          | None -> None | Some zs -> Some (x :: zs))
       | _ -> None)
    | Delete x :: rest ->
      (match old with
       | y :: ys when x = y -> apply ys rest | _ -> None)
    | Insert x :: rest ->
      (match apply old rest with
       | None -> None | Some zs -> Some (x :: zs)))} = fun old script ->

  apply_characterization old script;
  source_def script;
  target_def script;
  (match script with
   | [] -> ()
   | Insert _ :: rest -> apply_characterization old rest
   | Keep _ :: rest | Delete _ :: rest ->
     match old with
     | [] -> ()
     | _ :: tail -> apply_characterization tail rest);
  ()
