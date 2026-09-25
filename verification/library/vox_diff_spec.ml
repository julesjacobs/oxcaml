type operation : immutable_data mod total =
  | Keep of int | Delete of int | Insert of int
[@@inductive]
type script = operation list

let[@def] rec source (script : script) =
  match script with
  | [] -> []
  | Keep x :: rest | Delete x :: rest -> x :: source rest
  | Insert _ :: rest -> source rest

let[@def] rec target (script : script) =
  match script with
  | [] -> []
  | Keep x :: rest | Insert x :: rest -> x :: target rest
  | Delete _ :: rest -> target rest

let[@def] rec cost (script : script) =
  match script with
  | [] -> 0Z
  | Keep _ :: rest -> cost rest
  | Delete _ :: rest | Insert _ :: rest -> Bigint.add 1Z (cost rest)

let[@def] rec apply (old : int list) (script : script) =
  match script with
  | [] -> (match old with [] -> Some [] | _ :: _ -> None)
  | Keep x :: rest ->
    (match old with
     | y :: ys when x = y ->
       (match apply ys rest with None -> None | Some zs -> Some (x :: zs))
     | _ -> None)
  | Delete x :: rest ->
    (match old with y :: ys when x = y -> apply ys rest | _ -> None)
  | Insert x :: rest ->
    (match apply old rest with None -> None | Some zs -> Some (x :: zs))

let[@def] rec invert (script : script) =
  match script with
  | [] -> []
  | Keep x :: rest -> Keep x :: invert rest
  | Delete x :: rest -> Insert x :: invert rest
  | Insert x :: rest -> Delete x :: invert rest

let[@def] rec size (values : int list) =
  match values with [] -> 0Z | _ :: rest -> Bigint.add 1Z (size rest)

let[@def] rec script_size (script : script) =
  match script with [] -> 0Z | _ :: rest -> Bigint.add 1Z (script_size rest)

let rec (size_nonnegative @ total) (values : int list) :
    {u : unit | 0Z <= size values} =
  size_def values;
  (match values with [] -> () | _ :: rest -> size_nonnegative rest);
  let u = () in refine_ u

let[@def] rec distance (fuel : Bigint.t) (old : int list) (fresh : int list) =
  if fuel <= 0Z then 0Z else
  match old, fresh with
  | [], _ -> size fresh
  | _, [] -> size old
  | a :: ats, b :: bts ->
    if a = b then
      if fuel < 2Z then 0Z else distance (Bigint.sub fuel 2Z) ats bts
    else Bigint.add 1Z
      (let left = distance (Bigint.sub fuel 1Z) ats fresh in
       let right = distance (Bigint.sub fuel 1Z) old bts in
       if left <= right then left else right)
[@@decreases fuel]

let[@def] minimum_cost old fresh = distance (Bigint.add (size old) (size
  fresh)) old
  fresh

let (minimum_cost_equation @ total) (old : int list) (fresh : int list) :
    {u : unit | minimum_cost old fresh ===
      (match old, fresh with
       | [], _ -> size fresh
       | _, [] -> size old
       | a :: ats, b :: bts ->
         if a = b then minimum_cost ats bts
         else Bigint.add 1Z
           (if minimum_cost ats fresh <= minimum_cost old bts
            then minimum_cost ats fresh else minimum_cost old bts))} =
  size_nonnegative old;
  size_nonnegative fresh;
  size_def old;
  size_def fresh;
  minimum_cost_def old fresh;
  distance_def (Bigint.add (size old) (size fresh)) old fresh;
  (match old, fresh with
   | a :: ats, b :: bts ->
     size_nonnegative ats;
     size_nonnegative bts;
     minimum_cost_def ats bts;
     minimum_cost_def ats fresh;
     minimum_cost_def old bts
   | _ -> ());
  let u = () in refine_ u
