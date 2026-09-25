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

let rec (apply_source @ total) (script : script) :
    {u : unit | apply (source script) script === Some (target script)} =
  source_def script;
  target_def script;
  apply_def (source script) script;
  (match script with [] -> () | _ :: rest -> apply_source rest);
  let u = () in refine_ u

let[@def] rec invert (script : script) =
  match script with
  | [] -> []
  | Keep x :: rest -> Keep x :: invert rest
  | Delete x :: rest -> Insert x :: invert rest
  | Insert x :: rest -> Delete x :: invert rest

let rec (invert_correct @ total) (script : script) :
    {u : unit | source (invert script) === target script
      && target (invert script) === source script
      && cost (invert script) = cost script} =
  invert_def script;
  source_def script;
  target_def script;
  cost_def script;
  source_def (invert script);
  target_def (invert script);
  cost_def (invert script);
  (match script with [] -> () | _ :: rest -> invert_correct rest);
  let u = () in refine_ u

let[@def] rec restore_old (rev : script) (suffix : int list) =
  match rev with
  | [] -> suffix
  | Keep x :: rest | Delete x :: rest -> restore_old rest (x :: suffix)
  | Insert _ :: rest -> restore_old rest suffix

let[@def] rec restore_new (rev : script) (suffix : int list) =
  match rev with
  | [] -> suffix
  | Keep x :: rest | Insert x :: rest -> restore_new rest (x :: suffix)
  | Delete _ :: rest -> restore_new rest suffix

let rec (reverse_into @ total) : (rev : script) -> (suffix : script) ->
    {s : script | source s === restore_old rev (source suffix)
      && target s === restore_new rev (target suffix)
      && cost s = Bigint.add (cost rev) (cost suffix)} = fun rev suffix ->
  ghost_ (cost_def rev);
  ghost_ (restore_old_def rev (source suffix));
  ghost_ (restore_new_def rev (target suffix));
  match rev with
  | [] -> refine_ suffix
  | op :: rest ->
    let next = op :: suffix in
    ghost_ (cost_def next);
    ghost_ (source_def next);
    ghost_ (target_def next);
    let refine_ result = reverse_into rest next in
    refine_ result

let[@def] rec size (values : int list) =
  match values with [] -> 0Z | _ :: rest -> Bigint.add 1Z (size rest)

let rec (size_nonnegative @ total) (values : int list) :
    {u : unit | 0Z <= size values} =
  size_def values;
  (match values with [] -> () | _ :: rest -> size_nonnegative rest);
  let u = () in refine_ u

let[@def] rec suffix (whole : int list) (tail : int list) =
  ghost_ (whole === tail || match whole with
    | [] -> false | _ :: rest -> suffix rest tail)

let rec (suffix_bounds @ total) : (whole : int list) -> (tail : int list) ->
    {u : unit | if suffix whole tail then
      size tail <= size whole && (if size tail = size whole then tail === whole
        else true)
      else true} = fun whole tail ->
  suffix_def whole tail;
  size_def whole;
  (match whole with
   | [] -> ()
   | _ :: rest -> suffix_bounds rest tail);
  let u = () in refine_ u

let rec (suffix_step @ total) : (whole : int list) -> (head : int) ->
    (tail : int list) ->
    {u : unit | if suffix whole (head :: tail) then suffix whole tail else
      true} =
    fun whole head tail ->
  suffix_def whole (head :: tail);
  suffix_def whole tail;
  suffix_def tail tail;
  match whole with
  | [] -> let u = () in refine_ u
  | _ :: rest ->
    suffix_step rest head tail;
    let u = () in refine_ u

let rec (suffix_compare @ total) : (whole : int list) ->
    (shorter : int list) -> (longer : int list) ->
    {u : unit | if suffix whole shorter && suffix whole longer
      && size shorter <= size longer then suffix longer shorter else true} =
    fun whole shorter longer ->
  suffix_def whole shorter;
  suffix_def whole longer;
  suffix_def longer shorter;
  suffix_bounds whole shorter;
  suffix_bounds whole longer;
  (match whole with
   | [] -> ()
   | _ :: rest -> suffix_compare rest shorter longer);
  let u = () in refine_ u

let (size_zero @ total) (values : int list) :
    {u : unit | if size values = 0Z then values === [] else true} =
  size_def values;
  (match values with [] -> () | _ :: rest -> size_nonnegative rest);
  let u = () in refine_ u

let rec (apply_characterization @ total) : (old : int list) -> (script :
  script) ->
    {u : unit | apply old script ===
      (if old === source script then Some (target script) else None)} =
    fun old script ->
  apply_def old script;
  source_def script;
  target_def script;
  (match script with
   | [] -> ()
   | Insert _ :: rest -> apply_characterization old rest
   | Keep _ :: rest | Delete _ :: rest ->
     match old with [] -> () | _ :: tail -> apply_characterization tail rest);
  let u = () in refine_ u

let (inverse_patch @ total) (script : script) :
    {u : unit | apply (target script) (invert script) === Some (source script)}
      =
  invert_correct script;
  apply_source (invert script);
  let u = () in refine_ u

let[@def] rec script_size (script : script) =
  match script with [] -> 0Z | _ :: rest -> Bigint.add 1Z (script_size rest)

let rec (script_bounds @ total) (script : script) :
    {u : unit | 0Z <= cost script && cost script <= script_size script
      && script_size script <= Bigint.add (size (source script)) (size (target
        script))} =
  source_def script;
  target_def script;
  cost_def script;
  script_size_def script;
  size_def (source script);
  size_def (target script);
  (match script with [] -> () | _ :: rest -> script_bounds rest);
  let u = () in refine_ u
