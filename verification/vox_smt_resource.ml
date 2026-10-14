type t =
  { parent : t option;
    budget : int;
    mutable used : int;
    mutable until_check : int;
    checking : bool ref;
    cancelled : (unit -> bool) option;
    deadline : ((unit -> float) * float) option;
    mutable exhausted : string option
  }

exception Exhausted of string

let finite value =
  match classify_float value with
  | FP_normal | FP_subnormal | FP_zero -> true
  | FP_infinite | FP_nan -> false

let sample clock =
  let value = clock () in
  if not (finite value)
  then invalid_arg "Vox_smt_resource: non-finite clock";
  value

let create ?parent ?(budget = max_int) ?cancelled ?clock ?timeout () =
  if budget < 0 then invalid_arg "Vox_smt_resource.create: budget";
  let checking = match parent with
    | None -> ref false | Some parent -> parent.checking in
  let deadline = match timeout with
    | None -> None
    | Some duration ->
      if not (finite duration) || duration <= 0.
      then invalid_arg "Vox_smt_resource.create: timeout";
      let clock = match clock with
        | Some clock -> clock
        | None -> invalid_arg "Vox_smt_resource.create: timeout without clock"
      in
      if !checking then invalid_arg "Reentrant SMT resource callback";
      checking := true;
      let deadline = Fun.protect
          ~finally:(fun () -> checking := false)
          (fun () -> sample clock +. duration) in
      if not (finite deadline)
      then invalid_arg "Vox_smt_resource.create: deadline overflow";
      Some (clock, deadline)
  in
  { parent; budget; used = 0; until_check = 0; checking; cancelled; deadline;
    exhausted = None }

let stop t reason =
  t.exhausted <- Some reason;
  raise (Exhausted reason)

let rec check_inner t =
  (match t.exhausted with
  | Some reason -> raise (Exhausted reason)
  | None -> ());
  (match t.parent with
  | None -> ()
  | Some parent ->
    (try check_inner parent with Exhausted reason -> stop t reason));
  (match t.cancelled with
  | Some cancelled when cancelled () -> stop t "cancelled"
  | Some _ | None -> ());
  (match t.deadline with
  | Some (clock, deadline) when sample clock >= deadline -> stop t "timeout"
  | Some _ | None -> ());
  t.until_check <- 256

let check t =
  if !(t.checking) then invalid_arg "Reentrant SMT resource callback";
  t.checking := true;
  Fun.protect ~finally:(fun () -> t.checking := false)
    (fun () -> check_inner t)

let remaining t = t.budget - t.used
let used t = t.used

let rec limiting t =
  if !(t.checking) then invalid_arg "Reentrant SMT resource callback";
  match t.parent with
  | None -> t
  | Some parent ->
    let ancestor = limiting parent in
    if remaining ancestor <= remaining t then ancestor else t

let rec needs_check t amount =
  t.exhausted <> None || amount >= t.until_check
  || match t.parent with None -> false
    | Some parent -> needs_check parent amount

let rec reserve t amount =
  Option.iter (fun parent -> reserve parent amount) t.parent;
  t.used <- t.used + amount;
  t.until_check <- max 0 (t.until_check - min amount 256)

let charge_slow t amount =
  if amount < 0 then invalid_arg "Vox_smt_resource.charge: negative work";
  let limit = limiting t in
  if needs_check t amount || amount > remaining limit then check t;
  let limit = limiting t in
  let available = remaining limit in
  reserve t (min amount available);
  if amount > available then begin
    limit.exhausted <- Some "resource limit";
    stop t "resource limit"
  end

let charge t amount =
  if t.parent = None && not !(t.checking) && t.exhausted = None
     && amount >= 0 && amount < t.until_check && amount <= remaining t
  then begin
    t.used <- t.used + amount;
    t.until_check <- t.until_check - amount
  end else charge_slow t amount

let poll t =
  if t.parent = None && not !(t.checking) && t.exhausted = None
     && t.until_check > 1 && t.used < t.budget
  then begin
    t.used <- t.used + 1;
    t.until_check <- t.until_check - 1
  end else charge_slow t 1
