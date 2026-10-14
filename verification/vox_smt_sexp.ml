type t = Atom of string | String of string | List of t list
exception Error of string

let to_string ?resource expression =
  let poll () = Option.iter Vox_smt_resource.poll resource in
  let rec render expression =
    poll ();
    match expression with
    | Atom s -> s
    | String s ->
      let buffer = Buffer.create (String.length s + 2) in
      Buffer.add_char buffer '"';
      String.iter (fun c ->
        poll ();
        if c = '"' then Buffer.add_char buffer '"';
        Buffer.add_char buffer c) s;
      Buffer.add_char buffer '"';
      Buffer.contents buffer
    | List xs -> "(" ^ String.concat " " (List.map render xs) ^ ")"
  in
  render expression

module Lists = Hashtbl.Make (struct
  type t = int list

  let equal = ( = )

  let hash = List.fold_left (fun hash id -> (hash * 65599) lxor id) 0
end)

module Boundaries = Hashtbl.Make (struct
  type nonrec t = t

  let hash = Hashtbl.hash

  let equal = ( == )
end)

let reader ?resource get peek =
  let poll () = Option.iter Vox_smt_resource.poll resource in
  let get = match resource with
    | None -> get
    | Some _ -> fun () -> poll (); get () in
  let atoms = Hashtbl.create 128 in
  let strings = Hashtbl.create 16 in
  let lists = Lists.create 128 in
  let next = ref 0 in
  let intern cache key create =
    match Hashtbl.find_opt cache key with
    | Some value -> value
    | None ->
      let value = create (), !next in
      incr next;
      Hashtbl.add cache key value;
      value
  in
  let atom text = intern atoms text (fun () -> Atom text) in
  let string text = intern strings text (fun () -> String text) in
  let list elements =
    let ids = List.map snd elements in
    match Lists.find_opt lists ids with
    | Some value -> value
    | None ->
      let value = List (List.map fst elements), !next in
      incr next;
      Lists.add lists ids value;
      value
  in
  let rec skip () =
    match peek () with
    | Some (' ' | '\t' | '\r' | '\n') -> ignore (get ()); skip ()
    | Some ';' ->
      while (match get () with None | Some '\n' -> false | _ -> true) do () done;
      skip ()
    | _ -> ()
  in
  let buffer = Buffer.create 32 in
  let expression atom string list recurse () =
    skip ();
    match get () with
    | None -> raise (Error "Unexpected end of input")
    | Some '(' ->
      let rec items acc =
        skip ();
        match peek () with
        | Some ')' -> ignore (get ()); list (List.rev acc)
        | None -> raise (Error "Unclosed list")
        | _ -> let x = recurse () in items (x :: acc)
      in
      items []
    | Some ')' -> raise (Error "Unexpected closing parenthesis")
    | Some '"' ->
      Buffer.clear buffer;
      let rec string_contents () =
        match get () with
        | None -> raise (Error "Unclosed string")
        | Some '"' ->
          if peek () = Some '"'
          then (ignore (get ()); Buffer.add_char buffer '"'; string_contents ())
          else string (Buffer.contents buffer)
        | Some c -> Buffer.add_char buffer c; string_contents ()
      in string_contents ()
    | Some '|' ->
      Buffer.clear buffer;
      let rec quoted () =
        match get () with
        | None -> raise (Error "Unclosed quoted symbol")
        | Some '|' -> atom ("|" ^ Buffer.contents buffer ^ "|")
        | Some c -> Buffer.add_char buffer c; quoted ()
      in quoted ()
    | Some c ->
      Buffer.clear buffer;
      Buffer.add_char buffer c;
      while (match peek () with
        | None | Some (' ' | '\t' | '\r' | '\n' | '(' | ')' | ';') -> false
        | Some c -> ignore (get ()); Buffer.add_char buffer c; true) do () done;
      atom (Buffer.contents buffer)
  in
  let count = ref 0 in
  let boundaries = Boundaries.create 16 in
  let remember ((expression, _) as value) =
    Boundaries.replace boundaries expression value;
    expression
  in
  let rec canonical expression =
    poll ();
    match expression with
    | Atom text -> atom text
    | String text -> string text
    | List children ->
      match Boundaries.find_opt boundaries expression with
      | Some value -> value
      | None -> list (List.map canonical children)
  in
  let unshared_atom text = incr count; Atom text in
  let unshared_string text = incr count; String text in
  let unshared_list children =
    incr count;
    if !count < 1024 then List children
    else remember (list (List.map canonical children))
  in
  let rec unshared () =
    if !count >= 1024 then remember (shared ())
    else expression unshared_atom unshared_string unshared_list unshared ()
  and shared () = expression atom string list shared () in
  fun () ->
    Fun.protect
      ~finally:(fun () ->
        if !next > 0 then begin
          Hashtbl.reset atoms;
          Hashtbl.reset strings;
          Lists.reset lists;
          Boundaries.reset boundaries;
          next := 0
        end;
        count := 0)
      (fun () ->
        skip ();
        match peek () with
        | None -> None
        | Some _ -> Some (unshared ()))

let channels = ref []

let read ?resource channel =
  let get, peek, r =
    match List.find_opt (fun (c, _, _, _) -> c == channel) !channels with
    | Some (_, get, peek, r) -> get, peek, r
    | None ->
      let buffer = Bytes.create 4096 in
      let position = ref 0 and length = ref 0 in
      let peek () =
        if !position = !length then begin
          position := 0;
          length := input channel buffer 0 (Bytes.length buffer)
        end;
        if !length = 0 then None else Some (Bytes.get buffer !position)
      in
      let get () =
        let c = peek () in
        if c <> None then incr position;
        c
      in
      let r = reader get peek in
      channels := (channel, get, peek, r) :: !channels;
      get, peek, r
  in
  let r = match resource with
    | None -> r
    | Some _ -> reader ?resource get peek in
  match r () with
  | None ->
    channels := List.filter (fun (c, _, _, _) -> c != channel) !channels;
    None
  | result -> result
  | exception (Error _ as exn) -> raise exn
  | exception exn ->
    channels := List.filter (fun (c, _, _, _) -> c != channel) !channels;
    raise exn

let parse ?resource ?(stop = fun _ -> false) source =
  let pos = ref 0 in
  let peek () = if !pos = String.length source then None else Some source.[!pos] in
  let get () = let c = peek () in if c <> None then incr pos; c in
  let read = reader ?resource get peek in
  let rec loop acc = match read () with
    | None -> List.rev acc
    | Some x ->
      let acc = x :: acc in
      if stop x then List.rev acc else loop acc
  in loop []
