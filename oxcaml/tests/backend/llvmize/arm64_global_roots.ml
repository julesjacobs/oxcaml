type root

external register : string -> root = "arm64_global_root_register"
external get : root -> string = "arm64_global_root_get"
external set : root -> string -> unit = "arm64_global_root_set"
external remove : root -> unit = "arm64_global_root_remove"

let size = 64

let roots = Array.init size (fun i -> register (string_of_int i))

let expected = Array.init size string_of_int

let check () =
  for i = 0 to size - 1 do
    if get roots.(i) <> expected.(i)
    then failwith ("bad root " ^ string_of_int i)
  done

let () =
  for round = 1 to 200 do
    if round mod 10 = 0 then Gc.full_major () else Gc.minor ();
    let i = round mod size in
    let v = string_of_int (round + i) in
    expected.(i) <- v;
    set roots.(i) v;
    check ()
  done;
  Array.iter remove roots;
  print_endline "ok"
