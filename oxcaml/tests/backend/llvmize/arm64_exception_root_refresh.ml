let churn () =
  let rec loop n acc =
    if n = 0 then acc else loop (n - 1) (String.make 31 'x' :: acc)
  in
  ignore (loop 20_000 [])

let protect_like work always =
  try
    let result = work () in
    always ();
    result
  with exn ->
    always ();
    raise exn

let () =
  let count = ref 0 in
  let always () = incr count in
  let work () =
    churn ();
    raise Exit
  in
  (try ignore (protect_like work always) with Exit -> ());
  Printf.printf "%d\n" !count
