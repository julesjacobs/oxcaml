let rec f x =
  if not (x = 0 || x = 10_000 || x = 20_000)
  then 1 + f (x + 1)
  else
    try Sys.with_async_exns (fun () -> 1 + f (x + 1)) with Stack_overflow ->
      raise Stack_overflow

let run () =
  try
    Sys.with_async_exns (fun () -> ignore (f 0));
    false
  with Stack_overflow -> true

let () =
  let p = Sys.opaque_identity (ref 42) in
  Printf.printf "first=%b\n" (run ());
  for _ = 1 to 1_000 do
    ignore (Sys.opaque_identity (ref 1_000_000))
  done;
  Printexc.record_backtrace true;
  let second = run () in
  let backtrace = Printexc.get_backtrace () in
  Printf.printf "second=%b\n" second;
  Printf.printf "backtrace=%b\n" (String.length backtrace > 0);
  Printf.printf "p=%d\n" !p
