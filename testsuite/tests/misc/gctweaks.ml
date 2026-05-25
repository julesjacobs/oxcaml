(* TEST
 runtime5;
 {
   bytecode;
 }{
   native;
 }
*)

let () =
  let initial_active = Gc.Tweak.list_active () in
  (match Gc.Tweak.get "blorp" with
   | exception Invalid_argument _ -> ()
   | _ -> assert false);
  (match Gc.Tweak.set "blorp" 100 with
   | exception Invalid_argument _ -> ()
   | _ -> assert false);
  let def = Gc.Tweak.get "custom_work_max_multiplier" in
  Gc.Tweak.set "custom_work_max_multiplier" 100;
  Printf.printf "%d\n" (Gc.Tweak.get "custom_work_max_multiplier");
  let active = Gc.Tweak.list_active () in
  assert (List.mem ("custom_work_max_multiplier", 100) active);
  assert (
    List.filter
      (fun (name, _value) -> name <> "custom_work_max_multiplier")
      active
    = initial_active);
  Gc.Tweak.set "custom_work_max_multiplier" def;
  assert (Gc.Tweak.list_active () = initial_active);
  Printf.printf "ok\n";
  ()
