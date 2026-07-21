open Oxsmt_core

let failures = ref 0

let check name condition =
  if condition
  then Printf.printf "ok   %s\n" name
  else (
    incr failures;
    Printf.printf "FAIL %s\n" name)
;;

let env_on name =
  match Sys.getenv_opt name with
  | Some ("1" | "true" | "yes" | "on") -> true
  | Some _ | None -> false
;;

let () =
  let env = Env.create () in
  let ctx = Context.create env in
  let int_const name =
    Context.const ctx (Env.declare_fun env name (Rank.create [] Sort.int))
  in
  let eq = Context.eq ctx (int_const "x") (int_const "y") in
  let lazy_diseq_on = env_on "OXSMT_LAZY_INTERFACE_DISEQ" in
  let module U = Oxsmt_combine.Uflia_router in
  check
    "UFLIA negative equality follows the lazy-disequality gate"
    (match U.assert_to eq ~positive:false with
     | U.Both -> lazy_diseq_on
     | U.A -> not lazy_diseq_on
     | U.B -> false);
  let module D = Oxsmt_combine.Dtlia_router in
  check
    "DTLIA lazy disequality routing follows its independent gate"
    (match D.assert_to eq ~positive:false with
     | D.Both -> lazy_diseq_on
     | D.A -> not lazy_diseq_on
     | D.B -> false);
  check
    "DTLIA no-fabric mode retains sound lazy disequality handling"
    ((not D.fabric_disabled)
     || (not lazy_diseq_on)
     || D.assert_to eq ~positive:false = D.Both);
  if !failures > 0 then exit 1
;;
