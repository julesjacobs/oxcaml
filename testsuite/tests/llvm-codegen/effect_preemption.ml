(* TEST
 macos;
 arch_arm64;
 include unix;
 hasunix;
 runtime5;
 flags += " -alert -unsafe_multidomain -w -21 -O3 -llvm-backend";
 native;
*)

open Effect
open Effect.Deep

external preempt_self : unit -> unit = "caml_domain_preempt_self" [@@noalloc]

let with_preemption_setup f =
  Unix.setitimer Unix.ITIMER_REAL { it_interval = 0.001; it_value = 0.001 }
  |> ignore;
  Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ()))
  |> ignore;
  Fun.protect f
    ~finally:(fun () ->
      Unix.setitimer Unix.ITIMER_REAL { it_interval = 0.; it_value = 0. }
      |> ignore)

let () =
  let preempted = ref false in
  let data = ref [] in
  with_preemption_setup (fun () ->
    try_with
      (fun () ->
        let start_at = Sys.time () in
        while not !preempted do
          if Sys.time () -. start_at > 5.
          then failwith "did not get preempted";
          data := [ ref 1; ref 2; ref 3; ref 4; ref 5 ] :: !data;
          if List.length !data > 100 then data := []
        done;
        List.iter
          (fun refs ->
            if List.length refs <> 5 then failwith "corrupt list";
            List.iter (fun r -> if !r < 1 then failwith "corrupt ref") refs)
          !data)
      ()
      { effc =
          (fun (type a) (e : a t) ->
            match e with
            | Preemption ->
              Some
                (fun (k : (a, _) continuation) ->
                  preempted := true;
                  Gc.full_major ();
                  continue k ())
            | _ -> None)
      })
