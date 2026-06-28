(* TEST
 macos;
 arch_arm64;
 include unix;
 hasunix;
 runtime5;
 poll_insertion;
 flags += " -alert -unsafe_multidomain -w -21 -O3 -llvm-backend";
 native;
*)

open Effect
open Effect.Deep

external poll : unit -> unit = "%poll"

let run_with_preemption f effc =
  Domain.Tick.with_ ~interval_usec:1_000 (fun _ ->
    Preemptible.try_with ~on_tick:(fun () -> Preempt) f () { effc })

let () =
  let preempted = ref false in
  let data = ref [] in
  run_with_preemption
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
      (fun (type a) (e : a t) ->
        match e with
        | Preemption ->
          Some
            (fun (k : (a, _) continuation) ->
              preempted := true;
              Gc.full_major ();
              continue k ())
        | _ -> None)

let () =
  let preempted = ref false in
  let x = ref 0 in
  run_with_preemption
      (fun () ->
        let start_at = Sys.time () in
        while not !preempted do
          if Sys.time () -. start_at > 5.
          then failwith "did not get poll preempted";
          x := !x + 1;
          poll ()
        done;
        if !x <= 0 then failwith "poll loop did not run")
      (fun (type a) (e : a t) ->
        match e with
        | Preemption ->
          Some
            (fun (k : (a, _) continuation) ->
              preempted := true;
              continue k ())
        | _ -> None)
