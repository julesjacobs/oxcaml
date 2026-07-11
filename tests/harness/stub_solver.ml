(* Stub solver implementing the harness's solver CLI contract (tests/README.md).

   Stands in for the real solver until it exists. It does no SMT reasoning: it scans the
   .smt2 file for (check-sat) / (check-sat-assuming) commands and, for each one, prints a
   single (result ...) block reporting `unknown` with zero counters. This exercises the
   full harness path (one verdict block per check-sat, including push/pop /
   multi-check-sat files) and keeps the fixture goldens green. When the real solver lands
   it implements this same CLI. *)

open Harness_lib

let () =
  let file =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else (
      prerr_endline "stub_solver: expected a .smt2 file argument";
      exit 2)
  in
  let text =
    let ic = open_in_bin file in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    s
  in
  let n_checks =
    match Sexp.parse_all text with
    | exception Sexp.Parse_error _ -> 0
    | sexps ->
        List.fold_left
          (fun acc sx ->
            match sx with
            | Sexp.List (Sexp.Atom ("check-sat" | "check-sat-assuming") :: _) ->
                acc + 1
            | _ -> acc)
          0 sexps
  in
  for _ = 1 to n_checks do
    print_string
      "(result (verdict unknown) (counters (conflicts 0) (decisions 0) \
       (propagations 0)))\n"
  done
