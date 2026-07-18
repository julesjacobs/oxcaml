(* LIA theory-leaf Lean gate (lean-proofs lane, rung 2). Drives real .smt2 files through
   the shipped Session with the certificate recorder installed; for every recorded LIA
   Farkas [Conflict] leaf, emits an explicit reflective-Farkas proof of [False] over
   {!Farkas_prelude} and runs the [lean] kernel:

   - the leaf proof must be ACCEPTED (exit 0);
   - a CORRUPTED copy (one multiplier bumped by +1) must be REJECTED (nonzero) — the
     discrimination arm proving a bad certificate cannot pass.

   Honest numbers: a leaf whose premises this rung cannot translate is UNSUPPORTED (a
   loud, counted gap), never a fake pass. Exit nonzero iff a valid leaf was rejected or a
   corrupted leaf was accepted (either is a soundness break). *)

module Session = Oxsmt_interface.Session
module Parser = Oxsmt_smtlib_parser.Parser
module Recorder = Oxsmt_certificate.Recorder
module Sat = Oxsmt_solver.Sat
module Term = Oxsmt_core.Term
module Lia_leaf = Oxsmt_lean_export.Lean_lia_leaf
module Rational = Oxsmt_lia.Rational

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))
;;

let write_file path s =
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> output_string oc s)
;;

let lean_bin () =
  match Sys.getenv_opt "OXSMT_LEAN" with
  | Some p -> p
  | None ->
    Filename.concat
      (try Sys.getenv "HOME" with
       | Not_found -> "")
      ".dispatch/bin/lean"
;;

let prelude_text () =
  let path =
    match Sys.getenv_opt "OXSMT_FARKAS_PRELUDE" with
    | Some p -> p
    | None -> "tests/lean/farkas_prelude.lean"
  in
  try read_file path with
  | _ -> failwith ("cannot read Farkas prelude at " ^ path ^ " (set OXSMT_FARKAS_PRELUDE)")
;;

(* Spawn lean with an in-process wall-clock cap. Returns (exit_ok, output). *)
let run_lean ~timeout ~src ~file : bool * string =
  let lean = lean_bin () in
  let out_file = file ^ ".out" in
  write_file file src;
  let out_fd =
    Unix.openfile out_file [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
  in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let pid = Unix.create_process lean [| lean; file |] devnull out_fd out_fd in
  Unix.close out_fd;
  Unix.close devnull;
  let start = Unix.gettimeofday () in
  let rec wait () =
    let p, status = Unix.waitpid [ Unix.WNOHANG ] pid in
    if p = 0
    then
      if Unix.gettimeofday () -. start > timeout
      then (
        (try Unix.kill pid Sys.sigkill with
         | _ -> ());
        (try ignore (Unix.waitpid [] pid) with
         | _ -> ());
        false, "TIMEOUT")
      else (
        Unix.sleepf 0.02;
        wait ())
    else (
      let ok =
        match status with
        | Unix.WEXITED 0 -> true
        | _ -> false
      in
      ( ok
      , try read_file out_file with
        | _ -> "" ))
  in
  wait ()
;;

(* Corrupt a witness by bumping the FIRST premise's multiplier by +1. *)
let corrupt_witness (w : Recorder.lia_conflict_witness) : Recorder.lia_conflict_witness =
  match w.Recorder.premises with
  | [] -> w
  | p :: rest ->
    let bumped =
      { p with Recorder.multiplier = Rational.add p.Recorder.multiplier Rational.one }
    in
    { Recorder.premises = bumped :: rest }
;;

let cnt_leaf = ref 0
let cnt_verified = ref 0
let cnt_broken = ref 0
let cnt_unsupported = ref 0

let process_file ~prelude ~timeout ~logdir path : unit =
  let base = Filename.basename path in
  match read_file path with
  | exception _ -> ()
  | src ->
    let s = Session.create ~max_effort:200_000 () in
    let rec_ = Recorder.create () in
    Session.install_cert_trace s (Some (Recorder.trace rec_));
    Session.install_leaf_certificate_trace
      s
      (Some
         { Oxsmt_interface.Cdclt.on_theory_atom =
             (fun ~var ~atom -> Recorder.record_theory_atom rec_ ~var ~atom)
         ; on_euf_leaf = (fun ~clause -> Recorder.record_euf_leaf rec_ ~clause)
         ; on_dt_distinctness =
             (fun ~registry ~clause ~left ~right ->
               Recorder.record_dt_distinctness rec_ ~registry ~clause ~left ~right)
         ; on_lia_conflict =
             (fun ~premise_lits ~multipliers ->
               Recorder.record_lia_conflict rec_ ~premise_lits ~multipliers)
         });
    (match
       Parser.parse_into
         ~internal_mint:(Session.parse_minter s)
         (Session.env s)
         (Session.context s)
         src
     with
     | exception _ -> ()
     | parsed ->
       Session.set_datatypes s parsed.Parser.datatypes;
       Session.set_arrays s parsed.Parser.arrays;
       (match Session.assert_presolved s parsed.Parser.assertions with
        | exception _ -> ()
        | () ->
          (match Session.check_sat s with
           | exception _ -> ()
           | Session.Sat | Session.Unknown -> ()
           | Session.Unsat ->
             (* build var -> atom resolver *)
             let atom_tbl = Hashtbl.create 64 in
             List.iter
               (fun (a : Recorder.atom_event) ->
                 Hashtbl.replace atom_tbl a.Recorder.var a.Recorder.atom)
               (Recorder.atoms rec_);
             let resolve v = Hashtbl.find_opt atom_tbl v in
             List.iteri
               (fun i (ev : Recorder.theory_event) ->
                 match ev.Recorder.lia_witness with
                 | None -> ()
                 | Some w ->
                   incr cnt_leaf;
                   let name =
                     Printf.sprintf
                       "leaf_%s_%d"
                       (String.map
                          (fun c ->
                            if (c >= 'a' && c <= 'z')
                               || (c >= 'A' && c <= 'Z')
                               || (c >= '0' && c <= '9')
                            then c
                            else '_')
                          base)
                       i
                   in
                   (match Lia_leaf.emit_lia_leaf ~resolve ~name ev w with
                    | exception Lia_leaf.Gap g ->
                      incr cnt_unsupported;
                      Printf.printf "UNSUPPORTED  %s#%d :: %s\n%!" base i g
                    | body ->
                      let src_ok = prelude ^ "\n" ^ body in
                      let f = Filename.concat logdir (name ^ ".lean") in
                      let ok, out = run_lean ~timeout ~src:src_ok ~file:f in
                      (* discrimination: corrupted witness must be rejected *)
                      let corrupt_ok =
                        match
                          Lia_leaf.emit_lia_leaf ~resolve ~name ev (corrupt_witness w)
                        with
                        | exception Lia_leaf.Gap _ ->
                          false (* couldn't build => treat as rejected *)
                        | cbody ->
                          let cf = Filename.concat logdir (name ^ ".corrupt.lean") in
                          fst (run_lean ~timeout ~src:(prelude ^ "\n" ^ cbody) ~file:cf)
                      in
                      if not ok
                      then (
                        incr cnt_broken;
                        Printf.printf
                          "BROKEN       %s#%d :: valid leaf REJECTED: %s\n%!"
                          base
                          i
                          (match
                             String.split_on_char '\n' out
                             |> List.find_opt (fun l -> String.length l > 0)
                           with
                           | Some l -> l
                           | None -> "?"))
                      else if corrupt_ok
                      then (
                        incr cnt_broken;
                        Printf.printf
                          "BROKEN       %s#%d :: CORRUPTED leaf ACCEPTED\n%!"
                          base
                          i)
                      else (
                        incr cnt_verified;
                        Printf.printf "VERIFIED     %s#%d\n%!" base i)))
               (Recorder.theory_clauses rec_))))
;;

let expand_arg arg =
  if Sys.file_exists arg && Sys.is_directory arg
  then
    Sys.readdir arg
    |> Array.to_list
    |> List.filter (fun e -> Filename.check_suffix e ".smt2")
    |> List.map (fun e -> Filename.concat arg e)
    |> List.sort compare
  else [ arg ]
;;

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let timeout, args =
    match args with
    | "--timeout" :: t :: rest -> float_of_string t, rest
    | _ -> 30.0, args
  in
  let logdir =
    match Sys.getenv_opt "OXSMT_LEAN_LOGDIR" with
    | Some d -> d
    | None -> Filename.get_temp_dir_name ()
  in
  (try Unix.mkdir logdir 0o755 with
   | _ -> ());
  let prelude = prelude_text () in
  let files = List.concat_map expand_arg args in
  List.iter (fun p -> process_file ~prelude ~timeout ~logdir p) files;
  Printf.printf
    "\nlean_leaf_gate: LIA leaves=%d | VERIFIED=%d BROKEN=%d UNSUPPORTED=%d\n%!"
    !cnt_leaf
    !cnt_verified
    !cnt_broken
    !cnt_unsupported;
  if !cnt_broken > 0 then exit 1
;;
