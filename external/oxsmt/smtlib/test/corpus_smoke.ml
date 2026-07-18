(* Parse-only smoke test over a public benchmark corpus (make smtlib-corpus). Corpora live
   in ../corpora (a sibling, never in git), so this is a SEPARATE target from the
   committed round-trip test. Unsupported constructs are expected and merely counted; a
   crash (any exception other than the parser's own Malformed/Unsupported) is a bug and
   fails the run.

   Usage: corpus_smoke <corpus-dir> [--limit N] [--log PATH] Full per-file results go to
   the log; a digest prints to stdout. *)

module Parser = Oxsmt_smtlib_parser.Parser

let smt2_files dir =
  let rec walk acc dir =
    let entries =
      try Sys.readdir dir with
      | _ -> [||]
    in
    Array.sort String.compare entries;
    Array.fold_left
      (fun acc e ->
         let p = Filename.concat dir e in
         if
           try Sys.is_directory p with
           | _ -> false
         then walk acc p
         else if Filename.check_suffix p ".smt2"
         then p :: acc
         else acc)
      acc
      entries
  in
  List.sort String.compare (walk [] dir)
;;

let read_file p = In_channel.with_open_bin p In_channel.input_all

(* Bucket unsupported reasons by a short prefix key so the digest is compact. *)
let reason_key msg =
  let cut s =
    try String.sub s 0 (String.index s ':') with
    | Not_found -> s
  in
  let cut2 s =
    (* keep up to the second space-delimited word or the first paren *)
    match String.index_opt s '(' with
    | Some i when i > 0 -> String.trim (String.sub s 0 i)
    | _ -> s
  in
  cut2 (cut msg)
;;

(* Byte size of a file without reading it, for the resource cap below. *)
let file_size p =
  In_channel.with_open_bin p (fun ic -> Int64.to_int (In_channel.length ic))
;;

let () =
  let dir = ref "" in
  let limit = ref max_int in
  let log = ref "../logs/smtlib-corpus-smoke.log" in
  (* Resource guard: the parser builds terms eagerly, so a multi-MB instance (e.g. some
     QF_LIA files carry 200k+ nested let-bindings in tens of MB) can exhaust memory and
     endanger the box. Skip files above this cap; 20 MB keeps every QF_UFLIA file (max ~16
     MB) while excluding the pathological giants. *)
  let max_bytes = ref 20_000_000 in
  let rec args = function
    | [] -> ()
    | "--limit" :: n :: rest ->
      limit := int_of_string n;
      args rest
    | "--log" :: p :: rest ->
      log := p;
      args rest
    | "--max-bytes" :: n :: rest ->
      max_bytes := int_of_string n;
      args rest
    | d :: rest ->
      dir := d;
      args rest
  in
  args (List.tl (Array.to_list Sys.argv));
  if String.equal !dir ""
  then (
    prerr_endline
      "usage: corpus_smoke <corpus-dir> [--limit N] [--log PATH] [--max-bytes N]";
    exit 2);
  let files = smt2_files !dir in
  let files =
    if List.length files > !limit
    then List.filteri (fun i _ -> i < !limit) files
    else files
  in
  let ok = ref 0
  and malformed = ref 0
  and unsupported = ref 0
  and crashed = ref 0
  and skipped_large = ref 0 in
  let reasons : (string, int) Hashtbl.t = Hashtbl.create 32 in
  let bump k =
    Hashtbl.replace reasons k (1 + Option.value ~default:0 (Hashtbl.find_opt reasons k))
  in
  let oc = open_out !log in
  let t0 = Sys.time () in
  List.iter
    (fun path ->
       if file_size path > !max_bytes
       then (
         incr skipped_large;
         Printf.fprintf oc "SKIP-LARGE\t%d bytes\t%s\n" (file_size path) path)
       else (
         match Parser.parse (read_file path) with
         | parsed ->
           incr ok;
           Printf.fprintf oc "OK\t%d asserts\t%s\n" (List.length parsed.assertions) path
         | exception Parser.Malformed m ->
           incr malformed;
           bump ("MALFORMED " ^ reason_key m);
           Printf.fprintf oc "MALFORMED\t%s\t%s\n" m path
         | exception Parser.Unsupported m ->
           incr unsupported;
           bump ("UNSUPPORTED " ^ reason_key m);
           Printf.fprintf oc "UNSUPPORTED\t%s\t%s\n" m path
         | exception e ->
           incr crashed;
           Printf.fprintf oc "CRASH\t%s\t%s\n" (Printexc.to_string e) path))
    files;
  let dt = Sys.time () -. t0 in
  close_out oc;
  let total = List.length files in
  Printf.printf "corpus: %s\n" !dir;
  Printf.printf
    "files=%d  ok=%d  unsupported=%d  malformed=%d  skipped-large=%d  crashed=%d  (%.2fs \
     cpu)\n"
    total
    !ok
    !unsupported
    !malformed
    !skipped_large
    !crashed
    dt;
  let sorted =
    List.sort
      (fun (_, a) (_, b) -> Int.compare b a)
      (Hashtbl.fold (fun k v acc -> (k, v) :: acc) reasons [])
  in
  print_endline "top reasons:";
  List.iteri (fun i (k, v) -> if i < 15 then Printf.printf "  %6d  %s\n" v k) sorted;
  Printf.printf "full per-file log: %s\n" !log;
  (* A crash is a genuine bug; unsupported/malformed are expected corpus diversity. *)
  if !crashed > 0 then exit 1
;;
