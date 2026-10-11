(* Checks the flat hash table client's Lambda and Cmm, and the vacancy
   scan's Cmm; see flat_hashtbl_boundary.ml. Arguments: byte or native,
   then the directory of the dumps. *)
open Emitted_code

let lemmas = [ "count_bridge"; "lookup_agrees"; "count_agrees";
               "empty_view"; "put_view"; "erase_view"; "owner_def"; "bindings_def" ]

let () =
  let native = Sys.argv.(1) = "native" and dir = Sys.argv.(2) in
  let dump name = read (Filename.concat dir name) in
  let lambda = dump "client.lambda" in
  (* The generic client, before the instance with integer keys. *)
  let exercise =
    match find_all "\n   Key/" lambda with
    | (stop, _, _) :: _ -> String.sub lambda 0 stop
    | [] -> failwith "no Key module"
  in
  check
    (List.for_all (fun f -> occurs (f ^ "/") exercise)
       [ "replace_lookup"; "replace_length"; "update_framed" ])
    "the generic client is in the dump";
  check
    (not (occurs "(field_imm%s0%sV/" exercise
          || occurs "(field_imm%s1%sV/" exercise
          || occurs "(field_imm%s2%sV/" exercise
          || occurs "(field%s0%sV/" exercise || occurs "(field%s1%sV/" exercise
          || occurs "(field%s2%sV/" exercise))
    "the client reads none of the fields for Model and the ghost observers";
  check
    (not (List.exists (fun name -> occurs name exercise)
            (lemmas @ [ "caml_logical_map"; "routes"; "certificate" ])))
    "the client calls no logical map operation or lemma and builds no certificate";
  if native then begin
    check
      (occurs "p/%d[#()]" exercise)
      "permissions have the zero-width native layout";
    (* The lemmas' closures are fields of the table's module block, which
       the client builds when the functor is specialized in it; only calls
       count. *)
    List.iter
      (fun name ->
        let cmm = dump name in
        let callees = cmm_callees cmm in
        check
          (occurs "find_after_replace" cmm
           && not (occurs "caml_pref" cmm)
           && not
                (List.exists
                   (fun lemma ->
                     List.exists (fun callee -> occurs lemma callee) callees)
                   ("caml_logical_map" :: lemmas)))
          (name ^ " calls no ownership primitive, logical map operation or lemma"))
      [ "client.cmm"; "client-O3.cmm" ];
    (* At -O3 the table's functor instance is specialized: no call goes
       through a closure of the instance. *)
    check (not (occurs "caml_apply" (dump "client-O3.cmm")))
      "the -O3 client makes no generic application";
    check
      (List.exists (occurs "Vox_table_search__")
         (cmm_callees (dump "client-O3.cmm")))
      "the -O3 client calls the table's search directly";
    let vacancy = dump "vacancy.cmm" in
    let scan =
      let marker =
        match find_all "camlVox_table_vacancy__scan_%w%s(" vacancy with
        | (start, _, _) :: _ -> start
        | [] -> failwith "no scan function"
      in
      let rec back i =
        if String.sub vacancy i 9 = "(function" then i else back (i - 1)
      in
      balanced vacancy (back marker)
    in
    check (occurs ") : val*val\n" scan) "scan returns two unboxed scalars";
    check (not (occurs "(alloc" scan)) "scan does not allocate";
    check (count_word "rank/%d" scan = 1)
      "scan's rank is an unused ghost parameter";
    check (not (occurs "raise" scan)) "scan raises no exhaustion exception";
    check (not (occurs "compact" scan)) "scan does not compact"
  end;
  finish ()
