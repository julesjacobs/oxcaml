(* TEST
 include unix;
 include ocamlcommon;
*)

(* The persistent solver-result store, exercised directly against
   [Vox_backend.Cached] rather than through a compilation.

   These assertions used to sit at the end of [standalone_smt.ml], behind a
   pre-existing in-process oxsmt assertion that aborts the program long
   before reaching them, so the suite ran none of them and the store's own
   fixture was a claim rather than evidence.  They are here on their own so
   that the suite covers the store rather than the store's neighbours. *)

open Types

module R = Types.Refinement

let next_type_id = ref 40_000

let fresh_type_id () =
  incr next_type_id;
  !next_type_id

let arrow argument result =
  create_expr
    (Tarrow
       ( (Nolabel, Mode.Alloc.legacy, Mode.Alloc.legacy, None),
         argument,
         result,
         commu_ok ))
    ~level:0
    ~scope:0
    ~id:(fresh_type_id ())

let int_type = Predef.type_int
let bool_type = Predef.type_bool
let stdlib_path = Path.Pident (Ident.create_persistent "Stdlib")
let loc = Location.in_file "solver_cache_store.ml"

let env =
  Compmisc.init_path ();
  Compmisc.initial_env ()

let node type_ rexp_desc = R.create ~loc ~type_ rexp_desc
let int value = node int_type (Rexp_constant (Const_int value))
let bound binder = node binder.rb_type (Rexp_ident (Rbound binder.rb_id))

let bool value =
  node bool_type
    (Rexp_construct
       ( { rconstr_type_path = Predef.path_bool;
           rconstr_name = if value then "true" else "false";
         },
         [] ))

let primitive type_ name =
  let path = Path.Pdot (stdlib_path, name) in
  node type_ (Rexp_ident (Rfree (Rapp path)))

let apply type_ function_ arguments =
  node type_
    (Rexp_apply
       (function_, List.map (fun argument -> Nolabel, argument) arguments))

let binary name argument_type result_type left right =
  let function_type = arrow argument_type (arrow argument_type result_type) in
  apply result_type (primitive function_type name) [left; right]

let equal type_ left right = binary "=" type_ bool_type left right
let add left right = binary "+" int_type int_type left right
let subtract left right = binary "-" int_type int_type left right
let multiply left right = binary "*" int_type int_type left right
let greater left right = binary ">" int_type bool_type left right
let less_equal left right = binary "<=" int_type bool_type left right

let conjunction left right = binary "&&" bool_type bool_type left right
let disjunction left right = binary "||" bool_type bool_type left right

let negate argument =
  apply bool_type (primitive (arrow bool_type bool_type) "not") [argument]

let test_origin =
  Vox_vc.{ kind = "test"; name = Some "solver_cache_store"; span = Some loc }

let fact expression =
  Vox_vc.
    { expression; location = Some loc; scope = None; origin = test_origin }

let vc ?(facts = []) goal = Vox_vc.create ~loc ~facts ~goal

let x =
  { rb_id = Ident.create_scoped ~scope:1 "x";
    rb_type = int_type;
  }

let arithmetic_and_booleans =
  let x_value = equal int_type (bound x) (int 4) in
  let arithmetic =
    less_equal
      (subtract (multiply (add (bound x) (int 2)) (int 3)) (int 1))
      (int 20)
  in
  let booleans =
    disjunction (negate (bool false))
      (conjunction (bool true) (greater (bound x) (int (-1))))
  in
  vc ~facts:[fact x_value] (conjunction arithmetic booleans)

let shell_command script = "/bin/sh -c " ^ Filename.quote script

let persistent_contents =
  "(set-option :produce-unsat-cores true)\n\
   (declare-const shared Int)\n\
   (assert (= shared 0))\n\
   (check-sat)\n\
   (get-unsat-core)\n"

external unset_environment_variable : string -> bool
  = "caml_vox_unset_environment_variable"

let remove_path path =
  match Sys.readdir path with
  | exception Sys_error _ -> if Sys.file_exists path then Sys.remove path
  | entries ->
    Array.iter
      (fun basename -> Sys.remove (Filename.concat path basename))
      entries;
    Sys.rmdir path

(* Where the scratch goes.

   The store refuses a directory that anyone else could write to, anywhere
   along the path to it, and refuses one that is not 0700 and owned by the
   caller.  ocamltest's working directory will not do: it is created under
   the ambient umask, which on a group-writable build tree makes every
   component 0775.  Nor will [Filename.temp_file], which falls back to the
   shared system temporary directory when TMPDIR is unset, and nothing here
   should ever land there.

   So try the roots in order and ask the store itself which one it will
   accept, rather than guessing.  [cache_bucket_path] returns [None] for
   exactly the directories the store declines. *)
let store_accepts path =
  let saved_enabled = Sys.getenv_opt "VOX_SOLVER_CACHE" in
  let saved_directory = Sys.getenv_opt "VOX_SOLVER_CACHE_DIR" in
  let restore name = function
    | Some value -> Unix.putenv name value
    | None -> assert (unset_environment_variable name)
  in
  Unix.putenv "VOX_SOLVER_CACHE" "1";
  Unix.putenv "VOX_SOLVER_CACHE_DIR" path;
  let accepted =
    Option.is_some (Vox_backend.cache_bucket_path Vox_backend.Z3)
  in
  restore "VOX_SOLVER_CACHE" saved_enabled;
  restore "VOX_SOLVER_CACHE_DIR" saved_directory;
  accepted

let scratch_root =
  let candidates =
    [ Sys.getenv_opt "VOX_SOLVER_CACHE_TEST_ROOT";
      Sys.getenv_opt "TMPDIR";
      Some (Sys.getcwd ());
    ]
    |> List.filter_map Fun.id
    |> List.filter (fun root -> not (String.equal root ""))
  in
  let accepts root =
    let path = Filename.concat root "solver-cache-store-probe" in
    remove_path path;
    match Sys.mkdir path 0o700 with
    | exception Sys_error _ -> None
    | () ->
      let accepted = store_accepts path in
      remove_path path;
      if accepted then Some root else None
  in
  match List.find_map accepts candidates with
  | Some root -> root
  | None ->
    prerr_endline
      "solver_cache_store: no candidate scratch root is private enough for \
       the solver store; point TMPDIR at a directory only you can write to";
    exit 2

(* Distinct per process, so the bytecode and native runs of this test cannot
   collide, nor can two lanes sharing one scratch root. *)
let scratch_path name =
  Filename.concat scratch_root
    ("solver-cache-store-" ^ string_of_int (Unix.getpid ()) ^ "-" ^ name)

let make_scratch_directory name =
  let path = scratch_path name in
  remove_path path;
  Sys.mkdir path 0o700;
  path

let cache_test_directory = scratch_path "store"

let clear_cache_test_directory () = remove_path cache_test_directory

module Cache_test_backend = struct
  let backend = Vox_backend.Z3
  let capabilities = Vox_backend.capabilities backend
  let calls = Atomic.make 0

  let cache_key ~command (obligation : Vox_backend.obligation) =
    match Vox_smt.emit ~query:Vox_smt.Prove ~env:obligation.Vox_backend.env
            obligation.condition
    with
    | Error _ -> None
    | Ok payload ->
      Some
        (String.concat "|"
           [ "test-schema";
             Vox_backend.string_of_backend backend;
             Option.value command ~default:"none";
             payload;
           ])

  let discharge ~command (obligation : Vox_backend.obligation) =
    ignore (Atomic.fetch_and_add calls 1);
    let verdict =
      match command with
      | Some "failure" -> Vox_backend.Solver_error
      | Some "disproved" -> Vox_backend.Disproved
      | Some "not-proved" -> Vox_backend.Not_proved
      | Some "unknown" -> Vox_backend.Unknown
      | Some "solver-error" -> Vox_backend.Solver_error
      | Some "unavailable" -> Vox_backend.Unavailable
      | Some _ | None -> Vox_backend.Proved
    in
    { Vox_backend.backend;
      capabilities;
      verdict;
      location = obligation.condition.location;
      detail =
        (match command with
         | Some "metadata" -> Some "preserved detail"
         | Some _ | None -> None);
      unused_facts =
        (match command with
         | Some "metadata" -> Some [1]
         | Some _ | None -> Some []);
    }
end

module Cache_test_backend_again = struct
  include Cache_test_backend
end

module Cached_test = Vox_backend.Cached (Cache_test_backend)
module Cached_test_again = Vox_backend.Cached (Cache_test_backend_again)

module Other_cache_test_backend = struct
  include Cache_test_backend
  let backend = Vox_backend.Oxsmt
  let capabilities = Vox_backend.capabilities backend

  let cache_key ~command obligation =
    Option.map (fun key -> "other-backend|" ^ key)
      (Cache_test_backend.cache_key ~command obligation)
end

module Other_cached_test = Vox_backend.Cached (Other_cache_test_backend)

let cache_test_obligation ?prove_contents condition =
  Vox_backend.{ env; condition; prove_contents }

let cache_discriminate =
  shell_command
    "if grep -Fq '(assert (= false true))'; then printf 'unsat\\n'; \
     else printf 'sat\\n'; fi"

let cache_test_environment =
  [ "VOX_SOLVER_CACHE";
    "VOX_SOLVER_CACHE_DIR";
    "VOX_SOLVER_CACHE_COMPILER_IDENTITY";
    "VOX_Z3_SOLVER_VERSION";
    "VOX_OXSMT_LEGACY_EXTERNAL";
    "VOX_SOLVER_CACHE_MAX_BYTES";
    "VOX_LEAN";
    "OXSMT_ASSUMPTION_PREPROCESS";
    "OXSMT_LAZY_INTERFACE_DISEQ";
    "OXSMT_BINARY_INTERFACE_EQ";
    "OXSMT_COMBINE_INSEARCH";
    "OXSMT_DIRECT_TERM_ITE";
    "OXSMT_ASSUMPTION_FAST_COMPLEMENTS";
    "OXSMT_ASSUMPTION_PREPROCESS_PROPFOLD";
    "OXSMT_NIA";
    "PATH";
  ]

external file_stamp : string -> string = "caml_vox_file_stamp"

let restore_environment saved =
  List.iter
    (fun (name, value) ->
      match value with
      | Some value -> Unix.putenv name value
      | None -> assert (unset_environment_variable name))
    saved

let restore_environment_variable saved name =
  match List.assoc name saved with
  | Some value -> Unix.putenv name value
  | None -> assert (unset_environment_variable name)

let write_private_file filename contents =
  let channel = open_out_bin filename in
  output_string channel contents;
  close_out channel;
  Unix.chmod filename 0o600

let string_contains ~needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec loop index =
    index + needle_length <= haystack_length
    &&
    (String.equal (String.sub haystack index needle_length) needle
     || loop (index + 1))
  in
  loop 0

let cache_test_record ?(verdict = "p") key =
  let body =
    String.concat " "
      [ Digest.BLAKE256.to_hex (Digest.BLAKE256.string key);
        verdict;
        "u";
        "-";
      ]
  in
  let checksum =
    String.sub (Digest.BLAKE128.to_hex (Digest.BLAKE128.string body)) 0 16
  in
  body ^ " " ^ checksum ^ "\n"

let corrupt_cache_test_checksum record =
  let index = String.length record - 2 in
  let bytes = Bytes.of_string record in
  Bytes.set bytes index (if Bytes.get bytes index = '0' then '1' else '0');
  Bytes.unsafe_to_string bytes

(* Every result of one backend for one compiled source file shares a single
   log, so the tests below overwrite that one file rather than a file per
   key. *)
let cache_test_log () =
  Option.get (Vox_backend.cache_bucket_path Vox_backend.Z3)

let () =
  let saved_environment =
    List.map (fun name -> name, Sys.getenv_opt name) cache_test_environment
  in
  clear_cache_test_directory ();
  Sys.mkdir cache_test_directory 0o700;
  Unix.putenv "VOX_SOLVER_CACHE" "1";
  Unix.putenv "VOX_SOLVER_CACHE_DIR" cache_test_directory;
  Unix.putenv "VOX_Z3_SOLVER_VERSION" "test-solver-v1";
  Unix.putenv "VOX_OXSMT_LEGACY_EXTERNAL" "0";
  Fun.protect
    ~finally:(fun () ->
      clear_cache_test_directory ();
      (* Named here as well as where they are used, so that a failing
         assertion leaves nothing behind in the scratch root. *)
      remove_path (scratch_path "outside-target");
      remove_path (scratch_path "existing-target");
      restore_environment saved_environment)
    (fun () ->
      let obligation = cache_test_obligation arithmetic_and_booleans in
      let first = Cached_test.discharge ~command:(Some "same") obligation in
      let second =
        Cached_test_again.discharge ~command:(Some "same") obligation
      in
      assert (first.verdict = Vox_backend.Proved);
      assert (second.verdict = Vox_backend.Proved);
      assert (Atomic.get Cache_test_backend.calls = 1);
      let changed_goal = cache_test_obligation (vc (bool true)) in
      ignore (Cached_test.discharge ~command:(Some "same") changed_goal);
      let changed_facts =
        cache_test_obligation (vc ~facts:[fact (bool true)] (bool true))
      in
      ignore (Cached_test.discharge ~command:(Some "same") changed_facts);
      ignore (Cached_test.discharge ~command:(Some "option") obligation);
      ignore (Other_cached_test.discharge ~command:(Some "same") obligation);
      assert (Atomic.get Cache_test_backend.calls = 5);
      let log_count () =
        Array.fold_left
          (fun count basename ->
            if Filename.check_suffix basename ".log" then count + 1
            else count)
          0
          (Sys.readdir cache_test_directory)
      in
      (* Four results of one backend share one log; the second backend keeps
         a log of its own. *)
      assert (log_count () = 2);
      let failing =
        Cached_test.discharge ~command:(Some "failure") obligation
      in
      let failing_again =
        Cached_test.discharge ~command:(Some "failure") obligation
      in
      assert (failing.verdict = Vox_backend.Solver_error);
      assert (failing_again.verdict = Vox_backend.Solver_error);
      assert (Atomic.get Cache_test_backend.calls = 7);
      let corrupt_path = cache_test_log () in
      let channel = open_out_bin corrupt_path in
      output_string channel "not a cache entry";
      close_out channel;
      ignore (Cached_test.discharge ~command:(Some "corrupt") obligation);
      assert (Atomic.get Cache_test_backend.calls = 8);
      (* A link left at the log's name, dangling and resolving.

         The dangling case is the one that needs a test rather than an
         argument: [Sys.file_exists] follows symlinks and so reports a
         dangling one absent, and a writer that asked it first would then
         create and append to the link's target -- a file outside the store,
         chosen by whatever left the link.  Both cases must miss, must leave
         an ordinary private log behind, and must leave the target alone. *)
      let outside_target = scratch_path "outside-target" in
      let expect_link_replaced ~command ~target =
        let log = cache_test_log () in
        if Sys.file_exists log then Sys.remove log;
        Unix.symlink target log;
        let before = Atomic.get Cache_test_backend.calls in
        ignore (Cached_test.discharge ~command:(Some command) obligation);
        assert (Atomic.get Cache_test_backend.calls = before + 1);
        assert ((Unix.lstat log).st_kind = Unix.S_REG)
      in
      if Sys.file_exists outside_target then Sys.remove outside_target;
      expect_link_replaced ~command:"dangling-link" ~target:outside_target;
      assert (not (Sys.file_exists outside_target));
      let existing_target = scratch_path "existing-target" in
      write_private_file existing_target "untouched";
      expect_link_replaced ~command:"resolving-link" ~target:existing_target;
      let untouched =
        let channel = open_in_bin existing_target in
        Fun.protect
          ~finally:(fun () -> close_in_noerr channel)
          (fun () -> In_channel.input_all channel)
      in
      assert (String.equal untouched "untouched");
      Sys.remove existing_target;
      (* Concurrent appends into one log.  This was two domains, which the
         runtime on some machines refuses to spawn at all -- and where it
         does spawn only one, the rendezvous the sub-test needed hung rather
         than failed.  Forked writers have their own address space and their
         own descriptor, so they exercise the O_APPEND interleaving the
         store actually depends on. *)
      let concurrent_writers = 4 in
      let concurrent_records = 4 in
      let concurrent_obligation writer index =
        cache_test_obligation
          (vc
             ~facts:[fact (equal int_type (bound x) (int writer))]
             (equal int_type (int index) (int index)))
      in
      let start_signal = Filename.concat cache_test_directory "start" in
      let children =
        List.init concurrent_writers (fun writer ->
          match Unix.fork () with
          | 0 ->
            let status =
              try
                while not (Sys.file_exists start_signal) do
                  ignore (Unix.select [] [] [] 0.001)
                done;
                for index = 0 to concurrent_records - 1 do
                  let result =
                    Cached_test.discharge ~command:(Some "parallel")
                      (concurrent_obligation writer index)
                  in
                  assert (result.verdict = Vox_backend.Proved)
                done;
                0
              with _ -> 1
            in
            (* [_exit], so that a child never flushes the parent's buffers
               or runs the cleanup the parent owns. *)
            Unix._exit status
          | child -> child)
      in
      write_private_file start_signal "";
      List.iter
        (fun child ->
          match Unix.waitpid [] child with
          | _, Unix.WEXITED 0 -> ()
          | _, (Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _) ->
            assert false)
        children;
      Sys.remove start_signal;
      (* Every record the writers appended is found again by a reader that
         had already parsed this log before they ran, so no append was lost
         to interleaving and the stamp check noticed the ones that landed
         behind its back. *)
      let before_concurrent = Atomic.get Cache_test_backend.calls in
      for writer = 0 to concurrent_writers - 1 do
        for index = 0 to concurrent_records - 1 do
          let found =
            Cached_test.discharge ~command:(Some "parallel")
              (concurrent_obligation writer index)
          in
          assert (found.verdict = Vox_backend.Proved)
        done
      done;
      assert (Atomic.get Cache_test_backend.calls = before_concurrent);
      let expect_repeated_calls ~command ~verdict ~calls =
        let before = Atomic.get Cache_test_backend.calls in
        let first =
          Cached_test.discharge ~command:(Some command) obligation
        in
        let second =
          Cached_test.discharge ~command:(Some command) obligation
        in
        assert (first.verdict = verdict);
        assert (second.verdict = verdict);
        assert (Atomic.get Cache_test_backend.calls = before + calls)
      in
      expect_repeated_calls ~command:"disproved"
        ~verdict:Vox_backend.Disproved ~calls:1;
      List.iter
        (fun (command, verdict) ->
          expect_repeated_calls ~command ~verdict ~calls:2)
        [ "not-proved", Vox_backend.Not_proved;
          "unknown", Vox_backend.Unknown;
          "solver-error", Vox_backend.Solver_error;
          "unavailable", Vox_backend.Unavailable;
        ];
      Fun.protect
        ~finally:(fun () -> Unix.chmod cache_test_directory 0o700)
        (fun () ->
          Unix.chmod cache_test_directory 0o755;
          expect_repeated_calls ~command:"public-directory"
            ~verdict:Vox_backend.Proved ~calls:2);
      let public_entry_key =
        Option.get
          (Cache_test_backend.cache_key ~command:(Some "public-entry") obligation)
      in
      let public_entry_path = cache_test_log () in
      write_private_file public_entry_path
        (cache_test_record public_entry_key);
      Unix.chmod public_entry_path 0o644;
      let before_public_entry = Atomic.get Cache_test_backend.calls in
      let public_entry =
        Cached_test.discharge ~command:(Some "public-entry") obligation
      in
      assert (public_entry.verdict = Vox_backend.Proved);
      assert (Atomic.get Cache_test_backend.calls = before_public_entry + 1);
      let metadata_obligation = cache_test_obligation arithmetic_and_booleans in
      let metadata_location = Location.in_file "cache-hit-location.ml" in
      let relocated_condition =
        { arithmetic_and_booleans with location = metadata_location }
      in
      let relocated_obligation = cache_test_obligation relocated_condition in
      let metadata_first =
        Cached_test.discharge ~command:(Some "metadata") metadata_obligation
      in
      let metadata_cached =
        Cached_test.discharge ~command:(Some "metadata") relocated_obligation
      in
      assert (metadata_first.detail = Some "preserved detail");
      assert (metadata_cached.detail = Some "preserved detail");
      assert (metadata_cached.unused_facts = Some [1]);
      assert (metadata_cached.location = metadata_location);
      let expect_corrupt_miss command contents =
        write_private_file (cache_test_log ()) contents;
        let before = Atomic.get Cache_test_backend.calls in
        ignore (Cached_test.discharge ~command:(Some command) obligation);
        assert (Atomic.get Cache_test_backend.calls = before + 1)
      in
      let checksum_key =
        Option.get
          (Cache_test_backend.cache_key ~command:(Some "checksum") obligation)
      in
      expect_corrupt_miss "checksum"
        (corrupt_cache_test_checksum (cache_test_record checksum_key));
      let verdict_key =
        Option.get
          (Cache_test_backend.cache_key ~command:(Some "verdict") obligation)
      in
      expect_corrupt_miss "verdict"
        (cache_test_record ~verdict:"x" verdict_key);
      expect_corrupt_miss "oversized"
        (String.make ((8 * 1024 * 1024) + 1) 'x');
      let mismatch_target =
        Option.get
          (Cache_test_backend.cache_key ~command:(Some "mismatch") obligation)
      in
      expect_corrupt_miss "mismatch"
        (cache_test_record (mismatch_target ^ "-different"));
      let resynchronize_key =
        Option.get
          (Cache_test_backend.cache_key ~command:(Some "resynchronize")
             obligation)
      in
      let intact = cache_test_record resynchronize_key in
      write_private_file (cache_test_log ())
        (String.sub intact 0 (String.length intact / 2) ^ "\n" ^ intact);
      let before_resynchronize = Atomic.get Cache_test_backend.calls in
      ignore
        (Cached_test.discharge ~command:(Some "resynchronize") obligation);
      (* The truncated line is skipped and the intact one behind it is read,
         so an interleaved append costs its own record and no other. *)
      assert
        (Atomic.get Cache_test_backend.calls = before_resynchronize);
      Unix.putenv "VOX_SOLVER_CACHE_MAX_BYTES" "1";
      let before_eviction = Atomic.get Cache_test_backend.calls in
      ignore (Cached_test.discharge ~command:(Some "eviction") obligation);
      ignore (Cached_test.discharge ~command:(Some "eviction") obligation);
      assert (Atomic.get Cache_test_backend.calls = before_eviction + 2);
      restore_environment_variable saved_environment
        "VOX_SOLVER_CACHE_MAX_BYTES";
      let first_write_directory =
        make_scratch_directory "first-write-eviction"
      in
      Fun.protect
        ~finally:(fun () ->
          Unix.putenv "VOX_SOLVER_CACHE_DIR" cache_test_directory;
          restore_environment_variable saved_environment
            "VOX_SOLVER_CACHE_MAX_BYTES";
          Array.iter
            (fun basename ->
              Sys.remove (Filename.concat first_write_directory basename))
            (Sys.readdir first_write_directory);
          Sys.rmdir first_write_directory)
        (fun () ->
          let abandoned =
            Filename.concat first_write_directory "compact-abandoned.tmp"
          in
          let foreign = Filename.concat first_write_directory "z-foreign.log" in
          write_private_file abandoned (String.make (700 * 1024) 'a');
          write_private_file foreign (String.make (700 * 1024) 'b');
          Unix.putenv "VOX_SOLVER_CACHE_DIR" first_write_directory;
          Unix.putenv "VOX_SOLVER_CACHE_MAX_BYTES" (string_of_int (1024 * 1024));
          ignore
            (Cached_test.discharge ~command:(Some "first-write-eviction")
               obligation);
          assert (not (Sys.file_exists abandoned));
          assert (Sys.file_exists foreign));
      let z3_obligation condition prove_contents =
        Vox_backend.{ env; condition; prove_contents = Some prove_contents }
      in
      Unix.putenv "VOX_SOLVER_CACHE_COMPILER_IDENTITY" "test-build-a";
      let z3_key_a =
        Vox_backend.Z3_backend.cache_key ~command:(Some cache_discriminate)
          (z3_obligation (vc (bool false)) persistent_contents)
      in
      let compiler_executable =
        if Filename.is_implicit Sys.executable_name then
          Sys.getenv "PATH"
          |> String.split_on_char ':'
          |> List.find_map (fun directory ->
            let candidate = Filename.concat directory Sys.executable_name in
            try
              let status = Unix.stat candidate in
              if status.st_kind <> Unix.S_REG then None
              else begin
                Unix.access candidate [Unix.X_OK];
                Some candidate
              end
            with Unix.Unix_error _ -> None)
          |> Option.get
        else Sys.executable_name
      in
      (* The compiler is identified by its filesystem stamp rather than by a
         digest of its 42 MB of content, which cost 0.10s an invocation. *)
      let compiler_identity =
        compiler_executable ^ ":" ^ file_stamp compiler_executable
      in
      assert (not (String.equal (file_stamp compiler_executable) ""));
      assert
        (match z3_key_a with
         | Some key -> string_contains ~needle:compiler_identity key
         | None -> false);
      Unix.putenv "VOX_SOLVER_CACHE_COMPILER_IDENTITY" "test-build-b";
      let z3_key_b =
        Vox_backend.Z3_backend.cache_key ~command:(Some cache_discriminate)
          (z3_obligation (vc (bool false)) persistent_contents)
      in
      assert (z3_key_a <> z3_key_b);
      let no_custom =
        Vox_backend.{ env; condition = vc (bool false); prove_contents = None }
      in
      let custom_none =
        Vox_backend.
          { env; condition = vc (bool false); prove_contents = Some "none" }
      in
      assert
        (Vox_backend.Z3_backend.cache_key ~command:(Some cache_discriminate) no_custom
         <> Vox_backend.Z3_backend.cache_key ~command:(Some cache_discriminate)
              custom_none);
      assert (unset_environment_variable "VOX_Z3_SOLVER_VERSION");
      List.iter
        (fun command ->
          assert
            (Vox_backend.Z3_backend.cache_key ~command:(Some command) no_custom
             = None))
        [ "env X=1 z3 -in";
          "sh -c solver";
          "'z3' -in";
          "z3\\ -in";
          "timeout 25 z3 -in";
          "python3 mysolver.py";
        ];
      let shadow_directory = make_scratch_directory "solver-shadow" in
      let actual_directory = make_scratch_directory "solver-actual" in
      let shadow_solver = Filename.concat shadow_directory "z3" in
      let actual_solver = Filename.concat actual_directory "z3" in
      Fun.protect
        ~finally:(fun () ->
          if Sys.file_exists shadow_solver then Sys.remove shadow_solver;
          if Sys.file_exists actual_solver then Sys.remove actual_solver;
          Sys.rmdir shadow_directory;
          Sys.rmdir actual_directory)
        (fun () ->
          write_private_file shadow_solver "not executable";
          write_private_file actual_solver "first";
          Unix.chmod actual_solver 0o700;
          Unix.putenv "PATH" (shadow_directory ^ ":" ^ actual_directory);
          let path_key_a =
            Vox_backend.Z3_backend.cache_key ~command:(Some "z3 -in") no_custom
          in
          let replacement =
            Filename.temp_file ~temp_dir:actual_directory "replacement-" ""
          in
          write_private_file replacement "other";
          Unix.chmod replacement 0o700;
          Sys.rename replacement actual_solver;
          let path_key_b =
            Vox_backend.Z3_backend.cache_key ~command:(Some "z3 -in") no_custom
          in
          assert (Option.is_some path_key_a);
          assert (Option.is_some path_key_b);
          assert (path_key_a <> path_key_b));
      restore_environment_variable saved_environment "PATH";
      let original_directory = Sys.getcwd () in
      let lean_working_directory =
        make_scratch_directory "lean-working"
      in
      let lean_actual_directory = make_scratch_directory "lean-actual" in
      let working_lean = Filename.concat lean_working_directory "lean" in
      let actual_lean = Filename.concat lean_actual_directory "lean" in
      Fun.protect
        ~finally:(fun () ->
          Sys.chdir original_directory;
          if Sys.file_exists working_lean then Sys.remove working_lean;
          if Sys.file_exists actual_lean then Sys.remove actual_lean;
          Sys.rmdir lean_working_directory;
          Sys.rmdir lean_actual_directory)
        (fun () ->
          write_private_file working_lean "cwd decoy";
          Unix.chmod working_lean 0o700;
          write_private_file actual_lean "first";
          Unix.chmod actual_lean 0o700;
          Unix.putenv "VOX_LEAN" "lean";
          Unix.putenv "PATH" lean_actual_directory;
          Sys.chdir lean_working_directory;
          let lean_key_a =
            Vox_backend.Lean_backend.cache_key ~command:None obligation
          in
          let replacement =
            Filename.temp_file ~temp_dir:lean_actual_directory "replacement-" ""
          in
          write_private_file replacement "other";
          Unix.chmod replacement 0o700;
          Sys.rename replacement actual_lean;
          let lean_key_b =
            Vox_backend.Lean_backend.cache_key ~command:None obligation
          in
          assert (Option.is_some lean_key_a);
          assert (Option.is_some lean_key_b);
          assert (lean_key_a <> lean_key_b));
      restore_environment_variable saved_environment "VOX_LEAN";
      restore_environment_variable saved_environment "PATH";
      Unix.putenv "VOX_Z3_SOLVER_VERSION" "test-solver-v1";
      assert
        (Option.is_some
           (Vox_backend.Z3_backend.cache_key ~command:(Some "sh -c solver")
              no_custom));
      let replaceable_solver =
        Filename.concat cache_test_directory "z3"
      in
      write_private_file replaceable_solver "first";
      Unix.chmod replaceable_solver 0o700;
      let solver_key_a =
        Vox_backend.Z3_backend.cache_key
          ~command:(Some replaceable_solver) no_custom
      in
      let replacement =
        Filename.temp_file ~temp_dir:cache_test_directory "replacement-" ""
      in
      write_private_file replacement "other";
      Unix.chmod replacement 0o700;
      Sys.rename replacement replaceable_solver;
      let solver_key_b =
        Vox_backend.Z3_backend.cache_key
          ~command:(Some replaceable_solver) no_custom
      in
      Sys.remove replaceable_solver;
      assert (Option.is_some solver_key_a);
      assert (Option.is_some solver_key_b);
      assert (solver_key_a <> solver_key_b);
      Unix.putenv "VOX_SOLVER_CACHE_COMPILER_IDENTITY" "test-build-a";
      let discharge_custom condition =
        Vox_backend.discharge
          ~selection:(Vox_backend.Single Vox_backend.Z3)
          ~smt_solver:(Some cache_discriminate) ~oxsmt_solver:None
          ~prove_contents:persistent_contents ~env condition
      in
      let disproved = discharge_custom (vc (bool false)) in
      let not_proved = discharge_custom (vc (bool true)) in
      assert (disproved.verdict = Vox_backend.Disproved);
      assert (not_proved.verdict = Vox_backend.Not_proved);
      let oxsmt_obligation prove_contents =
        Vox_backend.{ env; condition = arithmetic_and_booleans; prove_contents }
      in
      let oxsmt_key_a =
        Vox_backend.Oxsmt_backend.cache_key ~command:None
          (oxsmt_obligation (Some "unused custom text a"))
      in
      let oxsmt_key_b =
        Vox_backend.Oxsmt_backend.cache_key ~command:None
          (oxsmt_obligation (Some "unused custom text b"))
      in
      assert (oxsmt_key_a = oxsmt_key_b);
      assert
        (match oxsmt_key_a with
         | Some key ->
           string_contains
             ~needle:"036b29692b057e98df701df6a1517991f4d98cdd"
             key
         | None -> false);
      List.iter
        (fun name ->
          let toggled =
            match List.assoc name saved_environment with
            | Some "cache-key-probe" -> "cache-key-probe-2"
            | Some _ | None -> "cache-key-probe"
          in
          Unix.putenv name toggled;
          let changed =
            Vox_backend.Oxsmt_backend.cache_key ~command:None
              (oxsmt_obligation None)
          in
          restore_environment_variable saved_environment name;
          let restored =
            Vox_backend.Oxsmt_backend.cache_key ~command:None
              (oxsmt_obligation None)
          in
          assert (changed <> restored))
        [ "OXSMT_ASSUMPTION_PREPROCESS";
          "OXSMT_LAZY_INTERFACE_DISEQ";
          "OXSMT_BINARY_INTERFACE_EQ";
          "OXSMT_COMBINE_INSEARCH";
          "OXSMT_DIRECT_TERM_ITE";
          "OXSMT_ASSUMPTION_FAST_COMPLEMENTS";
          "OXSMT_ASSUMPTION_PREPROCESS_PROPFOLD";
          "OXSMT_NIA";
        ]);
  print_endline
    "solver cache: persistent hits, exact inputs, invalidation, corruption, failures, and concurrent writes checked"
