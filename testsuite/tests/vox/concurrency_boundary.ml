(* TEST
 has-z3;
 source_directories = "${test_source_directory}/../../../verification/library";
 set here = "${test_source_directory}";
 set lib = "";
 set library_flags = "-extension refinement_types -principal";
 library_flags += " -alert -do_not_spawn_domains";
 set client_flags = "-extension refinement_types";
 client_flags += " -alert -do_not_spawn_domains";
 readonly_files = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml";
 readonly_files += " raw_memory.mli raw_memory.ml";
 readonly_files += " verified_atomic.mli verified_atomic.ml";
 readonly_files += " unique_cell.mli unique_cell.ml";
 readonly_files += " one_shot.mli one_shot.ml";
 readonly_files += " channel_buffer.mli channel_buffer.ml";
 readonly_files += " spin_lock.mli spin_lock.ml";
 readonly_files += " unique_lock.mli unique_lock.ml";
 readonly_files += " reference_lock.mli reference_lock.ml";
 {
   setup-ocamlc.opt-build-env;
   lib = "${test_build_directory_prefix}/ocamlc.opt";
   flags = "${library_flags}";
   compile_only = "true";
   all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml";
   all_modules += " raw_memory.mli raw_memory.ml";
   all_modules += " verified_atomic.mli verified_atomic.ml";
   all_modules += " unique_cell.mli unique_cell.ml";
   ocamlc.opt;
   compiler_output2 = "${lib}/one_shot.dump";
   flags = "${library_flags} -dlambda";
   all_modules = "one_shot.mli one_shot.ml";
   ocamlc.opt;
   compiler_output2 = "${lib}/channel_buffer.dump";
   flags = "${library_flags} -dlambda";
   all_modules = "channel_buffer.mli channel_buffer.ml";
   ocamlc.opt;
   compiler_output2 = "${lib}/spin_lock.dump";
   flags = "${library_flags} -dlambda";
   all_modules = "spin_lock.mli spin_lock.ml";
   ocamlc.opt;
   compiler_output2 = "${lib}/unique_lock.dump";
   flags = "${library_flags} -dlambda";
   all_modules = "unique_lock.mli unique_lock.ml";
   ocamlc.opt;
   compiler_output2 = "${lib}/reference_lock.dump";
   flags = "${library_flags} -dlambda";
   all_modules = "reference_lock.mli reference_lock.ml";
   ocamlc.opt;
   flags = "${client_flags}";
   compiler_output2 = "${lib}/ocamlc.opt.output";
   src = "${lib}/pref.cmi ${lib}/ghost_pref.cmi";
   src += " ${lib}/raw_memory.cmi ${lib}/verified_atomic.cmi";
   src += " ${lib}/unique_cell.cmi ${lib}/one_shot.cmi";
   src += " ${lib}/channel_buffer.cmi ${lib}/spin_lock.cmi";
   src += " ${lib}/unique_lock.cmi";
   src += " ${lib}/reference_lock.cmi";
   dst = "${test_build_directory_prefix}/ocamlc.opt.public/";
   compiler_directory_suffix = ".public";
   unset module;
   all_modules = "one_shot_demo.ml one_shot_public_client.ml";
   all_modules += " atomic_lock.ml unique_lock_demo.ml";
   all_modules += " unique_lock_buffer_client.ml";
   readonly_files = "concurrency_boundary.ml emitted_code.ml";
   readonly_files += " concurrency_boundary_check.ml";
   readonly_files += " one_shot_demo.ml one_shot_public_client.ml";
   readonly_files += " atomic_lock.ml unique_lock_demo.ml";
   readonly_files += " unique_lock_buffer_client.ml channel_buffer_demo.ml";
   readonly_files += " reference_lock_parallel.ml unique_lock_parallel.ml";
   setup-ocamlc.opt-build-env;
   copy;
   compile_only = "false";
   compiler_output2 = "${lib}.public/clients.output";
   binary_modules = "${lib}/pref ${lib}/ghost_pref ${lib}/raw_memory";
   binary_modules += " ${lib}/verified_atomic ${lib}/unique_cell";
   binary_modules += " ${lib}/one_shot ${lib}/channel_buffer";
   binary_modules += " ${lib}/spin_lock ${lib}/unique_lock";
   binary_modules += " ${lib}/reference_lock";
   all_modules = "one_shot_demo.ml";
   program = "${lib}.public/one_shot_demo.exe";
   ocamlc.opt;
   output = "${lib}.public/one_shot_demo.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/one_shot_demo.reference";
   run;
   check-program-output;
   all_modules = "one_shot_public_client.ml";
   program = "${lib}.public/one_shot_public_client.exe";
   ocamlc.opt;
   output = "${lib}.public/one_shot_public_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/one_shot_public_client.reference";
   run;
   check-program-output;
   all_modules = "atomic_lock.ml";
   program = "${lib}.public/atomic_lock.exe";
   ocamlc.opt;
   output = "${lib}.public/atomic_lock.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/atomic_lock.reference";
   run;
   check-program-output;
   all_modules = "unique_lock_demo.ml";
   program = "${lib}.public/unique_lock_demo.exe";
   ocamlc.opt;
   output = "${lib}.public/unique_lock_demo.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/unique_lock_demo.reference";
   run;
   check-program-output;
   all_modules = "unique_lock_buffer_client.ml";
   program = "${lib}.public/unique_lock_buffer_client.exe";
   ocamlc.opt;
   output = "${lib}.public/unique_lock_buffer_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/unique_lock_buffer_client.reference";
   run;
   check-program-output;
   check-ocamlc.opt-output;
   unset binary_modules;
   flags = "";
   compiler_output2 = "${lib}.public/checker.output";
   all_modules = "emitted_code.ml concurrency_boundary_check.ml";
   program = "${lib}.public/check.exe";
   ocamlc.opt;
   arguments = "${lib}";
   output = "${lib}.public/check.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/concurrency_boundary.checks.reference";
   run;
   check-program-output;
   flags = "${client_flags}";
   binary_modules = "${lib}/pref ${lib}/ghost_pref ${lib}/raw_memory";
   binary_modules += " ${lib}/verified_atomic ${lib}/unique_cell";
   binary_modules += " ${lib}/one_shot ${lib}/channel_buffer";
   binary_modules += " ${lib}/spin_lock ${lib}/unique_lock";
   binary_modules += " ${lib}/reference_lock";
   binary_modules += " unique_lock_demo unique_lock_buffer_client";
   run-expect;
   check-program-output;
   flags = "${client_flags}";
   compiler_output2 = "${lib}.public/parallel.output";
   binary_modules = "${lib}/pref ${lib}/ghost_pref ${lib}/raw_memory";
   binary_modules += " ${lib}/verified_atomic ${lib}/unique_cell";
   binary_modules += " ${lib}/one_shot ${lib}/channel_buffer";
   binary_modules += " ${lib}/spin_lock ${lib}/unique_lock";
   binary_modules += " ${lib}/reference_lock";
   all_modules = "channel_buffer_demo.ml";
   program = "${lib}.public/channel_buffer_demo.exe";
   ocamlc.opt;
   all_modules = "reference_lock_parallel.ml";
   program = "${lib}.public/reference_lock_parallel.exe";
   ocamlc.opt;
   all_modules = "unique_lock_parallel.ml";
   program = "${lib}.public/unique_lock_parallel.exe";
   binary_modules += " unique_lock_demo";
   ocamlc.opt;
   check-ocamlc.opt-output;
 }
 {
   setup-ocamlopt.opt-build-env;
   lib = "${test_build_directory_prefix}/ocamlopt.opt";
   flags = "${library_flags}";
   compile_only = "true";
   all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml";
   all_modules += " raw_memory.mli raw_memory.ml";
   all_modules += " verified_atomic.mli verified_atomic.ml";
   all_modules += " unique_cell.mli unique_cell.ml";
   ocamlopt.opt;
   compiler_output2 = "${lib}/one_shot.dump";
   flags = "${library_flags} -dlambda -dcmm";
   all_modules = "one_shot.mli one_shot.ml";
   ocamlopt.opt;
   compiler_output2 = "${lib}/channel_buffer.dump";
   flags = "${library_flags} -dlambda -dcmm";
   all_modules = "channel_buffer.mli channel_buffer.ml";
   ocamlopt.opt;
   compiler_output2 = "${lib}/spin_lock.dump";
   flags = "${library_flags} -dlambda -dcmm";
   all_modules = "spin_lock.mli spin_lock.ml";
   ocamlopt.opt;
   compiler_output2 = "${lib}/unique_lock.dump";
   flags = "${library_flags} -dlambda -dcmm";
   all_modules = "unique_lock.mli unique_lock.ml";
   ocamlopt.opt;
   compiler_output2 = "${lib}/reference_lock.dump";
   flags = "${library_flags} -dlambda -dcmm";
   all_modules = "reference_lock.mli reference_lock.ml";
   ocamlopt.opt;
   flags = "${client_flags}";
   compiler_output2 = "${lib}/ocamlopt.opt.output";
   src = "${lib}/pref.cmi ${lib}/ghost_pref.cmi";
   src += " ${lib}/raw_memory.cmi ${lib}/verified_atomic.cmi";
   src += " ${lib}/unique_cell.cmi ${lib}/one_shot.cmi";
   src += " ${lib}/channel_buffer.cmi ${lib}/spin_lock.cmi";
   src += " ${lib}/unique_lock.cmi";
   src += " ${lib}/reference_lock.cmi ${lib}/pref.cmx";
   src += " ${lib}/ghost_pref.cmx ${lib}/raw_memory.cmx";
   src += " ${lib}/verified_atomic.cmx ${lib}/unique_cell.cmx";
   src += " ${lib}/one_shot.cmx ${lib}/channel_buffer.cmx";
   src += " ${lib}/spin_lock.cmx ${lib}/unique_lock.cmx";
   src += " ${lib}/reference_lock.cmx";
   dst = "${test_build_directory_prefix}/ocamlopt.opt.public/";
   compiler_directory_suffix = ".public";
   unset module;
   all_modules = "one_shot_demo.ml one_shot_public_client.ml";
   all_modules += " atomic_lock.ml unique_lock_demo.ml";
   all_modules += " unique_lock_buffer_client.ml";
   readonly_files = "concurrency_boundary.ml emitted_code.ml";
   readonly_files += " concurrency_boundary_check.ml";
   readonly_files += " one_shot_demo.ml one_shot_public_client.ml";
   readonly_files += " atomic_lock.ml unique_lock_demo.ml";
   readonly_files += " unique_lock_buffer_client.ml channel_buffer_demo.ml";
   readonly_files += " reference_lock_parallel.ml unique_lock_parallel.ml";
   setup-ocamlopt.opt-build-env;
   copy;
   compile_only = "false";
   compiler_output2 = "${lib}.public/clients.output";
   binary_modules = "${lib}/pref ${lib}/ghost_pref ${lib}/raw_memory";
   binary_modules += " ${lib}/verified_atomic ${lib}/unique_cell";
   binary_modules += " ${lib}/one_shot ${lib}/channel_buffer";
   binary_modules += " ${lib}/spin_lock ${lib}/unique_lock";
   binary_modules += " ${lib}/reference_lock";
   all_modules = "one_shot_demo.ml";
   program = "${lib}.public/one_shot_demo.exe";
   ocamlopt.opt;
   output = "${lib}.public/one_shot_demo.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/one_shot_demo.reference";
   run;
   check-program-output;
   all_modules = "one_shot_public_client.ml";
   program = "${lib}.public/one_shot_public_client.exe";
   ocamlopt.opt;
   output = "${lib}.public/one_shot_public_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/one_shot_public_client.reference";
   run;
   check-program-output;
   all_modules = "atomic_lock.ml";
   program = "${lib}.public/atomic_lock.exe";
   ocamlopt.opt;
   output = "${lib}.public/atomic_lock.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/atomic_lock.reference";
   run;
   check-program-output;
   all_modules = "unique_lock_demo.ml";
   program = "${lib}.public/unique_lock_demo.exe";
   ocamlopt.opt;
   output = "${lib}.public/unique_lock_demo.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/unique_lock_demo.reference";
   run;
   check-program-output;
   all_modules = "unique_lock_buffer_client.ml";
   program = "${lib}.public/unique_lock_buffer_client.exe";
   ocamlopt.opt;
   output = "${lib}.public/unique_lock_buffer_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/unique_lock_buffer_client.reference";
   run;
   check-program-output;
   check-ocamlopt.opt-output;
   unset binary_modules;
   flags = "";
   compiler_output2 = "${lib}.public/checker.output";
   all_modules = "emitted_code.ml concurrency_boundary_check.ml";
   program = "${lib}.public/check.exe";
   ocamlopt.opt;
   arguments = "${lib}";
   output = "${lib}.public/check.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/concurrency_boundary.checks.reference";
   run;
   check-program-output;
   flags = "${client_flags}";
   compiler_output2 = "${lib}.public/parallel.output";
   binary_modules = "${lib}/pref ${lib}/ghost_pref ${lib}/raw_memory";
   binary_modules += " ${lib}/verified_atomic ${lib}/unique_cell";
   binary_modules += " ${lib}/one_shot ${lib}/channel_buffer";
   binary_modules += " ${lib}/spin_lock ${lib}/unique_lock";
   binary_modules += " ${lib}/reference_lock";
   all_modules = "channel_buffer_demo.ml";
   program = "${lib}.public/channel_buffer_demo.exe";
   ocamlopt.opt;
   all_modules = "reference_lock_parallel.ml";
   program = "${lib}.public/reference_lock_parallel.exe";
   ocamlopt.opt;
   all_modules = "unique_lock_parallel.ml";
   program = "${lib}.public/unique_lock_parallel.exe";
   binary_modules += " unique_lock_demo";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
 }
*)

(* The boundary of the one-shot channels and the reference locks. With
   each compiler, the ten library units are compiled with -principal, and
   concurrency_boundary_check.ml checks that the Lambda (and, natively, the
   Cmm) of the channel and lock libraries has no ghost primitive: heap
   operations, token split and join, an atomic's invariant key or a cell's
   location. The public clients are compiled against the libraries' .cmi
   files only (and .cmx files for native code), linked and run, except that
   the three that spawn domains are only linked: they need a multidomain
   runtime, and their own tests run them. The programs below are rejected
   against the same interfaces. *)

(* A positive control. *)
let make_lock () = Reference_lock.make 0;;
[%%expect{|
val make_lock : unit -> Reference_lock.t = <fun>
|}]

(* The unique-lock fixtures below use this instance. *)
module L = Unique_lock.Make(Unique_lock_demo.Data);;
[%%expect{|
module L :
  sig
    type t = Unique_lock.Make(Unique_lock_demo.Data).t
    type contents = Unique_lock_demo.Data.model option
    type 'a step =
      'a Unique_lock.Make(Unique_lock_demo.Data).step = {
      value : 'a;
      state : contents Ghost_pref.token @@ ghost;
    }
    val location :
      t @ local immutable -> contents Ghost_pref.t @ immutable ghost @@ total
    val owned :
      t @ immutable -> contents Ghost_pref.heap @ immutable -> bool @ ghost
      @@ total
    val owned_def :
      (a : t) @ immutable ->
      (h : contents Ghost_pref.heap) @ immutable ->
      {u : unit
        | (owned a h) ===
            (ghost_
               (let p = location a in
                match Ghost_pref.Heap.at h p with
                | Some (Some x) ->
                    h ===
                      (Ghost_pref.Heap.put (Ghost_pref.Heap.empty ()) p
                         (Some x))
                | _ -> false))}
      @@ total
    val make : Unique_lock_demo.Data.t @ unique total -> t @@ portable
    val try_acquire :
      (a : t) ->
      {r : (bool, contents) Ghost_pref.step
        | if r.Ghost_pref.value
          then owned a (Ghost_pref.own r.Ghost_pref.state)
          else
            (Ghost_pref.own r.Ghost_pref.state) ===
              (Ghost_pref.Heap.empty ())} @ unique
      @@ portable
    val release :
      (a : t) ->
      {t : contents Ghost_pref.token | owned a (Ghost_pref.own t)} @ unique
      ghost ->
      {t : contents Ghost_pref.token
        | (Ghost_pref.own t) === (Ghost_pref.Heap.empty ())} @ unique
      ghost @@ portable
    val take :
      (a : t) ->
      (token : {t : contents Ghost_pref.token
                 | match Ghost_pref.Heap.at (Ghost_pref.own t) (location a)
                   with
                   | Some (Some _) -> true
                   | _ -> false}) @ unique
      ghost ->
      {r : Unique_lock_demo.Data.t step
        | ((Ghost_pref.Heap.at (Ghost_pref.own token) (location a)) ===
             (Some (Some (Unique_lock_demo.Data.snapshot r.value))))
            &&
            ((Ghost_pref.own r.state) ===
               (Ghost_pref.Heap.put (Ghost_pref.own token) (location a) None))} @ unique
      @@ portable
    val put :
      (a : t) ->
      (value : Unique_lock_demo.Data.t) @ unique ->
      (token : {t : contents Ghost_pref.token
                 | (Ghost_pref.Heap.at (Ghost_pref.own t) (location a)) ===
                     (Some None)}) @ unique
      ghost ->
      {t : contents Ghost_pref.token
        | (Ghost_pref.own t) ===
            (Ghost_pref.Heap.put (Ghost_pref.own token) (location a)
               (Some (Unique_lock_demo.Data.snapshot value)))} @ unique
      ghost @@ portable
  end
|}]

(* channel-buffer-value *)
let bad (r : Channel_buffer.filled @ unique) :
    {n : int | n <> r.expected} =
  let n, _ = Channel_buffer.read_receipt r in n;;
[%%expect{|
Line 3, characters 46-47:
3 |   let n, _ = Channel_buffer.read_receipt r in n;;
                                                  ^
Error: Refinement could not be proved (counterexample)
Line 2, characters 15-30:
2 |     {n : int | n <> r.expected} =
                   ^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

(* channel-hidden *)
module A = One_shot.A;;
[%%expect{|
Line 1, characters 11-21:
1 | module A = One_shot.A;;
               ^^^^^^^^^^
Error: Unbound module "One_shot.A"
|}]

(* channel-reuse *)
let bad tx =
  One_shot.send tx 1;
  One_shot.send tx 2;;
[%%expect{|
Line 3, characters 16-18:
3 |   One_shot.send tx 2;;
                    ^^
Error: This value is used here, but it has already been used as unique at:
Line 2, characters 16-18:
2 |   One_shot.send tx 1;
                    ^^

|}]

(* channel-value *)
let bad () =
  let (tx, _ : {n : int | n = 42} One_shot.send *
      {n : int | n = 42} One_shot.recv) = One_shot.create () in
  One_shot.send tx 0;;
[%%expect{|
Line 4, characters 19-20:
4 |   One_shot.send tx 0;;
                       ^
Error: Refinement could not be proved (counterexample)
Line 2, characters 26-32:
2 |   let (tx, _ : {n : int | n = 42} One_shot.send *
                              ^^^^^^
  The refinement is stated here.
|}]

(* empty-release *)
let bad a = Reference_lock.release a (Ghost_pref.empty ());;
[%%expect{|
Line 1, characters 37-58:
1 | let bad a = Reference_lock.release a (Ghost_pref.empty ());;
                                         ^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
File "reference_lock.mli", line 21, characters 30-56:
  The refinement is stated here.
|}]

(* empty-take *)
let bad a = L.take a (Ghost_pref.empty ());;
[%%expect{|
Line 1, characters 21-42:
1 | let bad a = L.take a (Ghost_pref.empty ());;
                         ^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
File "unique_lock.mli", lines 35-36, characters 6-42:
  The refinement is stated here.
|}]

(* failed-acquire *)
let bad a =
  let r = Reference_lock.try_acquire a in
  if not r.value then Reference_lock.read_owned a (borrow_ r.state) else 0;;
[%%expect{|
Line 3, characters 59-66:
3 |   if not r.value then Reference_lock.read_owned a (borrow_ r.state) else 0;;
                                                               ^^^^^^^
Error: Refinement could not be proved (counterexample)
File "reference_lock.mli", line 24, characters 35-61:
  The refinement is stated here.
|}]

(* false-value *)
let bad : (a : Reference_lock.t) ->
    {t : int Ghost_pref.token | Reference_lock.owned a (Ghost_pref.own t)}
      @ local read ghost -> {n : int | n < 0} =
  fun a t -> Reference_lock.read_owned a t;;
[%%expect{|
Line 4, characters 13-42:
4 |   fun a t -> Reference_lock.read_owned a t;;
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 3, characters 39-44:
3 |       @ local read ghost -> {n : int | n < 0} =
                                           ^^^^^
  The refinement is stated here.
|}]

(* hidden *)
module A = Reference_lock.A;;
[%%expect{|
Line 1, characters 11-27:
1 | module A = Reference_lock.A;;
               ^^^^^^^^^^^^^^^^
Error: Unbound module "Reference_lock.A"
|}]

(* payload-reuse *)
module Buffer_lock = Unique_lock_buffer_client.L
let bad (x : Unique_lock_buffer_client.Data.t @ unique) =
  let _ = Buffer_lock.make x in
  Buffer_lock.make x;;
[%%expect{|
module Buffer_lock = Unique_lock_buffer_client.L
Line 4, characters 19-20:
4 |   Buffer_lock.make x;;
                       ^
Error: This value is used here, but it has already been used as unique at:
Line 3, characters 27-28:
3 |   let _ = Buffer_lock.make x in
                               ^

|}]

(* reuse-release *)
let bad a =
  let r = Reference_lock.try_acquire a in
  if r.value then begin
    let _ = Reference_lock.release a r.state in
    Reference_lock.read_owned a (borrow_ r.state)
  end else 0;;
[%%expect{|
Line 5, characters 32-49:
5 |     Reference_lock.read_owned a (borrow_ r.state)
                                    ^^^^^^^^^^^^^^^^^
Error: This value is borrowed here,
       but it has already been used as unique at:
Line 4, characters 37-44:
4 |     let _ = Reference_lock.release a r.state in
                                         ^^^^^^^

|}]

(* unique-hidden *)
module A = L.A;;
[%%expect{|
Line 1, characters 11-14:
1 | module A = L.A;;
               ^^^
Error: Unbound module "L.A"
|}]

(* unique-release-empty *)
let bad a =
  let r = L.try_acquire a in
  if r.value then begin
    ghost_ (L.owned_def a (Ghost_pref.own (borrow_ r.state)));
    let taken = L.take a r.state in
    let _ = L.release a taken.state in ()
  end;;
[%%expect{|
Line 6, characters 24-35:
6 |     let _ = L.release a taken.state in ()
                            ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
File "unique_lock.mli", line 30, characters 37-63:
  The refinement is stated here.
|}]

(* unique-token-reuse *)
let bad a =
  let r = L.try_acquire a in
  if r.value then begin
    ghost_ (L.owned_def a (Ghost_pref.own (borrow_ r.state)));
    let _ = L.take a r.state in
    let _ = L.take a r.state in ()
  end;;
[%%expect{|
Line 6, characters 21-28:
6 |     let _ = L.take a r.state in ()
                         ^^^^^^^
Error: This value is used here, but it has already been used as unique at:
Line 5, characters 21-28:
5 |     let _ = L.take a r.state in
                         ^^^^^^^

|}]

(* wrong-lock *)
let bad a b =
  let r = Reference_lock.try_acquire a in
  if r.value then let _ = Reference_lock.release b r.state in ();;
[%%expect{|
Line 3, characters 51-58:
3 |   if r.value then let _ = Reference_lock.release b r.state in ();;
                                                       ^^^^^^^
Error: Refinement could not be proved (counterexample)
File "reference_lock.mli", line 21, characters 30-56:
  The refinement is stated here.
|}]
