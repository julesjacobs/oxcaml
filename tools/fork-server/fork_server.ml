(* Hardened warm fork-server for ocamlopt.opt.

   The parent execs once (paying exec + runtime init + GC frametable build) and NEVER
   compiles, so its global compiler state (Clflags/Env/persistent_env) stays pristine;
   every compile runs in a child forked from that clean warm snapshot. This is what makes
   re-entering the driver safe despite its "cannot call [main] twice per process"
   restriction.

   Production features over the Phase-1 prototype:
   - real stdin/stdout/stderr forwarding (client fds passed via SCM_RIGHTS, dup2'd in the
     child) so diagnostics are byte-identical to a direct run;
   - parallel children (fork-and-return, async SIGCHLD reaping, pid->conn map);
   - per-request environment applied in the child (clearenv + putenv);
   - OCAMLRUNPARAM guard: the daemon's GC is fixed at its own startup, so a request whose
     OCAMLRUNPARAM differs is refused (client falls back to exec);
   - compiler-hash handshake: mismatch -> refuse -> client falls back to exec;
   - lifecycle: SIGTERM shutdown, per-child timeout, kill child on client disconnect,
     parent survives child crashes;
   - single-domain fork-safety assertion. *)

external recvmsg_fds
  :  Unix.file_descr
  -> bytes
  -> int * Unix.file_descr array
  = "forksrv_recvmsg_fds"

external clearenv : unit -> unit = "forksrv_clearenv"
external peer_uid : Unix.file_descr -> int = "forksrv_peer_uid"

let fd_int (fd : Unix.file_descr) : int = Obj.magic fd

(* Cap on a single request's declared payload length: refuse anything larger so a
   hostile/buggy client cannot drive the daemon to OOM. Real compile commands (argv + full
   environ) are a few KB; 4 MB is generous. *)
let max_request = 4 * 1024 * 1024

(* How long a connection may take to deliver its request before we drop it, so a silent
   (slowloris) connector cannot wedge the single-threaded accept loop. A cooperative
   client sends its whole request in well under a millisecond. *)
let request_recv_timeout = 3.0

(* Per-request WALL-CLOCK deadline across the whole recv_request. SO_RCVTIMEO is per-recv,
   so a slow-dribble client (a byte every <timeout) resets it forever and wedges the
   single-threaded accept loop; this total cap defeats that. A cooperative client delivers
   its whole (KB-sized) request in one segment and never approaches it. *)
let request_deadline = 5.0

let dbg fmt =
  match Sys.getenv_opt "FORKSRV_DEBUG" with
  | Some p when p <> "" ->
    Printf.ksprintf
      (fun s ->
        try
          let oc = open_out_gen [ Open_append; Open_creat ] 0o644 p in
          output_string oc s;
          output_char oc '\n';
          close_out oc
        with
        | _ -> ())
      fmt
  | _ -> Printf.ksprintf (fun _ -> ()) fmt
;;

let compile argv =
  Optmaindriver.main
    (module Unix : Compiler_owee.Unix_intf.S)
    argv
    Format.err_formatter
    ~flambda2:Flambda2.lambda_to_cmm
;;

(* [Unix.waitpid] reports signals using OCaml's internal (negative) signal numbers;
   translate to the OS number so we can report the conventional 128+signum exit code (e.g.
   SIGKILL -> 137, SIGSEGV -> 139). *)
let os_signum s =
  if s = Sys.sigkill
  then 9
  else if s = Sys.sigsegv
  then 11
  else if s = Sys.sigterm
  then 15
  else if s = Sys.sigabrt
  then 6
  else if s = Sys.sigxcpu
  then 24
  else if s = Sys.sigint
  then 2
  else if s = Sys.sigfpe
  then 8
  else if s = Sys.sigbus
  then 7
  else 0
;;

(* ---- config, read once at startup ---- *)
let server_runparam =
  match Sys.getenv_opt "OCAMLRUNPARAM" with
  | Some s -> s
  | None -> ""
;;

(* OCAMLLIB (like OCAMLRUNPARAM) is consumed at compiler process-INIT: the parent caches
   Config.standard_library from it before any compile, so the child's per-request
   clearenv+putenv CANNOT change it. Any such init-time env var must be part of the
   handshake guard, otherwise a request with a different value would silently resolve the
   wrong stdlib instead of failing open. PATH is exempt (read fresh when the child spawns
   `as`). *)
let server_ocamllib =
  match Sys.getenv_opt "OCAMLLIB" with
  | Some s -> s
  | None -> ""
;;

(* Extract a variable's value from the request's "K=V" env list ("" if absent), for
   comparison against the daemon's init-time value. *)
let env_get env key =
  let pfx = key ^ "=" in
  let plen = String.length pfx in
  match
    List.find_opt (fun kv -> String.length kv >= plen && String.sub kv 0 plen = pfx) env
  with
  | Some kv -> String.sub kv plen (String.length kv - plen)
  | None -> ""
;;

let timeout_s =
  match Sys.getenv_opt "FORKSRV_TIMEOUT" with
  | Some s ->
    (try float_of_string s with
     | _ -> 3600.)
  | None -> 3600.
;;

let server_hash =
  match Sys.getenv_opt "FORKSRV_HASH" with
  | Some s when s <> "" -> s
  | _ ->
    (try Digest.to_hex (Digest.file "/proc/self/exe") with
     | _ -> "unknown")
;;

(* Optional child-side GC pacing: one-shot children can run a very lax space_overhead
   (fewer major collections -> fewer COW-dirtying header writes) without the
   long-lived-heap cost. Captured at startup (child env is wiped by clearenv before the
   compile). *)
let child_space_overhead =
  match Sys.getenv_opt "FORKSRV_CHILD_SO" with
  | Some s ->
    (try Some (int_of_string s) with
     | _ -> None)
  | None -> None
;;

(* ---- global fds the child must close ---- *)
let listen_fd = ref Unix.stdin
let sigchld_rd = ref Unix.stdin
let sigchld_wr = ref Unix.stdin

type child =
  { ctrl : Unix.file_descr
  ; deadline : float
  }

let children : (int, child) Hashtbl.t = Hashtbl.create 64

let reply fd s =
  try ignore (Unix.write_substring fd s 0 (String.length s) : int) with
  | _ -> ()
;;

let close_fd fd =
  try Unix.close fd with
  | _ -> ()
;;

let close_fds = Array.iter close_fd

(* Read the full framed request: 4-byte big-endian length, then that many payload bytes;
   SCM_RIGHTS fds arrive with the first segment. *)
let recv_request ctrl =
  let give_up_at = Unix.gettimeofday () +. request_deadline in
  let bufsz = 262144 in
  let buf = Bytes.create bufsz in
  let n, fds = recvmsg_fds ctrl buf in
  (* Any early-return path MUST close the SCM_RIGHTS fds we already received, else a
     stream of malformed/truncated requests leaks fds -> EMFILE. *)
  if n < 4
  then (
    close_fds fds;
    None)
  else (
    let b i = Char.code (Bytes.get buf i) in
    let len = (b 0 lsl 24) lor (b 1 lsl 16) lor (b 2 lsl 8) lor b 3 in
    if len < 0 || len > max_request
    then (
      close_fds fds;
      None)
    else (
      let body = Buffer.create (min len bufsz) in
      Buffer.add_subbytes body buf 4 (n - 4);
      (try
         while Buffer.length body < len do
           (* Total wall-clock cap regardless of per-recv progress: kills a slow-dribble
              client that would otherwise reset SO_RCVTIMEO. *)
           if Unix.gettimeofday () > give_up_at then raise Exit;
           let want = min bufsz (len - Buffer.length body) in
           let m =
             try Unix.read ctrl buf 0 want with
             | _ -> 0 (* incl. SO_RCVTIMEO EAGAIN -> treat as short read *)
           in
           if m <= 0 then raise Exit;
           Buffer.add_subbytes body buf 0 m
         done
       with
       | Exit -> ());
      if Buffer.length body < len
      then (
        close_fds fds;
        None)
      else Some (fds, Buffer.contents body)))
;;

let parse_body body =
  let f = Array.of_list (String.split_on_char '\000' body) in
  let tag = f.(0)
  and hash = f.(1)
  and runparam = f.(2)
  and cwd = f.(3) in
  let argc = int_of_string f.(4) in
  let args = Array.sub f 5 argc in
  let envc = int_of_string f.(5 + argc) in
  let env = Array.to_list (Array.sub f (6 + argc) envc) in
  tag, hash, runparam, cwd, args, env
;;

let run_child fds cwd env args ctrl =
  dbg
    "child ENTER pid=%d nfds=%d cwd=%s argc=%d args=[%s] envc=%d"
    (Unix.getpid ())
    (Array.length fds)
    cwd
    (Array.length args)
    (String.concat "|" (Array.to_list args))
    (List.length env);
  if Array.length fds >= 3
  then (
    Unix.dup2 fds.(0) Unix.stdin;
    Unix.dup2 fds.(1) Unix.stdout;
    Unix.dup2 fds.(2) Unix.stderr);
  Array.iter (fun fd -> if fd_int fd > 2 then close_fd fd) fds;
  close_fd ctrl;
  close_fd !listen_fd;
  close_fd !sigchld_rd;
  close_fd !sigchld_wr;
  (* Close inherited copies of OTHER in-flight requests' control sockets, so a sibling's
     client disconnect is still detectable by the parent and fds do not accumulate across
     concurrent children. *)
  Hashtbl.iter (fun _ c -> close_fd c.ctrl) children;
  if Sys.getenv_opt "FORKSRV_NOCLEARENV" = None then clearenv ();
  List.iter
    (fun kv ->
      match String.index_opt kv '=' with
      | Some i ->
        (try
           Unix.putenv
             (String.sub kv 0 i)
             (String.sub kv (i + 1) (String.length kv - i - 1))
         with
         | _ -> ())
      | None -> ())
    env;
  (* A failed chdir must be an error: compiling in the wrong directory would silently
     produce wrong output (relative -I/-o and DWARF comp_dir). *)
  (match Unix.chdir cwd with
   | () -> ()
   | exception e ->
     Printf.eprintf "fork-server: chdir %s failed: %s\n%!" cwd (Printexc.to_string e);
     exit 2);
  (match child_space_overhead with
   | Some n -> Gc.set { (Gc.get ()) with Gc.space_overhead = n }
   | None -> ());
  let code =
    try compile args with
    | _ -> 2
  in
  exit code
;;

let accept_one () =
  let ctrl, _ = Unix.accept !listen_fd in
  Unix.set_close_on_exec ctrl;
  (* Per-user daemon: reject any peer whose uid differs from ours. Combined with a 0700
     runtime dir + 0600 socket this closes the local-RCE surface (a foreign connector
     could otherwise supply a PATH pointing at a trojan `as`). The compiler-hash is a
     version check only, never access control. *)
  if peer_uid ctrl <> Unix.getuid ()
  then close_fd ctrl
  else (
    (try Unix.setsockopt_float ctrl Unix.SO_RCVTIMEO request_recv_timeout with
     | _ -> ());
    match
      try recv_request ctrl with
      | _ -> None
    with
    | None -> close_fd ctrl
    | Some (fds, body) ->
      (match parse_body body with
       | exception _ ->
         close_fds fds;
         close_fd ctrl
       | tag, hash, runparam, cwd, args, env ->
         if tag <> "FSRV1"
         then (
           close_fds fds;
           close_fd ctrl)
         else if hash <> server_hash
         then (
           reply ctrl (Printf.sprintf "HASHMISS=%s\n" server_hash);
           close_fds fds;
           close_fd ctrl)
         else if runparam <> server_runparam
         then (
           reply ctrl (Printf.sprintf "RUNPARAMMISS=%s\n" server_runparam);
           close_fds fds;
           close_fd ctrl)
         else if env_get env "OCAMLLIB" <> server_ocamllib
         then (
           (* Init-time cached stdlib path differs; refuse so the shim falls back to a
              direct exec rather than resolving the wrong stdlib. *)
           reply ctrl (Printf.sprintf "OCAMLLIBMISS=%s\n" server_ocamllib);
           close_fds fds;
           close_fd ctrl)
         else (
           dbg "parent PRE-FORK tag=%s argc=%d" tag (Array.length args);
           let pid = Unix.fork () in
           if pid = 0
           then run_child fds cwd env args ctrl
           else (
             close_fds fds;
             Hashtbl.replace
               children
               pid
               { ctrl; deadline = Unix.gettimeofday () +. timeout_s }))))
;;

let reap () =
  let rec loop () =
    match
      try Unix.waitpid [ Unix.WNOHANG ] (-1) with
      | _ -> 0, Unix.WEXITED 0
    with
    | 0, _ -> ()
    | pid, status ->
      (match status with
       | Unix.WEXITED c -> dbg "reap pid=%d WEXITED %d" pid c
       | Unix.WSIGNALED s -> dbg "reap pid=%d WSIGNALED %d" pid s
       | Unix.WSTOPPED s -> dbg "reap pid=%d WSTOPPED %d" pid s);
      (match Hashtbl.find_opt children pid with
       | Some { ctrl; _ } ->
         let code =
           match status with
           | Unix.WEXITED c -> c
           | Unix.WSIGNALED s -> 128 + os_signum s
           | Unix.WSTOPPED s -> 128 + os_signum s
         in
         reply ctrl (Printf.sprintf "EXIT=%d\n" code);
         close_fd ctrl;
         Hashtbl.remove children pid
       | None -> ());
      loop ()
  in
  loop ()
;;

let kill_owner_of fd =
  (* client disconnected on this control fd -> kill the compile in progress *)
  Hashtbl.iter
    (fun pid c ->
      if c.ctrl = fd
      then (
        try Unix.kill pid Sys.sigkill with
        | _ -> ()))
    children
;;

let enforce_timeouts () =
  let now = Unix.gettimeofday () in
  Hashtbl.iter
    (fun pid c ->
      if now > c.deadline
      then (
        try Unix.kill pid Sys.sigkill with
        | _ -> ()))
    children
;;

let shutting_down = ref false

let () =
  let sockpath = Sys.argv.(1) in
  (* Fork-safety invariant, asserted in the PARENT before we ever fork: the daemon must be
     single-domain (fork carries only the calling domain; any other running domain would
     leave orphaned locks in the child). We never spawn a domain, so this holds by
     construction; assert it to catch a future regression. There is no public
     running-domain count, so we check we are the main domain. *)
  if not (Domain.is_main_domain ())
  then (
    prerr_endline "fork-server: refusing to run off the main domain";
    exit 70);
  (try Unix.unlink sockpath with
   | _ -> ());
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> shutting_down := true));
  let rd, wr = Unix.pipe () in
  Unix.set_close_on_exec rd;
  Unix.set_close_on_exec wr;
  sigchld_rd := rd;
  sigchld_wr := wr;
  let onebyte = Bytes.make 1 'x' in
  Sys.set_signal
    Sys.sigchld
    (Sys.Signal_handle
       (fun _ ->
         try ignore (Unix.write !sigchld_wr onebyte 0 1 : int) with
         | _ -> ()));
  let srv = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.set_close_on_exec srv;
  Unix.setsockopt srv Unix.SO_REUSEADDR true;
  Unix.bind srv (Unix.ADDR_UNIX sockpath);
  (* Defence in depth on top of the SO_PEERCRED check: make the socket file itself
     unreachable to other users. The operator should also place it in a 0700 per-user
     runtime dir. *)
  (try Unix.chmod sockpath 0o600 with
   | _ -> ());
  Unix.listen srv 128;
  listen_fd := srv;
  Printf.eprintf
    "fork-server(hardened) ready sock=%s pid=%d hash=%s runparam=%S\n%!"
    sockpath
    (Unix.getpid ())
    server_hash
    server_runparam;
  let drain = Bytes.create 4096 in
  while not !shutting_down do
    (* The whole iteration is wrapped: no transient errno (accept, fork EAGAIN, waitpid
       EINTR, a bad connection) may ever escape and kill the daemon. *)
    try
      let ctrl_fds = Hashtbl.fold (fun _ c acc -> c.ctrl :: acc) children [] in
      let deadline = Hashtbl.fold (fun _ c acc -> min acc c.deadline) children infinity in
      let timeout =
        if deadline = infinity then 5.0 else max 0.05 (deadline -. Unix.gettimeofday ())
      in
      let rd_ready =
        try
          let r, _, _ =
            Unix.select (!listen_fd :: !sigchld_rd :: ctrl_fds) [] [] timeout
          in
          r
        with
        | Unix.Unix_error (Unix.EINTR, _, _) -> []
      in
      if List.mem !sigchld_rd rd_ready
      then (
        (try ignore (Unix.read !sigchld_rd drain 0 (Bytes.length drain) : int) with
         | _ -> ());
        reap ());
      if List.mem !listen_fd rd_ready
      then (
        try accept_one () with
        | _ -> ());
      List.iter
        (fun fd -> if fd <> !listen_fd && fd <> !sigchld_rd then kill_owner_of fd)
        rd_ready;
      enforce_timeouts ()
    with
    | _ -> ()
  done;
  (* clean shutdown *)
  Hashtbl.iter
    (fun pid _ ->
      try Unix.kill pid Sys.sigterm with
      | _ -> ())
    children;
  (try Unix.close !listen_fd with
   | _ -> ());
  (try Unix.unlink sockpath with
   | _ -> ());
  Printf.eprintf "fork-server(hardened) shut down\n%!"
;;
