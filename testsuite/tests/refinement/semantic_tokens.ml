(* TEST
 readonly_files = "semantic_tokens_check.py";
 setup-ocamlc.byte-build-env;
 flags = "-vox-dump-vc-json tokens.json -c";
 compiler_output = "semantic_tokens.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/semantic_tokens_check.py \
           tokens.json ${test_source_directory}/semantic_tokens.ml";
 script;
*)

(* Per-identifier semantic tokens: proof/lemma calls and proof-value uses
   classify from typed totality/logicality metadata; ordinary imperative
   calls, arithmetic, and a same-spelling imperative function stay
   ordinary.  The companion check script pins each occurrence. *)

let key = Sys.opaque_identity 0

external key_law : unit -> unit{ key = 7 } @@ total = "%identity"

module Facts : sig
  val evidence : unit{ key = 7 }

  val lemma : int -> int -> unit{ key = 7 } @@ total
end = struct
  let evidence = key_law ()

  let (lemma @ total) (_a : int) (_b : int) : unit{ key = 7 } = key_law ()
end

let imperative_fn (x : int) = print_int x; x

let (local_lemma @ total) (_a : int) (_b : int) : unit{ key = 7 } =
  key_law ()

let use (y : int) =
  Facts.lemma (imperative_fn y) y;
  Facts.evidence;
  let alias = Facts.lemma in
  alias y y;
  let (partial @ total) = local_lemma y in
  partial y;
  y + 1

(* Same spelling as the lemma above, but imperative: stays ordinary. *)
let lemma (x : int) = print_int x; x

let shadowed (y : int) = ignore (lemma y); y
