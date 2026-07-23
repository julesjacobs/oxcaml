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

type law = unit{ key = 7 }

external aliased_law : unit -> law @@ total = "%identity"

type nonnegative = int{ _ >= 0 }

external aliased_total : int -> nonnegative @@ total = "%identity"

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

let (total_zero @ total) (_x : int) : int{ _ >= 0 } = 0

let (total_truth @ total) (_x : int) : bool{ _ = true } = true

let (consume_nonnegative @ total) (_x : int{ _ >= 0 }) : int = 0

let (plain_predicate @ total) (_x : int) = true

let (total_zero_two @ total) (_x : int) (_y : int) : int{ _ >= 0 } = 0

let flat_alias = Facts.lemma

let nested_alias =
  let inner_alias = Facts.lemma in
  inner_alias

let use (y : int) =
  let _zero = total_zero y in
  let _truth = total_truth y in
  let _consumed = consume_nonnegative 0 in
  let _predicate = plain_predicate y in
  let _ordinary_function_value = total_zero in
  let ordinary_partial = total_zero_two y in
  let _ordinary_partial_result = ordinary_partial y in
  let _aliased_total = aliased_total y in
  aliased_law ();
  flat_alias y y;
  nested_alias y y;
  Facts.lemma (imperative_fn y) y;
  Facts.evidence;
  let alias = Facts.lemma in
  alias y y;
  let (partial @ total) = local_lemma y in
  partial y;
  y + 1

(* Same spelling as the lemma above, but partial and effectful: stays
   ordinary. *)
let lemma (x : int) = print_int x; x

let shadowed (y : int) = ignore (lemma y); y
