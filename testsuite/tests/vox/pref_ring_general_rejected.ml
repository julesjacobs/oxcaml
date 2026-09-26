(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_ring.mli pref_ring.ml pref_ring_proofs.ml pref_ring_general.mli pref_ring_general.ml pref_ring_splice_general.mli pref_ring_splice_general.ml";
 readonly_files = "pref_ring_general_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run-expect;
   check-program-output;
 }
*)

(* Rejections for the generic ring reversal and range splice. The rings v3
   package checked these with a separate script; here they are expect cases. *)

open Pref_ring
open Pref_ring_general
module S = Pref_ring_splice_general;;
[%%expect{|
module S = Pref_ring_splice_general
|}]

(* Whole-ring reversal needs separated link cells. *)
module Missing_separation = struct
  let bad : (sentinel : node) @ immutable -> (ns : node list) @ immutable ghost ->
      (t : {t : node option Pref.token | ring (Pref.own t) sentinel ns}) @ unique ->
      node option Pref.token @ unique = fun sentinel ns t ->
    reverse sentinel ns t
end;;
[%%expect{|
Line 5, characters 24-25:
5 |     reverse sentinel ns t
                            ^
Error: Refinement could not be proved (counterexample)
|}, Principal{|
Line 3, characters 56-57:
3 |       (t : {t : node option Pref.token | ring (Pref.own t) sentinel ns}) @ unique ->
                                                            ^
Error: The value "t" has type "Pref_ring.node option Pref.token"
       but an expression was expected of type "'a Pref.token"
       The kind of Pref_ring.node option is
           immutable_data with Pref_ring.node
         because it's a boxed variant type.
       But the kind of Pref_ring.node option must be a subkind of
           immutable_data.
|}]

(* The range's last node must be the actual end of [first :: rest]. *)
module Wrong_endpoint = struct
  let bad : (s : node) @ immutable ghost -> (t : node) @ immutable ghost ->
      (prefix : node list) @ immutable ghost -> (first : node) @ immutable ->
      (rest : node list) @ immutable ghost -> (final : node) @ immutable ->
      (suffix : node list) @ immutable ghost ->
      (destination_prefix : node list) @ immutable ghost ->
      (destination_left : {n : node | n === S.last t destination_prefix}) @ immutable ->
      (destination_suffix : node list) @ immutable ghost ->
      (state : {state : node option Pref.token |
        ring (Pref.own state) s (append prefix (append (first :: rest) suffix)) &&
        ring (Pref.own state) t (append destination_prefix destination_suffix) &&
        separated (append (s :: append prefix (append (first :: rest) suffix))
          (t :: append destination_prefix destination_suffix))}) @ unique ->
      node option Pref.token @ unique =
    fun s t prefix first rest final suffix destination_prefix destination_left
      destination_suffix state ->
    S.splice s t prefix first rest final suffix destination_prefix
      destination_left destination_suffix state
end;;
[%%expect{|
Line 17, characters 35-40:
17 |     S.splice s t prefix first rest final suffix destination_prefix
                                        ^^^^^
Error: Refinement could not be proved (counterexample)
|}, Principal{|
Line 10, characters 23-28:
10 |         ring (Pref.own state) s (append prefix (append (first :: rest) suffix)) &&
                            ^^^^^
Error: The value "state" has type "Pref_ring.node option Pref.token"
       but an expression was expected of type "'a Pref.token"
       The kind of Pref_ring.node option is
           immutable_data with Pref_ring.node
         because it's a boxed variant type.
       But the kind of Pref_ring.node option must be a subkind of
           immutable_data.
|}]

(* The destination node must be the predecessor of the insertion point. *)
module Wrong_placement = struct
  let bad : (s : node) @ immutable ghost -> (t : node) @ immutable ghost ->
      (prefix : node list) @ immutable ghost -> (first : node) @ immutable ->
      (rest : node list) @ immutable ghost ->
      (final : {n : node | n === S.last first rest}) @ immutable ->
      (suffix : node list) @ immutable ghost ->
      (destination_prefix : node list) @ immutable ghost ->
      (destination_left : node) @ immutable ->
      (destination_suffix : node list) @ immutable ghost ->
      (state : {state : node option Pref.token |
        ring (Pref.own state) s (append prefix (append (first :: rest) suffix)) &&
        ring (Pref.own state) t (append destination_prefix destination_suffix) &&
        separated (append (s :: append prefix (append (first :: rest) suffix))
          (t :: append destination_prefix destination_suffix))}) @ unique ->
      node option Pref.token @ unique =
    fun s t prefix first rest final suffix destination_prefix destination_left
      destination_suffix state ->
    S.splice s t prefix first rest final suffix destination_prefix
      destination_left destination_suffix state
end;;
[%%expect{|
Line 19, characters 6-22:
19 |       destination_left destination_suffix state
           ^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}, Principal{|
Line 11, characters 23-28:
11 |         ring (Pref.own state) s (append prefix (append (first :: rest) suffix)) &&
                            ^^^^^
Error: The value "state" has type "Pref_ring.node option Pref.token"
       but an expression was expected of type "'a Pref.token"
       The kind of Pref_ring.node option is
           immutable_data with Pref_ring.node
         because it's a boxed variant type.
       But the kind of Pref_ring.node option must be a subkind of
           immutable_data.
|}]

(* Consumed handles and released raw tokens cannot be reused. *)
let ring_reuse () =
  let s = Owned.empty () in
  let _ = Owned.reverse s in
  Owned.reverse s;;
[%%expect{|
Line 4, characters 16-17:
4 |   Owned.reverse s;;
                    ^
Error: This value is used here, but it has already been used as unique at:
Line 3, characters 24-25:
3 |   let _ = Owned.reverse s in
                            ^

|}]

let ring_raw_reuse () =
  let s = Owned.empty () in
  let b = Owned.release s in
  let _ = Owned.adopt b in
  Owned.adopt b;;
[%%expect{|
Line 5, characters 14-15:
5 |   Owned.adopt b;;
                  ^
Error: This value is used here, but it has already been used as unique at:
Line 4, characters 22-23:
4 |   let _ = Owned.adopt b in
                          ^

|}]

let pair_reuse (s : S.Owned.t @ unique) =
  let _ = S.Owned.swap s in
  S.Owned.swap s;;
[%%expect{|
Line 3, characters 15-16:
3 |   S.Owned.swap s;;
                   ^
Error: This value is used here, but it has already been used as unique at:
Line 2, characters 23-24:
2 |   let _ = S.Owned.swap s in
                           ^

|}]

(* Private proofs and model definitions are hidden by the interfaces. *)
let hidden = Pref_ring_general.reverse_law;;
[%%expect{|
Line 1, characters 13-42:
1 | let hidden = Pref_ring_general.reverse_law;;
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unbound value "Pref_ring_general.reverse_law"
|}]

let hidden = Pref_ring_general.Owned.model_def;;
[%%expect{|
Line 1, characters 13-46:
1 | let hidden = Pref_ring_general.Owned.model_def;;
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unbound value "Pref_ring_general.Owned.model_def"
|}]

let hidden = Pref_ring_splice_general.chain_split;;
[%%expect{|
Line 1, characters 13-49:
1 | let hidden = Pref_ring_splice_general.chain_split;;
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unbound value "Pref_ring_splice_general.chain_split"
|}]

let hidden = Pref_ring_splice_general.Owned.source_def;;
[%%expect{|
Line 1, characters 13-54:
1 | let hidden = Pref_ring_splice_general.Owned.source_def;;
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unbound value "Pref_ring_splice_general.Owned.source_def"
Hint:   Did you mean "Pref_ring_splice_general.Owned.source_model"?
|}]
