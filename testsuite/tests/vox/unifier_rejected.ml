(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml unifier_spec.ml unifier_proofs.ml unifier.ml";
 readonly_files = "unifier_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run-expect;
   check-program-output;
 }
 {
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run-expectnat;
   check-program-output;
 }
*)

open Unifier_spec;;
[%%expect{|
|}]

module Missing_write = struct
  let bad : (h : node Pref.heap) @ immutable ghost ->
      (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      (t : {t : node Pref.token | Pref.own t === h && H.mem h p && H.mem h q
        && H.at h p === Some Var && H.at h q === Some Bool
        && not (p === q)}) @ unique ->
      {r : result | unified h p q r.#ok (Pref.own r.#state) r.#derivation && r.#ok}
      @ unique = fun h p q t ->
    let refine_ t = t in
    let search = ghost_ Leaf in
    let d = ghost_ (Bind_left search) in
    let ok = true in
    ghost_ (unified_def h p q ok h d);
    let r = #{ok; state = t; derivation = d} in refine_ r
end;;
[%%expect{|
Line 14, characters 48-57:
14 |     let r = #{ok; state = t; derivation = d} in refine_ r
                                                     ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Missing_occurs = struct
  let bad : (h : node Pref.heap) @ immutable ghost ->
      (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      (t : {t : node Pref.token | Pref.own t === h && H.mem h p && H.mem h q
        && H.at h p === Some Var && H.at h q === Some (Arrow (p, p))
        && not (p === q)}) @ unique ->
      {r : result | unified h p q r.#ok (Pref.own r.#state) r.#derivation && r.#ok}
      @ unique = fun h p q t ->
    let refine_ t = t in
    let t : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ t in
    let link = Link q in
    let refine_ t = Pref.write p link t in
    let after = ghost_ (Pref.own (borrow_ t)) in
    let search = ghost_ Leaf in
    let found = false in
    ghost_ (searched_def h p q found search);
    let d = ghost_ (Bind_left search) in
    let ok = true in
    ghost_ (unified_def h p q ok after d);
    let r = #{ok; state = t; derivation = d} in refine_ r
end;;
[%%expect{|
Line 20, characters 48-57:
20 |     let r = #{ok; state = t; derivation = d} in refine_ r
                                                     ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module False_rejection = struct
  let bad : (h : node Pref.heap) @ immutable ghost ->
      (p : node Pref.t) @ immutable ->
      (t : {t : node Pref.token | Pref.own t === h && H.mem h p
        && H.at h p === Some Bool}) @ unique ->
      {r : result | unified h p p r.#ok (Pref.own r.#state) r.#derivation && not r.#ok}
      @ unique = fun h p t ->
    let refine_ t = t in
    let d = ghost_ Clash in
    let ok = false in
    ghost_ (unified_def h p p ok h d);
    let r = #{ok; state = t; derivation = d} in refine_ r
end;;
[%%expect{|
Line 12, characters 48-57:
12 |     let r = #{ok; state = t; derivation = d} in refine_ r
                                                     ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Equality = struct
  let wrong (p : node Pref.t @ immutable) : {b : bool | not b} =
    let refine_ b = Pref.equal p p in refine_ b
end;;
[%%expect{|
Line 3, characters 38-47:
3 |     let refine_ b = Pref.equal p p in refine_ b
                                          ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Missing_second_child = struct
  let bad : (h : node Pref.heap) @ immutable ghost ->
      (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      (a : node Pref.t) @ immutable -> (b : node Pref.t) @ immutable ->
      (t : {t : node Pref.token | Pref.own t === h
        && H.mem h p && H.mem h q && H.mem h a && H.mem h b
        && H.at h p === Some (Arrow (a, a))
        && H.at h q === Some (Arrow (a, b)) && not (a === b)}) @ unique ->
      {r : result | unified h p q r.#ok (Pref.own r.#state) r.#derivation && r.#ok}
      @ unique = fun h p q a b t ->
    let refine_ t = t in
    let ok = true in
    let d = ghost_ (Children (a, a, a, b, h, ok, Same, Same)) in
    ghost_ (unified_def h p q ok h d);
    let same = ghost_ Same in
    ghost_ (unified_def h a b ok h same);
    let r = #{ok; state = t; derivation = d} in refine_ r
end;;
[%%expect{|
Line 17, characters 48-57:
17 |     let r = #{ok; state = t; derivation = d} in refine_ r
                                                     ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
