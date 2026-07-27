(* TEST
 readonly_files = "lemma_calls_check.py";
 setup-ocamlc.byte-build-env;
 flags = "-vox-dump-vc-json lemma_calls.json -c";
 compiler_output = "lemma_calls.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/lemma_calls_check.py \
           lemma_calls.json";
 script;
*)

(* A call whose only product is a proposition is recorded by span, so a
   consumer can ask whether any obligation read what it introduced.  Two
   things have to hold for that question to be answerable.  The recorded set
   must be exactly the calls that hand back nothing but evidence -- a partial
   function returning a refined unit also does something, and dropping it
   would not be a matter of tidiness.  And every fact must name every site
   that introduced it, not just the one whose name the pane shows: the fact
   environment keeps one entry per proposition, so a second call stating what
   an earlier one already stated, and two branch arms stating the same thing
   before their merge, all collapse into a single entry.  Reading only that
   entry's own origin is what would leave a load-bearing call looking unread. *)

external law : x:int -> unit{ x + 0 = x } @@ total = "%ignore"

(* No totality: the call does something besides state its predicate. *)
external effectful_law : x:int -> unit{ x + 0 = x } = "%ignore"

let single (a : int{ _ > 3 }) =
  let () = law ~x:a in
  (a : int{ _ > 2 })

let repeated (b : int{ _ > 3 }) =
  let () = law ~x:b in
  let () = law ~x:b in
  (b : int{ _ > 2 })

let branches (c : bool) (d : int{ _ > 3 }) =
  let () = if c then law ~x:d else law ~x:d in
  (d : int{ _ > 2 })

let not_evidence (e : int{ _ > 3 }) =
  let () = effectful_law ~x:e in
  (e : int{ _ > 2 })

(* A law an obligation genuinely needs.  Without one of these the usage half
   of this test cannot fail in the direction that matters: every reading of
   [used] above is "unread", so a mechanism that had stopped reading the
   solver's answer and returned "unread" for everything would satisfy it, and
   that is exactly the failure that would tell a reader to delete a call the
   proof depends on. *)
external needed : x:int -> unit{ x > 3 } @@ total = "%ignore"

let uses_law (f : int) =
  let () = needed ~x:f in
  (f : int{ _ > 2 })

(* An argument that does work of its own.  [outer]'s own proposition is read
   by nothing, but its span contains the call to [needed] whose proposition
   the goal below rests on, and the two are one span to anyone deleting the
   marked text.  So [outer] must not be offered as a call at all, while the
   inner call, whose span is its own, still is. *)
external outer : unit -> unit{ 2 > 1 } @@ total = "%ignore"

let nested_argument (g : int) =
  let () = outer (needed ~x:g) in
  (g : int{ _ > 2 })

(* The same, where the argument states a proposition an earlier call already
   stated.  The fact environment keeps one entry for it, so the only trace
   the inner call leaves is a producer added to that entry -- and a check
   that compared the propositions alone would see an unchanged environment
   and offer the outer call. *)
let nested_merged (h : int{ _ > 3 }) =
  let () = law ~x:h in
  let () = outer (law ~x:h) in
  (h : int{ _ > 2 })
