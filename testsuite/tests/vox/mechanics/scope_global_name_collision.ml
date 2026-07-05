(* TEST
 flags = "-vox-dry-run";
 expect;
*)

(* vox: solver-side value names are [g_ ^ sanitize(path)], and the
   sanitizer maps '.' and '_' alike to '_' -- NOT injective.  Two
   distinct module-qualified values whose paths differ only in such
   characters would emit two same-named Lean binders; the later
   shadows the earlier while both hypotheses still attach, making the
   VC context inconsistent and every goal provable.  Found by
   adversarial scope testing (A.B.c = 1 + A_B.c = 2 "certified" as
   999).  Rejected at registration instead. *)

module A = struct
  module B = struct
    let c : int{ _ = 1 } = 1
  end
end

module A_B = struct
  let c : int{ _ = 2 } = 2
end

let certified : int{ _ = 999 } = refine_ (A.B.c + A_B.c)
[%%expect{|
module A : sig module B : sig val c : int{ _ = 1 } end end
module A_B : sig val c : int{ _ = 2 } end
Line 1:
Error: vox: values A_B.c and A.B.c would share the solver-side name g_A_B_c; rename one of them
|}]
