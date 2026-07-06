(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: precedence-aware predicate printing.  The pretty-printer
   (Refinement.print) parenthesizes a sub-term only where the predicate
   grammar (parser.mly [vox_pred]) would otherwise reparse it as a
   different tree.  This is the round-trip contract: for each adversarial
   pair below the two source predicates mean DIFFERENT things, and the
   printed [... -> unit{ ... }] forms must differ accordingly -- one
   keeps parentheses, the other drops them.  The variables are
   dependent-arrow parameters (a module-level refinement may only mention
   such binders); predicates are untyped, so booleans are built from
   comparisons over the integer parameters, and nothing is verified. *)

module type S = sig
  (* Subtraction is left-associative and non-commutative: the
     left-nested tree drops its parens, the right-nested one keeps
     them. *)
  val sub_l : (a : int) -> (b : int) -> (c : int) -> unit{ (a - b) - c = 0 }
  val sub_r : (a : int) -> (b : int) -> (c : int) -> unit{ a - (b - c) = 0 }

  (* Three levels deep. *)
  val sub_lll :
    (a : int) -> (b : int) -> (c : int) -> (d : int) -> unit{ ((a - b) - c) - d = 0 }
  val sub_rrr :
    (a : int) -> (b : int) -> (c : int) -> (d : int) -> unit{ a - (b - (c - d)) = 0 }

  (* Division, likewise. *)
  val div_l : (a : int) -> (b : int) -> (c : int) -> unit{ (a / b) / c = 0 }
  val div_r : (a : int) -> (b : int) -> (c : int) -> unit{ a / (b / c) = 0 }

  (* Additive mixing: [a - b + c] is [(a - b) + c]; the other grouping
     keeps parens. *)
  val addsub_l : (a : int) -> (b : int) -> (c : int) -> unit{ (a - b) + c = 0 }
  val addsub_r : (a : int) -> (b : int) -> (c : int) -> unit{ a - (b + c) = 0 }

  (* Multiplicative binds tighter than additive. *)
  val muladd_bare : (a : int) -> (b : int) -> (c : int) -> unit{ a + b * c = 0 }
  val muladd_par : (a : int) -> (b : int) -> (c : int) -> unit{ (a + b) * c = 0 }
  val addmul_bare : (a : int) -> (b : int) -> (c : int) -> unit{ a * b + c = 0 }
  val addmul_par : (a : int) -> (b : int) -> (c : int) -> unit{ a * (b + c) = 0 }

  (* [mod] sits with multiplication. *)
  val mod_bare : (a : int) -> (b : int) -> (c : int) -> unit{ a mod b + c = 0 }
  val mod_par : (a : int) -> (b : int) -> (c : int) -> unit{ a mod (b + c) = 0 }

  (* Comparisons are non-associative: a chain keeps its parentheses,
     a single comparison does not. *)
  val cmp_chain : (a : int) -> (b : int) -> (c : int) -> unit{ (a = b) = c }
  val cmp_one : (a : int) -> (b : int) -> unit{ a = b }

  (* [&&] binds tighter than [||]; the [||] under a [&&] keeps parens. *)
  val andor_bare : (a : int) -> (b : int) -> (c : int) -> (d : int) -> unit{ a < b && b < c || c < d }
  val orand_par : (a : int) -> (b : int) -> (c : int) -> (d : int) -> unit{ (a < b || b < c) && c < d }

  (* [&&] is left-associative. *)
  val and_l : (a : int) -> (b : int) -> (c : int) -> (d : int) -> unit{ (a < b && b < c) && c < d }
  val and_r : (a : int) -> (b : int) -> (c : int) -> (d : int) -> unit{ a < b && (b < c && c < d) }

  (* [not] binds like application: tighter than the operators, so it
     parenthesizes a comparison or conjunction but not an atom. *)
  val not_cmp : (a : int) -> (b : int) -> unit{ not (a < b) }
  val not_atom : (a : int) -> unit{ not a }
  val not_and_bare : (a : int) -> (b : int) -> (c : int) -> unit{ not a && b < c }
  val not_and_par : (a : int) -> (b : int) -> (c : int) -> unit{ not (a < b && b < c) }

  (* Implication is loosest and right-associative; its right operand is
     a trailing position, so a chained [->] or a looser operator under
     it reads bare, while a left-nested implication keeps parens. *)
  val imp_r : (a : int) -> (b : int) -> (c : int) -> (d : int) -> unit{ a < b -> b < c -> c < d }
  val imp_l : (a : int) -> (b : int) -> (c : int) -> (d : int) -> unit{ (a < b -> b < c) -> c < d }
  val imp_or_bare : (a : int) -> (b : int) -> (c : int) -> (d : int) -> unit{ a < b -> b < c || c < d }
  val imp_or_par : (a : int) -> (b : int) -> (c : int) -> (d : int) -> unit{ (a < b -> b < c) || c < d }

  (* Quantifiers are loosest and extend maximally right; a quantifier
     as an operand parenthesizes, a quantifier body does not. *)
  val quant_body : (a : int) -> (b : int) -> unit{ forall_ z. a < z -> b < z }
  val quant_operand : (a : int) -> (b : int) -> (c : int) -> unit{ (forall_ z. a < z) && b < c }
end
[%%expect{|
module type S =
  sig
    val sub_l : (a : int) -> (b : int) -> (c : int) -> unit{ a - b - c = 0 }
    val sub_r :
      (a : int) -> (b : int) -> (c : int) -> unit{ a - (b - c) = 0 }
    val sub_lll :
      (a : int) ->
      (b : int) -> (c : int) -> (d : int) -> unit{ a - b - c - d = 0 }
    val sub_rrr :
      (a : int) ->
      (b : int) -> (c : int) -> (d : int) -> unit{ a - (b - (c - d)) = 0 }
    val div_l : (a : int) -> (b : int) -> (c : int) -> unit{ a / b / c = 0 }
    val div_r :
      (a : int) -> (b : int) -> (c : int) -> unit{ a / (b / c) = 0 }
    val addsub_l :
      (a : int) -> (b : int) -> (c : int) -> unit{ a - b + c = 0 }
    val addsub_r :
      (a : int) -> (b : int) -> (c : int) -> unit{ a - (b + c) = 0 }
    val muladd_bare :
      (a : int) -> (b : int) -> (c : int) -> unit{ a + b * c = 0 }
    val muladd_par :
      (a : int) -> (b : int) -> (c : int) -> unit{ (a + b) * c = 0 }
    val addmul_bare :
      (a : int) -> (b : int) -> (c : int) -> unit{ a * b + c = 0 }
    val addmul_par :
      (a : int) -> (b : int) -> (c : int) -> unit{ a * (b + c) = 0 }
    val mod_bare :
      (a : int) -> (b : int) -> (c : int) -> unit{ a mod b + c = 0 }
    val mod_par :
      (a : int) -> (b : int) -> (c : int) -> unit{ a mod (b + c) = 0 }
    val cmp_chain :
      (a : int) -> (b : int) -> (c : int) -> unit{ (a = b) = c }
    val cmp_one : (a : int) -> (b : int) -> unit{ a = b }
    val andor_bare :
      (a : int) ->
      (b : int) -> (c : int) -> (d : int) -> unit{ a < b && b < c || c < d }
    val orand_par :
      (a : int) ->
      (b : int) ->
      (c : int) -> (d : int) -> unit{ (a < b || b < c) && c < d }
    val and_l :
      (a : int) ->
      (b : int) -> (c : int) -> (d : int) -> unit{ a < b && b < c && c < d }
    val and_r :
      (a : int) ->
      (b : int) ->
      (c : int) -> (d : int) -> unit{ a < b && (b < c && c < d) }
    val not_cmp : (a : int) -> (b : int) -> unit{ not (a < b) }
    val not_atom : (a : int) -> unit{ not a }
    val not_and_bare :
      (a : int) -> (b : int) -> (c : int) -> unit{ not a && b < c }
    val not_and_par :
      (a : int) -> (b : int) -> (c : int) -> unit{ not (a < b && b < c) }
    val imp_r :
      (a : int) ->
      (b : int) -> (c : int) -> (d : int) -> unit{ a < b -> b < c -> c < d }
    val imp_l :
      (a : int) ->
      (b : int) ->
      (c : int) -> (d : int) -> unit{ (a < b -> b < c) -> c < d }
    val imp_or_bare :
      (a : int) ->
      (b : int) -> (c : int) -> (d : int) -> unit{ a < b -> b < c || c < d }
    val imp_or_par :
      (a : int) ->
      (b : int) ->
      (c : int) -> (d : int) -> unit{ (a < b -> b < c) || c < d }
    val quant_body :
      (a : int) -> (b : int) -> unit{ forall_ z. a < z -> b < z }
    val quant_operand :
      (a : int) ->
      (b : int) -> (c : int) -> unit{ (forall_ z. a < z) && b < c }
  end
|}]
