(* Nonlinear integer arithmetic (QF_NIA) support is DARK by default: a tri-state env lever
   following the fleet convention. Unlike the LRA flip this defaults OFF, so an unset (or
   force-OFF) environment is byte-identical to trunk — the QF_NIA/NIA logic names stay
   rejected at [set-logic] and no [.oxsmt.nia.mul] abstraction is ever built.

   - unset -> OFF (the dark default; byte-identical to trunk)
   - "1"/"true"/"yes"/"on" (any case, trimmed) -> ON
   - any other value (incl. "0"/"false"/typos) -> OFF (the conservative default). Read at
     most once; every gate routes through {!enabled}. *)
let value =
  lazy
    (match Sys.getenv_opt "OXSMT_NIA" with
     | None -> false
     | Some value ->
       (match String.trim (String.lowercase_ascii value) with
        | "1" | "true" | "yes" | "on" -> true
        | _ -> false))
;;

let enabled () = Lazy.force value

(* The single reserved function symbol name used to abstract a nonlinear integer product
   [a*b] as an uninterpreted [(.oxsmt.nia.mul a b) : Int]. One shared symbol means
   hash-consing gives congruence for free (identical [(a,b)] pairs are the same term).

   {b Soundness of the abstraction.} To the EUF+LIA combination this is an ORDINARY
   uninterpreted function, so any [unsat] holds for EVERY interpretation of it, including
   real multiplication — [unsat] is sound with or without the lemmas. The multiplication
   lemmas ({!Nia_lin.lemmas}) only ADD unsats, each a valid consequence of [a*b], so they
   preserve that. Every [sat] is re-checked by {!Model_check} which evaluates this symbol
   as REAL integer multiplication (not via the reconstructed uninterpreted table), so a
   model in which the product constraint is violated fails closed to [unknown]. This is
   the PAIRED consuming-side inertness check (Internal_minter pairing contract): the
   symbol is admitted to the parse-time minter, and its harmful effect is neutralised by
   real-multiplication re-evaluation + rank agreement (a mis-ranked or non-2-arg
   occurrence is not treated as a product), never a wrong verdict. *)
let mul_name = ".oxsmt.nia.mul"
let is_mul_name name = String.equal name mul_name
