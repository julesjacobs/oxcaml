; G2 trap (codex round-2 DISCRIMINATING version). The prior trap (a bare
; :source "(assert false)") did NOT discriminate: on the OLD quote-blind lexer the
; nested (assert false) stayed a sub-list of set-info and was ignored, so both
; readers REFUTED it. This version embeds a ')' INSIDE the string that, on a lexer
; without string handling, prematurely closes the set-info; the following
; (assert false) then becomes a TOP-LEVEL command injected into the theorem, and
; the tail re-balances into a second (ignored) set-info so the old parse stays
; well-formed and CERTIFIES a false unsat.
;
; Empirically verified against the pre-fix reader (trunk 0ff37e2, scratch build):
;   OLD reader: CERTIFIED  (injected `false` makes `p ∧ false` unsat)
;   NEW reader: REFUTED    (the string is one inert Str token; the real theorem is
;                           just `p`, refuted by the witness model p:=true sidecar)
(set-logic QF_UFLIA)
(declare-const p Bool)
(set-info :status unsat)
(assert p)
(set-info :s ") (assert false) (set-info :d 0")
(check-sat)
