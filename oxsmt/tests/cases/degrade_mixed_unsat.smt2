; DEGRADATION HONEYPOT (mixed EUF+LIA). x=y and f(x)<f(y) are independent atoms
; propositionally (skeleton SATISFIABLE), but x=y forces f(x)=f(y) by congruence, which
; contradicts f(x)<f(y): theory UNSAT. :status unsat; the wired v1 solver must answer
; `unknown` (THE SOUNDNESS RULE). A regression to `sat` fires a soundness failure. Becomes
; real `unsat` once EUF+LIA combination (M4) lands.
(set-logic QF_UFLIA)
(set-info :status unsat)
(declare-fun f (Int) Int)
(declare-const x Int)
(declare-const y Int)
(assert (= x y))
(assert (< (f x) (f y)))
(check-sat)
