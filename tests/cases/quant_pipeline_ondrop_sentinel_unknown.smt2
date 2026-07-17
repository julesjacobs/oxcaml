; ON-path drop-sentinel guard (quant-flip Q2.2), the pipeline-ON mirror of the OFF-path
; lemma_partial_drop_sat_degrades_unknown sentinel. The universal body [(> (* x x) x)] is
; NONLINEAR, outside the linear fragment the front-end quantified pipeline (ON) can lower,
; so [take_ir]/[clauses_of_assertion] (parser.ml) rejects the clause: it is DROPPED and the
; drop counter arms the always-live sat-degrade sentinel (loader: [incr dropped]). The
; ground core [(= a 1)] ALONE is SATISFIABLE (verified: model a=1), so WITHOUT the sentinel
; the pipeline would report a (wrong) `sat` — but the true status of
; [(= a 1) /\ forall x. x*x > x] is `unsat` (x=0 falsifies the universal, so the assertion
; set is unsatisfiable). The sentinel degrades the ground `Sat` to `Unknown`, which is
; sound. If the ON-path drop-sentinel were ever disarmed this golden flips to `sat` (a
; soundness violation). Runs under the DEFAULT (ON) in the harness; quant-pipeline-test
; additionally pins =1 and =0 (both must stay non-`sat`).
(set-logic AUFLIA)
(set-info :status unsat)
(declare-fun a () Int)
(assert (= a 1))
(assert (forall ((x Int)) (> (* x x) x)))
(check-sat)
