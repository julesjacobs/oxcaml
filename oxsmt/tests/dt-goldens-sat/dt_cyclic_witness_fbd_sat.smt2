; Cyclic-witness model-completion. The witness graph is ACYCLIC (occurs-check passes), but
; a FREE class's disequal-peer materialization ([fbd]) re-entered a class still on the build
; stack through the shared memo and baked its [Uninterp] cycle-break placeholder into a peer
; tree, which surfaced in the REAL model at a datatype (list) position and failed the
; checker's sort-inhabitance -> unknown. The fix materializes disequal peers on a SEPARATE
; memo so peer steering never pollutes the real model build.
;
; Reduced from a real Barrett QF_DT instance (barrett-jsat tests/v3/v3l90090). RED on trunk
; (unknown, even with the cross-sort spine landed), GREEN with the fbd-separation fix
; (checked sat).
(set-logic QF_DT)
(set-info :status sat)
(declare-datatypes ((nat 0)(list 0)(tree 0)) (((succ (pred nat)) (zero))
((cons (car tree) (cdr list)) (null))
((node (children list)) (leaf (data nat)))
))
(declare-fun x1 () nat)
(declare-fun x2 () nat)
(declare-fun x3 () nat)
(declare-fun x4 () list)
(declare-fun x5 () list)
(declare-fun x6 () list)
(declare-fun x7 () tree)
(declare-fun x8 () tree)
(declare-fun x9 () tree)
(assert (and (and (and (and (and (and (and (and (= (node x5) (car x6)) (= (node x6) (car null))) ((_ is node) (node (cons x9 x5)))) (not ((_ is null) x6))) (not ((_ is cons) x4))) (not ((_ is leaf) (node (children x9))))) (not ((_ is null) (cons x9 (children x7))))) (not (= x5 (cdr x5)))) (not (= x5 x6))))
(check-sat)
