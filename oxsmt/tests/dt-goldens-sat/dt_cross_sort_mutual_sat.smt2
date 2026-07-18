; Cross-sort (mutually-recursive) model completion. The [tree] sort recurses ONLY through
; [list] ([node (children list)] / [leaf (data nat)]) — no constructor of [tree] has a
; direct [tree] field. A satisfying model needs a [tree] value distinct from the peers
; forced by the disequalities; the base-only completion (which spines just a DIRECT
; self-recursive field) has a single [tree] value here, so the model self-check
; spuriously fails and the file degraded to [unknown]. The cross-sort spine realizes the
; [tree] value through the intermediate [list] ([node (cons ...)] of distinct list length),
; giving unboundedly many distinct trees, so this is now checked sat.
;
; Reduced from a real Barrett QF_DT instance (barrett-jsat tests/v1/v1l90094; the #62 lane
; established that this completion class is hard to reproduce with a hand-built tiny query
; because the search exhaustively splits small free classes before the free-completion
; path is reached). RED on trunk (unknown), GREEN with the cross-sort spine (checked sat).
(set-logic QF_DT)
(set-info :status sat)
(declare-datatypes ((nat 0)(list 0)(tree 0)) (((succ (pred nat)) (zero))
((cons (car tree) (cdr list)) (null))
((node (children list)) (leaf (data nat)))
))
(declare-fun x1 () nat)
(declare-fun x2 () list)
(declare-fun x3 () tree)
(assert (and (and (and (and (and (and (and (and ((_ is succ) (succ x1)) (not (= (data x3) (succ zero)))) (not ((_ is leaf) (node null)))) (not (= x2 null))) (not (= (succ x1) x1))) (not (= null (cdr x2)))) (not ((_ is zero) (data (node (cons x3 (cdr null))))))) ((_ is cons) (cdr (cons (node (cdr x2)) (cdr null))))) (not (= (cdr null) x2))))
(check-sat)
