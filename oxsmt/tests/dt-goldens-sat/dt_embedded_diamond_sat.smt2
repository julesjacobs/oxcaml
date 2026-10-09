;; DAG-blowup end-to-end regression guard for Dt_model_check (codex/fable dt-spine finding 2).
;; The datatype [A = a0 | a(rec:A, d:S0)] has a base case, so forcing [t] to be [a] via the
;; tester yields the ACCEPTED model t = a(base_tree A, base_tree S0). The field sort chain
;; [S0 = c0(S1,S1) ... S39 = c39(S40,S40)], [S40 = end] has no base case except at the
;; bottom, so [base_tree S0] is a SHARED DIAMOND DAG: 41 distinct physical Ctor nodes but
;; 2^40 root-to-leaf paths (the builder memoizes base_tree per sort, dt.ml). The model self-
;; check (Session.commit_sat -> Dt_model_check) walks that embedded field; the un-memoized
;; checker re-derived over all 2^40 paths and HUNG the DT sat authority on this trivially-sat
;; input (builder finished instantly). With the physical-identity-memoized inhabits/v_eq/ev
;; it visits each node once and answers immediately. Trivially sat (a witness exists: any
;; [a(...)] value). RED on trunk (times out); checked sat with the fix.
(set-logic QF_DT)
(set-info :status sat)
(declare-datatypes ((A 0) (S0 0) (S1 0) (S2 0) (S3 0) (S4 0) (S5 0) (S6 0) (S7 0) (S8 0) (S9 0) (S10 0) (S11 0) (S12 0) (S13 0) (S14 0) (S15 0) (S16 0) (S17 0) (S18 0) (S19 0) (S20 0) (S21 0) (S22 0) (S23 0) (S24 0) (S25 0) (S26 0) (S27 0) (S28 0) (S29 0) (S30 0) (S31 0) (S32 0) (S33 0) (S34 0) (S35 0) (S36 0) (S37 0) (S38 0) (S39 0) (S40 0)) (
((a0) (a (rec A) (d S0)))
((c0 (l0 S1) (r0 S1)))
((c1 (l1 S2) (r1 S2)))
((c2 (l2 S3) (r2 S3)))
((c3 (l3 S4) (r3 S4)))
((c4 (l4 S5) (r4 S5)))
((c5 (l5 S6) (r5 S6)))
((c6 (l6 S7) (r6 S7)))
((c7 (l7 S8) (r7 S8)))
((c8 (l8 S9) (r8 S9)))
((c9 (l9 S10) (r9 S10)))
((c10 (l10 S11) (r10 S11)))
((c11 (l11 S12) (r11 S12)))
((c12 (l12 S13) (r12 S13)))
((c13 (l13 S14) (r13 S14)))
((c14 (l14 S15) (r14 S15)))
((c15 (l15 S16) (r15 S16)))
((c16 (l16 S17) (r16 S17)))
((c17 (l17 S18) (r17 S18)))
((c18 (l18 S19) (r18 S19)))
((c19 (l19 S20) (r19 S20)))
((c20 (l20 S21) (r20 S21)))
((c21 (l21 S22) (r21 S22)))
((c22 (l22 S23) (r22 S23)))
((c23 (l23 S24) (r23 S24)))
((c24 (l24 S25) (r24 S25)))
((c25 (l25 S26) (r25 S26)))
((c26 (l26 S27) (r26 S27)))
((c27 (l27 S28) (r27 S28)))
((c28 (l28 S29) (r28 S29)))
((c29 (l29 S30) (r29 S30)))
((c30 (l30 S31) (r30 S31)))
((c31 (l31 S32) (r31 S32)))
((c32 (l32 S33) (r32 S33)))
((c33 (l33 S34) (r33 S34)))
((c34 (l34 S35) (r34 S35)))
((c35 (l35 S36) (r35 S36)))
((c36 (l36 S37) (r36 S37)))
((c37 (l37 S38) (r37 S38)))
((c38 (l38 S39) (r38 S39)))
((c39 (l39 S40) (r39 S40)))
((end))
))
(declare-fun t () A)
(assert ((_ is a) t))
(check-sat)
