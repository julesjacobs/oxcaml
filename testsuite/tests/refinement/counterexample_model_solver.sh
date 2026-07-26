#!/bin/sh
# A controlled stand-in for a solver, used to check how a model reply is read.
# Reads the query on stdin and answers according to its first argument, so the
# prove query is satisfiable, the disprove query is not, and the model query
# takes the shape being tested.
script=$(cat)
case "$script" in
  *get-model*)
    case "$1" in
      clean)
        printf 'sat\n(model\n(define-fun v_0 () Int 1)\n)\n' ;;
      banner)
        printf 'FakeSMT 1.0\nsat\n(model\n(define-fun v_0 () Int 1)\n)\n' ;;
      error_after)
        printf 'sat\n(model\n(define-fun v_0 () Int 1)\n)\n'
        printf '(error "model is incomplete")\n' ;;
      nonzero)
        printf 'sat\n(model\n(define-fun v_0 () Int 1)\n)\n'
        exit 1 ;;
      contradictory)
        printf 'sat\n(model\n(define-fun v_0 () Int 1)\n)\nunknown\n' ;;
      *)
        printf 'unknown\n' ;;
    esac ;;
  *get-unsat-core*)
    printf 'sat\n' ;;
  *)
    printf 'unsat\n' ;;
esac
