(* The stored lemma record (ADR-0012 §1.1). Pure data; the store and instantiation live in
   [Manager]. *)

open Oxsmt_core
module Sat = Oxsmt_solver.Sat

type origin =
  | Named of string
  | Anonymous

type t =
  { qvars : Qvar.t array
  ; body : Term.t
  ; triggers : Term.t list list
  ; id : int
  ; frame : Sat.var
  ; origin : origin
  }
