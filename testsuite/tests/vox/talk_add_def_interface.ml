(* TEST
 has-z3;
 setup-ocamlc.opt-build-env;
 flags = "-extension refinement_types -i";
 module = "talk_add_def_interface.ml";
 ocamlc.opt;
 check-ocamlc.opt-output;
*)

(* Talk, section 4g ("definitions and induction"): [ocamlc -i] prints a
   definition as a function whose type is its equation. Nothing proves
   [add_def]: it holds because [add] is a total, stateless function that
   satisfies its own body. [add_zero] is induction: the recursive call is
   the induction hypothesis. From the investigation's a1.ml. *)

type nat = Z | S of nat [@@inductive]

let[@def] rec add m n = match m with Z -> n | S m' -> S (add m' n)

(* Induction: add m Z = m *)
let rec (add_zero @ total) : (m : nat) -> {u : unit | add m Z === m} =
  fun m ->
    add_def m Z;
    match m with
    | Z -> ()
    | S m' -> add_zero m'
