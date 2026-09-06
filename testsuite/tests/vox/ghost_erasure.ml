(* TEST
 has-z3;
 flags = "-extension refinement_types -drawlambda -dcanonical-ids";
 { expect.opt; }
*)

module Proof = struct
  let rec (visit @ total) : (xs : int list) -> {u : unit | true} =
    fun xs ->
    match xs with
    | [] -> let u = () in refine_ u
    | _ :: rest ->
      let refine_ proof = visit rest in
      let u = () in refine_ u
end;;
[%%expect{|
(let
  (Proof/0 =
     (letrec
       (visit/0
          (function {nlocal = 0}
            xs/0[value<
                  (consts (0))
                   (non_consts ([0: ?,
                                 value<(consts (0)) (non_consts ([0: ?, *]))>]))>]
            : int
            (if (isint xs/0) (let (u/0 =[value<int>] 0) u/0)
              (let
                (rest/0 =a? (field_imm 1 xs/0)
                 *match*/0 =a? (field_imm 0 xs/0)
                 proof/0 =[value<int>] (apply visit/0 rest/0)
                 u/1 =[value<int>] 0)
                u/1))))
       (makeblock 0 visit/0)))
  (makeblock 0 Proof/0))
module Proof : sig val visit : int list -> {u : unit | true} end
|}]

module Client = struct
  let (run @ total) (xs : int list) =
    let refine_ proof = ghost_ (Proof.visit xs) in
    7
end;;
[%%expect{|
(let
  (Client/0 =
     (let
       (run/0 =
          (function {nlocal = 0}
            xs/1[value<
                  (consts (0))
                   (non_consts ([0: ?,
                                 value<(consts (0)) (non_consts ([0: ?, *]))>]))>]
            : int (let (proof/1 =[value<int>] 24029) 7)))
       (makeblock 0 run/0)))
  (makeblock 0 Client/0))
module Client : sig val run : int list -> int end
|}]
