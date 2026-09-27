(* TEST
 has-z3;
 flags = "-extension refinement_types -dlambda -dno-unique-ids";
 { expect; }
*)

(* Talk, section 5 ("testing"), point 4: preconditions are proved,
   postconditions are tested. The code generated for a property lemma
   evaluates only its claim, [sorted (insert x xs)]; it never re-checks the
   precondition [sorted xs], which every caller has proved. From the
   investigation's b1.ml. *)

let rec (sorted @ total) (xs : int list) : bool =
  match xs with
  | [] | [_] -> true
  | a :: (b :: _ as rest) -> a <= b && sorted rest

let rec (insert @ total) (x : int) (xs : int list) : int list =
  match xs with
  | [] -> [x]
  | y :: ys -> if x <= y then x :: xs else y :: insert x ys;;
[%%expect{|
(letrec
  (sorted
     (function {nlocal = 0}
       xs[value<
           (consts (0))
            (non_consts ([0: ?, value<(consts (0)) (non_consts ([0: ?, *]))>]))>]
       : int
       (catch
         (if xs
           (let (rest =a? (field_imm 1 xs))
             (if rest
               (&& (%int_lessequal (field_imm 0 xs) (field_imm 0 rest))
                 (apply sorted rest))
               (exit 2)))
           (exit 2))
        with (2) 1)))
  (apply (field_imm 1 (global Toploop!)) "sorted" sorted))
val sorted : int list -> bool = <fun>
(letrec
  (insert
     (function {nlocal = 0} x[value<int>]
       xs[value<
           (consts (0))
            (non_consts ([0: ?, value<(consts (0)) (non_consts ([0: ?, *]))>]))>]
       : (consts (0))
          (non_consts ([0: ?, value<(consts (0)) (non_consts ([0: ?, *]))>]))
       (if xs
         (let (y =a? (field_imm 0 xs))
           (if (%int_lessequal x y)
             (makeblock 0 (value<int>,value<
                                       (consts (0))
                                        (non_consts ([0: ?,
                                                      value<
                                                       (consts (0))
                                                        (non_consts (
                                                        [0: ?, *]))>]))>)
               x xs)
             (makeblock 0 (value<int>,value<
                                       (consts (0))
                                        (non_consts ([0: ?,
                                                      value<
                                                       (consts (0))
                                                        (non_consts (
                                                        [0: ?, *]))>]))>)
               y (apply insert x (field_imm 1 xs)))))
         (makeblock 0 (value<int>,value<
                                   (consts (0))
                                    (non_consts ([0: ?,
                                                  value<
                                                   (consts (0))
                                                    (non_consts ([0: ?, *]))>]))>)
           x 0))))
  (apply (field_imm 1 (global Toploop!)) "insert" insert))
val insert : int -> int list -> int list = <fun>
|}]

(* Property stated as a lemma, "proved" by a runtime check. *)
let prop_insert_sorted (x : int) (xs : {xs : int list | sorted xs}) :
    {u : unit | sorted (insert x xs)} =
  let u = () in assume_ u;;
[%%expect{|
(let
  (insert =? (apply (field_imm 0 (global Toploop!)) "insert")
   sorted =? (apply (field_imm 0 (global Toploop!)) "sorted")
   prop_insert_sorted =
     (function {nlocal = 0} x[value<int>]
       xs[value<
           (consts (0))
            (non_consts ([0: ?, value<(consts (0)) (non_consts ([0: ?, *]))>]))>]
       : int
       (let (u =[value<int>] 0)
         (if (apply sorted (apply insert x xs)) u
           (raise (makeblock 0 (getpredef Assert_failure!!) [0: "" 3 16]))))))
  (apply (field_imm 1 (global Toploop!)) "prop_insert_sorted"
    prop_insert_sorted))
val prop_insert_sorted :
  (x : int) ->
  (xs : {xs : int list | sorted xs}) -> {u : unit | sorted (insert x xs)} =
  <fun>
|}]
