(* TEST
 flags = "-extension refinement_types";
 has-z3;
 { expect; }
 { expect.opt; }
*)

let () =
  let module Int_list = struct
    type t = Nil | Cons of int * t [@@inductive]

    let[@def] rec append xs ys =
      match xs with
      | Nil -> ys
      | Cons (head, tail) -> Cons (head, append tail ys)

    let[@def] rec length xs =
      match xs with
      | Nil -> 0
      | Cons (_, tail) -> 1 + length tail

    let[@def] rec sum xs =
      match xs with
      | Nil -> 0
      | Cons (head, tail) -> head + sum tail
  end
  in
  let open Int_list in
  let module Laws = struct
    let (append_nil_left @ total) ys :
        {u : unit | append Nil ys === ys} =
      let nil = Nil in
      let refine_ equation = append_def nil ys in
      let u = () in
      refine_ u

    let rec (append_nil_right @ total) :
        (xs : t) ->
        {u : unit | append xs Nil === xs} @ immutable contended =
      fun xs ->
      let nil = Nil in
      let refine_ equation = append_def xs nil in
      match xs with
      | Nil ->
        let u = () in
        refine_ u
      | Cons (_, tail) ->
        let induction : {u : unit | append tail Nil === tail} =
          append_nil_right tail
        in
        let refine_ induction = induction in
        let u = () in
        refine_ u

    let rec (append_associative @ total) :
        (xs : t) ->
        (ys : t) ->
        (zs : t) ->
        {u : unit |
          append (append xs ys) zs === append xs (append ys zs)}
          @ immutable contended =
      fun xs ys zs ->
      let xy = append xs ys in
      let yz = append ys zs in
      let refine_ equation = append_def xs ys in
      let refine_ equation = append_def xy zs in
      let refine_ equation = append_def ys zs in
      let refine_ equation = append_def xs yz in
      match xs with
      | Nil ->
        let u = () in
        refine_ u
      | Cons (_, tail) ->
        let induction :
            {u : unit |
              append (append tail ys) zs === append tail (append ys zs)} =
          append_associative tail ys zs
        in
        let refine_ induction = induction in
        let u = () in
        refine_ u

    let rec (length_append @ total) :
        (xs : t) ->
        (ys : t) ->
        {u : unit | length (append xs ys) === length xs + length ys}
          @ immutable contended =
      fun xs ys ->
      let xy = append xs ys in
      let refine_ equation = append_def xs ys in
      let refine_ equation = length_def xy in
      let refine_ equation = length_def xs in
      match xs with
      | Nil ->
        let u = () in
        refine_ u
      | Cons (_, tail) ->
        let induction :
            {u : unit |
              length (append tail ys) === length tail + length ys} =
          length_append tail ys
        in
        let refine_ induction = induction in
        let u = () in
        refine_ u

    let rec (sum_append @ total) :
        (xs : t) ->
        (ys : t) ->
        {u : unit | sum (append xs ys) === sum xs + sum ys}
          @ immutable contended =
      fun xs ys ->
      let xy = append xs ys in
      let refine_ equation = append_def xs ys in
      let refine_ equation = sum_def xy in
      let refine_ equation = sum_def xs in
      match xs with
      | Nil ->
        let u = () in
        refine_ u
      | Cons (_, tail) ->
        let induction :
            {u : unit | sum (append tail ys) === sum tail + sum ys} =
          sum_append tail ys
        in
        let refine_ induction = induction in
        let u = () in
        refine_ u
  end
  in
  let xs = Cons (1, Cons (2, Nil)) in
  let ys = Cons (3, Cons (4, Cons (5, Nil))) in
  let zs = ghost_ (Cons (6, Nil)) in
  let refine_ left_identity = ghost_ (Laws.append_nil_left xs) in
  let refine_ right_identity = ghost_ (Laws.append_nil_right xs) in
  let refine_ associative = ghost_ (Laws.append_associative xs ys zs) in
  let refine_ length_append = ghost_ (Laws.length_append xs ys) in
  let refine_ sum_append = ghost_ (Laws.sum_append xs ys) in
  let result = append xs ys in
  Format.printf "length = %d, sum = %d@." (length result) (sum result);;
[%%expect{|
length = 5, sum = 15
|}]
