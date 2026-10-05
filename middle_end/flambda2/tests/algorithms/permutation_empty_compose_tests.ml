module Key = struct
  type t = int

  let compare = Int.compare

  let equal = Int.equal

  let hash = Hashtbl.hash

  let print = Format.pp_print_int
end

module Names = struct
  include Key
  include Flambda2_algorithms.Container_types.Make (Key)
end

module P = Flambda2_nominal.Permutation.Make (Names)

let all_permutations keys =
  let rec insert x = function
    | [] -> [[x]]
    | y :: ys -> (x :: y :: ys) :: List.map (fun zs -> y :: zs) (insert x ys)
  in
  List.fold_left
    (fun permutations x -> List.concat_map (insert x) permutations)
    [[]] keys

let of_images keys images =
  List.fold_left2
    (fun permutation key image ->
      if P.apply permutation key = image
      then permutation
      else P.compose_one ~first:permutation (P.apply permutation key) image)
    P.empty keys images

let () =
  let keys = [min_int; -1; 0; 1; max_int] in
  let queries = (min_int + 1) :: (max_int - 1) :: 17 :: keys in
  let permutations =
    all_permutations keys
    |> List.map (fun images -> images, of_images keys images)
  in
  List.iter
    (fun (images, first) ->
      List.iter2
        (fun key image -> assert (P.apply first key = image))
        keys images;
      assert (P.compose ~second:P.empty ~first == first);
      assert (P.compose ~second:first ~first:P.empty == first);
      List.iter
        (fun (_, second) ->
          let composed = P.compose ~second ~first in
          List.iter
            (fun key ->
              assert (P.apply composed key = P.apply second (P.apply first key));
              assert (P.apply (P.inverse composed) (P.apply composed key) = key))
            queries)
        permutations)
    permutations;
  let cancelled = P.compose_one ~first:(P.compose_one ~first:P.empty 0 1) 0 1 in
  assert (P.is_empty cancelled);
  List.iter
    (fun (_, p) ->
      assert (P.compose ~second:cancelled ~first:p == p);
      let expected = if P.is_empty p then cancelled else p in
      assert (P.compose ~second:p ~first:cancelled == expected))
    permutations
