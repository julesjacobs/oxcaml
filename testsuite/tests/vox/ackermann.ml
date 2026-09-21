(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_ackermann.ml vox_union_find_potential.ml vox_union_find_levels.ml vox_union_find_path_cost.ml ackermann.ml";
 { bytecode; }
 { native; }
 { flags += " -principal"; bytecode; }
 { flags += " -principal"; native; }
*)

module A = Vox_ackermann
let () =
  let cap = 4096Z in
  let k = 3Z in let t = 1Z in let x = 1Z in
  let result = A.iter cap k t x in
  assert (Bigint.equal result 2047Z)

let () =
  List.iter (fun (n, expected) ->
    let capacity = Bigint.of_int n in
    let positive : {n : Bigint.t | n >= 1Z} = assume_ capacity in
    let refine_ actual = A.inverse positive in
    assert (Bigint.equal actual (Bigint.of_int expected)))
    [1, 1; 2, 1; 3, 1; 4, 2; 7, 2; 8, 3; 2047, 3; 2048, 4]

let () =
  List.iter (fun cap ->
    let table = Array.make_matrix 5 (cap + 1) cap in
    for x = 0 to cap do table.(0).(x) <- min cap (x + 1) done;
    for level = 1 to 4 do
      for x = 0 to cap do
        let value = ref x in
        for _ = 1 to x + 1 do value := table.(level - 1).(!value) done;
        table.(level).(x) <- !value
      done
    done;
    for level = 0 to 4 do
      for start = 0 to cap do
        let expected = ref start in
        for count = 0 to cap do
          let actual = A.iter (Bigint.of_int cap) (Bigint.of_int level)
            (Bigint.of_int count) (Bigint.of_int start) in
          assert (Bigint.equal actual (Bigint.of_int !expected));
          expected := table.(level).(!expected)
        done
      done
    done)
    [1; 2; 3; 4; 7; 8; 12; 20]

module P = Vox_union_find_potential
let () =
  for n = 2 to 20 do
    let cap = Bigint.of_int n in
    let capacity : {n : Bigint.t | n >= 1Z} = assume_ cap in
    let refine_ alpha = A.inverse capacity in
    for r = 0 to n - 2 do
      let rank = Bigint.of_int r in
      for s = r + 1 to n - 1 do
        let parent = Bigint.of_int s in
        let before = P.node_phi cap alpha rank parent in
        assert (Bigint.compare before 0Z >= 0);
        assert (Bigint.compare before (Bigint.mul alpha rank) <= 0);
        for t = s to n - 1 do
          let after = P.node_phi cap alpha rank (Bigint.of_int t) in
          assert (Bigint.compare after before <= 0)
        done
      done
    done
  done
