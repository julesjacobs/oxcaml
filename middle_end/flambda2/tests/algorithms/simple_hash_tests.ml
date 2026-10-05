module Ids = Flambda2_identifiers.Int_ids
module Simple = Ids.Simple
module Coercion = Ids.Coercion
module Rec_info_expr = Ids.Rec_info_expr
module K = Flambda2_kinds.Flambda_kind

let id (simple : Simple.t) =
  (simple :> Flambda2_algorithms.Table_by_int_id.Id.t)

module Element = struct
  type t =
    { simple : Simple.t;
      coercion : Coercion.t
    }

  let flags = 3

  let print ppf t = Simple.print ppf t.simple

  let hash t = Hashtbl.hash (Simple.hash t.simple, Coercion.hash t.coercion)

  let equal a b =
    Simple.equal a.simple b.simple && Coercion.equal a.coercion b.coercion
end

module Reference = Flambda2_algorithms.Table_by_int_id.Make (Element)

let () =
  let unit = Compilation_unit.of_string "Simple_hash_tests" in
  Current_unit.set
    (Unit_info.make_dummy ~input_name:"simple_hash_tests.ml" unit);
  Ids.reset ();
  let variables =
    Array.init 512 (fun i ->
        Ids.Variable.create ("v" ^ string_of_int i) K.value)
  in
  let reference = Reference.create () in
  let simples =
    Array.init 4096 (fun i ->
        let simple = Simple.var variables.(i mod Array.length variables) in
        let from =
          Rec_info_expr.var
            variables.(((i / 8) + 17) mod Array.length variables)
        in
        let from =
          if i land 1 = 0
          then Rec_info_expr.succ from
          else Rec_info_expr.unroll_to 3 from
        in
        let to_ = Rec_info_expr.succ from in
        let coercion = Coercion.change_depth ~from ~to_ in
        let actual = Simple.with_coercion simple coercion in
        assert (id actual = Reference.add reference { Element.simple; coercion });
        assert (Simple.with_coercion simple coercion = actual);
        assert (Coercion.equal (Simple.coercion actual) coercion);
        actual)
  in
  let selected =
    Array.fold_left
      (fun set simple -> Simple.Set.add simple set)
      Simple.Set.empty simples
  in
  let actual_export = Simple.export selected in
  let reference_export =
    Reference.export reference ~iter:(fun f ->
        Simple.Set.iter (fun simple -> f (id simple)) selected)
  in
  let actual_bytes = Marshal.to_string actual_export [] in
  assert (actual_bytes = Marshal.to_string reference_export []);
  let importer : Simple.importer = Marshal.from_string actual_bytes 0 in
  let reference_importer : Reference.serializable =
    Marshal.from_string actual_bytes 0
  in
  Ids.reset ();
  let mapping = Hashtbl.create 512 in
  Array.iter
    (fun v -> Hashtbl.add mapping v (Ids.Variable.create "imported" K.value))
    variables;
  let import_var v = Hashtbl.find mapping v in
  let import_simple simple =
    Simple.pattern_match simple
      ~name:(fun name ~coercion:_ ->
        Ids.Name.pattern_match name
          ~var:(fun var -> Simple.var (import_var var))
          ~symbol:(fun _ -> assert false))
      ~const:(fun _ -> assert false)
  in
  let destination = Reference.create () in
  Array.iter
    (fun simple ->
      let actual =
        Simple.import importer simple ~import_var
          ~import_const:(fun _ -> assert false)
          ~import_symbol:(fun _ -> assert false)
      in
      let original = Reference.import reference_importer (id simple) in
      let coercion =
        Coercion.map_depth_variables original.coercion ~f:import_var
      in
      let expected =
        Reference.add destination
          { Element.simple = import_simple original.simple; coercion }
      in
      assert (id actual = expected);
      assert (Coercion.equal (Simple.coercion actual) coercion))
    simples
