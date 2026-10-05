(* TEST
 include ocamlcommon;
 flags = "-I ${ocamlsrcdir}/utils -I ${ocamlsrcdir}/typing";
 native;
*)

open Jkind_types

let calls = ref 0

let context =
  { Jkind.jkind_of_type =
      (fun _ -> incr calls; Some (Jkind.Builtin.immediate ~why:Enumeration));
    is_abstract = (fun _ -> failwith "unexpected abstract-kind query");
    lookup_type = (fun _ -> failwith "unexpected type lookup") }

let make layout ~with_bounds externality =
  let original = Jkind.for_boxed_tuple [None, Predef.type_int] in
  { original with
    Types.jkind =
      { base = Layout layout;
        with_bounds =
          (if with_bounds then original.jkind.with_bounds else No_with_bounds);
        mod_bounds = { original.jkind.mod_bounds with externality } } }

let externality jkind =
  Jkind.get_externality_upper_bound ~context Env.empty jkind

let crossing jkind = Jkind.get_mode_crossing ~context Env.empty jkind

let check layout ~with_bounds bound expected expected_calls =
  let jkind = make layout ~with_bounds bound in
  calls := 0;
  assert (Jkind_axis.Externality.equal (externality jkind) expected);
  assert (!calls = expected_calls);
  calls := 0;
  assert (Mode.Crossing.equal (crossing jkind) jkind.jkind.mod_bounds.crossing);
  assert (!calls = if with_bounds then 1 else 0)

let () =
  let scannable = Layout.Sort (Sort.Base Scannable, Scannable_axes.max) in
  let float64 = Layout.Sort (Sort.Base Float64, Scannable_axes.max) in
  let open Jkind_axis.Externality in
  check scannable ~with_bounds:false Internal Internal 0;
  check scannable ~with_bounds:false External64 External64 0;
  check float64 ~with_bounds:false Internal External 0;
  check scannable ~with_bounds:true Internal Internal 0;
  check scannable ~with_bounds:true External External 1;
  check float64 ~with_bounds:true Internal External 1

let () =
  let changes = ref [] in
  Sort.set_change_log (fun change -> changes := change :: !changes);
  List.iter
    (fun with_bounds ->
      List.iter
        (fun get ->
          let outer = Sort.Var (Sort.new_var ~level:1) in
          let inner = Sort.Var (Sort.new_var ~level:0) in
          assert (Sort.equate ~allow_mutation:true outer inner);
          assert (Sort.equate ~allow_mutation:true inner (Sort.Base Scannable));
          let jkind =
            make (Layout.Sort (outer, Scannable_axes.max)) ~with_bounds
              Jkind_axis.Externality.Internal
          in
          changes := [];
          get jkind;
          assert (List.length !changes = 1);
          List.iter Sort.undo_change !changes;
          changes := [];
          get jkind;
          assert (List.length !changes = 1))
        [(fun jkind -> ignore (externality jkind));
         (fun jkind -> ignore (crossing jkind))])
    [false; true]
