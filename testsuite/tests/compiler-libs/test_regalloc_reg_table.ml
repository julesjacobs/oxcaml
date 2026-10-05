(* TEST
 include ocamlcommon;
 include ocamloptcomp;
 flags = "-I ${ocamlsrcdir}/utils -I ${ocamlsrcdir}/backend -I ${ocamlsrcdir}/backend/regalloc";
 native;
*)

module Graph = Regalloc_interf_graph
module Table = Regalloc_reg_table
module State = Regalloc_irc_state

let test_graph () =
  Regalloc_utils.set_function_specific_params ["BIT_MATRIX_THRESHOLD:0"];
  let left = Reg.create Cmm.Val in
  let right = Reg.create Cmm.Val in
  let sibling =
    Reg.create_alias
      (Reg.For_testing.with_loc left Proc.loc_exn_bucket.Reg.loc)
      ~typ:Cmm.Int
  in
  let negative =
    Reg.For_printing.create ~name:left.Reg.name ~typ:Cmm.Val
      ~stamp:(Reg.Stamp.of_int_unsafe (-1)) ~preassigned:false ~loc:Unknown
  in
  let graph = Graph.make () in
  List.iter (Graph.init_register graph) [left; right; sibling; negative];
  Graph.set_degree graph left 11;
  Graph.set_degree graph sibling 19;
  Graph.set_degree graph negative 23;
  assert (Graph.degree graph left = 11);
  assert (Graph.degree graph sibling = 19);
  assert (Graph.degree graph negative = 23);
  assert (Graph.get_max_degree graph = 23);
  Graph.init_register_with_infinite_degree graph sibling;
  assert (Graph.degree graph sibling = Graph.Degree.infinite);
  Graph.init_register graph left;
  assert (Graph.degree graph left = 0);
  assert (Graph.degree graph sibling = Graph.Degree.infinite);
  Graph.add_edge graph left right;
  Graph.add_edge graph right left;
  assert (Graph.degree graph left = 1);
  assert (Graph.degree graph right = 1);
  assert (Graph.mem_edge graph left right);
  assert (Graph.adj_list graph left = [right]);
  assert (Graph.adj_list graph right = [left]);
  let later = Array.init 2049 (fun _ -> Reg.create Cmm.Val) in
  Array.iter (Graph.init_register graph) later;
  Array.iteri
    (fun index reg ->
      Graph.set_degree graph reg (index mod 17);
      assert (Graph.decr_degree graph reg = index mod 17);
      assert (Graph.degree graph reg = (index mod 17) - 1))
    later;
  assert (Graph.get_max_degree graph = 23);
  let last = later.(Array.length later - 1) in
  Graph.init_register graph last;
  Graph.add_edge graph left last;
  assert (Graph.adj_list graph left = [last; right]);
  let visited = ref [] in
  Graph.iter_adjacent graph left ~f:(fun reg -> visited := reg :: !visited);
  assert (List.rev !visited = [last; right]);
  Graph.clear graph;
  assert (Graph.get_max_degree graph = 0);
  assert (not (Graph.mem_edge graph left right));
  assert (match Graph.adj_list graph left with
    | exception Not_found -> true
    | _ -> false);
  List.iter (Graph.init_register graph) [left; right; sibling; negative];
  Graph.set_degree graph left 31;
  Graph.set_degree graph sibling 37;
  Graph.set_degree graph negative 41;
  assert (Graph.degree graph left = 31);
  assert (Graph.degree graph sibling = 37);
  assert (Graph.degree graph negative = 41);
  assert (Graph.get_max_degree graph = 41);
  Regalloc_utils.set_function_specific_params ["BIT_MATRIX_THRESHOLD:100000"];
  let graph = Graph.make () in
  List.iter (Graph.init_register graph) [left; right];
  Graph.add_edge graph left right;
  assert (Graph.degree graph left = 1);
  assert (Graph.degree graph right = 1);
  Graph.clear graph;
  assert (not (Graph.mem_edge graph left right));
  List.iter (Graph.init_register graph) [left; right];
  Graph.add_edge graph left right;
  assert (Graph.mem_edge graph left right);
  Regalloc_utils.set_function_specific_params []

let test_table () =
  let original = Reg.create Cmm.Val in
  let alias =
    Reg.create_alias
      (Reg.For_testing.with_loc original Proc.loc_exn_bucket.Reg.loc)
      ~typ:Cmm.Int
  in
  let make stamp typ =
    Reg.For_printing.create ~name:original.Reg.name ~typ
      ~stamp:(Reg.Stamp.of_int_unsafe stamp) ~preassigned:false ~loc:Unknown
  in
  let sparse = make 1000000 Cmm.Val in
  let negative = make (-1) Cmm.Int in
  let zero = make 0 Cmm.Int in
  let table = Table.create (Reg.For_testing.get_stamp ()) in
  let keys = [original; alias; sparse; negative; zero] in
  List.iteri (fun i reg -> Table.replace table reg (ref i)) keys;
  let preserved = List.map (Table.find table) keys in
  List.iteri
    (fun i reg ->
      let value = ref (100 + i) in
      Table.replace table reg value;
      assert (Table.find table reg == value);
      assert (!(List.nth preserved i) = i))
    keys;
  assert (Table.fold (fun _ value total -> total + !value) table 0 = 510);
  let missing = make 1000001 Cmm.Val in
  assert (match Table.find table missing with
    | exception Not_found -> true
    | _ -> false);
  Table.clear table;
  assert (Table.fold (fun _ _ n -> n + 1) table 0 = 0);
  List.iter
    (fun reg ->
      assert (match Table.find table reg with
        | exception Not_found -> true
        | _ -> false))
    keys;
  List.iteri (fun i reg -> Table.replace table reg (ref i)) (List.rev keys);
  assert (Table.fold (fun _ value total -> total + !value) table 0 = 10)

let test_state () =
  Regalloc_utils.set_function_specific_params [];
  let cfg =
    Cfg.create ~fun_name:"register_table_fixture" ~fun_args:[||]
      ~fun_codegen_options:[] ~fun_dbg:Debuginfo.none ~fun_contains_calls:false
      ~fun_num_stack_slots:(Stack_class.Tbl.make 0)
      ~fun_poll:Lambda.Default_poll
      ~next_instruction_id:(InstructionId.make_sequence ())
      ~fun_ret_type:Cmm.typ_void ~fun_phantom_lets:Backend_var.Map.empty
      ~allowed_to_be_irreducible:false
    |> fun cfg ->
    Cfg_with_layout.create cfg ~layout:(Doubly_linked_list.make_empty ())
    |> Cfg_with_infos.make
  in
  let affinity = Regalloc_affinity.compute cfg [] in
  let left = Reg.create Cmm.Val in
  let right = Reg.create Cmm.Val in
  let state =
    State.make ~initial:[left; right]
      ~stack_slots:(Regalloc_stack_slots.make ()) ~affinity ()
  in
  assert (State.reg_work_list state left = Regalloc_irc_utils.RegWorkList.Initial);
  assert (State.color state left = None);
  assert (State.color state right = None);
  let seen = ref [] in
  State.iter_and_clear_initial state ~f:(fun reg -> seen := reg :: !seen);
  assert (List.rev !seen = [left; right]);
  State.add_simplify_work_list state left;
  assert (State.choose_and_remove_simplify_work_list state == left);
  State.add_freeze_work_list state left;
  assert (State.mem_freeze_work_list state left);
  State.remove_freeze_work_list state left;
  State.add_spill_work_list state left;
  assert (State.mem_spill_work_list state left);
  State.remove_spill_work_list state left;
  let color = match Proc.loc_exn_bucket.Reg.loc with
    | Reg color -> color
    | Unknown | Stack _ -> assert false
  in
  State.set_color state left (Some color);
  assert (State.color state left = Some color);
  assert (State.color state right = None);
  State.add_coalesced_nodes state right;
  State.add_alias state right left;
  assert (State.find_alias state right == left);
  let move = Regalloc_utils.Instruction.dummy in
  State.add_move_list state left move;
  State.add_work_list_moves state move;
  assert (State.is_move_related state left);
  let moves = ref [] in
  State.iter_node_moves state left ~f:(fun move -> moves := move :: !moves);
  assert (!moves = [move]);
  assert (State.choose_and_remove_work_list_moves state == move);
  assert (not (State.is_move_related state left));
  let later = Reg.create Cmm.Val in
  State.add_initial_one state later;
  assert (State.color state later = None);
  State.reset state ~new_inst_temporaries:[later] ~new_block_temporaries:[];
  assert (State.color state left = None);
  assert (State.find_alias state right == right);
  assert (State.color state later = None);
  assert (not (State.is_move_related state left));
  assert (State.reg_work_list state later = Regalloc_irc_utils.RegWorkList.Initial)

let () =
  test_graph ();
  test_table ();
  test_state ()
