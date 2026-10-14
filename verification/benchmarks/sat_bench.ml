open Vox_sat_solver

exception Benchmark_timeout

let read_dimacs path =
  let channel = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () ->
      let variables = ref (-1) and expected = ref (-1) in
      let clauses = ref [] and clause = ref [] in
      (try
         while true do
           let line = String.trim (input_line channel) in
           if line <> "" && line.[0] <> 'c'
           then
             if line.[0] = 'p'
             then
               Scanf.sscanf line "p cnf %d %d" (fun n m ->
                   if !variables >= 0 || n < 0 || m < 0
                   then failwith ("invalid DIMACS header: " ^ path);
                   variables := n;
                   expected := m)
             else
               String.split_on_char ' '
                 (String.map (fun c -> if c = '\t' then ' ' else c) line)
               |> List.iter (fun word ->
                   if word <> ""
                   then begin
                     let literal = int_of_string word in
                     if literal = 0
                     then begin
                       clauses := Array.of_list (List.rev !clause) :: !clauses;
                       clause := []
                     end
                     else begin
                       if literal = min_int || abs literal > !variables
                       then failwith ("invalid DIMACS literal: " ^ path);
                       clause := literal :: !clause
                     end
                   end)
         done
       with End_of_file -> ());
      if !variables < 0 || !clause <> [] || List.length !clauses <> !expected
      then failwith ("incomplete DIMACS: " ^ path);
      !variables, Array.of_list (List.rev !clauses))

let status clauses assumptions = function
  | Sat model ->
    let holds literal =
      if literal > 0 then model.(literal) else not model.(-literal)
    in
    if
      not
        (Array.for_all (Array.exists holds) clauses
        && Array.for_all holds assumptions)
    then failwith "invalid SAT model";
    'S'
  | Unsat core ->
    if not (Array.for_all (fun literal -> Array.mem literal assumptions) core)
    then failwith "invalid assumption core";
    'U'
  | Unknown -> raise Benchmark_timeout

let timed f =
  let start = Sys.time () in
  let result = f () in
  result, Sys.time () -. start

let median values =
  let values = Array.of_list values in
  Array.sort Float.compare values;
  let middle = Array.length values / 2 in
  if Array.length values land 1 = 0
  then (values.(middle - 1) +. values.(middle)) /. 2.
  else values.(middle)

let run ~repeats ~timeout ~assumption_vars path =
  let variables, clauses = read_dimacs path in
  let create () =
    let solver = create ~variables () in
    Array.iter (add_clause solver) clauses;
    solver
  in
  let solve solver assumptions =
    let deadline = Unix.gettimeofday () +. timeout in
    let should_stop () = Unix.gettimeofday () >= deadline in
    solve ~assumptions ~should_stop solver
  in
  let queries =
    Array.init
      (2 * min assumption_vars variables)
      (fun i -> [| (if i land 1 = 0 then (i / 2) + 1 else -((i / 2) + 1)) |])
  in
  let loads = ref [] and cold = ref [] and warm = ref [] and fresh = ref [] in
  let totals = ref [] in
  let answer = ref 'X' and answers = ref "" in
  let stats = ref (statistics (Vox_sat_solver.create ())) in
  for _ = 1 to repeats do
    Gc.full_major ();
    let solver, load = timed create in
    let result, elapsed = timed (fun () -> solve solver [||]) in
    let next_answer = status clauses [||] result in
    if !answer <> 'X' && !answer <> next_answer
    then failwith "SAT result changed between repetitions";
    answer := next_answer;
    stats := statistics solver;
    loads := load :: !loads;
    cold := elapsed :: !cold;
    totals := (load +. elapsed) :: !totals;
    let elapsed = ref 0. in
    let results =
      Array.map
        (fun assumptions ->
          let result, duration = timed (fun () -> solve solver assumptions) in
          elapsed := !elapsed +. duration;
          status clauses assumptions result)
        queries
    in
    warm := !elapsed :: !warm;
    let next_answers = String.init (Array.length results) (Array.get results) in
    if !answers <> "" && !answers <> next_answers
    then failwith "Assumption results changed between repetitions";
    answers := next_answers;
    elapsed := 0.;
    Array.iteri
      (fun i assumptions ->
        let result, duration =
          timed (fun () -> solve (create ()) assumptions)
        in
        elapsed := !elapsed +. duration;
        if status clauses assumptions result <> results.(i)
        then failwith ("incremental/fresh result mismatch: " ^ path))
      queries;
    fresh := !elapsed :: !fresh
  done;
  let stats = !stats in
  Printf.printf
    ("%s\t%d\t%d\t%c\t%.9f\t%.9f\t%.9f\t%.9f\t%.9f\t%s"
   ^^ "\t%d\t%d\t%d\t%d\t%d\t%d\n%!")
    path variables (Array.length clauses) !answer (median !loads) (median !cold)
    (median !totals) (median !warm) (median !fresh) !answers stats.decisions
    stats.propagations stats.conflicts stats.restarts stats.learnt_clauses
    stats.deleted_clauses

let () =
  let repeats = ref 5 and timeout = ref 30. and assumption_vars = ref 8 in
  let paths = ref [] in
  Arg.parse
    [ "--repeats", Arg.Set_int repeats, "Number of repetitions";
      "--timeout", Arg.Set_float timeout, "Seconds per solve";
      ( "--assumption-vars",
        Arg.Set_int assumption_vars,
        "Query both polarities of this many variables (zero for cold only)" ) ]
    (fun path -> paths := path :: !paths)
    "sat_bench [options] FILE.cnf ...";
  if !repeats <= 0 || !timeout <= 0. || !assumption_vars < 0
  then invalid_arg "positive repeats and timeout required";
  List.iter
    (fun path ->
      try
        run ~repeats:!repeats ~timeout:!timeout
          ~assumption_vars:!assumption_vars path
      with Benchmark_timeout -> Printf.printf "%s\tTIMEOUT\n%!" path)
    (List.rev !paths)
