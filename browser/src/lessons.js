export const lessons = [
  {
    id: "windows",
    chapter: "01",
    label: "Refinement types",
    title: "A window with a contract",
    intro: "A window starts and ends inside a buffer. Its width should be exactly the distance between those endpoints.",
    body: "The result type states that relationship. <code>assume_</code> checks the inputs at runtime; Vox proves the result contract automatically.",
    challenge: "Replace <code>stop - start</code> in the implementation with <code>stop + start</code>. The contract should catch the mistake before execution.",
    detail: "A proved result contract applies when the function returns normally. Invalid endpoints still fail the runtime check.",
    code: `external ( <= ) : int -> int -> bool @@ total = "%lessequal"

let width :
    (length : int) -> (start : int) -> (stop : int) ->
    {w : int | w = stop - start && 0 <= w && w <= length} =
  fun length start stop ->
  let length : {n : int | 0 <= n} = assume_ length in
  let start : {n : int | 0 <= n && n <= length} = assume_ start in
  let stop : {n : int | start <= n && n <= length} = assume_ stop in
  let result = stop - start in
  result

let () =
  let result = width 10 4 10 in
  Printf.printf "width = %d\\n" result
`
  },
  {
    id: "testing",
    chapter: "02",
    label: "Testing and verification",
    title: "A proof needs the right question",
    intro: "A bound can be true even when the program computes the wrong answer. Tests help challenge the specification itself.",
    body: "This deliberately weak contract promises only a bounded result. The implementation returns the buffer length. Vox can prove that contract, but the test expects the actual window width.",
    challenge: "First repair the result contract to require <code>w = stop - start</code>. Observe the verification error. Then repair the implementation.",
    detail: "Verification and execution have separate results. A successful proof does not make a failing test disappear.",
    code: `external ( <= ) : int -> int -> bool @@ total = "%lessequal"

let width :
    (length : int) -> (start : int) -> (stop : int) ->
    {w : int | 0 <= w && w <= length} =
  fun length start stop ->
  let length : {n : int | 0 <= n} = assume_ length in
  let start : {n : int | 0 <= n && n <= length} = assume_ start in
  let stop : {n : int | start <= n && n <= length} = assume_ stop in
  let result = length in
  result

let () =
  let result = width 10 4 10 in
  Printf.printf "Expected 6; got %d\\n%!" result;
  assert (result = 6)
`
  },
  {
    id: "total",
    chapter: "03",
    label: "Termination checking",
    title: "Show that recursion makes progress",
    intro: "A result contract describes what comes back. The new total mode also constrains the computation that gets there.",
    body: "The recursive call decreases <code>n</code>. The guard prevents integer wraparound on subtraction. Remove the decrease and watch Vox reject the definition.",
    challenge: "Change <code>countdown (n - 1)</code> to <code>countdown n</code>. Then repair the recursive call.",
    detail: "Existing OxCaml modes are assumed familiar. Totality, statefulness and ghostness are distinct properties.",
    code: `let rec (countdown @ total) n =
  if n > 0 then countdown (n - 1) else 0
[@@decreases n]

let () =
  Printf.printf "countdown 5 = %d\\n" (countdown 5)
`
  },
  {
    id: "lemmas",
    chapter: "04",
    label: "Lemmas and induction",
    title: "Prove an optimization preserves its result",
    intro: "Constant folding changes an expression tree. Its value should stay the same for every input.",
    body: "A lemma is a function whose result contract states a fact. <code>[@def]</code> exposes a definition through a generated lemma. Recursive calls to <code>fold_correct</code> establish the induction hypotheses for the two subtrees.",
    challenge: "Change <code>Lit (a + b)</code> to <code>Lit (a - b)</code> in <code>add</code>. Its correctness lemma should fail.",
    detail: "This proof uses termination checking from the previous chapter. The lemma calls execute here; the next chapter introduces erased evidence.",
    code: `module Expr = struct
  type t = Lit of int | Input | Add of t * t [@@inductive]

  let[@def] rec (eval @ total) expression input =
    match expression with
    | Lit n -> n
    | Input -> input
    | Add (left, right) -> eval left input + eval right input

  let[@def] add (left @ total) (right @ total) : t @ total =
    match left, right with
    | Lit a, Lit b -> Lit (a + b)
    | Lit 0, _ -> right
    | _, Lit 0 -> left
    | _ -> Add (left, right)

  let (add_correct @ total) (left @ total) (right @ total) input :
      {u : unit |
        eval (add left right) input === eval left input + eval right input} =
    let result = add left right in
    add_def left right;
    eval_def left input;
    eval_def right input;
    eval_def result input;
    let u = () in
    match left, right with
    | Lit _, Lit _ -> u
    | Lit 0, _ -> u
    | _, Lit 0 -> u
    | _ -> u

  let[@def] rec fold (expression @ total) : t @ total =
    match expression with
    | Lit _ | Input -> expression
    | Add (left, right) -> add (fold left) (fold right)

  let rec (fold_correct @ total) :
      (expression : t) -> (input : int) ->
      {u : unit | eval (fold expression) input === eval expression input}
        @ immutable contended =
    fun expression input ->
    fold_def expression;
    eval_def expression input;
    let u = () in
    match expression with
    | Lit _ | Input -> u
    | Add (left, right) ->
      fold_correct left input;
      fold_correct right input;
      let left = fold left in
      let right = fold right in
      add_correct left right input;
      u

end

let () =
  let expression = Expr.Add (Expr.Input, Expr.Lit 0) in
  Expr.fold_correct expression 7;
  Printf.printf "folded result = %d\\n" (Expr.eval (Expr.fold expression) 7)
`
  },
  {
    id: "ghost",
    chapter: "05",
    label: "Ghost code and data",
    title: "Keep evidence without storing it",
    intro: "A result can carry a real value and an erased observation for later reasoning.",
    body: "The new <code>ghost</code> mode keeps <code>previous</code> available to specifications. It cannot supply an executable value. The consumer uses the saved observation only in its result contract.",
    challenge: "Try returning <code>r.previous</code> from <code>current</code>. The compiler should reject the ghost-to-real flow.",
    detail: "The ghost field carries data. It is more than an erased call to a proof function.",
    code: `external ( <= ) : int -> int -> bool @@ total = "%lessequal"

type step = {
  value : int;
  previous : int @@ ghost;
}

let (remember_max @ total) x acc :
    {r : step | r.previous = acc && x <= r.value && acc <= r.value} =
  let value = if x >= acc then x else acc in
  let result = { value; previous = ghost_ acc } in
  result

let current :
    (r : {s : step | s.previous <= s.value}) ->
    {v : int | let s = r in s.previous <= v} = fun r ->
  let result = r.value in
  result

let () =
  let step = remember_max 7 3 in
  let value = current step in
  Printf.printf "current = %d\\n" value
`
  },
  {
    id: "integers",
    chapter: "06",
    label: "63-bit execution",
    title: "The same integers in the browser",
    intro: "Verification and execution must agree on machine-integer behavior, including overflow.",
    body: "This runtime preserves the 63-bit integers used by Vox. The compiler and the program both execute inside WebAssembly on your device.",
    challenge: "Try other boundary expressions. The printed integer size should remain <code>63</code>.",
    detail: "This prototype runs single-domain bytecode programs. Parallel-domain execution and the remaining tutorial chapters are still to be added.",
    code: `let () =
  Printf.printf "integer size = %d bits\\n" Sys.int_size;
  Printf.printf "max_int = %d\\n" max_int;
  Printf.printf "max_int + 1 = %d\\n" (max_int + 1);
  Printf.printf "min_int = %d\\n" min_int;
  assert (max_int + 1 = min_int)
`
  }
];
