# A short guide to refinements

A refinement type is an ordinary type carrying a proof obligation. The value in
`int{ _ > 0 }` is a plain `int`, but every value that flows into that type must
satisfy the predicate `_ > 0`, where `_` stands for the value being constrained.
The verifier discharges each obligation for you.

Every snippet below is taken verbatim from a curated example in the `Examples`
explorer, so you can open it and watch it verify.

## Contracts at the call site

A refined parameter is a precondition. The obligation lands wherever the
function is called:

```
let positive (x : int{ _ > 0 }) = x

(* The call site carries the obligation [7 > 0], discharged here. *)
let seven = positive 7
```

Open **Sixty seconds** (`overview.ml`) and put the cursor on `positive 7` to see
the discharged obligation.

## Branches become facts

Inside a conditional, the verifier learns the test in the `then` branch and its
negation in the `else` branch:

```
let abs (x : int) =
  (if x >= 0 then x else 0 - x : int{ _ >= 0 })
```

In the `then` branch it knows `x >= 0`; in the `else` branch it knows the
negation, so `0 - x` is nonnegative too. Either path meets the annotation
`int{ _ >= 0 }`. This is **Branches become facts** (`abs.ml`).

## Where to go next

- **Guarding a refined call** — a precondition discharged from an `if` guard.
- **Let-binder facts** — a refined `let` recorded as a fact for later.
- **Recursion by induction** — a recursive call's refined result as the
  induction hypothesis.
- **When you're wrong** — a deliberately false obligation, reported disproved.

Each is one click away in the explorer.
