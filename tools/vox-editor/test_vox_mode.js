// Headless token test for the vox CodeMirror mode (vox-mode.js).
//
// CodeMirror 5's runmode addon is not vendored here, so this drives the
// real mode through a tiny runMode (CodeMirror.voxTokenize, defined in
// vox-mode.js) that uses only the public getMode / startState /
// StringStream API.  CodeMirror's core loads in node under a minimal DOM
// stub; the two mode files are evaluated through their plain-browser UMD
// branch (their CommonJS require path assumes a different vendor layout).
//
// Run: node test_vox_mode.js   (exits non-zero on the first mismatch)

const assert = require("assert");
const fs = require("fs");
const path = require("path");

// --- load CodeMirror + mllike + vox-mode in node ------------------------
const HERE = __dirname;
global.navigator = { userAgent: "node", platform: "node", vendor: "" };
function elStub() {
  return {
    style: {}, className: "",
    setAttribute() {}, getAttribute() { return null; },
    appendChild() {}, removeChild() {}, insertBefore() {},
    cloneNode() { return elStub(); },
    addEventListener() {}, removeEventListener() {},
    getElementsByTagName() { return []; },
    childNodes: [], firstChild: null, options: [],
  };
}
global.document = {
  createElement: elStub, createElementNS: elStub, createTextNode: elStub,
  documentElement: elStub(), body: elStub(),
  createRange: () => ({
    setEnd() {}, setStart() {},
    getBoundingClientRect() { return {}; }, getClientRects() { return []; },
  }),
  addEventListener() {},
};
global.window = global;
global.window.getComputedStyle = () => ({});

const CM = require(path.join(HERE, "vendor/codemirror/codemirror.js"));
function loadBrowserUMD(p) {
  const code = fs.readFileSync(p, "utf8");
  // module/exports/define left undefined -> the file's `else mod(CodeMirror)`
  // (plain browser) branch runs and registers against CM.
  new Function("CodeMirror", "module", "exports", "define", code)(CM);
}
loadBrowserUMD(path.join(HERE, "vendor/codemirror/mode/mllike/mllike.js"));
loadBrowserUMD(path.join(HERE, "vox-mode.js"));

// The class-bearing token subsequence: [text=class, ...], dropping tokens
// the mode leaves uncoloured (brackets, plain Lean names, whitespace).
function classes(src) {
  return CM.voxTokenize(src)
    .filter((t) => t[1] !== null && t[0].trim() !== "")
    .map((t) => t[0] + "=" + t[1]);
}

let failures = 0;
function check(name, src, expected) {
  const got = classes(src);
  try {
    assert.deepStrictEqual(got, expected);
    console.log("ok   - " + name);
  } catch (e) {
    failures++;
    console.log("FAIL - " + name);
    console.log("  expected: " + JSON.stringify(expected));
    console.log("  got:      " + JSON.stringify(got));
  }
}
function refute(name, src, absentClass) {
  const got = classes(src);
  if (got.some((t) => t.endsWith("=" + absentClass))) {
    failures++;
    console.log("FAIL - " + name + " (unexpected " + absentClass + ")");
    console.log("  got: " + JSON.stringify(got));
  } else {
    console.log("ok   - " + name);
  }
}

// --- refinements: braces, the hole, italic interior ---------------------
// Every token INSIDE the braces carries an extra `vox-refine-body` class
// (rendered italic) on top of its ordinary class; the { } frame does not.
check(
  "refinement: delim + hole + italic interior",
  "let div (a : int) (b : int{ not (_ = 0) }) : int = a / b",
  ["let=keyword", "div=variable", "a=variable", ":=operator", "int=type",
   "b=variable", ":=operator", "int=type", "{=vox-refine-delim",
   "not=variable vox-refine-body", "_=vox-hole vox-refine-body",
   "==operator vox-refine-body", "0=number vox-refine-body",
   "}=vox-refine-delim", ":=operator", "int=type", "==operator",
   "a=variable", "b=variable"]
);
// Consistency: a spec function like `len` is a plain variable inside a
// refinement, exactly as it is in program code -- the same name never
// pops in one place and lies flat in another (only the italic body and
// the brace colour set the refinement apart).
check(
  "refinement: spec functions read as plain variables, italic",
  "let rec append (a : ilist) (b : ilist) : ilist{ len _ = len a + len b } = b",
  ["let=keyword", "rec=keyword", "append=variable", "a=variable",
   ":=operator", "ilist=variable", "b=variable", ":=operator",
   "ilist=variable", ":=operator", "ilist=variable", "{=vox-refine-delim",
   "len=variable vox-refine-body", "_=vox-hole vox-refine-body",
   "==operator vox-refine-body", "len=variable vox-refine-body",
   "a=variable vox-refine-body", "+=operator vox-refine-body",
   "len=variable vox-refine-body", "b=variable vox-refine-body",
   "}=vox-refine-delim", "==operator", "b=variable"]
);
// The same `len`, in a plain function body, is an identical plain variable
// (no special class in OR out of a refinement).
check(
  "consistency: spec function in program code is a plain variable",
  "let n = len a",
  ["let=keyword", "n=variable", "==operator", "len=variable", "a=variable"]
);
refute("spec-app class is gone entirely (program code)",
  "let n = len a", "vox-spec-app");
refute("spec-app class is gone entirely (refinement)",
  "let f : int{ len _ = 0 } = g", "vox-spec-app");

// --- ghost markers and quantifiers --------------------------------------
check(
  "marker: trailing-underscore name + [@@vox.decreases]",
  "let rec total_ fib n = fib (n - 1)\n[@@vox.decreases n]",
  ["let=keyword", "rec=keyword", "total_=vox-marker", "fib=variable",
   "n=variable", "==operator", "fib=variable", "n=variable", "-=operator",
   "1=number", "[@@=vox-attr", "vox.decreases=vox-attr-name", "n=variable"]
);
check(
  "quantifier: exists_ binder in a refinement (italic interior)",
  "let six : unit{ exists_ y. y = 3 && 6 = 2 * y } = ()",
  ["let=keyword", "six=variable", ":=operator", "unit=type",
   "{=vox-refine-delim", "exists_=vox-quant vox-refine-body",
   "y=variable vox-refine-body", ".=operator vox-refine-body",
   "y=variable vox-refine-body", "==operator vox-refine-body",
   "3=number vox-refine-body", "&=operator vox-refine-body",
   "&=operator vox-refine-body", "6=number vox-refine-body",
   "==operator vox-refine-body", "2=number vox-refine-body",
   "*=operator vox-refine-body", "y=variable vox-refine-body",
   "}=vox-refine-delim", "==operator"]
);

// --- let mutable, <- assignment, for-loops ------------------------------
check(
  "mutable: let mutable + <- assignment",
  "let f () : int{ _ = 4 } =\n  let mutable m = 3 in\n  m <- m + 1;\n  m",
  ["let=keyword", "f=variable", ":=operator", "int=type",
   "{=vox-refine-delim", "_=vox-hole vox-refine-body",
   "==operator vox-refine-body", "4=number vox-refine-body",
   "}=vox-refine-delim", "==operator", "let=keyword", "mutable=vox-mutable",
   "m=variable", "==operator", "3=number", "in=keyword", "m=variable",
   "<-=vox-assign", "m=variable", "+=operator", "1=number", "m=variable"]
);
check(
  "for-loop: `for` keyword (mllike omits it) + [@vox.invariant]",
  "for i = 1 to n do x <- x + 1 done [@vox.invariant x = i - 1]",
  ["for=keyword", "i=variable", "==operator", "1=number", "to=keyword",
   "n=variable", "do=keyword", "x=variable", "<-=vox-assign", "x=variable",
   "+=operator", "1=number", "done=keyword", "[@=vox-attr",
   "vox.invariant=vox-attr-name", "x=variable", "==operator", "i=variable",
   "-=operator", "1=number"]
);

// --- embedded Lean block ------------------------------------------------
check(
  "lean block: extension + delimiters + Lean interior (kw/sort/tactic/op/comment)",
  "[%%vox.lean {lean|\ntheorem t (n : Int) : fib n = fib n := by\n  grind -- ok\n|lean}]",
  ["[%%=vox-attr", "vox.lean=vox-attr-name", "{lean|=vox-lean-delim",
   "theorem=vox-lean-keyword", "Int=vox-lean-sort", ":==vox-lean-op",
   "by=vox-lean-keyword", "grind=vox-lean-tactic", "-- ok=vox-lean-comment",
   "|lean}=vox-lean-delim"]
);
check(
  "lean block: nested /- -/ block comment spans lines",
  "[%%vox.lean {lean|\n/- b -/\ndef d (a : VoxU) : Prop := a\n|lean}]",
  ["[%%=vox-attr", "vox.lean=vox-attr-name", "{lean|=vox-lean-delim",
   "/-=vox-lean-comment", " b -/=vox-lean-comment", "def=vox-lean-keyword",
   "VoxU=vox-lean-sort", "Prop=vox-lean-sort", ":==vox-lean-op",
   "|lean}=vox-lean-delim"]
);

// --- pane predicate text: refine-start option ---------------------------
// The proof pane tokenizes bare predicate fragments (no surrounding
// `type{ ... }`) with { refine: 1 }, so they colour + italicize exactly
// like the buffer's refinement interior. Same tokens, same classes.
(function () {
  const got = CM.voxTokenize("len r = len t + len b", { refine: 1 })
    .filter((t) => t[1] !== null && t[0].trim() !== "")
    .map((t) => t[0] + "=" + t[1]);
  const expected = [
    "len=variable vox-refine-body", "r=variable vox-refine-body",
    "==operator vox-refine-body", "len=variable vox-refine-body",
    "t=variable vox-refine-body", "+=operator vox-refine-body",
    "len=variable vox-refine-body", "b=variable vox-refine-body",
  ];
  try {
    assert.deepStrictEqual(got, expected);
    console.log("ok   - pane: refine-start tokenizes bare predicate italic");
  } catch (e) {
    failures++;
    console.log("FAIL - pane: refine-start tokenizes bare predicate italic");
    console.log("  expected: " + JSON.stringify(expected));
    console.log("  got:      " + JSON.stringify(got));
  }
  // textContent invariant: the concatenation of token texts is the input
  // byte-for-byte (the pane relies on this to keep provenance keys + layout).
  const round = CM.voxTokenize("not (i = 0)", { refine: 1 })
    .map((t) => t[0]).join("");
  try {
    assert.strictEqual(round, "not (i = 0)");
    console.log("ok   - pane: tokenizer preserves text byte-for-byte");
  } catch (e) {
    failures++;
    console.log("FAIL - pane: tokenizer preserves text byte-for-byte");
  }
})();

// --- negatives: the safe-zone gating -----------------------------------
// A record brace is NOT a refinement brace; `mutable` still reads as vox.
check(
  "record: brace is not a refinement, mutable is vox",
  "type varr = { mutable arr : int array }",
  ["type=keyword", "varr=variable", "==operator", "mutable=vox-mutable",
   "arr=variable", ":=operator", "int=type", "array=variable"]
);
refute("record brace is not vox-refine-delim",
  "type varr = { mutable arr : int array }", "vox-refine-delim");
// A {| ... |} quoted string is one string token, hole/braces inert inside.
refute("quoted string interior inert",
  "let s = {|hi { _ } there|}", "vox-hole");
// Comment and string interiors never trigger vox tokens.
refute("comment interior inert (hole)",
  "(* int{ _ = 0 } total_ x *)\nlet y = 1", "vox-hole");
refute("comment interior inert (marker)",
  "(* int{ _ = 0 } total_ x *)\nlet y = 1", "vox-marker");
refute("string interior inert (hole)",
  'let s = "int{ _ } total_"', "vox-hole");
refute("string interior inert (refine delim)",
  'let s = "int{ _ } total_"', "vox-refine-delim");

if (failures) {
  console.log("\n" + failures + " FAILURE(S)");
  process.exit(1);
}
console.log("\nALL VOX-MODE TOKEN TESTS PASSED");
