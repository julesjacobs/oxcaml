import { test, after } from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import { Worker } from "node:worker_threads";
import createRuntime from "../public/assets/vox-runtime.js";
import { lessons } from "../src/lessons.js";
const asset = (name) =>
  fs.readFileSync(new URL("../public/assets/" + name, import.meta.url));
const manifest = JSON.parse(asset("stdlib-manifest.json")),
  packed = asset("stdlib.data"),
  compiler = asset("compiler.byte");
const solver = new Worker(new URL("./solver.mjs", import.meta.url));
after(() => solver.terminate());
async function compile(source, answer) {
  const models = [];
  const events = [],
    messages = [];
  const runtime = await createRuntime({
    noInitialRun: true,
    preRun: [
      (m) => {
        m.ENV.OCAMLLIB = "/stdlib";
        m.ENV.OCAMLRUNPARAM = "d=1,s=256k";
      },
    ],
    print: (s) => messages.push(s),
    printErr: (s) => messages.push(s),
    voxEvent: (e) => events.push(e),
    voxSolve: (script) => {
      if (answer !== undefined) return answer;
      const control = new SharedArrayBuffer(1024 * 1024),
        view = new Int32Array(control);
      solver.postMessage({
        script,
        control,
        proof: events.findLast((e) => e.type === "proof"),
      });
      assert.notEqual(
        Atomics.wait(view, 0, 0, 15000),
        "timed-out",
        "Z3 worker responded",
      );
      const size = Atomics.load(view, 2);
      if (size)
        models.push(
          JSON.parse(
            new TextDecoder().decode(new Uint8Array(control, 12, size)),
          ),
        );
      return Atomics.load(view, 1);
    },
  });
  runtime.FS.mkdir("/stdlib");
  runtime.FS.mkdir("/work");
  for (const f of manifest)
    runtime.FS.writeFile(
      "/stdlib/" + f.name,
      packed.subarray(f.offset, f.offset + f.length),
    );
  runtime.FS.writeFile("/compiler.byte", compiler);
  runtime.FS.writeFile("/work/lesson.ml", source);
  const previousExitCode = process.exitCode;
  try {
    runtime.callMain(["/compiler.byte"]);
  } finally {
    process.exitCode = previousExitCode;
  }
  const success = events.some((e) => e.type === "compiled");
  return {
    events,
    models,
    messages,
    success,
    bytecode: success ? runtime.FS.readFile("/work/lesson.byte") : null,
  };
}
async function execute(bytecode) {
  const output = [];
  const runtime = await createRuntime({
    noInitialRun: true,
    print: (s) => output.push(s),
    printErr: (s) => output.push(s),
    preRun: [
      (m) => {
        m.ENV.OCAMLRUNPARAM = "d=1,s=256k";
      },
    ],
    voxEvent: () => {},
    voxSolve: () => {
      throw Error("Unexpected runtime proof");
    },
  });
  runtime.FS.mkdir("/work");
  runtime.FS.writeFile("/work/lesson.byte", bytecode);
  const previousExitCode = process.exitCode;
  let exitCode;
  try {
    exitCode = runtime.callMain(["/work/lesson.byte"]);
  } finally {
    process.exitCode = previousExitCode;
  }
  return { exitCode, output: output.join("\n") };
}
const code = (id) => lessons.find((l) => l.id === id).code;
for (const lesson of lessons)
  test("compile and execute " + lesson.id, async () => {
    const result = await compile(lesson.code);
    assert.equal(result.success, true, JSON.stringify(result.events));
    const execution = await execute(result.bytecode);
    if (lesson.id === "testing") {
      assert.notEqual(execution.exitCode, 0);
      assert.match(execution.output, /Expected 6; got 10/);
    } else assert.equal(execution.exitCode, 0, execution.output);
    if (lesson.id === "integers") {
      assert.match(execution.output, /integer size = 63 bits/);
      assert.match(execution.output, /max_int = 4611686018427387903/);
      assert.match(execution.output, /max_int \+ 1 = -4611686018427387904/);
    }
    const metadata = result.events.find((e) => e.type === "metadata");
    assert.ok(metadata.hovers.length > 0);
  });
for (const [name, source] of [
  [
    "wrong result",
    code("windows").replace(
      "let result = stop - start",
      "let result = stop + start",
    ),
  ],
  [
    "non-decreasing recursion",
    code("total").replace("countdown (n - 1)", "countdown n"),
  ],
  ["wrong optimization", code("lemmas").replace("Lit (a + b)", "Lit (a - b)")],
  [
    "ghost-to-real flow",
    code("ghost").replace("let result = r.value", "let result = r.previous"),
  ],
  ["syntax error", "let x ="],
])
  test("reject " + name, async () => {
    const result = await compile(source);
    assert.equal(result.success, false);
    assert.ok(result.events.some((e) => e.type === "diagnostic"));
  });
for (const answer of [2, 3, 4, 5])
  test("unproved solver result " + answer + " never executes", async () => {
    const result = await compile(code("windows"), answer);
    assert.equal(result.success, false);
    assert.equal(result.bytecode, null);
  });
test("runtime input check fails independently of verification", async () => {
  const result = await compile(
    code("windows").replace("width 10 4 10", "width 10 11 10"),
  );
  assert.equal(result.success, true);
  assert.notEqual((await execute(result.bytecode)).exitCode, 0);
});
test("GC and allocation in the Wasm runtime", async () => {
  const result = await compile(
    'let () = for i = 1 to 50 do let a = Array.init 20000 (fun n -> string_of_int n) in Gc.full_major (); assert (a.(19999) = "19999") done; print_endline "gc ok"',
  );
  assert.equal(result.success, true, JSON.stringify(result.events));
  const execution = await execute(result.bytecode);
  assert.equal(execution.exitCode, 0);
  assert.match(execution.output, /gc ok/);
});

test("testing lesson repairs its implementation after strengthening the contract", async () => {
  const source = code("testing")
    .replace("{w : int | 0 <= w", "{w : int | w = stop - start && 0 <= w")
    .replace("let result = length", "let result = stop - start");
  const result = await compile(source);
  assert.equal(result.success, true, JSON.stringify(result.events));
  assert.equal((await execute(result.bytecode)).exitCode, 0);
});
test("Wasm architecture is reported consistently", async () => {
  const result = await compile(
    "let () = assert (Sys.arch = Sys.Wasm); assert (Sys.word_size = 64); assert (Sys.int_size = 63)",
  );
  assert.equal(result.success, true);
  assert.equal((await execute(result.bytecode)).exitCode, 0);
});

test("failed window obligation has named exact counterexample values", async () => {
  const result = await compile(
    code("windows").replace(
      "let result = stop - start",
      "let result = stop + start",
    ),
  );
  assert.equal(result.success, false);
  const proof = result.events.find((e) => e.type === "proof");
  assert.ok(proof.goal.text.includes("stop"));
  assert.ok(proof.assumptions.length > 0);
  assert.equal(result.models.length, 1);
  const values = result.models[0].values;
  assert.ok(values.some((v) => v.name === "start"));
  assert.ok(values.every((v) => v.available));
  for (const value of values.filter((v) => v.kind === "int63"))
    assert.equal(
      BigInt.asIntN(63, BigInt(value.value)).toString(),
      value.value,
    );
});
test("successful obligations expose goals without invented counterexamples", async () => {
  const result = await compile(code("windows"));
  assert.equal(result.success, true);
  assert.equal(result.models.length, 0);
  for (const proof of result.events.filter((e) => e.type === "proof")) {
    assert.ok(proof.goal.label);
    assert.ok(proof.goal.text);
    assert.ok(proof.symbols.length);
  }
});

test("window counterexamples prefer small inputs and satisfy the displayed conditions", async () => {
  const result = await compile(
    code("windows").replace(
      "let result = stop - start",
      "let result = stop + start",
    ),
  );
  const model = result.models[0];
  assert.equal(model.small, true);
  const inputs = Object.fromEntries(
    model.values
      .filter((v) => !v.internal)
      .map((v) => [v.name, BigInt(v.value)]),
  );
  assert.deepEqual(inputs, { length: 1n, start: 1n, stop: 1n });
  const proof = result.events.find((e) => e.type === "proof");
  assert.equal(proof.assumptions.length, 5);
  assert.ok(!proof.goal.text.includes("reachable["));
  assert.ok(proof.assumptions.every((a) => !a.text.includes("value[")));
});
test("small model search preserves the original countermodel when bounds are unsatisfiable", async () => {
  const result = await compile(
    code("windows")
      .replace("let result = stop - start", "let result = stop + start")
      .replace("0 <= n && n <= length", "10 <= n && n <= length"),
  );
  assert.equal(result.success, false);
  assert.equal(result.models.length, 1);
  assert.ok(!result.models[0].small);
  assert.ok(
    BigInt(result.models[0].values.find((v) => v.name === "start").value) >=
      10n,
  );
});

test('a failed induction proof returns constructed countermodel values', async () => {
  const result = await compile(code('lemmas').replace('Lit (a + b)', 'Lit (a - b)'));
  assert.equal(result.success, false);
  assert.ok(result.models.some(model => model.values.some(v => v.kind === 'datatype' && v.available)));
});
