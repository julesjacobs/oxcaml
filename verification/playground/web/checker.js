// Loads Z3 (WebAssembly) and the Vox checker (js_of_ocaml) into one global
// scope and returns a synchronous check function. Used by worker.js in the
// browser and by differential/check-node.js in Node.
//
//   const check = await createChecker({ initZ3, loadChecker, fetchBytes });
//   check('main.ml', source, wantLambda)
//     -> { status, output, lambda, queries, solverMs, totalMs }
//
// initZ3 is the Emscripten factory from z3-solver's z3-built.js,
// loadChecker(): evaluates vox.js and returns its exports, and
// fetchBytes(path): the contents of a file of the site as a Uint8Array.

async function createChecker({ initZ3, loadChecker, fetchBytes, z3Options = {} }) {
  const z3 = await initZ3(z3Options);
  const config = z3._Z3_mk_config();
  const context = z3._Z3_mk_context_rc(config);
  z3._Z3_del_config(config);
  const evaluate = (ctx, text) => z3.ccall('Z3_eval_smtlib2_string', 'string', ['number', 'string'], [ctx, text]);
  const version = evaluate(context, '(get-info :version)').trim();

  let queries = 0;
  let solverMs = 0;
  // The verifier's only way to the solver: SMT-LIB text in, Z3's output out.
  globalThis.voxZ3Eval = (text) => {
    const started = performance.now();
    try {
      return evaluate(context, text);
    } finally {
      queries += 1;
      solverMs += performance.now() - started;
    }
  };

  // The interfaces the checker reads: one bundle, with an index of
  // [path, offset, length] entries.
  const index = JSON.parse(new TextDecoder().decode(await fetchBytes('lib/index.json')));
  const bundle = await fetchBytes('lib/bundle.bin');
  const files = index.files.map(([path, offset, length]) => [path, bundle.subarray(offset, offset + length)]);

  const checker = loadChecker();
  for (const [path, bytes] of files) {
    let text = '';
    for (let i = 0; i < bytes.length; i += 0x8000) {
      text += String.fromCharCode.apply(null, bytes.subarray(i, i + 0x8000));
    }
    globalThis.jsoo_create_file('/static/' + path, text);
  }

  function check(name, source, wantLambda) {
    queries = 0;
    solverMs = 0;
    const started = performance.now();
    const result = checker.voxCheck(name, source, !!wantLambda);
    return {
      status: result.status,
      output: result.output,
      lambda: result.lambda,
      queries,
      solverMs,
      totalMs: performance.now() - started,
    };
  }
  check.z3Version = version.replace(/^\(:version "(.*)"\)$/, '$1');
  check.revision = index.revision;
  return check;
}

if (typeof module !== 'undefined') module.exports = { createChecker };
