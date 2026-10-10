// Loads Z3 (WebAssembly) and the Vox checker (js_of_ocaml) into one global
// scope and returns a synchronous check function. Used by worker.js in the
// browser and by differential/check-node.js in Node.
//
//   const check = await createChecker({ initZ3, loadChecker, fetchBytes });
//   check('main.ml', source, wantLambda)
//     -> { status, output, locations, lambda, queries, solverMs, totalMs }
//
// initZ3 is the Emscripten factory from z3-solver's z3-built.js,
// loadChecker(): evaluates vox.js and returns its exports, and
// fetchBytes(path): the contents of a file of the site as a Uint8Array.

async function createChecker({ initZ3, loadChecker, fetchBytes, z3Options = {} }) {
  const z3 = await initZ3(z3Options);
  // Z3 runs timeouts (its :timeout option, and the time limits inside its
  // default tactics) on timer threads, which it starts on first use and then
  // reuses. Each thread is a worker. A worker started while this one runs a
  // synchronous check could not load until the check returned, so Z3 would
  // wait for it forever: start the workers now.
  if (z3.PThread) {
    for (let i = 0; i < 4; i++) z3.PThread.allocateUnusedWorker();
    await Promise.all(z3.PThread.unusedWorkers.map((worker) => z3.PThread.loadWasmModuleToWorker(worker)));
  }
  const evaluate = (context, text) =>
    z3.ccall('Z3_eval_smtlib2_string', 'string', ['number', 'string'], [context, text]);
  const newContext = () => {
    const config = z3._Z3_mk_config();
    const context = z3._Z3_mk_context_rc(config);
    z3._Z3_del_config(config);
    return context;
  };
  let context = newContext();
  const version = evaluate(context, '(get-info :version)').trim()
    .replace(/^\(:version "(.*)"\)$/, '$1');
  // The verifier checks it against the version proofs are checked with, as it
  // checks what `z3 -version` reports natively (vox_smt_solver.ml).
  globalThis.voxZ3Version = version;

  let queries = 0;
  let solverMs = 0;
  // The verifier's only way to the solver: SMT-LIB text in, Z3's output out.
  // Each query starts with (reset). A Z3 process restarts its resource count
  // there, but a context of the API keeps counting across resets, so every
  // query gets a fresh context: resource counts and limits then mean what
  // they mean natively.
  globalThis.voxZ3Eval = (text) => {
    const started = performance.now();
    try {
      if (text.startsWith('(reset)')) {
        z3._Z3_del_context(context);
        context = newContext();
        queries += 1;
      }
      return evaluate(context, text);
    } finally {
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
      // Where the output prints each location, in UTF-8 bytes, with the
      // location itself: see vox_playground.ml.
      locations: Array.from(result.locations, (l) => ({
        first: l.first, last: l.last, role: l.role, severity: l.severity, file: l.file,
        start: { ...l.start }, end: { ...l.end },
      })),
      lambda: result.lambda,
      queries,
      solverMs,
      totalMs: performance.now() - started,
    };
  }
  check.z3Version = version;
  check.revision = index.revision;
  return check;
}

if (typeof module !== 'undefined') module.exports = { createChecker };
