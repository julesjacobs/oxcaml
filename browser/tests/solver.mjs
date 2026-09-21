import { findCountermodel } from "../src/countermodel.js";
import { parentPort } from "node:worker_threads";
import { init } from "z3-solver";
const { Z3 } = await init();
parentPort.on("message", async ({ script, control, proof }) => {
  const view = new Int32Array(control),
    config = Z3.mk_config(),
    ctx = Z3.mk_context(config);
  Z3.del_config(config);
  try {
    const result = await Z3.eval_smtlib2_string(ctx, script);
    if (/^sat\b/.test(result)) {
      const model = await findCountermodel(Z3, ctx, proof, script);
      const bytes = new TextEncoder().encode(JSON.stringify(model));
      if (bytes.length > control.byteLength - 12)
        throw new Error("Test model too large");
      new Uint8Array(control, 12, bytes.length).set(bytes);
      Atomics.store(view, 2, bytes.length);
    }
    Atomics.store(
      view,
      1,
      /^unsat\b/.test(result) ? 0 : /^sat\b/.test(result) ? 1 : 3,
    );
  } catch {
    Atomics.store(view, 1, 4);
  } finally {
    Z3.del_context(ctx);
    Atomics.store(view, 0, 1);
    Atomics.notify(view, 0);
  }
});
