import { findCountermodel } from "../src/countermodel.js";
importScripts("/assets/z3-built.js");
const { init } = require("z3-solver");
let api,
  current = null,
  queue = [];
let cancelledThrough = -1;
const finish = (request, status) => {
  const control = new Int32Array(request.control);
  Atomics.store(control, 1, status);
  Atomics.store(control, 0, 1);
  Atomics.notify(control, 0);
};
async function drain() {
  if (current || !api) return;
  const request = queue.shift();
  if (!request) return;
  if (request.revision <= cancelledThrough) {
    finish(request, 5);
    return drain();
  }
  const cfg = api.Z3.mk_config(),
    ctx = api.Z3.mk_context(cfg);
  api.Z3.del_config(cfg);
  current = { ...request, ctx };
  try {
    const output = await api.Z3.eval_smtlib2_string(
      ctx,
      request.script + "\n(get-info :reason-unknown)",
    );
    const status =
      request.revision <= cancelledThrough
        ? 5
        : /^unsat\b/.test(output)
          ? 0
          : /^sat\b/.test(output)
            ? 1
            : output.includes("timeout")
              ? 2
              : /^unknown\b/.test(output)
                ? 3
                : 4;
    let countermodel = null,
      modelError = null;
    if (status === 1) {
      try {
        countermodel = await findCountermodel(
          api.Z3,
          ctx,
          request.proof,
          request.script,
        );
      } catch (error) {
        modelError = String(error);
      }
    }
    if (request.revision > cancelledThrough)
      postMessage({
        type: "proof-result",
        revision: request.revision,
        index: request.proof?.index,
        status,
        countermodel,
        modelError,
        script: request.script,
      });
    finish(request, request.revision <= cancelledThrough ? 5 : status);
    if (status === 4)
      postMessage({
        type: "solver-error",
        message: output,
        revision: request.revision,
      });
  } catch (error) {
    finish(request, 4);
    postMessage({
      type: "solver-error",
      message: String(error),
      revision: request.revision,
    });
  } finally {
    api.Z3.del_context(ctx);
    current = null;
    drain();
  }
}
onmessage = ({ data }) => {
  if (data.type === "cancel") {
    cancelledThrough = Math.max(cancelledThrough, data.revision);
    if (current?.revision === data.revision) api.Z3.interrupt(current.ctx);
    queue = queue.filter((q) => {
      if (q.revision === data.revision) {
        finish(q, 5);
        return false;
      }
      return true;
    });
  } else if (data.type === "solve") {
    queue.push(data);
    drain();
  }
};
init({
  locateFile: (file) => "/assets/" + file,
  mainScriptUrlOrBlob: "/assets/z3-built.js",
})
  .then((value) => {
    api = value;
    postMessage({ type: "ready", version: api.Z3.get_full_version() });
    drain();
  })
  .catch((error) => postMessage({ type: "fatal", message: String(error) }));
