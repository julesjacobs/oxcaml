// vox-web check worker.
//
// Owns a CheckBackend and answers `check`/`goal` requests off the main
// thread — the seam where a slow, synchronous jsoo'd compiler would run
// without freezing the UI.  Protocol (main <-> worker):
//
//   -> { type: "config", backend: "mock"|"remote"|"in-browser", opts }
//   -> { type: "check",  id, source, revision }
//   -> { type: "goal",   id, source, line, col, revision }
//   <- { type: "checkResult"|"goalResult", id, result }
//   <- { type: "error",  id, error }
//
// Each request carries an id so the main thread can drop stale replies
// (same discipline as app.js's `revision`).

/* global importScripts, VoxBackends */
importScripts("backends.js");

let backend = VoxBackends.make("mock");

self.onmessage = async function (ev) {
  const msg = ev.data || {};
  if (msg.type === "config") {
    try {
      backend = VoxBackends.make(msg.backend, msg.opts || {});
    } catch (e) {
      self.postMessage({ type: "error", id: msg.id, error: String(e) });
    }
    return;
  }
  try {
    if (msg.type === "check") {
      const result = await backend.check(msg.source, msg.revision);
      self.postMessage({ type: "checkResult", id: msg.id, result });
    } else if (msg.type === "goal") {
      const result = await backend.goal(msg.source, msg.line, msg.col, msg.revision);
      self.postMessage({ type: "goalResult", id: msg.id, result });
    } else {
      self.postMessage({ type: "error", id: msg.id, error: "unknown message type: " + msg.type });
    }
  } catch (e) {
    self.postMessage({ type: "error", id: msg.id, error: String(e && e.message ? e.message : e) });
  }
};
