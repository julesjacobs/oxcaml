// The checker's worker: Z3 (WebAssembly) and the Vox checker (JavaScript)
// run here, so that a long check does not block the page. Messages:
//   in:  { id, name, source }         out: { id, result } or { id, error }
//   out, once loaded: { ready: { z3Version, revision, loadMs } } or { failed }
importScripts('checker.js', 'z3-built.js');

async function load() {
  const started = performance.now();
  // Z3's threads are workers started from its script. They are started from
  // a copy in memory: some browsers (Chromium 152 in a cross-origin isolated
  // page, for one) refuse to start a worker from a URL inside a worker.
  const z3Script = await (await fetch('z3-built.js')).blob();
  const check = await createChecker({
    initZ3: self.initZ3,
    z3Options: { mainScriptUrlOrBlob: z3Script },
    loadChecker: () => {
      importScripts('vox.js');
      return self;
    },
    fetchBytes: async (path) => {
      const response = await fetch(path);
      if (!response.ok) throw new Error(`Could not load ${path} (${response.status}).`);
      return new Uint8Array(await response.arrayBuffer());
    },
  });
  postMessage({ ready: { z3Version: check.z3Version, revision: check.revision,
                         loadMs: performance.now() - started } });
  return check;
}

const loaded = load().catch((error) => {
  postMessage({ failed: String(error && error.message || error) });
  throw error;
});

onmessage = async ({ data }) => {
  const check = await loaded;
  try {
    postMessage({ id: data.id, result: check(data.name, data.source, true) });
  } catch (error) {
    postMessage({ id: data.id, error: String(error && error.message || error) });
  }
};
