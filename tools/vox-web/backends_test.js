// Node test for the check backends (logic only; no browser).
// Run: node backends_test.js
const assert = require("assert");
const B = require("./backends.js");

async function main() {
  // MockBackend: deterministic, no network, correct shape + revision.
  const mock = B.make("mock");
  const r = await mock.check("let f a i = a.(i)", 7);
  assert.strictEqual(r.revision, 7, "mock echoes revision");
  assert.strictEqual(r.ok, false, "mock reports not-ok (has a failed VC)");
  const vcs = r.regions.filter((x) => x.kind === "vc");
  assert.strictEqual(vcs.length, 2, "mock has 2 VCs");
  assert.ok(vcs.some((v) => v.status === "proved"), "one proved VC");
  const failed = vcs.find((v) => v.status === "failed");
  assert.ok(failed && failed.counterexample.length, "failed VC carries a counterexample");
  const g = await mock.goal("src", 1, 2, 3);
  assert.strictEqual(g.revision, 3, "mock goal echoes revision");
  assert.ok(Array.isArray(g.goals), "mock goal has goals[]");

  // RemoteBackend: posts JSON to /check and /goal via injected fetch.
  const calls = [];
  const fakeFetch = async (url, init) => {
    calls.push({ url, body: JSON.parse(init.body), method: init.method });
    return { json: async () => ({ ok: true, regions: [], echoedUrl: url }) };
  };
  const remote = B.make("remote", { baseUrl: "http://h:9", fetch: fakeFetch });
  const rr = await remote.check("SRC", 11);
  assert.strictEqual(calls[0].url, "http://h:9/check");
  assert.strictEqual(calls[0].method, "POST");
  assert.deepStrictEqual(calls[0].body, { source: "SRC", revision: 11 });
  assert.strictEqual(rr.echoedUrl, "http://h:9/check");
  await remote.goal("SRC", 4, 5, 12);
  assert.strictEqual(calls[1].url, "http://h:9/goal");
  assert.deepStrictEqual(calls[1].body, { source: "SRC", line: 4, col: 5, revision: 12 });

  // InBrowserBackend: honest failure with a pointer to NOTES.
  const ib = B.make("in-browser");
  await assert.rejects(() => ib.check(), /NOTES\.md/, "in-browser check explains itself");

  // make(): rejects unknown kinds.
  assert.throws(() => B.make("nope"), /unknown backend/);

  console.log("backends_test: OK (mock, remote, in-browser stub, dispatch)");
}

main().catch((e) => {
  console.error("backends_test FAILED:", e);
  process.exit(1);
});
