// Run the modules written by testsuite/tests/vox/hmc_compilation_examples.ml
// (with HMC_EXAMPLES_DIR set) in Node's WebAssembly engine and compare the
// status, the result word and the final memory with the WebAssembly model's.
//
//   node run-examples-node.js DIR
const fs = require('node:fs');
const path = require('node:path');
const assert = require('node:assert/strict');

const dir = process.argv[2];
const word = (lo, hi) => BigInt(lo) + (BigInt(hi) << 32n);
let count = 0;
for (const line of fs.readFileSync(path.join(dir, 'expected.txt'), 'utf8').trim().split('\n')) {
  const [name, status, lo, hi] = line.split(' ');
  const bytes = fs.readFileSync(path.join(dir, `${name}.wasm`));
  assert.equal(WebAssembly.validate(bytes), true, name);
  const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes));
  const actual = instance.exports.run() >>> 0;
  assert.equal(actual, Number(status), `${name}: status`);
  if (actual === 1) {
    assert.equal(BigInt.asUintN(64, instance.exports.tag.value), 1n, `${name}: tag`);
    assert.equal(BigInt.asUintN(64, instance.exports.payload.value), word(lo, hi), `${name}: payload`);
  }
  const memory = fs.readFileSync(path.join(dir, `${name}.memory`));
  assert.deepEqual(Buffer.from(instance.exports.memory.buffer), memory, `${name}: memory`);
  console.log(`${name}: status ${actual}` + (actual === 1 ? `, result ${instance.exports.payload.value}` : ''));
  count++;
}
console.log(`${count} modules agree with the model in Node ${process.version} (status, result and final memory)`);
