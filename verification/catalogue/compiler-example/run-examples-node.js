// Run the modules written by testsuite/tests/vox/hmc_compilation_examples.ml
// (with HMC_EXAMPLES_DIR set) in Node's WebAssembly engine and compare the
// status, the result word and the final memory with the WebAssembly model's.
// Each run instantiates its module afresh, sets the exported global `payload`
// to the input and calls `run`, as the model's host does.
//
//   node run-examples-node.js DIR
const fs = require('node:fs');
const path = require('node:path');
const assert = require('node:assert/strict');

const dir = process.argv[2];
const word = (lo, hi) => BigInt(lo) + (BigInt(hi) << 32n);
const modules = new Set();
let count = 0;
for (const line of fs.readFileSync(path.join(dir, 'expected.txt'), 'utf8').trim().split('\n')) {
  const [name, inputLo, inputHi, status, lo, hi] = line.split(' ');
  const input = word(inputLo, inputHi);
  const run = `${name} (input ${input})`;
  const bytes = fs.readFileSync(path.join(dir, `${name}.wasm`));
  assert.equal(WebAssembly.validate(bytes), true, name);
  const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes));
  instance.exports.payload.value = input;
  const actual = instance.exports.run() >>> 0;
  assert.equal(actual, Number(status), `${run}: status`);
  if (actual === 1) {
    assert.equal(BigInt.asUintN(64, instance.exports.tag.value), 1n, `${run}: tag`);
    assert.equal(BigInt.asUintN(64, instance.exports.payload.value), word(lo, hi), `${run}: payload`);
  }
  const memory = fs.readFileSync(path.join(dir, `${name}-${input}.memory`));
  assert.deepEqual(Buffer.from(instance.exports.memory.buffer), memory, `${run}: memory`);
  console.log(`${run}: status ${actual}` + (actual === 1 ? `, result ${instance.exports.payload.value}` : ''));
  modules.add(name);
  count++;
}
console.log(`${count} runs of ${modules.size} modules agree with the model in Node ${process.version} ` +
  '(status, result and final memory)');
