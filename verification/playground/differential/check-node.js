// Runs the playground's checker (the built site: vox.js and Z3 WebAssembly)
// in Node, on the same code path as the browser worker.
//
//   node check-node.js SITE_DIR FILE.ml...        prints each result
//   node check-node.js SITE_DIR --json FILE.ml... prints JSON lines
//
// Each file is checked under its base name, as `ocamlc -c FILE.ml` in its
// directory would. With --lambda the erased program is included.
'use strict';
const fs = require('fs');
const path = require('path');

async function main() {
  const [site, ...rest] = process.argv.slice(2);
  const json = rest.includes('--json');
  const lambda = rest.includes('--lambda');
  const files = rest.filter((a) => !a.startsWith('--'));
  const root = path.resolve(site);
  const { createChecker } = require(path.join(root, 'checker.js'));
  const started = performance.now();
  const check = await createChecker({
    initZ3: require(path.join(root, 'z3-built.js')),
    loadChecker: () => require(path.join(root, 'vox.js')),
    fetchBytes: async (file) => new Uint8Array(fs.readFileSync(path.join(root, file))),
  });
  const loadMs = performance.now() - started;
  if (!json) console.log(`loaded in ${loadMs.toFixed(0)} ms; Z3 ${check.z3Version}`);
  for (const file of files) {
    const source = fs.readFileSync(file, 'utf8');
    const result = check(path.basename(file), source, lambda);
    if (json) {
      console.log(JSON.stringify({ file, ...result }));
    } else {
      console.log(`== ${file}: status ${result.status}, ${result.queries} solver calls, ` +
        `${result.solverMs.toFixed(0)} ms in Z3, ${result.totalMs.toFixed(0)} ms total`);
      process.stdout.write(result.output);
      if (lambda && result.lambda) process.stdout.write(result.lambda);
    }
  }
  await new Promise((resolve) => process.stdout.write('', resolve));
  process.exit(0);
}

main().catch(async (error) => {
  console.error(error);
  await new Promise((resolve) => process.stderr.write('', resolve));
  process.exit(1);
});
