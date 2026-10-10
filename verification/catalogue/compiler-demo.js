const demo = document.getElementById('compiler-example');
if (demo) {
  const buttons = [...demo.querySelectorAll('[data-wasm-case]')];
  const result = demo.querySelector('[data-wasm-result]');
  const detail = demo.querySelector('[data-wasm-detail]');
  async function read(url) {
    const response = await fetch(url);
    if (!response.ok) throw new Error(`Could not load the saved artifact (${response.status}).`);
    return response.arrayBuffer();
  }
  async function checked(asset) {
    const bytes = await read(asset.url);
    const digest = await crypto.subtle.digest('SHA-256', bytes);
    const hash = [...new Uint8Array(digest)].map(byte => byte.toString(16).padStart(2, '0')).join('');
    if (hash !== asset.sha256) throw new Error('The saved artifact hash does not match its recorded evidence.');
    return bytes;
  }
  for (const button of buttons) button.addEventListener('click', async () => {
    buttons.forEach(item => { item.disabled = true; });
    result.textContent = 'Running…';
    detail.textContent = '';
    demo.dataset.state = 'running';
    try {
      const response = await fetch('demos/compiler/manifest.json');
      if (!response.ok) throw new Error('Could not load the saved example manifest.');
      const manifest = await response.json();
      const example = manifest.cases[Number(button.dataset.wasmCase)];
      if (!example) throw new Error('This example is unavailable.');
      const [bytes, expectedMemory] = await Promise.all([checked(manifest.wasm), checked(example.memory)]);
      const { instance } = await WebAssembly.instantiate(bytes);
      instance.exports.payload.value = BigInt(example.input);
      const status = instance.exports.run() >>> 0;
      const tag = BigInt.asUintN(64, instance.exports.tag.value).toString();
      const payload = BigInt.asUintN(64, instance.exports.payload.value).toString();
      const actual = new Uint8Array(instance.exports.memory.buffer);
      const expected = new Uint8Array(expectedMemory);
      if (status !== example.status || tag !== example.tag || payload !== example.output) {
        throw new Error(`Observed status ${status}, tag ${tag}, result ${payload}; the recorded result did not match.`);
      }
      if (actual.length !== expected.length || actual.some((byte, index) => byte !== expected[index])) {
        throw new Error('The result matched, but final memory did not match the recorded model execution.');
      }
      result.textContent = `Input ${example.input} → result ${payload}`;
      detail.textContent = 'Executed in this browser. Result and every memory byte match the recorded model execution.';
      demo.dataset.state = 'passed';
    } catch (error) {
      result.textContent = 'Example did not pass';
      detail.textContent = error.message;
      demo.dataset.state = 'failed';
    } finally {
      buttons.forEach(item => { item.disabled = false; });
    }
  });
}
