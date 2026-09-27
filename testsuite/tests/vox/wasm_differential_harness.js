// Engine side of the differential test of the Vox WebAssembly model.
// Usage: node wasm_differential_harness.js DIR FROM TO [TIMEOUT_MS]
// For each id in [FROM, TO), reads DIR/<id>.wasm (the module as the model sees
// it) and, if present, DIR/<id>.variant.wasm (the same module exporting every
// global as g<k>), and DIR/<id>.input, the input as an unsigned decimal.
// Prints one line per id; on a return, writes the final
// memory to DIR/<id>.memory. Each module runs in a worker thread that is
// terminated after TIMEOUT_MS.
'use strict';
const fs = require('fs');
const path = require('path');
const { Worker, isMainThread, parentPort } = require('worker_threads');

function show(v) {
  if (v === undefined) return 'void';
  if (typeof v === 'bigint') return 'i64:' + BigInt.asUintN(64, v).toString();
  if (typeof v === 'number' && Number.isInteger(v)) return 'i32:' + (v >>> 0).toString();
  return 'other:' + String(v);
}

function clean(s) { return String(s).replace(/\s+/g, '_'); }

// Instantiate with no imports (the modules define their own memory and table),
// set the exported global payload to the input and call the export run.
function execute(bytes, input) {
  let instance;
  try {
    instance = new WebAssembly.Instance(new WebAssembly.Module(bytes), {});
  } catch (e) {
    return { outcome: 'instantiate_error', detail: e.constructor.name + ':' + e.message };
  }
  const exports = instance.exports;
  try {
    exports.payload.value = input;
  } catch (e) {
    return { outcome: 'input_error', detail: e.constructor.name + ':' + e.message };
  }
  try {
    const value = exports.run();
    return { outcome: 'return', value: show(value), exports };
  } catch (e) {
    if (e instanceof WebAssembly.RuntimeError) return { outcome: 'trap', detail: e.message };
    if (e instanceof RangeError) return { outcome: 'stack_overflow', detail: e.message };
    return { outcome: 'error', detail: e.constructor.name + ':' + e.message };
  }
}

// Recognizer for the binary subset that the model decodes (wasm_module.ml and
// the section decoders it calls), written from their definitions but sharing
// no code with them. Returns 'yes' or 'no:<first reason>'. It decides only
// decoding: a module in the subset may still fail validation.
function modelSubset(b) {
  let p = 0;
  let end = b.length;
  const fail = (why) => { throw new Error(why); };
  const byte = () => { if (p >= end) fail('end_of_input'); return b[p++]; };
  const expect = (bytes, what) => { for (const x of bytes) if (byte() !== x) fail(what); };
  // An unsigned LEB128 of at most five bytes whose fifth byte is below 16.
  const u32 = () => {
    let value = 0;
    for (let i = 0; i < 5; i++) {
      const c = byte();
      if (i === 4 && c >= 16) fail('u32');
      value += (c & 0x7f) * 2 ** (7 * i);
      if (c < 128) return value;
    }
    return fail('u32');
  };
  // i32.const and i64.const immediates: signed LEB128 of exactly 5 and 10 bytes.
  const i32 = () => {
    let value = 0;
    for (let i = 0; i < 4; i++) { const c = byte(); if (c < 128) fail('i32.const_length'); value += (c & 0x7f) * 2 ** (7 * i); }
    const c = byte();
    if (!(c < 8 || (c >= 120 && c < 128))) fail('i32.const_last_byte');
    return value + (c & 0x0f) * 2 ** 28;
  };
  const i64 = () => {
    for (let i = 0; i < 9; i++) if (byte() < 128) fail('i64.const_length');
    const c = byte();
    if (c !== 0 && c !== 127) fail('i64.const_last_byte');
  };
  const plain = new Set([0x00, 0x01, 0x05, 0x0b, 0x0f, 0x1a, 0x1b, 0x45, 0x46, 0x47, 0x49, 0x4b, 0x4d, 0x4f,
    0x50, 0x51, 0x54, 0x6a, 0x6b, 0x6c, 0x71, 0x72, 0x7c, 0x7d, 0x83, 0x84, 0x86, 0x88, 0xa7, 0xad]);
  const instruction = () => {
    const op = byte();
    switch (op) {
      case 0x0c: case 0x0d: case 0x10: case 0x20: case 0x21: case 0x22: case 0x23: case 0x24:
        u32(); return { op };
      case 0x02: case 0x03: case 0x04:
        if (byte() !== 0x40) fail('block_type'); return { op };
      case 0x11:
        u32(); if (byte() !== 0x00) fail('call_indirect_table'); return { op };
      case 0x41: return { op, value: i32() };
      case 0x42: i64(); return { op };
      case 0x28: case 0x36: if (u32() > 2) fail('alignment'); u32(); return { op };
      case 0x29: case 0x37: if (u32() > 3) fail('alignment'); u32(); return { op };
      default:
        if (!plain.has(op)) fail('opcode_0x' + op.toString(16)); return { op };
    }
  };
  const zeroOffset = () => {
    const c = instruction(); if (c.op !== 0x41 || c.value !== 0) fail('segment_offset');
    if (instruction().op !== 0x0b) fail('segment_offset_end');
  };
  const limits = () => { if (byte() !== 0x01) fail('limits_flag'); const min = u32(); const max = u32(); if (min > max) fail('limits_order'); return max; };
  const section = (id, parse) => {
    if (byte() !== id) fail('section_' + id + '_expected');
    const size = u32();
    const stop = p + size;
    if (stop > b.length) fail('section_' + id + '_size');
    const outer = end; end = stop; parse();
    if (p !== stop) fail('section_' + id + '_trailing_bytes');
    end = outer;
  };
  try {
    expect([0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00], 'header');
    let types = 0, functions = 0;
    section(1, () => {
      types = u32();
      for (let i = 0; i < types; i++) {
        expect([0x60, 0x00], 'function_type');
        const n = byte();
        if (n === 1) { const t = byte(); if (t !== 0x7f && t !== 0x7e) fail('result_type'); }
        else if (n !== 0) fail('result_count');
      }
    });
    section(3, () => { functions = u32(); for (let i = 0; i < functions; i++) if (u32() >= types) fail('type_index'); });
    section(4, () => { if (u32() !== 1) fail('table_count'); if (byte() !== 0x70) fail('table_type'); limits(); });
    section(5, () => { if (u32() !== 1) fail('memory_count'); if (limits() > 65536) fail('memory_maximum'); });
    section(6, () => {
      const n = u32();
      for (let i = 0; i < n; i++) {
        const t = byte();
        if (byte() > 1) fail('mutability');
        const c = instruction();
        if (!((t === 0x7f && c.op === 0x41) || (t === 0x7e && c.op === 0x42))) fail('global_initializer');
        if (instruction().op !== 0x0b) fail('global_initializer_end');
      }
    });
    section(7, () => {
      if (u32() !== 4) fail('export_count');
      expect([3, 0x72, 0x75, 0x6e, 0], 'export_run'); u32();
      expect([6, 0x6d, 0x65, 0x6d, 0x6f, 0x72, 0x79, 2], 'export_memory'); u32();
      expect([3, 0x74, 0x61, 0x67, 3], 'export_tag'); u32();
      expect([7, 0x70, 0x61, 0x79, 0x6c, 0x6f, 0x61, 0x64, 3], 'export_payload'); u32();
    });
    section(9, () => {
      if (u32() !== 1) fail('element_count'); if (u32() !== 0) fail('element_flag');
      zeroOffset(); const n = u32(); for (let i = 0; i < n; i++) u32();
    });
    section(10, () => {
      if (u32() !== functions) fail('code_count');
      for (let f = 0; f < functions; f++) {
        const size = u32();
        const stop = p + size;
        if (stop > end) fail('body_size');
        const outer = end; end = stop;
        const locals = u32();
        for (let i = 0; i < locals; i++) {
          if (byte() !== 1) fail('local_entry_count');
          const t = byte(); if (t !== 0x7f && t !== 0x7e) fail('local_type');
        }
        const frames = ['function'];
        while (p < stop) {
          const { op } = instruction();
          if (frames.length === 0) fail('code_after_function_end');
          if (op === 0x02 || op === 0x03) frames.push('block');
          else if (op === 0x04) frames.push('then');
          else if (op === 0x05) { if (frames[frames.length - 1] !== 'then') fail('else'); frames[frames.length - 1] = 'else'; }
          else if (op === 0x0b) frames.pop();
        }
        if (frames.length !== 0) fail('unclosed_block');
        end = outer;
      }
    });
    section(11, () => {
      if (u32() !== 1) fail('data_count'); if (u32() !== 0) fail('data_flag');
      zeroOffset(); const n = u32(); if (p + n > end) fail('data_size'); p += n;
    });
    if (p !== b.length) fail('bytes_after_data_section');
    return 'yes';
  } catch (e) {
    return 'no:' + clean(e.message);
  }
}

function global(exports, name) {
  const g = exports[name];
  return g instanceof WebAssembly.Global ? show(g.value) : 'missing';
}

function run(dir, id) {
  const file = path.join(dir, id + '.wasm');
  const bytes = fs.readFileSync(file);
  const fields = ['id=' + id];
  const valid = WebAssembly.validate(bytes);
  fields.push('valid=' + (valid ? 1 : 0), 'subset=' + modelSubset(bytes));
  if (!valid) {
    let detail = 'none';
    try { new WebAssembly.Module(bytes); } catch (e) { detail = e.message; }
    fields.push('outcome=invalid', 'detail=' + clean(detail));
    return fields.join(' ');
  }
  const input = BigInt(fs.readFileSync(path.join(dir, id + '.input'), 'utf8'));
  const main = execute(bytes, input);
  fields.push('outcome=' + main.outcome);
  if (main.outcome === 'return') {
    fields.push('value=' + main.value, 'tag=' + global(main.exports, 'tag'),
                'payload=' + global(main.exports, 'payload'));
    fs.writeFileSync(path.join(dir, id + '.memory'), Buffer.from(main.exports.memory.buffer));
  }
  const variantFile = path.join(dir, id + '.variant.wasm');
  if (fs.existsSync(variantFile)) {
    const variantBytes = fs.readFileSync(variantFile);
    if (!WebAssembly.validate(variantBytes)) {
      fields.push('variant=invalid');
    } else {
      const variant = execute(variantBytes, input);
      fields.push('variant=' + variant.outcome);
      if (variant.outcome === 'return') {
        const values = [];
        for (let k = 0; variant.exports['g' + k] !== undefined; k++) values.push(global(variant.exports, 'g' + k));
        fields.push('variant_value=' + variant.value, 'globals=' + (values.length ? values.join(',') : 'none'));
      }
    }
  }
  if (main.detail !== undefined) fields.push('detail=' + clean(main.detail));
  return fields.join(' ');
}

if (!isMainThread) {
  parentPort.on('message', ({ dir, id }) => {
    let line;
    try { line = run(dir, id); } catch (e) { line = 'id=' + id + ' valid=? outcome=harness_error detail=' + clean(e.stack); }
    parentPort.postMessage(line);
  });
} else {
  const [dir, from, to, timeout] = process.argv.slice(2);
  const limit = timeout ? Number(timeout) : 10000;
  let worker = null;
  const spawn = () => { worker = new Worker(__filename); worker.unref(); };
  const one = (id) => new Promise((resolve) => {
    const timer = setTimeout(() => {
      worker.removeAllListeners('message');
      worker.terminate();
      spawn();
      const bytes = fs.readFileSync(path.join(dir, id + '.wasm'));
      const valid = WebAssembly.validate(bytes);
      resolve('id=' + id + ' valid=' + (valid ? 1 : 0) + ' subset=' + modelSubset(bytes) + ' outcome=timeout');
    }, limit);
    worker.once('message', (line) => { clearTimeout(timer); resolve(line); });
    worker.postMessage({ dir, id });
  });
  (async () => {
    spawn();
    const out = [];
    for (let id = Number(from); id < Number(to); id++) {
      if (!fs.existsSync(path.join(dir, id + '.wasm'))) continue;
      out.push(await one(id));
    }
    process.stdout.write(out.join('\n') + (out.length ? '\n' : ''));
    await worker.terminate();
  })();
}
