import { test } from "node:test";
import assert from "node:assert/strict";
import { decodeCountermodel } from "../src/countermodel.js";
const proof = {
  symbols: [
    { id: "v0", name: "x", kind: "int63" },
    { id: "v1", name: "count", kind: "bigint" },
    { id: "v2", name: "flag", kind: "bool" },
  ],
  constructors: [],
};
test("model decoding keeps signed 63-bit values and unbounded integers exact", () => {
  const values = decodeCountermodel(
    "((v0 #b100000000000000000000000000000000000000000000000000000000000000) (v1 999999999999999999999999999) (v2 false))",
    proof,
  );
  assert.deepEqual(
    values.map((v) => v.value),
    ["-4611686018427387904", "999999999999999999999999999Z", "false"],
  );
});
test("negative integers, bitvector numerals and constructed values use Vox names", () => {
  const p = {
    symbols: [{ id: "v0", name: "tree", kind: "datatype" }],
    constructors: [
      { id: "c0", name: "Leaf", fields: [{ name: "value", kind: "int63" }] },
    ],
  };
  assert.equal(
    decodeCountermodel("((v0 (c0 (_ bv9223372036854775807 63))))", p)[0].value,
    "Leaf(-1)",
  );
  assert.equal(
    decodeCountermodel(
      "((v0 (- 4)) (v1 (- 99999999999999999999)) (v2 true))",
      proof,
    )[0].value,
    "-4",
  );
});
test("unsupported values remain explicitly undecoded; malformed responses fail", () => {
  assert.equal(
    decodeCountermodel("((v0 (mystery 1)))", proof)[0].available,
    false,
  );
  assert.throws(() => decodeCountermodel('(error "model unavailable")', proof));
  assert.throws(() => decodeCountermodel("((v0 1)", proof));
});
