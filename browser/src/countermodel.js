export function parseSExpressions(text) {
  const tokens =
    text
      .match(/;[^\n]*|\(|\)|"(?:""|[^"])*"|\|[^|]*\||[^\s()]+/g)
      ?.filter((t) => !t.startsWith(";")) ?? [];
  let pos = 0;
  function read() {
    const token = tokens[pos++];
    if (token === undefined || token === ")")
      throw new Error("Malformed solver response");
    if (token !== "(") return token;
    const list = [];
    while (tokens[pos] !== ")") {
      if (pos === tokens.length) throw new Error("Unclosed solver response");
      list.push(read());
    }
    pos++;
    return list;
  }
  const expressions = [];
  while (pos < tokens.length) expressions.push(read());
  return expressions;
}
const raw = (value) =>
  Array.isArray(value) ? "(" + value.map(raw).join(" ") + ")" : value;
function integer(value) {
  if (typeof value === "string") {
    if (/^-?\d+$/.test(value)) return BigInt(value);
    if (/^#x[0-9a-f]+$/i.test(value)) return BigInt("0x" + value.slice(2));
    if (/^#b[01]+$/.test(value)) return BigInt("0b" + value.slice(2));
  }
  if (Array.isArray(value) && value.length === 2 && value[0] === "-")
    return -integer(value[1]);
  if (
    Array.isArray(value) &&
    value.length === 3 &&
    value[0] === "_" &&
    /^bv\d+$/.test(value[1])
  )
    return BigInt(value[1].slice(2));
  throw new Error("Non-numeric model value");
}
function display(value, kind, constructors) {
  if (kind === "int63") return BigInt.asIntN(63, integer(value)).toString();
  if (kind === "bigint") return integer(value).toString() + "Z";
  if (kind === "bool" && ["true", "false"].includes(value)) return value;
  const head = Array.isArray(value) ? value[0] : value;
  const constructor = constructors.find((c) => c.id === head);
  if (constructor) {
    const args = Array.isArray(value) ? value.slice(1) : [];
    if (args.length !== constructor.fields.length)
      throw new Error("Unexpected constructor arity");
    return (
      constructor.name +
      (args.length
        ? "(" +
          args
            .map((v, i) => display(v, constructor.fields[i].kind, constructors))
            .join(", ") +
          ")"
        : "")
    );
  }
  if (kind === "opaque") return "abstract value " + raw(value);
  throw new Error("Unsupported model value");
}
export function decodeCountermodel(text, proof) {
  const expressions = parseSExpressions(text);
  const pairs = expressions.find(
    (e) =>
      Array.isArray(e) &&
      e.every(
        (pair) =>
          Array.isArray(pair) && pair.length === 2 && /^v\d+$/.test(pair[0]),
      ),
  );
  if (!pairs) throw new Error("No model values returned");
  return proof.symbols.map((symbol) => {
    const entry = pairs.find((pair) => pair[0] === symbol.id);
    if (!entry) return { ...symbol, value: "Not returned", available: false };
    try {
      return {
        ...symbol,
        value: display(entry[1], symbol.kind, proof.constructors),
        available: true,
      };
    } catch {
      return { ...symbol, value: raw(entry[1]), available: false };
    }
  });
}
export async function inspectCountermodel(Z3, ctx, proof) {
  if (!proof?.symbols.length) return { values: [], rawModel: "" };
  const rawModel = await Z3.eval_smtlib2_string(
    ctx,
    "(get-value (" + proof.symbols.map((s) => s.id).join(" ") + "))",
  );
  return { values: decodeCountermodel(rawModel, proof), rawModel };
}

export async function findCountermodel(Z3, ctx, proof, script) {
  const original = await inspectCountermodel(Z3, ctx, proof);
  const inputs =
    proof?.symbols.filter(
      (s) => !s.internal && ["int63", "bigint"].includes(s.kind),
    ) ?? [];
  if (!inputs.length) return original;
  const bounds = inputs.map((s) =>
    script.includes(`(declare-fun ${s.id} () (_ BitVec 63))`)
      ? `(and (bvsle (bvneg (_ bv1 63)) ${s.id}) (bvsle ${s.id} (_ bv1 63)))`
      : `(and (<= (- 1) ${s.id}) (<= ${s.id} 1))`,
  );
  const modelQuery = `(push)\n(set-option :timeout 250)\n(assert (and ${bounds.join(" ")}))\n(check-sat)`;
  try {
    const result = await Z3.eval_smtlib2_string(ctx, modelQuery);
    if (/^sat\b/.test(result))
      return {
        ...(await inspectCountermodel(Z3, ctx, proof)),
        modelQuery,
        small: true,
      };
  } catch {
    /* The original countermodel remains valid if the extra search fails. */
  }
  return original;
}
