const statusNames = [
  "Proved",
  "Counterexample",
  "Timed out",
  "Unknown",
  "Solver failed",
  "Cancelled",
];
function element(tag, text, className) {
  const node = document.createElement(tag);
  if (text !== undefined) node.textContent = text;
  if (className) node.className = className;
  return node;
}
export function createGoalPanel(root, navigate) {
  let obligations = new Map(),
    selected = null;
  function render() {
    root.replaceChildren();

    if (!obligations.size) {
      root.append(element("p", "No proof goals yet.", "goal-empty"));
      return;
    }
    const picker = element("select");
    picker.setAttribute("aria-label", "Proof obligation");
    for (const proof of obligations.values()) {
      const option = element(
        "option",
        `${proof.index}. ${proof.goal.label} — ${statusNames[proof.status] ?? "Checking"}`,
      );
      option.value = String(proof.index);
      option.selected = proof.index === selected;
      picker.append(option);
    }
    picker.onchange = () => {
      selected = Number(picker.value);
      render();
    };
    if (obligations.size > 1) root.append(picker);
    const proof = obligations.get(selected);
    if (!proof) return;
    const location = element(
      "button",
      `${proof.goal.label} · ${statusNames[proof.status] ?? "Checking"} ↗`,
      "goal-source",
    );
    location.title = "Show source";
    location.onclick = () => navigate(proof.from, proof.to);
    root.append(location);
    const assumptions = element("details");
    assumptions.open = true;
    assumptions.append(
      element("summary", `Assumptions (${proof.assumptions.length})`),
    );
    for (const fact of proof.assumptions) {
      const row = element("div", undefined, "goal-fact");
      row.title = fact.label;
      row.append(element("pre", fact.text));
      assumptions.append(row);
    }
    if (!proof.assumptions.length)
      assumptions.append(element("p", "No assumptions."));
    root.append(
      assumptions,
      element("h3", "Goal"),
      element("pre", proof.goal.text, "goal-conclusion"),
    );
    if (proof.status === 1) {
      root.append(element("h3", "Counterexample"));
      root.append(
        element(
          "p",
          "Solver model; not replayed as program inputs.",
          "goal-note",
        ),
      );
      if (proof.countermodel?.values.some((v) => !v.internal)) {
        const table = element("table");
        const head = element("tr");
        head.append(element("th", "Vox name"), element("th", "Value"));
        table.append(head);
        for (const value of proof.countermodel.values.filter(
          (v) => !v.internal,
        )) {
          const row = element("tr");
          row.append(
            element("td", value.name),
            element(
              "td",
              value.value + (value.available ? "" : " (solver notation)"),
            ),
          );
          table.append(row);
        }
        root.append(table);
      } else
        root.append(
          element(
            "p",
            proof.modelError
              ? "The goal failed, but model values could not be decoded."
              : !proof.countermodel
                ? "Retrieving model values…"
                : "This obligation has no named values.",
            "goal-note",
          ),
        );
    }
    if (proof.opaqueFunctions.length)
      root.append(
        element(
          "p",
          `Opaque functions or operations: ${proof.opaqueFunctions.join(", ")}. A countermodel may indicate missing facts about these operations rather than a runtime failure.`,
          "goal-note",
        ),
      );
    if (proof.script) {
      const raw = element("details");
      raw.append(
        element("summary", "Solver details"),
        element("pre", proof.script),
      );
      if (proof.countermodel?.values.some((v) => v.internal))
        raw.append(
          element(
            "pre",
            proof.countermodel.values
              .filter((v) => v.internal)
              .map((v) => v.name + " = " + v.value)
              .join("\n"),
          ),
        );
      if (proof.countermodel?.modelQuery)
        raw.append(element("pre", proof.countermodel.modelQuery));
      if (proof.countermodel?.rawModel)
        raw.append(element("pre", proof.countermodel.rawModel));
      if (proof.modelError) raw.append(element("pre", proof.modelError));
      root.append(raw);
    }
  }
  render();
  return {
    clear() {
      obligations = new Map();
      selected = null;
      render();
    },
    add(proof) {
      obligations.set(proof.index, proof);
      if (selected === null) selected = proof.index;
      render();
    },
    result(result) {
      const proof = obligations.get(result.index);
      if (!proof) return;
      Object.assign(proof, result);
      if (result.status !== 0) selected = result.index;
      render();
    },
  };
}
