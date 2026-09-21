self.onmessage = async ({ data }) => {
  const { revision, source, assets, mode, bytecode } = data;
  const send = (message) => self.postMessage({ ...message, revision });
  let activeProof;
  let compiled = false,
    failed = false;
  try {
    const runtimeURL = "/assets/vox-runtime.js";
    const { default: createRuntime } = await import(
      /* @vite-ignore */ runtimeURL
    );
    const runtime = await createRuntime({
      noInitialRun: true,
      preRun: [
        (m) => {
          m.ENV.OCAMLLIB = "/stdlib";
          m.ENV.OCAMLRUNPARAM = "d=1,s=256k";
        },
      ],
      locateFile: (file) => "/assets/" + file,
      print: (text) =>
        send({ type: mode === "run" ? "stdout" : "compiler-output", text }),
      printErr: (text) =>
        send({ type: mode === "run" ? "stderr" : "compiler-output", text }),
      voxEvent: (event) => {
        if (event.type === "proof") activeProof = event;
        if (event.type === "compiled") compiled = true;
        if (event.type === "diagnostic") failed = true;
        send(event);
      },
      voxSolve: (script) => {
        const control = new SharedArrayBuffer(8),
          view = new Int32Array(control);
        send({ type: "solve", script, control, proof: activeProof });
        const result = Atomics.wait(view, 0, 0, 7000);
        return result === "timed-out" ? 2 : Atomics.load(view, 1);
      },
    });
    runtime.ENV.OCAMLLIB = "/stdlib";
    runtime.ENV.OCAMLRUNPARAM = "d=1,s=256k";
    runtime.FS.mkdir("/work");
    if (mode === "run") {
      runtime.FS.writeFile("/work/lesson.byte", new Uint8Array(bytecode));
      const exitCode = runtime.callMain(["/work/lesson.byte"]);
      send({ type: "executed", exitCode: exitCode ?? 0 });
    } else {
      runtime.FS.mkdir("/stdlib");
      const packed = new Uint8Array(assets.data);
      for (const file of assets.manifest)
        runtime.FS.writeFile(
          "/stdlib/" + file.name,
          packed.subarray(file.offset, file.offset + file.length),
        );
      runtime.FS.writeFile("/compiler.byte", new Uint8Array(assets.compiler));
      runtime.FS.writeFile("/work/lesson.ml", source);
      send({ type: "checking" });
      runtime.callMain(["/compiler.byte"]);
      if (compiled) {
        const output = runtime.FS.readFile("/work/lesson.byte");
        send({ type: "bytecode", bytecode: output });
      } else if (!failed)
        send({
          type: "fatal",
          message: "The compiler exited without producing a program.",
        });
    }
  } catch (error) {
    send({ type: "fatal", message: String(error), mode });
  }
};
