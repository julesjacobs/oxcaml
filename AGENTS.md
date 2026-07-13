Build a sound, fast SMT solver in pure OCaml, for the OxCaml refinement type system.
The master owns the schedule: the project ships on time. Nimble and goal-directed —
prototypes over design docs, code over text, no rituals, no process cruft.
Soundness bugs are the only emergency. To land: make test and make gate must pass,
plus two-model code review (codex + fable).
Write plainly. No AI-slop language. De-AI all your writing.
No invented jargon: name things by what they do ("the function that creates internal
symbols"), not by coinages ("minting door", "O-MINTER"). Every message must stand on
its own: write for a reader who knows SMT but has read no other message or file.
When unsure, make the call and keep moving.
Any process or step that takes longer than 5 minutes must be thought about:
can we eliminate this? can we speed it up?
Parallelize - you can run max 20 agents - if you're running fewer than 5, rethink.
