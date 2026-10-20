# AGENTS.md — okay-face

Read before anything else. Then `specs/face-core.md` and `specs/faces.md`: the code follows the specs, and a change of
design is a change of the spec first.

- The library is written on okay-simple's interface (`okay.effects.Effectful`), over any `Effects`, one source for
  Scala 3.3 and 2.13 in the shared style of okay-simple (operations carry `answer`; rows as
  `({ type L[x] = Sum[R, E, x] })#L` in library source). No dependency but `okay-effects` and its modules; okay, the platform, only under `interop/`, optional, never
  required — okay-face is complete on okay-simple alone, as okay-simple is on nothing.
- face-core imports no platform and no JSON library; `Value`, `View`, `Schema`, `Chosen` are values with no functions.
- Every face is a loop over `Step`; what a face adds is how it shows a view and reads a choice, nothing else.
- Tests on both compilers, on the machine and on `Free`. Documentation code blocks compile and run.
- Commits: plain messages, what and why, no attribution trailers.
