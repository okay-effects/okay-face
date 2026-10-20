# okay-face — an application's capabilities as effects, its faces as handlers

An application has several faces: a screen for a person, tools for an agent, resources for a program. Each shows WHAT
IS THERE and WHAT CAN BE DONE. okay-face writes that once, as a program over an algebraic effect, and each face is a
handler: a line terminal, an MCP server, a test script today; a web page and REST later. Built on
[okay-simple](https://github.com/okay-effects/okay-simple)'s interface, for Scala 3 and 2.13, over any of its
implementations, with no dependency.

Status: specification. Read [specs/face-core.md](specs/face-core.md) — the four data types, the `Face` effect and the
step machine — and [specs/faces.md](specs/faces.md) — the test, terminal and MCP faces. The first application is
[verity](https://github.com/okay-effects/verity), a keeper of one's personal data.
