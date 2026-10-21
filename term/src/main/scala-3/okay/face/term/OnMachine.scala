package okay.face.term

/** `import okay.face.term.*`: the terminal face on the machine — `terminal(steps)` and `prompting`, the person as a handler of `Fields` */
val onMachine: Terminal[okay.effects.machine.Machine.type] = new Terminal[okay.effects.machine.Machine.type]()(okay.face.onMachine)
export onMachine.{terminal, prompting}
