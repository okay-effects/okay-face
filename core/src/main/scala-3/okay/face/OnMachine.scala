package okay.face

/** `import okay.face.*`: the words of `Face` on the machine — `offer`, `show`, `confirm`, `choose`, `fill`, `fillOrCancel`, and `steps` */
val onMachine: Faces[okay.effects.machine.Machine.type] = Faces()(using okay.effects.machine.Machine)
export onMachine.{offer, show, confirm, choose, fill, fillOrCancel, steps}
