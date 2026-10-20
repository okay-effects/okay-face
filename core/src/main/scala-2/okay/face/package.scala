package okay

/** `import okay.face._`: the words of `Face` on the machine — `offer`, `show`, `confirm`, `choose`, `fill`, `fillOrCancel`, and `steps` */
package object face extends okay.face.Faces[okay.effects.machine.Machine.type]()(okay.effects.machine.Machine)
