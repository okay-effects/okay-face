package okay.face

/** `import okay.face.airstream._`: the bridge on the machine, over okay-simple's default streams */
package object airstream extends okay.face.airstream.AirStreams[okay.effects.machine.Machine.type, okay.effects.stream.`package`.type](
  okay.effects.stream.`package`)(scala.scalajs.concurrent.JSExecutionContext.Implicits.queue)
