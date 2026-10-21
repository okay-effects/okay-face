package okay.faces.airstream

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/** `import okay.faces.airstream.*`: the bridge on the machine, over okay-simple's default streams */
val onMachine: AirStreams[okay.effects.machine.Machine.type, okay.effects.stream.onMachine.type] =
  new AirStreams[okay.effects.machine.Machine.type, okay.effects.stream.onMachine.type](okay.effects.stream.onMachine)
export onMachine.{fromEventStream, fromSignal, toEventStream, fromProgram, into, varState}
