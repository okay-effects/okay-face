// the JVM always; Scala.js and Scala Native when asked (`sbt -Dplatforms=jvm,js,native`) — okay-simple's versions
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.12")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.4.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.4.0")
// the web face's tests run under jsdom: `sbt -Dplatforms=js laminarJS/test`, jsdom from NODE_PATH (or npm install
// jsdom);
// the environment is a library of the build, not a plugin
libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.1"
