/** the libraries' Scala 3: the LTS, as okay-simple's; and Scala 2.13 — one source for both */
val scala3 = "3.3.8"
val scala2 = "2.13.18"
import sbtcrossproject.{CrossProject, CrossType}
import Platforms._

ThisBuild / organization := "io.github.okay-effects"
ThisBuild / organizationName := "okay-effects"
ThisBuild / homepage := Some(url("https://github.com/okay-effects/okay-face"))
ThisBuild / scmInfo := Some(ScmInfo(url("https://github.com/okay-effects/okay-face"),
  "scm:git:https://github.com/okay-effects/okay-face.git"))
ThisBuild / developers := List(Developer("sergey-scherbina", "Sergiy Shcherbyna", "sergey.scherbina@gmail.com",
  url("https://github.com/sergey-scherbina")))
ThisBuild / startYear := Some(2026)
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / scalaVersion := scala3
/** okay-simple, the only dependency: its release from its Pages repository — until 0.1.1 is released, with the `json` and
 * `shape` modules this needs, the snapshot published locally: in okay-simple, `sbt +shape/publishLocal` and its dependencies */
ThisBuild / resolvers += "okay-effects" at "https://okay-effects.github.io/okay-simple/maven"
val okayEffects = "0.1.1-SNAPSHOT"

def major(v: String): String = if (v.startsWith("3")) "3" else "2"

/** one source for both compilers, and each its own convenience in scala-2/ and scala-3/ */
def crossBuilt: Seq[Def.Setting[?]] = Seq(
  crossScalaVersions := Seq(scala3, scala2),
  scalacOptions ++= (if (scalaVersion.value.startsWith("3")) Seq("-deprecation", "-feature")
                     else Seq("-deprecation", "-feature", "-Xlint")),
  libraryDependencies += "org.scalameta" %%% "munit" % "1.2.3" % Test)

/** THE PLATFORMS BUILT: the JVM always; Scala.js and Scala Native when asked —
 * `sbt -Dplatforms=jvm,js,native ...`, or the environment's `OKAY_PLATFORMS=js,native` */
val platformsAsked: Set[String] =
  (sys.props.get("platforms") orElse sys.env.get("OKAY_PLATFORMS")).getOrElse("jvm").split(',').map(_.trim.toLowerCase).toSet + "jvm"
def platformsOf(can: Set[String]): Seq[sbtcrossproject.Platform] = Seq[sbtcrossproject.Platform](JVMPlatform) ++
  (if (platformsAsked("js") && can("js")) Seq(JSPlatform) else Nil) ++
  (if (platformsAsked("native") && can("native")) Seq(NativePlatform) else Nil)
def crossBoth(id: String, dir: String, can: Set[String] = Set("js", "native")): CrossProject =
  CrossProject(id, file(dir))(platformsOf(can): _*).crossType(CrossType.Pure)
    .settings(crossBuilt).jvmConfigure(_.withId(id)).onJS(_.withId(id + "JS")).onNative(_.withId(id + "Native"))

/** FACE-CORE: View, Action, the effect Face, the step machine — over okay-simple's `shape` (Shape, Schema) and `json` (Json) */
lazy val core = crossBoth("core", "core").settings(name := "okay-face",
  libraryDependencies ++= Seq("io.github.okay-effects" %%% "okay-effects" % okayEffects,
    "io.github.okay-effects" %%% "okay-effects-shape" % okayEffects,
    "io.github.okay-effects" %%% "okay-effects-machine" % okayEffects % Test))

lazy val root = (project in file(".")).aggregate(core.jvm).settings(name := "okay-face-root", publish / skip := true)
