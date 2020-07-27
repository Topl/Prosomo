import sbt.Keys.organization
import sbtassembly.MergeStrategy

name := "prosomo"

trapExit := false

lazy val commonSettings = Seq(
  scalaVersion := "2.12.8",
  organization := "co.topl",
  version := "0.7.8"
)

scalaVersion := "2.12.8"
organization := "co.topl"
version := "0.7.8"

mainClass in (Compile, run) := Some("prosomo.Prosomo")
mainClass in runMain := Some("prosomo.Prosomo")
mainClass in stage := Some("prosomo.Prosomo")
mainClass in assembly := Some("prosomo.Prosomo")

test in assembly := {}

enablePlugins(UniversalPlugin)
enablePlugins(JavaAppPackaging)
enablePlugins(DockerPlugin)


val circeVersion = "0.9.0"
val akkaVersion = "2.5.24"
val akkaHttpVersion = "10.1.9"

resolvers ++= Seq("Bintray" at "https://jcenter.bintray.com/")

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1"

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "iodb" % "0.3.+",
  "org.iq80.leveldb" % "leveldb" % "0.12"
)

val networkDependencies = Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-http-core" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-parsing" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-protobuf" % akkaVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "org.bitlet" % "weupnp" % "0.1.4",
  "commons-net" % "commons-net" % "3.6"
)

val apiDependencies = Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "de.heikoseeberger" %% "akka-http-circe" % "1.20.0"
)

val loggingDependencies = Seq(
  "ch.qos.logback" % "logback-classic" % "1.3.0-alpha4"
)

val scorexUtil = "org.scorexfoundation" %% "scorex-util" % "0.1.6"

val testingDependencies = Seq(
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test",
  "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % "test",
  "org.scalactic" %% "scalactic" % "3.0.3" % "test",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+",
  scorexUtil, (scorexUtil % Test).classifier("tests")
)

libraryDependencies ++= Seq(
  "com.iheart" %% "ficus" % "1.4.2",
  "org.scorexfoundation" %% "scrypto" % "2.1.7",
  scorexUtil
) ++ networkDependencies ++ apiDependencies ++ loggingDependencies ++ testingDependencies

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-feature",
  "-deprecation")

testOptions in Test += Tests.Argument("-oD", "-u", "target/test-reports")

pomIncludeRepository := { _ => false }

val credentialFile = Path.userHome / ".ivy2" / ".credentials"
credentials ++= (for {
  file <- if (credentialFile.exists) Some(credentialFile) else None
} yield Credentials(file)).toSeq

lazy val testkit = Project(id = "testkit", base = file(s"testkit"))
  .dependsOn(basics)
  .settings(commonSettings: _*)

lazy val examples = Project(id = "examples", base = file(s"examples"))
  .dependsOn(basics, testkit)
  .settings(commonSettings: _*)

lazy val basics = Project(id = "scorex", base = file("."))
  .settings(commonSettings: _*)

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq

//publishing settings

publishMavenStyle := true

publishArtifact in Test := false

parallelExecution in Test := false

logBuffered in Test := false

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-f", "sbttest.log", "-oDG")

fork := false

pomIncludeRepository := { _ => false }

homepage := Some(url("https://github.com/Topl/Prosomo"))

assemblyMergeStrategy in assembly ~= { old: ((String) => MergeStrategy) => {
    case ps if ps.endsWith(".SF")      => MergeStrategy.discard
    case ps if ps.endsWith(".DSA")     => MergeStrategy.discard
    case ps if ps.endsWith(".RSA")     => MergeStrategy.discard
    case ps if ps.endsWith(".xml")     => MergeStrategy.first
    case PathList("module-info.class") => MergeStrategy.discard
    case PathList("module-info.java")  => MergeStrategy.discard
    case "META-INF/truffle/instrument" => MergeStrategy.concat
    case "META-INF/truffle/language"   => MergeStrategy.rename
    case x => old(x)
  }
}
