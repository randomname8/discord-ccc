name := "ccc-discord"
version := "0.1"

scalaVersion := "2.12.6"
fork := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Yno-adapted-args", "-Xlint", "-Ypartial-unification", "-opt-warnings:_", "-Ywarn-extra-implicit", "-Ywarn-inaccessible", "-Ywarn-infer-any", "-Ywarn-nullary-override", "-Ywarn-nullary-unit", "-Ywarn-numeric-widen")
scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:_", "-opt:_", "-Xlint")

import Commons._

lazy val root = Project("ccc-discord", file(".")).dependsOn(ccc, headache)
lazy val jmh = Project("ccc-discord-jmh", file("jmh")).dependsOn(root).enablePlugins(JmhPlugin)

libraryDependencies ++= Seq(
  "com.beachape" %% "enumeratum" % "1.5.12",
  "org.scala-stm" %% "scala-stm" % "0.8",
  "io.github.soc" %% "regextractor" % "0.2",
  "org.slf4j" % "slf4j-simple" % "1.7.25",
  "com.twitter" %% "chill" % "0.9.1",
  "com.boundary" % "high-scale-lib" % "1.0.6",
  "org.agrona" % "agrona" % "0.9.21",
  "io.dropwizard.metrics" % "metrics-core" % "3.1.0",
  "com.sun.activation" % "javax.activation" % "1.2.0",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.openjdk.jol" % "jol-core" % "0.9" % "test",
)
resolvers += "jitpack.io" at "https://jitpack.io"
mainClass in reStart := Some("ccc.DevAppReloader")

enablePlugins(JavaAppPackaging)
mappings in (Compile, packageDoc) := Seq()
javaOptions in Universal ++= Seq(
  //"-J-Xmx120m",
  "-J-Xms120m",
  "-J-Xss256K",
  "-J-XX:CICompilerCount=2",
  "-J-XX:VMThreadStackSize=2048",
  "-Dprism.lcdtext=false",
  "-Dprism.text=t2k",
  "-Dcom.sun.javafx.fontSize=22",
)


mainClass in (Compile, packageBin) := Some("discordccc.DiscordChat")
