name := "ccc-discord"
version := "0.1"

scalaVersion := "2.12.6"
fork := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Yno-adapted-args", "-Xlint", "-Ypartial-unification", "-opt:_", "-opt-warnings:_", "-Ywarn-extra-implicit", "-Ywarn-inaccessible", "-Ywarn-infer-any", "-Ywarn-nullary-override", "-Ywarn-nullary-unit", "-Ywarn-numeric-widen")
scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:_", "-opt:_", "-Xlint")

lazy val ccc = RootProject(file("../ccc"))
lazy val headache = RootProject(file("../headache"))
lazy val root = Project("ccc-discord", file(".")).enablePlugins(DeployerPlugin).dependsOn(ccc, headache)

libraryDependencies ++= Seq(
  "com.beachape" %% "enumeratum" % "1.5.12",
  "org.scala-stm" %% "scala-stm" % "0.8",
  "org.slf4j" % "slf4j-simple" % "1.7.25",
  "com.twitter" %% "chill" % "0.9.1",
  "io.dropwizard.metrics" % "metrics-core" % "3.1.0",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.openjdk.jol" % "jol-core" % "0.9" % "test",
)
resolvers += "jitpack.io" at "https://jitpack.io"
mainClass in reStart := Some("ccc.DevAppReloader")


mainClass in (Compile, packageBin) := Some("discordccc.CccDiscord")
javaOptions in (Proguard, proguard) := Seq("-Xss8M")
proguardOptions in Proguard ++= Seq(
  "-dontwarn", "-ignorewarnings", "-printmapping proguard-obfuscation-mappings",
  "-optimizations class/merging/vertical,class/marking/final,code/simplification/*", "-optimizationpasses 2",
  //"-dontoptimize",
  "-dontobfuscate",
  ProguardOptions.keepMain("discordccc.CccDiscord"),
  """-keep enum org.nibor.autolink.LinkType {
    public protected *;
  }""")
proguardInputFilter in Proguard := {
  case f if f.name contains "JDA-3.5.1_339.jar" => Some("!natives/**")
  case file => None
}
proguardMergeStrategies in Proguard += ProguardMerge.first("META-INF/MANIFEST.MF")


//add resources to the resulting compact jar
proguardOutputFilter in Proguard := { f => Some("!**.png") }
mappings in compactDeploy := {
  var resourcesDirectories = (Compile/unmanagedResourceDirectories).value
  val localMappings = (Compile/resources).value.filter(f => f.ext == "png" || f.ext == "css").map(f => f -> resourcesDirectories.iterator.flatMap(f relativeTo _).next.toString)
  resourcesDirectories = (ccc/Compile/unmanagedResourceDirectories).value
  val cccMappings = (ccc/Compile/resources).value.filter(f => f.ext == "png" || f.ext == "css").map(f => f -> resourcesDirectories.iterator.flatMap(f relativeTo _).next.toString)
  localMappings ++ cccMappings
}

//need to add jmods in java9
proguardLibraries in Proguard ++= {
  import scala.sys.process._
  val productJar = (Compile/exportedProductJars).value.head
  val jmods = Seq("jdeps", "-s", productJar.data.getAbsolutePath).!!.split("\n").map(_.split(" -> ").last).filterNot("not found".==).toSeq
  jmods map (mod => file(scala.util.Properties.javaHome + s"/jmods/$mod.jmod"))
}
