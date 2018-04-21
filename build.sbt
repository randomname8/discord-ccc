name := "ccc-discord"
version := "0.1"

scalaVersion := "2.12.4"
fork := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Yinfer-argument-types", "-Yno-adapted-args", "-Xlint", "-Ypartial-unification", "-opt:_", "-opt-warnings:_", "-Ywarn-extra-implicit", "-Ywarn-inaccessible", "-Ywarn-infer-any", "-Ywarn-nullary-override", "-Ywarn-nullary-unit", "-Ywarn-numeric-widen")
scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:_", "-opt:_", "-Xlint")

lazy val ccc = RootProject(file("../ccc"))
lazy val root = Project("ccc-discord", file(".")).enablePlugins(DeployerPlugin).dependsOn(ccc)

libraryDependencies ++= Seq(
  "org.asynchttpclient" % "async-http-client" % "2.0.33",
  "com.beachape" %% "enumeratum" % "1.5.12",
  "com.github.benhutchison" %% "prickle" % "1.1.14",
  "org.json4s" %% "json4s-native" % "3.5.2",
  "org.scala-stm" %% "scala-stm" % "0.8",
  "net.dv8tion" % "JDA" % "3.5.1_339" exclude ("net.java.dev.jna", "jna"),
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
