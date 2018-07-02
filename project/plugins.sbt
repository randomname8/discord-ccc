addSbtPlugin("io.spray" % "sbt-revolver" % "0.9.1")
//addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.3")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.4")
dependsOn(RootProject(file("../../sbt-deployer")))