import sbt._, Keys._

object Commons {
  lazy val ccc = RootProject(file("../ccc"))
  lazy val headache = RootProject(file("../headache"))
}
