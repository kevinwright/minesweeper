import sbt._

trait Defaults {
  def androidPlatformName = "android-7"
}
class Android(info: ProjectInfo) extends ParentProject(info) {
  override def shouldCheckOutputDirectories = false
  override def updateAction = task { None }

  lazy val main  = project(".", "android", new MainProject(_))
  lazy val tests = project("tests",  "tests", new TestProject(_), main)

  class MainProject(info: ProjectInfo) extends AndroidProject(info) with Defaults with IdeaPlugin {
    val scalaToolsSnapshots = ScalaToolsSnapshots
    val mavenLocal = "Local Maven Repository" at  "file://"+Path.userHome+"/.m2/repository"

    val scalatest = "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.RC3-SNAPSHOT" % "test"
    val core = "lsug.scaladojo.minesweeper" % "core" % "1.0-SNAPSHOT"
  }

  class TestProject(info: ProjectInfo) extends AndroidTestProject(info) with Defaults
}