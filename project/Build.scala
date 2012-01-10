import sbt._

object MetaphorBuild extends Build {
  lazy val root = Project("metaphor", file("."))
			.configs( DevTest )
			.settings( inConfig(DevTest)(Defaults.testTasks) : _*)

   def unitFilter(name: String): Boolean = name endsWith "DevTest"
   def devFilter(name: String): Boolean = (name endsWith "Test") && !unitFilter(name)

   lazy val DevTest = config("dev") extend (Test)
}
