import sbt._

object MetaphorBuild extends Build {
  lazy val root = Project("metaphor", file("."))
			.configs( DevTest )

	def unitFilter(name: String): Boolean = name.endsWith("Test") && !devFilter(name)
	def devFilter(name: String): Boolean = name.endsWith("DevTest")

   lazy val DevTest = config("dev") extend (Test)
}
