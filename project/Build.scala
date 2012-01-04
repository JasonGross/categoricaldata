import sbt._

object MetaphorBuild extends Build {
  lazy val root = Project("metaphor", file("."))
			.configs( DevTest )
			.settings( inConfig(DevTest)(Defaults.testSettings) : _*)

   lazy val DevTest = config("dev") extend (Test)
}
