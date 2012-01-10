name := "metaphor"

organization := "net.metaphor"

version := "0.1.0"

scalaVersion := "2.9.1"

retrieveManaged := true

resolvers ++= Seq(
	"Java.net Maven2 Repository" at "http://download.java.net/maven/2/",
	"tqft.net Maven repository" at "http://tqft.net/releases",
	"Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
	"Sonatype Nexus Releases" at "https://oss.sonatype.org/content/repositories/releases",
	"Scala Snapshots" at "http://scala-tools.org/repo-snapshots/"
)

// Project dependencies
libraryDependencies ++= Seq(
	"net.tqft.toolkit" %% "toolkit" % "0.1.1",
	"org.bowlerframework" %% "core" % "0.5.1"
)

// lift-json
libraryDependencies ++= {
  val liftVersion = "2.4-M4" // Put the current/latest lift version here
  Seq(
    "net.liftweb" %% "lift-json" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-json-ext" % liftVersion % "compile->default"
    )
}

// Test dependencies
libraryDependencies ++= Seq(
	"junit" % "junit" % "4.8" % "test,dev",
	"org.scalatest" %% "scalatest" % "1.6.1" % "compile,test,dev",
	"org.mortbay.jetty" % "jetty" % "6.1.22" % "test,dev"
)

// Dependencies for jetty:run
libraryDependencies ++= Seq(
	"org.mortbay.jetty" % "jetty" % "6.1.22" % "container"
)

testOptions in Test += Tests.Filter(unitFilter)

testOptions in DevTest += Tests.Filter(devFilter)

seq(webSettings :_*)

port in container.Configuration := 8081

scalacOptions ++= Seq("-Xexperimental")

publishTo := Some(Resolver.sftp("toolkit.tqft.net Maven repository", "tqft.net", "tqft.net/releases") as ("scottmorrison", new java.io.File("/Users/scott/.ssh/id_rsa")))
