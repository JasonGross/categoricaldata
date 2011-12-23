name := ""

organization := ""

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.0"

retrieveManaged := true

resolvers ++= Seq(
	"Java.net Maven2 Repository" at "http://download.java.net/maven/2/",
	"tqft.net Maven repository" at "http://tqft.net/releases",
	"Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
	"Sonatype Nexus Releases" at "https://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
	"junit" % "junit" % "4.8" % "test",
	"org.scalatest" %% "scalatest" % "1.6.1" % "test",
	"net.tqft.toolkit" %% "toolkit" % "1.0-SNAPSHOT"
)

scalacOptions ++= Seq("-Xexperimental")

publishTo := Some(Resolver.sftp("toolkit.tqft.net Maven repository", "tqft.net", "tqft.net/releases") as ("scottmorrison", new java.io.File("/Users/scott/.ssh/id_rsa")))
