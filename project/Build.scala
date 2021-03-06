import sbt._
import Keys._

object TwinBuild extends Build {

  object V {
    val depProject = "master"
  }

  object Projects {
    lazy val depProject = RootProject(uri("git://github.com/pelotom/effectful.git#%s".format(V.depProject)))
  }

  lazy val scalaOpts = Seq(
    //      "-deprecation,",
    "-feature"
  )

  lazy val libDeps = Seq(
    "org.http4s" %% "http4s-dsl" % "0.8.3",
    "org.http4s" %% "http4s-blazeserver" % "0.8.3",
    "org.http4s" %% "http4s-blazeclient" % "0.8.3",
    "org.http4s" %% "http4s-core" % "0.8.3",
    "org.scalaz" %% "scalaz-core" % "7.1.3",
    "org.scalaz" %% "scalaz-concurrent" % "7.1.3",
    "com.typesafe" % "config" % "1.3.0",
    "io.dropwizard.metrics" % "metrics-json" % "3.1.2",
    "net.ceedubs" % "ficus_2.11" % "1.1.2",
    "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.4",
    "org.slf4j" % "slf4j-log4j12" % "1.7.12"
  )

  lazy val res = Seq(
    "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
    Resolver.sonatypeRepo("snapshots")
  )

  lazy val simple = Defaults.coreDefaultSettings ++ Seq(
    scalaVersion := "2.11.6",
    scalacOptions ++= scalaOpts,
    version := "0.5",
    fork in run := true,
    connectInput in run := true,
    resolvers ++= res,
    libraryDependencies ++= libDeps
  )

  // Library dependencies
  lazy val myProject = Project("Twin", file("."))
    .settings(simple)
    .dependsOn(Projects.depProject)

}
