name := "Twin"

version := "1.0"

scalaVersion := "2.11.6"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "org.http4s" %% "http4s-dsl" % "0.8.2" // to use the core dsl
libraryDependencies += "org.http4s" %% "http4s-blazeserver" % "0.8.2" // to use the blaze backend
libraryDependencies += "org.http4s" %% "http4s-blazeclient" % "0.8.2" // to use the blaze client
libraryDependencies += "org.http4s" %% "http4s-core" % "0.8.2"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.3"
libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.1.3"

libraryDependencies += "com.typesafe" % "config" % "1.3.0"