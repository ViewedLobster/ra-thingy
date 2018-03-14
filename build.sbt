import Dependencies._

lazy val root = (project in file("."))
  .aggregate(core,plugin)
  .settings(
    inThisBuild(List(
      organization := "ellen",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "ra-thingy"
  )

lazy val core = (project in file("core")).
  settings(
    name := "core",
    scalacOptions ++= Seq("-feature"),
    libraryDependencies += scalaTest % Test
  )

lazy val plugin = (project in file("plugin")).
  settings(
    name := "plugin",
    scalacOptions ++= Seq("-feature"),
    libraryDependencies += { "org.scala-lang" % "scala-library" % scalaVersion.value },
    libraryDependencies += { "org.scala-lang" % "scala-compiler" % scalaVersion.value },
    libraryDependencies += { "org.scala-lang" % "scala-reflect" % scalaVersion.value },
    resourceDirectory in Compile := baseDirectory.value /"src"/"main"/"scala"/"ocapdatarace"/"embedded" 
  )

