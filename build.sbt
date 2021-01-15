val dottyVersion = "0.27.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := "2.13.1",

    libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.0",

    scalacOptions ++= Seq(
    "-Xfatal-warnings"
    )
  )
