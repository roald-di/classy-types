Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = project
  .in(file("."))
  .settings(
    scalaVersion :=  "3.2.1",
//    scalaVersion :=  "3.3.0-RC1-bin-20221224-6f5bb34-NIGHTLY",
//    scalacOptions ++= Seq("-language:experimental.erasedDefinitions")
  )
