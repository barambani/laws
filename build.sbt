lazy val commonSettings = Seq (
  scalaOrganization in ThisBuild := "org.typelevel",
  scalaVersion := "2.12.2-bin-typelevel-4"
)

lazy val `laws` = (project in file(".")).settings(commonSettings: _*)

initialCommands := """
  import MonadLaws._
"""

scalacOptions ++= Seq (
  "-feature",
  "-deprecation",
  "-target:jvm-1.8"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.13.5" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

logLevel := Level.Info
