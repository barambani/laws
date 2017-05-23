lazy val commonSettings = Seq (
  scalaOrganization := "org.typelevel",
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

logLevel := Level.Info
