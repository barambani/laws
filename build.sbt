inThisBuild(Seq(
  scalaOrganization := "org.typelevel",
  scalaVersion := "2.12.3-bin-typelevel-4"
))

lazy val prjcSettings = Seq (
  version := "1.0.0",
  name := "Laws"
)

lazy val `laws` = (project in file(".")).settings(prjcSettings: _*)

scalacOptions ++= Seq (
  "-feature",
  "-deprecation",
  "-Ywarn-unused-import",
  "-unchecked",
  "-Yno-adapted-args",
  "-Ywarn-value-discard",
  "-Ywarn-dead-code",
  "-target:jvm-1.8"
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.scalacheck" %% "scalacheck" % "1.13.5" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

logLevel := Level.Info
