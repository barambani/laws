import sbt._

object Dependencies {

  val externalDependencies = Seq(
    "com.chuusai"     %% "shapeless"    % "2.3.3",
    "com.github.ghik" %% "silencer-lib" % "1.4.2",
    "org.scalacheck"  %% "scalacheck"   % "1.14.0"  % "test",
    compilerPlugin("com.github.ghik" %% "silencer-plugin" % "1.4.2")
  )
}
