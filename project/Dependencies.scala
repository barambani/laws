import sbt._

object Dependencies {

  val externalDependencies = Seq(
    "com.chuusai"     %% "shapeless"    % "2.3.3",
    "org.scalacheck"  %% "scalacheck"   % "1.14.0"  % "test"
  )
}
