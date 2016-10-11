name := "indb"
 
version := "0.1"
 
scalaVersion := "2.11.8"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq (
  "org.http4s" %% "http4s-blaze-client" % "0.14.7a",
  "org.http4s" %% "http4s-circe"        % "0.14.7a",
  "io.circe"   %% "circe-generic"       % "0.5.2",
  "co.fs2"     %% "fs2-core"            % "0.9.1",
  "co.fs2"     %% "fs2-scalaz"          % "0.1.0"
)

scalacOptions ++= Seq(
  "-target:jvm-1.8",
  "-Xfatal-warnings",
  "-Xlint",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-deprecation",
  "-feature",
  "-language:_",
  "-unchecked"
)

val opsNotForConsole = Set("-Ywarn-unused-import", "-Xfatal-warnings")

scalacOptions in (Compile, console) ~= { _ filterNot opsNotForConsole.apply }
scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value

testOptions in Test += Tests.Argument("-oF")

