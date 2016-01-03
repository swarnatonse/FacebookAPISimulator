//name := "project2"
//
//version := "1.0"
//
//scalaVersion := "2.11.7"
//
//libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.4"
//libraryDependencies += "com.typesafe.akka" % "akka-remote_2.11" % "2.3.4"

name := "sprayApiExample"

version := "1.0"

scalaVersion := "2.11.2"

libraryDependencies ++= {
  val akkaV = "2.3.6"
  val sprayV = "1.3.2"
  Seq(
    "io.spray"            %%  "spray-can"     % sprayV,
    "io.spray"            %%  "spray-routing" % sprayV,
    "io.spray"            %%  "spray-json"    % "1.3.1", //has not been updated to 1.3.2 yet
    "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
    "commons-io" % "commons-io" % "2.4",
    "commons-codec" % "commons-codec" % "1.6"
  )
}
