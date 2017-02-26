name := "Project4_client"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "io.spray" %% "spray-can" % "1.3.3",
  "io.spray" %% "spray-httpx" % "1.3.3",
  "io.spray" %% "spray-http" % "1.3.3",
  "io.spray" %% "spray-routing" % "1.3.3",
  "io.spray" %% "spray-client" % "1.3.3",
  "io.spray" %% "spray-json" % "1.3.1",
  "com.typesafe.akka" %% "akka-actor" % "2.3.11",
  "com.typesafe.akka" %% "akka-testkit" % "2.3.11",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "junit" % "junit" % "4.12" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "com.typesafe.akka" %% "akka-remote" % "2.3.10",
  "net.liftweb" %% "lift-json" % "2.6.2",
  "com.typesafe.akka" %% "akka-slf4j" % "2.3.11",
  "ch.qos.logback" % "logback-classic" % "1.0.13",
  "commons-io" % "commons-io" % "2.4",
  "commons-codec" % "commons-codec" % "1.6"


)

libraryDependencies <++= scalaVersion(v =>
  Seq("org.scala-lang" % "scala-actors" % v)
)



    