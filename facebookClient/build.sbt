name := "sprayApiExample"

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies ++= {
  val akkaV = "2.3.9"
  val sprayV = "1.3.3"
  Seq(
    "io.spray"            %%  "spray-can"     % sprayV,
    "io.spray"            %%  "spray-servlet" % sprayV,
    "io.spray"            %%  "spray-routing" % sprayV,
    "io.spray"            %%  "spray-json"    % "1.3.1", //has not been updated yet
    "io.spray"            %%  "spray-client"    % "1.3.2",
    "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
    "commons-codec" % "commons-codec" % "1.10"
  )
}
