name := """PapaCarloTutorial"""

version := "1.0"

scalaVersion := "2.11.7"

// Change this to another test framework if you prefer
libraryDependencies ++= Seq(
  "name.lakhin.eliah.projects.papacarlo" % "papa-carlo_2.11" % "0.8.0-SNAPSHOT"
)

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

