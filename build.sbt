// import AssemblyKeys._

name := "VoiceTrap"

version := "0.2.1-SNAPSHOT" // analysis only!

organization := "de.sciss"

homepage := Some( url( "https://github.com/Sciss/VoiceTrap" ))

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

scalaVersion := "2.10.3"

resolvers += "Oracle Repository" at "http://download.oracle.com/maven"  // required for sleepycat

libraryDependencies ++= Seq(
  "de.sciss" %% "soundprocesses"    % "1.3.2+",
  "de.sciss" %% "lucrestm-bdb"      % "1.6.+",
  "de.sciss" %% "strugatzki"        % "1.3.+",
  "de.sciss" %% "play-json-sealed"  % "0.1.+",
  "de.sciss" %% "fileutil"          % "1.0.+"
)

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-no-specialization", "-feature")   // "-Xelide-below", "INFO"

testOptions in Test += Tests.Argument("-oF")

fork in run := true  // required for shutdown hook, and also the scheduled thread pool, it seems

// ---- packaging ----

// seq( assemblySettings: _* )
//
// test in assembly := {}
