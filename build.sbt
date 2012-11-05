import AssemblyKeys._

name := "VoiceTrap"

version := "0.0.1-SNAPSHOT"

organization := "de.sciss"

homepage := Some( url( "https://github.com/Sciss/VoiceTrap" ))

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

scalaVersion := "2.9.2" // "2.10.0-M7"

// scalaBinaryVersion := "2.10.0-M7"

// crossScalaVersions in ThisBuild := Seq( "2.10.0-M6", "2.9.2" )

libraryDependencies ++= Seq(
   "de.sciss" %% "soundprocesses" % "1.1.0-SNAPSHOT",
   "de.sciss" %% "strugatzki" % "1.0.0-SNAPSHOT"
)

// libraryDependencies in ThisBuild <+= scalaVersion { sv =>
//    val v = sv match {
//       case "2.10.0-M7" => "1.9-2.10.0-M7-B1"
//       case _ => "1.8"
//    }
//    "org.scalatest" %% "scalatest" % v % "test"
// }

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked", "-no-specialization" )   // "-Xelide-below", "INFO"

testOptions in Test += Tests.Argument("-oF")

fork in run := true  // required for shutdown hook, and also the scheduled thread pool, it seems

// ---- packaging ----

seq( assemblySettings: _* )

test in assembly := {}
