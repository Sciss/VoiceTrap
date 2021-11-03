lazy val root = project.in(file("."))
  .settings(
    name          := "VoiceTrap",
    version       := "0.1.0",
    organization  := "de.sciss",
    homepage      := Some(url("https://github.com/Sciss/VoiceTrap")),
    licenses      := Seq("GPL v2+" -> url("https://www.gnu.org/licenses/gpl-2.0.txt")),
    scalaVersion  := "2.10.7",
    resolvers += "Oracle Repository" at "https://download.oracle.com/maven", // required for sleepycat
    libraryDependencies ++= Seq(
      "de.sciss" %% "soundprocesses"  % "1.3.1", // "1.1.2",
      "de.sciss" %% "strugatzki"      % "1.3.0", // "1.1.0"
      "de.sciss" %% "lucrestm-bdb"    % "1.6.0",
    ),
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-no-specialization"), // "-Xelide-below", "INFO"
    Test / testOptions += Tests.Argument("-oF"),
    run / fork := true, // required for shutdown hook, and also the scheduled thread pool, it seems
    // ---- packaging ----
    //    Test / assembly := {}
  )
