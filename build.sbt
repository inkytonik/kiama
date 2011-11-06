name := "kiama"

version := "1.2.0-SNAPSHOT"

organization := "com.googlecode"

// Scala compiler settings

scalaVersion := "2.9.1"

scalacOptions ++= Seq ("-deprecation", "-unchecked")

// Interactive settings

logLevel := Level.Info

shellPrompt <<= (name, version) { (n, v) => _ => n + " " + v + "> " }

// Execution

mainClass := None

parallelExecution in Test := false

// Dependencies

libraryDependencies ++= 
    Seq (
        "org.scala-lang" % "jline" % "2.9.1",
        "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test",
        "org.scalatest" %% "scalatest" % "1.6.1" % "test",
        "junit" % "junit" % "4.8.1" % "test"
    )

// Source code locations

// Specyify how to find source and test files.  Main sources are
//    - in src directory
//    - all .scala files, except
// Test sources, which are
//    - files whose names end in Tests.scala, which are actual test sources
//    - Scala files within the examples src

scalaSource <<= baseDirectory { _ / "src" }

unmanagedSources in Test <<= scalaSource map { s => {
    val egs = s / "org" / "kiama" / "example" ** "*.scala"
    ((s ** "*Tests.scala") +++ egs).get
}}

unmanagedSources in Compile <<= (scalaSource, unmanagedSources in Test) map { (s, tests) =>
    ((s ** "*.scala") --- tests).get
}

// Resources

unmanagedResourceDirectories <<= scalaSource { Seq (_) }

unmanagedResourceDirectories in Test <<= unmanagedResourceDirectories

// Test resources are the non-Scala files in the source that are not hidden
unmanagedResources in Test <<= scalaSource map { s => {
    (s ** (-"*.scala" && -HiddenFileFilter)).get
}}

// Publishing

publishTo <<= version { v =>
    val nexus = "http://nexus.scala-tools.org/content/repositories/"
    if (v.trim.endsWith ("SNAPSHOT"))
        Some ("snapshots" at nexus + "snapshots/") 
    else
        Some ("releases" at nexus + "releases/")
}

credentials += Credentials (Path.userHome / ".ivy2" / ".credentials")
