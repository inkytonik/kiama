// Dependencies

// The dsinfo and dsprofile dependencies are here so that they end up
// in published dependencies since they are needed by the core.

libraryDependencies ++= Seq (
    "org.bitbucket.inkytonik.dsinfo" %% "dsinfo" % "0.2.0",
    "org.bitbucket.inkytonik.dsprofile" %% "dsprofile" % "0.2.0",
    "jline" % "jline" % "2.11",
    "org.scalacheck" %% "scalacheck" % "1.10.1" % "test",
    "org.scalatest" %% "scalatest" % "1.9.2" % "test"
)

// Interactive settings

shellPrompt <<= (name, version) { (n, v) =>
     _ => "kiama " + n + " " + v + "> "
}

// Source code locations

// Specify how to find source and test files.  Main sources are
//    - in src directory
//    - all .scala files, except
// Test sources, which are
//    - files whose names end in Tests.scala, which are actual test sources
//    - Scala files within the examples src

scalaSource in Compile <<= baseDirectory { _ / "src" }

scalaSource in Test <<= scalaSource in Compile

unmanagedSources in Test <<= (scalaSource in Test) map { s => {
    val egs = s / "org" / "kiama" / "example" ** "*.scala"
    ((s ** "*Tests.scala") +++ egs).get
}}

unmanagedSources in Compile <<=
    (scalaSource in Compile, unmanagedSources in Test) map { (s, tests) =>
        ((s ** "*.scala") --- tests).get
    }

// Resources

unmanagedResourceDirectories in Compile <<= (scalaSource in Compile) { Seq (_) }

unmanagedResourceDirectories in Test <<= unmanagedResourceDirectories in Compile

// There are no compile resources
unmanagedResources in Compile := Seq ()

// Test resources are the non-Scala files in the source that are not hidden
unmanagedResources in Test <<= (scalaSource in Test) map { s => {
    (s ** (-"*.scala" && -HiddenFileFilter)).get
}}

// Fork the runs and connect sbt's input and output to the forked process so
// that we are immune to version clashes with the JLine library used by sbt

fork in run := true

connectInput in run := true

outputStrategy in run := Some (StdoutOutput)

// Don't run tests in parallel because some bits are not thread safe yet

parallelExecution in Test := false

// Some useful imports for demos and testing in console

initialCommands in console := """
    import org.kiama._
    import attribution.Attribution._
    import rewriting.Rewriter._
""".stripMargin

initialCommands in console in Test <<= (initialCommands in console) { cmds =>
    cmds + """
        import example.json.JSONTree._
        import example.json.PrettyPrinter._
    """.stripMargin
}

// No publishing, it's done in the root project

publish := {}

publishLocal := {}
