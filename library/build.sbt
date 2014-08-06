import com.typesafe.sbt.pgp.PgpKeys.{publishSigned, publishLocalSigned}

// Source code locations

// Specify how to find source and test files.  Main sources are
//    - in src directory
//    - all .scala files, except
// Test sources, which are
//    - files whose names end in Tests.scala, which are actual test sources
//    - Scala files within the examples src

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := (scalaSource in Compile).value

unmanagedSources in Test := {
    val s = (scalaSource in Test).value
    val egs = s / "org" / "kiama" / "example" ** "*.scala"
    ((s ** "*Tests.scala") +++ egs).get
}

unmanagedSources in Compile := {
    val s = (scalaSource in Compile).value
    val tests = (unmanagedSources in Test).value
    ((s ** "*.scala") --- tests).get
}

// Resources

unmanagedResourceDirectories in Compile := Seq ((scalaSource in Compile).value)

unmanagedResourceDirectories in Test := (unmanagedResourceDirectories in Compile).value

// There are no compile resources
unmanagedResources in Compile := Seq ()

// Test resources are the non-Scala files in the source that are not hidden
unmanagedResources in Test := {
    val s = (scalaSource in Test).value
    (s ** (-"*.scala" && -HiddenFileFilter)).get
}

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

initialCommands in console in Test :=
    (initialCommands in console).value + """
        import example.json.JSONTree._
        import example.json.PrettyPrinter._
    """.stripMargin

// No publishing, it's done in the root project

publish := {}

publishLocal := {}

publishSigned := {}

publishLocalSigned := {}
