// Dependencies

libraryDependencies ++= Seq (
    "org.bitbucket.inkytonik.dsname" %% "dsname" % "0.1.0-SNAPSHOT"
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
    (s ** "*Tests.scala").get
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

// Documentation

// Link the documentation to the source in the main repository

scalacOptions in (Compile, doc) <++= baseDirectory map {
    bd => Seq (
        "-sourcepath",
            bd.getAbsolutePath,
        "-doc-source-url",
            "https://code.google.com/p/kiama/source/browse€{FILE_PATH}.scala"
    )
}

