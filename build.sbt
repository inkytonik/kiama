import com.typesafe.sbt.pgp.PgpKeys.{publishSigned, publishLocalSigned}

import scalariform.formatter.preferences._
import SbtScalariform.ScalariformKeys

import sbtunidoc.Plugin.UnidocKeys.unidoc

// Settings for entire build

version in ThisBuild := "2.1.0-SNAPSHOT"

organization in ThisBuild := "org.bitbucket.inkytonik.kiama"

scalaVersion in ThisBuild := "2.12.2"
crossScalaVersions in ThisBuild := Seq ("2.12.2", "2.11.11", "2.10.6")

scalacOptions in ThisBuild := {
    // Turn on all lint warnings, except:
    //  - stars-align: incorrectly reports problems if pattern matching of
    //    unapplySeq extractor doesn't match sequence directly
    val lintOption =
        if (scalaVersion.value.startsWith("2.10"))
            "-Xlint"
        else
            "-Xlint:-stars-align,_"
    Seq(
        "-deprecation",
        "-feature",
        "-sourcepath", baseDirectory.value.getAbsolutePath,
        "-unchecked",
        "-Xcheckinit",
        "-Xfatal-warnings",
        lintOption
    )
}

resolvers in ThisBuild ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
)

libraryDependencies in ThisBuild ++= {
    val dsinfoVersion =
        if (scalaVersion.value.startsWith("2.10"))
            "0.3.0"
        else
            "0.4.0"
    val dsprofileVersion =
        if (scalaVersion.value.startsWith("2.10"))
            "0.3.0"
        else
            "0.4.0"
    Seq(
        // Caching:
        "com.google.code.findbugs" % "jsr305" % "3.0.2",
        "com.google.guava" % "guava" % "21.0",
        // DSL support:
        "org.bitbucket.inkytonik.dsinfo" %% "dsinfo" % dsinfoVersion,
        // Profiling:
        "org.bitbucket.inkytonik.dsprofile" %% "dsprofile" % dsprofileVersion,
        // Command-line handling:
        "org.rogach" %% "scallop" % "2.1.1",
        // Reflection
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        // REPLs:
        "jline" % "jline" % "2.14.3",
        // Testing:
        "org.scalacheck" %% "scalacheck" % "1.13.5" % "test",
        "org.scalatest" %% "scalatest" % "3.0.2" % "test"
    )
}

incOptions in ThisBuild :=
    (incOptions in ThisBuild).value.
        withNameHashing(true).
        withLogRecompileOnMacro(false)

logLevel in ThisBuild := Level.Info

shellPrompt in ThisBuild := {
    state =>
        Project.extract(state).currentRef.project + " " + version.value +
            " " + scalaVersion.value + "> "
}

mainClass in ThisBuild := None

// Project settings

val subProjectSettings =
    scalariformSettings ++
    Seq(
        // No publishing, it's done in the root project

        publish := {},
        publishLocal := {},
        publishSigned := {},
        publishLocalSigned := {},

        // Source code formatting

        ScalariformKeys.preferences :=
            ScalariformKeys.preferences.value
                .setPreference(AlignSingleLineCaseStatements, true)
                .setPreference(IndentSpaces, 4)
                .setPreference(SpaceBeforeColon, true)
                .setPreference(SpacesAroundMultiImports, false)
    )

// Project configuration:
//   - core project containing macros and code that they need
//   - library project containing everything else, including all tests
//   - kiama (root) project aggregates core and library

def setupProject(project : Project, projectName : String) : Project =
    project.settings(
        name := projectName
    )

def setupSubProject(project : Project, projectName : String) : Project =
    setupProject(
        project,
        projectName
    ).settings(
        subProjectSettings : _*
    )

lazy val core =
    setupSubProject(
        project in file("core"),
        "core"
    )

lazy val library =
    setupSubProject(
        project in file("library"),
        "library"
    ).settings(
        javaOptions ++= Seq("-Xss8M"),
        fork := true,
        connectInput in run := true,
        outputStrategy in run := Some(StdoutOutput),
        initialCommands in console := """
            import org.bitbucket.inkytonik.kiama._
            import rewriting.Rewriter._
        """.stripMargin,
        initialCommands in console in Test :=
            (initialCommands in console).value + """
                import example.json.JSONTree._
                import example.json.PrettyPrinter._
            """.stripMargin
    ).dependsOn(
        core % "compile-internal, test-internal"
    )

lazy val kiama =
    setupProject(
        project in file("."),
        "kiama"
    ).settings(
        unidocSettings : _*
    ).settings(
        // File mappings

        mappings in (Compile, packageBin) :=
            (mappings in (core, Compile, packageBin)).value ++
            (mappings in (library, Compile, packageBin)).value,
        mappings in (Compile, packageSrc) :=
            (mappings in (core, Compile, packageSrc)).value ++
            (mappings in (library, Compile, packageSrc)).value,
        mappings in (Test, packageBin) :=
            (mappings in (library, Test, packageBin)).value,
        mappings in (Test, packageSrc) :=
            (mappings in (library, Test, packageSrc)).value,

        // Unidoc

        doc in Compile := (doc in ScalaUnidoc).value,
        doc in Test := (doc in TestScalaUnidoc).value,
        target in unidoc in ScalaUnidoc := crossTarget.value / "api",
        target in unidoc in TestScalaUnidoc := crossTarget.value / "test-api",
        scalacOptions in (ScalaUnidoc, unidoc) ++= {
            val macroExpandOption =
                if (scalaVersion.value.startsWith("2.10"))
                "-Ymacro-no-expand"
                else
                "-Ymacro-expand:none"
                Seq(
                    macroExpandOption,
                    "-doc-source-url",
                    "https://bitbucket.org/inkytonik/kiama/src/default€{FILE_PATH}.scala"
                )
            },
        scalacOptions in (TestScalaUnidoc, unidoc) := (scalacOptions in (ScalaUnidoc, unidoc)).value,

        // Publishing

        publishTo := {
            val nexus = "https://oss.sonatype.org/"
            if (version.value.trim.endsWith("SNAPSHOT"))
                Some("snapshots" at nexus + "content/repositories/snapshots")
            else
                Some("releases" at nexus + "service/local/staging/deploy/maven2")
        },
        publishMavenStyle := true,
        publishArtifact in Test := true,
        pomIncludeRepository := { x => false },
        pomExtra := (
            <url>https://bitbucket.org/inkytonik/kiama</url>
            <licenses>
                <license>
                    <name>LGPL 3.0 license</name>
                    <url>http://www.opensource.org/licenses/lgpl-3.0.html</url>
                    <distribution>repo</distribution>
                </license>
            </licenses>
            <scm>
                <url>https://bitbucket.org/inkytonik/kiama</url>
                <connection>scm:hg:https://bitbucket.org/inkytonik/kiama</connection>
            </scm>
            <developers>
                <developer>
                   <id>inkytonik</id>
                   <name>Tony Sloane</name>
                   <url>https://bitbucket.org/inkytonik</url>
                </developer>
            </developers>
        )

    ).aggregate(core, library)
