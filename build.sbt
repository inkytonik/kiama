import com.typesafe.sbt.pgp.PgpKeys.{publishSigned, publishLocalSigned}

import scalariform.formatter.preferences._

// Settings for entire build

ThisBuild/version := "2.2.1-SNAPSHOT"

ThisBuild/organization := "org.bitbucket.inkytonik.kiama"

ThisBuild/scalaVersion := "2.12.8"
ThisBuild/crossScalaVersions := Seq ("2.12.8", "2.11.12", "2.10.7")

ThisBuild/scalacOptions := {
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
        "-Yrangepos",
        lintOption
    )
}

ThisBuild/resolvers ++=
    Seq(
        Resolver.sonatypeRepo("releases"),
        Resolver.sonatypeRepo("snapshots")
    )

ThisBuild/incOptions := (ThisBuild/incOptions).value.withLogRecompileOnMacro(false)

ThisBuild/logLevel := Level.Info

ThisBuild/shellPrompt := {
    state =>
        Project.extract(state).currentRef.project + " " + version.value +
            " " + scalaVersion.value + "> "
}

ThisBuild/mainClass := None

// Common project settings

val commonSettings =
    Seq(
        libraryDependencies :=
            Seq(
                "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
                "org.scalatest" %% "scalatest" % "3.0.5" % "test"
            ),

        // Formatting
        scalariformPreferences := scalariformPreferences.value
            .setPreference(AlignSingleLineCaseStatements, true)
            .setPreference(DanglingCloseParenthesis, Force)
            .setPreference(IndentSpaces, 4)
            .setPreference(SpaceBeforeColon, true)
            .setPreference(SpacesAroundMultiImports, false),

        // Publishing
        publishTo := {
            val nexus = "https://oss.sonatype.org/"
            if (version.value.trim.endsWith("SNAPSHOT"))
                Some("snapshots" at nexus + "content/repositories/snapshots")
            else
                Some("releases" at nexus + "service/local/staging/deploy/maven2")
        },
        publishMavenStyle := true,
        Test/publishArtifact := true,
        pomIncludeRepository := { _ => false },
        pomExtra := (
            <url>https://bitbucket.org/inkytonik/kiama</url>
            <licenses>
                <license>
                    <name>Mozilla Public License, v. 2.0</name>
                    <url>http://mozilla.org/MPL/2.0/</url>
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
    )

// Project configuration:
//   - base project containing macros and code that they need
//   - core project containing main Kiama functionality, including its tests
//   - extras project containing utilities, including their tests and examples
//   - kiama (root) project aggregates base, core and extras

def setupProject(project : Project, projectName : String) : Project =
    project.settings(
        name := projectName
    )

def setupSubProject(project : Project, projectName : String) : Project =
    setupProject(
        project,
        projectName
    ).enablePlugins(
        ScalaUnidocPlugin
    ).settings(
        commonSettings : _*
    )

def baseLibraryDependencies (scalaVersion : String) : Seq[ModuleID] = {
    val dsinfoVersion =
        if (scalaVersion.startsWith("2.10"))
            "0.3.0"
        else
            "0.4.0"
    val dsprofileVersion =
        if (scalaVersion.startsWith("2.10"))
            "0.3.0"
        else
            "0.4.0"
    Seq(
        // Caching:
        "com.google.guava" % "guava" % "27.0-jre",
        // DSL support:
        "org.bitbucket.inkytonik.dsinfo" %% "dsinfo" % dsinfoVersion,
        // Profiling:
        "org.bitbucket.inkytonik.dsprofile" %% "dsprofile" % dsprofileVersion,
        // Reflection
        "org.scala-lang" % "scala-reflect" % scalaVersion
    )
}

val noPublishSettings =
    Seq(
        publish := {},
        publishLocal := {},
        publishSigned := {},
        publishLocalSigned := {}
    )

lazy val base =
    setupSubProject(
        project in file("base"),
        "base"
    ).settings(
        noPublishSettings : _*
    ).settings(
        libraryDependencies := baseLibraryDependencies(scalaVersion.value),
    )

val extrasProject = ProjectRef(file("."), "extras")

lazy val core =
    setupSubProject(
        project in file("core"),
        "kiama"
    ).settings(
        libraryDependencies ++= baseLibraryDependencies(scalaVersion.value),

        console/initialCommands := """
            import org.bitbucket.inkytonik.kiama._
            import rewriting.Rewriter._
        """.stripMargin,
        Compile/packageBin/mappings := (Compile/packageBin/mappings).value ++ (base/Compile/packageBin/mappings).value,
        Compile/packageSrc/mappings := (Compile/packageSrc/mappings).value ++ (base/Compile/packageSrc/mappings).value,

        // Unidoc so we combine docs from base and core (but not extras)
        Compile/doc := (ScalaUnidoc/doc).value,
        Test/doc := (TestScalaUnidoc/doc).value,
        ScalaUnidoc/unidoc/target := crossTarget.value / "api",
        TestScalaUnidoc/unidoc/target := crossTarget.value / "test-api",
        ScalaUnidoc/unidoc/scalacOptions ++=
            Seq(
                if (scalaVersion.value.startsWith("2.10"))
                    "-Ymacro-no-expand"
                else
                    "-Ymacro-expand:none",
                "-doc-source-url",
                    "https://bitbucket.org/inkytonik/kiama/src/master€{FILE_PATH}.scala"
            ),
        TestScalaUnidoc/unidoc/scalacOptions := (ScalaUnidoc/unidoc/scalacOptions).value,
        ScalaUnidoc/unidoc/unidocProjectFilter := inAnyProject -- inProjects(extrasProject),
        TestScalaUnidoc/unidoc/unidocProjectFilter := (ScalaUnidoc/unidoc/unidocProjectFilter).value
    ).dependsOn(
        base % "compile-internal; test-internal"
    )

lazy val extras =
    setupSubProject(
        project in file("extras"),
        "kiama-extras"
    ).settings(
        libraryDependencies ++=
            Seq(
                // Command-line handling:
                "org.rogach" %% "scallop" % "3.1.3",
                // REPLs:
                "jline" % "jline" % "2.14.6"
            ),
        javaOptions ++= Seq("-Xss8M"),
        fork := true,
        run/connectInput := true,
        run/outputStrategy := Some(StdoutOutput),
        Test/console/initialCommands :=
            (Test/console/initialCommands).value + """
                import org.bitbucket.inkytonik.kiama._
                import example.json.PrettyPrinter._
                import example.json.JSONTree._
            """.stripMargin,
        Compile/doc/scalacOptions ++=
            Seq(
                if (scalaVersion.value.startsWith("2.10"))
                    "-Ymacro-no-expand"
                else
                    "-Ymacro-expand:none",
                "-doc-source-url",
                    "https://bitbucket.org/inkytonik/kiama/src/master€{FILE_PATH}.scala"
            ),
        Test/doc/scalacOptions := (Compile/doc/scalacOptions).value
    ).dependsOn(
        base % "compile-internal; test-internal",
        core % "compile; test->test"
    )

lazy val root =
    setupProject(
        project in file("."),
        "root"
    ).settings(
        noPublishSettings : _*
    ).aggregate(
        core, extras
    )
