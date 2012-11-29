// import de.johoop.findbugs4sbt.FindBugs.findbugsSettings
// import de.johoop.findbugs4sbt.ReportType.FancyHtml

// import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
// import com.typesafe.tools.mima.plugin.MimaKeys.previousArtifact

// Main settings

name := "kiama"

version := "1.4.0-B5-SNAPSHOT"

organization := "com.googlecode.kiama"

// Scala compiler settings

scalaVersion := "2.10.0-RC3"

scalaBinaryVersion <<= scalaVersion

crossScalaVersions := Seq ("2.9.2", "2.10.0-RC3")

scalacOptions := Seq ("-deprecation", "-unchecked")

scalacOptions in Compile <<= (scalaVersion, scalacOptions) map {
    (version, options) =>
        val versionOptions =
            version match {
                case "2.9.2" =>
                    Seq ()
                case _ =>
                    Seq ("-feature",
                         "-language:higherKinds,implicitConversions")
            }
        options ++ versionOptions
}

scalacOptions in Test <<= (scalaVersion, scalacOptions) map {
    (version, options) =>
        val versionOptions =
            version match {
                case "2.9.2" =>
                    Seq ()
                case _ =>
                    Seq ("-feature",
                         "-language:implicitConversions,postfixOps")
            }
        options ++ versionOptions
}

// Migration manager (mima)

// mimaDefaultSettings

// previousArtifact <<= (name, organization) { (n, o) =>
//     Some (o % (n + "_2.9.2") % "1.3.0")
// }

// Interactive settings

logLevel := Level.Info

shellPrompt <<= (name, version) { (n, v) =>
     _ => n + " " + v + "> "
}

// No main class since Kiama is a library

mainClass := None

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

// Dependencies

libraryDependencies <++= scalaVersion {
    version =>
        Seq (
            "jline" % "jline" % "1.0",
            "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
            if (version.startsWith ("2.10"))
                "org.scalatest" %% "scalatest" % "2.0.M5-B1" % "test"
            else
                "org.scalatest" %% "scalatest" % "2.0.M6-SNAP1" % "test"
        )
}

resolvers ++= Seq (
    Resolver.sonatypeRepo ("releases"),
    Resolver.sonatypeRepo ("snapshots")
)

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

// Publishing

publishTo <<= version { v =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith ("SNAPSHOT"))
        Some ("snapshots" at nexus + "content/repositories/snapshots")
    else
        Some ("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := true

pomIncludeRepository := { x => false }

pomExtra := (
    <url>http://kiama.googlecode.com</url>
    <licenses>
        <license>
            <name>LGPL 3.0 license</name>
            <url>http://www.opensource.org/licenses/lgpl-3.0.html</url>
            <distribution>repo</distribution>
        </license>
    </licenses>
    <scm>
        <url>https://kiama.googlecode.com/hg</url>
        <connection>scm:hg:https://kiama.googlecode.com/hg</connection>
    </scm>
    <developers>
        <developer>
           <id>inkytonik</id>
           <name>Tony Sloane</name>
           <url>https://code.google.com/u/inkytonik</url>
        </developer>
    </developers>
)

// findbugs4sbt: commented out by default since it brings in many dependencies
// and is used rarely. See also imports at top of file.

// seq (findbugsSettings : _*)
//
// // The next two settings ensure that the test classes are included,
// // otherwise it just check the main library files.  There appears to
// // be no way to add more than one path to be analyzed.
// findbugsAnalyzedPath <<= target { identity[File] }
//
// // Normally it just depends on compile in Compile so changes in the
// // test files do not trigger a rebuild before the findbugs task runs
// findbugsPathSettings <<= findbugsPathSettings.dependsOn (compile in Test)
//
// findbugsReportType := FancyHtml
//
// findbugsReportName := "findbugsReport.html"
//
// findbugsExcludeFilters := Some (
//     <FindBugsFilter>
//         <!-- For a private final foo = exp construct where exp is constant, scalac
//              seems to inline the constant into the foo() accessor, but also leaves
//              the private foo field, which is now not used -->
//         <Match>
//             <Class name="org.kiama.example.obr.RISCEncoder$" />
//             <Bug pattern="UUF_UNUSED_FIELD" />
//             <Or>
//                 <Field name="firsttemp" />
//                 <Field name="lasttemp" />
//                 <Field name="memreg" />
//             </Or>
//         </Match>
//         <Match>
//             <Class name="org.kiama.example.obr.SPARCEncoder" />
//             <Bug pattern="UUF_UNUSED_FIELD" />
//             <Or>
//                 <Field name="arg1reg" />
//                 <Field name="arg2reg" />
//                 <Field name="memreg" />
//                 <Field name="resreg" />
//             </Or>
//         </Match>
//         <!-- Places where a method is added to an anonymous class.  Findbugs thinks
//              it can't be called, but it can. -->
//         <Match>
//             <Class name="~org\.kiama\.example\.iswim\.tests\.SECDTests\$\$anonfun\$[1-8]\$\$anon\$[1-8]" />
//             <Method name="verify" />
//             <Bug pattern="UMAC_UNCALLABLE_METHOD_OF_ANONYMOUS_CLASS" />
//         </Match>
//         <Match>
//             <Class name="org.kiama.example.iswim.secd.HeapOps$RefValue$$anon$1" />
//             <Method name="toDoc" />
//             <Bug pattern="UMAC_UNCALLABLE_METHOD_OF_ANONYMOUS_CLASS" />
//         </Match>
//         <!-- Places where a pattern match ensures that a value is null or not null,
//              but then subsequent code checks it again. -->
//         <Match>
//             <Class name="org.kiama.example.iswim.driver.Main$" />
//             <Method name="processArgs$1" />
//             <Bug pattern="RCN_REDUNDANT_NULLCHECK_WOULD_HAVE_BEEN_A_NPE" />
//         </Match>
//         <Match>
//             <Class name="org.kiama.example.iswim.compiler.Parser$class" />
//             <Method name="stripComments$1" />
//             <Bug pattern="RCN_REDUNDANT_NULLCHECK_OF_NONNULL_VALUE" />
//         </Match>
//         <Match>
//             <Class name="org.kiama.example.picojava.TypeAnalysis$$anonfun$5$$anonfun$apply$3" />
//             <Method name="apply" />
//             <Bug pattern="RCN_REDUNDANT_NULLCHECK_OF_NONNULL_VALUE" />
//         </Match>
//         <!-- Case classes are implemented using a static MODULE$ link to the actual
//              class.  Findbugs complains that this field is written. -->
//         <Match>
//             <Field name="MODULE$" />
//             <Bug pattern="ST_WRITE_TO_STATIC_FROM_INSTANCE_METHOD" />
//         </Match>
//         <!-- These seem to be due to a pattern matching code fragment which first tests
//              the value for null, and if that's true, jumps to a block to produce a match
//              error.  That block loads the null value.  Seems to be (mostly) for cases
//              where the pattern match is on the LHS off assignment or in comprehension.
//              FIXME: We assume that all of these bugs are due to this reason, but haven't
//              checked every case. -->
//         <Match>
//             <Method name="apply" />
//             <Bug pattern="NP_LOAD_OF_KNOWN_NULL_VALUE" />
//         </Match>
//         <Match>
//             <Class name="org.kiama.example.obr.RISCTransformation$" />
//             <Method name="org$kiama$example$obr$RISCTransformation$$cblock" />
//             <Bug pattern="NP_LOAD_OF_KNOWN_NULL_VALUE" />
//         </Match>
//         <Match>
//             <Class name="org.kiama.example.obr.tests.ObrExecTests" />
//             <Method name="org$kiama$example$obr$tests$ObrExecTests$$exectest" />
//             <Bug pattern="NP_LOAD_OF_KNOWN_NULL_VALUE" />
//         </Match>
//         <Match>
//             <Class name="org.kiama.example.til.TIL2_3$class" />
//             <Method name="transform" />
//             <Bug pattern="NP_LOAD_OF_KNOWN_NULL_VALUE" />
//         </Match>
//         <Match>
//             <Class name="org.kiama.util.PrettyPrinterBase$class" />
//             <Method name="string" />
//             <Bug pattern="NP_LOAD_OF_KNOWN_NULL_VALUE" />
//         </Match>
//         <!-- A possible null value can reach a dereference, but for now we are ok with
//              it since null checks are not present in much of the code anyway. -->
//         <Match>
//             <Class name="org.kiama.example.obr.SymbolTable$Variable" />
//             <Method name="&lt;init&gt;" />
//             <Bug pattern="NP_NULL_ON_SOME_PATH_MIGHT_BE_INFEASIBLE" />
//         </Match>
//         <!-- Findbugs doesn't like the classes that Scala produces whose names are
//              something starting with an upper case letter then $something,
//              complaining that it doesn't start with an upper case letter... -->
//         <Match>
//             <Or>
//                 <Class name="~.*\$class" />
//                 <Class name="org.kiama.example.lambda2.Evaluator$freshvar$" />
//                 <Class name="org.kiama.example.picojava.benchmark.PicoJavaBenchmark$delayedInit$body" />
//             </Or>
//             <Bug pattern="NM_CLASS_NAMING_CONVENTION" />
//         </Match>
//         <!-- These ones actually *do* start with a lower case letter, but that is
//              OK due to their role to implement the package object. -->
//         <Match>
//             <Or>
//                 <Class name="org.kiama.package" />
//                 <Class name="org.kiama.package$" />
//             </Or>
//             <Bug pattern="NM_CLASS_NAMING_CONVENTION" />
//         </Match>
//         <!-- These classes have "Exception" in their name for a good reason... -->
//         <Match>
//             <Or>
//                 <Class name="org.kiama.example.iswim.secd.SECDBase$MkUserException" />
//                 <Class name="org.kiama.example.iswim.secd.SECDBase$PushMachineException" />
//                 <Class name="org.kiama.example.iswim.secd.SECDBase$RaiseException" />
//             </Or>
//             <Bug pattern="NM_CLASS_NOT_EXCEPTION" />
//         </Match>
//         <!-- Allow assert as a method name since used by testing frameworks. -->
//         <Match>
//             <Method name="assert" />
//             <Bug pattern="NM_FUTURE_KEYWORD_USED_AS_MEMBER_IDENTIFIER" />
//         </Match>
//         <!-- These are methods derived from objects, whose names *should* start with
//              an upper case letter. -->
//         <Match>
//             <Or>
//                 <Method name="FocusTest" />
//                 <Method name="MaxDiscarded" />
//                 <Method name="MaxSize" />
//                 <Method name="MinSize" />
//                 <Method name="MinSuccessful" />
//                 <Method name="PropertyCheckConfig" />
//                 <Method name="Workers" />
//                 <Method name="Error" />
//                 <Method name="Failure" />
//                 <Method name="NoSuccess" />
//                 <Method name="OnceParser" />
//                 <Method name="Parser" />
//                 <Method name="Success" />
//                 <Method name="Pos" />
//             </Or>
//             <Bug pattern="NM_METHOD_NAMING_CONVENTION" />
//         </Match>
//         <Match>
//             <Or>
//                 <Class name="org.kiama.attribution.AttributionTests" />
//                 <Class name="org.kiama.attribution.DynamicAttributionTests" />
//             </Or>
//             <Or>
//                 <Method name="Leaf" />
//                 <Method name="Pair" />
//                 <Method name="GenSeqTree" />
//                 <Method name="ListTree" />
//                 <Method name="MapTree" />
//                 <Method name="PairTree" />
//                 <Method name="SetTree" />
//                 <Method name="Unused" />
//             </Or>
//             <Bug pattern="NM_METHOD_NAMING_CONVENTION" />
//         </Match>
//         <Match>
//             <Class name="org.kiama.attribution.DynamicAttributionTests$$anonfun$8" />
//             <Or>
//                 <Method name="ExtensionOne$1" />
//                 <Method name="ExtensionTwo$1" />
//             </Or>
//             <Bug pattern="NM_METHOD_NAMING_CONVENTION" />
//         </Match>
//         <Match>
//             <Class name="org.kiama.attribution.DynamicAttributionTests$$anonfun$9" />
//             <Method name="Extension$1" />
//             <Bug pattern="NM_METHOD_NAMING_CONVENTION" />
//         </Match>
//         <Match>
//             <Class name="org.kiama.example.RISC.RISC" />
//             <Method name="Mem" />
//             <Bug pattern="NM_METHOD_NAMING_CONVENTION" />
//         </Match>
//         <Match>
//             <Package name="~org\.kiama\.example\.iswim.*" />
//             <Or>
//                 <Method name="ExnContValue" />
//                 <Method name="FalseValue" />
//                 <Method name="IntValue" />
//                 <Method name="RecordValue" />
//                 <Method name="RefValue" />
//                 <Method name="StringValue" />
//                 <Method name="TrueValue" />
//                 <Method name="UserExceptionValue" />
//                 <Method name="ClosureValue" />
//                 <Method name="ContValue" />
//                 <Method name="EmptyValue" />
//                 <Method name="PrimValue" />
//                 <Method name="EmptyCont" />
//             </Or>
//             <Bug pattern="NM_METHOD_NAMING_CONVENTION" />
//         </Match>
//         <Match>
//             <Package name="org.kiama.example.picojava" />
//             <Or>
//                 <Method name="Value" />
//                 <Method name="Variable" />
//                 <Method name="BlockStmts" />
//                 <Method name="Body" />
//                 <Method name="Name" />
//                 <Method name="Superclass" />
//                 <Method name="IdnUse" />
//                 <Method name="ObjectReference" />
//                 <Method name="Block" />
//                 <Method name="Type" />
//                 <Method name="Condition" />
//                 <Method name="PrimValue" />
//             </Or>
//             <Bug pattern="NM_METHOD_NAMING_CONVENTION" />
//         </Match>
//         <Match>
//             <Package name="org.kiama.rewriting" />
//             <Or>
//                 <Method name="Term" />
//                 <Method name="Equal" />
//                 <Method name="NotSame" />
//                 <Method name="Same" />
//             </Or>
//             <Bug pattern="NM_METHOD_NAMING_CONVENTION" />
//         </Match>
//         <Match>
//             <Class name="org.kiama.util.PrettyPrinterTests" />
//             <Method name="Val" />
//             <Bug pattern="NM_METHOD_NAMING_CONVENTION" />
//         </Match>
//         <!-- Scala generates classes that are serializable but contain non-transient
//              non-serializable instance fields, particularly $outer.  Not sure if that
//              has any implications, but it's none of our business... -->
//         <Match>
//             <Or>
//                 <Bug pattern="SE_BAD_FIELD" />
//                 <Bug pattern="SE_BAD_FIELD_STORE" />
//             </Or>
//         </Match>
//         <!-- Some cases where mutable object references are stored in other objects
//              or returned. Due to use of App, for comprehension, anon functions inside
//              defs  -->
//         <Match>
//             <Class name="org.kiama.example.picojava.benchmark.PicoJavaBenchmark$" />
//             <Field name="scala$App$$_args" />
//             <Or>
//                 <Bug pattern="EI_EXPOSE_REP" />
//                 <Bug pattern="EI_EXPOSE_REP2" />
//             </Or>
//         </Match>
//         <Match>
//             <Class name="org.kiama.util.CompilerBase$$anonfun$driver$1" />
//             <Field name="newargs$1" />
//             <Bug pattern="EI_EXPOSE_REP2" />
//         </Match>
//         <Match>
//             <Class name="org.kiama.util.TestCompiler$$anonfun$filetest$1$1" />
//             <Field name="cmd$1" />
//             <Bug pattern="EI_EXPOSE_REP2" />
//         </Match>
//         <Match>
//             <Class name="org.kiama.util.TestCompiler$$anonfun$filetests$1" />
//             <Field name="children$1" />
//             <Bug pattern="EI_EXPOSE_REP2" />
//         </Match>
//         <Match>
//             <Class name="org.kiama.util.TestCompiler$$anonfun$filetests$1$$anonfun$apply$1" />
//             <Field name="args$2" />
//             <Bug pattern="EI_EXPOSE_REP2" />
//         </Match>
//         <Match>
//             <Class name="org.kiama.util.TestCompiler$$anonfun$infiletests$1$1" />
//             <Field name="args$1" />
//             <Bug pattern="EI_EXPOSE_REP2" />
//         </Match>
//         <!-- These synchronisation problems seem to be due to something the compiler
//              is doing, not sure what. Ignoring for now... -->
//         <Match>
//             <Or>
//                 <Class name="org.kiama.example.iswim.secd.SECDBase" />
//                 <Class name="org.kiama.example.obr.SyntaxAnalysis" />
//             </Or>
//             <Bug pattern="IS2_INCONSISTENT_SYNC" />
//         </Match>
//         </FindBugsFilter>)
