// Dependencies

//libraryDependencies ++= Seq (
//    "org.bitbucket.inkytonik.dsname" %% "dsname" % "0.1.0-SNAPSHOT",
//    "org.bitbucket.inkytonik.dsprofile" %% "dsprofile" % "0.2.0-SNAPSHOT"
//)

// Interactive settings

shellPrompt <<= (name, version) { (n, v) =>
     _ => "kiama " + n + " " + v + "> "
}

// Scala compiler settings

scalacOptions in Compile <<= (scalaVersion, scalacOptions) map {
    (version, options) =>
        val versionOptions =
            if (version.startsWith ("2.10"))
                Seq ("-feature",
                     "-language:higherKinds,implicitConversions")
            else
                Seq ()
        options ++ versionOptions
}

scalacOptions in Test <<= (scalaVersion, scalacOptions) map {
    (version, options) =>
        val versionOptions =
            if (version.startsWith ("2.10"))
                Seq ("-feature",
                     "-language:implicitConversions,postfixOps")
            else
                Seq ()
        options ++ versionOptions
}
