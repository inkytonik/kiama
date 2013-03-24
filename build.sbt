// Main settings

version in ThisBuild := "1.5.0-SNAPSHOT"

organization in ThisBuild := "com.googlecode.kiama"

// Scala compiler settings

scalaVersion in ThisBuild := "2.10.0"

scalacOptions in ThisBuild := Seq ("-deprecation", "-unchecked")

// Dependency resolution

resolvers in ThisBuild ++= Seq (
    Resolver.sonatypeRepo ("releases"),
    Resolver.sonatypeRepo ("snapshots")
)

// Migration manager (mima)

// mimaDefaultSettings

// previousArtifact in ThisBuild <<= (name, organization) { (n, o) =>
//     Some (o % (n + "_2.10") % "1.4.0")
// }

// Interactive settings

logLevel in ThisBuild := Level.Info

shellPrompt <<= (name, version) { (n, v) =>
     _ => "kiama " + n + " " + v + "> "
}

// No main class since Kiama is a library

mainClass in ThisBuild := None
