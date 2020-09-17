resolvers += Resolver.sbtPluginRepo("releases")

// Required plugins:

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.3")
addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.8.3")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.2-1")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.15.0")

// Optional plugins:

// addSbtPlugin("de.johoop" % "findbugs4sbt" % "1.1.2")
// addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.2.0")
